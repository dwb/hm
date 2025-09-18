;;; agent-sandbox.el --- Restricted elisp execution for AI agents  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Dan Brown

;; Author: Dan Brown <dan@stompydan.net>

;; Provides a macro that validates and executes a restricted subset of
;; Emacs Lisp on behalf of an AI agent. The agent proposes elisp code;
;; the macro checks it against an allowlist of forms and a set of
;; permitted file paths before evaluating anything. If validation
;; fails, the macro returns a structured error explaining what was
;; disallowed.
;;
;; File access is governed by:
;;   1. The project root (passed as a keyword argument to the macro).
;;   2. Permission rules parsed from Claude Code settings files, which
;;      follow the gitignore pattern syntax documented at
;;      https://code.claude.com/docs/en/permissions#read-and-edit
;;
;; Settings files are read from (highest to lowest precedence):
;;   - <project-root>/.claude/settings.local.json
;;   - <project-root>/.claude/settings.json
;;   - ~/.claude/settings.json
;;
;; Permission rules use four path pattern types:
;;   //path   - absolute path from filesystem root
;;   ~/path   - path relative to home directory
;;   /path    - path relative to project root
;;   path     - path relative to current directory (or ./path)
;;
;; Within patterns, * matches within a single directory component and
;; ** matches recursively across directories, per gitignore semantics.
;;
;; Deny rules take precedence over allow rules.

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'byte-opt)

;;; Form validation

(defun agent-sandbox--function-allowed-p (sym)
  "Return non-nil if SYM names a function that the sandbox permits.
Currently permits functions marked `pure' or `side-effect-free'
by the byte compiler."
  (and (symbolp sym)
       (fboundp sym)
       (not (special-form-p (symbol-function sym)))
       (or (get sym 'pure)
           (get sym 'side-effect-free))))

(defun agent-sandbox--validate-form (form)
  "Validate FORM for sandbox execution.
Returns nil if the form is allowed, or an error message string if it
is not.  Validates recursively into subforms."
  (cond
   ;; Atoms: numbers, strings, keywords, nil, t are fine
   ((or (not form) (eq form t) (keywordp form)
        (numberp form) (stringp form))
    nil)
   ;; Quoted forms: data, not code
   ((and (consp form) (eq (car form) 'quote))
    nil)
   ;; Function call
   ((consp form)
    (let ((head (car form)))
      (cond
       ;; Head must be a symbol (no lambda calls)
       ((not (symbolp head))
        (format "non-symbol in call position: %S" head))
       ;; Check if the function is allowed
       ((not (agent-sandbox--function-allowed-p head))
        (format "function not allowed: %s" head))
       ;; Recursively validate arguments
       (t
        (cl-loop for arg in (cdr form)
                 for err = (agent-sandbox--validate-form arg)
                 when err return err)))))
   ;; Bare symbols (variable references) -- reject for now, since we
   ;; have no variable binding mechanism yet
   ((symbolp form)
    (format "unbound symbol reference: %s" form))
   (t
    (format "unsupported form: %S" form))))

(cl-defun agent-sandbox-eval (&key project-root body)
  "Evaluate a sandboxed elisp form.
PROJECT-ROOT is the project root directory for permission evaluation.
BODY is the form to validate and evaluate.  Only pure and
side-effect-free functions are permitted.  Returns an alist with
`status' (`ok' or `error'), and either `value' or `message'."
  (let* ((err (agent-sandbox--validate-form body)))
    (if err
        (list (cons 'status 'error)
              (cons 'message err))
      (condition-case eval-err
          (list (cons 'status 'ok)
                (cons 'value (eval body t)))
        (error
         (list (cons 'status 'error)
               (cons 'message (format "evaluation error: %s" eval-err))))))))

;;; Settings file parsing

(defun agent-sandbox--read-json-file (path)
  "Read and parse a JSON file at PATH, returning nil if it does not exist."
  (when (file-exists-p path)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents path)
          (json-parse-buffer :object-type 'alist :array-type 'list))
      (error
       (message "agent-sandbox: failed to parse %s: %s" path err)
       nil))))

(defun agent-sandbox--settings-files (project-root)
  "Return the list of settings files to consult, highest precedence first.
PROJECT-ROOT is the root directory of the project."
  (let ((project (file-name-as-directory (expand-file-name project-root))))
    (seq-filter #'identity
                (list (concat project ".claude/settings.local.json")
                      (concat project ".claude/settings.json")
                      (expand-file-name "~/.claude/settings.json")))))

(defun agent-sandbox--parse-permissions (settings)
  "Extract the permissions alist from a parsed SETTINGS alist.
Returns an alist with `allow' and `deny' keys, each holding a list of
rule strings, or nil if no permissions are defined."
  (let ((perms (alist-get 'permissions settings)))
    (when perms
      (list (cons 'allow (alist-get 'allow perms))
            (cons 'deny  (alist-get 'deny perms))))))

(defun agent-sandbox--collect-permissions (project-root)
  "Collect merged permission rules from all settings files.
PROJECT-ROOT is the root directory of the project.  Returns an alist
with `allow' and `deny' keys.  Rules from higher-precedence files
appear earlier in each list.  Deny rules always take precedence over
allow rules during evaluation (handled by the matching functions)."
  (let ((files (agent-sandbox--settings-files project-root))
        (allow-rules nil)
        (deny-rules nil))
    (dolist (file files)
      (let* ((settings (agent-sandbox--read-json-file file))
             (perms (agent-sandbox--parse-permissions settings)))
        (when perms
          (setq allow-rules (append allow-rules (alist-get 'allow perms)))
          (setq deny-rules  (append deny-rules  (alist-get 'deny perms))))))
    (list (cons 'allow allow-rules)
          (cons 'deny  deny-rules))))

;;; Permission rule pattern matching

(defun agent-sandbox--parse-file-rule (rule)
  "Parse a Read or Edit permission RULE string.
Returns nil if RULE is not a file permission rule (i.e. not a Read(...)
or Edit(...) rule).  Otherwise returns an alist with keys:
  `tool'    - symbol, `read' or `edit'
  `pattern' - the path pattern string inside the parens"
  (when (string-match (rx bos (group (or "Read" "Edit"))
                          "(" (group (* anything)) ")" eos)
                      rule)
    (list (cons 'tool (intern (downcase (match-string 1 rule))))
          (cons 'pattern (match-string 2 rule)))))

(defun agent-sandbox--expand-pattern (pattern project-root)
  "Expand a permission path PATTERN to an absolute glob pattern.
PROJECT-ROOT is the project root directory.  Handles the four pattern
types from the Claude Code permission spec:
  //path  -> /path                    (absolute)
  ~/path  -> <home>/path              (home-relative)
  /path   -> <project-root>/path      (project-relative)
  path    -> <default-directory>/path  (cwd-relative, also ./path)"
  (let ((project (file-name-as-directory (expand-file-name project-root))))
    (cond
     ;; Absolute: //path -> /path
     ((string-prefix-p "//" pattern)
      (substring pattern 1))
     ;; Home-relative: ~/path
     ((string-prefix-p "~/" pattern)
      (expand-file-name (substring pattern 2) "~/"))
     ;; Project-relative: /path
     ((string-prefix-p "/" pattern)
      (concat project (substring pattern 1)))
     ;; CWD-relative: path or ./path
     ((string-prefix-p "./" pattern)
      (expand-file-name (substring pattern 2) default-directory))
     (t
      (expand-file-name pattern default-directory)))))

(defun agent-sandbox--glob-to-regexp (glob)
  "Convert a gitignore-style GLOB pattern to an Emacs regexp.
Handles * (single directory component) and ** (recursive match)."
  (let ((i 0)
        (len (length glob))
        (parts nil))
    (while (< i len)
      (let ((c (aref glob i)))
        (cond
         ;; **/ or ** at end: match any number of directory components
         ((and (eq c ?*)
               (< (1+ i) len)
               (eq (aref glob (1+ i)) ?*))
          (push ".*" parts)
          ;; Skip the two stars and an optional following slash
          (setq i (+ i 2))
          (when (and (< i len) (eq (aref glob i) ?/))
            (setq i (1+ i))))
         ;; * : match within a single directory component (no /)
         ((eq c ?*)
          (push "[^/]*" parts)
          (setq i (1+ i)))
         ;; ? : match a single non-slash character
         ((eq c ??)
          (push "[^/]" parts)
          (setq i (1+ i)))
         ;; Escape regexp-special characters
         ((memq c '(?. ?^ ?$ ?+ ?{ ?} ?| ?\( ?\) ?\[ ?\]))
          (push (regexp-quote (string c)) parts)
          (setq i (1+ i)))
         (t
          (push (string c) parts)
          (setq i (1+ i))))))
    (concat "\\`" (apply #'concat (nreverse parts)) "\\'")))

(defun agent-sandbox--pattern-matches-p (pattern file project-root)
  "Return non-nil if FILE matches PATTERN under PROJECT-ROOT.
PATTERN is a raw Claude Code permission pattern string.  FILE is an
absolute file path."
  (let* ((expanded (agent-sandbox--expand-pattern pattern project-root))
         (regexp (agent-sandbox--glob-to-regexp expanded)))
    (string-match-p regexp file)))

(defun agent-sandbox--file-permitted-p (file tool project-root permissions)
  "Return non-nil if FILE is permitted for TOOL under PERMISSIONS.
FILE is an absolute path.  TOOL is a symbol, `read' or `edit'.
PROJECT-ROOT is the project root directory.  PERMISSIONS is an alist
as returned by `agent-sandbox--collect-permissions'.

Evaluation order: deny rules are checked first.  If any deny rule
matches, access is denied.  Then allow rules are checked.  If any
allow rule matches, access is allowed.  If no rule matches, access
defaults to allowed for files under the project root and denied
otherwise."
  (let ((deny-rules  (alist-get 'deny permissions))
        (allow-rules (alist-get 'allow permissions))
        (tool-name   (if (eq tool 'read) "Read" "Edit")))
    ;; Check deny rules first
    (catch 'result
      (dolist (rule deny-rules)
        (let ((parsed (agent-sandbox--parse-file-rule rule)))
          ;; A bare "Read" or "Edit" deny matches all files for that tool.
          (when (and (null parsed) (string= rule tool-name))
            (throw 'result nil))
          (when (and parsed
                     (eq (alist-get 'tool parsed) tool)
                     (agent-sandbox--pattern-matches-p
                      (alist-get 'pattern parsed) file project-root))
            (throw 'result nil))))
      ;; Check allow rules
      (dolist (rule allow-rules)
        (let ((parsed (agent-sandbox--parse-file-rule rule)))
          (when (and (null parsed) (string= rule tool-name))
            (throw 'result t))
          (when (and parsed
                     (eq (alist-get 'tool parsed) tool)
                     (agent-sandbox--pattern-matches-p
                      (alist-get 'pattern parsed) file project-root))
            (throw 'result t))))
      ;; Default: allow if under project root, deny otherwise
      (string-prefix-p (file-name-as-directory (expand-file-name project-root))
                       (expand-file-name file)))))

;;; Tests

(require 'ert)

(ert-deftest agent-sandbox-test-parse-file-rule ()
  "Parsing Read(...) and Edit(...) rule strings."
  (let ((r (agent-sandbox--parse-file-rule "Read(./.env)")))
    (should (equal (alist-get 'tool r) 'read))
    (should (equal (alist-get 'pattern r) "./.env")))
  (let ((r (agent-sandbox--parse-file-rule "Edit(/src/**/*.ts)")))
    (should (equal (alist-get 'tool r) 'edit))
    (should (equal (alist-get 'pattern r) "/src/**/*.ts")))
  ;; Non-file rules return nil
  (should-not (agent-sandbox--parse-file-rule "Bash(git *)"))
  (should-not (agent-sandbox--parse-file-rule "Read"))
  (should-not (agent-sandbox--parse-file-rule "WebFetch(domain:example.com)")))

(ert-deftest agent-sandbox-test-expand-pattern-absolute ()
  "//path expands to /path."
  (should (equal (agent-sandbox--expand-pattern "//Users/alice/secrets/**" "/proj")
                 "/Users/alice/secrets/**")))

(ert-deftest agent-sandbox-test-expand-pattern-home ()
  "~/path expands relative to home."
  (let ((expected (expand-file-name "Documents/*.pdf" "~/")))
    (should (equal (agent-sandbox--expand-pattern "~/Documents/*.pdf" "/proj")
                   expected))))

(ert-deftest agent-sandbox-test-expand-pattern-project-relative ()
  "/path expands relative to project root."
  (should (equal (agent-sandbox--expand-pattern "/src/**/*.ts" "/home/user/proj")
                 "/home/user/proj/src/**/*.ts")))

(ert-deftest agent-sandbox-test-expand-pattern-cwd-relative ()
  "Bare path and ./path expand relative to default-directory."
  (let ((default-directory "/home/user/proj/"))
    (should (equal (agent-sandbox--expand-pattern "*.env" "/ignored")
                   "/home/user/proj/*.env"))
    (should (equal (agent-sandbox--expand-pattern "./foo/bar" "/ignored")
                   "/home/user/proj/foo/bar"))))

(ert-deftest agent-sandbox-test-glob-to-regexp-star ()
  "* matches within a single directory component."
  (let ((re (agent-sandbox--glob-to-regexp "/src/*.ts")))
    (should (string-match-p re "/src/foo.ts"))
    (should (string-match-p re "/src/.hidden.ts"))
    (should-not (string-match-p re "/src/sub/foo.ts"))))

(ert-deftest agent-sandbox-test-glob-to-regexp-doublestar ()
  "** matches across directory boundaries."
  (let ((re (agent-sandbox--glob-to-regexp "/src/**/*.ts")))
    (should (string-match-p re "/src/foo.ts"))
    (should (string-match-p re "/src/a/b/c/foo.ts"))))

(ert-deftest agent-sandbox-test-glob-to-regexp-literal-dots ()
  "Dots in patterns are literal, not regexp wildcards."
  (let ((re (agent-sandbox--glob-to-regexp "/path/.env")))
    (should (string-match-p re "/path/.env"))
    (should-not (string-match-p re "/path/Xenv"))))

(ert-deftest agent-sandbox-test-glob-to-regexp-question-mark ()
  "? matches a single non-slash character."
  (let ((re (agent-sandbox--glob-to-regexp "/src/?.ts")))
    (should (string-match-p re "/src/a.ts"))
    (should-not (string-match-p re "/src/ab.ts"))
    (should-not (string-match-p re "/src//.ts"))))

(ert-deftest agent-sandbox-test-pattern-matches-p ()
  "Integration: pattern string matched against a file path."
  (should (agent-sandbox--pattern-matches-p
           "/src/**/*.el" "/home/user/proj/src/lib/foo.el" "/home/user/proj"))
  (should-not (agent-sandbox--pattern-matches-p
               "/src/**/*.el" "/home/user/proj/test/foo.el" "/home/user/proj"))
  (should (agent-sandbox--pattern-matches-p
           "//tmp/scratch.txt" "/tmp/scratch.txt" "/irrelevant")))

(ert-deftest agent-sandbox-test-file-permitted-deny-precedence ()
  "Deny rules take precedence over allow rules."
  (let ((perms '((allow . ("Read(/src/**)"))
                 (deny  . ("Read(/src/secret/**)")))))
    (should (agent-sandbox--file-permitted-p
             "/proj/src/foo.el" 'read "/proj" perms))
    (should-not (agent-sandbox--file-permitted-p
                 "/proj/src/secret/keys.el" 'read "/proj" perms))))

(ert-deftest agent-sandbox-test-file-permitted-bare-deny ()
  "A bare tool deny (e.g. \"Edit\") blocks all files for that tool."
  (let ((perms '((allow . nil)
                 (deny  . ("Edit")))))
    (should-not (agent-sandbox--file-permitted-p
                 "/proj/src/foo.el" 'edit "/proj" perms))
    ;; Read is unaffected
    (should (agent-sandbox--file-permitted-p
             "/proj/src/foo.el" 'read "/proj" perms))))

(ert-deftest agent-sandbox-test-file-permitted-default-in-project ()
  "Files under the project root are allowed by default when no rules match."
  (let ((perms '((allow . nil) (deny . nil))))
    (should (agent-sandbox--file-permitted-p
             "/proj/src/foo.el" 'read "/proj" perms))
    (should-not (agent-sandbox--file-permitted-p
                 "/other/place/foo.el" 'read "/proj" perms))))

(ert-deftest agent-sandbox-test-file-permitted-cross-tool ()
  "A Read deny does not affect Edit, and vice versa."
  (let ((perms '((allow . nil)
                 (deny  . ("Read(/secret/**)")))))
    (should-not (agent-sandbox--file-permitted-p
                 "/proj/secret/x" 'read "/proj" perms))
    (should (agent-sandbox--file-permitted-p
             "/proj/secret/x" 'edit "/proj" perms))))

(ert-deftest agent-sandbox-test-parse-permissions ()
  "Extracting permissions from a parsed settings alist."
  (let* ((settings '((permissions . ((allow . ("Read(/src/**)"))
                                     (deny  . ("Edit"))))))
         (perms (agent-sandbox--parse-permissions settings)))
    (should (equal (alist-get 'allow perms) '("Read(/src/**)")))
    (should (equal (alist-get 'deny perms) '("Edit"))))
  ;; No permissions key returns nil
  (should-not (agent-sandbox--parse-permissions '((model . "opus")))))

(ert-deftest agent-sandbox-test-settings-files ()
  "Settings file list is returned in precedence order."
  (let ((files (agent-sandbox--settings-files "/home/user/proj")))
    (should (= (length files) 3))
    (should (string-suffix-p ".claude/settings.local.json" (nth 0 files)))
    (should (string-suffix-p ".claude/settings.json" (nth 1 files)))
    (should (string-prefix-p (expand-file-name "~/") (nth 2 files)))))

;;; Macro tests

(ert-deftest agent-sandbox-test-eval-pure-arithmetic ()
  "Pure arithmetic is allowed and evaluated."
  (let ((result (agent-sandbox-eval :project-root "/proj" :body '(+ 1 2 3))))
    (should (equal (alist-get 'status result) 'ok))
    (should (equal (alist-get 'value result) 6))))

(ert-deftest agent-sandbox-test-eval-side-effect-free ()
  "Side-effect-free functions like `concat' are allowed."
  (let ((result (agent-sandbox-eval :project-root "/proj"
                                    :body '(concat "hello" " " "world"))))
    (should (equal (alist-get 'status result) 'ok))
    (should (equal (alist-get 'value result) "hello world"))))

(ert-deftest agent-sandbox-test-eval-rejects-side-effects ()
  "Functions with side effects are rejected."
  (let ((result (agent-sandbox-eval :project-root "/proj"
                                    :body '(delete-file "/tmp/foo"))))
    (should (equal (alist-get 'status result) 'error))
    (should (string-match-p "delete-file" (alist-get 'message result)))))

(ert-deftest agent-sandbox-test-eval-nested-validation ()
  "Nested calls are validated recursively."
  (let ((result (agent-sandbox-eval :project-root "/proj"
                                    :body '(+ 1 (delete-file "/tmp/foo")))))
    (should (equal (alist-get 'status result) 'error))
    (should (string-match-p "delete-file" (alist-get 'message result)))))

(ert-deftest agent-sandbox-test-eval-literals ()
  "Literal values are allowed."
  (dolist (body '(42 "hello" nil t :keyword))
    (let ((result (agent-sandbox-eval :project-root "/proj" :body body)))
      (should (equal (alist-get 'status result) 'ok)))))

(ert-deftest agent-sandbox-test-eval-quoted-data ()
  "Quoted forms are allowed without inspecting their contents."
  (let ((result (agent-sandbox-eval :project-root "/proj"
                                    :body '(quote (delete-file "anything")))))
    (should (equal (alist-get 'status result) 'ok))
    (should (equal (alist-get 'value result) '(delete-file "anything")))))

(ert-deftest agent-sandbox-test-eval-unknown-function ()
  "Unknown/unbound functions are rejected."
  (let ((result (agent-sandbox-eval :project-root "/proj"
                                    :body '(my-nonexistent-fn 1 2))))
    (should (equal (alist-get 'status result) 'error))
    (should (string-match-p "my-nonexistent-fn" (alist-get 'message result)))))

(ert-deftest agent-sandbox-test-eval-special-forms-rejected ()
  "Special forms like `setq' are rejected."
  (let ((result (agent-sandbox-eval :project-root "/proj"
                                    :body '(setq x 42))))
    (should (equal (alist-get 'status result) 'error))
    (should (string-match-p "setq" (alist-get 'message result)))))

(provide 'agent-sandbox)
;;; agent-sandbox.el ends here
