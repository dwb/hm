;;; lib/my-org-jj.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(eval-when-compile (require 'subr-x))

(defvar org-stored-links)
(defvar org-dblock-start-re)
(defvar org-dblock-end-re)
(defvar org-src-lang-modes)
(defvar diff-font-lock-keywords)
(defvar diff-font-lock-syntax)
(declare-function org-entry-get "org")
(declare-function org-mode "org")
(declare-function org-update-all-dblocks "org")
(declare-function org-escape-code-in-string "org-src")
(declare-function org-link-set-parameters "ol")
(declare-function org-link-store-props "ol")
(declare-function vc-diff-internal "vc")
(declare-function vc-find-revision-no-save "vc")

;; Org "jj:" links to a file at a jj revision, optionally at a line:
;;
;;   [[jj:src/foo.el::abc123]]
;;   [[jj:src/foo.el::abc123::42]]
;;
;; PATH is resolved like a "file:" link. The revision is any revset that
;; resolves to one commit. Following a link shows the file in a read-only
;; buffer that does not visit a file.

(defvar-local my/org-jj-file nil
  "Absolute name of the file shown in this jj revision buffer.")
(put 'my/org-jj-file 'permanent-local t)

(defvar-local my/org-jj-revision nil
  "The jj revision shown in this jj revision buffer.")
(put 'my/org-jj-revision 'permanent-local t)

(defun my/org-jj--parse-link (link)
  "Parse jj LINK into a list (PATH REV LINE).
LINK has the form PATH::REV or PATH::REV::LINE. PATH is the text before
the first \"::\", since REV may itself contain \"::\". A final \"::\"
followed only by digits is taken as LINE, otherwise LINE is nil."
  (let ((sep (or (string-search "::" link)
                 (user-error "No revision in jj link: %s" link))))
    (let ((path (substring link 0 sep))
          (rest (substring link (+ sep 2))))
      (save-match-data
        (if (string-match "\\`\\(.+\\)::\\([0-9]+\\)\\'" rest)
            (list path (match-string 1 rest)
                  (string-to-number (match-string 2 rest)))
          (list path rest nil))))))

;; These call jj directly because the signatures of vc-jj's private
;; process helpers differ between vc-jj releases.
(defun my/org-jj--output (&rest args)
  "Run jj with ARGS in `default-directory' and return stdout as a string.
Ignore stderr, where jj prints warnings, unless jj fails. In that case
signal an error that includes it."
  (let ((stderr (make-temp-file "my-org-jj-stderr")))
    (unwind-protect
        (with-temp-buffer
          (let ((status (apply #'process-file "jj" nil (list t stderr) nil
                               args)))
            (unless (eq status 0)
              (error "jj %s failed: %s" (string-join args " ")
                     (with-temp-buffer
                       (insert-file-contents stderr)
                       (string-trim (buffer-string)))))
            (buffer-string)))
      (delete-file stderr))))

(defun my/org-jj--lines (&rest args)
  "Run jj with ARGS in `default-directory' and return stdout as lines.
See `my/org-jj--output' for error handling."
  (split-string (apply #'my/org-jj--output args) "\n" t))

(defun my/org-jj--fileset (file)
  "Return a jj fileset expression matching exactly FILE."
  (format "root:%S"
          (file-relative-name file (vc-call-backend 'JJ 'root file))))

(defun my/org-jj--commit-ids (revset)
  "Return the commit IDs of the commits in jj REVSET.
Run jj in `default-directory'."
  (my/org-jj--lines "log" "--no-graph" "-r" revset
                    "-T" "commit_id ++ \"\\n\""))

(defun my/org-jj--commit-id (rev)
  "Return the commit ID that jj revision REV resolves to.
Run jj in `default-directory'."
  (car (my/org-jj--commit-ids rev)))

(defun my/org-jj--working-copy-commit-id (file)
  "Return the ID of a commit holding the current saved content of FILE.
Return the parent of @ when @ has one parent and does not change FILE.
Unlike @, that commit is not replaced by the next working-copy snapshot,
so the link does not depend on a hidden commit that `jj util gc' can
remove. Otherwise return @."
  (let ((changed (my/org-jj--lines "diff" "--name-only" "-r" "@" "--"
                                   (my/org-jj--fileset file)))
        (parents (my/org-jj--commit-ids "parents(@)")))
    (if (and (null changed) (length= parents 1))
        (car parents)
      (my/org-jj--commit-id "@"))))

(defun my/org-jj--link (file commit line)
  "Return a list (LINK DESCRIPTION) for FILE at COMMIT and LINE."
  (list (format "jj:%s::%s::%d" (abbreviate-file-name file) commit line)
        (format "%s@%s:%d"
                (file-name-nondirectory file) (substring commit 0 12) line)))

(defun my/org-jj-revision-buffer (file rev)
  "Return a read-only buffer showing FILE at jj revision REV.
The buffer does not visit a file."
  (let ((buf (get-buffer-create
              (format "%s.~%s~" (file-name-nondirectory file) rev))))
    (with-current-buffer buf
      (setq default-directory (file-name-directory file))
      (let ((inhibit-read-only t))
        (vc-find-revision-no-save file rev 'JJ buf))
      ;; vc-find-revision-no-save uses delay-mode-hooks, which also
      ;; stops global-font-lock-mode from enabling font-lock.
      (font-lock-mode 1)
      (setq my/org-jj-file file
            my/org-jj-revision rev))
    buf))

(defun my/org-jj-open (link _arg)
  "Follow jj LINK. See `my/org-jj--parse-link' for its form."
  (pcase-let ((`(,path ,rev ,line) (my/org-jj--parse-link link)))
    (pop-to-buffer (my/org-jj-revision-buffer (expand-file-name path) rev))
    (when line
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun my/org-jj-store-link (&optional _interactive)
  "Store a jj link to the current line of a jj revision buffer.
The revision is stored as a commit ID, so links from buffers opened
with relative revsets such as \"@-\" keep pointing at the same content."
  (when my/org-jj-revision
    (pcase-let ((`(,link ,desc)
                 (my/org-jj--link my/org-jj-file
                                  (my/org-jj--commit-id my/org-jj-revision)
                                  (line-number-at-pos nil t))))
      (org-link-store-props :type "jj" :link link :description desc)
      t)))

(defun my/org-jj-store-link-at-commit ()
  "Store a jj link to the current line of this file at its current commit.
The link goes in `org-stored-links', for `org-insert-link'. See
`my/org-jj--working-copy-commit-id' for how the commit is chosen.

This is a separate command so that `org-store-link' keeps storing
\"file:\" links in buffers visiting files tracked by jj."
  (interactive)
  (unless (and buffer-file-name (eq (vc-backend buffer-file-name) 'JJ))
    (user-error "Buffer is not visiting a file tracked by jj"))
  (require 'ol)
  (let ((entry (my/org-jj--link
                buffer-file-name
                (my/org-jj--working-copy-commit-id buffer-file-name)
                (line-number-at-pos nil t))))
    (setq org-stored-links (cons entry (delete entry org-stored-links)))
    (message "Stored: %s%s" (cadr entry)
             (if (buffer-modified-p)
                 " (buffer has unsaved changes, so the line may not match)"
               ""))))

;; Diffs in Org, as a dynamic block and as a link.
;;
;; A "jj-diff" dynamic block shows one file's diff between two
;; revisions, filled in by jj when the block is updated (C-c C-x C-u, or
;; `org-update-all-dblocks'):
;;
;;   #+BEGIN: jj-diff :from "3f2a91c0" :to "7a564005" :file "src/foo.el" :lines (40 90)
;;   #+END:
;;
;; :from and :to, or :rev on its own, give the revisions, as for
;; "jj diff". They must be quoted, because Org reads block parameters
;; with `read': an unquoted 7a564005 would become a number. :file is
;; relative to the repository root. :lines (START END), which is
;; optional, keeps only hunks whose lines on the new side overlap START
;; to END. :context N sets the number of context lines. jj runs in the
;; directory given by the inherited JJ_REPO property, if set, and
;; otherwise in the directory of the Org file.
;;
;; `my/org-jj-diff-uncovered' reports hunks that no block shows, and
;; `my/org-jj-diff-visit' (also on C-c C-o) opens the file line for the
;; diff line at point.
;;
;; A "jj-diff:" link opens a `vc-diff' buffer for PATH between FROM and
;; TO. PATH is resolved like a "file:" link and may be a directory:
;;
;;   [[jj-diff:src/foo.el::3f2a91c0::7a564005]]

(cl-defstruct (my/org-jj-hunk (:constructor my/org-jj-hunk-create)
                              (:copier nil))
  "One hunk of a diff. TEXT includes the \"@@\" header line."
  old-start old-lines new-start new-lines text)

(cl-defstruct (my/org-jj-file-diff (:constructor my/org-jj-file-diff-create)
                                   (:copier nil))
  "The diff of one file. HEADER is the text before the first hunk."
  path header hunks)

(cl-defstruct (my/org-jj-dblock (:constructor my/org-jj-dblock-create)
                                (:copier nil))
  "A jj-diff dynamic block. POS is the start of its #+BEGIN line.
DIRECTORY is where jj runs for this block."
  pos params directory)

(defconst my/org-jj--hunk-header-regexp
  (concat "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?"
          " \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@")
  "Regexp matching a unified diff hunk header.")

(defun my/org-jj--root ()
  "Return the root directory of the jj repository at `default-directory'."
  (file-name-as-directory (car (my/org-jj--lines "root"))))

(defun my/org-jj--next-match (regexp bound)
  "Return the start of the next match for REGEXP after point, before BOUND.
Return BOUND if there is no match. Do not move point."
  (save-excursion
    (if (re-search-forward regexp bound t)
        (match-beginning 0)
      bound)))

(defun my/org-jj--match-number (group default)
  "Return match GROUP as a number, or DEFAULT if it did not match."
  (if-let* ((s (match-string group)))
      (string-to-number s)
    default))

(defun my/org-jj--parse-hunk (end)
  "Parse the hunk whose header starts at point and ends before END.
Leave point at the end of the hunk."
  (looking-at my/org-jj--hunk-header-regexp)
  (let ((start (point))
        (old-start (my/org-jj--match-number 1 0))
        (old-lines (my/org-jj--match-number 2 1))
        (new-start (my/org-jj--match-number 3 0))
        (new-lines (my/org-jj--match-number 4 1)))
    (forward-line 1)
    (goto-char (my/org-jj--next-match my/org-jj--hunk-header-regexp end))
    (my/org-jj-hunk-create :old-start old-start :old-lines old-lines
                           :new-start new-start :new-lines new-lines
                           :text (buffer-substring-no-properties
                                  start (point)))))

(defun my/org-jj--diff-header-path (header)
  "Return the repository-relative path named in diff HEADER."
  (save-match-data
    (cond ((string-match "^\\+\\+\\+ b/\\(.*\\)$" header)
           (match-string 1 header))
          ((string-match "^--- a/\\(.*\\)$" header)
           (match-string 1 header))
          ((string-match "\\`diff --git a/.* b/\\(.*\\)$" header)
           (match-string 1 header)))))

(defun my/org-jj--parse-file-diff (end)
  "Parse the file diff that starts at point and ends before END.
Leave point at END."
  (let* ((header-end (my/org-jj--next-match my/org-jj--hunk-header-regexp end))
         (header (buffer-substring-no-properties (point) header-end))
         (hunks '()))
    (goto-char header-end)
    (while (< (point) end)
      (push (my/org-jj--parse-hunk end) hunks))
    (my/org-jj-file-diff-create :path (my/org-jj--diff-header-path header)
                                :header header
                                :hunks (nreverse hunks))))

(defun my/org-jj-parse-diff (text)
  "Parse git-format diff TEXT into a list of `my/org-jj-file-diff'."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((files '()))
      (while (re-search-forward "^diff --git " nil t)
        (goto-char (match-beginning 0))
        (let ((end (save-excursion
                     (forward-line 1)
                     (my/org-jj--next-match "^diff --git " (point-max)))))
          (push (my/org-jj--parse-file-diff end) files)))
      (nreverse files))))

(defun my/org-jj-hunk-overlaps-p (hunk lines)
  "Return non-nil if the new side of HUNK overlaps LINES, a list (START END).
A hunk that adds no lines counts as covering its new-side start line."
  (let* ((start (my/org-jj-hunk-new-start hunk))
         (end (+ start (max 0 (1- (my/org-jj-hunk-new-lines hunk))))))
    (and (<= start (cadr lines))
         (>= end (car lines)))))

(defun my/org-jj--file-diff-text (file-diff lines)
  "Return the text of FILE-DIFF, keeping only hunks that overlap LINES.
With LINES nil, return the whole diff. Otherwise return nil if no hunk
overlaps LINES."
  (let ((hunks (if lines
                   (seq-filter (lambda (hunk)
                                 (my/org-jj-hunk-overlaps-p hunk lines))
                               (my/org-jj-file-diff-hunks file-diff))
                 (my/org-jj-file-diff-hunks file-diff))))
    (when (or hunks (null lines))
      (concat (my/org-jj-file-diff-header file-diff)
              (mapconcat #'my/org-jj-hunk-text hunks "")))))

(defun my/org-jj--dblock-string (params key)
  "Return string parameter KEY of jj-diff block PARAMS, or nil if absent."
  (when-let* ((value (plist-get params key)))
    (if (stringp value)
        value
      (user-error "jj-diff block %s must be a quoted string, not %S"
                  key value))))

(defun my/org-jj--dblock-file (params)
  "Return the :file parameter of jj-diff block PARAMS."
  (or (my/org-jj--dblock-string params :file)
      (user-error "jj-diff block has no :file")))

(defun my/org-jj--dblock-lines (params)
  "Return the :lines parameter of jj-diff block PARAMS, or nil if absent."
  (when-let* ((lines (plist-get params :lines)))
    (if (and (proper-list-p lines)
             (length= lines 2)
             (seq-every-p #'natnump lines))
        lines
      (user-error "jj-diff block :lines must be (START END), not %S" lines))))

(defun my/org-jj--dblock-revision-args (params)
  "Return the \"jj diff\" revision arguments for jj-diff block PARAMS."
  (let ((from (my/org-jj--dblock-string params :from))
        (to (my/org-jj--dblock-string params :to))
        (rev (my/org-jj--dblock-string params :rev)))
    (cond ((and rev (not (or from to))) (list "-r" rev))
          ((and from to (not rev)) (list "--from" from "--to" to))
          (t (user-error "jj-diff block needs :from and :to, or :rev")))))

(defun my/org-jj--dblock-context-args (params)
  "Return the \"jj diff\" context arguments for jj-diff block PARAMS."
  (when-let* ((context (plist-get params :context)))
    (unless (natnump context)
      (user-error "jj-diff block :context must be a number, not %S" context))
    (list "--context" (number-to-string context))))

(defun my/org-jj--dblock-side-revision (params side)
  "Return the revision on SIDE of jj-diff block PARAMS.
SIDE is `old' or `new'."
  (let ((rev (my/org-jj--dblock-string params :rev)))
    (pcase side
      ('new (or rev (my/org-jj--dblock-string params :to)))
      ('old (if rev
                (format "(%s)-" rev)
              (my/org-jj--dblock-string params :from))))))

(defun my/org-jj--repo-directory ()
  "Return the directory to run jj in for the Org entry at point.
This is the inherited JJ_REPO property if set, otherwise
`default-directory'."
  (if-let* ((repo (org-entry-get nil "JJ_REPO" t)))
      (file-name-as-directory (expand-file-name repo))
    default-directory))

(defun my/org-jj--diff (&rest args)
  "Run \"jj diff --git\" with ARGS and return the parsed result.
See `my/org-jj-parse-diff'."
  (my/org-jj-parse-diff (apply #'my/org-jj--output "diff" "--git" args)))

(defun my/org-jj--dblock-content (params)
  "Return the content for a jj-diff block with PARAMS.
Run jj in `default-directory'."
  (let* ((file-diff (car (apply #'my/org-jj--diff
                                (append
                                 (my/org-jj--dblock-revision-args params)
                                 (my/org-jj--dblock-context-args params)
                                 (list "--" (format "root:%S"
                                                    (my/org-jj--dblock-file
                                                     params)))))))
         (text (and file-diff
                    (my/org-jj--file-diff-text
                     file-diff (my/org-jj--dblock-lines params)))))
    (if text
        (concat "#+begin_src diff\n" (org-escape-code-in-string text)
                "#+end_src")
      "jj-diff: no changes match this block.")))

(defun org-dblock-write:jj-diff (params)
  "Insert the diff for a jj-diff dynamic block with PARAMS.
See the commentary before `my/org-jj-hunk' in lib/my-org-jj.el for the
parameters. Errors are inserted into the block, so that updating all
blocks in a buffer does not stop at the first bad one."
  (let ((default-directory (my/org-jj--repo-directory)))
    (insert (condition-case err
                (my/org-jj--dblock-content params)
              (error (format "jj-diff: %s" (error-message-string err)))))))

(defun my/org-jj--dblock-at-begin ()
  "Return the jj-diff block whose #+BEGIN line is at point, or nil."
  (save-excursion
    (forward-line 0)
    (let ((case-fold-search t))
      (when (and (looking-at org-dblock-start-re)
                 (equal (match-string-no-properties 1) "jj-diff"))
        (my/org-jj-dblock-create
         :pos (point)
         :params (read (concat "(" (match-string-no-properties 3) ")"))
         :directory (my/org-jj--repo-directory))))))

(defun my/org-jj--dblocks ()
  "Return the jj-diff blocks in the current buffer, in buffer order."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search t)
            (blocks '()))
        (while (re-search-forward org-dblock-start-re nil t)
          (when-let* ((dblock (my/org-jj--dblock-at-begin)))
            (push dblock blocks)))
        (nreverse blocks)))))

(defun my/org-jj--dblock-at-point ()
  "Return the jj-diff block whose content contains point, or nil.
Return nil on the block's #+BEGIN and #+END lines."
  (save-excursion
    (forward-line 0)
    (let ((case-fold-search t))
      (unless (looking-at-p org-dblock-end-re)
        (and (re-search-backward "^[ \t]*#\\+\\(?:BEGIN\\|END\\):" nil t)
             (my/org-jj--dblock-at-begin))))))

(defun my/org-jj--dblock-shows-p (dblock file-diff hunk)
  "Return non-nil if DBLOCK shows HUNK of FILE-DIFF.
HUNK nil stands for a file diff without hunks, such as a binary file."
  (let ((params (my/org-jj-dblock-params dblock)))
    (and (equal (my/org-jj--dblock-file params)
                (my/org-jj-file-diff-path file-diff))
         (if-let* ((lines (my/org-jj--dblock-lines params)))
             (and hunk (my/org-jj-hunk-overlaps-p hunk lines))
           t))))

(defun my/org-jj--hunk-label (file-diff hunk)
  "Return a one-line description of HUNK of FILE-DIFF."
  (concat (my/org-jj-file-diff-path file-diff) " "
          (if hunk
              (car (split-string (my/org-jj-hunk-text hunk) "\n"))
            "(no hunks)")))

(defun my/org-jj--coverage-problems (dblocks)
  "Return descriptions of coverage problems for DBLOCKS.
All of DBLOCKS must have the same revisions and directory."
  (let* ((params (my/org-jj-dblock-params (car dblocks)))
         (default-directory (my/org-jj-dblock-directory (car dblocks)))
         (file-diffs (apply #'my/org-jj--diff
                            (my/org-jj--dblock-revision-args params)))
         (problems '()))
    (dolist (file-diff file-diffs)
      (dolist (hunk (or (my/org-jj-file-diff-hunks file-diff) '(nil)))
        (let ((count (seq-count (lambda (dblock)
                                  (my/org-jj--dblock-shows-p
                                   dblock file-diff hunk))
                                dblocks)))
          (unless (= count 1)
            (push (format "%s: %s"
                          (if (zerop count)
                              "not shown"
                            (format "shown %d times" count))
                          (my/org-jj--hunk-label file-diff hunk))
                  problems)))))
    (dolist (dblock dblocks)
      (unless (seq-some (lambda (file-diff)
                          (seq-some (lambda (hunk)
                                      (my/org-jj--dblock-shows-p
                                       dblock file-diff hunk))
                                    (or (my/org-jj-file-diff-hunks file-diff)
                                        '(nil))))
                        file-diffs)
        (push (format "block at line %d shows nothing"
                      (line-number-at-pos (my/org-jj-dblock-pos dblock) t))
              problems)))
    (nreverse problems)))

(defun my/org-jj--dblock-group-key (dblock)
  "Return the key that groups DBLOCK with blocks for the same diff."
  (cons (my/org-jj-dblock-directory dblock)
        (my/org-jj--dblock-revision-args (my/org-jj-dblock-params dblock))))

(defun my/org-jj-diff-coverage-report ()
  "Return a report of hunks not shown exactly once by this buffer's blocks.
Return nil if every hunk of the diffs is shown exactly once by the
jj-diff blocks in this buffer. Blocks are grouped by repository and
revisions. For each group, compare the whole diff between those
revisions with what the blocks show. Also report blocks that show
nothing."
  (let ((report '()))
    (pcase-dolist (`(,key . ,dblocks)
                   (seq-group-by #'my/org-jj--dblock-group-key
                                 (my/org-jj--dblocks)))
      (when-let* ((problems (my/org-jj--coverage-problems dblocks)))
        (push (concat (format "jj diff %s (in %s)\n"
                              (string-join (cdr key) " ") (car key))
                      (mapconcat (lambda (p) (concat "  " p "\n")) problems))
              report)))
    (when report
      (string-join (nreverse report) "\n"))))

(defun my/org-jj-diff-uncovered ()
  "Show hunks not shown exactly once by the jj-diff blocks in this buffer.
See `my/org-jj-diff-coverage-report'."
  (interactive)
  (if-let* ((report (my/org-jj-diff-coverage-report)))
      (with-current-buffer (get-buffer-create "*jj-diff coverage*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert report))
        (special-mode)
        (display-buffer (current-buffer)))
    (message "Every hunk is shown exactly once")))

(defun my/org-jj-diff-file-report (file &optional update)
  "Return the jj-diff coverage report for Org FILE, or nil if there is none.
With UPDATE non-nil, first update all dynamic blocks in FILE and save it.
Work in a temporary buffer, so that no buffer or window changes. Signal
an error if a buffer visits FILE with unsaved changes.

This is for use from outside Emacs, through emacsclient. See
`my/org-jj-diff-coverage-report' for the report."
  (let ((file (expand-file-name file)))
    (when-let* ((buf (find-buffer-visiting file)))
      (when (buffer-modified-p buf)
        (user-error "Buffer %s has unsaved changes" (buffer-name buf))))
    (with-temp-buffer
      (insert-file-contents file)
      (setq default-directory (file-name-directory file))
      (delay-mode-hooks (org-mode))
      (when update
        (org-update-all-dblocks)
        (write-region nil nil file))
      (my/org-jj-diff-coverage-report))))

(defun my/org-jj--diff-line-position (bound)
  "Return (SIDE . LINE) for the diff line at point.
SIDE is `new' for added and context lines and `old' for removed lines.
LINE is the line number on that side. Look back no further than BOUND
for the hunk header. Return nil if point is not in a hunk."
  (save-excursion
    (let ((target (line-beginning-position)))
      (end-of-line)
      (when (re-search-backward my/org-jj--hunk-header-regexp bound t)
        (let ((old (my/org-jj--match-number 1 0))
              (new (my/org-jj--match-number 3 0)))
          (if (= (point) target)
              (cons 'new (max 1 new))
            (forward-line 1)
            (while (< (point) target)
              (pcase (char-after)
                (?+ (cl-incf new))
                (?- (cl-incf old))
                (?\s (cl-incf old) (cl-incf new)))
              (forward-line 1))
            (if (eq (char-after) ?-)
                (cons 'old old)
              (cons 'new new))))))))

(defun my/org-jj-diff-visit ()
  "Show the file at the line for the diff line at point in a jj-diff block.
Added and context lines show the file at the new revision, and removed
lines at the old revision. See `my/org-jj-revision-buffer'."
  (interactive)
  (let* ((dblock (or (my/org-jj--dblock-at-point)
                     (user-error "Not in a jj-diff block")))
         (position (or (my/org-jj--diff-line-position
                        (my/org-jj-dblock-pos dblock))
                       (user-error "Not on a line of a diff hunk")))
         (params (my/org-jj-dblock-params dblock))
         (default-directory (my/org-jj-dblock-directory dblock))
         (file (expand-file-name (my/org-jj--dblock-file params)
                                 (my/org-jj--root))))
    (pop-to-buffer (my/org-jj-revision-buffer
                    file (my/org-jj--dblock-side-revision
                          params (car position))))
    (goto-char (point-min))
    (forward-line (1- (cdr position)))))

(defun my/org-jj-diff-open-at-point ()
  "Visit the diff line at point if it is in a jj-diff block.
For `org-open-at-point-functions'."
  (when (my/org-jj--dblock-at-point)
    (my/org-jj-diff-visit)
    t))

(defun my/org-jj--parse-diff-link (link)
  "Parse jj-diff LINK of the form PATH::FROM::TO into a list."
  (let ((parts (split-string link "::")))
    (unless (length= parts 3)
      (user-error "jj-diff link is not PATH::FROM::TO: %s" link))
    parts))

(defun my/org-jj-diff-link-open (link _arg)
  "Follow jj-diff LINK. See `my/org-jj--parse-diff-link' for its form."
  (pcase-let ((`(,path ,from ,to) (my/org-jj--parse-diff-link link)))
    (vc-diff-internal t (list 'JJ (list (expand-file-name path))) from to)))

;; Source language highlighting in Org "diff" source blocks, including
;; those written by jj-diff blocks.

(defun my/org-jj--diff-overlay-faces-to-text (limit)
  "Add the faces of `diff-mode' overlays between point and LIMIT to the text.
This is a font-lock matcher, run after the keywords of `diff-mode'.
`diff-mode' puts source language and refinement faces on overlays,
which Org does not copy from the buffer it fontifies source blocks in.
Always return nil, so font-lock does nothing else with this matcher."
  (let ((start (point)))
    (dolist (ol (overlays-in start limit))
      (when-let* (((memq (overlay-get ol 'diff-mode) '(syntax fine)))
                  (face (overlay-get ol 'face)))
        (add-face-text-property (max start (overlay-start ol))
                                (min limit (overlay-end ol))
                                face))))
  nil)

(define-derived-mode my/org-jj-src-diff-mode diff-mode "Diff"
  "Major mode for fontifying Org \"diff\" source blocks.
Highlight the text of each hunk using the major mode for its file name,
from the hunk alone, as in `diff-font-lock-syntax' `hunk-only'. Put
these faces in text properties, so that
`org-src-font-lock-fontify-block' copies them into the Org buffer."
  (setq-local diff-font-lock-syntax 'hunk-only)
  (setq-local font-lock-defaults
              (cons (append diff-font-lock-keywords
                            '((my/org-jj--diff-overlay-faces-to-text)))
                    (cdr font-lock-defaults))))

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("diff" . my/org-jj-src-diff)))

(with-eval-after-load 'org
  (add-hook 'org-open-at-point-functions #'my/org-jj-diff-open-at-point)
  ;; vc-diff-internal loads vc-jj when needed, so this link does not
  ;; wait for vc-jj like the "jj:" link does.
  (org-link-set-parameters "jj-diff" :follow #'my/org-jj-diff-link-open)
  (with-eval-after-load 'vc-jj
    (org-link-set-parameters "jj"
                             :follow #'my/org-jj-open
                             :store #'my/org-jj-store-link)))

(provide 'my-org-jj)
