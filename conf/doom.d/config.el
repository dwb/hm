;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; TODO
;; * binary search movement hydra?
;; * if switched-to project tab doesn't show a buffer from that project, show magit
;;   status instead.

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Dani Brown"
      user-mail-address "d@dani.cool")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-operandi)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(when-let ((d (seq-find #'file-exists-p '("~/Library/Mobile Documents/com~apple~CloudDocs/org/"
                                          "~/org/"))))
  (setq org-directory d))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Right Alt behaves as regular Option on macOS
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none))

;; Disprefer showing the same buffer in two windows in the same frame
(setq switch-to-prev-buffer-skip 'this)

(setq window-sides-vertical t)

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(setq history-delete-duplicates t)

(setq switch-to-buffer-obey-display-actions t)
(setf switch-to-buffer-in-dedicated-window 'pop)

(setq compilation-scroll-output 'first-error)
(setq compilation-auto-jump-to-first-error t)

(setq next-error-message-highlight t)
(setq completions-detailed t)
(setq help-enable-symbol-autoload t)

(setf treesit-font-lock-level 4)

(setq! evil-want-C-u-scroll nil
       evil-want-C-d-scroll nil)

(setf desktop-path (list (concat doom-local-dir "state/desktop")))
;; this is more annoying than useful tbh
(desktop-save-mode 0)

(global-visual-fill-column-mode 1)
(setq-default fill-column 100)

(setq-default term-prompt-regexp nil)

(setf confirm-kill-processes nil)

(defun my/save-all-file-buffers ()
  (save-some-buffers t))

(add-hook 'focus-out-hook #'my/save-all-file-buffers)

(rx-define my-file-name (+ (not ?/)))

(setf find-sibling-rules
      `(;; C
        (,(rx (group my-file-name) ".c" string-end)
         ,(rx (backref 1) ".h"))
        (,(rx (group my-file-name) ".h" string-end)
         ,(rx (backref 1) ".c"))
        ;; C++
        (,(rx (group my-file-name) ".cpp" string-end)
         ,(rx (backref 1) ".hpp"))
        (,(rx (group my-file-name) ".hpp" string-end)
         ,(rx (backref 1) ".cpp"))
        ;; Go
        (,(rx (group my-file-name) ".go" string-end)
         ,(rx (backref 1) "_test.go"))
        (,(rx (group my-file-name) "_test.go" string-end)
         ,(rx (backref 1) ".go"))))

;; doom tries to be clever but i think this is the cause of whitespace-mode turning on
;; when i don't want it to
(remove-hook 'after-change-major-mode-hook #'doom-highlight-non-default-indentation-h)

;; non-essential

(require 'cl-lib)
(eval-when-compile (require 'subr-x))

(eval-when-compile
  ;; Support map pattern in pcase
  (require 'map))

(require 's)

(setf vterm-compile-module-use-nix t)

(defun my/add-paths-from-my-env-to-exec-path ()
  "Parse the PATH variable from the output of `printenv` and add its paths to `exec-path`."
  (when-let* ((path-line (thread-last (shell-command-to-string "printenv")
                                      (s-lines)
                                      (seq-find (lambda (line) (string-prefix-p "PATH=" line)))))
              (path-value (substring path-line 5))
              (paths (split-string path-value path-separator)))
    (setenv "PATH" path-value)
    (dolist (path paths)
      (add-to-list 'exec-path path))))

(my/add-paths-from-my-env-to-exec-path)

(require 'ansi-color)
(defun my/colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/colorize-compilation-buffer)

(set-frame-name "dev")

;; Set font to the first good one
;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2022-12/msg00665.html
(ignore-errors
  (when-let ((monitors (car (display-monitor-attributes-list)))
             (mm-size (map-elt monitors 'mm-size))
             (geometry (map-elt monitors 'geometry))
             (wpix (nth 2 geometry))
             (wmm (nth 0 mm-size))
             (is-retina (>= (/ wpix wmm) 4))
             (goodfonts (list
                         ;; (font-spec :family "Fantasque Sans Mono"
                         ;;            :size (if is-retina 12.0 15.0))
                         (font-spec :family "Iosevka SS08" :weight 'medium
                                    :size (if is-retina 12.0 14.0))))
             (ffl (font-family-list))
             (matching-font (seq-find
                             #'(lambda (f) (member (symbol-name (font-get f :family))
                                                   ffl))
                             goodfonts)))
    (setf doom-font matching-font)))

(setf doom-big-font-increment 2)

;; macOS tries to set SSH_AUTH_SOCK to something helpful but it really isn't
;; turns out it is quite helpful
;; (when-let ((home    (getenv "HOME"))
;;            (newsock (concat (file-name-as-directory home) ".gnupg/S.gpg-agent.ssh")))
;;   (when (file-exists-p newsock) (setenv "SSH_AUTH_SOCK" newsock)))

;; add private lib dir to load-path
(add-to-list 'load-path (expand-file-name (concat doom-user-dir "lib")))

;; core emacs remaps
(map!
 :g "s-o" #'find-file

 :leader

 :desc "Rename visited file"
 "f R" #'rename-visited-file)

(load "my-core-ext.el")

(use-package! posframe)

;; add vendored/custom packages

(require 'fix-doom-popup)

;; (require 'my-eglot-fixes)
;; (my-eglot-fixes-mode)

(require 'my-nix)
(require 'go-ts)

;; (my/if-nix
;;  (my/gimme 'nodejs_20))

(require 'alfred)
(require 'rotate)

;; (require 'subproject)
;; enabled after projectile so that subprojects are looked at first

;; (require 'subprojects)
;; (require 'lsp-subprojects)
;; (require 'doom-subprojects)
(require 'my-buffer-mgmt)
;; needs add-node-modules-path:
;; (require 'yarn-project)
(require 'evil-little-word)

(require 'switch-to-buffer-history)

(require 'project-per-tab)

(require 'clique)
(require 'clique-doom)

;; (use-package! vundo
;;   :bind (("<f7>" . vundo))
;;   :config (progn
;;             (setq vundo-glyph-alist vundo-unicode-symbols)
;;             (map!
;;              :map vundo-mode-map
;;              "l" #'vundo-forward
;;              "h" #'vundo-backward
;;              "j" #'vundo-next
;;              "k" #'vundo-previous
;;              "<escape>" #'vundo-quit)))

(use-package evil-owl
  :config
  (setf evil-owl-display-method 'posframe
        evil-owl-extra-posframe-args '(:width 50 :height 20 :border-width 2 :border-color "#000000")
        evil-owl-max-string-length 50
        evil-owl-idle-delay 0.1)
  (evil-owl-mode))

(use-package! org-modern)

(use-package! flymake-popon)
(use-package! eldoc-box
  :config
  (eldoc-box-hover-at-point-mode 1)
  (after! eglot
    (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t)))

;; XXX: this is erroring and appears very unloved
;; (use-package! eslint-flymake
;;   :config
;;   (setf eslint-flymake-command '("yarn" "exec" "eslint")))

(use-package! gotest)

(use-package! treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; https://github.com/renzmann/treesit-auto/issues/127
  (delete 'glsl treesit-auto-langs)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; gptai
(if-let ((user  "d@dani.cool")
         (entry (car (auth-source-search :max 1
                                         :host "api.openai.com" :user user
                                         :port "https"
                                         :require '(:user :port :secret))))
         (secretf (plist-get entry :secret))
         (secret (if (functionp secretf) (funcall secretf) secretf)))
    (use-package! gptai
      :config
      (setf gptai-username user)
      (setf gptai-model "gpt-4o")
      (setf gptai-api-key secret)))

(use-package chatgpt-shell
  :config
  (add-hook! org-mode
    (with-demoted-errors "chatgpt-shell: %S"
      (require 'ob-chatgpt-shell)
      (ob-chatgpt-shell-setup)))
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (auth-source-pick-first-password :host "api.openai.com")))
   (chatgpt-shell-model-version "gpt-4")))

(use-package org-ai
  :ensure t
  :commands (org-ai-mode
             org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  (setq org-ai-default-chat-model "gpt-4")
  (org-ai-install-yasnippets))

(use-package! gptel
 :config
 (setq-default
  gptel-backend
  (gptel-make-ollama "Ollama"
                     :host "localhost:11434"
                     :models '("codellama:34b-instruct-q6_K"))))

(use-package! norns)
(use-package! combobulate)

(use-package! copilot
  :config
  (add-hook! go-ts-mode (copilot-mode))
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(use-package nushell-ts-mode)

(use-package unison-ts-mode)
(use-package unison-daemon)

;;;
;;; BETTER WINDOW SIZE PRESERVATION
;;;

(add-to-list 'window-persistent-parameters '(my/window-preserved-size-any-buffer . writable))

(defun my/window-preserve-size-any-buffer (&optional window horizontal preserve)
  "Preserve height of window WINDOW.
WINDOW must be a live window and defaults to the selected one.
Optional argument HORIZONTAL non-nil means preserve the width of
WINDOW.

PRESERVE t means to preserve the current height/width of WINDOW's
body in frame and window resizing operations whenever possible.
The height/width of WINDOW will change only if Emacs has no other
choice.  Resizing a window whose height/width is preserved never
throws an error.

PRESERVE nil means to stop preserving the height/width of WINDOW,
lifting the respective restraint induced by a previous call of
`window-preserve-size' for WINDOW.  Calling `enlarge-window',
`shrink-window', `split-window' or `fit-window-to-buffer' with
WINDOW as argument also removes the respective restraint.

Other values of PRESERVE are reserved for future use."
  (setq window (window-normalize-window window t))
  (let ((p (my/window-preserve-size-orig window horizontal preserve)))
    (set-window-parameter window 'my/window-preserved-size-any-buffer (cdr p))))

(defun my/set-window-preserve-size-any-buffer-data (window data)
  (setq window (window-normalize-window window t))
  (when (equal 3 (length data)) (setq data (cdr data)))
  (set-window-parameter window 'my/window-preserved-size-any-buffer data))

(defun my/window-stop-preserve-size-any-buffer (&optional window _horizontal _preserve)
  "just stop it ok"
  (setq window (window-normalize-window window t))
  (set-window-parameter window 'my/window-preserved-size-any-buffer nil))

(unless (fboundp 'my/window-preserve-size-orig)
  (defalias 'my/window-preserve-size-orig (symbol-function 'window-preserve-size)))

(defun my/window-preserve-size-with-sync (oldfun &optional window horizontal preserve)
  (let ((data (funcall oldfun window horizontal preserve)))
    (my/set-window-preserve-size-any-buffer-data window data)))

(advice-add 'window-preserve-size :around #'my/window-preserve-size-with-sync)

(defun my/window-buffer-change-preserve-size-new-buffer (&optional _frame)
  (let* ((window (selected-window))
         (data (window-parameter window 'my/window-preserved-size-any-buffer)))
    (when (and data (not (equal data '(nil nil))))
      (set-window-parameter window 'window-preserved-size (cons (current-buffer) data)))))

(add-hook 'window-buffer-change-functions #'my/window-buffer-change-preserve-size-new-buffer)

;; (defun fix-terminal-window ()
;;   (interactive)
;;   (display-buffer-in-side-window  (current-buffer) '((side . right)))
;;   ())

;;;
;;; FUNCTIONS
;;;

(defun my/switch-buffer (buffers)
  (let* ((items (mapcar #'buffer-name buffers))
         ;; (histbufs (seq-intersection buffer-name-history buffer-names))
         ;; (items (seq-uniq (append histbufs buffer-names)))
         (buffer (consult--read items
                                :state (consult--buffer-state)
                                :category 'buffer
                                :require-match (confirm-nonexistent-file-or-buffer)
                                :prompt "Switch to buffer: "
                                :history 'buffer-name-history
                                ;; :default (car items)
                                ;; :sort nil
                                )))
    ;; When the buffer does not belong to a source,
    ;; create a new buffer with the name.
    (funcall consult--buffer-display buffer)))

(defun my/vertico-switch-buffer-closest-cliques ()
  (interactive)
  (my/switch-buffer (clique-current-buffer-friends :having 'workspace)))

;; (defun my/switch-term ()
;;   (interactive)
;;   (my/switch-buffer (clique-current-buffer-friends :having '(workspace term))))

(defun my/window-undedicate (&optional win)
  (interactive)
  (set-window-dedicated-p win nil))

(defun my/reset-workspace ()
  (interactive)
  (let ((dir default-directory))
    (unless (magit-toplevel dir)
      (user-error "%s is not a magit repository" dir))
    (magit-status-setup-buffer dir)
    (thread-last
      (clique-current-buffer-friends :having '(tabproject file))
      (seq-filter #'buffer-live-p)
      (seq-do #'kill-buffer))))



(defun my/select-main-window ()
  (interactive)
  (if-let ((w (window-main-window)))
      (progn
        (evil-force-normal-state)
        (prog1
            (select-window w)
          (evil-force-normal-state)))
    (user-error! "No candidate windows")))

(map!
 :g "C-<escape>" #'my/select-main-window

 (:when IS-MAC
   :g "s-k" #'kill-this-buffer)

 :map evil-window-map
 ("<f6>" #'my/select-main-window))

(after! tab-bar
  (map!
   :desc "Switch tab"
   "s-;" #'tab-switch

   :leader
   :desc "Switch tab"
   ";" #'tab-switch))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; (use-package! flow-js2-mode)
;; (use-package! window-purpose)


(use-package! literate-calc-mode)

;; eagar-load vterm to load my mappings below
(use-package! vterm :ensure t)

(defun my/window-resize-to (window size &optional horizontal ignore pixelwise)
  (setq window (window-normalize-window window))
  (let* ((delta (- size (window-size window horizontal pixelwise))))
    (when (not (equal delta 0))
      (window-resize window delta horizontal ignore pixelwise))))

(defun my/window-resize-body-to (window body &optional horizontal ignore pixelwise)
  (setq window (window-normalize-window window))
  (let* ((current (if horizontal (window-body-width window pixelwise) (window-body-height window pixelwise)))
         (delta (- body current)))
    (when (not (equal delta 0))
      (window-resize window delta horizontal ignore pixelwise))))

(defun my/resize-any-buffer-size-preserved-window (&optional window)
  (setq window (window-normalize-window window))
  (let* ((dims (window-parameter window 'my/window-preserved-size-any-buffer)))
    (when dims
      (cl-destructuring-bind (h v) dims
        (when h
          (my/window-resize-to window h t nil t)
          (window-preserve-size window t t))
        (when v
          (my/window-resize-to window v nil nil t)
          (window-preserve-size window nil t))))))

(defun my/resize-any-buffer-size-preserved-windows (frame-or-window)
  (when (equal frame-or-window 'frame)
    (dolist (window (window-list nil 'never-minibuf))
      (my/resize-any-buffer-size-preserved-window window))))

(defun my/window-resize-standard-width (&optional window)
  "Sets WINDOW, which will be the selected window if nil, to the standard width"
  (interactive)
  (my/window-resize-body-to window 101 t))

(defun my/workspace-list-last-first ()
  (let ((ws (+workspace-list))
        (last +workspace--last))
    (if last
        (cons last (remove last ws))
      ws)))

(defun my/workspace/switch-to (index)
  "Switch to a workspace at a given INDEX. A negative number will start from the
end of the workspace list."
  (interactive
   (list (or current-prefix-arg
             (if (modulep! :completion ivy)
                 (ivy-read "Switch to workspace: "
                           (+workspace-list-names)
                           :caller #'+workspace/switch-to
                           :preselect +workspace--last)
               (completing-read "Switch to workspace: "
                                (+workspace-list-names)
                                nil nil nil nil
                                +workspace--last)))))
  (when (and (stringp index)
             (string-match-p "^[0-9]+$" index))
    (setq index (string-to-number index)))
  (condition-case-unless-debug ex
      (let ((names (+workspace-list-names))
            (old-name (+workspace-current-name)))
        (cond ((numberp index)
               (let ((dest (nth index names)))
                 (unless dest
                   (error "No workspace at #%s" (1+ index)))
                 (+workspace-switch dest)))
              ((stringp index)
               (+workspace-switch index t))
              (t
               (error "Not a valid index: %s" index)))
        (unless (called-interactively-p 'interactive)
          (if (equal (+workspace-current-name) old-name)
              (+workspace-message (format "Already in %s" old-name) 'warn)
            (+workspace/display))))
    ('error (+workspace-error (cadr ex) t))))


(after! compile
  ;; typescript project build support
  (setf compilation-error-regexp-alist-alist
        (assq-delete-all 'my-typescript-build compilation-error-regexp-alist-alist))
  (add-to-list 'compilation-error-regexp-alist-alist
               `(my-typescript-build
                 ,(rx (seq bol (submatch (+ (not (any ?: space)))) ":" (submatch (+ digit)) ":" (submatch (+ digit)) (+ space) "-" (+ space) "error"))
                 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'my-typescript-build))

(after! ediff
  ;; ediff uses switch-to-buffer all over the place so really doesn't work
  ;; with switch-to-buffer-obey-display-actions. here we advise all ediff commands
  ;; to override the option to nil for the duration.
  (mapatoms
   (lambda (sym)
     (when (and (commandp sym)
                (let ((sn (symbol-name sym)))
                  (or (equal "ediff" sn)
                      (string-prefix-p "ediff-" sn))))
       (let ((advice-name (intern (format "my/trad-switch-to-buffer/%s" sym))))
         (defalias advice-name
           (lambda (orig-fun &rest args)
             (let ((switch-to-buffer-obey-display-actions nil))
               (apply orig-fun args)))
           (format "Run %s with switch-to-buffer-obey-display-actions set to nil" sym))
         (advice-add sym :around advice-name))))))

(after! elisp-mode
  (defun my/eval-print-last-sexp-pp-advice (orig-fun &rest args)
    (let ((result (apply orig-fun args)))
      (if (not (equal result 'nil))
          (pp result)
        result)))
  (advice-add 'eval-print-last-sexp :around #'my/eval-print-last-sexp-pp-advice))

(after! persp-mode
  (setq persp-add-buffer-on-after-change-major-mode 'free)

  (persp-def-buffer-save/load
   :mode 'vterm-mode :tag-symbol 'def-vterm-buffer
   :save-vars '(major-mode default-directory))

  (add-hook 'persp-activated-functions #'my/resize-any-buffer-size-preserved-windows)

  (defun my/persp-add-current-buffer ()
    (persp-add-buffer (current-buffer) (get-current-persp) nil nil))

  (after! deadgrep
    (add-hook! deadgrep-mode #'my/persp-add-current-buffer))

  (map!
   :leader
   :desc "Switch workspace"
   ";" #'my/workspace/switch-to
   :desc "Switch to last workspace"
   "M-;" #'+workspace/other
   ))

(after! compile

  ;; create a new comint-derived major mode for compilation buffers that
  ;; sets read-only mode, word wrap mode, and forces evil normal state
  (define-derived-mode my/compilation-mode comint-mode "Compilation"
    "Major mode for compilation buffers."
    (setq-local truncate-lines nil)
    (setq-local word-wrap t)
    ;; (+word-wrap-mode 1)
    ;; turns out that setting read-only-mode is what stops regexes from working
    (read-only-mode 1)
    (evil-normal-state))

  ;; always use comint for compilation otherwise my error regexes don't work
  (defun my/compilation-start-args-advice (args)
    (cl-destructuring-bind (command &optional mode &rest r) args
      (if (not mode)
          (append (list command t) r)
        args)))
  (advice-add 'compilation-start :filter-args #'my/compilation-start-args-advice)

  (set-popup-rule!
    (rx string-start "*my/compilation*")
    :side 'right :width 83 :vslot -4 :select t :quit nil :ttl 0))

(after! evil
  (use-package! evil-textobj-tree-sitter :ensure t)

  ;; enable evil in minibuffer
  (setf evil-want-minibuffer t)
  (setf evil-undo-system 'undo-redo)

  (defmacro my/define-and-bind-text-object (key sym desc start-regex end-regex)
    (let ((inner-name (make-symbol (concat "my/evil-i-" sym)))
          (outer-name (make-symbol (concat "my/evil-a-" sym))))
      `(progn
         (evil-define-text-object ,inner-name (count &optional beg end type) ,desc
           (evil-select-paren ,start-regex ,end-regex beg end type count nil))
         (evil-define-text-object ,outer-name (count &optional beg end type) ,desc
           (evil-select-paren ,start-regex ,end-regex beg end type count t))
         (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
         (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

  (my/define-and-bind-text-object "l" "line" "Line" "^\\s-*" "\\s-*$")

  ;; still doesn't work, but more cleverly this time
  (evil-define-command my/evil-use-blackhole-register ()
    "Use blackhole register for the next command."
    :keep-visual t
    :repeat ignore
    :suppress-operator t
    (interactive)
    (setq evil-this-register "_"))

  ;; (defun my/evil-use-blackhole-register ()
  ;;   (evil-use-register "_"))

  (defun my/scroll-up-a-bit ()
    (interactive)
    (evil-scroll-line-up 20))

  (defun my/scroll-down-a-bit ()
    (interactive)
    (evil-scroll-line-down 20))

  (map!
   :desc "use blackhole register"
   :n "-" 'my/evil-use-blackhole-register

   :n "g 2" #'duplicate-dwim
   :n "g A" #'find-sibling-file
   :n "g z" #'exchange-point-and-mark

   :desc "ex"
   :n "RET" #'evil-ex

   :n "<prior>" #'my/scroll-up-a-bit
   :n "<next>" #'my/scroll-down-a-bit

   :desc "window"
   :g "<f6>" 'evil-window-map

   :mode Info-mode
   :n "m" #'Info-menu
   :n "f" #'link-hint-open-link
   :n "C-i" #'Info-history-forward))

(after! evil-surround
  (add-to-list 'evil-surround-pairs-alist '(?o . ("{|" . "|}"))))

(after! evil-markdown
  ;; don't clobber gj/gk, argh
  (evil-markdown-set-key-theme (remove 'navigation evil-markdown-key-theme)))

(after! evil-textobj-tree-sitter
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner")))

(after! org
  (setq! org-agenda-start-on-weekday t)
  (setq! org-blank-before-new-entry
         '((heading . t) (plain-list-item . nil)))

  (defun my/show-org ()
    (interactive)
    (if-let ((tab-index (tab-bar--tab-index-by-name "org")))
        (tab-bar-select-tab (1+ tab-index))
      (projectile-switch-project-by-name "org")))

  (defun my/org-mode-return ()
    "Add new list item, heading or table row with RET."
    (interactive)
    (if (org-in-item-p)
        (org-insert-item)
      (org-return)))

  (map!
   :mode org-mode

   :n "g k" #'evil-previous-visual-line
   :n "g j" #'evil-next-visual-line

   ;; these are actually quite annoying, let's think of something else
   ;; :n "h" #'org-up-element
   ;; :n "j" nil
   ;; :n "k" nil
   ;; :n "l" #'org-down-element

   :i "RET" #'my/org-mode-return
   ))

(after! window-purpose
  (add-to-list 'window-persistent-parameters '(purpose-dedicated . writable))

  (purpose-mode)
  (require 'window-purpose-x)
  (purpose-x-kill-setup)
  ;; (purpose-x-magit-single-off)
  (add-to-list 'purpose-user-mode-purposes '(vterm-mode . terminal))
  (add-to-list 'purpose-user-mode-purposes '(deadgrep-mode . search))
  (purpose-compile-user-configuration)

  (setq persp-window-state-put-function
        #'(lambda (pwc &optional frame rwin)
            (when (or rwin (setq rwin (frame-root-window
                                       (or frame (selected-frame)))))
              (when (fboundp 'window-state-put)
                (window-state-put pwc rwin 'safe)))))

  (defun my/vertico-switch-workspace-buffer-with-purpose ()
    "Switch to another buffer in the same workspace.

Use consult narrowing with another workspace number to open a buffer from that workspace
 BUG but it opens it in the current workspace (ivy also does this, but who cares)"
    (interactive)
    (when-let ((purpose (purpose-buffer-purpose (current-buffer)))
               (ws-buffers (+workspace-buffer-list (+workspace-current)))
               (buffers
                (seq-filter
                 (lambda (b) (eq purpose
                                 (purpose-buffer-purpose b)))
                 ws-buffers))
               (items (mapcar #'buffer-name buffers))
               (buffer (consult--read items
                                      :state (consult--buffer-state)
                                      :category 'buffer
                                      :require-match
                                      (confirm-nonexistent-file-or-buffer)
                                      :prompt (format "Switch to buffer (%s:%s): "
                                                      (+workspace-current-name) purpose)
                                      :history 'consult--buffer-history
                                      :default (cdr (seq-intersection consult--buffer-history items))
                                      :sort nil)))
      ;; When the buffer does not belong to a source,
      ;; create a new buffer with the name.
      (funcall consult--buffer-display buffer)))

  (defun my/ivy-switch-buffer-with-purpose (workspace other)
    (let* ((current (not other))
           (purpose (purpose-buffer-purpose (current-buffer)))
           (make-filter (lambda (f)
                          (lambda (b)
                            (let ((buffer (car b)))
                              (when (stringp buffer)
                                (setq buffer (get-buffer buffer)))
                              (and
                               (funcall f b)
                               (eq purpose (purpose-buffer-purpose buffer)))))))
           prompt action filter update unwind)
      (cond ((and workspace current)
             (setq prompt (format "Switch to workspace %s buffer: " purpose)
                   action #'ivy--switch-buffer-action
                   filter (funcall make-filter #'+ivy--is-workspace-other-buffer-p)))
            (workspace
             (setq prompt (format "Switch to workspace %s buffer in other window: " purpose)
                   action #'ivy--switch-buffer-other-window-action
                   filter (funcall make-filter #'+ivy--is-workspace-buffer-p)))
            (current
             (setq prompt (format "Switch to %s buffer: " purpose)
                   action #'ivy--switch-buffer-action))
            ((setq prompt (format "Switch to %s buffer in other window: " purpose)
                   action #'ivy--switch-buffer-other-window-action)))
      (when +ivy-buffer-preview
        (cond ((not (and ivy-use-virtual-buffers
                         (eq +ivy-buffer-preview 'everything)))
               (setq update #'+ivy--switch-buffer-preview
                     unwind #'+ivy--switch-buffer-unwind))
              ((setq update #'+ivy--switch-buffer-preview-all
                     unwind #'+ivy--switch-buffer-unwind))))
      (ivy-read prompt 'internal-complete-buffer
                :action action
                :predicate filter
                :update-fn update
                :unwind unwind
                :preselect (buffer-name (other-buffer (current-buffer)))
                :matcher #'ivy--switch-buffer-matcher
                :keymap ivy-switch-buffer-map
                ;; NOTE A clever disguise, needed for virtual buffers.
                :caller #'ivy-switch-buffer)))

  (defun my/current-workspace-buffers-purpose-source (purpose)
    `(:name ,(+workspace-current-name)
      :state    #'consult--buffer-state
      :items ,(lambda ()
                (when-let ((ws-buffers (+workspace-buffer-list (+workspace-current)))
                           (buffers
                            (seq-filter
                             (lambda (b) (eq purpose
                                             (purpose-buffer-purpose b)))
                             ws-buffers))))
                (mapcar #'buffer-name buffers))))

  (defun my/ivy-switch-workspace-buffer-with-purpose (&optional arg)
    "Switch to another buffer within the current workspace, with the same purpose as the current buffer.

If ARG (universal argument), open selection in other-window."
    (interactive "P")
    (my/ivy-switch-buffer-with-purpose t arg))

  (map!
   :leader

   (:when (modulep! :completion ivy)
    (:after ivy
     :desc "Switch workspace purpose buffer"
     "," #'my/ivy-switch-workspace-buffer-with-purpose

     (:prefix "b"
      :desc "Switch workspace purpose buffer"
      "b" #'my/ivy-switch-workspace-buffer-with-purpose
      :desc "Switch workspace buffer"
      "w" #'persp-switch-to-buffer)))

   (:when (modulep! :completion vertico)
    (:after vertico
     :desc "Switch workspace purpose buffer"
     "," #'my/vertico-switch-workspace-buffer-with-purpose
     ))

   (:prefix "b"
    :after my-buffer-mgmt
    :desc "Kill other workspace buffers of purpose"
    "K" #'my/kill-other-workspace-buffers-of-purpose)

   (:prefix "w"
    (:prefix (";" . "purpose")
     :desc "Dedicate window to purpose"
     "p" #'purpose-toggle-window-purpose-dedicated
     :desc "Dedicate window to buffer"
     "b" #'purpose-toggle-window-buffer-dedicated))))

(after! deadgrep
  (setq deadgrep-display-buffer-function #'display-buffer))

(after! switch-to-buffer-history
  (switch-to-buffer-history-mode 1))

(after! project
  (defun my/project-try-vc-without-home (dir)
    (not (equal (file-truename dir) (file-truename "~/"))))

  ;; TODO: more general way of ignoring home when finding project
  (advice-add 'project-try-vc :before-while #'my/project-try-vc-without-home))

(after! projectile
  (setf projectile-enable-caching t)
  (setf projectile-file-exists-local-cache-expire (* 5 60))

  (after! subproject
    (add-to-list 'project-find-functions #'subproject-find))
  (add-to-list 'projectile-ignored-projects "~/"))

(after! whitespace
  (map!
   :leader
   :desc "Toggle showing whitespace characters"
   "t W" #'whitespace-mode))


(after! vterm
  (add-to-list 'vterm-eval-cmds '("find-file" find-file-other-window))
  (add-hook! vterm-mode #'my/window-undedicate)

  (defun my/project-vterm ()
    "Open a vterm in the current project."
    (interactive)
    (my/project-vterm-named ""))

  (defun my/vterm-buffer-name (project &optional name)
    (let* ((suffix (if (and name (length> name 0)) (format " %s" name) ""))
           (pn (doom-project-name (project-root project))))
      (format "*vterm %s%s*" pn suffix)))

  (defun my/project-vterm-named (arg)
    "Open a vterm in the current project with a particular name."
    (interactive "MTerminal name: ")
    (let ((vterm-buffer-name (my/vterm-buffer-name (project-current) arg)))
      (vterm)))

  (defun my/rename-vterm-buffer (arg)
    "Rename a vterm buffer to a nice pattern"
    (interactive "MNew name: ")
    (when (not (eq major-mode 'vterm-mode)) (error "not a vterm buffer"))
    (let ((name arg))
      (rename-buffer (my/vterm-buffer-name (project-current) name))))

  (defun my/set-vterm-width ()
    (interactive)
    (my/window-resize-standard-width)
    (my/window-preserve-size-any-buffer nil t t))

  (defun my/vterm-send-c-w ()
    (interactive)
    (vterm-send-key "C-w"))

  (defun my/vterm-send-c-x ()
    (interactive)
    (vterm-send-key "C-x"))

  (map!
   :unless (modulep! :ui workspaces)
   (:leader
    :desc "Open project vterm"
    "o t" #'my/project-vterm)

   :map vterm-mode-map

   :desc "New vterm"
   "C-c n" #'my/project-vterm-named

   :desc "Send Esc"
   :i    "<escape>" #'vterm-send-escape
   :desc "Send Esc"
   :i "C-c C-e" #'vterm-send-escape

   :desc "Normal state"
   "C-c <escape>" #'evil-force-normal-state

   :desc "Find URL"
   "C-c l" #'link-hint-open-link

   :desc "Send C-x"
   :i "C-c C-x" #'my/vterm-send-c-x

   :desc "Send C-w"
   :i "C-c C-w" #'my/vterm-send-c-w
   :desc "window"
   :i "C-c w" 'evil-window-map
   :desc "window"
   :i "C-w" 'evil-window-map

   :desc "Switch buffer"
   :i "C-c ," #'switch-to-buffer

   :desc "Rename buffer"
   "C-c r" #'my/rename-vterm-buffer

   :desc "Clear"
   :i "C-c C-l" #'vterm-clear-scrollback

   :desc "Set standard width"
   "C-c s" #'my/set-vterm-width)

  (defun my/vterm-reenable-mode-line ()
    (hide-mode-line-mode -1)
    (when (fboundp 'doom-modeline-mode)
      (doom-modeline-mode))
    (+word-wrap-mode -1))

  (add-hook! vterm-mode :append #'my/vterm-reenable-mode-line)

  (after! consult
    (setf my/consult-source-vterm-buffer
          `(:name     "vterm Buffer"
            :narrow   ?t
            :default t
            :category buffer
            :face     consult-buffer
            :history  buffer-name-history
            :state    ,#'consult--buffer-state
            :items
            ,(lambda () (consult--buffer-query :sort 'visibility
                                               :as #'buffer-name
                                               :mode 'vterm-mode
                                               :predicate
                                               (lambda (buf)
                                                 (seq-find #'(lambda (pb) (eq (car pb) buf))
                                                           (window-prev-buffers)))))))

    (defun my/consult-term-buffer ()
      (interactive)
      (consult-buffer `(my/consult-source-vterm-buffer ,@consult-buffer-sources)))

    (map!
     :map vterm-mode-map

     :desc "Switch buffer"
     :i "C-c ," #'my/consult-term-buffer)))

(after! doom-modeline
  (setq doom-modeline-persp-name t)

  (after! window-purpose
    (doom-modeline-def-segment purpose
      "Shows the window's purpose from window-purpose"
      (let ((face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)))
        (concat
         (propertize (purpose--modeline-string)
                     'face face)
         (doom-modeline-spc))))

    (doom-modeline-def-modeline 'my-main
      '(bar workspace-name window-number modals
            matches buffer-info remote-host buffer-position word-count parrot
            selection-info)
      '(objed-state misc-info persp-name purpose battery grip irc mu4e gnus
            github debug repl lsp minor-modes input-method indent-info buffer-encoding
            major-mode process vcs checker))
    (defun my/setup-custom-doom-modeline ()
     (doom-modeline-set-modeline 'my-main 'default))
    (add-hook 'doom-modeline-mode-hook 'my/setup-custom-doom-modeline)))

(after! rotate

  (map!
   :leader
   :desc "Rotate windows"
   "w }" #'rotate-window))

(after! magit
  ;; doom sets this to #'+magit-display-buffer-fn, which is a nicer behaviour
  ;; but causes infinite recursion when window-purpose is loaded, for some reason.
  ;; turn on (setq purpose-message-on-p t) to see this in action.
  ;; TODO: investigate this to get the nicer behaviour without the bug.
  (after! window-purpose
    (setq magit-display-buffer-function #'magit-display-buffer-traditional))

  (setq magit-list-refs-sortby "-committerdate")

  (setq git-commit-style-convention-checks
        (cl-remove 'overlong-summary-line git-commit-style-convention-checks))

  (defun my/magit-get-fork-point (from-rev)
    (let ((buf (generate-new-buffer " *temp my/magit/git-merge-base*" t)))
      (unwind-protect (when (eql 0
                                 (magit-process-git buf
                                                    (list "merge-base" "--fork-point" from-rev)))
                        (with-current-buffer buf
                          (goto-char 0)
                          (current-word)))
        (kill-buffer buf))))

  (defun my/magit-diff-fork-point (rev &optional args files)
    "Show differences between commit and its fork point compared to given revision.

TODO: update following doc

REV-OR-RANGE should be a range or a single revision.  If it is a
revision, then show changes in the working tree relative to that
revision.  If it is a range, but one side is omitted, then show
changes relative to `HEAD'.

If the region is active, use the revisions on the first and last
line of the region as the two sides of the range.  With a prefix
argument, instead of diffing the revisions, choose a revision to
view changes along, starting at the common ancestor of both
revisions (i.e., use a \"...\" range)."
    (interactive (cons (magit-read-branch-or-commit "Diff against fork point from")
                       (magit-diff-arguments)))

    (if-let ((fprev (my/magit-get-fork-point rev)))
        (magit-diff-setup-buffer fprev nil args files)
      (user-error "No fork point found")))

  ;; this might be slow
  ;; (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

  (defun my/magit-last-few-commit-messages ()
    (let ((buf (generate-new-buffer " *temp my/magit/last-few-commit-messages*" t)))
      (unwind-protect (when (eql 0
                                 (magit-process-git buf
                                                    (list "log"
                                                    "--walk-reflogs"
                                                    "-n" "50"
                                                    "--format=%s")))
                        (with-current-buffer buf
                          (goto-char (point-max))
                          (move-beginning-of-line nil)
                          (let ((out) (done))
                            (while (not done)
                              (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                                          (line-end-position))))
                                (unless (or (string= line "") (string= line (car out)))
                                  (push line out)))
                              (when (bobp) (setf done t))
                              (forward-line -1))
                            out)))
        (kill-buffer buf))))

  (defvar my/magit-commit-history nil)

  (defun my/magit-stage-all-and-commit (message)
    (interactive
     (list (let ((my/magit-commit-history (my/magit-last-few-commit-messages)))
             (read-from-minibuffer "Commit message: "
                                   nil nil nil
                                   'my/magit-commit-history))))
    (let ((magit-no-confirm t))
      (magit-stage-modified nil))
    (magit-commit-create (list "-m" message))
    (when (fboundp 'git-gutter:update-all-windows)
      (git-gutter:update-all-windows)))

  (map!
   :n "<f5>" #'magit-dispatch

   :map magit-status-mode-map
   (:g "s-r" #'magit-refresh)
   (:n "h" #'magit-section-up)
   (:n "j" #'magit-section-forward)
   (:n "k" #'magit-section-backward)

   :map magit-process-mode-map
   (:n "x" #'magit-process-kill)

   :leader
   (:desc "Magit push"
    "g P" #'magit-push
    :desc "Magit stage all and commit"
    "g C-c" #'my/magit-stage-all-and-commit)))

(after! forge
  (setq forge-topic-list-limit '(1000 . -1)))

(after! lsp-mode
  (setq lsp-idle-delay 1.0)
  (setq-default lsp-enable-file-watchers nil) ;; This causes problems with large repos, but means changing branches is worse
  (setq-default lsp-go-directory-filters ["-vendor" "-manifests"])

  (lsp-register-custom-settings
   '(("gopls.memoryMode" "DegradeClosed")
     ("gopls.staticcheck" t t)
     ("gopls.usePlaceholders" t t)
     ("gopls.analyses.fieldalignment" t t)
     ("gopls.analyses.nilness" t t)
     ("gopls.analyses.shadow" t t)
     ("gopls.analyses.unusedparams" t t)
     ("gopls.analyses.unusedwrite" t t)
     ("eslint.useESLintClass" t t)))

  (lsp-subprojects-mode))

(after! eglot
  (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server" "--lsp")))
  (add-to-list 'eglot-server-programs '(lua-mode . ("lua-language-server")))
  (add-to-list 'eglot-server-programs '(swift-mode . ("xcrun" "sourcekit-lsp")))
  (add-to-list 'eglot-server-programs '(nix-mode . ("nixd")))
  (add-to-list 'eglot-server-programs '(nushell-ts-mode . ("nu" "--lsp")))
  (add-to-list 'eglot-server-programs '(unison-ts-mode "127.0.0.1" 5757))

  ;; eglot clobbers these, but we want to keep, for example, eslint-flycheck
  (add-to-list 'eglot-stay-out-of 'flymake-diagnostic-functions)

  (setq-default eglot-workspace-configuration
                '((:gopls .
                   ((staticcheck . t)
                    (matcher . "CaseSensitive")))))

  (defun my/eglot-setup-flymake ()
    (if (eglot-managed-p)
        (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t)
      (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend t)))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-setup-flymake)

  (add-hook! 'before-save-hook (when (eglot-managed-p) (eglot-format-buffer)))

  (after! treesit
    (add-to-list 'eglot-server-programs '(go-ts-mode "gopls"))
    (put 'go-ts-mode 'eglot-language-id "go")

    (add-hook! go-ts-mode #'eglot-ensure)
    (add-hook! go-ts-mode (add-hook 'before-save-hook 'eglot-format-buffer -80 t))

    (add-to-list 'eglot-server-programs '(typescript-ts-mode "typescript-language-server" "--stdio"))
    (add-to-list 'eglot-server-programs '(tsx-ts-mode "typescript-language-server" "--stdio"))
    (put 'typescript-ts-mode 'eglot-language-id "typescript")
    (put 'tsx-ts-mode 'eglot-language-id "typescriptreact")

    (add-hook! typescript-ts-mode #'eglot-ensure)
    (add-hook! tsx-ts-mode #'eglot-ensure)

    (when nil
      (dolist (p major-mode-remap-alist)
        (when-let ((mode (car p))
                   (tsmode (cdr p))
                   (eglotdef (eglot--lookup-mode mode))
                   (definedmodes (car eglotdef))
                   (svr (cdr eglotdef)))
          (unless (and (listp definedmodes) (memq tsmode definedmodes))
            (add-to-list 'eglot-server-programs `(,tsmode . ,svr))))))))

(after! lsp-javascript

  (defun my/lsp-typescript-javascript-tsx-jsx-activate-p (filename &optional _)
    "Don't activate for .js(x) files"
    (not (string-match-p "\\.mjs\\|\\.jsx?\\'" filename)))

  (defun my/lsp-clients-flow-activate-p (_file-name &optional _mode)
    "Don't activate flow for typescript files!!"
    (not (or (derived-mode-p 'typescript-mode) (derived-mode-p 'typescript-tsx-mode))))

  (advice-add 'lsp-typescript-javascript-tsx-jsx-activate-p :after-while #'my/lsp-typescript-javascript-tsx-jsx-activate-p)
  (advice-add 'lsp-clients-flow-activate-p :after-while #'my/lsp-clients-flow-activate-p))

(after! go-mode
  (add-hook 'before-save-hook 'gofmt-before-save))

(after! hl-line
  ;; only highlight current line in active window
  (setq hl-line-sticky-flag nil))

(after! deadgrep
  (map! :map deadgrep-mode-map
        :n
        "g r" 'deadgrep-restart
        "[" 'deadgrep-backward-match
        "]" 'deadgrep-forward-match))

(after! prettier
  (defun my/prettier-ignore-buffer ()
    (let ((hasconfig (when-let ((fn (buffer-file-name)))
                       (seq-some #'(lambda (cfn) (locate-dominating-file fn cfn))
                                 '(".prettierrc" ".prettierrc.json"
                                   ".prettierrc.yml" ".prettierrc.yaml"
                                   ".prettierrc.json5" ".prettierrc.js"
                                   ".prettierrc.cjs" "prettier.config.js"
                                   "prettier.config.cjs" ".prettierrc.toml")))))
      (or (prettier--in-node-modules-p) (not hasconfig))))

  (add-to-list 'prettier-major-mode-parsers '(typescript-ts-mode typescript))
  (add-to-list 'prettier-major-mode-parsers '(tsx-ts-mode typescript))

  (setf prettier-mode-ignore-buffer-function #'my/prettier-ignore-buffer)

  (global-prettier-mode 1)


  (add-to-list 'prettier-major-mode-parsers '(typescript-tsx-mode typescript babel-ts)))

(after! ace-window
  (setq aw-dispatch-always t))

(after! rustic
  (setq rustic-format-on-save t))


(after! literate-calc-mode
  (add-to-list 'auto-mode-alist '("\\.calc\\'" . literate-calc-mode))
  (add-hook! literate-calc-mode (text-scale-increase 5)))

(after! clique
  (after! project-per-tab
    (defclique tabproject
      :pred #'(lambda ()
                (when-let ((p (project-current))
                           (bp (if (featurep 'subproject) (subproject-parent-or-self p) p))
                           (tp (project-per-tab-project-of-tab)))
                  (equal bp tp)))))

  (map!
   :leader
   (:prefix "b"
    :desc "Reset workspace"
    "K" #'my/reset-workspace)))


(after! dired
  (setf dired-kill-when-opening-new-dired-buffer t)

  (after! imenu
    (defun my/dired-imenu-prev-index-position ()
      "Find the previous file in the buffer."
      (dired-previous-line 1))

    (defun my/dired-imenu-extract-index-name ()
      "Return the name of the file at point."
      (dired-get-filename 'verbatim))

    (defun my/dired-setup-imenu ()
      "Configure imenu for the current dired buffer."
      (set (make-local-variable 'imenu-prev-index-position-function)
           'my/dired-imenu-prev-index-position)
      (set (make-local-variable 'imenu-extract-index-name-function)
           'my/dired-imenu-extract-index-name))

    (add-hook! dired-mode 'my/dired-setup-imenu)))

(after! yarn-project
  (yarn-project-mode 1))

(after! Info-mode
  (map! :mode Info-mode
        (:n "f" #'link-hint-open-link)))

(after! helpful
  (defun my/undedicate-helpful ()
    (when (eq major-mode 'helpful-mode)
      (set-window-dedicated-p nil nil)))

  (when (modulep! :ui popup)
    (unless (boundp '+popup-create-window-hook)
      (setf +popup-create-window-hook '()))
    (add-to-list '+popup-create-window-hook #'my/undedicate-helpful))

  (map! :mode helpful-mode
        (:n "f" #'link-hint-open-link
         :n "C-o" #'previous-buffer)))

(after! evil-little-word
  (map!
   :m "M-W" #'evil-forward-little-word-begin
   :m "M-b" #'evil-backward-little-word-begin
   :m "M-w" #'evil-forward-little-word-end
   :m "M-B" #'evil-backward-little-word-end
   :textobj "M-w" #'evil-a-little-word #'evil-inner-little-word))

(after! consult
  (defun consult--buffer-preview ()
    "Buffer preview function."
    (let ((orig-buf (current-buffer)) (win-prev-bufs 'unset) other-win)
      (lambda (action cand)
        (when (eq action 'preview)
          (when (and (eq consult--buffer-display #'switch-to-buffer-other-window)
                     (not other-win))
            (switch-to-buffer-other-window orig-buf)
            (setq other-win (selected-window)))
          (let ((win (or other-win (selected-window)))
                (switch-to-buffer-obey-display-actions))
            (when (window-live-p win)
              (with-selected-window win
                (cond
                 ((and cand (get-buffer cand))
                  (when (eq win-prev-bufs 'unset)
                    (setq win-prev-bufs (window-prev-buffers)))
                  (switch-to-buffer cand 'norecord))
                 ((buffer-live-p orig-buf)
                  (switch-to-buffer orig-buf 'norecord)
                  (when (not (eq win-prev-bufs 'unset))
                    (set-window-prev-buffers win win-prev-bufs))))))))))))

(after! vertico
  (defun my/vertico-config-experiments ()
    (vertico-multiform-mode)

    (setq vertico-multiform-commands
          '((consult-grep buffer indexed)))

    (setq vertico-multiform-categories
          '((consult-grep buffer)))))

(after! lispy
  ;; fix where square brackets don't insert themselves in insert mode
  (map!
   :mode lispy-mode
   ;; XXX: i really just want the regular evil [/] submap here
   ;; :n "[" 'lispy-backward
   ;; :n "]" 'lispy-forward
   :i "[" 'lispy-open-square
   :i "]" 'lispy-close-square))

(after! lispyville
  (setf lispyville-key-theme
        '((operators normal)
          c-w
          (prettify insert)
          (atom-movement t)
          slurp/barf-cp
          additional
          additional-motions
          ;; text-objects ;; these appear everywhere, fucksake
          commentary
          (escape insert)
          additional-insert))
  (lispyville-set-key-theme))

(after! doom-subprojects
  (map! :leader :desc "subprojects" "p SPC" subprojects-doom-map))

(after! xwidget
  (cl-defun my/xwidget-webkit-new-session (url &key name prefix title-name-pattern callback)
    "Create a new webkit session buffer with URL."
    (let*
        ((bufname (or name (generate-new-buffer-name (or prefix "*xwidget-webkit*"))))
         (callback (or callback (if title-name-pattern
                                    (lambda (xwidget xwidget-event-type)
                                      (xwidget-webkit-callback xwidget xwidget-event-type)
                                      (when (and (buffer-live-p (xwidget-buffer xwidget))
                                                 (eq xwidget-event-type 'load-changed))
                                        (let ((title (xwidget-webkit-title xwidget)))
                                          (rename-buffer (format title-name-pattern title) t))))
                                  #'xwidget-webkit-callback)))
         xw)
      (with-current-buffer (get-buffer-create bufname)
        (setq xwidget-webkit-last-session-buffer (current-buffer))
        ;; The xwidget id is stored in a text property, so we need to have
        ;; at least character in this buffer.
        ;; Insert invisible url, good default for next `g' to browse url.
        (let ((start (point)))
          (insert url)
          (put-text-property start (+ start (length url)) 'invisible t)
          (setq xw (xwidget-insert
                    start 'webkit bufname
                    (xwidget-window-inside-pixel-width (selected-window))
                    (xwidget-window-inside-pixel-height (selected-window)))))
        (xwidget-put xw 'callback callback)
        (xwidget-webkit-mode)
        (xwidget-webkit-goto-uri (xwidget-webkit-last-session) url)
        (current-buffer)))))

(after! dash-docs
  (setf dash-docs-browser-func #'eww)

  (when (modulep! 'xwidget-internal)
    (require 'xwidget)
    (defun my/dash-docs-xwidget-browse (url)
      (display-buffer (my/xwidget-webkit-new-session url
                                                     :name "*dash-docs*"
                                                     :title-name-pattern "*dash-docs: %s *")))
    (setf dash-docs-browser-func #'my/dash-docs-xwidget-browse))

  (set-popup-rule!
    (rx string-start "*dash-docs")
    :side 'bottom :height 0.4 :select t :ttl 0))

(after! treesit
  (setf treesit-language-source-alist
        '((css "https://github.com/tree-sitter/tree-sitter-css")
          (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
          (nu . ("https://github.com/nushell/tree-sitter-nu" "main" "src"))
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (swift "https://github.com/alex-pinkus/tree-sitter-swift")
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (lua "https://github.com/tjdevries/tree-sitter-lua")
          (unison "https://github.com/kylegoetz/tree-sitter-unison")))

  (dolist (s treesit-language-source-alist)
    (unless (treesit-language-available-p (car s))
      (treesit-install-language-grammar (car s))))

  (defun my/treesit-update-all ()
    (interactive)
    (dolist (l treesit-language-source-alist)
      (cl-destructuring-bind (lang &rest _) l
        (treesit-install-language-grammar lang))))

  (dolist (m '((python-mode . python-ts-mode)
               (go-mode . go-ts-mode)
               (css-mode . css-ts-mode)
               (typescript-mode . typescript-ts-mode)
               (typescript-tsx-mode . tsx-ts-mode) ; for the benefit of the reverse mapping
               (js-mode . js-ts-mode)
               (css-mode . css-ts-mode)
               (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist m))

  (after! go-ts (my/setup-go-ts-mode)))

(after! flymake
  (after! consult
    (map!
     :leader
     :prefix ("d" . "diagnostics")
     :desc "all diagnostics" "d" #'consult-flymake))

  (after! treesit
    (add-hook 'typescript-ts-base-mode-hook #'flymake-mode))

  (defun my/next-error-with-flymake (orig &rest args)
    (interactive)
    (if (not flymake-mode)
        (apply orig args)
      (call-interactively #'flymake-goto-next-error)))

  (defun my/previous-error-with-flymake (orig &rest args)
    (interactive)
    (if (not flymake-mode)
        (apply orig args)
      (call-interactively #'flymake-goto-prev-error)))

  (advice-add 'next-error :around #'my/next-error-with-flymake)
  (advice-add 'previous-error :around #'my/previous-error-with-flymake)

  (map!
   :leader
   :prefix ("d". "diagnostics")
   :desc "buffer diagnositics" "b" #'flymake-show-buffer-diagnostics
   :desc "project diagnositics" "p" #'flymake-show-project-diagnostics))

(after! flymake-popon
  (global-flymake-popon-mode))

(after! tab-bar
  (tab-bar-mode)
  (tab-rename "home"))

(after! avy
  (after! embark
    (defun avy-action-embark (pt)
      (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0))))
      t)

    (setf (alist-get ?\; avy-dispatch-alist) 'avy-action-embark)))

(after! evil-easymotion
  (map!
   :n "s-<up>" #'evilem-motion-previous-line
   :n "s-<down>" #'evilem-motion-next-line))

(after! calc
  (map!
   :leader
   :desc "calculator"
   "o c" #'calc))

;;; doom modules config


(when (modulep! :ui popup)
  (defun my/disable-popup-close-on-esc ()
    (remove-hook 'doom-escape-hook #'+popup-close-on-escape-h))

  (add-hook! +popup-mode :append #'my/disable-popup-close-on-esc)

  ;; disable modeline management by popup module
  (remove-hook '+popup-buffer-mode-hook #'+popup-set-modeline-on-enable-h)

  (setq +popup-defaults
        (list :side   'bottom
              :height #'+popup-shrink-to-fit
              :width  83
              :quit   t
              :modeline t
              :select t
              :ttl    5)))

(when (modulep! :editor word-wrap)
  (add-hook! (prog-mode text-mode) #'+word-wrap-mode))

(after! vterm

  (set-popup-rule!
    (rx string-start "*" (? "doom:") "vterm")
    :side 'right :width 101 :vslot -4 :select t :quit nil :ttl 0))

(defun my/compilation-buffer-p (bn &rest _)
  (let ((buffer (window-normalize-buffer bn)))
    (when buffer
      (with-current-buffer buffer
        (derived-mode-p 'compilation-mode)))))

(set-popup-rule!
  #'my/compilation-buffer-p
  :side 'right :width 81 :modeline t :select t :ttl 0)


;; this looks like it's crashing emacs on startup. dunno why
;;(when (modulep! :editor word-wrap)
;;  (+global-word-wrap-mode 1))

;; after popup setup
(when (boundp 'project-per-tab-mode)
  (project-per-tab-mode))

(load (concat doom-user-dir "config-local.el") t)

(after! server
  (when (not (server-running-p server-name))
    (server-start)))


