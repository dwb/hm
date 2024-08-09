;;; lib/lsp-subprojects.el -*- lexical-binding: t; -*-

(require 'subprojects)

(defvar-local lsp-subprojects-enable nil
  "Keep LSP servers to subprojects")

(put 'lsp-subprojects-enable 'safe-local-variable #'booleanp)

(defun lsp--calculate-root-considering-subprojects (session file-name)
  "Calculate project root for FILE-NAME in SESSION."
  (unless (equal file-name (buffer-file-name))
    (error "lsp-subprojects: we assume lsp--calculate-root is called with the file name of the current buffer, and it hasn't been"))
  (and
   (->> session
        (lsp-session-folders-blacklist)
        (--first (and (lsp--files-same-host it file-name)
                      (lsp-f-ancestor-of? it file-name)
                      (prog1 t
                        (lsp--info "File %s is in blacklisted directory %s" file-name it))))
        not)
   (or
    (and lsp-subprojects-enable (subproject-of-buffer))
    (when lsp-auto-guess-root
      (lsp--suggest-project-root))
    (lsp-find-session-folder session file-name)
    (unless lsp-auto-guess-root
      (lsp--find-root-interactively session)))))

(define-minor-mode lsp-subprojects-mode "TODO doc"
  :global t
  :require 'lsp-subprojects
  :group 'lsp-mode
  (if lsp-subprojects-mode
      (advice-add 'lsp--calculate-root :override #'lsp--calculate-root-considering-subprojects)
    (advice-remove 'lsp--calculate-root #'lsp--calculate-root-considering-subprojects)))

(provide 'lsp-subprojects)
