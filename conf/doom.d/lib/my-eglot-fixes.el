;;; lib/my-eglot-fixes.el -*- lexical-binding: t; -*-

(defun my/eglot--running-server (project mode)
  (when-let ((projsrv (gethash project eglot--servers-by-project)))
    (cl-find mode projsrv
             :key #'eglot--major-modes
             :test #'memq)))

(defun my/eglot--major-mode-unmapped ()
  (or
   (when-let ((m (and (boundp 'major-mode-remap-alist)
                      (rassq major-mode major-mode-remap-alist))))
             (car m))
   major-mode))

(defun my/eglot--guess-contact (&optional interactive)
  "Helper for `eglot'.
Return (MANAGED-MODE PROJECT CLASS CONTACT LANG-ID).  If INTERACTIVE is
non-nil, maybe prompt user, else error as soon as something can't
be guessed."
  (let* ((guessed-mode (if buffer-file-name (my/eglot--major-mode-unmapped)))
         (guessed-mode-name (and guessed-mode (symbol-name guessed-mode)))
         (main-mode
          (cond
           ((and interactive
                 (or (>= (prefix-numeric-value current-prefix-arg) 16)
                     (not guessed-mode)))
            (intern
             (completing-read
              "[eglot] Start a server to manage buffers of what major mode? "
              (mapcar #'symbol-name (eglot--all-major-modes)) nil t
              guessed-mode-name nil guessed-mode-name nil)))
           ((not guessed-mode)
            (eglot--error "Can't guess mode to manage for `%s'" (current-buffer)))
           (t guessed-mode)))
         (triplet (eglot--lookup-mode main-mode))
         (managed-modes (car triplet))
         (language-id (or (cadr triplet)
                          (string-remove-suffix "-mode" (symbol-name guessed-mode))))
         (guess (caddr triplet))
         (guess (if (functionp guess)
                    (funcall guess interactive)
                  guess))
         (class (or (and (consp guess) (symbolp (car guess))
                         (prog1 (unless current-prefix-arg (car guess))
                           (setq guess (cdr guess))))
                    'eglot-lsp-server))
         (program (and (listp guess)
                       (stringp (car guess))
                       ;; A second element might be the port of a (host, port)
                       ;; pair, but in that case it is not a string.
                       (or (null (cdr guess)) (stringp (cadr guess)))
                       (car guess)))
         (base-prompt
          (and interactive
               "Enter program to execute (or <host>:<port>): "))
         (full-program-invocation
          (and program
               (cl-every #'stringp guess)
               (combine-and-quote-strings guess)))
         (prompt
          (and base-prompt
               (cond (current-prefix-arg base-prompt)
                     ((null guess)
                      (format "[eglot] Couldn't guess LSP server for `%s'\n%s"
                              main-mode base-prompt))
                     ((and program
                           (not (file-name-absolute-p program))
                           (not (eglot--executable-find program t)))
                      (if full-program-invocation
                          (concat (format "[eglot] I guess you want to run `%s'"
                                          full-program-invocation)
                                  (format ", but I can't find `%s' in PATH!"
                                          program)
                                  "\n" base-prompt)
                        (eglot--error
                         (concat "`%s' not found in PATH, but can't form"
                                 " an interactive prompt for to fix %s!")
                         program guess))))))
         (contact
          (or (and prompt
                   (split-string-and-unquote
                    (read-shell-command
                     prompt
                     full-program-invocation
                     'eglot-command-history)))
              guess)))
    (list managed-modes (eglot--current-project) class contact language-id)))

(defun my/eglot-current-server ()
  "Return logical Eglot server for current buffer, nil if none."
  (setq eglot--cached-server
        (let ((project (eglot--current-project)))
          (or eglot--cached-server
              (my/eglot--running-server project (my/eglot--major-mode-unmapped))
              (and eglot-extend-to-xref
                   buffer-file-name
                   (gethash (expand-file-name buffer-file-name)
                            eglot--servers-by-xrefed-file))))))

(define-minor-mode my-eglot-fixes-mode
  "my eglot fixes!!!"
  :global t

  (cl-labels
      ((advise (op)
         (dolist (old '(eglot-current-server eglot--guess-contact))
           (funcall op old (intern (format "my/%s" old)))))
       (add (old new) (advice-add old :override new))
       (remove (old new) (advice-remove old new)))

    (let ((op (if my-eglot-fixes-mode
                  #'add #'remove)))
      (advise op))))

(provide 'my-eglot-fixes)
