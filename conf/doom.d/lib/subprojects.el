;;; lib/subprojects.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'projectile)

(defvar-local subproject-dirs nil
  "not implemented")
(defvar-local subproject-patterns nil
  "glob patterns, using file-expand-wildcards, not regexps")
(defvar-local subproject-functions nil
  "not implemented")

(defun subprojects-valid-dir-local-config-p (val)
  "Return non-nil if VAL is a list of strings."
  (and
   (listp val)
   (seq-every-p #'stringp val)))

(put 'subproject-dirs 'safe-local-variable #'subprojects-valid-dir-local-config-p)
(put 'subproject-patterns 'safe-local-variable #'subprojects-valid-dir-local-config-p)

(defun subprojects-of-project-of-buffer (&optional buffer-or-name)
  (with-current-buffer (or buffer-or-name (current-buffer))
    (when-let ((root (projectile-project-root))
               (ptns subproject-patterns))
      (seq-mapcat
       #'(lambda (p)
           (if (string-search ".." p)
               (prog1 nil
                 (message (format "subprojects: disallowed parent directory in pattern: %s" p)))
             (seq-map #'file-name-as-directory (file-expand-wildcards (concat root p) t))))
       ptns))))

(defun subproject-of-buffer (&optional buffer-or-name)
  (with-current-buffer (or buffer-or-name (current-buffer))
    (if-let ((fn (buffer-file-name))
             (subprojs (subprojects-of-project-of-buffer)))
        (seq-find #'(lambda (p) (string-prefix-p p fn)) subprojs))))

(provide 'subprojects)
