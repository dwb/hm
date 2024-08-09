;;; dominating-files.el --- Search for dominating files  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Dan Brown

;; Author: Dan Brown <dan@stompydan.net>
;; Keywords: lisp, lisp, lisp, lisp, lisp, lisp, lisp, lisp, lisp, files,

(defun open-dominating-file (file)
  "Starting at FILE, look up directory hierarchy for directory containing NAME.
FILE can be a file or a directory.  If it's a file, its directory will
serve as the starting point for searching the hierarchy of directories.
Stop at the first parent directory containing a file NAME,
and return the directory.  Return nil if not found.
Instead of a string, NAME can also be a predicate taking one argument
\(a directory) and returning a non-nil value if that directory is the one for
which we're looking.  The predicate will be called with every file/directory
the function needs to examine, starting with FILE."
  (interactive "f")
  ;; Represent /home/luser/foo as ~/foo so that we don't try to look for
  ;; `name' in /home or in /.
  (setq file (abbreviate-file-name (expand-file-name file)))
  (let ((root nil)
        try)
    (while (not (or root
                    (null file)
                    (string-match locate-dominating-stop-dir-regexp file)))
      (setq try (if (stringp name)
                    (and (file-directory-p file)
                         (file-exists-p (expand-file-name name file)))
                  (funcall name file)))
      (cond (try (setq root file))
            ((equal file (setq file (file-name-directory
                                     (directory-file-name file))))
             (setq file nil))))
    (if root (file-name-as-directory root))))

(defun dominating-files (file to)
  "Starting at FILE, look up directory hierarchy, collecting all file names,
until reaching TO. Files with names found sooner (that is, closer to FILE)
will shadow files with the same name found further away."
  ())
