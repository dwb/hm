;;; doom-subprojects.el --- subprojects for doom     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Dan Brown

;; Author: Dan Brown <dan@stompydan.net>
;; Keywords: files


(defun subprojects-doom-browse ()
  "Browse files from the current subproject's root."
  (interactive)
  (doom-project-browse (subproject-of-buffer)))

(defun subprojects-doom-find-file ()
  "Find files from the current subproject's root."
  (interactive)
  (+vertico/find-file-in (subproject-of-buffer)))

(defun subprojects-doom-search (&optional arg)
  "Conduct a text search in the current project root.
If prefix ARG is set, include ignored/hidden files."
  (interactive "P")
  (let* ((dir (subproject-of-buffer)))
    (+vertico-file-search :in dir :all-files arg)))

(defvar subprojects-doom-map (make-sparse-keymap))
(map! :map subprojects-doom-map
      ("." #'subprojects-doom-browse
       "/" #'subprojects-doom-search)
       "SPC" #'subprojects-doom-find-file)

(provide 'doom-subprojects)
