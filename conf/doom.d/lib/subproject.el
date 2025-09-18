;;; subproject.el --- Subproject support for built-in project.el -*- lexical-binding: t -*-

;; Author: Dani Brown <d@dani.cool>
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: project
;; URL: https://github.com/dwb/emacs-subproject/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides support for subprojects using the built-in
;; project.el package.  The main function, `subproject-find`, can be
;; added directly to the `project-find-functions` list.

(require 'cl-lib)
(require 'project)

;;; Code:

(defvar-local subproject-patterns nil
  "List of globs for matching subprojects.")

(defun subproject-valid-dir-local-config-p (val)
  "Return non-nil if VAL is a list of strings."
  (and
   (listp val)
   (seq-every-p #'stringp val)))

(put 'subproject-patterns 'safe-local-variable #'subprojects-valid-dir-local-config-p)

(defvar subproject-inhibit-find nil
  "Disable finding subprojects. Deprecated, does nothing now.")

(defvar subproject-allow-find nil
  "Allow finding subprojects.")

(defun subproject-with-inhibiting-find (fn &rest args)
  (let ((subproject-inhibit-find t))
    (apply fn args)))

(defun subproject-with-allowing-find (fn &rest args)
  (let ((subproject-allow-find t))
    (apply fn args)))

(defun subproject-project-p (project)
  (and (listp project) (eq (car project) 'subproject)))

(defun subproject-alist (project)
  (when (subproject-project-p project)
    (caddr project)))

(defun subproject-parent-project (project)
  (alist-get 'parent (subproject-alist project)))

(defun subproject-parent-or-self (project)
  (if (subproject-project-p project)
      (subproject-parent-project project)
    project))

(defun subproject-relative-root (project)
  (when (subproject-project-p project)
    (cadr project)))

(defun subproject-root (project)
  "Retrieve the root directory of a subproject PROJECT."
  (when (subproject-project-p project)
    (expand-file-name
     (subproject-relative-root project)
     (project-root (subproject-parent-project project)))))

(cl-defmethod project-root ((project (head subproject)))
  (subproject-root project))

(defun subproject-find (dir)
  "Find the subproject for DIR.
Return a list representing the subproject if the current file is
under a path matching a list of patterns in `subproject-patterns'. The
list representing the subproject should look like
`(subproject \"project/local/path\" (parent . PARENT-PROJECT))'."
  (when (and subproject-allow-find (not subproject-inhibit-find))
    (when-let* ((subproject-inhibit-find 'subproject-find)
                (parent-project (project-current nil dir))
                (root (project-root parent-project))
                (relative-dir (file-relative-name dir root))
                (matched-dir (seq-some #'(lambda (pat)
                                          (let ((pat (concat "\\`" pat "/")))
                                            (when (string-match pat relative-dir)
                                              (substring relative-dir (match-beginning 0) (match-end 0)))))
                                        subproject-patterns)))
      (list 'subproject (file-name-as-directory matched-dir) `((parent . ,parent-project))))))

(defun subproject-unload-function ()
  (setf project-find-functions (delq 'subproject-find project-find-functions)))

(provide 'subproject)

;;; subproject.el ends here
