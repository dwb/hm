;;; project-per-tab.el --- Support a "one tab per project" workflow -*- lexical-binding: t; -*-

;; Author: Dani Brown <d@dani.cool>
;; URL: https://github.com/fritzgrabo/project-per-tab
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Support a "one tab per project" workflow.
;; TODO:
;; * Kill appropriate buffers when closing a project tab.

;;; Code:

(require 'seq)
(require 'project)

(require 'subproject)

;; TODO: This is a hack to ensure that projectile is loaded before
;; we try to get the current project of a buffer. Otherwise the project
;; recorded in a tab won't necessarily match the project of the buffer.
(require 'projectile nil t)

(eval-when-compile (require 'subr-x))



(defgroup project-per-tab ()
  "Support a \"one tab group per project\" workflow."
  :group 'project)

(defcustom project-per-tab-tab-group "project"
  "Put project-specific tabs in this tab group."
  :type 'string)

(defcustom project-per-tab-include-subprojects nil
  "Also create new tabs for subprojects."
  :type 'boolean
  :local t)



(define-minor-mode project-per-tab-mode
  "Try and keep projects in their own tab"
  :group 'project
  :global t

  (setf display-buffer-alist
        (assq-delete-all
         'project-per-tab--display-buffer-matcher
         display-buffer-alist))
  (remove-hook 'kill-buffer-hook #'project-per-tab--kill-buffer-hook)
  (setf tab-bar-tab-pre-close-functions
        (delq
         'project-per-tab--kill-all-buffers
         tab-bar-tab-pre-close-functions))

  (when project-per-tab-mode
    ;; doesn't work well enough yet -
    ;; sometimes the hook is called with the current buffer not being the
    ;; one that is killed!!
    ;; might have to advise replace-buffer-in-windows
    ;; (add-hook 'kill-buffer-hook #'project-per-tab--kill-buffer-hook)
    (push '(project-per-tab--display-buffer-matcher
            project-per-tab--display-buffer)
          display-buffer-alist)
    (add-to-list 'tab-bar-tab-pre-close-functions
                 #'project-per-tab--kill-all-buffers)))

(defun project-per-tab-project-of-tab (&optional tab)
  (let ((tab (or tab (project-per-tab--current-tab))))
    (alist-get 'project tab)))

(defun project-per-tab-set-project-of-tab (project &optional force)
  (when-let ((project (project-per-tab--normalise-project project))
             (tab (project-per-tab--current-tab))
             (tabparams (cdr tab)))
    (if-let (oldproj (and (not force) (alist-get 'project tabparams)))
        (unless (equal oldproj project)
          (warn "project-per-tab: Not overwriting tab's project with different project"))
      (let ((newparams (assq-delete-all 'project tabparams)))
        (setf (cdr tab) (cons `(project . ,project) newparams))))))

(defvar project-per-tab-tab-name-function
  #'project-per-tab-tab-name
  "Function to find the tab group name for a directory.

The function is expected to take a directory as its single
argument and to return the tab group name to represent the
contained project.")

(defun project-per-tab-tab-name (project)
  "Derive tab name for project.

Returns the value of `tab-name' or `project-name', if
present, and falls back to the directory file name otherwise.

In addition, uses `tab-name-template' or
`project-name-template', if present, as the format-string in a
call to `format'. The format-string is expected to have a single
\"%s\" sequence which will be substituted by the project name."
  (when-let* ((project (project-per-tab--normalise-project project))
              (dir (project-root project))
              (name (file-name-nondirectory (directory-file-name dir)))
              (name-template (or (and (boundp 'tab-name-template) tab-group-name-template)
                                 (and (boundp 'project-name-template) project-name-template)
                                 "%s")))
    (format name-template name)))



(defun project-per-tab--current-tab ()
  (assq 'current-tab (funcall tab-bar-tabs-function nil)))

(defun project-per-tab--normalise-project (project)
  (if project-per-tab-include-subprojects
      project
    (subproject-parent-or-self project)))

(defun project-per-tab--project-of-buffer (buffer-or-name)
  (when-let* ((buffer (get-buffer buffer-or-name))
              (fn (buffer-file-name buffer))
              (dir (file-name-parent-directory fn)))
    (project-current nil dir)))

(defun project-per-tab--display-buffer-matcher (buffer _arg)
  (project-per-tab--project-of-buffer buffer))

(defun project-per-tab--display-buffer (buffer alist)
  (when-let ((project (with-current-buffer buffer (project-current)))
             (name (funcall project-per-tab-tab-name-function project)))
    (prog1
        (display-buffer-in-tab buffer
                               (append
                                `((tab-name . ,name)
                                  (tab-group . ,project-per-tab-tab-group))
                                alist))
      (project-per-tab-set-project-of-tab project))))

(defun project-per-tab--kill-buffer-hook ()
  "Close the tab if the only remaining displayed buffer is unrelated to the project"
  (when-let ((tab (project-per-tab--current-tab))
             (tabname (alist-get 'name tab))
             (buf (current-buffer))
             (proj (project-current))
             (tabproj (project-per-tab-project-of-tab)))
    (when (and
           (buffer-file-name buf)
           (not (minibuffer-window-active-p (selected-window)))
           (equal proj tabproj)
           (one-window-p)
           (null (seq-filter #'(lambda (b)
                                 (with-current-buffer (get-buffer (car b))
                                   (equal tabproj (project-current))))
                             (window-prev-buffers))))
      (message "Closing %s tab: %s was the last project buffer." tabname buf)
      (let ((project-per-tab--clearing-tab-project-buffers t))
        (tab-bar-close-tab)))))

(defvar project-per-tab--clearing-tab-project-buffers nil)

(defun project-per-tab--kill-all-buffers (tab _onlyinframe)
  (when (not project-per-tab--clearing-tab-project-buffers)
    (when-let ((project-per-tab--clearing-tab-project-buffers t)
               (project (project-per-tab-project-of-tab tab))
               (buffers (project-buffers project)))
      (seq-do #'kill-buffer buffers))))

(provide 'project-per-tab)
;;; project-per-tab.el ends here
