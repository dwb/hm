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



(defvar-keymap project-per-tab-map
  :doc "Overrides to make sure buffers mostly keep to their own project tab"
  "<remap> <find-file>" #'project-per-tab-find-file
  "<remap> <switch-to-buffer>" #'pop-to-buffer
  "<remap> <consult-buffer>" #'project-per-tab-consult-buffer
  "<remap> <projectile-find-file>" #'project-per-tab-projectile-find-file)

;; reload
;; (progn (project-per-tab-mode -1) (project-per-tab-mode))

(define-minor-mode project-per-tab-mode
  "Try and keep projects in their own tab"
  :group 'project
  :global t
  :keymap project-per-tab-map

  ;; v1
  (setf display-buffer-alist
        (assq-delete-all
         'project-per-tab--display-buffer-matcher
         display-buffer-alist))
  ;; v2
  (setf display-buffer-base-action
        (pcase display-buffer-base-action
          (`(,(and (pred listp) fns) . ,alist)
           `(,(remq #'project-per-tab--display-buffer fns) . ,alist))
          (`(,(and fn (guard (eql fn #'project-per-tab--display-buffer))) . ,alist)
           `(nil . ,alist))
          (else else)))
  (remove-hook 'kill-buffer-hook #'project-per-tab--kill-buffer-hook)

  (advice-remove 'find-file #'project-per-tab-find-file)

  (when project-per-tab-mode
    ;; (advice-add 'find-file :override #'project-per-tab-find-file)
    (add-hook 'kill-buffer-hook #'project-per-tab--kill-buffer-hook)
    (setf display-buffer-base-action
          (pcase display-buffer-base-action
            (`(,(and (pred listp) fns) . ,alist)
             `(,(cons #'project-per-tab--display-buffer fns) . ,alist))
            (`(,fn . ,alist)
             `(,(list #'project-per-tab--display-buffer fn) . ,alist))
            ('nil '(#'project-per-tab--display-buffer . nil))
            (else (error "project-per-tab-mode: unexpected value of display-buffer-base-action: %s" else))))))

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
              (name-template (or (and (boundp 'tab-name-template) tab-name-template)
                                 (and (boundp 'project-name-template) project-name-template)
                                 "%s")))
    (format name-template name)))

;; Key map commands

(defun project-per-tab-find-file (filename &optional wildcards)
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
        (mapcar 'pop-to-buffer (nreverse value))
      (pop-to-buffer value))))

(with-eval-after-load 'consult
  (defun project-per-tab-consult-buffer ()
    "Variant of `consult-buffer', using `pop-to-buffer'."
    (interactive)
    (let ((consult--buffer-display #'pop-to-buffer))
      (consult-buffer))))

(with-eval-after-load 'projectile
  (defun project-per-tab-projectile-find-file (&optional invalidate-cache)
    "Jump to a project's file using completion and show it in the right tab.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
    (interactive "P")
    (projectile--find-file invalidate-cache #'project-per-tab-find-file)))



(defun project-per-tab--current-tab ()
  (assq 'current-tab (funcall tab-bar-tabs-function nil)))

(defun project-per-tab--normalise-project (project)
  (if project-per-tab-include-subprojects
      project
    (subproject-parent-or-self project)))

(defun project-per-tab--project-of-buffer (buffer-or-name)
  (when-let* ((buffer (get-buffer buffer-or-name)))
    (with-current-buffer buffer
      (project-current))))

(defun project-per-tab--display-buffer-matcher (buffer _arg)
  (project-per-tab--project-of-buffer buffer))

(defun project-per-tab--buffer-matches-project-p (buffer project)
  (equal project (project-per-tab--project-of-buffer buffer)))

(defun project-per-tab--display-buffer (buffer alist)
  (when-let ((project (with-current-buffer buffer (project-current)))
             (name (funcall project-per-tab-tab-name-function project)))
    (prog1
        (display-buffer-in-tab buffer
                               (append
                                `((tab-name . ,name)
                                  (tab-group . ,project-per-tab-tab-group)
                                  (reusable-frames . 0))
                                alist))
      (project-per-tab-set-project-of-tab project))))

;; experimental
(defun project-per-tab--kill-buffer-hook ()
  "Close the tab if the only remaining displayed buffer is unrelated to the project"
  (when-let ((tab (project-per-tab--current-tab))
             (tabname (alist-get 'name tab))
             (proj (project-current))
             (tabproj (project-per-tab-project-of-tab)))
    (when (and
           (not (minibuffer-window-active-p (selected-window)))
           (one-window-p)
           (equal proj tabproj)
           (null (match-buffers #'project-per-tab--buffer-matches-project-p nil tabproj)))
      (message "Closing %s tab: that was the last project buffer." tabname)
      (tab-bar-close-tab))))

;; TODO: make work for killing the last buffer of a project anywhere,
;; don't depend on what's displayed.
(defun project-per-tab--kill-buffer-hook ()
  "Close the tab if the only remaining displayed buffer is unrelated to the project"
  (when-let ((tab (project-per-tab--current-tab))
             (tabname (alist-get 'name tab))
             (proj (project-current))
             (tabproj (project-per-tab-project-of-tab)))
    (when (and
           (not (minibuffer-window-active-p (selected-window)))
           (one-window-p)
           (equal proj tabproj)
           (null (seq-filter #'(lambda (b)
                                 (with-current-buffer (get-buffer (car b))
                                   (equal tabproj (project-current))))
                             (window-prev-buffers))))
      (message "Closing %s tab: that was the last project buffer." tabname)
      (tab-bar-close-tab))))

(provide 'project-per-tab)
;;; project-per-tab.el ends here
