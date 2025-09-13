;;; frame-project-dedicate.el -*- lexical-binding: t; -*-

;; Author: Dani Brown <d@dani.cool>
;; URL: https://github.com/dwb/frame-project-dedicate.el
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))
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

;; Using this package, an Emacs frame can optionally be dedicated to a
;; project. This works by setting the frame's buffer-predicate to only
;; allow buffers from that project, or no project (in the case of help
;; buffers, etc). It includes convenience commands to allow easily
;; switching to a file in a project, opening the dedicated frame in the
;; process, and helps with identifying project-dedicated frames in the
;; UI.

;;; Code:

(require 'project)
(require 'seq)

;;; Variables

(defvar-local frame-project-dedicate--honorary-project nil
  "Buffer-local variable indicating which project this buffer should be allowed in.
This is used for special buffers like *Help*, *Messages*, etc.")

;;; Core Functions

(defconst frame-project-dedicate-project-root-frame-parameter 'frame-project-dedicate-project-root)

(defun frame-project-dedicate--get-frame-project-root (frame)
  "Get the project root string that FRAME is dedicated to, if any."
  (frame-parameter frame frame-project-dedicate-project-root-frame-parameter))

(defun frame-project-dedicate--get-frame-project (frame)
  "Get the project object that FRAME is dedicated to, if any."
  (when-let* ((project-root (frame-project-dedicate--get-frame-project-root frame)))
    (project-current nil project-root)))

(defun frame-project-dedicate--get-project-frames ()
  "Return list of frames that are dedicated to projects."
  (filtered-frame-list #'frame-project-dedicate--get-frame-project-root))

(defun frame-project-dedicate--find-project-frame (project)
  "Find existing frame dedicated to PROJECT, if any."
  (let ((project-root (project-root project)))
    (seq-find (lambda (frame)
                (string= (frame-project-dedicate--get-frame-project-root frame) project-root))
              (frame-list))))

(defun frame-project-dedicate--set-frame-project (frame project)
  "Dedicate FRAME to PROJECT."
  (let ((project-root (project-root project)))
    (set-frame-parameter frame
                         frame-project-dedicate-project-root-frame-parameter
                         project-root)
    (frame-project-dedicate-ensure-installed-in-frame frame)))

(defun frame-project-dedicate--set-frame-name (&optional frame)
  (let* ((frame (or frame (selected-frame)))
         (project (frame-project-dedicate--get-frame-project frame))
         (name (format "project: %s" (project-name project))))
    (unless project (user-error "not a project-dedicated frame"))
    (set-frame-name name)))

(defun frame-project-dedicate--buffer-allowed-p (project buffer)
  "Return t if BUFFER should be allowed in a frame dedicated to PROJECT."
  (message "frame-project-dedicate--buffer-allowed-p")
  (let ((project-root (project-root project)))
    (with-current-buffer buffer
      (or
       ;; Buffer has honorary project that matches
       (and frame-project-dedicate--honorary-project
            (string= frame-project-dedicate--honorary-project project-root))
       ;; Buffer belongs to the project (using project.el functionality)
       (when-let* ((buffer-file (buffer-file-name buffer)))
         (if-let* ((buffer-project (project-current nil)))
             ;; Buffer has a project, check if it matches
             (string= (project-root buffer-project) project-root)
           ;; Buffer has no project but might be in the project directory
           (string-prefix-p project-root (file-truename buffer-file))))
       ;; Special buffers without files that should always be allowed
       (frame-project-dedicate--special-buffer-p buffer)))))

(defun frame-project-dedicate--special-buffer-p (buffer)
  "Return t if BUFFER is a special buffer that should be allowed in any frame."
  (with-current-buffer buffer
    (or
     ;; No associated file and starts with * (like *scratch*, *Messages*)
     (and (null (buffer-file-name))
          (string-prefix-p "*" (buffer-name)))
     ;; Specific buffer types we always want to allow
     (derived-mode-p 'help-mode 'completion-list-mode 'messages-buffer-mode))))

;;; Helper Functions for Commands

(defun frame-project-dedicate--get-project-frame-alist ()
  "Return alist of (project-name . frame) for all project-dedicated frames."
  (mapcar (lambda (frame)
            (let* ((project-root (frame-project-dedicate--get-frame-project-root frame))
                   (project-name (file-name-nondirectory 
                                 (directory-file-name project-root))))
              (cons project-name frame)))
          (frame-project-dedicate--get-project-frames)))

(defun frame-project-dedicate--select-project-frame ()
  "Interactively select a project frame and return the frame."
  (let* ((frame-alist (frame-project-dedicate--get-project-frame-alist))
         (project-names (mapcar #'car frame-alist)))
    (if (null project-names)
        (user-error "No project-dedicated frames exist")
      (let* ((selected-name (completing-read "Switch to project frame: " project-names nil t))
             (selected-frame (cdr (assoc selected-name frame-alist))))
        selected-frame))))

(defun frame-project-dedicate--select-project-and-switch-or-create ()
  "Select a project and switch to its frame (creating if needed)."
  (let* ((dir (funcall project-prompter))
         (project (project-current t dir)))
    (project-remember-project project)
    ;; Check for existing frame first
    (if-let* ((existing-frame (frame-project-dedicate--find-project-frame project)))
        (progn
          (select-frame existing-frame)
          (message "Switched to existing frame for project: %s" 
                   (file-name-nondirectory (directory-file-name (project-root project)))))
      ;; Create new dedicated frame
      (let ((frame (make-frame)))
        (frame-project-dedicate--set-frame-project frame project)
        (select-frame frame)
        ;; Open the project root in dired as a starting point
        (dired (project-root project))))))

(defun frame-project-dedicate-ensure-installed-in-frame (frame)
  (message "frame-project-dedicate-ensure-installed-in-frame %S with frame params %S" frame (frame-parameters frame))
  (when-let* ((project (frame-project-dedicate--get-frame-project frame)))
    (message "setting project %S" project)
    (frame-project-dedicate--set-frame-name frame)
    (set-frame-parameter frame
                         'buffer-predicate
                         (apply-partially
                          #'frame-project-dedicate--buffer-allowed-p
                          project))))

(defun frame-project-dedicate-ensure-installed-in-frames (&optional frames)
  (seq-each #'frame-project-dedicate-ensure-installed-in-frame
            (or frames (frame-list))))

(add-to-list 'after-make-frame-functions #'frame-project-dedicate-ensure-installed-in-frame)

(with-eval-after-load 'desktop
  (add-hook 'desktop-after-read-hook #'frame-project-dedicate-ensure-installed-in-frames))

(frame-project-dedicate-ensure-installed-in-frames)

;;; Commands

;;;###autoload
(defun frame-project-dedicate-switch (arg)
  "Switch between project-dedicated frames or create new ones.

Without prefix ARG: Present a list of existing project-dedicated
frames to switch between.

With prefix ARG: Present a list of all projects (including those
without dedicated frames). When selecting one, either switch to
the existing frame or create a new one."
  (interactive "P")
  (if arg
      (frame-project-dedicate--select-project-and-switch-or-create)
    (let ((frame (frame-project-dedicate--select-project-frame)))
      (raise-frame frame)
      (select-frame frame))))

;; Obsolete commands - kept for backward compatibility
;;;###autoload
(defun frame-project-dedicate-new-frame (dir)
  "Create a new frame dedicated to project in DIR.
If called interactively, prompt for a project.
Reuse existing frame if one is already dedicated to this project.

This command is obsolete. Use `frame-project-dedicate-switch' with prefix arg instead."
  (interactive (list (funcall project-prompter)))
  (let ((project (project-current t dir)))
    (project-remember-project project)
    ;; Check for existing frame first
    (if-let* ((existing-frame (frame-project-dedicate--find-project-frame project)))
        (progn
          (select-frame existing-frame)
          (message "Switched to existing frame for project: %s" 
                   (file-name-nondirectory (directory-file-name (project-root project)))))
      ;; Create new dedicated frame
      (let ((frame (make-frame)))
        (frame-project-dedicate--set-frame-project frame project)
        (select-frame frame)
        ;; Open the project root in dired as a starting point
        (dired (project-root project))))))

;;;###autoload
(defun frame-project-dedicate-find-file (dir)
  "Find file from project in DIR in a frame dedicated to that project.
Create the dedicated frame if it doesn't exist.
Reuse existing frame if one is already dedicated to this project.

This command is obsolete. Use `frame-project-dedicate-switch' with prefix arg instead."
  (interactive (list (funcall project-prompter)))
  (let* ((project (project-current t dir))
         (filename (let ((default-directory (project-root project)))
                     (project-find-file))))
    (project-remember-project project)
    ;; Look for existing frame dedicated to this project
    (if-let* ((existing-frame (frame-project-dedicate--find-project-frame project)))
        (progn
          (select-frame existing-frame)
          (find-file filename))
      ;; Create new dedicated frame
      (let ((frame (make-frame)))
        (frame-project-dedicate--set-frame-project frame project)
        (select-frame frame)
        (find-file filename)))))

(make-obsolete 'frame-project-dedicate-new-frame 'frame-project-dedicate-switch "1.0")
(make-obsolete 'frame-project-dedicate-find-file 'frame-project-dedicate-switch "1.0")

;;;###autoload
(defun frame-project-dedicate-set-honorary-project (dir)
  "Set the current buffer's honorary project to project in DIR.
This allows special buffers to appear in project-dedicated frames."
  (interactive (list (funcall project-prompter)))
  (let* ((project (project-current t dir))
         (project-root (project-root project)))
    (project-remember-project project)
    (setq frame-project-dedicate--honorary-project project-root)
    (message "Buffer '%s' now has honorary project: %s" 
             (buffer-name) 
             (file-name-nondirectory (directory-file-name project-root)))))

(provide 'frame-project-dedicate)
