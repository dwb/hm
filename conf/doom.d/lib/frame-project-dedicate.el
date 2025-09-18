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
  (seq-filter #'frame-project-dedicate--get-frame-project-root
              (or (frame-list-z-order) (frame-list))))

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

(defun frame-project-dedicate--set-frame-name (frame)
  (let* ((project (frame-project-dedicate--get-frame-project frame))
         (name (format "project: %s" (project-name project))))
    (unless project (user-error "not a project-dedicated frame"))
    ;; set-frame-name only works on the selected frame
    (modify-frame-parameters frame (list (cons 'name name)))))

(defun frame-project-dedicate--project-placeholder-buffer (project)
  (let* ((name (format "*project %s*" (project-name project)))
         (buffer (get-buffer name)))
    (or buffer
        (with-current-buffer (get-buffer-create name)
            (prog1 (current-buffer)
              (insert "yay, ")
              (insert (project-name project))
              (setq-local default-directory (project-root project))
              (read-only-mode))))))

(defvar frame-project-dedicate--called-recursively nil)
(defvar frame-project-dedicate--project-buffers-cache nil)

(defmacro frame-project-dedicate--with-project-buffers-cache (&rest body)
  `(let* ((frame-project-dedicate--project-buffers-cache
          (or frame-project-dedicate--project-buffers-cache
              (make-hash-table :test #'equal))))
     ,@body))

(defun frame-project-dedicate--get-project-buffers (project)
  (when (null frame-project-dedicate--project-buffers-cache)
    (error "frame-project-dedicate--get-project-buffers: not called within frame-project-dedicate--with-project-buffers-cache"))
  (if-let* ((cache frame-project-dedicate--project-buffers-cache)
            (buffers (gethash project cache)))
      buffers
    (puthash project (project-buffers project) cache)))

(defun frame-project-dedicate--buffer-allowed-p (project buffer)
  "Return t if BUFFER should be allowed in a frame dedicated to PROJECT."
  (if frame-project-dedicate--called-recursively
      (prog1 (buffer-live-p buffer)
        ;; (setq my/fpdbt (backtrace-get-frames 'backtrace-get-frames))
        ;; (message "frame-project-dedicate--buffer-allowed-p: called recursively")
        )

    (let ((frame-project-dedicate--called-recursively t))
      (when (buffer-live-p buffer)
        (frame-project-dedicate--with-project-buffers-cache
         (or
          ;; Buffer has honorary project that matches
          ;; TODO - prob use buffer-local-value
          ;; (and frame-project-dedicate--honorary-project
          ;;      (string= frame-project-dedicate--honorary-project project-root))
          ;; Buffer belongs to the project (using project.el functionality)
          (seq-contains-p (frame-project-dedicate--get-project-buffers project) buffer)
          ;; Special buffers without files that should always be allowed
          (frame-project-dedicate--special-buffer-p buffer)))))))

(defun frame-project-dedicate--special-buffer-p (buffer)
  "Return t if BUFFER is a special buffer that should be allowed in any frame."
  (let* ((major-mode (buffer-local-value 'major-mode buffer)))
    (derived-mode-p 'special-mode)))

;;; Helper Functions for Commands

(defun frame-project-dedicate--get-project-frame-alist ()
  "Return alist of (project-name . frame) for all project-dedicated frames."
  (seq-map (lambda (frame)
             (let* ((project-root (frame-project-dedicate--get-frame-project-root frame))
                    (project-name (file-name-nondirectory
                                   (directory-file-name project-root))))
               (cons project-name frame)))
           (frame-project-dedicate--get-project-frames)))

(defvar frame-project-dedicate--select-project-frame-history)

(defun frame-project-dedicate--select-project-frame ()
  "Interactively select a project frame and return the frame."
  (let* ((frame-alist-unsorted (frame-project-dedicate--get-project-frame-alist))
         (frame-alist (append (cdr frame-alist-unsorted) (list (car frame-alist-unsorted))))
         (project-names (seq-map #'car frame-alist)))
    (if (null project-names)
        (user-error "No project-dedicated frames exist")
      (let* ((selected-name (completing-read "Switch to project frame: " frame-alist nil t nil 'frame-project-dedicate--select-project-frame-history (caar frame-alist)))
             (selected-frame (cdr (assoc selected-name frame-alist #'string=))))
        selected-frame))))

(defun frame-project-dedicate--select-project-and-switch-or-create (&optional project initial-buffer)
  "Select a project and switch to its frame (creating if needed)."
  (let* ((project (or project
                      (project-current t (funcall project-prompter)))))
    (project-remember-project project)
    ;; Check for existing frame first
    (if-let* ((existing-frame (frame-project-dedicate--find-project-frame project)))
        (prog1 existing-frame
          (select-frame existing-frame)
          (raise-frame existing-frame)
          (message "Switched to existing frame for project: %s"
                   (project-root project)))
      ;; Create new dedicated frame
      (let ((frame (make-frame)))
        (prog1 frame
          (frame-project-dedicate--set-frame-project frame project)
          (select-frame frame)
          (let* ((buffer (frame-project-dedicate--project-placeholder-buffer project)))
            (set-window-buffer nil buffer)
            (set-buffer buffer))
          (set-window-prev-buffers (selected-window) nil)
          (raise-frame frame)
          (cond ((eq t initial-buffer)) ;; caller will deal with initial buffer
                (initial-buffer (switch-to-buffer initial-buffer))
                (t (project-find-file))))))))

(defun frame-project-dedicate-project-of-buffer (buffer)
  (project-current nil (buffer-local-value 'default-directory buffer)))

(defun frame-project-dedicate-ensure-installed-in-frame (frame)
  (when-let* ((project (frame-project-dedicate--get-frame-project frame)))
    (frame-project-dedicate--set-frame-name frame)
    (set-frame-parameter frame
                         'buffer-predicate
                         (lambda (b) (frame-project-dedicate--buffer-allowed-p project b)))))

(defun frame-project-dedicate-ensure-installed-in-frames (&optional frames)
  (seq-do #'frame-project-dedicate-ensure-installed-in-frame
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
      (select-frame frame)
      (raise-frame frame))))

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

(defun frame-project-dedicate--server-switch-buffer-advice (&optional next-buffer &rest _)
  "Select the appropriate project frame before server displays buffer.
This is needed because `server-switch-buffer' bypasses `display-buffer'
when `server-window' is nil, calling `switch-to-buffer' directly."
  (when (bufferp next-buffer)
    (when-let* ((project (frame-project-dedicate-project-of-buffer next-buffer))
                (frame (frame-project-dedicate--find-project-frame project)))
      (select-frame-set-input-focus frame))))

(defun frame-project-dedicate-display-buffer-use-dedicated-frame (buffer alist)
  "Display BUFFER in an existing frame that should be dedicated to it.

ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

If ALIST has a non-nil `inhibit-switch-frame' entry, avoid
raising the frame.  If ALIST has a non-nil `inhibit-same-window'
entry, avoid using the currently selected window (only useful
with a frame-predicate that allows using the selected frame).

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (when-let* ((project (frame-project-dedicate-project-of-buffer buffer))
              (project-root (project-root project))
              (predicate
               (lambda (frame)
                 ;; (and (not (eq frame (selected-frame)))
                 ;;      (get-lru-window frame))))
                 (string= project-root
                          (frame-project-dedicate--get-frame-project-root frame))))
              (frame (or (seq-find predicate (frame-list))
                         (frame-project-dedicate--select-project-and-switch-or-create
                          project t)))
              (window (get-largest-window
                       frame nil (cdr (assq 'inhibit-same-window alist)))))
    (prog1 (window--display-buffer buffer window 'reuse alist)
      (unless (cdr (assq 'inhibit-switch-frame alist))
        (window--maybe-raise-frame frame)))))

(define-minor-mode frame-project-dedicate-mode nil :global t
  (if frame-project-dedicate-mode
      (progn
        (let* ((orig display-buffer-base-action)
               (fns (car orig))
               (attrs (cdr orig))
               (newfn #'frame-project-dedicate-display-buffer-use-dedicated-frame))
          (setf display-buffer-base-action
                (cons (cons newfn fns) attrs)))
        (advice-add 'server-switch-buffer :before
                    #'frame-project-dedicate--server-switch-buffer-advice))
    (let* ((orig display-buffer-base-action)
           (newfns (delq 'frame-project-dedicate-display-buffer-use-dedicated-frame
                         (car orig))))
      (setf display-buffer-base-action
            (cons newfns (cdr orig))))
    (advice-remove 'server-switch-buffer
                   #'frame-project-dedicate--server-switch-buffer-advice)))

(defun frame-project-dedicate-unload-function ()
  (prog1 nil
    (frame-project-dedicate-mode -1)))

(provide 'frame-project-dedicate)
