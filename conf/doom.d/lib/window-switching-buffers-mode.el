;; -*- lexical-binding: t; -*-

(defvar window-switching-buffers nil "Set when a window is deciding which buffer to switch to")

(defun wsb/switch-buffer-set-window (&optional window &rest _r)
  (let ((window (window-normalize-window window t)))
    (setq window-switching-buffers window)))

(defun wsb/switch-buffer-clear-window (&rest _r)
  (setq window-switching-buffers nil))

(defmacro wsb/debug-with-switching-window (&rest body)
  `(progn
     (wsb/switch-buffer-set-window)
     (prog1
         (progn ,@body)
       (wsb/switch-buffer-clear-window))))

(define-minor-mode window-swiching-buffers-mode nil
  :global t
  (if window-swiching-buffers-mode
      (progn
        (advice-add 'switch-to-prev-buffer :before #'wsb/switch-buffer-set-window)
        (advice-add 'switch-to-next-buffer :before #'wsb/switch-buffer-set-window)
        (advice-add 'switch-to-prev-buffer :after #'wsb/switch-buffer-clear-window)
        (advice-add 'switch-to-next-buffer :after #'wsb/switch-buffer-clear-window))

    (advice-remove 'switch-to-prev-buffer :before #'wsb/switch-buffer-set-window)
    (advice-remove 'switch-to-next-buffer :before #'wsb/switch-buffer-set-window)
    (advice-add 'switch-to-prev-buffer :after #'wsb/switch-buffer-clear-window)
    (advice-add 'switch-to-next-buffer :after #'wsb/switch-buffer-clear-window)))

(provide 'window-switching-buffers-mode)
