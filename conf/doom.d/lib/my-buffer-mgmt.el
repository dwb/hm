;;; lib/buffer-mgmt.el -*- lexical-binding: t; -*-

(require 'seq)

(defun my/kill-other-workspace-buffers-of-purpose ()
  "Given the current buffer, kill other buffers in the same workspace and purpose."
  (interactive)
  (let* ((persp (get-current-persp))
         (keepbuf (current-buffer))
         (purpose (purpose-buffer-purpose keepbuf))
         (perspbufs (safe-persp-buffers persp))
         (bufs (seq-filter
                (lambda (b) (and
                             (not (eq b keepbuf))
                             (eq purpose (purpose-buffer-purpose b))))
                perspbufs)))
    (if bufs
        (progn
          (dolist (b bufs) (kill-buffer b))
          (message "Killed %d %s buffers in workspace" (length bufs) purpose))
      (message "No other %s buffers in workspace" purpose))))

(provide 'my-buffer-mgmt)
