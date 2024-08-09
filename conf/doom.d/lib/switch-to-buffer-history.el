;;; lib/switch-to-buffer-history.el -*- lexical-binding: t; -*-

(defun switch-to-buffer-with-history (oldfun &rest r)
  (let ((oldbuf (current-buffer)))
    (prog1 (apply oldfun r)
      (when (and (not (eq (current-buffer) oldbuf))
                 (buffer-live-p oldbuf))
        (add-to-history 'buffer-name-history (buffer-name oldbuf))))))

(define-minor-mode switch-to-buffer-history-mode
  "Advises switch-to-buffer to add the selected buffer to buffer-name-history"
  :global t
  (if switch-to-buffer-history-mode
      (advice-add 'switch-to-buffer :around 'switch-to-buffer-with-history)
    (advice-remove 'switch-to-buffer 'switch-to-buffer-with-history)))

(provide 'switch-to-buffer-history)
