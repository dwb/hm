;;; lib/fix-doom-popup.el -*- lexical-binding: t; -*-

(when (modulep! :ui popup)
  (defun my/popup-put-user-alist-back (&rest _r)
    (setf display-buffer-alist (append +popup--old-display-buffer-alist display-buffer-alist)))

  (defun my/advise-popup ()
    (advice-add 'set-popup-rule! :after #'my/popup-put-user-alist-back)
    (advice-add 'set-popup-rules! :after #'my/popup-put-user-alist-back)
    (my/popup-put-user-alist-back))

  (add-hook! +popup-mode #'my/advise-popup))

(provide 'fix-doom-popup)
