;;; lib/go-mode-extns.el -*- lexical-binding: t; -*-

(defun my/assert-file-buffer-with-mode (buf mode)
  (unless buf
    (error "no buffer given"))
  (with-current-buffer buf
    (unless (buffer-file-name buf)
      (error (format "%s is not visiting a file" buf)))
    (when mode
      (unless (eq mode major-mode)
        (error (format "major-mode of %s is %s, not %s" buf major-mode mode))))))

(after! go-mode

  (defun my/go-test-subpackage (&optional buf)
    "Tests the first-level subpackage of the current buffer, and descendents"
    (interactive)

    (or buf (setq buf (current-buffer)))
    (my/assert-file-buffer-with-mode buf go-mode)

    (unless buf
      (error "no buffer given"))

    (setq fn (buffer-file-name buf))
    (unless fn (error (format "%s is not visiting a file")))))
