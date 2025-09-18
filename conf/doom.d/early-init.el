;; auto-compile (requires auto-compile in packages.el)
(setq load-prefer-newer t)
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(add-to-list 'default-frame-alist '(width . 220))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))
