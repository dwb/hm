;;; my-calc.el --- my extensions for calc            -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Dani Brown

;; Author: Dani Brown <d@dani.cool>
;; Keywords: tools, tools, tools

(defun my/make-frame-for-calc ()
  (let ((frames (make-frame-names-alist))
        (name "calc"))
    (or
     (alist-get name frames nil nil #'equal)
     (make-frame `((name . ,name))))))

(my/make-frame-for-calc)

(defun my/calc-new-frame ()
  (let ((calc-window-hook (cons #'my/make-frame-for-calc calc-window-hook)))))

(defun my/calc (&optional arg full-display interactive)
  "The Emacs Calculator.  Full documentation is listed under `calc-mode'."
  (interactive "P\ni\np")
  (if arg
      (unless (eq arg 0)
        (require 'calc-ext)
        (if (= (prefix-numeric-value arg) -1)
            (calc-grab-region (region-beginning) (region-end) nil)
          (when (= (prefix-numeric-value arg) -2)
            (calc-keypad))))
    ;; If the selected window changes here, Emacs may think that the
    ;; selected window is read only, and no on screen keyboard should
    ;; be displayed.  Make sure that any active on screen keyboard is
    ;; not hidden by accident.
    (let ((touch-screen-display-keyboard t))
      (when (get-buffer-window "*Calc Keypad*")
        (calc-keypad)
        (set-buffer (window-buffer)))
      (if (derived-mode-p 'calc-mode)
          (calc-quit)
        (calc-create-buffer)
        (setq calc-was-keypad-mode nil)
        (if (or (eq full-display t)
                (and (null full-display) calc-full-mode))
            (switch-to-buffer (current-buffer) t)
          (if (get-buffer-window (current-buffer))
              (select-window (get-buffer-window (current-buffer)))
            (if calc-window-hook
                (run-hooks 'calc-window-hook)
              (let ((w (get-largest-window)))
                (if (and pop-up-windows
                         (> (window-height w)
                            (+ window-min-height calc-window-height 2)))
                    (progn
                      (setq w (split-window w
                                            (- (window-height w)
                                               calc-window-height 2)
                                            nil))
                      (set-window-buffer w (current-buffer))
                      (select-window w))
                  (pop-to-buffer (current-buffer)))))))
        (with-current-buffer (calc-trail-buffer)
          (and calc-display-trail
               (calc-trail-display 1 t)))
        (message (substitute-command-keys
                  (concat "Welcome to the GNU Emacs Calculator!  \\<calc-mode-map>"
                          "Press \\[calc-help] or \\[calc-help-prefix] for help, \\[calc-quit] to quit")))
        (run-hooks 'calc-start-hook)
        (and (windowp full-display)
             (window-point full-display)
             (select-window full-display))
        (and calc-make-windows-dedicated
             (set-window-dedicated-p nil t))
        (calc-check-defines)
        (when (and calc-said-hello interactive)
          (sit-for 2)
          (message ""))
        (setq calc-said-hello t)))))

(provide 'my-calc)
