;;; lib/my-core-ext.el -*- lexical-binding: t; -*-

(require 'seq)

(defun my/eq-or-memq (val elt)
  (if (listp val) (memq elt val) (eq elt val)))

(defun my/major-mode-unmapped (&optional mode)
  (let ((mode (or mode major-mode)))
    (or (and (boundp 'major-mode-remap-alist)
             (rassq mode major-mode-remap-alist))
        mode)))

(defun my/tab-bar-tab-name (&optional tab frame)
  (let* ((tab (or tab (tab-bar--current-tab)))
         (explicit-name (alist-get 'explicit-name tab))
         (tab-name (alist-get 'name tab))
         (name (if explicit-name tab-name
                 (let* ((tabs (funcall tab-bar-tabs-function frame))
                        (tab-index (seq-position tabs tab)))
                   (+ 1 tab-index)))))
    (format "%s" name)))
