;;; rotate.el --- Rotate the layout of emacs

;; Copyright (C) 2013  daichirata

;; Author: daichi.hirata <hirata.daichi at gmail.com>
;; Version: 0.1.0
;; Keywords: window, layout
;; URL: https://github.com/daichirata/emacs-rotate

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'cl-seq)

(eval-when-compile (require 'cl-lib))

(defvar rotate-count 0)

(defvar rotate-functions
  '(rotate:even-horizontal
    rotate:even-vertical
    rotate:main-horizontal
    rotate:main-vertical
    rotate:tiled))

;;;###autoload
(defun rotate-layout ()
  (interactive)
  (let* ((len (length rotate-functions))
         (func (elt rotate-functions (% rotate-count len))))
    (prog1 (message "%s" func)
      (call-interactively func)
      (if (>= rotate-count (- len 1))
          (setq rotate-count 0)
        (cl-incf rotate-count)))))

;;;###autoload
(defun rotate-window ()
  (interactive)
  (let* ((sl (reverse (rotate:window-state-list)))
         (nsl (append (cdr sl) (list (car sl)))))
    (cl-loop for w in (rotate:window-list)
          for s in (reverse nsl)
          do (window-state-put s w))
    (select-window (next-window))))

;;;###autoload
(defun rotate:even-horizontal ()
  (interactive)
  (rotate:refresh-window #'rotate:horizontally-n))

;;;###autoload
(defun rotate:even-vertical ()
  (interactive)
  (rotate:refresh-window #'rotate:vertically-n))

;;;###autoload
(defun rotate:main-horizontal ()
  (interactive)
  (rotate:refresh-window #'rotate:main-horizontally-n))

;;;###autoload
(defun rotate:main-vertical ()
  (interactive)
  (rotate:refresh-window #'rotate:main-vertically-n))

;;;###autoload
(defun rotate:main-vertical-right ()
  (interactive)
  (rotate:refresh-window #'rotate:main-vertically-right-n))

;;;###autoload
(defun rotate:tiled ()
  (interactive)
  (rotate:refresh-window #'rotate:tiled-n))

(defun rotate:main-horizontally-n (num)
  (if (<= num 2)
      (split-window-horizontally
       (floor (* (window-width) (/ 2.0 3.0))))
    (split-window-vertically)
    (other-window 1)
    (rotate:horizontally-n (- num 1))))

(defun rotate:main-vertically-n (num)
  (if (<= num 2)
      (split-window-vertically
       (floor (* (window-height) (/ 2.0 3.0))))
    (split-window-horizontally)
    (other-window 1)
    (rotate:vertically-n (- num 1))))

(defun rotate:main-vertically-right-n (num)
  (if (<= num 2)
      (split-window-vertically
       (floor (* (window-height) (/ 1.0 3.0))))
    (split-window-horizontally)
    (other-window 1)
    (rotate:vertically-n (- num 1))))

(defun rotate:horizontally-n (num)
  (if (<= num 2)
      (split-window-horizontally)
    (split-window-horizontally
     (- (window-width) (/ (window-width) num)))
    (rotate:horizontally-n (- num 1))))

(defun rotate:vertically-n (num)
  (if (<= num 2)
      (split-window-vertically)
    (split-window-vertically
     (- (window-height) (/ (window-height) num)))
    (rotate:vertically-n (- num 1))))

(defun rotate:tiled-n (num)
  (cond
   ((<= num 2)
    (split-window-vertically))
   ((<= num 6)
    (rotate:tiled-2column num))
   (t
    (rotate:tiled-3column num))))

(defun rotate:tiled-2column (num)
  (rotate:vertically-n (/ (+ num 1) 2))
  (dotimes (i (/ num 2))
    (split-window-horizontally)
    (other-window 2)))

(defun rotate:tiled-3column (num)
  (rotate:vertically-n (/ (+ num 2) 3))
  (dotimes (i (/ (+ num 1) 3))
    (rotate:horizontally-n 3)
    (other-window 3))
  (when (= (% num 3) 2)
    (other-window -1)
    (delete-window)))

(defun rotate:window-list ()
  (window-list nil nil (minibuffer-window)))

(defun rotate:buffer-list ()
  (mapcar (lambda (w) (window-buffer w)) (rotate:window-list)))

(defun rotate:window-state-list ()
  (mapcar (lambda (w) (window-state-get w)) (rotate:window-list)))

(defun rotate:refresh-window (proc)
  (when (not (one-window-p))
    (let ((window-num (count-windows))
          (state-list (rotate:window-state-list))
          (current-pos (cl-position (selected-window) (rotate:window-list))))
      (delete-other-windows)
      (funcall proc window-num)
      (cl-loop for w in (rotate:window-list)
            for s in state-list
            do (window-state-put s w))
      (select-window (nth current-pos (rotate:window-list))))))

(provide 'rotate)
