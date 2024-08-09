;;; lib/bsmov.el -*- lexical-binding: t; -*-

;; binary-search movement

(require 'hydra)

(defvar bsmov--start-point)
(defvar bsmov--next-lines-move)

(defun bsmov--reset ()
  (message "reset")
  (setf bsmov--start-point nil
        bsmov--next-lines-move nil))

(defun bsmov--forward-line ()
  (interactive)
  (forward-line bsmov--next-lines-move)
  (bsmov--lines-next))

(defun bsmov--backward-line ()
  (interactive)
  (forward-line (- bsmov--next-lines-move))
  (bsmov--lines-next))

(defun bsmov--init ()
  (setf bsmov--start-point (point)))

(defun bsmov--init-lines ()
  (message "init-lines")
  (bsmov--init)
  (let* ((lines (/ (count-lines (point-min) (point-max)) 2)))
    (goto-char (point-min))
    (forward-line (1- lines))
    (setf bsmov--next-lines-move (/ lines 2))))

(defun bsmov--lines-next ()
  (message (format "lines was this %s" bsmov--next-lines-move))
  (let ((l (max 1 (/ bsmov--next-lines-move 2))))
    (setf bsmov--next-lines-move l)
    (message (format "now its this %s; %s" l bsmov--next-lines-move))))

(defun bsmov--confirm ()
  (interactive)
  (setf bsmov--next-lines-move nil))

(defun bsmov--cancel ()
  (interactive)
  (when-let ((p bsmov--start-point))
    (goto-char p)))

(defhydra bsmov-lines (evil-normal-state-map "g C-j"
                                              :hint nil
                                              :body-pre bsmov--init-lines)
  ("k" #'bsmov--backward-line "up" :column "Move")
  ("j" #'bsmov--forward-line "down" :column "Move")
  ("RET" #'bsmov--confirm "confirm" :exit t :column "Actions")
  ("ESC" #'bsmov--cancel "cancel" :exit t :column "Actions")
  ("q" #'bsmov--cancel "cancel" :exit t :column "Actions"))

(provide 'bsmov)
