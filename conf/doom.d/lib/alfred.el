;;; lib/alfred.el -*- lexical-binding: t; -*-

(require 'server)

(defmacro eval-printing-string-at (server &rest body)
  `(princ
    (format "%s\n" (server-eval-at ,server (quote ,@body)))))

(defmacro eval-printing-string (&rest body) `(eval-printing-string-at "server" ,@body))

(defun persp->alfred-item (p)
  (let ((n (persp-name p)))
    `((title . ,n)
      (arg . ,n))))

(defun persps->alfred-response ()
  `((items . ,(seq-map #'persp->alfred-item (seq-filter #'identity (persp-persps))))))

(defun named-tabs->alfred-item (t)
  (let ((n (alist-get 'name t)))
    `((title . ,n)
      (arg . ,n))))

(defun named-tabs->alfred-response ()
  `((items . ,(->> (tab-bar-tabs)
               (seq-filter #'(lambda (t) (alist-get 'name t)))
               (seq-map #'named-tabs->alfred-item)))))

(defun str->alfred-item (s)
  (let ((n (format "%s" s)))
    `((title . ,n)
      (arg . ,n))))

(defun cons->alfred-item (c)
  `((title . ,(format "%s" (car c)))
    (arg . ,(format "%s" (cdr c)))))

(provide 'alfred)
