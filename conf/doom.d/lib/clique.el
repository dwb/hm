;;; clique.el --- Buffers form cliques               -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Dan Brown

;; Author: Dan Brown <dan@stompydan.net>
;;

;; In `display-buffer`, we take a buffer and we want to choose in which window to put it.
;; In `switch-to-prev-buffer`, we take a buffer that is to be switched from, in a particular window.
;;
;; What is the common factor?

(require 'cl-lib)
(require 'ert)

(cl-defstruct clique
  (name nil :read-only t)
  (bufpred nil :read-only t)
  (intensity 0 :read-only t))

(setq clique-map (make-hash-table :test 'eq))
(setq clique-count 0)

(cl-defmacro defclique (name &key pred (intensity 0))
  `(let ((symname (quote ,name)))
     (prog1 symname
       (let* ((bufpred (lambda (b) (with-current-buffer b (funcall ,pred))))
              (newclique (make-clique :name symname :bufpred bufpred :intensity 0)))
         (puthash symname newclique clique-map)
         (cl-incf clique-count)))))

(defun clique-cliques-of-buffer (&optional buffer-or-name)
  (let ((buffer (get-buffer (or buffer-or-name (current-buffer))))
        (cliques))
    (maphash
     (lambda (name clq) (when (funcall (clique-bufpred clq) buffer)
                          (push name cliques)))
     clique-map)
    cliques))

(cl-defun clique-current-buffer-friends
    (&key (buffers (buffer-list))
          (having nil)
          (top-groups 0)
          (min-intensity 0))
  "Return a list of buffers taken from BUFFERS which share at least one clique
with BUFFER-OR-NAME, ordered stably such that the buffers that share the most
cliques are first."
  (let* ((buffer (current-buffer))
         (having (if (functionp having) having
                   (apply-partially #'cl-subsetp (ensure-list having))))
         (bufcliques (clique-cliques-of-buffer buffer))
         (groups (make-vector clique-count nil)))
    (seq-do #'(lambda (b)
                (when-let ((bc (clique-cliques-of-buffer b))
                           (bcinter (seq-intersection bufcliques bc)))
                  (when (and
                         (not (eq b buffer))
                         (or (not having) (funcall having bc))
                         (<= min-intensity (clique--set-intensity bcinter)))
                    (push b (elt groups (length bcinter))))))
            buffers)
    (setq groups (seq-reverse (seq-filter #'identity groups)))
    (seq-reduce #'(lambda (g out) (append out g))
                  (if (> top-groups 0) (seq-take groups top-groups) groups)
                  nil)))

(defun clique--set-intensity (s)
  (seq-reduce #'(lambda (total cn)
                  (let ((clique (gethash cn clique-map)))
                    (+ total (if clique (clique-intensity clique) 0))))
              s 0))

(cl-defun clique--set-eq (a b)
  (seq-set-equal-p a b #'eq))

(defun clique--set-lt (a b)
  "< function for clique sets. Clique sets are consistently-ordered lists of clique symbol-names."
  (let ((lena (length a))
        (lenb (length b)))
    (if (not (eq lena lenb))
        (< lena lenb)
      (let ((ia (clique--set-intensity a))
            (ib (clique--set-intensity b)))
        (< ia ib)))))

(defun clique--set-gt (a b)
  (not (clique--set-lt a b)))

(ert-deftest clique-set-test () :tags '(clique)
             (should (clique--set-eq '(asdf qwer))))


(defclique file
  :pred #'buffer-file-name)

(defclique prog
  :pred #'(lambda ()
            (derived-mode-p 'prog-mode)))

(defclique conf
  :pred #'(lambda ()
            (derived-mode-p 'conf-mode)))

(defclique term
  :pred #'(lambda ()
            (or
             (derived-mode-p 'comint-mode)
             (and (symbol-function 'vterm-mode) (derived-mode-p 'vterm-mode)))))

(defclique special
  :pred #'(lambda ()
            (derived-mode-p 'special-mode)))

(defclique windowprev
  :intensity 1
  :pred #'(lambda ()
            (let ((buf (current-buffer)))
              (seq-find
               #'(lambda (bpos) (eq (car bpos) buf))
               (window-prev-buffers)))))

(defun clique-test ()
  "Run clique.el unit tests"
  (interactive)
  (ert '(tag clique)))

(provide 'clique)
