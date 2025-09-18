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
  (buffn nil :read-only t)
  (intensity 0 :read-only t))

(setq clique-map (make-hash-table :test 'eq))
(setq clique-count 0)

(cl-defmacro defclique (name &rest body)
  "Define a clique NAME.
BODY is evaluated with the candidate buffer current.  It should return:
  nil — buffer is not part of this clique
  t   — binary membership (represented as the symbol NAME)
  any other value — grouping discriminator (represented as (NAME . VALUE))

Keyword arguments may precede the body:
  :intensity N  — weight for clique-set ordering (default 0)"
  (let ((intensity 0))
    (while (keywordp (car body))
      (pcase (pop body)
        (:intensity (setq intensity (pop body)))))
    `(let ((symname (quote ,name)))
       (prog1 symname
         (let* ((buffn (lambda (b) (with-current-buffer b (progn ,@body))))
                (newclique (make-clique :name symname :buffn buffn :intensity ,intensity)))
           (puthash symname newclique clique-map)
           (cl-incf clique-count))))))

(defun clique-cliques-of-buffer (&optional buffer-or-name any-cliques all-cliques)
  "Return the clique memberships of BUFFER-OR-NAME.
Each element is either a symbol (binary clique) or a cons cell
\(NAME . VALUE) for cliques with a grouping discriminator."
  (let ((buffer (get-buffer (or buffer-or-name (current-buffer))))
        (cliques))
    (maphash
     (lambda (_name clq)
       (when-let* ((result (funcall (clique-buffn clq) buffer))
                   (rn (if (eq result t)
                           (clique-name clq)
                         (cons (clique-name clq) result)))
                   ((or (null any-cliques)
                        (seq-contains-p any-cliques rn #'clique--buffer-clique-eq))))
         (push rn cliques)))
     clique-map)
    (when (xor (null all-cliques)
               (null (seq-difference all-cliques cliques #'clique--buffer-clique-eq)))
      cliques)))

(defun clique--entry-name (entry)
  "Return the clique name from a membership ENTRY (symbol or cons cell)."
  (if (consp entry) (car entry) entry))

(cl-defun clique-current-buffer-friends
    (&key
     (via-any nil)
     (via-all nil)
     (buffers (buffer-list))
     (having nil)
     (top-groups 0)
     (min-intensity 0))
  "Return a list of buffers taken from BUFFERS which share at least one clique
with the current buffer, ordered stably such that the buffers that share the
most cliques are first."
  (let* ((buffer (current-buffer))
         (having (if (functionp having) having
                   (let ((required (ensure-list having)))
                     (lambda (bc) (cl-subsetp required bc :test #'equal)))))
         (bufcliques (clique-cliques-of-buffer buffer via-any via-all))
         (groups (make-vector clique-count nil)))
    (seq-do #'(lambda (b)
                (when-let* ((bc (clique-cliques-of-buffer b via-any via-all))
                            (bcinter (seq-intersection bufcliques bc #'clique--buffer-clique-eq))
                            ((and
                              (not (eq b buffer))
                              (or (not having) (funcall having bc))
                              (<= min-intensity (clique--set-intensity bcinter)))))
                  (push b (elt groups (length bcinter)))))
            buffers)
    (setq groups (seq-reverse (seq-filter #'identity groups)))
    (seq-reduce #'(lambda (g out) (append out g))
                  (if (> top-groups 0) (seq-take groups top-groups) groups)
                  nil)))

(defun clique--set-intensity (s)
  (seq-reduce #'(lambda (total entry)
                  (let ((clique (gethash (clique--entry-name entry) clique-map)))
                    (+ total (if clique (clique-intensity clique) 0))))
              s 0))

(cl-defun clique--set-eq (a b)
  (seq-set-equal-p a b #'equal))

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

(defun clique--buffer-clique-name (c)
  (cond ((consp c) (car c))
        ((symbolp c) c)
        (t (error "buffer clique must be a cons cell or symbol"))))

(defun clique--buffer-clique-eq (a b)
  (cond ((and (symbolp a) (symbolp b)) (eq a b))
        ((and (consp a) (consp b)) (equal a b))
        (t (eq (clique--buffer-clique-name a)
               (clique--buffer-clique-name b)))))

(ert-deftest clique-set-test () :tags '(clique)
             (should (clique--set-eq '(asdf qwer))))


(defclique file
  (when (buffer-file-name) t))

(defclique prog
  (when (derived-mode-p 'prog-mode) t))

(defclique conf
  (when (derived-mode-p 'conf-mode) t))

(defclique term
  (when (or
         (derived-mode-p 'comint-mode)
         (and (symbol-function 'vterm-mode) (derived-mode-p 'vterm-mode)))
    t))

(defclique special
  (when (derived-mode-p 'special-mode) t))

(defclique windowprev
  :intensity 100
  (when (seq-find
         #'(lambda (bpos) (eq (car bpos) (current-buffer)))
         (window-prev-buffers))
    t))

(defclique project
           :intensity 50
           (when-let* ((p (project-current)))
             (project-name p)))

(defun clique-test ()
  "Run clique.el unit tests"
  (interactive)
  (ert '(tag clique)))

(provide 'clique)
