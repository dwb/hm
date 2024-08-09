;;; lib/evil-tree-sitter.el -*- lexical-binding: t; -*-

(defun tree-sitter-node-at-position (pos)
  (let ((root (tsc-root-node tree-sitter-tree)))
    (tsc-get-descendant-for-position-range root pos pos)))

(defun tree-sitter-set-region-to-node (node)
  (let ((start (tsc-node-start-position node))
        (end (- (tsc-node-end-position node) 0)))
    (deactivate-mark)
    (push-mark start t t)
    (goto-char end)
    (while (not (tsc-node-eq node (tree-sitter-node-at-point)))
      (backward-char))))

(defun tree-sitter-set-region-to-node-at-point (&optional node-type)
  (interactive)
  (let ((node (tree-sitter-node-at-point node-type)))
    (tree-sitter-set-region-to-node node)))

(defun tree-sitter-region-matches-node-at-point-p ()
  (let* ((node (tree-sitter-node-at-point))
         (start (tsc-node-start-position node))
         (end (- (tsc-node-end-position node) 1)))
    (and (eq start (region-beginning)) (eq end (region-end)))))

(defun blah ()
  (let* ((node (tree-sitter-node-at-point))
         (start (tsc-node-start-position node))
         (end (- (tsc-node-end-position node) 0)))
    (list (region-beginning) (- (region-end) 0) start end)))

(defun tree-sitter-set-region-to-parent-of-node-at-point (&optional node-type)
  (interactive)
  (let ((node (tsc-get-parent (tree-sitter-node-at-point node-type))))
    (tree-sitter-set-region-to-node node)))

(provide 'evil-tree-sitter)
