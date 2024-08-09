;;; lib/js-tree-sitter-mode.el -*- lexical-binding: t; -*-


(require 'tree-sitter)
(require 'tree-sitter-indent)

(add-to-list 'tree-sitter-major-mode-language-alist
             '(js-tree-sitter-mode . javascript))

(setq tree-sitter-indent-js-tree-sitter-scopes
      '((indent-all . ;; these nodes are always indented
                    (statement_block binary_expression))
        (indent-rest . ;; if parent node is one of this and node is not first → indent
                     (assignment_expression
                      import_statement
                      export_statement
                      switch_statement
                      ))
        (indent-body . ;; if parent node is one of this and current node is in middle → indent
                     (formal_parameters))
        (paren-indent . ;; if parent node is one of these → indent to paren opener
                      ())

        (align-char-to . ;; chaining char → node types we move parentwise to find the first chaining char
                       ((?. . (call_expression field_expression))))
        (aligned-siblings . ;; siblings (nodes with same parent) should be aligned to the first child
                          ())
        (multi-line-text . ;; if node is one of this, then don't modify the indent
                         ;; this is basically a peaceful way out by saying "this looks like something
                         ;; that cannot be indented using AST, so best I leave it as-is"
                         ())
        (outdent . ;; these nodes always outdent (1 shift in opposite direction)
                 ("}"
                  switch_case))))

(setq tree-sitter-indent-js-tree-sitter-scopes-semi-working
      '((indent-all . ;; these nodes are always indented
                    (statement_block switch_case))
        (indent-rest . ;; if parent node is one of this and node is not first → indent
                     ())
        (indent-body . ;; if parent node is one of this and current node is in middle → indent
                     (switch_body formal_parameters))
        (paren-indent . ;; if parent node is one of these → indent to paren opener
                      ())

        (align-char-to . ;; chaining char → node types we move parentwise to find the first chaining char
                       ((?. . (call_expression field_expression))))
        (aligned-siblings . ;; siblings (nodes with same parent) should be aligned to the first child
                          ())
        (multi-line-text . ;; if node is one of this, then don't modify the indent
                         ;; this is basically a peaceful way out by saying "this looks like something
                         ;; that cannot be indented using AST, so best I leave it as-is"
                         ())
        (outdent . ;; these nodes always outdent (1 shift in opposite direction)
                 ("}"))))

(setq js-tree-sitter-indent-offset 2)

;;;###autoload
(define-derived-mode js-tree-sitter-mode js-mode "JavaScript"
  "Major mode for highlighting and (basic) indenting of JavaScript files with tree-sitter."
  :group 'js-tree-sitter-mode

  (tree-sitter-hl-mode)
  (tree-sitter-indent-mode))

(provide 'js-tree-sitter-mode)
