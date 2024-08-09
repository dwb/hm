;;; lib/clique-doom.el -*- lexical-binding: t; -*-

(require 'clique)

(when (modulep! :ui workspaces)
  (defclique workspace
    :pred #'(lambda () (+workspace-contains-buffer-p))))

(defclique real
  :pred #'(lambda () (doom-real-buffer-p (current-buffer))))

(provide 'clique-doom)
