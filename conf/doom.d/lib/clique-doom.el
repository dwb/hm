;;; lib/clique-doom.el -*- lexical-binding: t; -*-

(require 'clique)

(when (modulep! :ui workspaces)
  (defclique workspace
             (+workspace-contains-buffer-p)))

(defclique real
  (doom-real-buffer-p (current-buffer)))

(provide 'clique-doom)
