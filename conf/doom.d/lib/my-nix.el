;;; lib/my-nix.el -*- lexical-binding: t; -*-

(defvar my/nix-present 'unknown)

(defun my/nix-present-p ()
  (when (executable-find "nix")
    (thread-last "nix store ping"
                 (shell-command-to-string)
                 (string-trim))))

(defmacro my/if-nix (if-present &rest if-absent)
  "Evaluates IF-PRESENT if nix is present on this system;
   IF-ABSENT (in an implicit progn) otherwise."

  `(if (my/nix-present-p) ,if-present (progn ,@if-absent)))

(defun my/nix-bin-path (attr)
  (thread-last attr
               (format "nix build --no-link --print-out-paths 'nixpkgs#%s' | head -n 1 2>/dev/null")
               (shell-command-to-string)
               (string-trim)
               ((lambda (s) (concat s "/bin")))
               (file-name-as-directory)
               ((lambda (p)
                  (if (file-directory-p p)
                      p
                    (user-error "my/nix-bin-path: %s doesn't output a bin directory" attr))))))

(defun my/gimme (attr)
  "Add the NixPkg given in ATTR to the environment."
  (add-to-list 'exec-path (my/nix-bin-path attr)))

(provide 'my-nix)
