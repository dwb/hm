;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

;; overrides

(package! projectile :pin "271007c6611fcb08ddd326d7de9727c2ad5ef265")

;; required for config

(package! s)

;; new stuff

;; Automatically compile Emacs Lisp libraries
;; not actually using this so disable for now
;; (package! auto-compile :pin "4cbd304698a897baf438400c9a2b31d3dfb3a7f9")

(package! posframe)

;; (package! flow-js2-mode :pin "7520bd")
(package! prettier)                     ; not pinned cos it needs to be from melpa
(package! deadgrep)
(package! protobuf-mode)
(package! literate-calc-mode)
(package! org-jira)
(package! org-preview-html)
(package! org-modern)

(package! evil-owl)

(package! nushell-ts-mode :recipe (:host github :repo "herbertjones/nushell-ts-mode"))

(package! add-node-modules-path)

(package! hyperbole)

(package! combobulate
  :recipe (:host github
           :repo "mickeynp/combobulate"
           :branch "main")
  :pin "06db63fa4eda00d7ef2ccba21c507b9dc1e21ef0")


(when (>= emacs-major-version 30)
  (package! eldoc :built-in t)
  (package! track-changes :built-in t))

(package! eldoc-box :pin "ebc0e2c13791f5a22cf81be050b32f0ebf726855")

(package! gptai
  :recipe (:host github :repo "antonhibl/gptai"
           :branch "main")
  :pin "5cdea5c85b102e1e57904ea6bb826cccd506067f")

(package! chatgpt-shell
  :recipe
  (:files ("*.el")))

(package! org-ai)
(package! gptel)

(package! norns :pin "022b433334cd1db4f83e77f055cd89e9f857076d")

(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))

(package! vterm
  :pin "f1c0c57484b2082586ed3c0d34ca9da995b0ef1f"
  :recipe
  (:host github
         :repo "dwb/emacs-libvterm"
         :branch "nix"
         :files ("CMakeLists.txt"
                 "flake.nix"
                 "flake.lock"
                 "elisp.c"
                 "elisp.h"
                 "emacs-module.h"
                 "etc"
                 "utf8.c"
                 "utf8.h"
                 "vterm.el"
                 "vterm-module.c"
                 "vterm-module.h")))

(package! evil-textobj-tree-sitter)

(package! gotest)

(package! treesit-auto :pin "b5fcf8e5515c5c5787073c1bc3f6f2bf5bfb1cf1")

(package! unison-ts-mode
  :pin "04cbd1f73f94346e68f9b42f8ab9d7ab8ab43ad3"
  :recipe
  (:host github
   :repo "fmguerreiro/unison-ts-mode"
   :branch "main"))

(package! unison-daemon
  :pin "270f8305be32c8a34fbf9f88f4dfdb13ebae6e9c"
  :recipe
  (:host github
   :repo "jmibanez/unison-daemon-el"
   :branch "main"))

;; disable / not really using / TODO: update to just-ts-mode and pin
;; (package! just-mode)

(package! lilypond-mode
  :pin "b7908403efe0761cec86c94db0c529db031246a0" ; 2.24.4
  :recipe
  (:type git
   :repo "https://git.savannah.gnu.org/git/lilypond.git/"
   :files ("elisp/*.el")
   :pre-build ("nix" "shell" "nixpkgs#python3" "-c" "python" "scripts/build/lilypond-words.py" "--el" "--dir=elisp/")))
