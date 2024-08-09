;;; lib/go-ts.el -*- lexical-binding: t; -*-

(defun my/setup-go-ts-mode ()
  (set-docsets! 'go-ts-mode "Go")
  (set-repl-handler! 'go-ts-mode #'gorepl-run)
  ;; (set-lookup-handlers! 'go-ts-mode
  ;;   :definition #'go-guru-definition
  ;;   :references #'go-guru-referrers
  ;;   :documentation #'godoc-at-point)

  (map! :map go-ts-mode-map
        :localleader
        "a" #'go-tag-add
        "d" #'go-tag-remove
        "e" #'+go/play-buffer-or-region
        "i" #'go-goto-imports      ; Go to imports
        (:prefix ("ri" . "imports")
          "a" #'go-import-add
          "r" #'go-remove-unused-imports)
        (:prefix ("b" . "build")
          :desc "go run ." "r" (cmd! (compile "go run ."))
          :desc "go build" "b" (cmd! (compile "go build"))
          :desc "go clean" "c" (cmd! (compile "go clean")))
        (:prefix ("t" . "test")
          "t" #'+go/test-rerun
          "a" #'+go/test-all
          "s" #'+go/test-single
          "n" #'+go/test-nested
          "f" #'+go/test-file
          "g" #'go-gen-test-dwim
          "G" #'go-gen-test-all
          "e" #'go-gen-test-exported
          (:prefix ("b" . "bench")
            "s" #'+go/bench-single
            "a" #'+go/bench-all))))

(provide 'go-ts)
