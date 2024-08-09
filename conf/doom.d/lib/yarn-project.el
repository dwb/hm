;;; yarn-project.el --- Helping out with yarn projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Dan Brown

;; Author: Dan Brown <dan@stompydan.net>
;; Keywords:

(require 'add-node-modules-path)

(defun yarn-project/add-node-modules-path (oldfun)
  (if-let ((yarnlockdir (locate-dominating-file default-directory "yarn.lock")))
      (let ((default-directory yarnlockdir))
        (funcall oldfun))
    (funcall oldfun)))

(define-minor-mode yarn-project-mode
  "Make various things aware of yarn projects"
  :global t

  (if yarn-project-mode
      (prog1 t
        (advice-add 'add-node-modules-path :around 'yarn-project/add-node-modules-path ))

    (advice-remove 'add-node-modules-path 'yarn-project/add-node-modules-path)))

(provide 'yarn-project)
