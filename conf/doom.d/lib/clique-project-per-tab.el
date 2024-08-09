;;; lib/clique-project-per-tab.el -*- lexical-binding: t; -*-

(require 'clique)
(require 'project-per-tab)

(defclique tabproject
           :pred #'(lambda ()
                     (when-let ((proj (project-per-tab-project-of-tab))
                                (buf (current-buffer)))
                       (memq buf (project-buffers proj)))))

(provide 'clique-project-per-tab)
