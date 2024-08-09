;;; eslint-flymake --- An ESLint backend for Flymake.   -*- lexical-binding: t -*-

;; Copyright (C) 2019 Javier Olaechea

;; Author: Javier Olaechea <pirata@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: javascript, languages, flymake
;; URL: http://github.com/emacs-pe/eslint-flymake

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides an ESLint backend for Flymake.
;; To enable it add the following to your init.el
;;
;;   (add-hook 'js-mode-hook 'eslint-flymake-setup-backend)
;;
;; Ideas for further development
;; - 'eslint-flymake-explain-diagnostic' to open rule explanation in
;;   the users browser.

;;; Code:

(require 'flymake)
(require 'json)
(require 'seq)

(defgroup eslint-flymake nil
  "Flymake backend for ESLint"
  :group 'programming
  :prefix "eslint-flymake-")

(defvar-local eslint-flymake-proc nil)

(defcustom eslint-flymake-command '("eslint")
  "The `eslint' command along with the arguments it should be called with."
  :type '(repeat  string)
  :group 'eslint-flymake)

;;; Public

(defun eslint-flymake (report-fn &rest _args)
  (when (process-live-p eslint-flymake-proc)
    (kill-process eslint-flymake-proc))

  (let* ((source-buffer (current-buffer))
         (file-name (buffer-file-name))
         (cmd (append
               eslint-flymake-command
               (list "-f" "json" "--stdin")
               (when file-name (list "--stdin-filename" file-name))
               (when (and (not file-name)
                          (derived-mode-p 'typescript-mode 'typescript-ts-base-mode))
                 (list "--parser" "@typescript-eslint/parser")))))
    (save-restriction
      (widen)
      (flymake-log :debug "Running %s" cmd)
      (setq eslint-flymake-proc
            (make-process :name "eslint-flymake"
                          :noquery t
                          :buffer (generate-new-buffer "*eslint-flymake*")
                          :connection-type 'pipe
                          :command cmd
                          :sentinel (eslint-flymake--make-sentinel source-buffer file-name report-fn)))
      (process-send-region eslint-flymake-proc (point-min) (point-max))
      (process-send-eof eslint-flymake-proc))))

(defun eslint-flymake-setup-backend ()
  (add-hook 'flymake-diagnostic-functions 'eslint-flymake nil t))

;;; Private

(defun eslint-flymake--make-sentinel (source-buffer file-name report-fn)
  (lambda (proc _event)
    (when (memq (process-status proc) '(signal exit))
      (unwind-protect
          (if (with-current-buffer source-buffer (eq proc eslint-flymake-proc))
              (with-current-buffer (process-buffer proc)
                (goto-char (point-min))
                (let* ((json-array-type 'vector)
                       (files (json-read))
                       (file (seq-find
                              #'(lambda (file) (string= (alist-get 'filePath file)
                                                        file-name))
                              files)))
                  (funcall report-fn
                           (when file
                             (seq-map (apply-partially #'eslint-flymake--diagnostic-from-message
                                       source-buffer)
                                      (alist-get 'messages file))))))

            (flymake-log :warning "Canceling obsolete check %s"
                         proc))

        (kill-buffer (process-buffer proc))))))

(defun eslint-flymake--diagnostic-from-message (source-buffer message)
  (let* ((line (alist-get 'line message))
         (col (alist-get 'column message))
         (region (flymake-diag-region source-buffer line col))
         (text (alist-get 'message message))
         (rule (alist-get 'ruleId message))
         (severity (pcase (alist-get 'severity message)
                     (1 :warning)
                     (2 :error)
                     (_ :note))))
    (flymake-make-diagnostic source-buffer
                             (car region) (cdr region)
                             severity
                             (format "%s: %s" rule text))))

;;; Setup

(with-eval-after-load 'js
  (if (boundp 'js-base-mode-hook)
      (add-hook 'js-base-mode-hook #'eslint-flymake-setup-backend)
    (add-hook 'js-mode-hook #'eslint-flymake-setup-backend)))

(with-eval-after-load 'typescript-ts-mode
  (add-hook 'typescript-ts-base-mode-hook #'eslint-flymake-setup-backend))

(provide 'eslint-flymake)
