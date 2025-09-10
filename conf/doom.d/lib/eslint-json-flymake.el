;;; eslint-json-flymake.el --- ESLint backend for Flymake using JSON output -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Claude
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (flymake "1.2.2") (json "1.2"))
;; Keywords: languages, javascript, typescript, flymake, eslint
;; URL:

;;; Commentary:

;; This package provides an ESLint backend for Flymake that uses ESLint's
;; JSON output format for more accurate and robust parsing.
;;
;; Features:
;; - Uses ESLint's JSON output format (-f json) for reliable parsing
;; - Highly configurable ESLint command (string, list, or function)
;; - Supports all ESLint severity levels (error, warning)
;; - Proper error handling and process management
;; - Shows rule names in diagnostic messages
;;
;; Usage:
;;
;; To enable this backend, add it to the appropriate mode hooks:
;;
;;   (add-hook 'js-mode-hook #'eslint-json-flymake-setup)
;;   (add-hook 'typescript-mode-hook #'eslint-json-flymake-setup)
;;
;; Configuration:
;;
;; The ESLint command can be configured in several ways:
;;
;; 1. As a string (command name only):
;;    (setq eslint-json-flymake-command "eslint")
;;
;; 2. As a list of strings (command and arguments):
;;    (setq eslint-json-flymake-command '("npx" "eslint" "--cache"))
;;
;; 3. As a function that returns a list of strings:
;;    (setq eslint-json-flymake-command
;;          (lambda ()
;;            (if (file-exists-p "node_modules/.bin/eslint")
;;                '("node_modules/.bin/eslint")
;;              '("npx" "eslint"))))

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

;;; Code:

(require 'flymake)
(require 'json)
(require 'seq)

(defgroup eslint-json-flymake nil
  "ESLint backend for Flymake using JSON output."
  :group 'flymake
  :group 'javascript
  :prefix "eslint-json-flymake-")

(defcustom eslint-json-flymake-command "eslint"
  "ESLint command to run.

This can be one of:
- A string: the command name (e.g., \"eslint\")
- A list of strings: command and arguments (e.g., '(\"npx\" \"eslint\" \"--cache\"))
- A function: called with no arguments, should return a list of strings

The command will automatically have the following arguments added:
- \"-f json\" (to get JSON output)
- \"--stdin\" (to read from stdin)
- \"--stdin-filename FILENAME\" (to specify the filename for context)"
  :type '(choice (string :tag "Command name")
                 (repeat :tag "Command and arguments" string)
                 (function :tag "Function returning command list"))
  :group 'eslint-json-flymake)

(defcustom eslint-json-flymake-show-rule-names t
  "Whether to show ESLint rule names in diagnostic messages."
  :type 'boolean
  :group 'eslint-json-flymake)

(defvar-local eslint-json-flymake--proc nil
  "Current ESLint process for this buffer.")

(defun eslint-json-flymake--build-command (filename)
  "Build the ESLint command list for FILENAME."
  (let ((base-command (cond
                       ((stringp eslint-json-flymake-command)
                        (list eslint-json-flymake-command))
                       ((functionp eslint-json-flymake-command)
                        (funcall eslint-json-flymake-command))
                       ((listp eslint-json-flymake-command)
                        eslint-json-flymake-command)
                       (t
                        (error "Invalid eslint-json-flymake-command: %s"
                               eslint-json-flymake-command)))))
    (append base-command
            (list "-f" "json"
                  "--stdin"
                  "--stdin-filename" filename))))

(defun eslint-json-flymake--diagnostic-for-message (source-buffer msg)
  (let* ((line (alist-get 'line msg))
         (column (alist-get 'column msg))
         (end-line (alist-get 'endLine msg))
         (end-column (alist-get 'endColumn msg))
         (severity (alist-get 'severity msg))
         (message-text (alist-get 'message msg))
         (rule-id (alist-get 'ruleId msg))
         (type (cond ((eq severity 2) :error)
                     ((eq severity 1) :warning)
                     (t :note)))
         (full-message (if (and eslint-json-flymake-show-rule-names
                                rule-id)
                           (format "%s (%s)" message-text rule-id)
                         message-text)))

    ;; Calculate buffer positions
    (when (and line column)
      (let* ((region (flymake-diag-region source-buffer line column))
             (beg (car region))
             (end (cdr region)))

        ;; If we have end position info, try to use it for better precision
        (when (and end-line end-column (not (eq line end-line)))
          (condition-case nil
              (let ((end-region (flymake-diag-region source-buffer end-line end-column)))
                (setq end (cdr end-region)))
            (error nil))) ; Fall back to single-line region
        (flymake-make-diagnostic source-buffer beg end type full-message)))))

(defun eslint-json-flymake--parse-json-output (eslint-buffer source-buffer filename)
  "Parse ESLint JSON output from ESLINT-BUFFER for SOURCE-BUFFER.
Returns a list of Flymake diagnostics."
  (save-mark-and-excursion
    (with-current-buffer eslint-buffer
      (widen)
      (goto-char (point-min))
      (condition-case err
          (let ((json-array-type 'list)
                (json-object-type 'alist)
                (json-key-type 'symbol))
            (thread-last
              (json-read)
              (seq-find #'(lambda (r) (thread-last r (alist-get 'filePath) (equal filename))))
              (alist-get 'messages)
              (seq-map
               (apply-partially #'eslint-json-flymake--diagnostic-for-message source-buffer))
              (seq-filter #'flymake--diag-p)))
        (error
         (flymake-log :error "Failed to parse ESLint JSON output: %s" err))))))

(defun eslint-json-flymake--format-error-message (exit-code stderr-output)
  "Format an error message from ESLint process EXIT-CODE and STDERR-OUTPUT."
  (format "ESLint process failed with exit code %d: %s"
          exit-code
          (if (string-empty-p stderr-output)
              "No error message"
            (string-trim stderr-output))))

(defun eslint-json-flymake-backend (report-fn &rest _args)
  "ESLint backend function for Flymake.
Calls REPORT-FN with a list of diagnostics or error information."
  (when (and eslint-json-flymake--proc
             (process-live-p eslint-json-flymake--proc))
    (kill-process eslint-json-flymake--proc))

  (let ((source-buffer (current-buffer))
        (filename (or (buffer-file-name)
                      (format "stdin.%s"
                              (cond ((derived-mode-p 'typescript-mode) "ts")
                                    ((derived-mode-p 'tsx-mode) "tsx")
                                    ((derived-mode-p 'js-mode) "js")
                                    ((derived-mode-p 'js2-mode) "js")
                                    ((derived-mode-p 'rjsx-mode) "jsx")
                                    (t "js"))))))

    (save-restriction
      (widen)
      (condition-case err
          (let* ((command (eslint-json-flymake--build-command filename))
                 (stdout-buffer (generate-new-buffer " *eslint-json-flymake*"))
                 (stderr-buffer (generate-new-buffer " *eslint-json-flymake-stderr*")))

            (flymake-log :debug "Running ESLint command: %s" (string-join command " "))

            (setq eslint-json-flymake--proc
                  (make-process
                   :name "eslint-json-flymake"
                   :noquery t
                   :connection-type 'pipe
                   :buffer stdout-buffer
                   :stderr stderr-buffer
                   :command command
                   :sentinel
                   (lambda (proc event)
                     (when (memq (process-status proc) '(signal exit))
                       (unwind-protect
                           (if (with-current-buffer source-buffer
                                 (eq proc eslint-json-flymake--proc))
                               (let ((exit-code (process-exit-status proc))
                                     (stderr-output (with-current-buffer stderr-buffer
                                                      (buffer-string))))

                                 (flymake-log :debug "ESLint finished with exit code %d" exit-code)
                                 (flymake-log :debug "ESLint stdout to: %s" stdout-buffer)
                                 (when (not (string-empty-p stderr-output))
                                   (flymake-log :debug "ESLint stderr: %s" stderr-output))

                                 (cond
                                  ;; Success (exit code 0) or linting issues found (exit code 1)
                                  ((or (eq exit-code 0) (eq exit-code 1))
                                   (let ((diagnostics (eslint-json-flymake--parse-json-output
                                                       stdout-buffer source-buffer filename)))
                                     (funcall report-fn diagnostics)))

                                  ;; ESLint configuration or other error
                                  (t
                                   (funcall report-fn
                                            :panic
                                            (eslint-json-flymake--format-error-message
                                             exit-code stderr-output)))))

                             (flymake-log :warning "Canceling obsolete ESLint check %s" proc))

                         ;; Cleanup
                         (dolist (buf (list stdout-buffer stderr-buffer))
                           (when (buffer-live-p buf)
                             (kill-buffer buf))))))))

            ;; Send buffer content to ESLint process
            (process-send-region eslint-json-flymake--proc (point-min) (point-max))
            (process-send-eof eslint-json-flymake--proc))

        (error
         (flymake-log :error "Failed to start ESLint process: %s" err)
         (funcall report-fn :panic (format "Failed to start ESLint: %s" err)))))))

;;;###autoload
(defun eslint-json-flymake-setup ()
  "Set up ESLint backend for Flymake in the current buffer."
  (add-hook 'flymake-diagnostic-functions #'eslint-json-flymake-backend nil t))

;; For backward compatibility
(define-obsolete-function-alias 'eslint-json-flymake-setup-backend
  #'eslint-json-flymake-setup "1.0.0")

(provide 'eslint-json-flymake)

;;; eslint-json-flymake.el ends here
