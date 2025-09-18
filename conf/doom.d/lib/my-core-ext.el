;;; lib/my-core-ext.el -*- lexical-binding: t; -*-

(require 'seq)

(defun my/eq-or-memq (val elt)
  (if (listp val) (memq elt val) (eq elt val)))

(defun my/major-mode-unmapped (&optional mode)
  (let ((mode (or mode major-mode)))
    (or (and (boundp 'major-mode-remap-alist)
             (rassq mode major-mode-remap-alist))
        mode)))

(defun my/tab-bar-tab-name (&optional tab frame)
  (let* ((tab (or tab (tab-bar--current-tab)))
         (explicit-name (alist-get 'explicit-name tab))
         (tab-name (alist-get 'name tab))
         (name (if explicit-name tab-name
                 (let* ((tabs (funcall tab-bar-tabs-function frame))
                        (tab-index (seq-position tabs tab)))
                   (+ 1 tab-index)))))
    (format "%s" name)))

(defun my/maybe-write-backtrace (directory name probability)
  "Write a backtrace to DIRECTORY with NAME and timestamp, with PROBABILITY.
DIRECTORY is the target directory path.
NAME is a string used in the filename.
PROBABILITY is an integer

Returns the filepath if backtrace was written, nil otherwise."
  (when (eq 0 (random probability))
    (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
           (filename (format "%s-%s.trace" name timestamp))
           (filepath (expand-file-name filename directory))
           (backtrace-str (with-output-to-string (backtrace))))
      ;; Ensure directory exists
      (unless (file-directory-p directory)
        (make-directory directory t))
      ;; Write backtrace to file
      (with-temp-file filepath
        (insert (format "Backtrace generated at: %s\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S"))
                "Function: " name "\n"
                "Probability: " (number-to-string probability) "\n"
                "===========================================\n\n"
                backtrace-str))
      (message "Backtrace written to %s" filepath)
      filepath)))
