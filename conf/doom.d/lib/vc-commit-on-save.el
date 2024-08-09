;;; lib/vc-commit-on-save.el -*- lexical-binding: t; -*-

(defun vc-commit-on-save/auto-commit-orig ()
  (if-let* ((filename (buffer-file-name))
            (vc-fileset (vc-deduce-fileset nil t 'state-model-only-files))
            (backend (car vc-fileset))
            (msg (concat "auto " (time-stamp-string))))
      (vc-checkin (list filename) backend msg)
    (warn "vc-commit-on-save-mode: can't commit this inappropriate file")))

(defun vc-commit-on-save/ (verbose)
  "Do the next logical version control operation on the current fileset.
This requires that all files in the current VC fileset be in the
same state.  If not, signal an error.

For merging-based version control systems:
  If every file in the VC fileset is not registered for version
   control, register the fileset (but don't commit).
  If every work file in the VC fileset is added or changed, pop
   up a *vc-log* buffer to commit the fileset.
  For a centralized version control system, if any work file in
   the VC fileset is out of date, offer to update the fileset.

For old-style locking-based version control systems, like RCS:
  If every file is not registered, register the file(s).
  If every file is registered and unlocked, check out (lock)
   the file(s) for editing.
  If every file is locked by you and has changes, pop up a
   *vc-log* buffer to check in the changes.  Leave a
   read-only copy of each changed file after checking in.
  If every file is locked by you and unchanged, unlock them.
  If every file is locked by someone else, offer to steal the lock."
  (interactive "P")
  (let* ((vc-fileset (vc-deduce-fileset nil t 'state-model-only-files))
         (backend (car vc-fileset))
         (files (nth 1 vc-fileset))
         ;; (fileset-only-files (nth 2 vc-fileset))
         ;; FIXME: We used to call `vc-recompute-state' here.
         (state (nth 3 vc-fileset))
         ;; The backend should check that the checkout-model is consistent
         ;; among all the `files'.
         (model (nth 4 vc-fileset)))

    ;; If a buffer has unsaved changes, a checkout would discard those
    ;; changes, so treat the buffer as having unlocked changes.
    (when (and (not (eq model 'implicit)) (eq state 'up-to-date))
      (dolist (file files)
        (let ((buffer (get-file-buffer file)))
          (and buffer
               (buffer-modified-p buffer)
               (setq state 'unlocked-changes)))))

    ;; Do the right thing.
    (cond
     ((eq state 'missing)
      (error "Fileset files are missing, so cannot be operated on"))
     ((eq state 'ignored)
      (error "Fileset files are ignored by the version-control system"))
     ((or (null state) (eq state 'unregistered))
      (cond (verbose
             (let ((backend (vc-read-backend "Backend to register to: ")))
               (vc-register (cons backend (cdr vc-fileset)))))
            (t
             (vc-register vc-fileset))))
     ;; Files are up-to-date, or need a merge and user specified a revision
     ((or (eq state 'up-to-date) (and verbose (eq state 'needs-update)))
      (cond
       (verbose
        ;; Go to a different revision.
        (let* ((revision
                ;; FIXME: Provide completion.
                (read-string "Branch, revision, or backend to move to: "))
               (revision-downcase (downcase revision)))
          (if (member
               revision-downcase
               (mapcar (lambda (arg) (downcase (symbol-name arg)))
                       vc-handled-backends))
              (let ((vsym (intern-soft revision-downcase)))
                (dolist (file files) (vc-transfer-file file vsym)))
            (dolist (file files)
              (vc-checkout file revision)))))
       ((not (eq model 'implicit))
        ;; check the files out
        (dolist (file files) (vc-checkout file)))
       (t
        ;; do nothing
        (message "Fileset is up-to-date"))))
     ;; Files have local changes
     ((vc-compatible-state state 'edited)
      (let ((ready-for-commit files))
        ;; CVS, SVN and bzr don't care about read-only (bug#9781).
        ;; RCS does, SCCS might (someone should check...).
        (when (memq backend '(RCS SCCS))
          ;; If files are edited but read-only, give user a chance to correct.
          (dolist (file files)
            ;; If committing a mix of removed and edited files, the
            ;; fileset has state = 'edited.  Rather than checking the
            ;; state of each individual file in the fileset, it seems
            ;; simplest to just check if the file exists.	 Bug#9781.
            (when (and (file-exists-p file) (not (file-writable-p file)))
              ;; Make the file-buffer read-write.
              (unless (y-or-n-p (format "%s is edited but read-only; make it writable and continue? " file))
                (error "Aborted"))
              ;; Maybe we somehow lost permissions on the directory.
              (condition-case nil
                  (set-file-modes file (logior (file-modes file) 128))
                (error (error "Unable to make file writable")))
              (let ((visited (get-file-buffer file)))
                (when visited
                  (with-current-buffer visited
                    (read-only-mode -1)))))))
        ;; Allow user to revert files with no changes
        (save-excursion
          (dolist (file files)
            (let ((visited (get-file-buffer file)))
              ;; For files with locking, if the file does not contain
              ;; any changes, just let go of the lock, i.e. revert.
              (when (and (not (eq model 'implicit))
                         (eq state 'up-to-date)
                         ;; If buffer is modified, that means the user just
                         ;; said no to saving it; in that case, don't revert,
                         ;; because the user might intend to save after
                         ;; finishing the log entry and committing.
                         (not (and visited (buffer-modified-p))))
                (vc-revert-file file)
                (setq ready-for-commit (delete file ready-for-commit))))))
        ;; Remaining files need to be committed
        (if (not ready-for-commit)
            (message "No files remain to be committed")
          (if (not verbose)
              (vc-checkin ready-for-commit backend)
            (let* ((revision (read-string "New revision or backend: "))
                   (revision-downcase (downcase revision)))
              (if (member
                   revision-downcase
                   (mapcar (lambda (arg) (downcase (symbol-name arg)))
                           vc-handled-backends))
                  (let ((vsym (intern revision-downcase)))
                    (dolist (file files) (vc-transfer-file file vsym)))
                (vc-checkin ready-for-commit backend nil nil revision)))))))
     ;; locked by somebody else (locking VCSes only)
     ((stringp state)
      ;; In the old days, we computed the revision once and used it on
      ;; the single file.  Then, for the 2007-2008 fileset rewrite, we
      ;; computed the revision once (incorrectly, using a free var) and
      ;; used it on all files.  To fix the free var bug, we can either
      ;; use `(car files)' or do what we do here: distribute the
      ;; revision computation among `files'.  Although this may be
      ;; tedious for those backends where a "revision" is a trans-file
      ;; concept, it is nonetheless correct for both those and (more
      ;; importantly) for those where "revision" is a per-file concept.
      ;; If the intersection of the former group and "locking VCSes" is
      ;; non-empty [I vaguely doubt it --ttn], we can reinstate the
      ;; pre-computation approach of yore.
      (dolist (file files)
        (vc-steal-lock
         file (if verbose
                  (read-string (format "%s revision to steal: " file))
                (vc-working-revision file))
         state)))
     ;; conflict
     ((eq state 'conflict)
      ;; FIXME: Is it really the UI we want to provide?
      ;; In my experience, the conflicted files should be marked as resolved
      ;; one-by-one when saving the file after resolving the conflicts.
      ;; I.e. stating explicitly that the conflicts are resolved is done
      ;; very rarely.
      (vc-mark-resolved backend files))
     ;; needs-update
     ((eq state 'needs-update)
      (dolist (file files)
        (if (yes-or-no-p (format
                          "%s is not up-to-date.  Get latest revision? "
                          (file-name-nondirectory file)))
            (vc-checkout file t)
          (when (and (not (eq model 'implicit))
                     (yes-or-no-p "Lock this revision? "))
            (vc-checkout file)))))
     ;; needs-merge
     ((eq state 'needs-merge)
      (dolist (file files)
        (when (yes-or-no-p (format
                            "%s is not up-to-date.  Merge in changes now? "
                            (file-name-nondirectory file)))
          (vc-maybe-resolve-conflicts
           file (vc-call-backend backend 'merge-news file)))))

     ;; unlocked-changes
     ((eq state 'unlocked-changes)
      (dolist (file files)
        (when (not (equal buffer-file-name file))
          (find-file-other-window file))
        (if (save-window-excursion
              (vc-diff-internal nil
                                (cons (car vc-fileset) (cons (cadr vc-fileset) (list file)))
                                (vc-working-revision file) nil)
              (goto-char (point-min))
              (let ((inhibit-read-only t))
                (insert
                 (format "Changes to %s since last lock:\n\n" file)))
              (not (beep))
              (yes-or-no-p (concat "File has unlocked changes.  "
                                   "Claim lock retaining changes? ")))
            (progn (vc-call-backend backend 'steal-lock file)
                   (clear-visited-file-modtime)
                   (write-file buffer-file-name)
                   (vc-mode-line file backend))
          (if (not (yes-or-no-p
                    "Revert to checked-in revision, instead? "))
              (error "Checkout aborted")
            (vc-revert-buffer-internal t t)
            (vc-checkout file)))))
     ;; Unknown fileset state
     (t
      (error "Fileset is in an unknown state %s" state)))))


(define-minor-mode vc-commit-on-save-mode
  "vc-checkin on save"
  :lighter " CommitOnSave"
  (if vc-commit-on-save-mode
      (add-hook 'after-save-hook #'vc-commit-on-save/auto-commit nil t)
    (remove-hook 'after-save-hook #'vc-commit-on-save/auto-commit t)))

(provide 'vc-commit-on-save)
