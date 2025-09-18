;;; lib/my-jj-squash.el -*- lexical-binding: t; -*-

;; Move some of the changes in the jj working copy @ into an ancestor.
;;
;; Show the changes in @ with `vc-root-diff' or `vc-diff', then remove
;; the changes that should stay in @ with the usual Diff mode commands:
;; `diff-hunk-kill', `diff-file-kill', `diff-split-hunk', or by editing
;; the hunks.  Then run `my/jj-squash-diff' to choose a destination
;; change and move what is left in the buffer into it.
;;
;; The changes are moved with "jj squash --interactive", using a diff
;; editor that applies the buffer's patch.  See
;; `my/jj-squash--apply-script'.

(require 'diff-mode)

(defgroup my/jj-squash nil
  "Move changes from the jj working copy into an ancestor."
  :group 'vc)

(defcustom my/jj-squash-destination-revset "trunk()..@-"
  "Revset of the changes that `my/jj-squash-diff' offers as destinations."
  :type 'string)

(defconst my/jj-squash--tool-name "my-jj-squash-apply"
  "Name of the jj diff editor defined by `my/jj-squash--tool-args'.")

;; Avoid shell variable names that jj substitutes in edit-args, such as
;; $left, $right, $output and $path.
(defconst my/jj-squash--apply-script
  "set -eu
from=$1 to=$2 patch_file=$3
find \"$to\" -mindepth 1 -maxdepth 1 -exec rm -rf -- {} +
cp -a \"$from\"/. \"$to\"/
chmod -R u+w \"$to\"
cd \"$to\"
git apply --recount \"$patch_file\""
  "Shell script that jj runs as the diff editor for `my/jj-squash-patch'.
Its arguments are jj's left and right directories and a patch file.
jj writes only the changed files into the directories, and makes the
files in the left directory read-only.  The script replaces the right
directory with a writable copy of the left one and applies the patch,
so the changes that jj moves are exactly those in the patch.

\"git apply\" applies a patch outside a Git repository like
\"patch\" does.  Unlike \"patch\", it rejects hunks whose context does
not match exactly, and with \"--recount\" it ignores the line counts in
hunk headers, which may be wrong after editing a hunk by hand.")

(defconst my/jj-squash--log-template
  (concat "commit_id ++ \"\\0\" ++ change_id.shortest(8) ++ \"\\0\""
          " ++ description.first_line() ++ \"\\0\""
          " ++ bookmarks.join(\", \") ++ \"\\n\"")
  "jj log template parsed by `my/jj-squash--destinations'.")

(defun my/jj-squash--call (&rest args)
  "Run jj with ARGS in `default-directory'.
Return a cons (STDOUT . STDERR) of strings.  Signal an error that
includes STDERR if jj fails."
  (let ((stderr-file (make-temp-file "my-jj-squash-stderr")))
    (unwind-protect
        (with-temp-buffer
          (let* ((status (apply #'process-file "jj" nil (list t stderr-file)
                                nil args))
                 (stderr (with-temp-buffer
                           (insert-file-contents stderr-file)
                           (string-trim (buffer-string)))))
            (unless (eq status 0)
              (error "jj %s failed: %s" (car args) stderr))
            (cons (buffer-string) stderr)))
      (delete-file stderr-file))))

(defun my/jj-squash--format-destination (change description bookmarks)
  "Return the completion candidate for a destination change.
CHANGE is its short change ID, DESCRIPTION the first line of its
description, and BOOKMARKS a string listing its bookmarks."
  (concat (propertize change 'face 'font-lock-constant-face)
          " "
          (if (string-empty-p description)
              (propertize "(no description set)" 'face 'shadow)
            description)
          (unless (string-empty-p bookmarks)
            (propertize (format " (%s)" bookmarks)
                        'face 'font-lock-keyword-face))))

(defun my/jj-squash--destinations (revset)
  "Return an alist of the changes in REVSET, newest first.
Each key is a completion candidate and each value a commit ID."
  (mapcar (lambda (line)
            (pcase-let ((`(,commit ,change ,description ,bookmarks)
                         (split-string line "\0")))
              (cons (my/jj-squash--format-destination
                     change description bookmarks)
                    commit)))
          (split-string (car (my/jj-squash--call
                              "log" "--no-graph" "-r" revset
                              "-T" my/jj-squash--log-template))
                        "\n" t)))

(defun my/jj-squash--ordered-table (candidates)
  "Return a completion table for CANDIDATES that keeps their order."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action action candidates string pred))))

(defun my/jj-squash-read-destination (prompt &optional revset)
  "Read a change in REVSET with PROMPT and return its commit ID.
REVSET defaults to `my/jj-squash-destination-revset'.  Each candidate
shows the change ID, the first line of the description and the
bookmarks, so any of them can be typed to find a change."
  (let* ((revset (or revset my/jj-squash-destination-revset))
         (destinations (or (my/jj-squash--destinations revset)
                           (user-error "No changes in %s" revset)))
         (choice (completing-read
                  prompt (my/jj-squash--ordered-table destinations)
                  nil t nil nil (caar destinations))))
    (cdr (assoc choice destinations))))

(defun my/jj-squash-working-copy-diff-p ()
  "Return non-nil if this buffer is a VC diff of the jj working copy.
Such buffers are made by `vc-diff' and `vc-root-diff' when they are
not given revisions."
  (declare (side-effect-free t))
  (and (derived-mode-p 'diff-mode)
       (eq diff-vc-backend 'JJ)
       (member diff-vc-revisions '((nil nil) ("@" "@")))))

(defun my/jj-squash--check-buffer ()
  "Signal a `user-error' unless `my/jj-squash-diff' can use this buffer."
  (unless (my/jj-squash-working-copy-diff-p)
    (user-error "Not a VC diff of the jj working copy"))
  (when (get-buffer-process (current-buffer))
    (user-error "The diff is not complete yet")))

(defun my/jj-squash--buffer-patch ()
  "Return the patch in this Diff mode buffer as a string.
Ignore narrowing, since the file headers that hunks need may be
outside it."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward "^diff --git " nil t)
        (user-error "No changes in this buffer")))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun my/jj-squash--tool-args (patch-file)
  "Return jj arguments that make the diff editor select PATCH-FILE.
See `my/jj-squash--apply-script'."
  (let ((tool my/jj-squash--tool-name))
    (list "--tool" tool
          "--config" "ui.diff-instructions=false"
          "--config" (format "merge-tools.%s.program=\"sh\"" tool)
          ;; A JSON array of strings is also a TOML array.
          "--config" (format "merge-tools.%s.edit-args=%s" tool
                             (json-serialize
                              (vector "-c" my/jj-squash--apply-script "sh"
                                      "$left" "$right" patch-file))))))

(defun my/jj-squash-patch (patch destination)
  "Move the changes in PATCH from the jj working copy into DESTINATION.
PATCH is a git-format diff of @ against its parents, as made by
\"jj diff --git -r @\", or a part of one.  DESTINATION is a revision.
The working copy keeps its description and is kept when it becomes
empty.  Run jj in `default-directory' and return the messages it
printed."
  (let ((patch-file (let ((coding-system-for-write 'utf-8-unix))
                      (make-temp-file "my-jj-squash" nil ".patch" patch))))
    (unwind-protect
        (cdr (apply #'my/jj-squash--call
                    "squash" "--into" destination
                    "--keep-emptied" "--use-destination-message"
                    (my/jj-squash--tool-args patch-file)))
      (delete-file patch-file))))

;;;###autoload
(defun my/jj-squash-diff (destination)
  "Move the changes shown in this Diff mode buffer into DESTINATION.
The buffer must show the changes in the jj working copy @, as made by
`vc-diff' or `vc-root-diff'.  Remove the changes that should stay in
@ before running this command.  Interactively, read DESTINATION from
the changes in `my/jj-squash-destination-revset'.

Afterwards, refresh the buffer to show the changes left in @."
  (interactive
   (progn
     (my/jj-squash--check-buffer)
     (list (my/jj-squash-read-destination "Squash into: "))))
  (my/jj-squash--check-buffer)
  (message "%s" (my/jj-squash-patch (my/jj-squash--buffer-patch)
                                    destination))
  (revert-buffer nil t))

(provide 'my-jj-squash)
;;; my-jj-squash.el ends here
