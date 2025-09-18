;;; raycast-emacs.el --- Bridge for the Emacs Raycast extension -*- lexical-binding: t; -*-

;; Companion package for the personal Raycast "Emacs" extension
;; (see conf/raycast/emacs).  Raycast invokes `emacsclient --eval' with forms
;; that call `raycast-emacs-dispatch'.  This file turns those calls into elisp
;; actions and returns base64-encoded JSON so the TypeScript side can decode
;; results without dealing with elisp print escaping.
;;
;; Wire protocol:
;;   emacsclient --eval '(raycast-emacs-dispatch "METHOD" "BASE64-JSON-ARGS")'
;; returns a base64-encoded JSON envelope: {"ok":bool,"value":...,"error":str}.
;;
;; To expose new commands to Raycast's "Emacs Commands" list, either call
;; `raycast-emacs-register-command' or add entries to `raycast-emacs-commands'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defgroup raycast-emacs nil
  "Bridge for the Emacs Raycast extension."
  :group 'external
  :prefix "raycast-emacs-")

(defcustom raycast-emacs-commands nil
  "User-defined commands exposed to Raycast's \"Emacs Commands\" list.
Each entry is a plist accepting the same keys as
`raycast-emacs-register-command': :id :title :subtitle :icon :handler."
  :type '(repeat plist)
  :group 'raycast-emacs)

(defvar raycast-emacs--commands (make-hash-table :test #'equal)
  "Registered commands keyed by id.  Values are plists.")

(defvar raycast-emacs--command-order nil
  "Command ids in registration order (most recent first).")

(defvar raycast-emacs--frame-counter 0
  "Monotonic counter used to assign stable per-frame ids.")


;;; Registry

(cl-defun raycast-emacs-register-command (&key id title subtitle icon handler)
  "Register a command exposed to Raycast under ID.
TITLE and SUBTITLE are shown in the Raycast list.  ICON is an optional
Raycast icon name (unknown names fall back gracefully).  HANDLER is the
function run when the command is chosen; it may take zero arguments or a
single alist of arguments supplied by Raycast."
  (unless id
    (error "raycast-emacs-register-command: :id is required"))
  (unless (gethash id raycast-emacs--commands)
    (push id raycast-emacs--command-order))
  (puthash id (list :id id :title (or title id) :subtitle subtitle
                    :icon icon :handler handler)
           raycast-emacs--commands)
  id)

(defun raycast-emacs--all-commands ()
  "Return all commands as plists: `raycast-emacs-commands' then registered."
  (let ((seen (make-hash-table :test #'equal))
        (result nil))
    (dolist (entry raycast-emacs-commands)
      (let ((id (plist-get entry :id)))
        (when (and id (not (gethash id seen)))
          (puthash id t seen)
          (push entry result))))
    (dolist (id (reverse raycast-emacs--command-order))
      (unless (gethash id seen)
        (puthash id t seen)
        (push (gethash id raycast-emacs--commands) result)))
    (nreverse result)))

(defun raycast-emacs--find-command (id)
  "Return the command plist for ID, or nil."
  (or (gethash id raycast-emacs--commands)
      (cl-find id raycast-emacs-commands
               :key (lambda (e) (plist-get e :id)) :test #'equal)))


;;; JSON transport

(defun raycast-emacs--encode (value)
  "Encode VALUE to base64-encoded UTF-8 JSON (single line)."
  (base64-encode-string
   (encode-coding-string (json-encode value) 'utf-8)
   t))

(defun raycast-emacs--decode-args (base64)
  "Decode BASE64 (JSON object) into an alist, or nil when empty."
  (let ((json (decode-coding-string (base64-decode-string base64) 'utf-8)))
    (if (string-empty-p (string-trim json))
        nil
      (json-parse-string json :object-type 'alist :array-type 'list))))

(defun raycast-emacs-dispatch (method args-base64)
  "Entry point called by the Raycast extension.
METHOD is a string; ARGS-BASE64 is base64-encoded JSON.  Returns a
base64-encoded JSON envelope."
  (raycast-emacs--encode
   (condition-case err
       (let* ((args (raycast-emacs--decode-args args-base64))
              (value (raycast-emacs--handle method args)))
         (list :ok t :value value :error nil))
     (error (list :ok :json-false :value nil
                  :error (error-message-string err))))))

(defun raycast-emacs--handle (method args)
  "Dispatch METHOD with ARGS (an alist).  Returns a JSON-encodable value."
  (pcase method
    ("list-commands" (raycast-emacs--commands-json))
    ("run-command"   (raycast-emacs--run (alist-get 'id args) args))
    ("list-buffers"  (raycast-emacs--buffers-json))
    ("switch-buffer" (raycast-emacs--switch-buffer (alist-get 'name args)))
    ("list-frames"   (raycast-emacs--frames-json))
    ("select-frame"  (raycast-emacs--select-frame (alist-get 'id args)))
    (_ (error "Unknown method: %s" method))))


;;; Commands

(defun raycast-emacs--commands-json ()
  "Return registered commands as a vector of JSON objects."
  (apply #'vector
         (mapcar (lambda (c)
                   (list :id (plist-get c :id)
                         :title (or (plist-get c :title) (plist-get c :id))
                         :subtitle (plist-get c :subtitle)
                         :icon (plist-get c :icon)))
                 (raycast-emacs--all-commands))))

(defun raycast-emacs--call (handler args)
  "Call HANDLER, passing ARGS when it accepts at least one argument."
  (let ((max (cdr (func-arity handler))))
    (if (or (eq max 'many) (and (integerp max) (>= max 1)))
        (funcall handler args)
      (funcall handler))))

(defun raycast-emacs--run (id args)
  "Run the registered command ID with ARGS, then focus Emacs."
  (let ((command (raycast-emacs--find-command id)))
    (unless command (error "No such command: %s" id))
    (let ((handler (plist-get command :handler)))
      (unless (functionp handler)
        (error "Command %s has no handler" id))
      (raycast-emacs--call handler args)
      (select-frame-set-input-focus (selected-frame))
      t)))


;;; Buffers

(defun raycast-emacs--buffer-subtitle (buffer)
  "Return a subtitle for BUFFER: project name and major mode."
  (with-current-buffer buffer
    (let ((mode (format-mode-line mode-name))
          (project (ignore-errors
                     (when-let* ((p (project-current nil)))
                       (file-name-nondirectory
                        (directory-file-name (project-root p)))))))
      (string-trim
       (if (and project (> (length project) 0))
           (format "%s · %s" project mode)
         (format "%s" mode))))))

(defun raycast-emacs--buffers-json ()
  "Return visible buffers as a vector of JSON objects."
  (apply #'vector
         (delq nil
               (mapcar
                (lambda (buffer)
                  (let ((name (buffer-name buffer)))
                    (unless (string-prefix-p " " name)
                      (list :name name
                            :subtitle (raycast-emacs--buffer-subtitle buffer)
                            :modified (if (buffer-modified-p buffer) t :json-false)))))
                (buffer-list)))))

(defun raycast-emacs--switch-buffer (name)
  "Display buffer NAME, honouring display actions, and focus its frame."
  (let ((buffer (get-buffer name)))
    (unless buffer (error "No such buffer: %s" name))
    (let ((window (display-buffer buffer)))
      (if window
          (progn
            (select-window window)
            (select-frame-set-input-focus (window-frame window)))
        (pop-to-buffer buffer)
        (select-frame-set-input-focus (selected-frame))))
    t))


;;; Frames

(defun raycast-emacs--frame-id (frame)
  "Return a stable id for FRAME, assigning one if needed."
  (or (frame-parameter frame 'raycast-emacs-id)
      (let ((id (format "f%d" (cl-incf raycast-emacs--frame-counter))))
        (set-frame-parameter frame 'raycast-emacs-id id)
        id)))

(defun raycast-emacs--frame-title (frame)
  "Return a display title for FRAME."
  (let ((name (frame-parameter frame 'name)))
    (if (stringp name) name "Emacs")))

(defun raycast-emacs--frame-project (frame)
  "Return the project name dedicated to FRAME, or nil."
  (when (fboundp 'frame-project-dedicate--get-frame-project-root)
    (let ((root (ignore-errors
                  (frame-project-dedicate--get-frame-project-root frame))))
      (when (stringp root)
        (file-name-nondirectory (directory-file-name root))))))

(defun raycast-emacs--visible-frame-p (frame)
  "Return non-nil when FRAME is a normal, visible, top-level frame."
  (and (eq (frame-visible-p frame) t)
       (not (frame-parameter frame 'parent-frame))))

(defun raycast-emacs--frames-json ()
  "Return visible top-level frames as a vector of JSON objects."
  (apply #'vector
         (delq nil
               (mapcar
                (lambda (frame)
                  (when (raycast-emacs--visible-frame-p frame)
                    (list :id (raycast-emacs--frame-id frame)
                          :title (raycast-emacs--frame-title frame)
                          :subtitle (raycast-emacs--frame-project frame))))
                (frame-list)))))

(defun raycast-emacs--select-frame (id)
  "Raise and focus the frame with the given ID."
  (let ((frame (cl-find id (frame-list)
                        :key (lambda (f) (frame-parameter f 'raycast-emacs-id))
                        :test #'equal)))
    (unless frame (error "No such frame: %s" id))
    (select-frame-set-input-focus frame)
    (raise-frame frame)
    t))


;;; Built-in commands (also serve as registration examples)

(raycast-emacs-register-command
 :id "new-frame" :title "New Frame" :icon "PlusCircle"
 :handler (lambda (&optional _args) (select-frame (make-frame))))

(raycast-emacs-register-command
 :id "save-all-buffers" :title "Save All Buffers" :icon "SaveDocument"
 :handler (lambda (&optional _args) (save-some-buffers t)))

(raycast-emacs-register-command
 :id "org-capture" :title "Org Capture" :icon "Pencil"
 :handler (lambda (&optional _args)
            (run-with-timer 0 nil #'(lambda ()
                                      (find-file (concat (file-name-as-directory org-directory) "inbox.org"))
                                      (call-interactively #'org-capture)))))

(raycast-emacs-register-command
 :id "org-todo-list" :title "Todo List" :icon "List"
 :handler (lambda (&optional _args)
            (find-file (concat (file-name-as-directory org-directory) "inbox.org"))
            (org-todo-list)))

(provide 'raycast-emacs)
;;; raycast-emacs.el ends here
