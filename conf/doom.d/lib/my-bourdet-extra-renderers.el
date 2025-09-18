;;; ../.config/home-manager/conf/doom.d/lib/my-bourdet-extra-renderers.el -*- lexical-binding: t; -*-

(require 'bourdet)

;;; Atlassian

(bourdet-register-tool-result-formatter
 "mcp__claude_ai_Atlassian__searchAtlassian"
 (lambda (_tool-name result-text is-error)
   (if is-error
       (propertize (format "✗ Error: %s" result-text) 'face 'error)
     (condition-case nil
         (let* ((json (json-parse-string result-text :object-type 'alist))
                (results (alist-get 'results json))
                (parts nil))
           (push (format "Atlassian search: %d result%s"
                         (length results)
                         (if (= (length results) 1) "" "s"))
                 parts)
           (push "" parts)
           (seq-do
            (lambda (node)
              (let* ((title (alist-get 'title node))
                     (id (alist-get 'id node))
                     (type (alist-get 'type node))
                     (url (alist-get 'url node))
                     (text (alist-get 'text node))
                     (metadata (alist-get 'metadata node))
                     (scope (and metadata (alist-get 'searchScope metadata))))
                (push (propertize (or title "(untitled)") 'face 'bold) parts)
                (push (format "  Type: %s%s"
                              (or type "?")
                              (if scope (format "  Scope: %s" scope) ""))
                      parts)
                (when id
                  (push (format "  ID: %s" id) parts))
                (when url
                  (push (format "  URL: %s" url) parts))
                (when (and text (stringp text) (not (string-empty-p text)))
                  (let ((trimmed (if (> (length text) 300)
                                     (concat (substring text 0 300) "…")
                                   text)))
                    (push (format "  %s" trimmed) parts)))
                (push "" parts)))
            results)
           (string-join (nreverse parts) "\n"))
       (error nil)))))

(bourdet-register-tool-result-formatter
 "mcp__claude_ai_Atlassian__getConfluencePage"
 (lambda (_tool-name result-text is-error)
   (if is-error
       (propertize (format "✗ Error: %s" result-text) 'face 'error)
     (condition-case nil
         (let* ((json (json-parse-string result-text :object-type 'alist))
                (content (alist-get 'content json))
                (nodes (alist-get 'nodes content))
                (total (alist-get 'totalCount content))
                (parts '()))
           (when (and total (> total (length nodes)))
             (push (format "Showing %d of %d pages\n" (length nodes) total) parts))
           (seq-do
            (lambda (node)
              (let* ((title (alist-get 'title node))
                     (id (alist-get 'id node))
                     (status (alist-get 'status node))
                     (subtype (alist-get 'subtype node))
                     (type (alist-get 'type node))
                     (last-modified (alist-get 'lastModified node))
                     (space (alist-get 'space node))
                     (space-key (and space (alist-get 'key space)))
                     (space-name (and space (alist-get 'name space)))
                     (author (alist-get 'author node))
                     (author-name (and author (alist-get 'displayName author)))
                     (links (alist-get '_links node))
                     (web-url (alist-get 'webUrl node))
                     (webui (and links (alist-get 'webui links)))
                     (body (alist-get 'body node))
                     (summary (alist-get 'summary node))
                     (header '()))
                ;; Build header lines
                (when title
                  (push (propertize title 'face 'bold) header))
                (when id
                  (push (format "ID: %s  Type: %s%s  Status: %s"
                                id
                                (or type "?")
                                (if (and subtype (not (string-empty-p subtype)))
                                    (format "/%s" subtype) "")
                                (or status "?"))
                        header))
                (when (or space-name space-key)
                  (push (format "Space: %s%s"
                                (or space-name "")
                                (if space-key (format " [%s]" space-key) ""))
                        header))
                (when author-name
                  (push (format "Author: %s" author-name) header))
                (when last-modified
                  (push (format "Modified: %s" last-modified) header))
                (when web-url
                  (push (format "URL: %s" web-url) header))
                (when (and (not web-url) webui)
                  (push (format "Path: %s" webui) header))
                ;; Assemble
                (push (mapconcat #'identity (nreverse header) "\n") parts)
                ;; Body or summary
                (cond
                 (body
                  (push "---" parts)
                  (push (bourdet--fontify-markdown body) parts))
                 (summary
                  (push (format "Summary: %s" summary) parts)))))
            nodes)
           (mapconcat #'identity (nreverse parts) "\n"))
       (error nil)))))

(bourdet-register-tool-result-formatter
 "mcp__claude_ai_Atlassian__searchJiraIssuesUsingJql"
 (lambda (_tool-name result-text is-error)
   (if is-error
       (propertize (format "✗ Jira search error:\n%s" result-text)
                   'face 'error)
     (condition-case nil
         (let* ((parsed (json-parse-string result-text :object-type 'alist))
                (issues (alist-get 'issues parsed))
                (total (alist-get 'totalCount issues))
                (nodes (alist-get 'nodes issues))
                (search-url (alist-get 'webUrl issues))
                (parts '()))
           (push (format "Jira results: %s issue%s"
                         total (if (eql total 1) "" "s"))
                 parts)
           (when search-url
             (push (format "Search: %s" search-url) parts))
           (push "" parts)
           (seq-do
            (lambda (node)
              (let* ((key (alist-get 'key node))
                     (url (alist-get 'webUrl node))
                     (fields (alist-get 'fields node))
                     (summary (alist-get 'summary fields))
                     (status (alist-get 'name (alist-get 'status fields)))
                     (itype (alist-get 'name (alist-get 'issuetype fields)))
                     (subtask (alist-get 'subtask (alist-get 'issuetype fields)))
                     (project-obj (alist-get 'project fields))
                     (project-key (alist-get 'key project-obj))
                     (project-name (alist-get 'name project-obj))
                     (assignee-obj (alist-get 'assignee fields))
                     (assignee (if (and assignee-obj (not (eq assignee-obj :null)))
                                   (or (alist-get 'displayName assignee-obj)
                                       (alist-get 'name assignee-obj))
                                 "Unassigned"))
                     (desc-raw (alist-get 'description fields))
                     (description (when (and desc-raw (not (eq desc-raw :null))
                                             (stringp desc-raw))
                                    (if (> (length desc-raw) 500)
                                        (concat (substring desc-raw 0 500) "…")
                                      desc-raw)))
                     (comments-obj (alist-get 'comment fields))
                     (comment-total (when comments-obj
                                      (alist-get 'total comments-obj))))
                (push (format "── %s: %s" key summary) parts)
                (push (format "   Status: %s  |  Type: %s%s  |  Project: %s (%s)"
                              status
                              itype
                              (if (eq subtask t) " ✓ subtask"
                                (if subtask " ✓ subtask" ""))
                              project-name project-key)
                      parts)
                (push (format "   Assignee: %s" assignee) parts)
                (when url
                  (push (format "   URL: %s" url) parts))
                (when (and comment-total (> comment-total 0))
                  (push (format "   Comments: %s" comment-total) parts))
                (when description
                  (push (format "   Description:\n%s"
                                (bourdet--fontify-markdown description))
                        parts))
                (push "" parts)))
            nodes)
           (string-join (nreverse parts) "\n"))
       (error nil)))))

(bourdet-register-tool-result-formatter
 "mcp__claude_ai_Atlassian__searchConfluenceUsingCql"
 (lambda (_tool-name result-text is-error)
   (if is-error
       (propertize (format "✗ Error: %s" result-text) 'face 'error)
     (condition-case nil
         (let* ((json (json-parse-string result-text :object-type 'alist))
                (content (alist-get 'content json))
                (total (alist-get 'totalCount content))
                (nodes (alist-get 'nodes content))
                (search-url (alist-get 'webUrl content))
                (parts nil))
           (push (format "Confluence search: %d result%s"
                         total (if (= total 1) "" "s"))
                 parts)
           (when search-url
             (push (format "Search URL: %s" search-url) parts))
           (push "" parts)
           (seq-do
            (lambda (node)
              (let* ((title (alist-get 'title node))
                     (id (alist-get 'id node))
                     (type (alist-get 'type node))
                     (status (alist-get 'status node))
                     (modified (alist-get 'lastModified node))
                     (summary (alist-get 'summary node))
                     (url (alist-get 'webUrl node))
                     (space (alist-get 'space node))
                     (space-name (and space (alist-get 'name space)))
                     (space-key (and space (alist-get 'key space)))
                     (author (alist-get 'author node))
                     (author-name (and author (alist-get 'displayName author))))
                (push (propertize (or title "(untitled)") 'face 'bold) parts)
                (push (format "  ID: %s  Type: %s  Status: %s"
                              (or id "?") (or type "?") (or status "?"))
                      parts)
                (when (or space-name space-key)
                  (push (format "  Space: %s [%s]"
                                (or space-name "?") (or space-key "?"))
                        parts))
                (when author-name
                  (push (format "  Author: %s" author-name) parts))
                (when modified
                  (push (format "  Modified: %s" modified) parts))
                (when url
                  (push (format "  URL: %s" url) parts))
                (when (and summary (not (string-empty-p summary)))
                  (let ((trimmed (if (> (length summary) 200)
                                     (concat (substring summary 0 200) "…")
                                   summary)))
                    (push (format "  Summary: %s" trimmed) parts)))
                (push "" parts)))
            nodes)
           (string-join (nreverse parts) "\n"))
       (error nil)))))

;;; Linear

(bourdet-register-tool-result-formatter
 "mcp__claude_ai_Linear__get_project"
 (lambda (_tool-name result-text is-error)
   (if is-error
       (propertize (format "✗ Error: %s" result-text) 'face 'error)
     (condition-case nil
         (let* ((data (json-parse-string result-text :object-type 'alist))
                (name (alist-get 'name data))
                (id (alist-get 'id data))
                (url (alist-get 'url data))
                (icon (alist-get 'icon data))
                (color (alist-get 'color data))
                (summary (alist-get 'summary data))
                (description (alist-get 'description data))
                (status (alist-get 'status data))
                (status-name (and status (not (eq status :null))
                                  (alist-get 'name status)))
                (priority (alist-get 'priority data))
                (pri-name (and priority (not (eq priority :null))
                               (alist-get 'name priority)))
                (lead (alist-get 'lead data))
                (lead-name (and lead (not (eq lead :null))
                                (alist-get 'name lead)))
                (teams (alist-get 'teams data))
                (labels (alist-get 'labels data))
                (initiatives (alist-get 'initiatives data))
                (milestones (alist-get 'milestones data))
                (resources (alist-get 'resources data))
                (start-date (alist-get 'startDate data))
                (target-date (alist-get 'targetDate data))
                (created (alist-get 'createdAt data))
                (updated (alist-get 'updatedAt data))
                (parts nil))
           ;; Title
           (push (propertize (or name "(unnamed project)") 'face 'bold) parts)
           ;; Core metadata
           (let ((meta nil))
             (when status-name (push (format "Status: %s" status-name) meta))
             (when pri-name (push (format "Priority: %s" pri-name) meta))
             (when lead-name (push (format "Lead: %s" lead-name) meta))
             (when meta
               (push (string-join (nreverse meta) "  |  ") parts)))
           ;; Teams
           (when (and teams (> (length teams) 0))
             (push (format "Teams: %s"
                           (string-join
                            (mapcar (lambda (t)
                                      (let ((tname (alist-get 'name t))
                                            (tkey (alist-get 'key t)))
                                        (if tkey (format "%s [%s]" tname tkey) tname)))
                                    (append teams nil))
                            ", "))
                   parts))
           ;; Labels
           (when (and labels (> (length labels) 0))
             (push (format "Labels: %s"
                           (string-join (append labels nil) ", "))
                   parts))
           ;; Initiatives
           (when (and initiatives (> (length initiatives) 0))
             (push (format "Initiatives: %s"
                           (string-join
                            (mapcar (lambda (i) (alist-get 'name i))
                                    (append initiatives nil))
                            ", "))
                   parts))
           ;; Dates
           (let ((dates nil))
             (when start-date (push (format "Start: %s" start-date) dates))
             (when target-date (push (format "Target: %s" target-date) dates))
             (when created (push (format "Created: %s" created) dates))
             (when updated (push (format "Updated: %s" updated) dates))
             (when dates
               (push (string-join (nreverse dates) "  |  ") parts)))
           ;; Icon/color
           (when (and icon (not (eq icon :null)) (stringp icon)
                      (not (string-empty-p icon)))
             (push (format "Icon: %s" icon) parts))
           (when (and color (not (eq color :null)) (stringp color)
                      (not (string-empty-p color)))
             (push (format "Color: %s" color) parts))
           ;; URL
           (when url
             (push (format "URL: %s" url) parts))
           ;; Milestones
           (when (and milestones (> (length milestones) 0))
             (push (format "Milestones: %s"
                           (string-join
                            (mapcar (lambda (m) (or (alist-get 'name m)
                                                    (alist-get 'id m)))
                                    (append milestones nil))
                            ", "))
                   parts))
           ;; Resources
           (when (and resources (> (length resources) 0))
             (push (format "Resources (%d):" (length resources)) parts)
             (seq-do
              (lambda (r)
                (let ((rtitle (alist-get 'title r))
                      (rurl (alist-get 'url r))
                      (rtype (alist-get 'type r)))
                  (push (format "  • %s%s%s"
                                (or rtitle "(untitled)")
                                (if rtype (format " [%s]" rtype) "")
                                (if rurl (format "\n    %s" rurl) ""))
                        parts)))
              (append resources nil)))
           ;; Summary
           (when (and summary (stringp summary)
                      (not (string-empty-p summary)))
             (push (format "Summary: %s" summary) parts))
           ;; Description — truncated with pointer to URL
           (when (and description (stringp description)
                      (not (string-empty-p description)))
             (push "---" parts)
             (let ((truncated (if (> (length description) 600)
                                  (concat (substring description 0 600) "\n…(truncated, see URL)")
                                description)))
               (push (bourdet--fontify-markdown truncated) parts)))
           (string-join (nreverse parts) "\n"))
       (error nil)))))

(bourdet-register-tool-result-formatter
 "mcp__claude_ai_Linear__list_issues"
 (lambda (_tool-name result-text is-error)
   (if is-error
       (propertize (format "✗ Error: %s" result-text) 'face 'error)
     (condition-case nil
         (let* ((data (json-parse-string result-text :object-type 'alist))
                (issues (alist-get 'issues data))
                (has-next (alist-get 'hasNextPage data))
                (parts nil))
           (push (format "%d issue%s%s"
                         (length issues)
                         (if (= (length issues) 1) "" "s")
                         (if (eq has-next t) " (more available)" ""))
                 parts)
           (push "" parts)
           (seq-do
            (lambda (issue)
              (let* ((id (alist-get 'id issue))
                     (title (alist-get 'title issue))
                     (status (alist-get 'status issue))
                     (priority (alist-get 'priority issue))
                     (pri-name (and priority (alist-get 'name priority)))
                     (assignee (alist-get 'assignee issue))
                     (project (alist-get 'project issue))
                     (labels (alist-get 'labels issue))
                     (url (alist-get 'url issue))
                     (branch (alist-get 'gitBranchName issue))
                     (description (alist-get 'description issue))
                     (meta nil))
                ;; Title line
                (push (format "── %s: %s"
                              (propertize (or id "?") 'face 'bold)
                              (or title "?"))
                      parts)
                ;; Metadata
                (when status (push (format "Status: %s" status) meta))
                (when pri-name (push (format "Priority: %s" pri-name) meta))
                (when assignee (push (format "Assignee: %s" assignee) meta))
                (when project (push (format "Project: %s" project) meta))
                (when meta
                  (push (concat "   " (string-join (nreverse meta) "  |  ")) parts))
                ;; Labels
                (when (and labels (> (length labels) 0))
                  (push (format "   Labels: %s"
                                (string-join (append labels nil) ", "))
                        parts))
                ;; Branch
                (when (and branch (stringp branch) (not (string-empty-p branch)))
                  (push (format "   Branch: %s" branch) parts))
                ;; URL
                (when url
                  (push (format "   %s" url) parts))
                ;; Description (truncated)
                (when (and description (stringp description)
                           (not (string-empty-p description)))
                  (let ((truncated (if (> (length description) 200)
                                       (concat (substring description 0 200) "…")
                                     description)))
                    (push (format "   %s" (string-join
                                           (split-string truncated "\n" t)
                                           " "))
                          parts)))
                (push "" parts)))
            issues)
           (string-join (nreverse parts) "\n"))
       (error nil)))))

(bourdet-register-tool-result-formatter
 "mcp__claude_ai_Linear__get_issue"
 (lambda (_tool-name result-text is-error)
   (if is-error
       (propertize (format "✗ Error: %s" result-text) 'face 'error)
     (condition-case nil
         (let* ((data (json-parse-string result-text :object-type 'alist))
                (id (alist-get 'id data))
                (title (alist-get 'title data))
                (status (alist-get 'status data))
                (url (alist-get 'url data))
                (priority (alist-get 'priority data))
                (pri-name (and priority (alist-get 'name priority)))
                (assignee (alist-get 'assignee data))
                (created-by (alist-get 'createdBy data))
                (team (alist-get 'team data))
                (project (alist-get 'project data))
                (labels (alist-get 'labels data))
                (branch (alist-get 'gitBranchName data))
                (created (alist-get 'createdAt data))
                (updated (alist-get 'updatedAt data))
                (archived (alist-get 'archivedAt data))
                (completed (alist-get 'completedAt data))
                (due (alist-get 'dueDate data))
                (cycle-id (alist-get 'cycleId data))
                (sla-type (alist-get 'slaType data))
                (sla-started (alist-get 'slaStartedAt data))
                (sla-breach (alist-get 'slaBreachesAt data))
                (sla-medium (alist-get 'slaMediumRiskAt data))
                (sla-high (alist-get 'slaHighRiskAt data))
                (attachments (alist-get 'attachments data))
                (documents (alist-get 'documents data))
                (description (alist-get 'description data))
                (parts nil))
           ;; Title line
           (push (propertize (format "%s: %s" (or id "?") (or title "?"))
                             'face 'bold)
                 parts)
           ;; Core metadata
           (let ((meta nil))
             (when status (push (format "Status: %s" status) meta))
             (when pri-name (push (format "Priority: %s" pri-name) meta))
             (when assignee (push (format "Assignee: %s" assignee) meta))
             (when created-by (push (format "Creator: %s" created-by) meta))
             (when team (push (format "Team: %s" team) meta))
             (when project (push (format "Project: %s" project) meta))
             (when meta
               (push (string-join (nreverse meta) "  |  ") parts)))
           ;; Labels
           (when (and labels (> (length labels) 0))
             (push (format "Labels: %s"
                           (string-join (append labels nil) ", "))
                   parts))
           ;; Branch
           (when (and branch (stringp branch) (not (string-empty-p branch)))
             (push (format "Branch: %s" branch) parts))
           ;; Dates
           (let ((dates nil))
             (when created (push (format "Created: %s" created) dates))
             (when updated (push (format "Updated: %s" updated) dates))
             (when completed (push (format "Completed: %s" completed) dates))
             (when (and archived (not (eq archived :null)))
               (push (format "Archived: %s" archived) dates))
             (when due (push (format "Due: %s" due) dates))
             (when dates
               (push (string-join (nreverse dates) "  |  ") parts)))
           ;; SLA info (only if started)
           (when (and sla-started (not (eq sla-started :null)))
             (let ((sla-parts (list (format "SLA: %s, started %s" (or sla-type "?") sla-started))))
               (when (and sla-breach (not (eq sla-breach :null)))
                 (push (format "breaches %s" sla-breach) sla-parts))
               (push (string-join sla-parts " ") parts)))
           ;; Cycle
           (when cycle-id
             (push (format "Cycle: %s" cycle-id) parts))
           ;; URL
           (when url
             (push (format "URL: %s" url) parts))
           ;; Attachments / documents summaries
           (when (and attachments (> (length attachments) 0))
             (push (format "Attachments: %d" (length attachments)) parts))
           (when (and documents (> (length documents) 0))
             (push (format "Documents: %d" (length documents)) parts))
           ;; Description
           (when (and description (stringp description)
                      (not (string-empty-p description)))
             (push "---" parts)
             (push (bourdet--fontify-markdown description) parts))
           (string-join (nreverse parts) "\n"))
       (error nil)))))

(bourdet-register-tool-formatter
 "mcp__claude_ai_Linear__save_issue"
 (lambda (_tool-name input &optional _session)
   (let* ((known-keys '(title team project priority description
                        state assignee labels estimate dueDate
                        parentId cycleId subscriberIds))
          (parts nil))
     (when-let ((title (alist-get 'title input)))
       (push (format "Title: %s" (propertize title 'face 'bold)) parts))
     (when-let ((team (alist-get 'team input)))
       (push (format "Team: %s" team) parts))
     (when-let ((project (alist-get 'project input)))
       (push (format "Project: %s" project) parts))
     (when-let ((pri (alist-get 'priority input)))
       (let ((label (pcase pri
                      (0 "None") (1 "Urgent") (2 "High")
                      (3 "Medium") (4 "Low")
                      (_ (format "%s" pri)))))
         (push (format "Priority: %s" label) parts)))
     (when-let ((state (alist-get 'state input)))
       (push (format "State: %s" state) parts))
     (when-let ((assignee (alist-get 'assignee input)))
       (push (format "Assignee: %s" assignee) parts))
     (when-let ((labels (alist-get 'labels input)))
       (push (format "Labels: %s"
                     (if (listp labels)
                         (string-join (mapcar (lambda (l) (format "%s" l)) labels) ", ")
                       (format "%s" labels)))
             parts))
     (when-let ((est (alist-get 'estimate input)))
       (push (format "Estimate: %s" est) parts))
     (when-let ((due (alist-get 'dueDate input)))
       (push (format "Due: %s" due) parts))
     (when-let ((pid (alist-get 'parentId input)))
       (push (format "Parent: %s" pid) parts))
     (when-let ((cid (alist-get 'cycleId input)))
       (push (format "Cycle: %s" cid) parts))
     (when-let ((subs (alist-get 'subscriberIds input)))
       (push (format "Subscribers: %s"
                     (if (listp subs)
                         (string-join (mapcar (lambda (s) (format "%s" s)) subs) ", ")
                       (format "%s" subs)))
             parts))
     ;; Catch-all: render any keys we didn't handle above
     (dolist (pair input)
       (unless (memq (car pair) known-keys)
         (let ((val (cdr pair)))
           (push (format "%s: %s" (car pair)
                         (cond
                          ((eq val t) "✓")
                          ((eq val :false) "✗")
                          (t (format "%s" val))))
                 parts))))
     ;; Description last, syntax-highlighted as markdown
     (when-let ((desc (alist-get 'description input)))
       (push (concat "Description:\n" (bourdet--fontify-markdown desc)) parts))
     (when parts
       (string-join (nreverse parts) "\n")))))

(bourdet-register-tool-summary
 "mcp__claude_ai_Linear__save_issue"
 (lambda (_tool-name input)
   (let* ((title (alist-get 'title input))
          (team (alist-get 'team input))
          (pri (alist-get 'priority input))
          (pri-label (pcase pri
                       (1 "urgent") (2 "high") (3 "medium") (4 "low") (_ nil)))
          (summary (concat "Save issue"
                           (when title (format ": %s" title))
                           (when team (format " [%s]" team))
                           (when pri-label (format " (%s)" pri-label)))))
     (cons summary nil))))

(bourdet-register-tool-result-formatter
 "mcp__claude_ai_Linear__save_issue"
 (lambda (_tool-name result-text is-error)
   (if is-error
       (propertize (format "✗ Save failed: %s" result-text)
                   'face 'error)
     (condition-case nil
         (let* ((data (json-parse-string result-text :object-type 'alist))
                (id (alist-get 'id data))
                (title (alist-get 'title data))
                (status (alist-get 'status data))
                (url (alist-get 'url data))
                (assignee (alist-get 'assignee data))
                (priority (alist-get 'priority data))
                (priority-name (and priority (alist-get 'name priority))))
           (concat
            (propertize (format "✓ %s" id) 'face 'success)
            (format " %s" title)
            (if status (format "\n  Status: %s" status) "")
            (if priority-name (format "  Priority: %s" priority-name) "")
            (if assignee (format "  Assignee: %s" assignee) "")
            (if url (format "\n  %s" url) "")))
       (error nil)))))

(bourdet-register-tool-result-formatter
 "mcp__claude_ai_Linear__get_document"
 (lambda (_tool-name result-text is-error)
   (if is-error
       (propertize (format "✗ Error: %s" result-text) 'face 'error)
     (condition-case nil
         (let* ((data (json-parse-string result-text :object-type 'alist))
                (title (alist-get 'title data))
                (url (alist-get 'url data))
                (slug (alist-get 'slugId data))
                (created (alist-get 'createdAt data))
                (updated (alist-get 'updatedAt data))
                (archived (alist-get 'archivedAt data))
                (icon (alist-get 'icon data))
                (color (alist-get 'color data))
                (creator (alist-get 'creator data))
                (creator-name (and creator (not (eq creator :null))
                                   (alist-get 'name creator)))
                (updater (alist-get 'updatedBy data))
                (updater-name (and updater (not (eq updater :null))
                                   (alist-get 'name updater)))
                (project (alist-get 'project data))
                (project-name (and project (not (eq project :null))
                                   (alist-get 'name project)))
                (initiative (alist-get 'initiative data))
                (initiative-name (and initiative (not (eq initiative :null))
                                      (alist-get 'name initiative)))
                (issue (alist-get 'issue data))
                (issue-id (and issue (not (eq issue :null))
                               (or (alist-get 'identifier issue)
                                   (alist-get 'id issue))))
                (content (alist-get 'content data))
                (parts nil))
           ;; Title
           (push (propertize (or title "(untitled)") 'face 'bold) parts)
           ;; Metadata line
           (let ((meta nil))
             (when creator-name (push (format "Creator: %s" creator-name) meta))
             (when (and updater-name
                        (not (equal updater-name creator-name)))
               (push (format "Updated by: %s" updater-name) meta))
             (when project-name (push (format "Project: %s" project-name) meta))
             (when initiative-name (push (format "Initiative: %s" initiative-name) meta))
             (when issue-id (push (format "Issue: %s" issue-id) meta))
             (when meta
               (push (string-join (nreverse meta) "  |  ") parts)))
           ;; Dates
           (let ((dates nil))
             (when created (push (format "Created: %s" created) dates))
             (when updated (push (format "Updated: %s" updated) dates))
             (when (and archived (not (eq archived :null)))
               (push (propertize (format "Archived: %s" archived)
                                 'face 'warning)
                     dates))
             (when dates
               (push (string-join (nreverse dates) "  |  ") parts)))
           ;; Icon/color if present
           (when (and icon (not (eq icon :null)) (stringp icon)
                      (not (string-empty-p icon)))
             (push (format "Icon: %s" icon) parts))
           (when (and color (not (eq color :null)) (stringp color)
                      (not (string-empty-p color)))
             (push (format "Color: %s" color) parts))
           ;; URL
           (when url
             (push (format "URL: %s" url) parts))
           ;; Content
           (when (and content (stringp content)
                      (not (string-empty-p content)))
             (push "---" parts)
             (push (bourdet--fontify-markdown content) parts))
           (string-join (nreverse parts) "\n"))
       (error nil)))))

(bourdet-register-tool-formatter
 "mcp__claude_ai_Linear__create_document"
 (lambda (tool-name input &optional _session)
   (let* ((title (alist-get 'title input))
          (project (alist-get 'project input))
          (content (alist-get 'content input))
          (parts nil))
     (when title
       (push (concat (propertize "Title: " 'face 'bold) title) parts))
     (when project
       (push (concat (propertize "Project: " 'face 'bold) project) parts))
     (when content
       (push (concat (propertize "Content:" 'face 'bold)
                     "\n" (bourdet--fontify-markdown content))
             parts))
     ;; Catch any unexpected fields
     (dolist (pair input)
       (unless (memq (car pair) '(title project content))
         (push (concat (propertize (format "%s: " (capitalize (symbol-name (car pair))))
                                   'face 'bold)
                       (if (stringp (cdr pair))
                           (cdr pair)
                         (format "%s" (cdr pair))))
               parts)))
     (when parts
       (string-join (nreverse parts) "\n")))))

(bourdet-register-tool-summary
 "mcp__claude_ai_Linear__create_document"
 (lambda (_tool-name input)
   (let ((title (alist-get 'title input))
         (project (alist-get 'project input)))
     (cons (concat "Create document"
                   (when title (concat " \"" title "\""))
                   (when project (concat " in " project)))
           nil))))

(bourdet-register-tool-formatter
 "mcp__claude_ai_Linear__update_document"
 (lambda (tool-name input &optional _session)
   (let* ((id (alist-get 'id input))
          (title (alist-get 'title input))
          (content (alist-get 'content input))
          (parts nil))
     (when id
       (push (concat (propertize "ID: " 'face 'bold) id) parts))
     (when title
       (push (concat (propertize "Title: " 'face 'bold) title) parts))
     (when content
       (push (concat (propertize "Content:" 'face 'bold)
                     "\n" (bourdet--fontify-markdown content))
             parts))
     ;; Catch any unexpected fields
     (dolist (pair input)
       (unless (memq (car pair) '(id title content))
         (push (concat (propertize (format "%s: " (capitalize (symbol-name (car pair))))
                                   'face 'bold)
                       (if (stringp (cdr pair))
                           (cdr pair)
                         (format "%s" (cdr pair))))
               parts)))
     (when parts
       (string-join (nreverse parts) "\n")))))

(bourdet-register-tool-summary
 "mcp__claude_ai_Linear__update_document"
 (lambda (_tool-name input)
   (let ((id (alist-get 'id input))
         (title (alist-get 'title input)))
     (cons (concat "Update document "
                   (or title id "?"))
           nil))))

(provide 'my-bourdet-extra-renderers)
