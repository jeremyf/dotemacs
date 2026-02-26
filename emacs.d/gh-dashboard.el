;;; gh-dashboard.el --- GitHub Dashboard -*- lexical-binding: t; -*-

(require 'json)
(require 'vtable)

(defvar ii/gh-dashboard-repos '()
  "List of GitHub repositories to track.")

(defgroup ii/gh-dashboard nil
  "GitHub Dashboard customization."
  :group 'tools)

(defface ii/gh-dashboard-repo-face
  '((t :weight bold :height 1.1 :foreground "#51afef"))
  "Face for repository names.")

(defface ii/gh-dashboard-header-face
  '((t :inherit font-lock-constant-face :weight bold :underline nil))
  "Face for section headers.")

(defun ii/gh-dashboard-visit-at-point ()
  "Visit the PR, Issue, Commit, or Workflow at point."
  (interactive)
  (let ((repo (get-text-property (point) 'ii-repo))
        (num  (get-text-property (point) 'ii-num))
        (sha  (get-text-property (point) 'ii-sha))
        (url  (get-text-property (point) 'ii-url)))
    (cond
     ;; 1. PR or Issue (Forge with Browser fallback)
     (num
      (condition-case nil
          (progn
            (require 'forge)
            (let ((forge-repo (forge-get-repository repo)))
              (if forge-repo
                  (forge-visit-topic (forge-get-topic forge-repo num))
                (message "Repo '%s' not indexed in Forge. Opening in browser..." repo)
                (browse-url (format "https://github.com/%s/issues/%d" repo num)))))
        (error (browse-url (format "https://github.com/%s/issues/%d" repo num)))))

     ;; 2. Commit (Browser for now, easier for multi-repo)
     (sha
      (browse-url (format "https://github.com/%s/commit/%s" repo sha)))

     ;; 3. Workflow Run (Browser)
     (url
      (browse-url url))

     (t (message "Nothing to visit here.")))))

(defun ii/gh-dashboard--fetch-repo (repo)
  "Fetch PRs, Issues, Commits, and Workflows for REPO."
  (message "Fetching %s..." repo)
  (let* ((parts (split-string repo "/"))
         (owner (car parts))
         (name (cadr parts))
         ;; 1. Fetch GraphQL Data (PRs, Issues, Commits with SHA)
         (gql-query (format "query {
  repository(owner: \"%s\", name: \"%s\") {
    pullRequests(states: OPEN, first: 10, orderBy: {field: CREATED_AT, direction: DESC}) {
      totalCount
      nodes { number title author { login } }
    }
    issues(states: OPEN, first: 10, orderBy: {field: CREATED_AT, direction: DESC}) {
      totalCount
      nodes { number title author { login } }
    }
    defaultBranchRef {
      target {
        ... on Commit {
          history(first: 5) {
            nodes {
              oid
              messageHeadline
              author { name user { login } }
              statusCheckRollup { state }
            }
          }
        }
      }
    }
  }
}" owner name))
         (json-object-type 'hash-table)
         (gql-buffer (generate-new-buffer " *gh-gql*"))
         (gql-exit (call-process "gh" nil gql-buffer nil "api" "graphql" "-f" (concat "query=" gql-query)))
         (gql-out (with-current-buffer gql-buffer (buffer-string)))
         ;; 2. Fetch Workflow Runs (REST API with URL)
         (run-buffer (generate-new-buffer " *gh-runs*"))
         (run-exit (call-process "gh" nil run-buffer nil "api" (format "repos/%s/actions/runs?per_page=5" repo)))
         (run-out (with-current-buffer run-buffer (buffer-string))))

    (unwind-protect
        (cond
         ((/= gql-exit 0) (list :repo repo :error (concat "GQL Error: " gql-out)))
         ((/= run-exit 0) (list :repo repo :error (concat "Run Error: " run-out)))
         (t
          (condition-case err
              (let* ((json (json-read-from-string gql-out))
                     (data (gethash "repository" (gethash "data" json)))
                     (branch-ref (gethash "defaultBranchRef" data))
                     (commits (when (and branch-ref (not (eq branch-ref 'json-null)))
                                (gethash "nodes" (gethash "history" (gethash "target" branch-ref)))))
                     (pr-data (gethash "pullRequests" data))
                     (issue-data (gethash "issues" data))
                     (run-json (json-read-from-string run-out))
                     (runs (append (gethash "workflow_runs" run-json) nil)))
                (list :repo repo
                      :pr-count (gethash "totalCount" pr-data)
                      :issue-count (gethash "totalCount" issue-data)
                      :prs (mapcar (lambda (node)
                                     (list :number (gethash "number" node)
                                           :title (gethash "title" node)
                                           :author (gethash "login" (gethash "author" node))))
                                   (append (gethash "nodes" pr-data) nil))
                      :issues (mapcar (lambda (node)
                                        (list :number (gethash "number" node)
                                              :title (gethash "title" node)
                                              :author (gethash "login" (gethash "author" node))))
                                      (append (gethash "nodes" issue-data) nil))
                      :commits (mapcar (lambda (c)
                                         (let* ((rollup (gethash "statusCheckRollup" c))
                                                (author (gethash "author" c))
                                                (user (gethash "user" author)))
                                           (list :sha (gethash "oid" c)
                                                 :msg (gethash "messageHeadline" c)
                                                 :author (or (gethash "name" author)
                                                             (when (and user (not (eq user 'json-null))) (gethash "login" user))
                                                             "Unknown")
                                                 :state (if (and rollup (not (eq rollup 'json-null)))
                                                            (gethash "state" rollup)
                                                          "NONE"))))
                                       commits)
                      :workflows (mapcar (lambda (r)
                                           (let ((actor (gethash "triggering_actor" r)))
                                             (list :title (gethash "display_title" r)
                                                   :author (or (gethash "login" actor) "Unknown")
                                                   :status (gethash "status" r)
                                                   :conclusion (gethash "conclusion" r)
                                                   :url (gethash "html_url" r))))
                                         runs)))
            (error (list :repo repo :error (error-message-string err))))))
      (kill-buffer gql-buffer)
      (kill-buffer run-buffer))))

(defun ii/gh-dashboard--render-workflow-icons (workflows)
  "Render status icons for WORKFLOWS."
  (if workflows
      (mapconcat (lambda (run)
                   (let* ((status (plist-get run :status))
                          (conclusion (plist-get run :conclusion)))
                     (cond ((string= status "completed")
                            (if (string= conclusion "success")
                                (propertize "●" 'face 'success)
                              (propertize "●" 'face 'error)))
                           (t (propertize "○" 'face 'warning)))))
                 workflows " ")
    (propertize "No runs" 'face 'shadow)))

(defun ii/gh-dashboard-show-details ()
  "Show the detailed breakout for the selected repository."
  (interactive)
  (let ((data (vtable-current-object)))
    (if (not data)
        (message "No repository selected.")
      (let ((buffer (get-buffer-create (format "*GH Detail: %s*" (plist-get data :repo)))))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (ii/gh-dashboard--render-detailed-breakout data)
            (goto-char (point-min))
            (read-only-mode 1)
            (local-set-key (kbd "q") 'quit-window)
            (local-set-key (kbd "RET") 'ii/gh-dashboard-visit-at-point)))
        (pop-to-buffer buffer)))))

(defun ii/gh-dashboard--render-detailed-breakout (data)
  "Render the detailed breakout (Commits/Workflows) for DATA."
  (let ((repo (plist-get data :repo))
        (error (plist-get data :error)))
    (insert (propertize repo 'face 'ii/gh-dashboard-repo-face))
    (if error
        (insert "\n  " (propertize error 'face 'error) "\n\n")
      (insert (propertize (format "  (PRs: %d, Issues: %d)"
                                  (plist-get data :pr-count)
                                  (plist-get data :issue-count))
                          'face 'shadow) "\n\n")

      ;; Pull Requests Section
      (let ((prs (plist-get data :prs)))
        (when prs
          (insert (propertize "Open Pull Requests" 'face 'ii/gh-dashboard-header-face) "\n")
          (dolist (pr prs)
            (let ((line (format "  #%-5d %s: %s\n"
                                (plist-get pr :number)
                                (propertize (format "%-12s" (plist-get pr :author)) 'face 'font-lock-variable-name-face)
                                (plist-get pr :title))))
              (insert (propertize line
                                  'ii-repo repo
                                  'ii-num (plist-get pr :number)
                                  'mouse-face 'highlight))))
          (insert "\n")))

      ;; Issues Section
      (let ((issues (plist-get data :issues)))
        (when issues
          (insert (propertize "Open Issues" 'face 'ii/gh-dashboard-header-face) "\n")
          (dolist (issue issues)
            (let ((line (format "  #%-5d %s: %s\n"
                                (plist-get issue :number)
                                (propertize (format "%-12s" (plist-get issue :author)) 'face 'font-lock-variable-name-face)
                                (plist-get issue :title))))
              (insert (propertize line
                                  'ii-repo repo
                                  'ii-num (plist-get issue :number)
                                  'mouse-face 'highlight))))
          (insert "\n")))

      ;; Commits Section
      (insert (propertize "Recent Commits" 'face 'ii/gh-dashboard-header-face) "\n")
      (dolist (commit (plist-get data :commits))
        (let* ((state (plist-get commit :state))
               (icon (cond ((string= state "SUCCESS") (propertize "●" 'face 'success))
                           ((member state '("FAILURE" "ERROR")) (propertize "●" 'face 'error))
                           ((member state '("PENDING" "EXPECTED")) (propertize "○" 'face 'warning))
                           (t (propertize "○" 'face 'shadow))))
               (line (format "  %s  %s: %s\n"
                             icon
                             (propertize (format "%-12s" (plist-get commit :author)) 'face 'font-lock-variable-name-face)
                             (plist-get commit :msg))))
          (insert (propertize line
                              'ii-repo repo
                              'ii-sha (plist-get commit :sha)
                              'mouse-face 'highlight))))
      (insert "\n")

      ;; Workflows Section
      (insert (propertize "Recent Workflows" 'face 'ii/gh-dashboard-header-face) "\n")
      (if (plist-get data :workflows)
          (dolist (run (plist-get data :workflows))
            (let* ((status (plist-get run :status))
                   (conclusion (plist-get run :conclusion))
                   (icon (cond ((string= status "completed")
                                (if (string= conclusion "success")
                                    (propertize "●" 'face 'success)
                                  (propertize "●" 'face 'error)))
                               (t (propertize "○" 'face 'warning))))
                   (line (format "  %s  %s: %s\n"
                                 icon
                                 (propertize (format "%-12s" (plist-get run :author)) 'face 'font-lock-variable-name-face)
                                 (plist-get run :title))))
              (insert (propertize line
                                  'ii-url (plist-get run :url)
                                  'mouse-face 'highlight))))
        (insert (propertize "  No workflow history found.\n" 'face 'shadow)))
      (insert "\n  Press 'q' to return, 'RET' on any item to visit it."))))

(defun ii/gh-dashboard ()
  "Display the GitHub Dashboard using vtable."
  (interactive)
  (let* ((buffer (get-buffer-create "*GH Dashboard*"))
         (repo-data (mapcar #'ii/gh-dashboard--fetch-repo ii/gh-dashboard-repos)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Remove underlines from all text in this buffer
        (face-remap-add-relative 'default :underline nil)
        (face-remap-add-relative 'vtable-header :underline nil)

        (insert (propertize "GitHub Dashboard\n" 'face '(:height 1.5 :weight bold :underline nil)))
        (insert (propertize (concat "Updated: " (current-time-string) "\n\n") 'face '(:inherit shadow :underline nil)))

        (make-vtable
         :columns '((:name "Repository" :width 35)
                    (:name "PRs" :width 6 :align right)
                    (:name "Issues" :width 12 :align right)
                    (:name "Workflow Status" :width 20))
         :objects repo-data
         :separator-width 3
         :getter (lambda (object index table)
                   (cond
                    ((eq index 0) (propertize (plist-get object :repo) 'face 'ii/gh-dashboard-repo-face))
                    ((eq index 1) (number-to-string (or (plist-get object :pr-count) 0)))
                    ((eq index 2) (number-to-string (or (plist-get object :issue-count) 0)))
                    ((eq index 3) (ii/gh-dashboard--render-workflow-icons (plist-get object :workflows)))))
         :sort-by '((0 . ascend))
         :keymap (let ((map (make-sparse-keymap)))
                   (define-key map (kbd "RET") 'ii/gh-dashboard-show-details)
                   (define-key map (kbd "g") 'ii/gh-dashboard)
                   (define-key map (kbd "q") 'quit-window)
                   map))

        (goto-char (point-min))
        (read-only-mode 1)))
    (switch-to-buffer buffer)))

(provide 'gh-dashboard)
;;; gh-dashboard.el ends here
