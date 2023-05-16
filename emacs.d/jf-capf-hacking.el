
(defun jf/version-control/issue-capf ()
    "Complete links."
    (cond
      ((looking-back "/[[:word:][:digit:]_\-]+#[[:digit:]]+" 40)
        (message "Project and issue"))
      ((looking-back "/[[:word:][:digit:]_\-]+" 40)
        (message "Project"))
      ((looking-back "#[[:digit:]]+" 40)
        (message "Issue")))
    ;; (when (looking-back "\\(/[[:word:][:digit:]_\-]+\\)?#[[:digit:]]+" 40)
      (let ((right (point))
             (left (or
                     (save-excursion
                       ;; First check for the project
                       (search-backward-regexp "/[[:word:][:digit:]_\-]+#[[:digit:]]+" (- (point) 40) nil) (point))
                     (save-excursion
                       ;; Now check for simply the issue number
                       (search-backward-regexp "#[[:digit:]]+" (- (point) 40)) (point)))))
        (list left right
          ;; Call without parameters, getting a links (filtered by CAPF magic)
          (jf/version-control/text)
          :exit-function
          #'jf/version-control/unfurl-issue-to-url
          ;; Proceed with the next completion function if the returned titles
          ;; do not match. This allows the default Org capfs or custom capfs
          ;; of lower priority to run.
          :exclusive 'yes))
  (defun jf/version-control/text ()
   (s-match-strings-all "\\(/[[:word:][:digit:]_\-]+\\)?#[[:digit:]]+" (buffer-string)))
  (defun jf/version-control/unfurl-issue-to-url (text _status)
    (let* ((parts (s-split "#" text))
            (issue (cadr parts))
            (project-dir (if (s-present? (car parts))
                           (format "%s/git%s" (getenv "HOME") (car parts))
                           (cdr (project-current)))))
    (delete-char (- (length text)))
      (insert (format "%s #%s" project-dir issue))))

(cl-defun jf/version-control/known-project-names (&key (prefix "/"))
  "Return a list of project, prepending PREFIX to each."
  (mapcar (lambda (proj)
            (concat prefix (f-base proj)))
    projectile-known-projects))

(cl-defun jf/version-control/unfurl-project-as-issue-url-template (project &key (prefix "/"))
  "Return the issue URL template for the given PROJECT.

Use the provided PREFIX for checks."
  (let* ((project-path
           (car (seq-filter (lambda (el)
                              (or
                                (s-ends-with? (concat project prefix) el)
                                (s-ends-with? project el)))
                  projectile-known-projects)))
          (remote
            (s-trim (shell-command-to-string
                      (format "cd %s && git remote get-url origin" project-path)))))
    (s-replace ".git" "/issues/%s" remote)))
