;;; jf-project --- Connective Tissue for Projects -*- lexical-binding: t -*-

;; Copyright (C) 2022 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary

;; There are three interactive functions:
;;
;; - `jf/project/jump-to/notes'
;; - `jf/project/jump-to/project-work-space'
;; - `jf/project/jump-to/timesheet'
;;
;; Let's talk of the three targets for jumping.
;;
;; Notes: Each project has an index.  The index is a place for high-level notes
;; and links to related concepts:
;;
;; Project Space: Each project has different spaces where I do work, examples
;; include the following:
;;
;; - Agenda :: Where I track time.
;; - Code :: Where I write code.
;; - Discussion :: Where I discuss the project with collaborators.
;; - Notes :: Where I take larger conceptual notes.
;; - Project board :: Where I see what's in flight.
;; - Remote :: Where I read/write issues and pull requests.
;;
;; Timesheet: For many projects, I track time.  This lets me jump to today's
;; headline for the given project.  The headline is where I record tasks to
;; which I then track time.
;;
;; Each project's index is assumed to be an `org-mode' file with two top-level
;; keywords:
;;
;; `#+PROJECT_NAME:'
;; `#+PROJECT_PATHS:'
;;
;; There should be one `#+PROJECT_NAME:' keyword and there can be many
;; `#+PROJECT_PATHS:'.  Each `#+PROJECT_PATHS:' is a `cons' cell.  The `car' is
;; the label and the `cdr' is the path.  The path can be a filename or a URL.
;;
;; The `jf/project/jump-to/project-work-space' will prompt for a project then a
;; workspace.  From there, it will jump to the corresponding location.

;;; Code

;;;; Dependencies
(require 's)
(require 'f)
(require 'pulsar)
(require 'jf-org-mode)

;;;; Interactive Commands
(cl-defun jf/project/jump-to/notes (&key project)
  "Jump to the given PROJECT's notes file.

Determine the PROJECT by querying `jf/project/list-projects'."
  (interactive)
  (let* ((project (or (s-presence project)
                    (jf/project/find-dwim)))
	        (filename (cdar (jf/project/list-projects :project project))))
    (find-file filename)))

(bind-key "s-2" 'jf/project/jump-to/project-work-space)
(defun jf/project/jump-to/project-work-space (project)
  "Prompt for PROJECT then workspace and open that workspace."
  (interactive (list (jf/project/find-dwim)))
  (let*
    ;; Get the project's file name
    ((filename (cdar (jf/project/list-projects :project project)))
      (paths-cons-list (jf/project/project-paths-for filename))
      (path-name (completing-read (format "Links for %s: " project) paths-cons-list nil t))
      (path (alist-get path-name paths-cons-list nil nil #'string=)))
    (cond
      ((s-starts-with? "http" path)
        (eww-browse-with-external-browser path))
      ((f-dir-p path)
        (dired path))
      ((f-file-p path)
        (if (string= "pdf" (f-ext path))
	        (shell-command (concat "open " path))
	        (find-file path)))
      ;; Try the path as an org-link (e.g. path == "denote:20230328T093100")
      (t (when-let* ((type-target (s-split ":" path))
                      ;; There's a registered handler for the protocol
                      ;; (e.g. "denote")
                      (follow-func (org-link-get-parameter
                                     (car type-target) :follow)))
           (funcall follow-func (cadr type-target))
           ;; We tried...and don't know how to handle this.
           (progn
	           (message "WARNING: Project %s missing path name \"%s\" (with path %s)"
		           project path-name path)
	           (jf/project/jump-to/notes :project project)))))))

(defun jf/project/project-paths-for (filename)
  "Find the project paths for the given FILENAME.

Added in cases where we want to inject the actual file."
  (with-current-buffer (find-file-noselect filename)
    (let ((paths (cl-maplist #'read (cdar (org-collect-keywords '("PROJECT_PATHS"))))))
      (setq paths (cons (cons "Notes" filename) paths)))))

;;;; Support Functions
(cl-defun jf/project/list-projects (&key (project ".+")
					                           (directory org-directory))
  "Return a list of `cons' that match the given PROJECT.

The `car' of the `cons' is the project (e.g. \"Take on Rules\").
The `cdr' is the fully qualified path to that projects notes file.

The DIRECTORY defaults to `org-directory' but you can specify otherwise."
  (mapcar (lambda (line)
	          (let* ((slugs (s-split ":" line))
		                (proj (s-trim (car (cdr slugs))))
		                (filename (s-trim (car slugs))))
	            (cons proj filename)))
	  (split-string-and-unquote
	    (shell-command-to-string
	      (concat
	        "rg \"^#\\+PROJECT_NAME: +(" project ") *$\" " directory
	        " --only-matching --no-ignore-vcs --with-filename -r '$1' "
	        "| tr '\n' '@'"))
	    "@")))

(cl-defun jf/project/get-project-from/project-source-code (&key (directory org-directory))
  "Return the current \"noted\" project name.

Return nil if the current buffer is not part of a noted project.

Noted projects would be found within the given DIRECTORY."
  (when-let ((project_path_to_code_truename (cdr (project-current))))
    (let ((project_path_to_code (jf/filename/tilde-based
                                  project_path_to_code_truename)))
      ;; How to handle multiple projects?  Prompt to pick one
      (let ((filename (s-trim (shell-command-to-string
			                          (concat
				                          "rg \"^#\\+PROJECT_PATHS: .*"
				                          project_path_to_code " *\\\"\" "
				                          directory " --files-with-matches "
				                          " --no-ignore-vcs --ignore-case")))))
	      (unless (string-equal "" filename)
	        (with-current-buffer (find-file-noselect (file-truename filename))
            (jf/project/get-project-from/current-buffer-is-project)))))))

(defun jf/project/get-project-from/current-clock ()
  "Return the current clocked project's name or nil."
  ;; This is a naive implementation that assumes a :task: has the clock.  A
  ;; :task:'s immediate ancestor is a :projects:.
  (when-let ((m (and
		              (fboundp 'org-clocking-p) ;; If this isn't set, we ain't
		              ;; clocking.
		              (org-clocking-p)
		              org-clock-marker)))
    (with-current-buffer (marker-buffer m)
      (goto-char m)
      (jf/project/get-project-from/current-buffer-is-project))))

(defun jf/project/get-project-from/current-buffer-is-project ()
  "Return the PROJECT_NAME keyword of current buffer."
  (cadar (org-collect-keywords (list "PROJECT_NAME"))))

(defun jf/project/find-dwim ()
  "Find the current project based on context.

When the `current-prefix-arg' is set always prompt for the project."
  ;; `jf/project/get-project-from/current-agenda-buffer'
  (or
    (and (not current-prefix-arg)
	    (or
        (jf/project/get-project-from/current-buffer-is-project)
	      (jf/project/get-project-from/current-clock)
	      (jf/project/get-project-from/project-source-code)))
    (completing-read "Project: " (jf/project/list-projects))))

;; The default relevant `magit-list-repositories'
;; The following command shows all "project" directories
;;
(defvar jf/git-project-paths
  '(("~/git/takeonrules.source/" . 1)
     ("~/git/burning_wheel_lifepaths/" . 1)
     ("~/git/dotzshrc/" .  1)
     ("~/git/dotemacs/" . 1)
     ("~/git/emacs-bookmarks/" . 1)
     ("~/git/org" . 1)
     ("~/git/takeonrules.source/themes/hugo-tufte" . 1))
  "A list of `con' cells where the `car' is the name of a directory
and the `cdr' is a ranking.  I have pre-populated this list with
repositories that are generally available on both machines.")

(defun jf/git-project-paths/dynamic ()
  "Return a list of code repository paths."
  (split-string-and-unquote
    (s-trim-right
      (shell-command-to-string
        (concat
          "rg \"^#\\+PROJECT_PATHS: +[^\\.]+\\. +\\\"(~/git/[^/]+/)\\\"\\)\" "
          "~/git/org --no-ignore-vcs --replace='$1' "
          "--only-matching --no-filename")))
    "\n"))

(dolist (path (jf/git-project-paths/dynamic))
  (add-to-list 'jf/git-project-paths (cons path 1)))

(setq magit-repository-directories jf/git-project-paths)

(defun jf/org-mode/agenda-files ()
  "Return a list of note files containing 'agenda' tag.

Uses the fd command (see https://github.com/sharkdp/fd)

We want files to have the 'projects' `denote' keyword."
  (let ((projects (mapcar (lambda (el) (cdr el)) (jf/project/list-projects))))
    (when (file-exists-p jf/agenda-filename/scientist) (setq projects (cons jf/agenda-filename/scientist projects)))
    (when (file-exists-p jf/agenda-filename/personal) (setq projects (cons jf/agenda-filename/personal projects)))
    projects))

(defun jf/org-mode/agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (jf/org-mode/agenda-files)))
(advice-add 'org-agenda :before #'jf/org-mode/agenda-files-update)
(advice-add 'org-todo-list :before #'jf/org-mode/agenda-files-update)
(add-hook 'after-init-hook #'jf/org-mode/agenda-files-update)

(defun jf/org-mode/capture/project-task/find ()
  "Find the project file and position to the selected task."
  (let* ((project (jf/project/find-dwim))
          (filename (cdar (jf/project/list-projects :project project)))
          (tasks (jf/org-mode/existing-tasks filename))
          (task-name (completing-read (format "Task for %s: " project) tasks)))
    ;; Defer finding this file as long as possible.
    (find-file filename)
    (if-let (task (alist-get task-name tasks nil nil #'string=))
      (goto-char (org-element-property :contents-end task))
      (progn
        (goto-char (point-max))
        ;; Yes make this a top-level element.  It is easy to demote and move
        ;; around.
        (insert "* TODO " task-name " :tasks:\n\n")))))

(defun jf/org-mode/existing-tasks (&optional filename)
  "Return an alist of existing tasks in given FILENAME.

Each member's `car' is title and `cdr' is `org-mode' element.

Members of the sequence either have a tag 'tasks' or are in a todo state."
  (with-current-buffer (or (and filename (find-file-noselect filename)) (current-buffer))
    (mapcar (lambda (headline)
              (cons (org-element-property :title headline) headline))
      (org-element-map
        (org-element-parse-buffer 'headline)
        'headline
        (lambda (headline)
          (and
            (or (eq (org-element-property :todo-type headline) 'todo)
              (member "tasks" (org-element-property :tags headline)))
            headline))))))

(add-to-list 'org-capture-templates
  '("t" "Task (for Project)"
    plain (function jf/org-mode/capture/project-task/find)
    "%i\n%?"
    :empty-lines-before 1
    :empty-lines-after 1
     :jump-to-capture t))

(defun jf/org-mode/blog-entry? (&optional buffer)
  (when-let* ((buffer (or buffer (current-buffer)))
               (file (buffer-file-name buffer)))
    (and (denote-file-is-note-p file)
      (string-match-p "\\/blog-posts\\/" file))))

(cl-defun jf/denote? (&key (buffer (current-buffer)))
  (when-let* ((file (buffer-file-name buffer)))
    (denote-file-is-note-p file)))

(transient-define-suffix jf/project/convert-document-to-project (&optional buffer)
  "Conditionally convert the current BUFFER to a project.

This encodes the logic for creating a project."
  :description "Convert to project…"
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (if-let* ((file (buffer-file-name buffer))
                 (_proceed (and
                   (denote-file-is-note-p file)
                   (derived-mode-p 'org-mode)
                   (not (jf/project/get-project-from/current-buffer-is-project))))
                 (existing-title (org-get-title))
                 (file-type (denote-filetype-heuristics file))
                 (keywords (denote-retrieve-keywords-value file file-type)))
        (progn
          ;; Goto the 5th line
          (goto-line 5)
          (insert "\n#+PROJECT_NAME: " existing-title
            "\n#+CATEGORY: " existing-title)
          (setq keywords (cons "projects" keywords))
          (denote-rewrite-keywords file keywords file-type)
          (call-interactively #'denote-rename-file-using-front-matter))
        (user-error "Unable to convert buffer to project")))))

(provide 'jf-project)
;;; jf-project.el ends here
