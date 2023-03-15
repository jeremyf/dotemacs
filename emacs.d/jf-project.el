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
  (let* ((project (or (s-presence project) (jf/project/find-dwim)))
	 (filename (cdar (jf/project/list-projects :project project))))
    (find-file filename)))

(bind-key "s-2" 'jf/project/jump-to/project-work-space)
(cl-defun jf/project/jump-to/project-work-space (project)
  "Prompt for PROJECT then workspace and open that workspace."
  (interactive (list (jf/project/find-dwim)))
  (let*
      ;; Get the project's file name
      ((filename (cdar (jf/project/list-projects :project project)))
       (paths-cons-list (with-current-buffer (find-file-noselect filename)
			  (cl-maplist #'read (cdar (org-collect-keywords '("PROJECT_PATHS"))))))
       (path-name (completing-read "Path: " paths-cons-list nil t))
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
     (t (progn
	  (message "WARNING: Project %s missing path name \"%s\" (with path %s)"
		   project path-name path)
	  (jf/project/jump-to/notes :project project))))))

(cl-defun jf/project/jump-to/timesheet (&key
					project
					(tag "projects")
					(within_headline
					 (format-time-string "%Y-%m-%d %A")))
  "Jump to the agenda for the given PROJECT."
  (interactive)
  (with-current-buffer (find-file jf/primary-agenda-filename-for-machine)
    (let* ((project (or (s-presence project) (jf/project/find-dwim)))
	   (start (org-element-map (org-element-parse-buffer)
		      'headline
		    ;; Finds the begin position of:
		    ;; - a level 4 headline
		    ;; - that is tagged as a :projects:
		    ;; - is titled as the given project
		    ;; - and is within the given headline
		    (lambda (hl)
		      (and (=(org-element-property :level hl) 4)
			   ;; I can't use the :title attribute as it
			   ;; is a more complicated structure; this
			   ;; gets me the raw string.
			   (string= project
				    (plist-get (cadr hl) :raw-value))
			   (member tag
				   (org-element-property :tags hl))
			   ;; The element must have an ancestor with
			   ;; a headline of today
			   (string= within_headline
				    (plist-get
				     ;; I want the raw title, no
				     ;; styling nor tags
				     (cadr
				      (car
				       (org-element-lineage hl)))
				     :raw-value))
			   (org-element-property :begin hl)))
		    nil t)))
      (if start
	  (progn
	    (goto-char start)
	    (pulsar-pulse-line))
	(progn
	  (end-of-buffer)
	  (user-error "No \"%s\" timesheet entry for today" project))))))

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
    (let ((project_path_to_code (string-replace
				 (getenv "HOME")
				 "~"
				 project_path_to_code_truename)))
      ;; How to handle multiple projects?  Prompt to pick one
      (let ((filename (s-trim (shell-command-to-string
			       (concat
				"rg \"^#\\+PROJECT_PATHS: .*"
				project_path_to_code " *$\" "
				directory " --files-with-matches "
				" --no-ignore-vcs --ignore-case")))))
	(unless (string-equal "" filename)
	  (with-current-buffer (find-file-noselect (file-truename filename))
	    (cadar (org-collect-keywords (list "PROJECT_NAME")))))))))

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
      (jf/project/get-project-from/current-agenda-buffer))))

(cl-defun jf/project/get-project-from/current-buffer-is-agenda (&key (buffer (current-buffer)))
  "Returns the current project's name or nil based on point in current buffer."
  (when (and (buffer-file-name buffer) (f-file-p (buffer-file-name buffer)))
    (when (string-equal
	   (file-truename (buffer-file-name buffer))
	   (file-truename jf/primary-agenda-filename-for-machine))
      (jf/project/get-project-from/current-agenda-buffer))))

(cl-defun jf/project/get-project-from/current-agenda-buffer ()
  "Find the `org-mode' task at point for the current buffer."
  (unless (eq 1 (point))
    (when-let ((element (org-element-at-point)))
      (if (member "projects" (org-element-property :tag element))
	  (org-element-property :raw-value element)
	(if (eq 'headline (org-element-type element))
	    ;; TODO: I'd prefer something a little more elegant and not
	    ;; presumptive about the structure of the agenda file.  But this
	    ;; works for now.
	    (pcase (org-element-property :level element)
	      (1 nil)
	      (2 nil)
	      (3 nil)
	      (4 (org-element-property :raw-value element))
	      (_ (progn
		   (org-up-heading-safe)
		   (jf/project/get-project-from/current-agenda-buffer))))
	  (progn
            (org-back-to-heading-or-point-min)
	    (jf/project/get-project-from/current-agenda-buffer)))))))

(defun jf/project/find-dwim ()
  "Find the current project based on context.

When the `current-prefix-arg' is set always prompt for the project."
  ;; `jf/project/get-project-from/current-agenda-buffer'
  (or
   (and (not current-prefix-arg)
	(or (jf/project/get-project-from/current-buffer-is-agenda)
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


(provide 'jf-project)
;;; jf-project.el ends here
