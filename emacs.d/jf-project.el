;;; jf-project --- Connective Tissue for Projects -*- lexical-binding: t -*-

;; Copyright (C) 2022 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary

;; I have primary spaces where I work on a project:
;;
;; - Agenda :: Where I track time.
;; - Code :: Where I write code.
;; - Discussion :: Where I discuss the project with collaborators.
;; - Notes :: Where I take larger conceptual notes.
;; - Project board :: Where I see what's in flight.
;; - Remote :: Where I read/write issues and pull requests.
;;
;; I want a quick way navigate to those various workspaces.  I wrote
;; `jf/project/navigation-menu'.  I can set a current project via
;; `jf/project/current-project-set'.  With that set, I can quickly jump to the
;; aforementioned project workspaces.
;;
;; If one is not set, I use `jf/project/find-dwim' to attempt to determine
;; context.  And failing that, the function prompts for a project from a known
;; list.  (I can also provide a prefix arg which short-circuits the DWIM fuction
;; to always prompt for a project.)
;;
;; The list of workspaces is hard-coded but easy to envision an arbitrary list
;; of keywords.
;;
;; The present implementation leverages `org-collect-keywords'; looking at lines
;; that are of the form "#+ARBITRARY_KEY:".  I could scan for keywords that
;; match a given form.
;;
;; See https://takeonrules.com/2022/11/19/project-dispatch-menu-with-org-mode-metadata-denote-and-transient/
;; for some of the background of this package.

;;; Code

;;;; Dependencies
(require 's)
(require 'f)
(require 'transient)
(require 'pulsar)
(require 'jf-org-mode)

;;;; Interactive Commands
(cl-defun jf/project/jump-to-timesheet (&key
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
	  (error "No \"%s\" timesheet entry for today" project))))))

(cl-defun jf/project/jump-to-notes (&key project)
  "Jump to the given PROJECT's notes file.

Determine the PROJECT by querying `jf/project/list-projects'."
  (interactive)
  (let* ((project (or (s-presence project) (jf/project/find-dwim)))
	 (filename (cdar (jf/project/list-projects :project project))))
    (find-file filename)))

(defmacro jf/project/create-jump-to-fn (key)
  (let* ((default_keyword (concat "PROJECT_PATH_TO_" (upcase key)))
	 (key (downcase key))
	 (fn (intern (concat "jf/project/jump-to-" key)))
	 (docstring (concat "Jump to the given PROJECT's " key ".\n\n
Check URL, directory, and filename.")))
    `(cl-defun ,fn (&key project (keyword ,default_keyword))
       ,docstring
       (interactive)
       (let* ((project (or (s-presence project) (jf/project/find-dwim)))
	      (filename (cdar (jf/project/list-projects :project project))))
	 (with-current-buffer (find-file-noselect filename)
	   (if-let ((path (cadar (org-collect-keywords (list keyword)))))
	       (cond
		((s-starts-with? "http" path)
		 (eww-browse-with-external-browser path))
		((f-dir-p path)
		 (dired path))
		((f-file-p path)
		 (find-file path))
		(t (progn
		     (message "WARNING: Project %s missing %s \"%s\""
			      project ,key path)
		     (jf/project/jump-to-notes :project project))))
	     (progn
	       (message "WARNING: Project %s has no %s" project ,key)
	       (jf/project/jump-to-notes :project project))))))))

(jf/project/create-jump-to-fn "board")
(jf/project/create-jump-to-fn "code")
(jf/project/create-jump-to-fn "remote")
(jf/project/create-jump-to-fn "discussion")

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

(cl-defun jf/project/get-project-from-project-current (&key (directory org-directory))
  "Return the current \"noted\" project name.

Return nil if the current buffer is not part of a noted project.

Noted projects would be found within the given DIRECTORY."
  (when-let ((project_path_to_code_truename (cdr (project-current))))
    (let ((project_path_to_code (string-replace
				 (getenv "HOME")
				 "~"
				 project_path_to_code_truename)))
      (let ((filename (s-trim (shell-command-to-string
			       (concat
				"rg \"^#\\+PROJECT_PATH_TO_CODE: +"
				project_path_to_code " *$\" "
				directory " --files-with-matches "
				" --no-ignore-vcs")))))
	(unless (string-equal "" filename)
	  (with-current-buffer (find-file-noselect (file-truename filename))
	    (cadar (org-collect-keywords (list "PROJECT_NAME")))))))))

(defun jf/project/get-project-from-current-clock ()
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
      (jf/project/get-project-from-current-agenda-buffer))))

(cl-defun jf/project/get-project-from-current-buffer-is-agenda (&key (buffer (current-buffer)))
  "Returns the current project's name or nil based on point in current buffer."
  (when (and (buffer-file-name buffer) (f-file-p (buffer-file-name buffer)))
    (when (string-equal
	   (file-truename (buffer-file-name buffer))
	   (file-truename jf/primary-agenda-filename-for-machine))
      (jf/project/get-project-from-current-agenda-buffer))))

(cl-defun jf/project/get-project-from-current-agenda-buffer ()
  "Find the `org-mode' task at point for the current buffer."
  (unless (eq 1 (point))
    (when-let ((element (org-element-at-point)))
      (if (member "project" (org-element-property :tag element))
	  (org-element-property :title element)
	(if (eq 'headline (org-element-type element))
	    ;; TODO: I'd prefer something a little more elegant and not
	    ;; presumptive about the structure of the agenda file.  But this
	    ;; works for now.
	    (pcase (org-element-property :level element)
	      (1 nil)
	      (2 nil)
	      (3 nil)
	      (4 (org-element-property :title element))
	      (_ (progn
		   (org-up-heading-safe)
		   (jf/project/get-project-from-current-agenda-buffer))))
	  (progn
            (org-back-to-heading-or-point-min)
	    (jf/project/get-project-from-current-agenda-buffer)))))))

(defun jf/project/find-dwim ()
  "Find the current project based on context.

When the `current-prefix-arg' is set always prompt for the project."
  ;; `jf/project/get-project-from-current-agenda-buffer'
  (or
   (and (not current-prefix-arg)
	(or (jf/project/get-project-from-current-clock)
	    (jf/project/get-project-from-current-buffer-is-agenda)
	    (jf/project/get-project-from-project-current)))
   (completing-read "Project: " (jf/project/list-projects))))

;;;; Dispatch Menu

;; I want a single command that will popup a menu.  Similar to my old org-roam
;; context menus, I want to set the current project.
;;
;; I want the lower case keys to be for the current project and the upper case
;; keys to prompt for project.

(defvar jf/project/current-project
  nil
  "The current contextual project.")

(bind-key "s-2" 'jf/project/navigation-menu)
(transient-define-prefix jf/project/navigation-menu ()
  "My Project menu."
  ["Projects"
   ["Current project"
    ("." jf/project/current-project-set :transient t)
    ("b" "Board…" (lambda () (interactive)
		    (jf/project/jump-to-board
		     :project jf/project/current-project)))
    ("c" "Code…" (lambda () (interactive)
		   (jf/project/jump-to-code
		    :project jf/project/current-project)))
    ("d" "Discussion…" (lambda () (interactive)
			 (jf/project/jump-to-discussion
			  :project jf/project/current-project)))
    ("n" "Notes…" (lambda () (interactive)
		    (jf/project/jump-to-notes
		     :project jf/project/current-project)))
    ("r" "Remote…" (lambda () (interactive)
		     (jf/project/jump-to-remote
		      :project jf/project/current-project)))
    ("t" "Timesheet…" (lambda () (interactive)
		     (jf/project/jump-to-timesheet
		      :project jf/project/current-project)))]
   ["Other projects"
    ""
    ("B" "Board…" jf/project/jump-to-board)
    ("C" "Code…" jf/project/jump-to-code)
    ("D" "Discussion…" jf/project/jump-to-discussion)
    ("N" "Notes…" jf/project/jump-to-notes)
    ("R" "Remote…" jf/project/jump-to-remote)
    ("T" "Timesheet…" jf/project/jump-to-timesheet)]])

(transient-define-suffix jf/project/current-project-set (project)
  "Select FILES to use as source for work desk."
  :description '(lambda ()
		  (concat
		   "Current: "
		   (propertize
		    (format "%s" jf/project/current-project)
		    'face 'transient-argument)))
  (interactive (list (completing-read "Project: " (jf/project/list-projects))))
  ;; Ensure that the empty
  (setq jf/project/current-project (s-presence project)))

(provide 'jf-project)
;;; jf-project.el ends here
