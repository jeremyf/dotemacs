;;; jf-project --- Connective Tissue for Projects -*- lexical-binding: t -*-

;; Copyright (C) 2022 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;; I have five primary places where I work on a project:
;;
;; - Agenda
;; - Code
;; - Notes
;; - Project board
;; - Remote
;;
;; These are the standard elements.
;;
;; Ultimately, I want a quick way to bring up a menu of options to jump to a
;; location.  That menu can be based on high-level properties of the
;; corresponding note.
;;
;; But I can envision having an arbitrary list.  What would that list look like?
;;
;; The present implementation leverages `org-collect-keywords'; looking at lines
;; that are of the form "#+ARBITRARY_KEY:".  I could scan for keywords that
;; match a given form.

;;; Code

;;;; Dependencies
(require 'jf-denote)
(require 'jf-org-mode)
(require 'transient)

;;;; Interactive Commands

(cl-defun jf/project/jump-to-agenda (&optional project
				     &key
				     (tag "project")
				     (within_headline
				      (format-time-string "%Y-%m-%d %A")))
  "Jump to the agenda for the given PROJECT."
  (interactive)
  (let ((the-project (or project (jf/project/find-dwim))))
    (with-current-buffer (find-file jf/primary-agenda-filename-for-machine)
      (let ((start (org-element-map (org-element-parse-buffer)
		       'headline
		     ;; Finds the begin position of:
		     ;; - a level 4 headline
		     ;; - that is tagged as a :project:
		     ;; - is titled as the given project
		     ;; - and is within the given headline
		     (lambda (hl)
		       (and (=(org-element-property :level hl) 4)
			    ;; I can't use the :title attribute as it
			    ;; is a more complicated structure; this
			    ;; gets me the raw string.
			    (string= the-project
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
	(goto-char start)
	(pulsar-pulse-line)))))

(cl-defun jf/project/jump-to-board (&optional
				    project
				    &key (keyword "PROJECT_PATH_TO_BOARD"))
  "Jump to the given PROJECT's project board."
  (interactive)
  (let* ((the-project (or project (jf/project/find-dwim)))
	 (filename (cdar (jf/project/list-projects :project the-project))))
    (with-current-buffer (find-file-noselect filename)
      (let ((url (cadar (org-collect-keywords (list keyword)))))
	(eww-browse-with-external-browser url)))))

(cl-defun jf/project/jump-to-code (&optional
				   project
				   &key
				   (keyword "PROJECT_PATH_TO_CODE"))
  "Jump to the given PROJECT's source code."
  (interactive)
  (let* ((the-project (or project (jf/project/find-dwim)))
	 (filename (cdar (jf/project/list-projects :project the-project))))
    (with-current-buffer (find-file-noselect filename)
      (let ((filename (file-truename (cadar
				      (org-collect-keywords (list keyword))))))
	(if (f-dir-p filename)
	    (dired filename)
	  (find-file filename))))))

(cl-defun jf/project/jump-to-notes (&optional project)
  "Jump to the given PROJECT's notes file.

Determine the PROJECT by querying `jf/project/list-projects'."
  (interactive)
  (let* ((the-project (or project (jf/project/find-dwim)))
	 (filename (cdar (jf/project/list-projects :project the-project))))
    (find-file filename)))

(cl-defun jf/project/jump-to-remote (&optional
				     project
				     &key
				     (keyword "PROJECT_PATH_TO_REMOTE"))
  "Jump to the given PROJECT's remote."
  (interactive)
  (let* ((the-project (or project (jf/project/find-dwim)))
	 (filename (cdar (jf/project/list-projects :project the-project))))
    (with-current-buffer (find-file-noselect filename)
      (let ((url (cadar (org-collect-keywords (list keyword)))))
	(eww-browse-with-external-browser url)))))

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

;; (defun jf/project/org-agenda-project-at-point ()
;;   "Find the `org-mode' task at point."
;;   (if (eq 1 (point))
;;       nil
;;     (let ((element (org-element-at-point)))
;;       (if (eq 'headline (org-element-type element))
;; 	  (if (member "project" (org-element-property :tag element))
;; 	      (org-element-property :title element)
;; 	    (progn
;; 	      (org-up-heading-safe)
;; 	      (jf/project/org-agenda-project-at-point)))
;; 	(progn
;;           (org-back-to-heading-or-point-min)
;; 	  (unless (eq 1 (point))
;;             (jf/project/org-agenda-project-at-point)))))))

(defun jf/project/find-dwim ()
  "Find the current project."
  ;; Ideally this would be able to be smart about the current buffer and make
  ;; some decisions.  However, I was struggling with
  ;; `jf/project/org-agenda-project-at-point'
  (completing-read "Project: " (jf/project/list-projects)))

;;;; Dispatch Menu


;; I want a single command that will popup a menu.  Similar to my old org-roam
;; context menus, I want to set the current project.
;;
;; I want the lower case keys to be for the current project and the upper case
;; keys to prompt for project.

(defvar jf/project/current-project
  nil
  "The current contextual project.")

(bind-key "s-2" 'jf/project/menu)
(transient-define-prefix jf/project/menu ()
  "My Project menu."
  ["Projects"
   ["Current project"
    ("a" "Agenda…" (lambda () (interactive)
		     (jf/project/jump-to-agenda jf/project/current-project)))
    ("b" "Board…" (lambda () (interactive)
		    (jf/project/jump-to-board jf/project/current-project)))
    ("c" "Code…" (lambda () (interactive)
		   (jf/project/jump-to-code jf/project/current-project)))
    ("n" "Notes…" (lambda () (interactive)
		    (jf/project/jump-to-notes jf/project/current-project)))
    ("r" "Remote…" (lambda () (interactive)
		     (jf/project/jump-to-remote jf/project/current-project)))
    ("." jf/project/transient-current-project :transient t)]
   ["Other projects"
    ("A" "Agenda…" jf/project/jump-to-agenda)
    ("B" "Board…" jf/project/jump-to-board)
    ("C" "Code…" jf/project/jump-to-code)
    ("N" "Notes…" jf/project/jump-to-notes)
    ("R" "Notes…" jf/project/jump-to-remote)]
   ])

(transient-define-suffix jf/project/transient-current-project (project)
  "Select FILES to use as source for work desk."
  :description '(lambda ()
		  (concat
		   "Current Project:"
		   (propertize
		    (format "%s" jf/project/current-project)
		    'face 'transient-argument)))
  (interactive (list (completing-read "Project: " (jf/project/list-projects))))
  (setq jf/project/current-project project))

(provide 'jf-project)
;;; jf-project.el ends here
