;;; jf-org-mode.el --- Org-Mode configuratoino -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;;; Code
;; Pre-amble to prepare for `org-mode'

;; I maintain a list of data directories, each might have “relevant to
;; org-mode” files.  The `jf/org-agenda-files' reads the file system to gather
;; sources for `org-mode' agenda.
(defun jf/is-work-machine? ()
  "Am I working on my machine"
  (f-file? (file-truename "~/git/org/denote/scientist/20221021T221357--scientist-agenda__scientist.org")))

(defvar jf/primary-agenda-filename-for-machine
  (if (jf/is-work-machine?)
      "~/git/org/denote/scientist/20221021T221357--scientist-agenda__scientist.org"
    "~/git/org/agenda.org"))

(defconst jf/data-directories
  (list
   jf/tor-home-directory
   "~/git/org/scientist/"
   "~/git/org/denote/scientist"
   "~/git/dotzshrc/"
   "~/git/dotemacs/"
   "~/git/org/")
  "Relevant data directories for my day to day work.")

(cl-defun jf/org-agenda-files (&key
			       (paths jf/data-directories)
			       (basenames '("agenda.org")))
  "Return the list of filenames where BASENAMES exists in PATHS."
  ;; I want to include my configuration file in my agenda querying.
  (setq returning-list '("~/git/org/denote/scientist/20221021T221357--scientist-agenda__scientist.org"))
  (dolist (path paths)
    (dolist (basename basenames)
      (when (f-exists-p (f-join path basename))
	(add-to-list 'returning-list (f-join path basename)))))
  returning-list)
    ;;; Begin Org Mode (all it's glory)
(use-package org
  :straight (org :type built-in)
  :hook (org-mode . turn-on-visual-line-mode)
  (org-mode . jf/org-completion-at-point-functions)
  ;; Disable org-indent-mode; it's easy enough to enable.  The primary reason is
  ;; that it does not play nice with the multi-cursor package.  And I'd prefer
  ;; to have that work better by default.
  ;;
  ;; (org-mode . org-indent-mode)
  :bind ("C-j" . jf/jump-to-agenda-or-mark)
  :bind (:map org-mode-map ("C-j" . jf/jump-to-agenda-or-mark))
  :custom (org-use-speed-commands t)
  (org-time-stamp-rounding-minutes '(0 15))
  :config
  (setq org-confirm-babel-evaluate #'jf/org-confirm-babel-evaluate
	;; I'd prefer to use the executable, but that doe not appear to be the
	;; implementation of org-babel.
	org-plantuml-jar-path (concat
			       (string-trim
				(shell-command-to-string "brew-path plantuml"))
			       "/libexec/plantuml.jar")
	org-insert-heading-respect-content t
	org-catch-invisible-edits 'show-and-error
	org-use-fast-todo-selection 'expert
	org-log-into-drawer t
	org-imenu-depth 3
	org-export-with-sub-superscripts nil
	org-agenda-log-mode-items '(clock)
	org-directory (file-truename "~/git/org")
	org-agenda-files (jf/org-agenda-files
			  :paths jf/data-directories
			  :basenames '("agenda.org"))
	org-default-notes-file (concat
				org-directory
				"/captured-notes.org")
	org-log-done 'time
	org-todo-keywords '((type "TODO(t)"
				  "IN-PROGRESS(i!)"
				  "BLOCKED(b@/!)"
				  "IN-REVIEW(r!)"
				  "|"
				  "DONE(d!)"
				  "DELEGATED(g@/!)"
				  "HOLD(h@/!)"
				  "SENT-FORWARD(s!)"
				  "CANCELLED(c@)")))
  (setq org-capture-templates
	'(("@"
	   "All Todo"
	   entry (file+olp
		  jf/primary-agenda-filename-for-machine
		  "General Todo Items")
	   "* TODO %?\n  %i\n  %a"
	   :empty-lines-before 1)
	  ("b" "Blocker"
	   plain (file+function
		  jf/primary-agenda-filename-for-machine
		  jf/org-mode-agenda-find-blocked-node)
	   "***** BLOCKED %^{Describe the blocker} :blocker:"
	   :immediate-finish t
	   :jump-to-captured t
	   :empty-lines-after 1)
	  ("c" "Contents to Current Clocked Task"
	   plain (clock)
	   "%i%?"
	   :empty-lines 1)
	  ("d" "Day with plain entry"
	   plain (file+olp+datetree jf/primary-agenda-filename-for-machine)
	   "%i"
	   :empty-lines 1
	   :time-prompt t
	   :immediate-finish t)
	  ("m" "Merge Request"
	   plain (file+function
		  jf/primary-agenda-filename-for-machine
		  jf/org-mode-agenda-find-merge-request-node)
	   "***** IN-PROGRESS %^{URL of Merge Request} :mergerequest:"
	   :immediate-finish t
	   :jump-to-captured t
	   :empty-lines-after 1
	   )
	  ;; Needed for the first project of the day; to ensure the datetree is
	  ;; properly generated.
	  ("p" "Project"
	   entry (file+olp+datetree jf/primary-agenda-filename-for-machine)
	   "* %(jf/org-mode-agenda-project-prompt) :projects:\n\n%?"
	   :empty-lines-before 1
	   :immediate-finish t
	   :empty-lines-after 1)
	  ("t" "Task"
	   ;; I tried this as a node, but that created headaches.  Instead I'm
	   ;; making the assumption about project/task depth.
	   plain (file+function
		  jf/primary-agenda-filename-for-machine
		  jf/org-mode-agenda-find-project-node)
	   ;; The five ***** is due to the assumptive depth of the projects and
	   ;; tasks.
	   "***** TODO %^{Describe the task} :tasks:\n\n"
	   :jump-to-captured t
	   :immediate-finish t
	   :clock-in t)))


  (setq org-latex-default-class "jf/article")

  (org-babel-do-load-languages 'org-babel-load-languages
			       (append org-babel-load-languages
				       '((emacs-lisp . t)
					 (shell . t)
					 (plantuml . t)
					 (ruby . t))))
  (add-to-list 'org-structure-template-alist '("m" . "marginnote"))
  (add-to-list 'org-structure-template-alist '("i" . "inline_comments"))
  :init
  (require 'ox)
  ;; I grabbed from the following LaTeX class from
  ;; https://www.reddit.com/r/emacs/comments/3zcr43/nooborgmode_custom_latexpdf_export_custom_style/.
  ;; I’m trash with LaTeX, but like the layout thusfar.
  (add-to-list 'org-latex-classes
	       '("jf/article"
		 "\\documentclass[11pt,a4paper]{article}
	    \\usepackage[utf8]{inputenc}
	    \\usepackage[T1]{fontenc}
	    \\usepackage{fixltx2e}
	    \\usepackage{graphicx}
	    \\usepackage{longtable}
	    \\usepackage{float}
	    \\usepackage{wrapfig}
	    \\usepackage{rotating}
	    \\usepackage[normalem]{ulem}
	    \\usepackage{amsmath}
	    \\usepackage{textcomp}
	    \\usepackage{marvosym}
	    \\usepackage{wasysym}
	    \\usepackage{amssymb}
	    \\usepackage{hyperref}
	    \\usepackage{mathpazo}
	    \\usepackage{xcolor}
	    \\usepackage{enumerate}
	    \\definecolor{bg}{rgb}{0.95,0.95,0.95}
	    \\tolerance=1000
		  [NO-DEFAULT-PACKAGES]
		  [PACKAGES]
		  [EXTRA]

	    \\linespread{1.1}
	    \\hypersetup{pdfborder=0 0 0}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")))

  ;; \\hypersetup{colorlinks=false,pdfborderstyle={/S/U/W 1},pdfborder=0 0 1}"
  ;; Make TAB act as if it were issued from the buffer of the languages's major
  ;; mode.
  :custom (org-src-tab-acts-natively t)
  (org-clock-clocktable-default-properties '(:maxlevel 5 :link t :tags t))
  :bind (:map org-mode-map
	      ("C-c l i" . org-insert-link)
	      ("M-g o" . consult-org-heading))
  :bind (("C-c l s" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 ("C-s-t" . org-toggle-link-display)))

;; From https://oremacs.com/2017/10/04/completion-at-point/
(defun jf/org-completion-symbols ()
  "Look for \"=word=\" and allow completion of things like \"=wo\"."
  (when (looking-back "=[a-zA-Z]+")
    (let (cands)
      (save-match-data
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward "=\\([a-zA-Z]+\\)=" nil t)
	    (cl-pushnew
	     (match-string-no-properties 0) cands :test 'equal))
	  cands))
      (when cands
	(list (match-beginning 0) (match-end 0) cands)))))
(defun jf/org-completion-at-point-functions ()
  "The `completion-at-point-functions' I envision using for `org-mode'."
  (setq-local completion-at-point-functions
	      (list  #'tempel-expand
		     #'jf/org-completion-symbols
		     #'cape-dabbrev
		     #'cape-file
		     #'cape-history)))
(defun jf/org-confirm-babel-evaluate (lang body) nil)
(bind-key "s-8" 'jf/capture-region-contents-with-metadata)
(defun jf/capture-region-contents-with-metadata (start end parg)
  "Write text between START and END to currently clocked `org-mode' entry.

    With PARG kill the content instead."
  (interactive "r\nP")
  (let ((text (jf/region-contents-get-with-metadata start end)))
    (if (car parg)
	(kill-new text)
      (org-capture-string text "c"))))

;; With Heavy inspiration from http://www.howardism.org/Technical/Emacs/capturing-content.html
(defun jf/region-contents-get-with-metadata (start end)
  "Get the text between START and END returning an `org-mode' formatted string."
  (require 'magit)
  (require 'git-link)
  (let* ((file-name (buffer-file-name (current-buffer)))
	 (org-src-mode (replace-regexp-in-string
			"-mode"
			""
			(format "%s" major-mode)))
	 (func-name (which-function))
	 (type (cond
		((eq major-mode 'nov-mode) "QUOTE")
		((derived-mode-p 'prog-mode) "SRC")
		(t "SRC" "EXAMPLE")))
	 (code-snippet (buffer-substring-no-properties start end))
	 (file-base (if file-name
			(file-name-nondirectory file-name)
		      (format "%s" (current-buffer))))
	 (line-number (line-number-at-pos (region-beginning)))
	 (remote-link (when (magit-list-remotes)
			(progn
			  (call-interactively 'git-link)
			  (car kill-ring)))))
    (format (concat
	     (format "\n- Local File :: [[file:%s::%s]]" file-name line-number)
	     (when func-name (format "\n- Function Name :: =%s=" func-name))
	     (when (and remote-link file-name)
	       (format "\n- Remote URL :: [[%s]]" remote-link))
	     "\n\n#+BEGIN_%s %s"
	     "\n%s"
	     "\n#+END_%s\n")
	    type
	    org-src-mode
	    code-snippet
	    type)))

;;; Additionally Functionality for Org Mode
;; Cribbed from https://xenodium.com/emacs-dwim-do-what-i-mean/
(defun jf/org-insert-link-dwim (parg &rest args)
  "Like `org-insert-link' but with personal dwim preferences.

  With PARG, skip personal dwim preferences."
  (interactive "P")
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
	 (clipboard-url (when (string-match-p "^http" (current-kill 0))
			  (current-kill 0)))
	 (region-content (when (region-active-p)
			   (buffer-substring-no-properties (region-beginning)
							   (region-end)))))
    (cond
     ((car parg)
      (call-interactively 'org-insert-link nil nil))
     ((and region-content clipboard-url (not point-in-link))
      (delete-region (region-beginning) (region-end))
      (insert (org-make-link-string clipboard-url region-content)))
     ((and clipboard-url (not point-in-link))
      (insert (org-make-link-string
	       clipboard-url
	       (read-string "Title: "
			    (with-current-buffer
				(url-retrieve-synchronously clipboard-url)
			      (dom-text (car
					 (dom-by-tag (libxml-parse-html-region
						      (point-min)
						      (point-max))
						     'title))))))))
     (t
      (call-interactively 'org-insert-link)))))

;;; Org Mode time tracking and task tracking adjustments

;; I work on several different projects each day; helping folks get unstuck.  I
;; also need to track and record my time.
(bind-key "C-j" 'jf/jump-to-agenda-or-mark)
(cl-defun jf/jump-to-agenda-or-mark (prefix-arg)
  "Jump to and from current agenda item to mark.

  With PREFIX_ARG go to beginning of today's headline."
  (interactive "P")
  (require 'org-capture)
  (if (car prefix-arg)
      ;; Jump to where we would put a project were we to capture it.
      (org-capture-goto-target "p")
    (if (string= (buffer-file-name) (file-truename
				     jf/primary-agenda-filename-for-machine))
	(call-interactively #'consult-global-mark)
      (progn
	(call-interactively #'set-mark-command)
	(if (when (and (fboundp 'org-clocking-p) (org-clocking-p)) t)
	    (org-clock-goto)
	  ;; Jump to where we would put a project were we to capture it.
	  (org-capture-goto-target "p")))))
  (require 'pulsar)
  (pulsar-pulse-line))
(defun jf/org-mode-agenda-project-prompt ()
  "Prompt for project based on existing projects in agenda file.

    Note: I tried this as interactive, but the capture templates
    insist that it should not be interactive."
  (completing-read
   "Project: "
   (sort
    (-distinct
     (org-map-entries
      (lambda ()
	(org-element-property :raw-value (org-element-at-point)))
      "+LEVEL=4+projects" 'agenda)
     ) #'string<)))

;; When I jump to a new task for the day, I want to position that task within
;; the prompted project.  Inspiration from
;; https://gist.github.com/webbj74/0ab881ed0ce61153a82e.
(cl-defun jf/org-mode-agenda-find-project-node
    (&key
     (tag "projects")
     (project (jf/org-mode-agenda-project-prompt))
     ;; The `file+olp+datetree` directive creates a headline like “2022-09-03 Saturday”.
     (within_headline (format-time-string "%Y-%m-%d %A")))
  "Find and position the cursor at the end of
    the given PROJECT WITHIN_HEADLINE."
  ;; We need to be using the right agenda file.
  (with-current-buffer (find-file-noselect
			jf/primary-agenda-filename-for-machine)
    (let ((existing-position (org-element-map
				 (org-element-parse-buffer)
				 'headline
			       ;; Finds the end position of:
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
				      (org-element-property :end hl)))
			       nil t)))
      (if existing-position
	  ;; Go to the existing position for this project
	  (goto-char existing-position)
	(progn
	  ;; Go to the end of the file and append the project to the end
	  (end-of-buffer)
	  ;; Ensure we have a headline for the given day
	  (unless (org-element-map
		      (org-element-parse-buffer)
		      'headline
		    (lambda (hl)
		      (string= within_headline
			       (plist-get
				;; I want the raw title, no styling nor tags
				(cadr (car (org-element-lineage hl)))
				:raw-value))))
	    (insert (concat "\n\n*** "within_headline)))
	  (insert (concat "\n\n**** " project " :" tag ":\n\n")))))))

(cl-defun jf/org-mode-agenda-find-blocked-node ()
  "Add a blocker node to today."
  (jf/org-mode-agenda-find-project-node :tag "blockers"
					:project (concat
						  "Blockers for "
						  (format-time-string
						   "%Y-%m-%d"))))

(cl-defun jf/org-mode-agenda-find-merge-request-node ()
  "Add a mergerequest node to today."
  (jf/org-mode-agenda-find-project-node :tag "mergerequests"
					:project (concat "Merge Requests for "
							 (format-time-string
							  "%Y-%m-%d"))))

;; Takes my notes for the day and formats them for a summary report.
(defun jf/org-mode-agenda-to-stand-up-summary (parg)
  "Copy to the kill ring the day's time-tracked summary.

When given PREFIX-ARG, prompt for the day of interest.

NOTE: This follows the convention that projects are on headline 4 and
tasks within projects are headline 5."
  (interactive "P")
  (with-current-buffer (find-file-noselect
			jf/primary-agenda-filename-for-machine)
    (save-excursion
      (let ((within_headline
	     ;; Use the CCYY-MM-DD Dayname format and prompt for a date if
	     ;; PREFIX-ARG given.
	     (format-time-string "%Y-%m-%d %A"
				 (when (car parg)
				   (org-read-date nil t nil "Pick a day:" )))))
	(kill-new
	 (concat "*Summary of " within_headline "*\n\n"
		 (s-trim
		  (s-join
		   "\n"
		   (org-element-map
		       (org-element-parse-buffer)
		       'headline
		     (lambda (hl)
		       (when (member
			      within_headline
			      (mapcar
			       (lambda (ancestor)
				 (plist-get (cadr ancestor) :raw-value))
			       (org-element-lineage hl)))
			 (pcase (org-element-property :level hl)
			   (4 (concat "\n" (plist-get (cadr hl) :raw-value)))
			   (5 (concat "- " (plist-get (cadr hl) :raw-value)))
			   (_ nil)))))))))
	(jf/create-scratch-buffer)
	(yank)))))

;; I’m responsible for tracking my work time.  I want a way to quickly see what
;; that is for the current week.
;;
;; A utility function providing an overrview
(cl-defun jf/org-mode-weekly-report ()
  "Jump to my weekly time tracker.

Useful for providing me with an overview of my total tracked time
for the week."
  (interactive)
  (find-file jf/primary-agenda-filename-for-machine)
  (require 'pulsar)
  (pulsar-pulse-line)
  (org-clock-report 4))

;; Another task at end of month is to transcribing my agenda’s timesheet to
;; entries in our time tracking software.  From the day’s project link in the
;; =org-clock-report=, I want to copy the headlines of each of the tasks.  I
;; fill out my time sheets one day at a time.
(defun jf/org-mode-time-entry-for-project-and-day ()
  "Function to help report time for Scientist.com

Assumes that I'm on a :projects: headline.

- Sum the hours (in decimal form) for the tasks.
- Create a list of the tasks.
- Write this information to the message buffer.
- Then move to the next heading level.
"
  (interactive)
  (let* ((project (plist-get (cadr (org-element-at-point)) :raw-value))
	 (tasks (s-join "\n"
			(org-with-wide-buffer
			 (when (org-goto-first-child)
			   (cl-loop collect (concat "- "
						    (org-no-properties
						     (org-get-heading t t)))
				    while (outline-get-next-sibling))))))
	 (hours (/ (org-clock-sum-current-item) 60.0))
	 (output (format "Tasks:\n%s\nProject: %s\nHours: %s\n"
			 tasks
			 project
			 hours
			 )))
    (kill-new tasks)
    (message output)))

;;; Extra Org Mode Export Function(s)

;; Org Mode has built-in capabilities for exporting to HTML (and other
;; languages).  The following function does just a bit more.  It converts the
;; org region to HTML and sends it to the clipboard as an RTF datatype.
;;
;; Why is that nice?  As an RTF datatype, the paste receiver better handles the
;; HTML (e.g., I can more readily paste into an Email and it pastes as
;; expected).
;;
;; See
;; https://kitchingroup.cheme.cmu.edu/blog/2016/06/16/Copy-formatted-org-mode-text-from-Emacs-to-other-applications/
;; for more details.  One addition I made was to add the ~-inputencoding UTF-8~
;; switch.  Without it, I would end up with some weird characters from odd
;; smartquote handling.
(require 'jf-formatting)

;; (define-key org-mode-map (kbd "~") #'org-insert-backtick)
;; (defun org-insert-backtick ()
;;   "Insert a backtick using `org-self-insert-command'."
;;   (interactive)
;;   (setq last-command-event ?`)
;;   (call-interactively #'org-self-insert-command))

(defvar-local org-insert-tilde-language nil
  "Default language name in the current Org file.
If nil, `org-insert-tilde' after 2 tildes inserts an \"example\"
block.  If a string, it inserts a \"src\" block with the given
language name.")

;; (define-key org-mode-map (kbd "`") #'org-insert-tilde)
;; (defun org-insert-tilde ()
;;   "Insert a tilde using `org-self-insert-command'."
;;   (interactive)
;;   (if (string= (buffer-substring-no-properties (- (point) 3) (point))
;; 	       "\n~~")
;;       (progn (delete-char -2)
;; 	     (if org-insert-tilde-language
;; 		 (insert (format "#+begin_src %s\n#+end_src"
;; 				 org-insert-tilde-language))
;; 	       (insert "#+begin_example\n#+end_example"))
;; 	     (forward-line -1)
;; 	     (if (string= org-insert-tilde-language "")
;; 		 (move-end-of-line nil)
;; 	       (org-edit-special)))
;;     (setq last-command-event ?~)
;;     (call-interactively #'org-self-insert-command)))

;; In
;; https://takeonrules.com/2022/02/26/note-taking-with-org-roam-and-transclusion/,
;; I wrote about ~org-transclusion~.  The quick version, ~org-transclusion~
;; allows you to include text from one file into another.  This allows for
;; document composition.
(use-package org-transclusion
  :straight t
  :init (setq org-transclusion-exclude-elements '(property-drawer keyword)))


;; I love the work of Daniel Mendler (https://github.com/minad).
;; This package gives a bit of visual chrome to org files.
(use-package org-modern
  :straight (:host github :repo "minad/org-modern")
  :custom (org-modern-star '("◉" "○" "◈" "◇" "•"))
  :hook (org-mode . org-modern-mode))


;;; Org Export and Composition Functionality

(setq org-export-global-macros (list))
(use-package ox
  :straight (ox :type built-in)
  :config
  (add-to-list 'org-export-global-macros
	       '("kbd" . "@@html:<kbd>@@$1@@html:</kbd>@@"))

  (add-to-list 'org-export-global-macros
	       '("cite" . "@@html:<cite>@@$1@@html:</cite>@@"))

  (add-to-list 'org-export-global-macros
	       '("dfn" . "@@html:<dfn>@@$1@@html:</dfn>@@"))

  (add-to-list 'org-export-global-macros
	       '("scene-date" . "#+begin_marginnote\nThe scene occurs on @@html:<span class=\"time\">@@$1@@html:</span>@@.\n#+end_marginnote")))

(add-to-list 'org-export-global-macros
	     '("mention" . "@@hugo:{{< glossary key=\"@@$1@@hugo:\" >}}@@"))
(add-to-list 'org-export-global-macros
	     '("abbr" . "@@hugo:{{< glossary key=\"@@$1@@hugo:\" abbr=\"t\" >}}@@"))
(add-to-list 'org-export-global-macros
	     '("abbr-plural" . "@@hugo:{{< glossary key=\"@@$1@@hugo:\" abbr=\"t\" plural=\"t\" >}}@@"))
(add-to-list 'org-export-global-macros
	     '("i" . "@@html:<i class=\"dfn\">@@$1@@html:</i>@@"))

(add-to-list 'org-export-global-macros
	     '("linkToSeries" . "@@hugo:{{< linkToSeries \"@@$1@@hugo:\" >}}@@"))'

(defun jf/org-link-delete-link ()
  "Remove the link part of an org-mode link at point and keep
only the description"
  (interactive)
  (let ((elem (org-element-context)))
    (when (eq (car elem) 'link)
      (let* ((content-begin (org-element-property :contents-begin elem))
	     (content-end  (org-element-property :contents-end elem))
	     (link-begin (org-element-property :begin elem))
	     (link-end (org-element-property :end elem)))
	(when (and content-begin content-end)
	  (let ((content (buffer-substring-no-properties
			  content-begin content-end)))
	    (delete-region link-begin link-end)
	    (insert (concat content " "))))))))

(defun jf/force-org-rebuild-cache (prefix-arg)
  "Call functions to rebuild the applicable `org-mode' and `org-roam' cache(s).

When given PREFIX_ARG, clear the org-roam database (via
 `org-roam-db-clear-all') then sync.  This will slow down the sync."
  (interactive "P")
  (org-id-update-id-locations)
  (when (fboundp 'org-roam-db-clear-all)
    (progn
      (when (car prefix-arg) (org-roam-db-clear-all))
      (org-roam-db-sync)
      (org-roam-update-org-id-locations))))

(cl-defun jf/org-agenda-carry-forward-task ()
  "Carry an `org-mode' task node forward."
  (interactive)
  (save-excursion
    (let* ((day-project-task
	    (jf/org-agenda-get-day-and-project-and-task-at-point))
           (from-project (plist-get day-project-task :project))
           (from-task (plist-get day-project-task :task)))
      ;; Narrowing the region to perform quicker queries on the element
      (narrow-to-region (org-element-property :begin from-task)
                        (org-element-property :end from-task))

      ;; Grab each section for the from-task and convert that into text.
      ;;
      ;; Yes we have the from-task, however, we haven't parsed that entity.
      ;; Without parsing that element, the `org-element-contents' returns nil.
      (let ((content (s-join "\n" (org-element-map (org-element-parse-buffer)
				      'section
                                    (lambda (section)
                                      (mapconcat
                                       (lambda (element)
                                         (pcase (org-element-type element)
                                           ;; I want to skip my time entries
                                           ('drawer nil)
                                           (_ (buffer-substring-no-properties
                                               (org-element-property
						:begin element)
                                               (org-element-property
						:end element)))))
                                       (org-element-contents section)
                                       "\n"))))))
	(widen)
        (org-capture-string (format "%s %s :%s:\n\n%s %s %s :%s:\n%s"
                                    (s-repeat (org-element-property :level from-project) "*")
                                    (org-element-property :raw-value from-project)
                                    (s-join ":" (org-element-property :tags from-project))
                                    (s-repeat (org-element-property :level from-task) "*")
                                    (org-element-property :todo-keyword from-task)
                                    (org-element-property :raw-value from-task)
                                    (s-join ":" (org-element-property :tags from-task))
                                    content)
                            "d"))
      ;; Now that we've added the content, let's tidy up the from-task.
      (goto-char (org-element-property :contents-begin from-task))
      ;; Prompt for the todo state of the original task.
      (call-interactively 'org-todo))))

(defun jf/org-agenda-get-day-and-project-and-task-at-point ()
  "Return a plist of :day, :project, and :task for element at point."
  (let* ((task (jf/org-agenda-task-at-point))
	 (project (progn
		    (org-up-heading-safe)
		    (org-element-at-point)))
	 (day (progn
		(org-up-heading-safe)
		(org-element-at-point))))
    (list :project project :task task :day day)))

(defun jf/org-agenda-task-at-point ()
  "Find the `org-mode' task at point."
  (let ((element (org-element-at-point)))
    (if (eq 'headline (org-element-type element))
        (pcase (org-element-property :level element)
          (1 (error "Selected element is a year"))
          (2 (error "Selected element is a month"))
          (3 (error "Selected element is a day"))
          (4 (error "Selected element is a project"))
          (5 (progn (message "Found %s" element) element))
          (_ (progn (org-up-heading-safe) (jf/org-agenda-task-at-point))))
      (progn
        (org-back-to-heading)
        (jf/org-agenda-task-at-point)))))

(defun jf/org-agenda-task-at-point ()
  "Find the `org-mode' task at point."
  (let ((element (org-element-at-point)))
    (if (eq 'headline (org-element-type element))
        (pcase (org-element-property :level element)
          (1 (error "Selected element is a year"))
          (2 (error "Selected element is a month"))
          (3 (error "Selected element is a day"))
          (4 (error "Selected element is a project"))
          (5 (progn (message "Found %s" element) element))
          (_ (progn (org-up-heading-safe) (jf/org-agenda-task-at-point))))
      (progn
        (org-back-to-heading)
        (jf/org-agenda-task-at-point)))))

(use-package htmlize
  :straight t
  :bind ("C-M-s-c" . jf/formatted-copy-org-to-html)
  :config
  ;; The following functions build on both org and the htmlize package.  I
  ;; define them as part of the config because without the package these won't
  ;; work.
  ;;
  ;; For this to work, I needed to permit my \"~/bin/emacsclient\" in the
  ;; Security & Privacy > Accessibility system preference.
  ;;
  ;; http://mbork.pl/2021-05-02_Org-mode_to_Markdown_via_the_clipboard
  (defun jf/org-copy-region-as-markdown ()
    "Copy the region (in Org) to the system clipboard as Markdown."
    (interactive)
    (require 'ox)
    (if (use-region-p)
	(let* ((region
		(buffer-substring-no-properties
		 (region-beginning)
		 (region-end)))
	       (markdown
		(org-export-string-as region 'md t '(:with-toc nil))))
	  (gui-set-selection 'CLIPBOARD markdown))))

  ;; I have found that Slack resists posting rich content, so I often need to
  ;; open up TextEdit, paste into an empty file, copy the contents, and then
  ;; paste into Slack.
  (defun jf/formatted-copy-org-to-html (prefix)
    "Export region to HTML, and copy it to the clipboard.

When given the PREFIX arg, paste the content into TextEdit (for future copy)."
    (interactive "P")
    (save-window-excursion
      (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
	     (html (with-current-buffer buf (buffer-string))))
	(with-current-buffer buf
	  (shell-command-on-region
	   (point-min)
	   (point-max)
	   "textutil -inputencoding UTF-8 -stdout -stdin -format html -convert rtf | pbcopy"))
	(kill-buffer buf)
	;; Paste into TextEdit
	(when (car prefix)
	  (ns-do-applescript
	   (concat
	    "tell application \"TextEdit\"\n"
	    "\tactivate\n"
	    "\tset myrtf to the clipboard as «class RTF »\n"
	    "\tset mydoc to make new document\n"
	    "\tset text of mydoc to myrtf\n"
	    "end tell")))
	))))

;; https://www.reddit.com/r/emacs/comments/yjobc2/what_method_do_you_use_to_create_internal_links/
(defun jf/org-parse-headline (x)
  (plist-get (cadr x) :raw-value))

(defun jf/org-get-headlines ()
  (org-element-map (org-element-parse-buffer)
      'headline #'jf/org-parse-headline))

(defun jf/org-link-to-headline ()
  "Insert an internal link to a headline."
  (interactive)
  (let* ((headlines (jf/org-get-headlines))
	 (choice (completing-read "Headings: " headlines nil t))
	 (desc (read-string "Description: " choice)))
    (org-insert-link buffer-file-name (concat "*" choice) desc)))

(provide 'jf-org-mode)
;;; jf-org-mode.el ends here
