;;; jf-org-mode.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;;; Code
;;; Pre-amble to prepare for `org-mode'

;; I maintain a list of data directories, each might have “relevant to
;; org-mode” files.  The `jf/org-agenda-files' reads the file system to gather
;; sources for `org-mode' agenda.
(defun is-work-machine? ()
  "Am I working on my machine"
  (file-exists-p (file-truename "~/git/org/scientist/.keep")))

(defvar jf/primary-agenda-filename-for-machine
  (if (is-work-machine?)
      "~/git/org/scientist/agenda.org"
    "~/git/org/agenda.org"))

(defconst jf/data-directories
  (list
   jf/tor-home-directory
   "~/git/org/scientist/"
   "~/git/dotzshrc/"
   "~/git/dotemacs/"
   "~/git/org/")
  "Relevant data directories for my day to day work.")

(cl-defun jf/org-agenda-files (&key
			       (paths jf/data-directories)
			       (basenames '("agenda.org")))
  "Return the list of filenames where BASENAMES exists in PATHS."
  ;; I want to include my configuration file in my agenda querying.
  (setq returning-list '("~/git/dotemacs/emacs.d/configuration.org"))
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
  :bind ("C-j" . jf/jump-to-agenda-or-mark)
  :bind (:map org-mode-map ("C-j" . jf/jump-to-agenda-or-mark))
  :custom (org-use-speed-commands t)
  (org-time-stamp-rounding-minutes '(0 15))
  :config
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
		(list (cape-super-capf
		       #'tempel-expand
		       #'jf/org-completion-symbols
		       #'cape-dabbrev
		       #'cape-file
		       #'cape-dict
		       #'cape-ispell
		       #'cape-keyword
		       #'cape-history
		       #'elisp-completion-at-point))))
  (defun jf/org-confirm-babel-evaluate (lang body) nil)
  (setq org-confirm-babel-evaluate #'jf/org-confirm-babel-evaluate
	;; I'd prefer to use the executable, but that doe not appear to be the
	;; implementation of org-babel.
	org-plantuml-jar-path (concat (string-trim (shell-command-to-string "brew-path plantuml")) "/libexec/plantuml.jar")
	org-insert-heading-respect-content t
	org-catch-invisible-edits 'show-and-error
	org-use-fast-todo-selection 'expert
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
	org-todo-keywords '((type "TODO(t)" "IN-PROGRESS(i!)" "BLOCKED(b@/!)" "IN-REVIEW(r!)" "|" "DONE(d!)" "DELEGATED(g@/!)" "CANCELLED(c@)"))
	)
  (setq org-capture-templates
	'(("@"
	   "All Todo"
	   entry (file+olp jf/primary-agenda-filename-for-machine "General Todo Items")
	   "* TODO %?\n  %i\n  %a"
	   :empty-lines-before 1)
	  ("b" "Blocker"
	   plain (file+function jf/primary-agenda-filename-for-machine jf/org-mode-agenda-find-blocked-node)
	   "***** BLOCKED %^{Describe the blocker} :blocker:"
	   :immediate-finish t
	   :jump-to-captured t
	   :empty-lines-after 1)
	  ("c" "Contents to Current Clocked Task"
	   plain (clock)
	   "%i%?"
	   :empty-lines 1)
	  ("m" "Merge Request"
	   plain (file+function jf/primary-agenda-filename-for-machine jf/org-mode-agenda-find-merge-request-node)
	   "***** IN-PROGRESS %^{URL of Merge Request} :mergerequest:"
	   :immediate-finish t
	   :jump-to-captured t
	   :empty-lines-after 1
	   )
	  ;; Needed for the first project of the day; to ensure the datetree is
	  ;; properly generated.
	  ("p" "Project"
	   entry (file+olp+datetree jf/primary-agenda-filename-for-machine)
	   "* %(jf/org-mode-agenda-project-prompt) :project:\n\n%?"
	   :empty-lines-before 1
	   :immediate-finish t
	   :empty-lines-after 1)
	  ("t" "Task"
	   ;; I tried this as a node, but that created headaches.  Instead I'm
	   ;; making the assumption about project/task depth.
	   plain (file+function jf/primary-agenda-filename-for-machine jf/org-mode-agenda-find-project-node)
	   ;; The five ***** is due to the assumptive depth of the projects and tasks.
	   "***** TODO %^{Describe the task} :task:\n\n"
	   :jump-to-captured t
	   :immediate-finish t
	   :clock-in t)))

  (bind-key "s-8" 'jf/capture-region-contents-with-metadata)
  (defun jf/capture-region-contents-with-metadata (start end parg)
    "Write selected text between START and END to currently clocked `org-mode' entry.

    With PARG kill the content instead."
    (interactive "r\nP")
    (let ((text (jf/region-contents-get-with-metadata start end)))
      (if (car parg)
	  (kill-new text)
	(org-capture-string (concat "-----\n" text) "c"))))

  ;; With Heavy inspiration from http://www.howardism.org/Technical/Emacs/capturing-content.html
  (defun jf/region-contents-get-with-metadata (start end)
    "Get the region contents between START and END and return an `org-mode' formatted string."
    (require 'magit)
    (require 'git-link)
    (let* ((file-name (buffer-file-name (current-buffer)))
	   (org-src-mode (replace-regexp-in-string
			  "-mode"
			  ""
			  (format "%s" major-mode)))
	   (func-name (which-function))
	   (type (if (derived-mode-p 'prog-mode) "SRC" "EXAMPLE"))
	   (code-snippet (buffer-substring-no-properties start end))
	   (file-base (file-name-nondirectory file-name))
	   (line-number (line-number-at-pos (region-beginning)))
	   (remote-link (when (magit-list-remotes)
			  (progn
			    (call-interactively 'git-link)
			    (car kill-ring))))
	   (local-link (if (null func-name)
			   (format "From [[file:%s::%s][%s]]:"
				   file-name
				   line-number
				   file-base)
			 (format "From ~%s~ (in [[file:%s::%s][%s]]):"
				 func-name
				 file-name
				 line-number
				 file-base))))
      (format (concat "\n#+BEGIN_%s %s"
		      "\n%s"
		      "\n#+END_%s\n"
		      "\n- Local :: %s"
		      (when remote-link
			(format "\n- Remote :: [[%s][%s]]" remote-link (or func-name file-name))))
	      type
	      org-src-mode
	      code-snippet
	      type
	      local-link)))

  (setq org-latex-default-class "jf/article")

  (org-babel-do-load-languages 'org-babel-load-languages
			       (append org-babel-load-languages
				       '((emacs-lisp . t)
					 (shell . t)
					 (plantuml . t)
					 (ruby . t))))
  :init
  (add-to-list 'org-structure-template-alist '("M" . "marginnote"))
  (add-to-list 'org-structure-template-alist '("S" . "sidenote"))
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
	      ("C-c l i" . jf/org-insert-link-dwim)
	      ("s-2" . consult-org-heading))
  :bind (("C-c l s" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 ("C-s-t" . org-toggle-link-display)))

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
			    (with-current-buffer (url-retrieve-synchronously clipboard-url)
			      (dom-text (car
					 (dom-by-tag (libxml-parse-html-region
						      (point-min)
						      (point-max))
						     'title))))))))
     (t
      (call-interactively 'org-insert-link)))))

;;; Org Mode time tracking and task tracking adjustments

;; I work on several different projects each day; helping folks get unstuck.  I also need to track and record my time.
(bind-key "C-j" 'jf/jump-to-agenda-or-mark)
(cl-defun jf/jump-to-agenda-or-mark (prefix-arg)
  "Jump to and from current agenda item to mark.

  With PREFIX_ARG go to beginning of today's headline."
  (interactive "P")
  (require 'org-capture)
  (if (car prefix-arg)
      ;; Jump to where we would put a project were we to capture it.
      (org-capture-goto-target "p")
    (if (string= (buffer-file-name) (file-truename jf/primary-agenda-filename-for-machine))
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
	(org-element-property :title (org-element-at-point)))
      "+LEVEL=4+project" 'agenda)
     ) #'string<)))

;; When I jump to a new task for the day, I want to position that task within
;; the prompted project.  Inspiration from
;; https://gist.github.com/webbj74/0ab881ed0ce61153a82e.
(cl-defun jf/org-mode-agenda-find-project-node (&key
						(tag "project")
						(project (jf/org-mode-agenda-project-prompt))
						;; The `file+olp+datetree` directive creates a headline like “2022-09-03 Saturday”.
						(within_headline (format-time-string "%Y-%m-%d %A")))
  "Find and position the cursor at the end of
    the given PROJECT WITHIN_HEADLINE."
  ;; We need to be using the right agenda file.
  (with-current-buffer (find-file-noselect jf/primary-agenda-filename-for-machine)
    (let ((existing-position (org-element-map
				 (org-element-parse-buffer)
				 'headline
			       ;; Finds the end position of:
			       ;; - a level 4 headline
			       ;; - that is tagged as a :project:
			       ;; - is titled as the given project
			       ;; - and is within the given headline
			       (lambda (hl)
				 (and (=(org-element-property :level hl) 4)
				      ;; I can't use the :title attribute as it is a
				      ;; more complicated structure; this gets me
				      ;; the raw string.
				      (string= project (plist-get (cadr hl) :raw-value))
				      (member tag (org-element-property :tags hl))
				      ;; The element must have an ancestor with a headline of today
				      (string= within_headline
					       (plist-get
						;; I want the raw title, no styling nor tags
						(cadr (car (org-element-lineage hl))) :raw-value))
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
				(cadr (car (org-element-lineage hl))) :raw-value))))
	    (insert (concat "\n\n*** "within_headline)))
	  (insert (concat "\n\n**** " project " :" tag ":\n\n")))))))

(cl-defun jf/org-mode-agenda-find-blocked-node ()
  "Add a blocker node to today."
  (jf/org-mode-agenda-find-project-node :tag "blockers"
					:project (concat "Blockers for " (format-time-string "%Y-%m-%d"))))

(cl-defun jf/org-mode-agenda-find-merge-request-node ()
  "Add a mergerequest node to today."
  (jf/org-mode-agenda-find-project-node :tag "mergerequests"
					:project (concat "Merge Requests for " (format-time-string "%Y-%m-%d"))))

;; Takes my notes for the day and formats them for a summary report.
(defun jf/org-mode-agenda-to-stand-up-summary (parg)
  "Copy to the kill ring the day's time-tracked summary.

When given PREFIX-ARG, prompt for the day of interest.

NOTE: This follows the convention that projects are on headline 4 and
tasks within projects are headline 5."
  (interactive "P")
  (with-current-buffer (find-file-noselect jf/primary-agenda-filename-for-machine)
    (save-excursion
      (let ((within_headline
	     ;; Use the CCYY-MM-DD Dayname format and prompt for a date if PREFIX-ARG given.
	     (format-time-string "%Y-%m-%d %A"
				 (when (car parg) (org-read-date nil t nil "Pick a day:" )))))
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
			       (lambda (ancestor) (plist-get (cadr ancestor) :raw-value))
			       (org-element-lineage hl)))
			 (if (=(org-element-property :level hl) 4)
			     (concat "\n" (plist-get (cadr hl) :raw-value))
			   (concat "- " (plist-get (cadr hl) :raw-value)))))
		     )))))
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
(defun jf/org-mode-tasks-for-project-and-day ()
  "Function to help report time for Scientist.com

Assumes that I'm on a :project: headline.

- Sum the hours (in decimal form) for the tasks.
- Create a list of the tasks.
- Write this information to the message buffer.
- Then move to the next heading level.
"
  (interactive)
  (let* ((project (plist-get (cadr (org-element-at-point)) :raw-value))
	 (tasks (s-join "\n" (org-with-wide-buffer
			      (when (org-goto-first-child)
				(cl-loop collect (concat "- " (org-no-properties (org-get-heading t t)))
					 while (outline-get-next-sibling))))))
	 (hours (/ (org-clock-sum-current-item) 60.0))
	 (output (format "Project: %s\nHours: %s\nTasks:\n%s" project hours tasks)))
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
(use-package htmlize
  :straight t
  :bind ("C-M-s-c" . jf/formatted-copy-org-to-html)
  :config
  ;; The following functions build on both org and the htmlize package.  I
  ;; define them as part of the config because without the package these won't
  ;; work.
  ;;
  ;; For this to work, I needed to permit my \"~/bin/emacsclient\" in the Security
  ;; & Privacy > Accessibility system preference.
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

  ;; I have found that Slack resists posting rich content, so I often need to open up TextEdit, paste into an empty file, copy the contents, and then paste into Slack.
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
	)))
  )

(define-key org-mode-map (kbd "~") #'org-insert-backtick)
(defun org-insert-backtick ()
  "Insert a backtick using `org-self-insert-command'."
  (interactive)
  (setq last-command-event ?`)
  (call-interactively #'org-self-insert-command))

(defvar-local org-insert-tilde-language nil
  "Default language name in the current Org file.
If nil, `org-insert-tilde' after 2 tildes inserts an \"example\"
block.  If a string, it inserts a \"src\" block with the given
language name.")

(define-key org-mode-map (kbd "`") #'org-insert-tilde)
(defun org-insert-tilde ()
  "Insert a tilde using `org-self-insert-command'."
  (interactive)
  (if (string= (buffer-substring-no-properties (- (point) 3) (point))
	       "\n~~")
      (progn (delete-char -2)
	     (if org-insert-tilde-language
		 (insert (format "#+begin_src %s\n#+end_src"
				 org-insert-tilde-language))
	       (insert "#+begin_example\n#+end_example"))
	     (forward-line -1)
	     (if (string= org-insert-tilde-language "")
		 (move-end-of-line nil)
	       (org-edit-special)))
    (setq last-command-event ?~)
    (call-interactively #'org-self-insert-command)))

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
	       '("date" . "@@html:<time datetime=\"$1\">@@$2@@html:</time>@@"))

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
	     '("linkToGame" . "@@hugo:{{< glossary key=\"@@$1@@hugo:\" abbr=\"t\" >}}@@"))
(add-to-list 'org-export-global-macros
	     '("i" . "@@html:<i class=\"dfn\">@@$1@@html:</i>@@"))

(add-to-list 'org-export-global-macros
	     '("sidenote" . "@@hugo:{{< sidenote >}}@@$1@@hugo:{{< /sidenote >}}@@"))

(add-to-list 'org-export-global-macros
	     '("linkToSeries" . "@@hugo:{{< linkToSeries \"@@$1@@hugo:\" >}}@@"))'

(cl-defun jf/org-macro-value-list (macro-name &key (dir org-directory))
  "List the unique inner text of all uses of MACRO-NAME in given DIR."
  (s-split
   "\n"
   (s-trim
    (shell-command-to-string
     (concat
      "rg \"\\{\\{\\{"
      macro-name
      "\\((.+?)\\)\\}\\}\\}"
      "\" --only-matching --no-filename -r '$1' "
      dir
      " | sort | uniq")))))


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
	      (let ((content (buffer-substring-no-properties content-begin content-end)))
		(delete-region link-begin link-end)
		(insert (concat content " "))))))))

(defun jf/force-org-rebuild-cache (prefix-arg)
  "Call some functions to rebuild the applicable `org-mode' and `org-roam' cache(s).

When given PREFIX_ARG, clear the org-roam database (via `org-roam-db-clear-all') then sync.  This will slow down the sync."
  (interactive "P")
  (org-id-update-id-locations)
  (when (fboundp 'org-roam-db-clear-all)
    (progn
      (when (car prefix-arg) (org-roam-db-clear-all))
      (org-roam-db-sync)
      (org-roam-update-org-id-locations))))

;; I want a function that I can “send forward a task”

;; Throughout my work week, I’m writing TODO items.  I find myself carrying
;; them forward from a previous day.  What I would like to do is from a
;; =:task:= headline:
;;
;; - Using =org-id-get-create= drop an identifier on the current task; we’ll save that.
;; - Create a new task for today and in the source task’s project; same headline, status, and tags.
;; - Using =org-id-get-create=, create and save the new identifier.
;; - On the new task:
;;   - Insert a link to the previous task with text “Carried forward from <DATE>” where date is the date of the previous task.
;;   - Insert the previous task’s content.
;;   - Save the buffer
;;   - Start a clock
;; - On the previous task:
;;   - Remove the status tag
;;   - Replace the now copied content with “Carried forward to <DATE>” where date is the date of the new task.
;; (cl-defun jf/org-carry-forward-task (from-task to-headline)
;;   "Carry the FROM-TASK forward to the TO-HEADLINE.

;; The FROM-TASK is an `org-entry'; the TO-HEADLINE is a string that
;; matches the 3rd-level date headline (e.g. \"CCYY-MM-DD
;; DayOfWeek\")."
;;   (interactive (list (jf/org-task-at-point) (jf/org-prompt-for-headline)))
;;   (jf/org-extract-new-entry from-task)
;;   (message "%s" from-task))

;; (defun jf/org-extract-new-entry (from-task)
;;   "The entry is of the form:

;; - Headline
;; - Metadata
;; - Content

;; The metadata is property drawers and such."
;;   (let ((text (buffer-substring-no-properties
;; 	       (org-element-property :begin from-task)
;; 	       (org-element-property :end from-task))))
;;     (org-id-get-create)
;;     (org-todo "")
;;     text))

;; (defun jf/org-task-at-point ()
;;   "Retrieve the task at point, assigning an :ID:."
;;   (save-excursion
;;     (org-back-to-heading)
;;     (org-element-at-point)))

;; (defun jf/org-prompt-for-headline ()
;;   "Prompt for headline"
;;   (format-time-string "%Y-%m-%d %A"))

(provide 'jf-org-mode)
;;; jf-org-mode.el ends here