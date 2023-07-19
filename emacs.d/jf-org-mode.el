;;; jf-org-mode.el --- Org-Mode configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;; Pre-amble to prepare for `org-mode'

;;; Code:
(require 'cl-lib)

;; I maintain a list of data directories, each might have “relevant to
;; org-mode” files.  The `jf/org-agenda-files' reads the file system to gather
;; sources for `org-mode' agenda.
(defun jf/is-work-machine? ()
  "Am I working on my company machine machine."
  (string= (getenv "USER") "jeremy"))

(defvar jf/org-mode/capture/filename
  "~/git/org/denote/melange/20230210T184422--example-code__programming.org"
  "The file where I'm capturing content.

By default this is my example code project.")

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

(defun jf/org-capf ()
  "The `completion-at-point-functions' I envision using for `org-mode'."
  (setq-local completion-at-point-functions
    (list (cape-super-capf
            #'jf/version-control/issue-capf
            #'jf/version-control/project-capf
            #'jf/org-capf-links
            #'tempel-expand
            #'cape-file))))

;; Cribbed from `org-roam' org-roam-complete-link-at-point
(defun jf/org-capf-links ()
  "Complete links."
  (when (and (thing-at-point 'symbol)
          (not (org-in-src-block-p))
          (not (save-match-data (org-in-regexp org-link-any-re))))
    ;; We want the symbol so that links such performing completion on "org-mode"
    ;; will look for links with the text of org-mode and then replace the text
    ;; "org-mode" with the returned link.
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (list (car bounds) (cdr bounds)
        ;; Call without parameters, getting a links (filtered by CAPF magic)
        (jf/org-links-with-text)
        :exit-function
        (lambda (text _status)
          ;; We want the properties of that link.  In the case of one match, the
          ;; provided text will have the 'link property.  However if the
          (let ((link (car (jf/org-links-with-text text))))
            (delete-char (- (length text)))
            (insert "[[" (get-text-property 0 'link link) "][" text "]]")))
        ;; Proceed with the next completion function if the returned titles
        ;; do not match. This allows the default Org capfs or custom capfs
        ;; of lower priority to run.
        :exclusive 'no))))

(defun jf/org-links-with-text (&optional given-link)
  "Return the `distinct-' `org-mode' links in the `current-buffer'.

Each element of the list will be a `propertize' string where the string value is
the text of the link and the \"link\" property will be the :raw-link.

When provided a GIVEN-LINK stop processing when we encounter the
first matching link."
  (let ((links (org-element-map
                 (org-element-parse-buffer)
                 'link
                 (lambda (link)
                   (when-let* ((left (org-element-property :contents-begin link))
                                (right (org-element-property :contents-end link)))
                   (let ((returning (propertize
                                      (buffer-substring-no-properties
                                        left
                                        right)
                                      'link (org-element-property :raw-link link))))
                       (if given-link
                         (when (string= given-link returning) returning)
                         returning))))
                 nil
                 given-link)))
    ;; Ensure that we have a distinct list.
    (if (listp links)
      (-distinct links)
      (list links))))

;;; Begin Org Mode (all it's glory)
(use-package org
  :straight (org :type built-in)
  :hook
  (org-mode . (lambda ()
                (jf/org-capf)
                (turn-on-visual-line-mode)
                (electric-pair-mode -1)))
  ;; Disable org-indent-mode; it's easy enough to enable.  The primary reason is
  ;; that it does not play nice with the multi-cursor package.  And I'd prefer
  ;; to have that work better by default.
  ;;
  ;; (org-mode . org-indent-mode)
  :bind ("C-c C-j" . jf/org-mode/jump-to-agenda-or-mark)
  ("C-c C-x C-j" . org-clock-goto)
  :bind (:map org-mode-map (("C-c C-j" . jf/org-mode/jump-to-agenda-or-mark)
                             ("C-x n t" . jf/org-mode/narrow-to-date)
                             ("C-j" . avy-goto-char-2)))
  :custom (org-use-speed-commands t)
  (org-time-stamp-rounding-minutes '(0 15))
  (org-clock-rounding-minutes 15)
  (org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                           (vm-imap . vm-visit-imap-folder-other-frame)
                           (gnus . org-gnus-no-new-news)
                           (file . find-file)
                           (wl . wl-other-frame)))
  :config
  (setq org-clock-persist 'history)
  (setq org-confirm-babel-evaluate #'jf/org-confirm-babel-evaluate
    org-fontify-quote-and-verse-blocks t
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
    org-hide-emphasis-markers t
    ;; turning off org-elements cache speeds up input latency
    ;; See https://www.reddit.com/r/emacs/comments/11ey9ft/weekly_tips_tricks_c_thread/
    org-element-use-cache nil
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
                          "STARTED(s!)"
                          "|"
                          "WAITING(w@/!)"
                          "CANCELED(c@/!)"
                          "DONE(d!)")))
  (setq org-capture-templates
    '(("@"
        "All Todo"
        entry (file+olp
                "~/git/org/agenda.org"
                "General Todo Items")
        "* TODO %?\n  %i\n  %a"
        :empty-lines-before 1)
       ("b" "Blocker"
         plain (file+function
                 jf/primary-agenda-filename-for-machine
                 jf/org-mode-agenda-find-blocked-node)
         "***** WAITING %^{Describe the blocker} :blocker:"
         :immediate-finish t
         :jump-to-captured t
         :empty-lines-after 1)
       ("c" "Content to Denote"
         plain (file denote-last-path)
         #'jf/denote-org-capture
         :no-save t
         :immediate-finish nil
         :kill-buffer t
         :jump-to-captured t)
       ("C" "Content to Clock"
         plain (clock)
         "%(jf/denote/capture-wrap :link \"%L\" :content \"%i\")"
         :empty-lines 1)
       ("d" "Day with plain entry"
         plain (file+olp+datetree jf/primary-agenda-filename-for-machine)
         "%i"
         :empty-lines 1
         :time-prompt t
         :immediate-finish t)
       ("I" "Immediate to Clock"
         plain (clock)
         "%i%?"
         :immediate-finish t)
       ("m" "Merge Request"
         plain (file+function
                 jf/primary-agenda-filename-for-machine
                 jf/org-mode-agenda-find-merge-request-node)
         "***** STARTED %^{URL of Merge Request} :mergerequest:"
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
         "***** %^{State|STARTED|TODO} %^{Describe the task} :tasks:\n\n"
         :jump-to-captured t
         :immediate-finish t
         :clock-in t)))

  (defun jf/denote-org-capture ()
    (let ((denote-directory (f-join denote-directory "blog-posts")))
      (denote-org-capture)))

  (setq org-latex-default-class "jf/article")

  (org-babel-do-load-languages 'org-babel-load-languages
    (append org-babel-load-languages
      '((emacs-lisp . t)
         (shell . t)
         (plantuml . t)
         (ruby . t))))
  (add-to-list 'org-structure-template-alist '("m" . "marginnote"))
  (add-to-list 'org-structure-template-alist '("D" . "details"))
  (add-to-list 'org-structure-template-alist '("S" . "summary"))
  (add-to-list 'org-structure-template-alist '("U" . "update"))
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

(with-eval-after-load 'org
  (org-clock-persistence-insinuate))

(defun jf/org-confirm-babel-evaluate (lang body)
  "Regardless of LANG and BODY approve it."
  nil)

;;; Additional Functionality for Org Mode
;; Cribbed from https://xenodium.com/emacs-dwim-do-what-i-mean/
(defun jf/org-insert-link-dwim (parg &rest args)
  "Like `org-insert-link' but with personal dwim preferences.

  With PARG, skip personal dwim preferences.  Pass ARGS."
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
(bind-key "C-c C-j" 'jf/org-mode/jump-to-agenda-or-mark)
(cl-defun jf/org-mode/jump-to-agenda-or-mark (&optional prefix)
  "Jump to and from current agenda item to mark.

With one PREFIX go to personal agenda.
With two PREFIX go to place where we would jump on capture."
  (interactive "p")
  (require 'org-capture)
  (require 'pulsar)
  (cond
    ((>= prefix 16) (org-capture-goto-target "p"))
    ((>= prefix 4) (find-file "~/git/org/agenda.org"))
    (t (if (string= (buffer-file-name) (file-truename
                                                       jf/primary-agenda-filename-for-machine))
                       (call-interactively #'consult-global-mark)
                       (progn
                         (call-interactively #'set-mark-command)
                         (if (when (and (fboundp 'org-clocking-p) (org-clocking-p)) t)
                           (org-clock-goto)
                           ;; Jump to where we would put a project were we to capture it.
                           (org-capture-goto-target "p"))))))
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
          "+LEVEL=4+projects" 'agenda))
      #'string<)))

;; When I jump to a new task for the day, I want to position that task within
;; the prompted project.  Inspiration from
;; https://gist.github.com/webbj74/0ab881ed0ce61153a82e.
(cl-defun jf/org-mode-agenda-find-project-node
  (&key
    (tag "projects")
    (project (jf/org-mode-agenda-project-prompt))
    ;; The `file+olp+datetree` directive creates a headline like “2022-09-03 Saturday”.
    (within_headline (format-time-string "%Y-%m-%d %A")))
  "Position `point' at the end of the given PROJECT WITHIN_HEADLINE.

And use the given TAG."
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
          (goto-char (point-max))
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

When given PARG, prompt for the day of interest.

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
                        (5 (if (and
                                 (member "mergerequest" (org-element-property :tags hl))
                                 (eq 'done (org-element-property :todo-type hl)))
                             nil
                             (concat "- " (plist-get (cadr hl) :raw-value))))
                        (_ nil)))))))))
        (jf/create-scratch-buffer)
        (yank)))))

(defun jf/org-mode/narrow-to-date (date)
  "Narrow agenda to given DATE agenda subtree."
  (interactive (list (if current-prefix-arg
                       (org-read-date nil nil nil "Pick a day:")
                       (format-time-string "%Y-%m-%d"))))
  (widen)
  (goto-char (point-max))
  (re-search-backward (concat "^\*\*\* " date))
  (end-of-line)
  (org-narrow-to-subtree)
  (message "Narrowing to %s agenda" date))

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
  "Function to help report time for Scientist.com.

Assumes that I'm on a :projects: headline.

- Sum the hours (in decimal form) for the tasks.
- Create a list of the tasks.
- Write this information to the message buffer.
- Then move to the next heading level."
  (interactive)
  (let* ((project (plist-get (cadr (org-element-at-point)) :raw-value))
          (tasks (s-join "\n"
                   (org-with-wide-buffer
                     (when (org-goto-first-child)
                       (cl-loop collect (concat "- "
                                          (org-no-properties
                                            (org-get-heading t t t t)))
                         while (outline-get-next-sibling))))))
          (hours (/ (org-clock-sum-current-item) 60.0))
          (output (format "Tasks:\n%s\nProject: %s\nHours: %s\n"
                    tasks
                    project
                    hours)))
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

(require 'dig-my-grave)
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
  :custom ((org-modern-star '("◉" "○" "◈" "◇" "•"))
            ;; Showing the depth of stars helps with the speed keys as well as
            ;; gives a clearer indicator of the depth of the outline.
            (org-modern-hide-stars nil))
  :config (global-org-modern-mode))

(use-package org-appear
  :straight (:type git :host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode))

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
    '("mark" . "@@html:<mark>@@$1@@html:</mark>@@"))
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
  "Remove the link part of `org-mode' keeping only description."
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

;; (defun jf/force-org-rebuild-cache (prefix-arg)
;;   "Call functions to rebuild the applicable `org-mode' and `org-roam' cache(s).

;; When given PREFIX_ARG, clear the org-roam database (via
;;  `org-roam-db-clear-all') then sync.  This will slow down the sync."
;;   (interactive "P")
;;   (org-id-update-id-locations)
;;   (when (fboundp 'org-roam-db-clear-all)
;;     (progn
;;       (when (car prefix-arg) (org-roam-db-clear-all))
;;       (org-roam-db-sync)
;;       (org-roam-update-org-id-locations))))

(cl-defun jf/org-agenda/send-forward-task ()
  "Send an `org-mode' task node forward."
  (interactive)
  (save-excursion
    (let* ((day-project-task
             (jf/org-agenda/timesheet/get-day-and-project-and-task-at-point))
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

(defun jf/org-agenda/timesheet/get-day-and-project-and-task-at-point ()
  "Return a plist of :day, :project, and :task for element at point."
  (let* ((task (jf/org-agenda-headline-for-level :level 5))
          (project (progn
                     (org-up-heading-safe)
                     (org-element-at-point)))
          (day (progn
                 (org-up-heading-safe)
                 (org-element-at-point))))
    (list :project project :task task :day day)))

(cl-defun jf/org-agenda-headline-for-level (&key (level 5))
  "Find the `org-mode' ancestor headline with LEVEL."
  (let ((element (org-element-at-point)))
    (if (eq 'headline (org-element-type element))
      (let ((element-level (org-element-property :level element)))
        (cond
          ((= level element-level)
            (progn (message "Found %s" element) element))
          ((> level element-level)
            (user-error "Selected element %s is higher level." element-level))
          ((< level element-level)
            (progn (org-up-heading-safe) (jf/org-agenda-headline-for-level :level level)))))
      (progn
        (org-back-to-heading)
        (jf/org-agenda-headline-for-level :level level)))))

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
(defun jf/org-parse-headline (headline)
  "Raw value of the given HEADLINE plist."
  (plist-get (cadr headline) :raw-value))

(defun jf/org-get-headlines ()
  "Get a plist of `org-mode' headlines within the current buffer."
  (org-element-map (org-element-parse-buffer)
    'headline #'jf/org-parse-headline))

(defun jf/org-link-to-headline ()
  "Insert an internal link to a headline."
  (interactive)
  (let* ((headlines (jf/org-get-headlines))
          (choice (completing-read "Headings: " headlines nil t))
          (desc (read-string "Description: " choice)))
    (org-insert-link buffer-file-name (concat "*" choice) desc)))

;; If the example doesn't exist, create the example in the file

(cl-defun jf/org-mode/capture/prompt-for-example
  (&optional given-mode &key (tag "example"))
  "Prompt for the GIVEN-MODE example with given TAG."
  (let* ((mode (or given-mode (completing-read "Example:"
                                '("Existing" "New" "Stored")))))
    (cond
      ((string= mode "New")
        (let ((example (read-string "New Example Name: "
                         nil
                         nil
                         (format-time-string "%Y-%m-%d %H:%M:%S"))))
          (with-current-buffer (find-file-noselect
                                 jf/org-mode/capture/filename)
            (jf/org-mode/capture/set-position-file :headline nil
              :tag "examples"
              :depth 1)
            (insert (s-format jf/org-mode/capture/example-template
                      'aget
                      (list (cons "example" example) (cons "tag" tag))))
            example)))
      ((string= mode "Existing")
        (with-current-buffer (find-file-noselect
                               jf/org-mode/capture/filename)
          (let ((examples (org-map-entries
                            (lambda ()
                              (org-element-property :title (org-element-at-point)))
                            (concat "+LEVEL=2+" tag) 'file)))
            (if examples
              (completing-read "Example: " examples nil t)
              (jf/org-mode/capture/prompt-for-example "New" :tag tag)))))
      ((string= mode "Stored")
        (or jf/org-mode/capture/stored-context
          (jf/org-mode/capture/prompt-for-example "Existing" :tag tag))))))

(defvar jf/org-mode/capture/example-template
  (concat "\n\n** TODO ${example} :${tag}:\n\n*** TODO Context\n\n"
    "*** Code :code:\n\n*** TODO Discussion\n\n*** COMMENT Refactoring\n\n"))

(defvar jf/org-mode/capture/stored-context
  nil
  "A cached value to help quickly capture items.")

(cl-defun jf/org-mode/capture/set-position-file
  (&key
    (headline (jf/org-mode/capture/prompt-for-example))
    (tag "code")
    (depth 3))
  "Position `point' at the end of HEADLINE.

The HEADLINE must have the given TAG and be at the given DEPTH
and be a descendent of the given PARENT_HEADLINE.  If the
HEADLINE does not exist, write it at the end of the file."
  ;; We need to be using the right agenda file.
  (with-current-buffer (find-file-noselect jf/org-mode/capture/filename)
    (setq jf/org-mode/capture/stored-context headline)
    (let* ((existing-position (org-element-map
                                (org-element-parse-buffer)
                                'headline
                                (lambda (hl)
                                  (and (=(org-element-property :level hl) depth)
                                    (member tag
                                      (org-element-property :tags hl))
                                    (if headline
                                      (string= headline
                                        (plist-get
                                          (cadr
                                            (car
                                              (org-element-lineage hl)))
                                          :raw-value))
                                      t)
                                    (org-element-property :end hl)))
                                nil t)))
      (goto-char existing-position))))

;; With Heavy inspiration from http://www.howardism.org/Technical/Emacs/capturing-content.html
(defvar jf/org-mode/capture/template/default
  (concat "\n**** ${function-name}"
    "\n:PROPERTIES:"
    "\n:CAPTURED_AT: ${captured-at}"
    "\n:REMOTE_URL: [[${remote-url}][${function-name}]]"
    "\n:LOCAL_FILE: [[file:${file-name}::${line-number}]]"
    "\n:FUNCTION_NAME: ${function-name}"
    "\n:END:\n"
    "\n#+BEGIN_${block-type} ${block-mode}"
    "\n${block-text}"
    "\n#+END_${block-type}"))

(defvar jf/org-mode/capture/template/while-clocking
  (concat "\n:PROPERTIES:"
    "\n:CAPTURED_AT: ${captured-at}"
    "\n:REMOTE_URL: [[${remote-url}][${function-name}]]"
    "\n:LOCAL_FILE: [[file:${file-name}::${line-number}]]"
    "\n:FUNCTION_NAME: ${function-name}"
    "\n:END:\n"
    "\n#+BEGIN_${block-type} ${block-mode}"
    "\n${block-text}"
    "\n#+END_${block-type}"))

(cl-defun jf/org-mode/capture/get-field-values (block-text)
  "Get the text between START and END returning a fields and values.

The return value is a list of `cons' with the `car' values of:

- function-name
- captured-at
- remote-url
- file-name
- line-number
- block-type
- block-mode
- block-text"
  (require 'magit)
  (require 'git-link)
  (let* ((file-name (buffer-file-name (current-buffer)))
          (org-src-mode (replace-regexp-in-string
                          "-\\(ts-\\)?mode"
                          ""
                          (format "%s" major-mode)))
          (func-name (which-function))
          (type (cond
                  ((eq major-mode 'nov-mode) "QUOTE")
                  ((derived-mode-p 'prog-mode) "SRC")
                  (t "SRC" "EXAMPLE")))
          (file-base (if file-name
                       (file-name-nondirectory file-name)
                       (format "%s" (current-buffer))))
          (line-number (line-number-at-pos (region-beginning)))
          (remote-link (when (magit-list-remotes)
                         (progn
                           (call-interactively 'git-link)
                           (car kill-ring)))))
    `(("function-name" . ,(or func-name "Unknown"))
       ("captured-at" . ,(format-time-string "%Y-%m-%d %H:%M"))
       ("remote-url" . ,remote-link)
       ("file-name" . ,file-name)
       ("line-number" . ,line-number)
       ("block-type" . ,type)
       ("block-mode" . ,org-src-mode)
       ("block-text" . , block-text))))

(cl-defun jf/denote/capture-wrap (&key link content)
  "Given the LINK and CONTENT return a string to insert into the capture."
  ;; We must do funny business with the link to discern the type.
  (let* ((elements (s-split "::" (string-replace "]]" "" (string-replace "[[" "" link))))
          (parts (s-split ":" (car elements)))
          (type (car parts))
          (path (s-join ":" (cdr parts))))
    (message "Opening %s with %s" path type)
    (cond
      ((string= "elfeed" type)
        (save-excursion
          (funcall (org-link-get-parameter type :follow) path)
          (let ((url (elfeed-entry-link elfeed-show-entry))
                 (title (elfeed-entry-title elfeed-show-entry))
                 (author (plist-get (car (plist-get (elfeed-entry-meta elfeed-show-entry) :authors)) :name)))
            (concat "#+attr_shortcode:"
              (when author (concat " :pre " author))
              (when title (concat " :cite " title))
              (when url (concat " :cite_url " url))
              "\n#+begin_blockquote\n" content "\n#+end_blockquote\n%?"))))
      ((string= "file" type)
        (save-excursion
          (org-link-open-as-file path nil)
          (s-format jf/org-mode/capture/template/while-clocking
            'aget
            (jf/org-mode/capture/get-field-values content))))
      (t "\n#+begin_example\n" content "\n#+end_example"))))

(defun jf/org-mode/capture/parameters (prefix)
  "A logic lookup table by PREFIX."
  (cond
    ;; When we're clocking and no prefix is given...
    ((and (= 1 prefix) (fboundp 'org-clocking-p) (org-clocking-p))
      (list :key "C" :template jf/org-mode/capture/template/while-clocking))
    ;; We're not clocking or we provided a prefix.
    (t (list :key "c" :template jf/org-mode/capture/template/default))))

(bind-key "s-8" 'jf/org-mode/capture/insert-content-dwim)
(cl-defun jf/org-mode/capture/insert-content-dwim (start end prefix)
  "Capture the text between START and END.

Without PREFIX and not clocking capture clock otherwise capture to Backlog."
  (interactive "r\np")
  ;; There is a data structure looking to exist.  That structure is:
  ;;
  ;; - org-capture-key (default "c")
  ;; - template jf/org-mode/capture/template/default
  (let ((params (jf/org-mode/capture/parameters prefix))
         (block-text (buffer-substring-no-properties start end)))
    (org-capture-string (s-format (plist-get params :template)
                          'aget
                          (jf/org-mode/capture/get-field-values block-text))
      (plist-get params :key))))

(defun jf/capture/text-from-stdin (text)
  "Capture TEXT to current file.

I envision this function called from the command-line."
  (if (and (fboundp 'org-clocking-p) (org-clocking-p))
    (org-capture-string text "I")
    (with-current-buffer (window-buffer)
      (goto-char (point-max))
      (insert "\n" text))))

(defun jf/org-mode/open-all-unresolved-pull-requests ()
  "Opens all unresolved pull requests identified in agenda."
  (interactive)
  (dolist (url (-distinct
                 (org-map-entries
                   (lambda ()
                     (org-element-property :raw-value (org-element-at-point)))
                   "+LEVEL=5+mergerequests+TODO=\"STARTED\"" 'agenda)))
    (eww-browse-with-external-browser url)))

(provide 'jf-org-mode)
;;; jf-org-mode.el ends here
