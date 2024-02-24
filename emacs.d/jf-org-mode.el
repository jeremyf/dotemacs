;;; jf-org-mode.el --- Org-Mode configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;; Pre-amble to prepare for `org-mode'

;;; Code:
(require 'cl-lib)

;; I maintain a list of data directories, each might have “relevant to
;; org-mode” files.  The `jf/org-mode/agenda-files' reads the file system to gather
;; sources for `org-mode' agenda.
(defun jf/is-work-machine? ()
  "Am I working on my company machine machine."
  (string= (getenv "USER") "jeremy"))

(defvar jf/org-mode/capture/filename
  "~/git/org/denote/melange/20230210T184422--example-code__programming.org"
  "The file where I'm capturing content.

By default this is my example code project.")

(defconst jf/agenda-filename/scientist
  "~/git/org/denote/scientist/20221021T221357--scientist-agenda__agenda_scientist.org")

(defconst jf/agenda-filename/personal
  "~/git/org/agenda.org")

(defconst jf/lore24-filename
  "~/git/org/denote/indices/20231225T130631--lore24-in-the-shadows-of-mont-brun__Lore24_campaigns_rpgs.org")

(defvar jf/primary-agenda-filename-for-machine
  (if (jf/is-work-machine?)
    jf/agenda-filename/scientist
    jf/agenda-filename/personal))

(defun jf/org-capf ()
  "The `completion-at-point-functions' I envision using for `org-mode'."
  (setq-local completion-at-point-functions
    (list (cape-capf-super
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
      (seq-uniq links)
      (list links))))

;;; Begin Org Mode (all it's glory)
(use-package org
  :straight (org :type git :host github :repo "emacsmirror/org")
  :hook
  (org-mode . (lambda ()
                (add-hook 'org-mode-hook
                  (lambda ()
                    (add-hook 'before-save-hook 'jf/org-add-ids-to-headlines-in-file nil 'local)))
                (jf/org-capf)
                (setq-local tab-width 8)
                (turn-on-visual-line-mode)
                (electric-pair-mode -1)))
  ;; Disable org-indent-mode; it's easy enough to enable.  The primary reason is
  ;; that it does not play nice with the multi-cursor package.  And I'd prefer
  ;; to have that work better by default.
  ;;
  ;; (org-mode . org-indent-mode)
  :bind ("C-c C-j" . jf/project/jump-to-task)
  ("C-c C-x C-j" . org-clock-goto)
  :bind (:map org-mode-map (("C-c j" . org-goto)
                             ("C-c C-j" . jf/project/jump-to-task)
                             ("C-x n t" . jf/org-mode/narrow-to-date)
                             ("C-j" . avy-goto-char-timer)))
  :config
  (org-clock-persistence-insinuate)
  (setq org-use-speed-commands t)
  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2 :stepskip0 t :fileskip0 t :filetitle t :tags t))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-goto-interface #'outline-path-completion)
  (setq org-time-stamp-rounding-minutes '(0 15))
  (setq org-clock-rounding-minutes 15)
  (setq org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                                (vm-imap . vm-visit-imap-folder-other-frame)
                                (gnus . org-gnus-no-new-news)
                                (file . find-file)
                                (wl . wl-other-frame)))
  (setq org-clock-persist 'history)
  (setq org-export-headline-levels 4)
  ;; When I would load the agenda, I'd invariably type "l" to list the entries.
  (setq org-agenda-start-with-log-mode t)
  ;; I continue to encounter issues with not properly generating table of
  ;; contents.  As such I used the following:
  ;;
  ;; https://emacs.stackexchange.com/questions/76255/why-is-the-toc-missing-in-org-mode-latex-output
  ;;
  ;; Note, I did change from pdflatex to lualatex as the LaTeX class I'm often
  ;; using are only available in Lua processing.
  (setq org-latex-pdf-process '("lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                 "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-compiler "lualatex")
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
    org-imenu-depth 4
    org-hide-emphasis-markers t
    ;; turning off org-elements cache speeds up input latency
    ;; See https://www.reddit.com/r/emacs/comments/11ey9ft/weekly_tips_tricks_c_thread/
    org-element-use-cache nil
    org-export-with-sub-superscripts '{}
    org-pretty-entities t
    org-pretty-entities-include-sub-superscripts nil
    org-agenda-log-mode-items '(clock)
    org-directory (file-truename "~/git/org")
    ;; org-agenda-files (jf/org-mode/agenda-files)
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

  (transient-define-suffix jf/denote-org-capture/filename-set ()
    "Work with `jf/denote-org-capture/filename'"
    :description '(lambda ()
                    (concat
                      "Denote Capture Filename: "
                      (propertize (format "%s" (and denote-last-path
                                                 (file-exists-p denote-last-path)
                                                 (denote-retrieve-filename-title denote-last-path)))
                        'face 'transient-argument)))
    (interactive)
    (if denote-last-path
      (setq denote-last-path nil)
      (let ((fname (buffer-file-name (current-buffer))))
        (setq denote-last-path (and (denote-file-is-note-p  fname) fname)))))

  (defun jf/denote-org-capture ()
    "An org-capture conformant function for capturing to a blog-post."
    (if denote-last-path
      denote-org-capture-specifiers
      (let ((denote-directory (f-join denote-directory "blog-posts")))
        (denote-org-capture))))

  ;; https://stackoverflow.com/questions/13340616/assign-ids-to-every-entry-in-org-mode
  (defun jf/org-add-ids-to-headlines-in-file ()
    "Conditionally add ID properties to all file's headlines without an ID."
    (interactive)
    (org-map-entries 'org-id-get-create))

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
  (add-to-list 'org-structure-template-alist '("i" . "inlinecomment"))
  ;; I grabbed from the following LaTeX class from
  ;; https://www.reddit.com/r/emacs/comments/3zcr43/nooborgmode_custom_latexpdf_export_custom_style/.
  ;; I’m trash with LaTeX, but like the layout thusfar.

  ;; \\hypersetup{colorlinks=false,pdfborderstyle={/S/U/W 1},pdfborder=0 0 1}"
  ;; Make TAB act as if it were issued from the buffer of the languages's major
  ;; mode.
  :custom (org-src-tab-acts-natively t)
  (org-clock-clocktable-default-properties '(:maxlevel 5 :link t :tags t))
  :bind (:map org-mode-map
          ("C-c l u" . jf/org-mode/convert-link-type)
          ("C-c l i" . org-insert-link)
          ("M-g o" . consult-org-heading))
  :bind (("C-c l s" . org-store-link)
          ("C-c a" . org-agenda)
          ("C-c c" . org-capture)
          ("C-s-t" . org-toggle-link-display)))


(with-eval-after-load 'org
  (use-package ox
    :straight (ox :type built-in))

  (add-to-list 'org-export-global-macros
    '("kbd" . "@@html:<kbd>@@$1@@html:</kbd>@@"))
  (add-to-list 'org-export-global-macros
    '("cite" . "@@html:<cite>@@$1@@html:</cite>@@"))
  (add-to-list 'org-export-global-macros
    '("dfn" . "@@html:<dfn>@@$1@@html:</dfn>@@"))
  (add-to-list 'org-export-global-macros
    '("mark" . "@@html:<mark>@@$1@@html:</mark>@@"))
  (add-to-list 'org-export-global-macros
    '("scene-date" . "#+begin_marginnote\nThe scene occurs on @@html:<span class=\"time\">@@$1@@html:</span>@@.\n#+end_marginnote"))
  (add-to-list 'org-export-global-macros
    '("mention" . "@@hugo:{{< glossary key=\"@@$1@@hugo:\" >}}@@"))
  (add-to-list 'org-export-global-macros
    '("abbr" . "@@hugo:{{< glossary key=\"@@$1@@hugo:\" abbr=\"t\" >}}@@"))
  (add-to-list 'org-export-global-macros
    '("abbr-plural" . "@@hugo:{{< glossary key=\"@@$1@@hugo:\" abbr=\"t\" plural=\"t\" >}}@@"))
  (add-to-list 'org-export-global-macros
    '("i" . "@@html:<i class=\"dfn\">@@$1@@html:</i>@@"))
  (add-to-list 'org-export-global-macros
    '("mechanic" . "@@html:<i class=\"mechanic\">@@$1@@html:</i>@@"))
  (add-to-list 'org-export-global-macros
    '("m" . "@@html:<i class=\"mechanic\">@@$1@@html:</i>@@"))
  (add-to-list 'org-export-global-macros
    '("newline" . "@@latex:\\@@ @@html:<br />@@"))
  (add-to-list 'org-export-global-macros
    '("newpage" . "@@latex:\newpage@@"))
  (add-to-list 'org-export-global-macros
    '("rune" . "@@hugo:<span class=\"rune\">@@$1@@hugo:</span>@@"))
  (add-to-list 'org-export-global-macros
    '("linkToSeries" . "@@hugo:{{< linkToSeries \"@@$1@@hugo:\" >}}@@"))
  (add-to-list 'org-latex-classes
    '("jf/article"
       "\\documentclass[11pt,a4paper]{article}"
       ("\\section{%s}" . "\\section{%s}")
       ("\\subsection{%s}" . "\\subsection{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection{%s}")
       ("\\paragraph{%s}" . "\\paragraph{%s}")
       ("\\subparagraph{%s}" . "\\subparagraph{%s}")))
  (setq org-latex-default-class "jf/article")

  (use-package ox-gfm
    :straight t
    :init
    (require 'ox-gfm))

  (use-package igist
    :straight t
    :config
    (setq igist-current-user-name "jeremyf")
    (setq igist-auth-marker 'igist))

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

  ;; For automatically showing the invisible parts of org-mode.
  (use-package org-appear
    :straight (:type git :host github :repo "awth13/org-appear")
    :hook (org-mode . org-appear-mode)))

(defun jf/org-confirm-babel-evaluate (lang body)
  "Regardless of LANG and BODY approve it."
  nil)

;;; Org Mode time tracking and task tracking adjustments

(defun jf/org-mode-agenda-project-prompt ()
  "Prompt for project based on existing projects in agenda file.

    Note: I tried this as interactive, but the capture templates
    insist that it should not be interactive."
  (completing-read
    "Project: "
    (sort
      (seq-uniq
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

;;; Org Export and Composition Functionality
(setq org-export-global-macros (list))

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
    (cond
      ;; The 'eww-mode never fires :(
      ((eq 'eww-mode major-mode)
        (save-excursion
          (let* ((url (plist-get eww-data :url))
                  (title (plist-get eww-data :title)))
            (concat "#+attr_shortcode:"
              (when title (concat " :cite " title))
              (when url (concat " :cite_url " url))
              "\n#+begin_blockquote\n" content "\n#+end_blockquote\n%?"))))
      ((string= "elfeed" type)
        (save-excursion
          (funcall (org-link-get-parameter type :follow) path)
          (let ((url (elfeed-entry-link elfeed-show-entry))
                 (title (elfeed-entry-title elfeed-show-entry))
                 (author (plist-get (car (plist-get (elfeed-entry-meta elfeed-show-entry) :authors)) :name)))
            (concat (when (or author title url) "#+attr_shortcode:")
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
      ((or (string= "http" type) (string= "https" type))
        (save-excursion
          (concat "#+attr_shortcode: :cite_url " link
            "\n#+begin_blockquote\n" content "\n#+end_blockquote\n%?")))
      (t (concat "\n#+begin_example\n" content "\n#+end_example")))))
(defun jf/org-mode/capture/parameters (prefix)
  "A logic lookup table by PREFIX."
  (cond
    ;; When we're clocking and no prefix is given...
    ((and (= 1 prefix) (fboundp 'org-clocking-p) (org-clocking-p))
      (list :key "i" :template jf/org-mode/capture/template/while-clocking))
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
    (org-capture-string text "i")
    (with-current-buffer (window-buffer)
      (goto-char (point-max))
      (insert "\n" text))))

(setq jf/campaign/file-name
  "~/git/org/denote/indices/20231127T184806--the-shadows-of-mont-brun-status-document__campaigns_projects_rpgs_StatusDocuments.org")

(cl-defun jf/campaign/named-element (&key tag)
  "Fetch PROPERTY from headlines with parent that has given TAG."
  (with-current-buffer (find-file-noselect jf/campaign/file-name)
    (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
      (lambda (headline)
        (and (member tag
               (org-element-property
                 :tags
                 ;; Get immediate parent
                 (car (org-element-lineage headline))))
          (org-element-property :title headline))))))

(defun jf/campaign/random-npc-as-entry ()
  "Create an NPC entry."
  (let* ((random-table/reporter
           ;; Bind a function that will only output the results of the
           ;; table, excluding the expression that generated the
           ;; results.
          (lambda (expression results) (format "%s" results)))
         (name
          (random-table/roll "In the Shadows of Mont Brun > Names"))
         (quirk
          (random-table/roll "Random NPC Quirks"))
         (alignment
          (random-table/roll "Noble House > Alignment"))
         (lore-table
          (random-table/roll "The One Ring > Lore"))
         (locations
          (s-join ", "
                  (completing-read-multiple
                   "Location(s): "
                   (jf/campaign/named-element :tag "locations"))))
         (factions
          (s-join ", "
                  (completing-read-multiple
                   "Faction(s): "
                   (jf/campaign/named-element :tag "factions")))))
    (format (concat
             "<<<%s>>>\n:PROPERTIES:\n:NAME:  %s\n:BACKGROUND:\n"
             ":LOCATIONS:  %s\n:DEMEANOR:\n:ALIGNMENT:  %s\n"
             ":QUIRKS:  %s\n:FACTIONS:  %s\n:END:\n\n%s")
    name name locations alignment quirk factions lore-table)))


(setq org-capture-templates
  '(("d" "To Denote"
      plain (file denote-last-path)
      #'jf/denote-org-capture
      :no-save t
      :immediate-finish nil
      :kill-buffer t
      :jump-to-captured t)
     ("c" "Content to Clock"
       plain (clock)
       "%(jf/denote/capture-wrap :link \"%L\" :content \"%i\")"
       :empty-lines 1)
     ("i" "Immediate to Clock"
       plain (clock)
       "%i%?"
       :immediate-finish t)
     ("k" "Kill to Clock"
       plain (clock)
       "%c" :immediate-finish t)
     ("l" "#Lore24 Entry"
       plain (file+olp+datetree jf/lore24-filename)
       "%?"
       :clock-in t
       :clock-keep t
       :empty-lines-before 1
       :jump-to-captured t)
     ("n" "NPC"
       entry (file+headline jf/campaign/file-name "Non-Player Characters")
       "* %(jf/campaign/random-npc-as-entry)\n%?"
       :empty-lines-after 1
       :jump-to-captured t)
     ("t" "Task (via Journal)"
       entry (function denote-journal-extras-new-or-existing-entry)
       "* %^{Task} :%(jf/project-as-tag):\n\n- Link to Project :: %(jf/project-as-link)\n\n%?"
       :empty-lines-before 1
       :empty-lines-after 1
       :clock-in t
       :clock-keep t
       :jump-to-captured t)
     ("T" "Task (via Project)"
       plain (function jf/org-mode/capture/project-task/find)
       "%?"
       :empty-lines-before 1
       :empty-lines-after 1
       :clock-in t
       :clock-keep t
       :jump-to-captured t)
     ("N" "Note for project task"
       plain (function jf/org-mode/capture/project-task/find)
       "%T\n\n%?"
       :empty-lines-before 1
       :empty-lines-after 1)))

(defvar jf/link-to-project nil)

(defun jf/project-as-tag ()
  "Prompt for project and kill link to project."
  (let* ((project
           (completing-read "Project: " (jf/project/list-projects)))
          (keyword
            (denote-sluggify-keyword project))
          (file
            (cdar (jf/project/list-projects :project project)))
          (identifier (denote-retrieve-filename-identifier file)))
    (setq jf/link-to-project (format "[[denote:%s][%s]]" identifier project))
    keyword))

(defun jf/project-as-link ()
  (let ((link jf/link-to-project))
    (setq jf/link-to-project nil)
    link))

(defun jf/derive-project-tag ()
  )
;; (use-package org-noter
;;              :straight
;;              (:repo "org-noter/org-noter"
;;                     :host github
;;                     :type git
;;                :files ("*.el" "modules/*.el"))
;;   :config
;;   (setq org-noter-doc-split-fraction '(0.67 . 0.33)))

(use-package org-bookmark-heading
  ;; Capture more robust org-mode bookmarks
  :straight t
  :preface
  (defun jf/org-bookmark-heading--display-path (path)
    "Return display string for PATH.

Returns the title at the PATH when file is a `denote' file."
    (if (denote-file-is-note-p path)
      (denote-retrieve-filename-title path)
      (org-bookmark-heading--display-path path))))

(defun jf/org-bookmark-heading-make-record (&rest app)
  "Coerce the absolute file path to home relative.

APP is the parameters for saving the bookmark."
  (let ((bookmark-alist (apply app)))
    (when-let ((home-relative-filename (jf/filename/tilde-based (alist-get 'filename bookmark-alist))))
      (setf (alist-get 'filename bookmark-alist) home-relative-filename))
    ;; Return the modified bookmark-alist
    bookmark-alist))
(advice-add #'org-bookmark-heading-make-record :around #'jf/org-bookmark-heading-make-record)


;; (use-package org-pdftools
;;   :straight t
;;   :hook (org-mode . org-pdftools-setup-link))

;; (use-package org-noter-pdftools
;;   :straight t
;;   :after org-noter
;;   :config
;;   ;; Add a function to ensure precise note is inserted
;;   (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
;;     (interactive "P")
;;     (org-noter--with-valid-session
;;      (let ((org-noter-insert-note-no-questions (if toggle-no-questions
;;                                                    (not org-noter-insert-note-no-questions)
;;                                                  org-noter-insert-note-no-questions))
;;            (org-pdftools-use-isearch-link t)
;;            (org-pdftools-use-freepointer-annot t))
;;        (org-noter-insert-note (org-noter--get-precise-info)))))

;;   ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
;;   (defun org-noter-set-start-location (&optional arg)
;;     "When opening a session with this document, go to the current location.
;; With a prefix ARG, remove start location."
;;     (interactive "P")
;;     (org-noter--with-valid-session
;;       (let ((inhibit-read-only t)
;;              (ast (org-noter--parse-root))
;;              (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
;;         (with-current-buffer (org-noter--session-notes-buffer session)
;;           (org-with-wide-buffer
;;             (goto-char (org-element-property :begin ast))
;;             (if arg
;;               (org-entry-delete nil org-noter-property-note-location)
;;               (org-entry-put nil org-noter-property-note-location
;;                 (org-noter-pdftools--pretty-print-location location))))))))
;;   (with-eval-after-load 'pdf-annot
;;     (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))


;; Convert the data ":PROPERTY: VALUE" into a latex item or markdown definition
;; term and detail.
(setq org-export-filter-node-property-functions
  '(jf/ox/transform-node-property-to-item))

(defun jf/ox/transform-node-property-to-item (data back-end channel)
  "Convert DATA to appropriate markup for given BACK-END.

CHANNEL is ignored."
  (let* ((field-value (s-split ":" data))
          (term (s-titleize (s-replace "_" " " (car field-value))))
          (value (s-trim (cadr field-value))))
    (if (s-blank? value)
      ""
      (cond
        ((eq back-end 'latex)
          (format "\\item[{%s:}] %s\n" term value))
        ((eq back-end 'md)
          (format "%s\n: %s\n" term value))
        (t data)))))

(defun jf/org-latex-property-drawer (_property-drawer contents _info)
  "Transcode a PROPERTY-DRAWER element from Org to LaTeX.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (and (org-string-nw-p contents)
    (format "\\begin{description}\n%s\\end{description}\n\\vspace{5mm}" contents)))

(advice-add #'org-latex-property-drawer :override #'jf/org-latex-property-drawer)

(defun jf/org-latex-format-basic-headline-function
  (_todo _todo-type _priority text _tags _info)
  "Only render the TEXT of the headlin.
See `org-latex-format-headline-function' for details."
  text)

;; Without these, I've lost table of contents in PDF exports.
(defun jf/org-export-change-options (plist backend)
  (cond
    ((equal backend 'html)
      (plist-put plist :with-toc nil)
      (plist-put plist :section-numbers nil))
    ((equal backend 'latex)
      (plist-put plist :with-toc 3)
      (plist-put plist :section-numbers nil)))
  plist)
(add-to-list 'org-export-filter-options-functions #'jf/org-export-change-options)

;;; From https://emacs.stackexchange.com/questions/22210/auto-update-org-tables-before-each-export
;; Recalculate all org tables in the buffer when saving.
(defvar-local jf/org-enable-buffer-wide-recalculation t
  "When non-nil, recalculate all dynamic regions when saving the file.

This variable is buffer local.")
;; Mark `jf/org-enable-buffer-wide-recalculation' as a safe local
;; variable as long as its value is t or nil. That way you are not prompted
;; to add that to `safe-local-variable-values' in custom.el.
(put 'jf/org-enable-buffer-wide-recalculation 'safe-local-variable #'booleanp)

(defun jf/org-recalculate-buffer-tables (&rest args)
  "Wrapper function for `org-table-recalculate-buffer-tables' and
`org-dblock-update' that runs that function only if
`jf/org-enable-buffer-wide-recalculation' is non-nil.

Also, this function has optional ARGS that is needed for any function that is
added to `org-export-before-processing-hook'. This would be useful if this
function is ever added to that hook."
  (when jf/org-enable-buffer-wide-recalculation
    (progn
      (org-table-recalculate-buffer-tables)
      (org-dblock-update '(4)))))

(defun jf/org-recalculate-before-save ()
  "Recalculate dynamic buffer regions before saving."
  (add-hook 'before-save-hook #'jf/org-recalculate-buffer-tables nil :local))
(add-hook 'org-mode-hook #'jf/org-recalculate-before-save)

(use-package org-web-tools
  :straight t)

(provide 'jf-org-mode)
;;; jf-org-mode.el ends here
