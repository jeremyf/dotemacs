;;; jf-menus --- A container for my Emacs menus -*- lexical-binding: t -*-

;; Copyright (C) 2022 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;;; Code

(bind-key "M-[" #'backward-paragraph)
(bind-key "s-[" #'backward-paragraph)
(bind-key "M-]" #'forward-paragraph)
(bind-key "s-]" #'forward-paragraph)

(transient-define-suffix jf/jump-to/violet-board ()
  "Jump to 💜 Violet 💜"
  :description "Jump to 💜 Violet 💜"
  (interactive)
  (require 'eww)
  (eww-browse-with-external-browser "https://github.com/orgs/scientist-softserv/projects/43"))
(bind-key "C-c l v" #'jf/jump-to/violet-board)

;; this suffix provides a dynamic description of the current host I want to use
;; for my blog.  And the prefix’s function toggles the host.
(global-set-key (kbd "s-1") 'jf/menu)
(transient-define-prefix jf/menu ()
  "A context specific \"mega\" menu."
  ;; Todo, can I get this section into a function so I can duplicate it in the jf/menu--tor?
  [["Jump to"
     ("j a" "Agenda" jf/org-mode/jump-to-agenda-or-mark)
     ("j A" "Agenda, Personal" (lambda () (interactive) (find-file "~/git/org/agenda.org")))
     ("j c" "Capture Backlog" (lambda () (interactive) (find-file jf/org-mode/capture/filename)))
     ("j g" "Global Mark" consult-global-mark)
     ("j h" "Hugo File" jf/jump_to_corresponding_hugo_file :if-derived org-mode)
     ("j m" "Mark" consult-mark)
     ("j p" "Jump in Pull requests" jf/org-mode/open-all-unresolved-pull-requests)
     ("j r" "Jump to Git Related" consult-git-related-find-file)
     ("j l" "Jump to List of Projects" magit-list-repositories)
     ;; ("j s" "Jump to Shortdoc" shortdoc-display-group)
     ("j v" jf/jump-to/violet-board)]
    ["Tasks"
      ("a" "Git Annotation" vc-annotate)
      ("c" "Capture region to clock…" (lambda (b e p) (interactive "r\nP") (jf/capture-region-contents-with-metadata b e p)))
      ("d" "Deadgrep…" deadgrep)
      ("i" "Clock in…" consult-clock-in)
      ("r" "Run command…" run-command)
      ("s" "Search note content…" consult-notes-search-in-all-notes)
      ("S" "Search note filename…" consult-notes)
      ("C-t" "Start a timer…" tmr-with-description)
      ("t" "Todo for project…" magit-todos-list)
      ("u" "Copy stand-up to kill ring" jf/org-mode-agenda-to-stand-up-summary)
      ("w" "Weekly hours report" jf/org-mode-weekly-report)
      ("x" "Export to TakeOnRules…" jf/export-org-to-tor :if-derived org-mode)]]
  [["Modes"
     ;; I could write functions for these, but this is concise enough
     ("m h" jf/hammerspoon-toggle-mode  :if-non-nil hammerspoon-edit-minor-mode)
     ("m t" "Typopunct ( )" typopunct-mode :if-nil typopunct-mode)
     ("m t" "Typopunct (*)" typopunct-mode :if-non-nil typopunct-mode)
     ("m o" "MacOS Native Option ( )" jf/toggle-osx-alternate-modifier :if-non-nil ns-alternate-modifier)
     ("m o" "MacOS Native Option (*)" jf/toggle-osx-alternate-modifier :if-nil ns-alternate-modifier)
     ]
    ["Grab Refs"
      ("g e" "Elfeed" jf/menu--org-capture-elfeed-show :if-derived elfeed-show-mode)
      ("g f" "Firefox" jf/menu--org-capture-firefox)
      ("g s" "Safari" jf/menu--org-capture-safari)
      ("g w" "Eww" jf/menu--org-capture-eww :if-derived eww-mode)
      ]])

(use-package file-info
  ;; Show the metadata of the current buffer's file; and then copy those values.
  ;; The package also appears highly extensible.
  :straight (:host github :repo "artawower/file-info.el")
  :bind ("C-x f" . file-info-show)
  :config
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params `(:poshandler posframe-poshandler-frame-center
						 :internal-border-width 2
						 :internal-border-color "#61AFEF"
						 :left-fringe 16
						 :right-fringe 16)))

(provide 'jf-menus)
;;; jf-menus.el ends here
