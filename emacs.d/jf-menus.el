;;; jf-menus --- A container for my Emacs menus -*- lexical-binding: t -*-

;; Copyright (C) 2022 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;;; Code

(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "s-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "s-]") 'forward-paragraph)

;; this suffix provides a dynamic description of the current host I want to use
;; for my blog.  And the prefix’s function toggles the host.

(global-set-key (kbd "s-1") 'jf/menu)
(transient-define-prefix jf/menu ()
  "A context specific \"mega\" menu."
  ;; Todo, can I get this section into a function so I can duplicate it in the jf/menu--tor?
  [["Markdown Utilities"
    ("k h" "Kill slug version of given heading…" jf/kill-new-markdown-heading-as-slug :if-derived (or markdown-mode html-mode))
    ("w a" "A-tag at point or region…" jf/tor-wrap-link-active-region-dwim  :if-derived (or markdown-mode html-mode))
    ("w c" "CITE-tag point or region…" jf/tor-wrap-cite-active-region-dwim  :if-derived (or markdown-mode html-mode))
    ("w d" "DATETIME-tag point or region…" jf/tor-wrap-date  :if-derived (or markdown-mode html-mode))
    ("w f" "Wrap word or region in pseudo-DFN…" jf/tor-wrap-as-pseudo-dfn  :if-derived (or markdown-mode html-mode))
    ]
   [
    "" ;; Deliberate space for menu
    ("w m" "Margin-note line or region…" jf/tor-wrap-as-marginnote-dwim  :if-derived (or markdown-mode html-mode))
    ("w p" "Wrap point or region as Poem…" jf/tor-wrap-in-poem  :if-derived (or markdown-mode html-mode))
    ("w s" "Side-note sentence or region…" jf/tor-wrap-as-sidenote-dwim  :if-derived (or markdown-mode html-mode))
    ("w w" "Wrap point or region in html…" jf/tor-wrap-in-html-tag  :if-derived (or markdown-mode html-mode))
    ]]
  [
   ;; ["Contexts"
   ;;  ("-b" "Burning Wheel…"  jf/menu--bwg)
   ;;  ("-e" "Eberron…" jf/menu--eberron)
   ;;  ("-w" "Register window configuration" window-configuration-to-register)
   ;;  ]
   ["Jump to"
    ("j a" "Agenda" jf/jump-to-agenda-or-mark)
    ("j b" "iBuffer" ibuffer)
    ("j g" "Global Mark" consult-global-mark)
    ("j h" "Hugo File" jf/jump_to_corresponding_hugo_file :if-derived org-mode)
    ("j m" "Mark" consult-mark)
    ;; ("j P" "Jump in PDF" doc-view-goto-page :if-derived doc-view-mode)
    ("j l" "Jump to List of Projects" magit-list-repositories)
    ;; ("j s" "Jump to Shortdoc" shortdoc-display-group)
    ("j v" "Jump to Violet Sprint Board" (lambda () (interactive) (eww-browse-with-external-browser "https://github.com/orgs/scientist-softserv/projects/43") ))
    ]
   ["Tasks"
    ("c" "Capture region to clock…" (lambda (b e p) (interactive "r\nP") (jf/capture-region-contents-with-metadata b e p)))
    ("d" "Deadgrep…" deadgrep)
    ("r" "Run command…" run-command)
    ("s" "Search note content…" consult-notes-search-in-all-notes)
    ("S" "Search note filename…" consult-notes)
    ("C-t" "Start a timer…" tmr-with-description)
    ("u" "Copy stand-up to kill ring" jf/org-mode-agenda-to-stand-up-summary)
    ("w" "Weekly hours report" jf/org-mode-weekly-report)
    ("x" "Export to TakeOnRules…" jf/export-org-to-tor :if-derived org-mode)
    ]]
  [
   ["Modes"
    ;; I could write functions for these, but this is concise enough
    ("m h" jf/hammerspoon-toggle-mode  :if-non-nil hammerspoon-edit-minor-mode)
    ("m t" "Typopunct ( )" typopunct-mode :if-nil typopunct-mode)
    ("m t" "Typopunct (*)" typopunct-mode :if-non-nil typopunct-mode)
    ("m o" "MacOS Native Option ( )" jf/toggle-osx-alternate-modifier :if-non-nil ns-alternate-modifier)
    ("m o" "MacOS Native Option (*)" jf/toggle-osx-alternate-modifier :if-nil ns-alternate-modifier)
    ]
   ;; ["Add Metadata"
   ;; ("C-e a" "Add epigraph properties…" jf/org-mode-add-epigraph-keys :if-derived org-mode)
   ;; ("C-e i" "Add epigraph at point…" jf/org-roam-insert-at-point-epigraph-macro :if-derived org-mode)
   ;; ("r a" "Ref add…" org-roam-ref-add :if-derived org-mode)
   ;; ("t s" "Tag as session report…" jf/org-tag-as-session-report :if-derived org-mode)
   ;; ("t a" "Tag add…" org-roam-tag-add :if-derived org-mode)
   ;; ("t S" "Tag as session Scene…" jf/org-tag-session-scene-with-date :if-derived org-mode)]
   ;; ["Context"
   ;; ("M-c" "Context set…" jf/org-auto-tags--set-by-context :transient t)
   ;; ("M-t" jf/org-auto-tags--transient :transient t)
   ;; ]

   ["Grab Refs"
    ("g e" "Elfeed" jf/menu--org-capture-elfeed-show :if-derived elfeed-show-mode)
    ("g f" "Firefox" jf/menu--org-capture-firefox)
    ("g s" "Safari" jf/menu--org-capture-safari)
    ("g w" "Eww" jf/menu--org-capture-eww :if-derived eww-mode)
    ]
   ])


(use-package file-info
  :straight (:host github :repo "artawower/file-info.el")
  :bind ("C-x f" . file-info-show)
  :config
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params `(:poshandler posframe-poshandler-frame-center
						 :internal-border-width 2
						 :internal-border-color "#61AFEF"
						 :left-fringe 16
						 :right-fringe 16)))
;; (add-to-list 'file-info-handlers `( :name "Git link"
;; 				       :handler (git-link (current-buffer))
;; 				       :cache t
;; 				       :face font-lock-string-face
;; 				       :bind "g"))
;; (unbind-key "C-c d") ;; 'file-info-show
;; (global-set-key (kbd "C-c d") 'jf/duplicate-current-line-or-lines-of-region)
(provide 'jf-menus)
;;; jf-menus.el ends here
