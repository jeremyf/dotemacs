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

(transient-define-suffix jf/jump-to/agenda-local ()
  "Jump to local agenda"
  :description "Agenda (Local)"
  (interactive)
  (find-file jf/agenda-filename/local))

(transient-define-suffix jf/shr/toggle-images ()
  "Toggle showing or hiding images"
  :description (lambda ()
                                   (format "Show SHR Images (%s)"
                   (if shr-inhibit-images " " "*")))
  (interactive)
  (setq shr-inhibit-images (not shr-inhibit-images)))

(transient-define-suffix jf/jump-to/code-backlog ()
  "Jump to coding backlog"
  :description "Capture Backlog"
  (interactive)
  (find-file jf/org-mode/capture/filename))

;; (transient-define-suffix jf/capture-region-to-clock (b e p)
;;   "Capture region to clock…"
;;   :description "Capture region to clock…"
;;   (interactive "r\nP")
;;   (jf/org-mode/capture/insert-content-dwim b e p))

(transient-define-suffix jf/org-mode/add-description (description)
  "Add Description to `org-mode'"
  :description "Add Description…"
  (interactive (list (read-string "Description: ")))
  (when (jf/org-mode/blog-entry?)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^$")
      (insert "\n#+DESCRIPTION: " description))))

(transient-define-suffix jf/org-mode/add-session-report (date game location)
  "Add session report metadata (DATE, GAME, and LOCATION) to current buffer."
  :description "Add Session…"
  (interactive (list
                 (org-read-date nil nil nil "Session Date")
                 (completing-read "Game: " (jf/tor-game-list))
                 (completing-read "Location: " jf/tor-session-report-location)))
  (when (jf/org-mode/blog-entry?)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^$")
      (insert "\n#+HUGO_CUSTOM_FRONT_MATTER: :sessionReport "
              "'((date . \"" date "\") (game . \"" game "\") "
              "(location . \"" location "\"))"))))

(defun jf/org-mode/blog-entry? (&optional buffer)
  (when-let* ((buffer (or buffer (current-buffer)))
               (file (buffer-file-name buffer)))
    (and (denote-file-is-note-p file)
      (string-match-p "\\/blog-posts\\/" file))))

(transient-define-suffix jf/enable-indent-for-tab-command ()
  :description "Enable `indent-for-tab-command'"
  (interactive)
  (keymap-global-set "TAB" #'indent-for-tab-command))

;; this suffix provides a dynamic description of the current host I want to use
;; for my blog.  And the prefix’s function toggles the host.
(keymap-global-set "s-1" 'jf/menu)
(transient-define-prefix jf/menu ()
  "A context specific \"mega\" menu."
  ;; Todo, can I get this section into a function so I can duplicate it in the jf/menu--tor?
  [["Jump to"
     ("j a" jf/jump-to/agenda-local)
     ("j c" "Capture Backlog" jf/jump-to/code-backlog)
     ("j d" "Denote File" jf/jump_to_corresponding_denote_file :if-derived markdown-mode)
     ("j g" "Global Mark" consult-global-mark)
     ("j h" "Hugo File" jf/jump_to_corresponding_hugo_file :if-derived org-mode)
     ("j m" "Mark" consult-mark)
     ;; ("j p" "Jump in Pull requests" jf/org-mode/open-all-unresolved-pull-requests)
     ("j r" "Jump to Git Related" consult-git-related-find-file)
     ("j l" "Jump to Magit Project Lists" magit-list-repositories)
     ;; ("j s" "Jump to Shortdoc" shortdoc-display-group)
     ;; ("j v" jf/jump-to/violet-board)
     ]
    ["Tasks"
      ("s" "Search note content…" consult-notes-search-in-all-notes)
      ("S" "Search note filename…" consult-notes)
      ("C-t" "Start a timer…" tmr-with-description)
      ("t" "Todo for project…" magit-todos-list)
      ("T" "Mastodon Toot…" mastodon-toot)
      ("u" jf/org-mode/agenda-files-update)]
    ["Denote"
      ("d a" jf/project/add-project-path :if jf/denote?)
      ("d c" jf/denote-org-capture/filename-set)
      ("d p" jf/project/convert-document-to-project :if jf/denote?)
      ]
    ["Blogging"
      ("b d" jf/org-mode/add-description :if jf/org-mode/blog-entry?)
      ("b r" jf/org-mode/add-session-report :if jf/org-mode/blog-entry?)
      ("b s" "Add Series…" jf/org-mode/add-series-to-file :if jf/org-mode/blog-entry?)
      ("b x" "Export to TakeOnRules…" jf/export-org-to-tor :if jf/org-mode/blog-entry?)]]
  [["Modes"
     ;; I could write functions for these, but this is concise enough
     ("m h" jf/hammerspoon-toggle-mode  :if-non-nil hammerspoon-edit-minor-mode)
     ("m t" "Typopunct ( )" typopunct-mode :if-nil typopunct-mode)
     ("m t" "Typopunct (*)" typopunct-mode :if-non-nil typopunct-mode)
     ("m o" "MacOS Native Option ( )" jf/toggle-osx-alternate-modifier :if-non-nil ns-alternate-modifier)
     ("m o" "MacOS Native Option (*)" jf/toggle-osx-alternate-modifier :if-nil ns-alternate-modifier)
     ("m i" jf/shr/toggle-images)
     ("TAB" jf/enable-indent-for-tab-command)
     ]
    ["Grab Refs"
      ("g e" "Elfeed" jf/capture/denote/from/elfeed-show-entry :if-derived elfeed-show-mode)
      ("g f" "Firefox" jf/menu--org-capture-firefox)
      ("g s" "Safari" jf/menu--org-capture-safari)
      ("g w" "Eww" jf/capture/denote/from/eww-data :if-derived eww-mode)
      ]])

;; (use-package file-info
;;   ;; Show the metadata of the current buffer's file; and then copy those values.
;;   ;; The package also appears highly extensible.
;;   :straight (:host github :repo "artawower/file-info.el")
;;   :bind ("C-x f" . file-info-show)
;;   :config
;;   (setq hydra-hint-display-type 'posframe)
;;   (setq hydra-posframe-show-params `(:poshandler posframe-poshandler-frame-center
;; 						 :internal-border-width 2
;; 						 :internal-border-color "#61AFEF"
;; 						 :left-fringe 16
;; 						 :right-fringe 16)))

(provide 'jf-menus)
;;; jf-menus.el ends here
