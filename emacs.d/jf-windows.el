;;; jf-windows.el --- Working to manage my windows -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary
;;
;; This package provides theme, frame, and window support.

;;; Code

;;;; Themes
(mapc #'disable-theme custom-enabled-themes)

(use-package window
  :straight (:type built-in)
  :custom
  (display-buffer-alist
   '(;; no windows
     ("\\`\\*Async Shell Command\\*\\'"
      (display-buffer-no-window))
     ;; I like the slide out window for this "context-type menus"
     ("\\*\\(eldoc\\|Ilist\\|Embark Actions\\|helpful .*\\)\\*"
      (display-buffer-in-side-window)
      (window-width . 0.5)
      (side . right)
      (slot . 0)
      (window-parameters . ((mode-line-format . (" %b")))))
     ;; Maybe I want re-use
     (t (display-buffer-reuse-window display-buffer-same-window))
     ;; (t (display-buffer-same-window))
     ;; These I want as part of the "default" windowing experience
     ;; ("\\*\\(elfeed\\|scratch\\).*"
     ;;  (display-buffer-same-window))
     ;; ;; Side Left
     ;; ;;
     ;; ;; I'd been using bufler with tabs.  However the tab behavior is not
     ;; ;; something that I regularly leverage; in part because tab grouping is not
     ;; ;; as predictable as I'd like.
     ;; ("\\*Bufler\\*"
     ;;  (display-buffer-in-side-window)
     ;;  (window-width . 0.67)
     ;;  (dedicated . t)
     ;;  (window-parameters . ((mode-line-format . ("Select a Buffer"))))
     ;;  (side . left)
     ;;  (slot . 0))
     ;; ;; Side Right
     ;; ;;
     ;; ;; Windows that provide supplementary context for the initiating buffer.

     ;; ("\\*Embark Export.*"
     ;;  (display-buffer-in-side-window)
     ;;  (window-width . 0.4)
     ;;  (dedicated . t)
     ;;  (side . right)
     ;;  (slot . 1))
     ;; ;; Side bottom
     ;; ;;
     ;; ("\\*\\(Org Select\\)\\*" ; the `org-capture' key selection
     ;;  (display-buffer-in-side-window)
     ;;  (dedicated . t)
     ;;  (side . bottom)
     ;;  (slot . -1)
     ;;  (window-height . fit-window-to-buffer))
     ;; ;; Pop a new frame
     ;; ((or . ((derived-mode . Man-mode)
     ;;              (derived-mode . woman-mode)
     ;;              "\\*\\(Man\\|woman\\).*"))
     ;;       (display-buffer-reuse-window display-buffer-pop-up-frame)
     ;;       (pop-up-frame-parameters . ((width . (text-pixels . 640))
     ;;                                   (height . (text-pixels . 640)))))
     ;; ("\\*\\(Agenda Commands\\|Embark Actions\\|Org Agenda\\)\\*"
     ;;       (display-buffer-reuse-mode-window display-buffer-at-bottom)
     ;;       (window-height . fit-window-to-buffer)
     ;;       (window-parameters . ((no-other-window . t)
     ;;                             ;; (mode-line-format . none)
     ;; 				 )))
     ;; ;; The junk drawer of *something* buffers.
     ;; ("\\*.*\\*"
     ;;  (display-buffer-in-side-window)
     ;;  (window-height . 0.40)
     ;;  (side . bottom)
     ;;  (slot . 0))
     )))

;; And now the theme.  I’ve chosen the modus themes (e.g. ~modus-vivendi~ and
;; ~modus-operandi~).  They provide a light and dark theme with a focus on visual
;; accessibility.


(defun jf/modus-themes-custom-faces ()
  "Set the various custom faces for both `treesit' and `tree-sitter'."
  (modus-themes-with-colors
    (custom-set-faces
     `(jf/org-faces-date
       ((,c :underline nil :foreground ,cyan-faint)))
     `(jf/org-faces-epigraph
       ((,c :underline nil :slant oblique :foreground ,fg-alt)))
     `(jf/org-faces-abbr
       ((,c :underline t :slant oblique :foreground ,fg-dim)))
     `(org-list-dt
       ((,c :bold t :slant italic :foreground ,fg-alt)))
     `(tree-sitter-hl-face:constant
       ((,c :slant italic :foreground ,magenta-cooler)))
     `(tree-sitter-hl-face:method.call
       ((,c :slant italic :foreground ,fg-alt)))
     `(tree-sitter-hl-face:operator
       ((,c :foreground ,red-faint)))
     `(tree-sitter-hl-face:property
       ((,c :inherit font-lock-variable-name-face)))
     `(font-lock-misc-punctuation-face
       ((,c :foreground ,green-warmer)))
     `(font-lock-regexp-face
       ((,c :foreground ,magenta-warmer))))))

(add-hook 'modus-themes-after-load-theme-hook
	  #'jf/modus-themes-custom-faces)

;; I love [[http://protesilaos.com][Prot]]’s attention to detail with the modus
;; themes.  Here’s my configuration for these two sibling themes.  There’s a
;; bit of chatter, but all told it sets things up how I like.
(use-package modus-themes
  ;; :straight (modus-themes :type built-in)
  :straight (:type git :host gitlab :repo "protesilaos/modus-themes" :branch "main")
  :init
  (setq modus-themes-italic-constructs t
	modus-themes-bold-constructs t
	modus-themes-mixed-fonts t
	modus-themes-variable-pitch-ui nil
	modus-themes-custom-auto-reload t
	modus-themes-disable-other-themes t
	modus-themes-common-palette-overrides '((bg-mode-line-active bg-blue-subtle)
						(border-mode-line-active blue-intense)
						(comment yellow-faint)
						(constant magenta-cooler)
						(docmarkup magenta-faint)
						(docstring green-faint)
						(fg-mode-line-active fg-main)
						(fnname magenta-warmer)
						(keyword cyan)
						(preprocessor cyan-cooler)
						(rx-backslash blue-cooler)
						(rx-construct magenta-warmer)
						(string green-cooler)
						(type magenta-cooler)
						(variable blue-warmer)
						(builtin magenta))
	modus-themes-completions '((matches . (extrabold))
				   (selection . (semibold accented))
				   (popup . (accented intense)))
	modus-themes-headings
	'((1 . (variable-pitch light 1.6))
	  (2 . (overline semibold 1.5))
	  (3 . (monochrome overline 1.4 background))
	  (4 . (overline 1.3))
	  (5 . (rainbow 1.2))
	  (6 . (rainbow 1.15))
	  (t . (rainbow 1.1)))))

(defun jf/dark ()
  "Toggle system-wide Dark or Light setting."
  (interactive)
  (shell-command "osascript -e 'tell application \"System Events\" to tell appearance preferences to set dark mode to not dark mode'")
  (jf/emacs-theme-by-osx-appearance))

(defun jf/emacs-theme-by-osx-appearance ()
  "Set theme based on OSX appearance state."
  (if (equal "Dark" (substring
		     (shell-command-to-string
		      "defaults read -g AppleInterfaceStyle")
		     0 4))
      (modus-themes-load-theme 'modus-vivendi-tinted)
    (modus-themes-load-theme 'modus-operandi-tinted)))

(jf/emacs-theme-by-osx-appearance)

;;;; Buffers and Tabs
;; https://github.com/alphapapa/bufler.el
;;
;; Why this instead of Centaur Tabs?  `bufler' integrates with `tab-bar-mode'
;; and `tab-lines-mode'.  Why is this important?  Because `centaur-tabs-mode'
;; hack the buffer to add the tabs; the impact was that popped buffers would
;; have sizing issues.
(use-package bufler
  :straight t
  :hook (after-init . (bufler-mode))
  :custom (bufler-columns '("Name" "VC" "Path"))
  :config
  (defun jf/bufler/tab-configuration ()
    (bufler-tabs-mode 1)
    (tab-bar-mode -1)
    (bufler-workspace-tabs))
  (setq tab-line-switch-cycling t)
  (defun jf/bufler-workspace-mode-lighter ()
    "Return the lighter string mode line."
    "Bflr")
  (advice-add #'bufler-workspace-mode-lighter
	      :override #'jf/bufler-workspace-mode-lighter
	      '((name . "wrapper")))
  ;; Ensuring that when I make a selection, it closes the bufler buffer.
  (defun jf/bufler-list-buffer-switch (&rest args)
    (kill-buffer "*Bufler*"))
  (advice-add 'bufler-list-buffer-switch :after 'jf/bufler-list-buffer-switch)

  :bind (:map bufler-list-mode-map
	      ("s-3" . quit-window)
	      ("s-\\" . quit-window))
  :bind (("s-3" . bufler-switch-buffer)
	 ("s-\\" . bufler-sidebar)
	 ;; ("s-\\" . jf/tab-bar-switch-prompt-for-tab)
	 ;; ("s-]" . tab-line-switch-to-next-tab)
	 ;; ("s-}" . tab-line-switch-to-next-tab)
	 ;; ("s-[" . tab-line-switch-to-prev-tab)
	 ;; ("s-{" . tab-line-switch-to-prev-tab)
	 ))

;; (defun jf/tab-bar-switch-to-next-tab ()
;;   "Move to the next `tab-bar' tab and open the first buffer."
;;   (interactive)
;;   (call-interactively 'tab-bar-switch-to-next-tab)
;;   (jf/tab-bar-activate-first-buffer))

;; (defun jf/tab-bar-switch-to-prev-tab ()
;;   "Move to the previous `tab-bar' tab and open the first buffer."
;;   (interactive)
;;   (call-interactively 'tab-bar-switch-to-prev-tab)
;;   (jf/tab-bar-activate-first-buffer))

;; (defun jf/tab-bar-activate-first-buffer ()
;;   "Switch to the first buffer in this buffer group.

;;   This is cribbed from `bufler-switch-buffer'."
;;   (let* ((path (frame-parameter nil 'bufler-workspace-path))
;; 	 (buffers (bufler-buffer-alist-at
;;                    path :filter-fns bufler-workspace-switch-buffer-filter-fns)))
;;     (switch-to-buffer (caar buffers)))
;;   ;; A hack to ensure that I have the top tabs; I don't need it because I could
;;   ;; use `jf/tab-bar-switch-prompt-for-tab'.
;;   (jf/bufler/tab-configuration))

;; (defun jf/tab-bar-switch-prompt-for-tab (name)
;;   "Switch to the NAME tab and prompt for a buffer."
;;   (interactive
;;    (let* ((recent-tabs (mapcar (lambda (tab)
;;                                  (alist-get 'name tab))
;;                                (bufler-workspace-tabs))))
;;      (list (completing-read "Select tab-bar: "
;;                             recent-tabs nil nil nil nil recent-tabs))))
;;   (tab-bar-select-tab (1+ (or (tab-bar--tab-index-by-name name) 0)))
;;   (bufler-switch-buffer)
;;   (jf/bufler/tab-configuration))

(provide 'jf-windows)
;;; jf-windows.el ends here