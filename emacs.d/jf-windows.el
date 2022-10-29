;;; jf-windows.el --- Working to manage my windows -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary
;;
;; This package provides font, theme, and window support.

;;; Code

;;;; Fonts
(use-package fontaine
  :straight t
  :config
  (setq fontaine-presets
        '((small
           :default-height 110)
          (default
           :default-height 155)
          (coding
           :default-height 155)
          (presenting
           :default-weight semilight
           :default-height 220
           :bold-weight extrabold)
          (reading
           :default-weight semilight
           :default-height 220
           :bold-weight extrabold)
          (t
           ;; Following Prot’s example, keeping these for for didactic purposes.
           :default-family "Iosevka Comfy Motion Fixed"
           :default-weight regular
           :default-height 155
           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil ; falls back to :default-family
           :fixed-pitch-serif-weight nil ; falls back to :default-weight
           :fixed-pitch-serif-height 1.0
           :variable-pitch-family "ETBembo"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil))))
(fontaine-set-preset 'default)

;;;; Themes
(mapc #'disable-theme custom-enabled-themes)

;; And now the theme.  I’ve chosen the modus themes (e.g. ~modus-vivendi~ and
;; ~modus-operandi~).  They provide a light and dark theme with a focus on visual
;; accessibility.

;; I love [[http://protesilaos.com][Prot]]’s attention to detail with the modus
;; themes.  Here’s my configuration for these two sibling themes.  There’s a
;; bit of chatter, but all told it sets things up how I like.
(use-package modus-themes
  ;; :straight (modus-themes :type built-in)
  :straight (:type git :host gitlab :repo "protesilaos/modus-themes" :branch "main")
  :init
  (setq
   modus-themes-vivendi-color-overrides
   ;; '((bg-region-accent-subtle . "#240f55")) ;; Default
   ;; '((bg-region-accent-subtle . "#323da2"));; Good candidate
   '((bg-region-accent-subtle . "#304466"))

   modus-themes-bold-constructs t
   modus-themes-completions '((matches . (extrabold))
                              (selection . (semibold accented))
                              (popup . (accented intense)))
   modus-themes-diffs nil
   modus-themes-fringes 'intense
   modus-themes-hl-line '(accented intense)
   modus-themes-intense-markup t
   modus-themes-links '(faint background)
   modus-themes-subtle-line-numbers t
   modus-themes-mixed-fonts t
   modus-themes-mode-line '(accented moody)
   modus-themes-org-blocks 'gray-background
   modus-themes-paren-match '(bold intense)
   modus-themes-prompts '(intense accented)
   modus-themes-region '(bg-only accented)
   modus-themes-scale-headings t
   modus-themes-slanted-constructs t
   modus-themes-subtle-line-numbers t
   modus-themes-syntax '(alt-syntax yellow-comments green-strings)
   modus-themes-tabs-accented t
   modus-themes-headings
   '((1 . (variable-pitch light 1.6))
     (2 . (overline semibold 1.4))
     (3 . (monochrome overline 1.2 background))
     (4 . (overline 1.1))
     (t . (rainbow 1.05)))))

(use-package ef-themes
  :straight (ef-themes :host nil :type git :repo "https://git.sr.ht/~protesilaos/ef-themes")
  :custom (ef-themes-headings ; read the manual's entry or the doc string
	   '((0 . (variable-pitch light 1.9))
             (1 . (variable-pitch light 1.8))
             (2 . (variable-pitch regular 1.7))
             (3 . (variable-pitch regular 1.6))
             (4 . (variable-pitch regular 1.5))
             (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
             (6 . (variable-pitch 1.3))
             (7 . (variable-pitch 1.2))
             (t . (variable-pitch 1.1))))
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t))

(ef-themes-select 'ef-cyprus)
;; (if (eq system-type 'darwin)
;;     (progn
;;       (defun jf/dark ()
;;         "Toggle system-wide Dark or Light setting."
;;         (interactive)
;;         (shell-command "osascript -e 'tell application \"System Events\" to tell appearance preferences to set dark mode to not dark mode'")
;;         (jf/emacs-theme-by-osx-appearance))

;;       (defalias 'modus-themes-toggle 'jf/dark)
;;       (defun jf/emacs-theme-by-osx-appearance ()
;;         "Set theme based on OSX apperance state."
;;         (if (equal "Dark" (substring (shell-command-to-string "defaults read -g AppleInterfaceStyle") 0 4))
;;             (modus-themes-load-vivendi)
;;           (modus-themes-load-operandi)))
;;       ;; And load the appropriate theme
;;       (jf/emacs-theme-by-osx-appearance))
;;   (progn
;;     (defun modus-themes-toggle ()
;;       "Toggle between `modus-operandi' and `modus-vivendi' themes."
;;       (interactive)
;;       (if (eq (car custom-enabled-themes) 'modus-vivendi)
;;           (modus-themes-load-vivendi)
;;         (modus-themes-load-operandi)))
;;     (modus-themes-load-operandi)))

;;;; Buffers and Tabs
;; https://github.com/alphapapa/bufler.el
;;
;; Why this instead of Centaur Tabs?  `bufler' integrates with `tab-bar-mode'
;; and `tab-lines-mode'.  Why is this important?  Because `centaur-tabs-mode'
;; hack the buffer to add the tabs; the impact was that popped buffers would
;; have sizing issues.
(use-package bufler
  :straight t
  :hook (after-init . (lambda () (bufler-mode) (bufler-tabs-mode 1)))
  :config
  (setq tab-line-switch-cycling t)
  :bind (:map bufler-list-mode-map ("s-3" . quit-window))
  :bind (("s-b" . bufler-switch-buffer)
	 ("s-3" . bufler)
	 ("s-\\" . jf/tab-bar-switch-prompt-for-tab)
	 ("s-]" . tab-line-switch-to-next-tab)
	 ("s-}" . jf/tab-bar-switch-to-next-tab)
	 ("s-[" . tab-line-switch-to-prev-tab)
	 ("s-{" . jf/tab-bar-switch-to-prev-tab)))

(defun jf/bufler-workspace-mode-lighter ()
  "Return the lighter string mode line."
  "Bflr")
(advice-add #'bufler-workspace-mode-lighter :override #'jf/bufler-workspace-mode-lighter '((name . "wrapper")))

(defun jf/tab-bar-switch-to-next-tab ()
  "Move to the next `tab-bar' tab and open the first buffer."
  (interactive)
  (call-interactively 'tab-bar-switch-to-next-tab)
  (jf/tab-bar-activate-first-buffer))

(defun jf/tab-bar-switch-to-prev-tab ()
  "Move to the previous `tab-bar' tab and open the first buffer."
  (interactive)
  (call-interactively 'tab-bar-switch-to-prev-tab)
  (jf/tab-bar-activate-first-buffer))

(defun jf/tab-bar-activate-first-buffer ()
  "Switch to the first buffer in this buffer group.

  This is cribbed from `bufler-switch-buffer'."
  (let* ((path (frame-parameter nil 'bufler-workspace-path))
	 (buffers (bufler-buffer-alist-at
                   path :filter-fns bufler-workspace-switch-buffer-filter-fns)))
    (switch-to-buffer (caar buffers)))
  ;; A hack to ensure that I have the top tabs; I don't need it because I could
  ;; use `jf/tab-bar-switch-prompt-for-tab'.
  (bufler-tabs-mode t))

(defun jf/tab-bar-switch-prompt-for-tab (name)
  "Switch to the NAME tab and prompt for a buffer."
  (interactive
   (let* ((recent-tabs (mapcar (lambda (tab)
                                 (alist-get 'name tab))
                               (bufler-workspace-tabs))))
     (list (completing-read "Select tab-bar: "
                            recent-tabs nil nil nil nil recent-tabs))))
  (tab-bar-select-tab (1+ (or (tab-bar--tab-index-by-name name) 0)))
  (bufler-switch-buffer)
  (bufler-tabs-mode t))

(provide 'jf-windows)
;;; jf-windows.el ends here