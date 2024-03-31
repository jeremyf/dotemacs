;;; jf-windows.el --- Working to manage my windows -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:
;;
;; This package provides theme, frame, and window support.

;;; Code:

;;;; Themes
(mapc #'disable-theme custom-enabled-themes)

(require 'prot-window)

(use-package window
  ;; Wrangle up how windows and buffers display.
  :straight (:type built-in)
  :config
  (defun jf/body-function/rspec-compilation (window)
    "Select the WINDOW and move to `end-of-buffer'."
    (select-window window)
    (end-of-buffer))
  (setq display-buffer-alist
    `(;; no window
       ("\\`\\*Async Shell Command\\*\\'"
         (display-buffer-no-window))
       ("\\`\\*\\(Warnings\\|Compile-Log\\|Org Links\\)\\*\\'"
         (display-buffer-no-window)
         (allow-no-window . t))
       ;; bottom side window
       ("\\*Org \\(Select\\|Note\\)\\*" ; the `org-capture' key selection and `org-add-log-note'
         (display-buffer-in-side-window)
         (dedicated . t)
         (side . bottom)
         (slot . 0)
         (window-parameters . ((mode-line-format . (" %b")))))
       ;; bottom buffer (NOT side window)
       ((or . ((derived-mode . flymake-diagnostics-buffer-mode)
                (derived-mode . flymake-project-diagnostics-mode)
                (derived-mode . messages-buffer-mode)
                (derived-mode . backtrace-mode)))
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . 0.3)
         (dedicated . t)
         (preserve-size . (t . t)))
       ("\\*Embark Actions\\*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . fit-window-to-buffer)
         (window-parameters . ((no-other-window . t)
                                (mode-line-format . (" %b")))))
       ("\\*\\(Output\\|Register Preview\\).*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom))
       ;; below current window
       ("\\(\\*Capture\\*\\|CAPTURE-.*\\)"
         (display-buffer-reuse-mode-window display-buffer-below-selected))
       ("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . 0.1)
         (dedicated . t)
         (preserve-size . (t . t)))
       ("\\*rspec-compilation\\*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (body-function . jf/body-function/rspec-compilation))
       ((derived-mode . reb-mode) ; M-x re-builder
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . 4) ; note this is literal lines, not relative
         (dedicated . t)
         (preserve-size . (t . t)))
       ((or . ((derived-mode . occur-mode)
                (derived-mode . grep-mode)
                (derived-mode . rg-mode)
                (derived-mode . Buffer-menu-mode)
                (derived-mode . log-view-mode)
                (derived-mode . help-mode) ; See the hooks for `visual-line-mode'
                "\\*\\(|Buffer List\\|Occur\\|vc-change-log\\).*"))
         (prot-window-display-buffer-below-or-pop)
         (dedicated . t)
         (body-function . prot-window-select-fit-size))
       ("\\*\\(Calendar\\|Bookmark Annotation\\|ert\\).*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (dedicated . t)
         (window-height . fit-window-to-buffer))
       ;; NOTE 2022-09-10: The following is for `ispell-word', though
       ;; it only works because I override `ispell-display-buffer'
       ;; with `prot-spell-ispell-display-buffer' and change the
       ;; value of `ispell-choices-buffer'.
       ("\\*ispell-top-choices\\*.*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . fit-window-to-buffer))
       ;; same window
       ;; NOTE 2023-02-17: `man' does not fully obey the
       ;; `display-buffer-alist'.  It works for new frames and for
       ;; `display-buffer-below-selected', but otherwise is
       ;; unpredictable.  See `Man-notify-method'.
       ((or . ((derived-mode . Man-mode)
                (derived-mode . woman-mode)
                "\\*\\(Man\\|woman\\).*"))
         (display-buffer-same-window))))
  (setq confirm-kill-emacs #'yes-or-no-p)
  :bind (("s-q" . #'jf/bury-or-unbury-buffer))
  :preface
  ;; For some reason, the C-x 5 0 keybindings don't set in my brain.
  (defun jf/bury-or-unbury-buffer (&optional prefix)
    "Without PREFIX `bury-buffer' a buffer.

With one universal PREFIX, `unbury-buffer'.
With two universal PREFIX `delete-frame'.
With three or more universal PREFIX `save-buffers-kill-emacs'."
    (interactive "p")
    (cond
      ((eq prefix nil)
        (if buffer-read-only (kill-current-buffer) (bury-buffer)))
      ((>= prefix 64)
        (progn
          (let ((save-silently t)) (recentf-save-list))
          (save-buffers-kill-emacs t)))
      ((>= prefix 16)
        (delete-frame))
      ((>= prefix 4)
        (unbury-buffer))
      (t
        (if buffer-read-only (kill-current-buffer) (bury-buffer))))))

;; Show tabs as they are tricky little creatures
(defface jf/tabs-face '((default :inherit font-lock-misc-punctuation-face))
  "Help me see tabs; they are tricky creatures.")
(defface jf/bom-face '((default :inherit font-lock-misc-punctuation-face))
  "Help me see BOM characters \"﻿\"; they are tricky!")

(add-hook 'prog-mode-hook
  (lambda ()
    (unless (member major-mode '(go-mode go-ts-mode))
    (font-lock-add-keywords nil '(("\t" . 'jf/tabs-face)))
    (font-lock-add-keywords nil '(("﻿" . 'jf/bom-face))))))
(add-hook 'text-mode-hook
  (lambda () (font-lock-add-keywords nil '(("\t" . 'jf/tabs-face)))
    (font-lock-add-keywords nil '(("﻿" . 'jf/bom-face)))
    ))

(defvar jf/themes-plist '()
  "The named themes by pallette.")

;; (load "jf-modus-themes.el")
(load "jf-ef-themes.el")

(defun jf/emacs-theme-by-osx-appearance ()
  "Function to load named theme."
  (load-theme (plist-get jf/themes-plist (jf/current-macos-interface-style))))

;;; Theming hooks to further customize colors
(defvar after-enable-theme-hook nil
   "Normal hook run after enabling a theme.")

(defun run-after-enable-theme-hook (&rest _args)
   "Run `after-enable-theme-hook'."
   (run-hooks 'after-enable-theme-hook))

(advice-add 'enable-theme :after #'run-after-enable-theme-hook)

(add-hook 'after-enable-theme-hook #'jf/theme-custom-faces)

(defun jf/current-macos-interface-style ()
  (if (equal "Dark"
        (substring
          (shell-command-to-string "defaults read -g AppleInterfaceStyle") 0 4))
    :dark :light))

(defun jf/dark ()
  "Toggle system-wide Dark or Light setting."
  (interactive)
  (shell-command "osascript -e 'tell application \"System Events\" to tell appearance preferences to set dark mode to not dark mode'")
  (jf/emacs-theme-by-osx-appearance))

(jf/emacs-theme-by-osx-appearance)

;;; Window splitting

;; https://macowners.club/posts/custom-functions-5-navigation/
(keymap-global-set "C-x 2" #'jf/nav-split-and-follow-below)
(defun jf/nav-split-and-follow-below ()
  "Split the selected window in two with the new window is below.
This uses `split-window-below' but follows with the cursor."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(keymap-global-set "C-x 3" #'jf/nav-split-and-follow-right)
(defun jf/nav-split-and-follow-right ()
  "Split the selected window in two with the new window is to the right.
This uses `split-window-right' but follows with the cursor."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(advice-add #'kill-buffer-and-window :after #'balance-windows)

(keymap-global-set "s-\\" #'jf/nav-toggle-split-direction)
(defun jf/nav-toggle-split-direction ()
  "Toggle window split from vertical to horizontal.
This work the other way around as well.
Credit: https://github.com/olivertaylor/dotfiles/blob/master/emacs/init.el"
  (interactive)
  (if (> (length (window-list)) 2)
    (error "Can't toggle with more than 2 windows")
    (let ((was-full-height (window-full-height-p)))
      (delete-other-windows)
      (if was-full-height
        (split-window-vertically)
        (split-window-horizontally))
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(provide 'jf-windows)
;;; jf-windows.el ends here
