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

(use-package window
  ;; Wrangle up how windows and buffers display.
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
       ("*Register Preview*" (display-buffer-reuse-window))
       (t (display-buffer-reuse-window display-buffer-same-window))))
  :bind (("s-q" . #'jf/bury-or-unbury-buffer))
  :config (setq confirm-kill-emacs #'yes-or-no-p)
  :preface
  ;; For some reason, the C-x 5 0 keybindings don't set in my brain.
  (defun jf/bury-or-unbury-buffer (&optional prefix)
    "Without PREFIX `bury-buffer' a buffer.

With one universal PREFIX, `unbury-buffer'.
With two universal PREFIX `delete-frame'.
With three or more universal PREFIX `save-buffers-kill-emacs'."
    (interactive "p")
    (cond
      ((eq prefix nil) (if buffer-read-only (kill-current-buffer) (bury-buffer)))
      ((>= prefix 64) (save-buffers-kill-emacs t))
      ((>= prefix 16) (delete-frame))
      ((>= prefix 4) (unbury-buffer))
      (t (if buffer-read-only (kill-current-buffer) (bury-buffer)))))
  (cl-defun jf/side-window-toggle ()
    "Either bury the dedicated buffer or open one based on `current-buffer'."
    (interactive)
    (if (window-dedicated-p)
      (bury-buffer)
      (call-interactively #'jf/display-buffer-in-side-window)))
  ;; Inspired by
  ;; https://www.reddit.com/r/emacs/comments/12l6c19/comment/jg98fk4/
  (cl-defun jf/display-buffer-in-side-window (&optional (buffer (current-buffer))
                                               &key (side 'right) (size 0.4) (slot 0))
    "Display BUFFER in dedicated side window.

With universal prefix, use left SIDE instead of right.  With two
universal prefixes, prompt for SIDE and SLOT and SIZE (which allows
setting up an IDE-like layout)."
    (interactive (list (current-buffer)
                   :side (pcase current-prefix-arg
                           ('nil 'right)
                           ('(4) 'left)
                           (_ (intern (completing-read "Side: "
                                        '(left right top bottom) nil t))))
                   :size (pcase current-prefix-arg
                           ('nil 0.45)
                           ('(4) 0.45)
                           (_ (read-number "Size: " 0.45)))
                   :slot (pcase current-prefix-arg
                           ('nil 0)
                           ('(4) 0)
                           (_ (read-number "Slot: ")))))
    (let ((display-buffer-mark-dedicated t)
           (size-direction (pcase side
                             ('right 'window-width)
                             ('left 'window-width)
                             (_ 'window-height))))
      ;; Question: Do I assume that I'll be focused in that new window?  If so,
      ;; consider `pop-to-buffer'.  Otherwise `display-buffer' will open the
      ;; buffer but leave focus in the originating window.
      (display-buffer buffer
        `(display-buffer-in-side-window
           (,size-direction . ,size)
           (side . ,side)
           (slot . ,slot)
           (window-parameters
             (mode-line-format . (" Dedicate: %b"))
             (no-delete-other-windows . t))))
      ;; The pulse makes sense when I'm using `display-buffer'.
      (pulsar-pulse-line-green))))

;; Show tabs as they are tricky little creatures
(defface jf/tabs-face '((default :inherit font-lock-misc-punctuation-face))
  "Help me see tabs; they are tricky creatures.")
(defface jf/bom-face '((default :inherit font-lock-misc-punctuation-face))
  "Help me see BOM characters \"﻿\"; they are tricky!")

(add-hook 'prog-mode-hook
  (lambda () (font-lock-add-keywords nil '(("\t" . 'jf/tabs-face)))
    (font-lock-add-keywords nil '(("﻿" . 'jf/bom-face)))
    ))
(add-hook 'text-mode-hook
  (lambda () (font-lock-add-keywords nil '(("\t" . 'jf/tabs-face)))
    (font-lock-add-keywords nil '(("﻿" . 'jf/bom-face)))
    ))

;; And now the theme.  I’ve chosen the modus themes (e.g. ~modus-vivendi~ and
;; ~modus-operandi~).  They provide a light and dark theme with a focus on visual
;; accessibility.
(defun jf/modus-themes-custom-faces ()
  "Set the various custom faces for both `treesit' and `tree-sitter'."
  (modus-themes-with-colors
    (custom-set-faces
      ;; `(org-meta-line
      ;;   ((,c (:foreground ,yellow-faint))))
      `(denote-faces-link
         ((,c (:inherit link
                :box (:line-width (1 . 1)
                       :color ,border-mode-line-inactive
                       :style released-button)))))
      `(jf/bom-face
         ((,c (:width ultra-expanded :box (:line-width (2 . 2)
                       :color ,underline-err
                       :style released-button)))))
      `(jf/tabs-face
         ((,c :underline (:style wave :color ,bg-blue-intense))))
      `(jf/org-faces-date
         ((,c :underline nil :foreground ,cyan-faint)))
      `(jf/org-faces-epigraph
         ((,c :underline nil :slant oblique :foreground ,fg-alt)))
      `(jf/org-faces-abbr
         ((,c :underline t :slant oblique :foreground ,fg-dim)))
      `(org-list-dt
         ((,c :bold t :slant italic :foreground ,fg-alt)))
      `(font-lock-misc-punctuation-face
         ((,c :foreground ,green-warmer)))
      `(org-block
         ((,c :background ,bg-ochre)))
      `(org-block-begin-line
         ((,c :background ,bg-lavender)))
      `(org-block-end-line
         ((,c :background ,bg-lavender)))
      `(hl-todo
         ((,c :foreground ,red-faint)))
      `(fill-column-indicator
         ((,c :width ultra-condensed :background ,fringe :foreground ,fringe)))
      `(font-lock-regexp-face
         ((,c :foreground ,red))))))

(add-hook 'modus-themes-after-load-theme-hook
  #'jf/modus-themes-custom-faces)

(use-package modus-themes
  ;; I love [[http://protesilaos.com][Prot]]’s attention to detail with the modus
  ;; themes.  Here’s my configuration for these two sibling themes.  There’s a
  ;; bit of chatter, but all told it sets things up how I like.
  :straight (:type git :host gitlab :repo "protesilaos/modus-themes" :branch "main")
  :init
  (setq modus-themes-italic-constructs t
    modus-themes-bold-constructs t
    modus-themes-mixed-fonts t
    modus-themes-variable-pitch-ui nil
    modus-themes-custom-auto-reload t
    modus-themes-disable-other-themes t
    modus-themes-org-blocks 'tinted-background
    modus-themes-common-palette-overrides
    '(
       (comment yellow-faint)
       (string green)
       ;; Favoring more of the defaults; below is some settings I've used for
       ;; quite a while (paired with the above)
       ;;
       ;;    (builtin magenta)
       ;;    (constant magenta-cooler)
       ;;    (docstring green-warmer)
       ;;    (docmarkup magenta-faint)
       ;;    (fnname magenta-warmer)
       ;;    (keyword cyan)
       ;;    (preprocessor cyan-cooler)
       ;;    (type magenta-cooler)
       ;;    (variable blue-warmer)
       ;;    (rx-construct red-faint)
       ;;    (rx-backslash blue-cooler)
       )
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


;; https://macowners.club/posts/custom-functions-5-navigation/
(global-set-key (kbd "C-x 2") #'jf/nav-split-and-follow-below)
(defun jf/nav-split-and-follow-below ()
  "Split the selected window in two with the new window is below.
This uses `split-window-below' but follows with the cursor."
  (interactive)
  (split-window-below)
  (other-window 1))

(global-set-key (kbd "C-x 3") #'jf/nav-split-and-follow-right)
(defun jf/nav-split-and-follow-right ()
  "Split the selected window in two with the new window is to the right.
This uses `split-window-right' but follows with the cursor."
  (interactive)
  (split-window-right)
  (other-window 1))

(global-set-key (kbd "s-\\") #'jf/nav-toggle-split-direction)
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
