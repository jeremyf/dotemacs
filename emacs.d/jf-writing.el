;;; jf-writing.el --- General writing functions -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;; Packages specifically here for helping with my writing activities.

;;; Code

(require 'jf-org-mode)
(require 'jf-denote)

(use-package emojify
  ;; All the people using emojiis; why not
  :straight t
  :config
  (defun --set-emoji-font (frame)
    "Adjust the font settings of FRAME so Emacs can display emoji properly."
    (if (eq system-type 'darwin)
	;; For NS/Cocoa
	(set-fontset-font t
			  'symbol
			  (font-spec :family "Apple Color Emoji")
			  frame
			  'prepend)
      ;; For Linux
      (set-fontset-font t
			'symbol
			(font-spec :family "Symbola")
			frame
			'prepend)))
  ;; For when Emacs is started in GUI mode:
  (--set-emoji-font nil)
  ;; Hook for when a frame is created with emacsclient
  ;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
  (add-hook 'after-make-frame-functions '--set-emoji-font))

(use-package sdcv-mode
  ;; This follows from
  ;; http://mbork.pl/2017-01-14_I'm_now_using_the_right_dictionary
  ;;
  ;; Namely I want to use a more inspiring dictionary for the poetry and prose.
  :straight (sdcv-mode :type git :host github :repo "gucong/emacs-sdcv")
  :bind ("C-c C-'" . sdcv-search))

(use-package flymake-proselint
  ;; I don't have much in the way of flymake actually running; this may be
  ;; superfluous.
  :straight t)

;; (use-package flymake-vale
;;   :straight (flymake-value :type git
;; 			   :host github
;; 			   :repo "tpeacock19/flymake-vale")
;;   :hook ((text-mode       . flymake-vale-load)
;;          (latex-mode      . flymake-vale-load)
;;          (org-mode        . flymake-vale-load)
;;          (markdown-mode   . flymake-vale-load)
;;          (message-mode    . flymake-vale-load)))

(use-package unicode-fonts
  ;; Before the emojii...
  :straight t
  :config (unicode-fonts-setup))


(use-package unfill
  ;; Provides the reverse of ~fill-paragraph~, and a toggle fill and unfill.  In
  ;; fact, the unfill/fill function of Emacs was the first editor function I saw
  ;; (shown to me by a friend in 2005) that had me strongly consider Emacs. Alas
  ;; I was not prepared for Emacs.
  :bind ("M-q" . unfill-toggle)
  :straight t)

(use-package hungry-delete
  ;; Delete multiple spaces in one delete stroke.
  :straight t
  :config (global-hungry-delete-mode))

(use-package move-text
  ;; A simple package ability to move lines up and down.
  :straight t
  :bind (([C-s-down] . move-text-down)
         ([C-s-up] . move-text-up)))

(use-package titlecase
  ;; The rules of “titlecase” are confounding.  The ~titlecase.el~ package
  ;; provides numerous ways to cast a string to “titlecase.”  I chose wikipedia
  ;; style as a quasi-opinionated compromise.
  :straight (titlecase :host github :repo "duckwork/titlecase.el")
  :custom (titlecase-style 'wikipedia))

(use-package multiple-cursors
  ;; Allow Emacs to work with multiple cursors.  See
  ;; https://melpa.org/#/multiple-cursors
  :bind (("C-M-SPC" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-s-<mouse-1>" . mc/add-cursor-on-click)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/edit-lines)) ;; CTRL+CMD+c
  :straight t)

(use-package iedit
  ;; Type \"C-;\" to select current symbol and all matches; Then edit at multiple
  ;; points.
  :straight t)

(provide 'jf-writing)
;;; jf-writing.el ends here
