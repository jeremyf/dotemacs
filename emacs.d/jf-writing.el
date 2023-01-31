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

;; This follows from
;; http://mbork.pl/2017-01-14_I'm_now_using_the_right_dictionary
(use-package sdcv-mode
  :straight (sdcv-mode :type git :host github :repo "gucong/emacs-sdcv")
  :bind ("C-c C-'" . sdcv-search))

(use-package flymake-proselint :straight t)

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
  :straight t
  :config (unicode-fonts-setup))


;; Provides the reverse of ~fill-paragraph~, and a toggle fill and unfill.
(use-package unfill
  :bind ("M-q" . unfill-toggle)
  :straight t)


;; Delete multiple spaces in one delete stroke.
(use-package hungry-delete
  :straight t
  :config (global-hungry-delete-mode))

;; A simple package ability to move lines up and down.
(use-package move-text
  :straight t
  :bind (([C-s-down] . move-text-down)
         ([C-s-up] . move-text-up)))

;; The rules of “titlecase” are confounding.  The ~titlecase.el~ package
;; provides numerous ways to cast a string to “titlecase.”  I chose wikipedia
;; style as a quasi-opinionated compromise.
(use-package titlecase
  :straight (titlecase :host github :repo "duckwork/titlecase.el")
  :custom (titlecase-style 'wikipedia))

;; Allow Emacs to work with multiple cursors.  See
;; https://melpa.org/#/multiple-cursors
(use-package multiple-cursors
  :after (region-bindings)
  :bind (("C-M-SPC" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-s-<mouse-1>" . mc/add-cursor-on-click)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/edit-lines)) ;; CTRL+CMD+c
  :bind ( :map region-bindings-mode-map
	  ("n" . mc/mark-next-symbol-like-this)
	  ("N" . mc/mark-next-like-this)
	  ("p" . mc/mark-previous-symbol-like-this)
	  ("P" . mc/mark-previous-like-this)
	  ("a" . mc/mark-all-symbols-like-this)
	  ("A" . mc/mark-all-like-this)
	  ("s" . mc/mark-all-in-region-regexp)
	  ("l" . mc/edit-ends-of-lines))
  :straight t)

;; Type \"C-;\" to select current symbol and all matches; Then edit at multiple
;; points.
(use-package iedit :straight t)

(provide 'jf-writing)
;;; jf-writing.el ends here
