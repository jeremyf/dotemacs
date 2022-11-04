;;; jf-formatting.el --- formatting functions -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary

;; This package is on shaky grounds; I don't know if it makes sense.  There's
;; whitespace and sentence considerations.  There's also `htmlize' which is for
;; exporting a region as markdown or HTML.

;;; Code

(use-package ethan-wspace
  ;; Whitespace hygene package.  The author's documentation and commentary
  ;; echoes my sentiments.
  :straight t
  :hook (before-save . delete-trailing-whitespace)
  :init (setq-default mode-require-final-newline nil)
  :config (global-ethan-wspace-mode 1))

(use-package fill-sentences-correctly
  ;; `fill-sentences-correctly-mode' ensures that `fill-paragraph' (e.g. M-q)
  ;; preserves two spaces.
  :straight (fill-sentences-correctly
	     :host github
	     :repo "duckwork/fill-sentences-correctly.el")
  :hook (fundamental-mode . fill-sentences-correctly-mode))

(use-package tomelr
  :straight (tomelr :host github :repo "kaushalmodi/tomelr"))

(provide 'jf-formatting)
;;; jf-formatting.el ends here
