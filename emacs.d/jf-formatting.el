;;; jf-formatting.el --- formatting functions -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary

;; This package is on shaky grounds; I don't know if it makes sense.  There's
;; whitespace and sentence considerations.  There's also `htmlize' which is for
;; exporting a region as markdown or HTML.

;;; Code

(use-package ws-butler
  :straight t
  :hook (prog-mode . ws-butler-mode))

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
