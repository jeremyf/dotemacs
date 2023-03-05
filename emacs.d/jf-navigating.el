;;; jf-navigating.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;;; Code

;;;; Packages
(use-package ace-window
  :straight t
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("C-x o" . ace-window)
	 ("M-o" . ace-window)))

(use-package avy
;; Pick a letter, avy finds all words with that at the beginning of it.  Narrow
;; results from there.
  :bind (("C-j" . avy-goto-char-2))
  :straight t)

(use-package imenu-list
  :custom (imenu-list-focus-after-activation t)
  (imenu-list-size 0.4)
  (imenu-list-position 'right)
  :bind ("s-4" . 'imenu-list-smart-toggle)
  :bind (:map imenu-list-major-mode-map ("o" . 'imenu-list-goto-entry))
  :straight t)

(use-package link-hint
  :straight t
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))

;;;; Custom Functions
;; (defun jf/scroll-down-half-page ()
;;   "Scroll down half a page while keeping the cursor centered"
;;   ;; See https://www.reddit.com/r/emacs/comments/r7l3ar/how_do_you_scroll_half_a_page/
;;   (interactive)
;;   (let ((ln (line-number-at-pos (point)))
;;         (lmax (line-number-at-pos (point-max))))
;;     (cond ((= ln 1) (move-to-window-line nil))
;;           ((= ln lmax) (recenter (window-end)))
;;           (t (progn
;;                (move-to-window-line -1)
;;                (recenter))))))

;; (defun jf/scroll-up-half-page ()
;;   "Scroll up half a page while keeping the cursor centered"
;;   ;; See https://www.reddit.com/r/emacs/comments/r7l3ar/how_do_you_scroll_half_a_page/
;;   (interactive)
;;   (let ((ln (line-number-at-pos (point)))
;;         (lmax (line-number-at-pos (point-max))))
;;     (cond ((= ln 1) nil)
;;           ((= ln lmax) (move-to-window-line nil))
;;           (t (progn
;;                (move-to-window-line 0)
;;                (recenter))))))

(provide 'jf-navigating)
;;; jf-navigating.el ends here
