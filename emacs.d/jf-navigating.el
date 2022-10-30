;;; jf-navigating.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;;; Code

(use-package ace-window
  :straight t
  :bind (("M-o" . ace-window)))

;; Pick a letter, avy finds all words with that at the beginning of it.  Narrow
;; results from there.
(use-package avy
  :bind (("C-c j" . avy-goto-char))
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

(provide 'jf-navigating)
;;; jf-navigating.el ends here
