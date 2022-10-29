;;; jf-illuminating.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;; Packages specifically here for helping with my coding activities.

;;; Code

;; I vascilate between yes and no; but invariably find myself stuck in a
;; recursed buffer.
(setq enable-recursive-minibuffers t)
(use-package recursion-indicator
  :straight t
  :config
  (recursion-indicator-mode))

(global-hl-line-mode)

(provide 'jf-illuminating)
;;; jf-illuminating.el ends here
