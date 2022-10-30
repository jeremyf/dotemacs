;;; jf-organizing.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;; Packages specifically here for helping with my writing activities.

;;; Code
(use-package project
  :straight t)

(use-package projectile
  ;; Convenient organization and commands
  :straight t
  :diminish 'projectile-mode
  :config (projectile-mode 1)
  :custom (projectile-project-search-path '("~/git/"))
  :bind ("s-." . projectile-toggle-between-implementation-and-test))

(provide 'jf-organizing)
;;; jf-organizing.el ends here
