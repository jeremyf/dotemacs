;;; jf-organizing.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;; Packages specifically here for helping with my writing activities.

;;; Code:
(use-package project
  ;; I'm unclear why I have this and projectile declared/required.
  ;;
  ;; TODO: Can I not require this?
  :straight t)

(use-package projectile
  ;; Convenient organization and commands for projects.
  :straight t
  :custom (projectile-project-search-path '("~/git/"))
  ;; (projectile-git-fd-args "-H -0 -E hyrax-webapp -E .git -tf --strip-cwd-prefix -c never")
  ;; (projectile-git-submodule-command "")
  :bind ("s-." . projectile-toggle-between-implementation-and-test)
  :config
  (projectile-mode 1)
  (defun jf/projectile-reset-known-projects ()
    "Reset known projects to `projectile-project-search-path'."
    (interactive)
    (require 's)
    (dolist (proj projectile-known-projects)
      (dolist (search-path projectile-project-search-path)
      (unless (s-starts-with? search-path proj)
        (projectile-remove-known-project proj)))))

(provide 'jf-organizing)
;;; jf-organizing.el ends here
