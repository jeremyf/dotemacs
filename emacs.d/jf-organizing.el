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

  ;; The default relevant `magit-list-repositories'
  ;; The following command shows all "project" directories
  (defvar jf/git-project-paths
    (mapcar (lambda (el) (cons el 1)) projectile-known-projects)
    "An alist of project directories.")

  (dolist (path
            (s-split "\n"
              (s-trim
                (shell-command-to-string "ls ~/git/org/denote/"))))
    (add-to-list 'jf/git-project-paths
      (cons (concat "~/git/org/denote" path) 1)))

  (setq magit-repository-directories jf/git-project-paths))

(use-package bookmark+
  ;; https://www.emacswiki.org/emacs/BookmarkPlus
  ;;
  ;; Enhancements to the built-in Emacs bookmarking feature.
  :straight t
  :demand t
  :config
  ;; Definitely want uniform windowing behavior.
  (define-key bookmark-bmenu-mode-map (kbd "M-o") #'ace-window)

  ;; when this is not set to `nil' explicitly, auto-save bookmarks
  ;; gets itself into an infinite loop attempting to autosave and
  ;; write the custom value to custom-file.el.  this happens only when
  ;; the buffer associated with the bookmark has not been saved. (to
  ;; reproduce the issue, remove the customize-set-value sexp, find a
  ;; new file, and wait 30 seconds; it'll start printing messages like
  ;; mad.  C-g will eventually break the loop.)  i only use one
  ;; bookmark file so this isn't a problem but it really does seem
  ;; like a bmkp bug.
  (customize-set-value 'bmkp-last-as-first-bookmark-file nil)
  (global-set-key (kbd "H-1") #'bookmark-bmenu-list)

  ;; auto-set bookmarks.
  (setq bmkp-automatic-bookmark-mode-delay 30))

(use-package org-bookmark-heading
  ;; Emacs bookmark support for Org-mode.
  ;;
  ;; Without this package, the default bookmarking for an `org-mode'
  ;; file is to use a regular expression (which might include things
  ;; like TODO state).  This approach is precarious.  With this package
  ;; we now store a bookmark to an `org-mode' file as the filename and
  ;; the UUID.  This does not quite solve the precarity in regards to
  ;; `denote' and it's filename convention.
  ;;
  ;; As indicated in the configuration, this is a drop-in no additional
  ;; configuration package.  Super sweet!
  :straight t)

(provide 'jf-organizing)
;;; jf-organizing.el ends here
