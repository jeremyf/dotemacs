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
  :straight t
  :demand t
  :config
  ;; (defun ii/bmkp-autoname-bookmark-function (position)
  ;;   (let* ((prj (ii/project-current-short-name))
  ;;          (prj-label (if (not (null prj)) (format "[%s] " prj))))
  ;;     (format "👀 %s%s (%d)"
  ;;             prj-label
  ;;             (if (buffer-file-name)
  ;;                 (abbreviate-file-name (buffer-file-name))
  ;;               (buffer-name))
  ;;             (abs position))))
  ;; (customize-set-variable 'bmkp-autoname-format "^👀 .*$")
  ;; (customize-set-value 'bmkp-autoname-bookmark-function
  ;;                      #'ii/bmkp-autoname-bookmark-function)
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

  (add-to-list 'bmkp-default-handlers-for-file-types
    '("^https?:" . browse-url))

  ;; auto-set bookmarks.
  (add-hook 'prog-mode-hook #'bmkp-automatic-bookmark-mode)
  (add-hook 'js-base-mode-hook #'bmkp-automatic-bookmark-mode)
  (setq bmkp-automatic-bookmark-mode-delay 30)

  ;; (set-face-attribute 'bmkp-url nil
  ;;                     :foreground (nord-color "frost-3"))
  ;; (set-face-attribute 'bmkp-bookmark-list nil
  ;;                     :inherit 'default
  ;;                     :background 'unspecified
  ;;                     :foreground (nord-color "frost-1"))
  ;; (set-face-attribute 'bmkp-t-mark nil
  ;;                     :foreground (nord-color "frost-0"))
  ;; (set-face-attribute 'bmkp-heading nil
  ;;                     :height 1.2
  ;;                     :foreground (nord-color "snow-storm-0"))
  ;; (set-face-attribute 'bmkp-local-directory nil
  ;;                     :inherit 'default
  ;;                     :background 'unspecified
  ;;                     :foreground (nord-color "aurora-2"))
  )

(provide 'jf-organizing)
;;; jf-organizing.el ends here
