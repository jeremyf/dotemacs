;;; -*- lexical-binding: t; -*-
;;; jnf-git.el --- Summary
;;
;;; Commentary:
;;
;;  This package includes the various configurations for git
;;  interactions.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The OMG awesome git client for emacs.
(use-package magit
  :straight t
  :init (use-package with-editor :straight t)

  ;; Adding format to git-commit-fill-column of 72 as best practice.
  (setq git-commit-fill-column 72)

  ;; Keeping the summary terse helps with legibility when you run a
  ;; report with only summary.
  (setq git-commit-summary-max-length 50)

  ;; Set the tabular display columns for the `magit-list-repositories'
  (setq magit-repolist-columns
      '(("Name"    25 magit-repolist-column-ident ())
        ("Version" 25 magit-repolist-column-version ())
        ("δ"        1 magit-repolist-column-dirty ())
        ("⇣"        3 magit-repolist-column-unpulled-from-upstream
         ((:right-align t)
          (:help-echo "Upstream changes not in branch")))
        ("⇡"        3 magit-repolist-column-unpushed-to-upstream
         ((:right-align t)
          (:help-echo "Local changes not in upstream")))
        ("Branch"  25 magit-repolist-column-branch ())
        ("Path"    99 magit-repolist-column-path ())))

  ;; The default relevant `magit-list-repositories'
  (setq magit-repository-directories
        `(("~/git/takeonrules.github.io/" . 1) ;; personal
          ("~/git/dotzshrc/" .  1) ;; all
          ("~/git/jnf-emacs-config/" . 1) ;; all
          ("~/git/org" . 1) ;; all
          ("~/git/org/archive" . 1) ;; personal
          ("~/git/org/daily" . 1) ;; ??
          ("~/git/org/hesburgh-libraries" . 1) ;; work
          ("~/git/org/personal" . 1) ;; personal
          ("~/git/org/public" . 1) ;; all
          ("~/git/takeonrules.github.io/themes/hugo-tufte" . 1))) ;; personal

  ;; Have magit-status go full screen and quit to previous
  ;; configuration.  Taken from
  ;; http://whattheemacsd.com/setup-magit.el-01.html#comment-748135498
  ;; and http://irreal.org/blog/?p=2253
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defadvice magit-mode-quit-window (after magit-restore-screen activate)
    (jump-to-register :magit-fullscreen))
  :config
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header))

(defun jnf/magit-list-repositories ()
  "Create a `magit-list-repositories' for my personal repositories."
  (interactive)
  (setq magit-repository-directories
      `(("~/git/takeonrules.github.io/" . 1)
        ("~/git/takeonrules.github.io/hugo-tufte" . 1)
        ("~/git/org" . 1)
        ("~/git/org/personal" . 1)
        ("~/git/org/public" . 1)
        ("~/git/org/archive" . 1)
        ("~/git/org/daily" . 1)
        ("~/git/org/hesburgh-libraries" . 1)
        ("~/git/jnf-emacs-config/" . 1)
        ("~/git/dotzshrc/" .  1)))
  (magit-list-repositories))

(use-package forge
  :straight t)

(use-package libgit
  :straight t)

(use-package magit-libgit
  :after (libgit magit)
  :straight t)

;; With the time machine, travel back and forth through a files history.
;;
;; While visiting a point in time, you can open
(use-package git-timemachine
  :straight t)

;; Show the current git state in the gutter Go ahead and edit a line
;; and look to the gutter for guidance.
(use-package git-gutter-fringe
  :straight (git-gutter-fringe :type git :host github :repo "emacsorphanage/git-gutter-fringe")
  :diminish 'git-gutter-mode
  :config (global-git-gutter-mode 't)
  (setq git-gutter:modified-sign "Δ"
        git-gutter:added-sign "+"
        git-gutter:deleted-sign "-"))

;; https://github.com/sshaw/git-link
;;
;; `M-x git-link` to add the current URL to the kill ring.  This is
;; particularly helpful for sharing links with other developers.  I
;; use this ALL OF THE TIME
(use-package git-link
  :config
  (defun git-link-codeberg (hostname dirname filename branch commit start end)
    (format "https://%s/%s/src/%s/%s"
	    hostname
	    dirname
	    (or branch commit)
            (concat filename
                    (when start
                      (concat "#"
                              (if end
                                  (format "L%s-%s" start end)
                                (format "L%s" start)))))))
  (defun git-link-commit-codeberg (hostname dirname commit)
    (format "https://%s/%s/commit/%s"
	    hostname
	    dirname
	    commit))
  (add-to-list 'git-link-remote-alist '("codeberg.org" git-link-codeberg))
  (add-to-list 'git-link-commit-remote-alist '("codeberg.org" git-link-commit-codeberg))
  (setq git-link-use-commit t) ;; URL will be SHA instead of branch
  :straight t)

(defun jnf/git-browse-to-repository (remote)
  "Open in external browser the current repository's given REMOTE.

Uses `eww-browse-with-external-browser' to determine external browser to use."
  (interactive (list (git-link--select-remote)))
  (git-link-homepage remote)
  (eww-browse-with-external-browser (car kill-ring)))

(message "%s" (git-link-homepage))
(use-package git-messenger
  :config (setq git-messenger:show-detail t)
  :custom (git-messenger:use-magit-popup t)
  :bind (("s-6" . git-messenger:popup-message)
         ("<f6>" . git-messenger:popup-message))
  :straight t)

(provide 'jnf-git.el)
;;; jnf-git.el ends here