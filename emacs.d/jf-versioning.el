;;; jf-versioning.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;;; Code
(use-package git-modes :straight t)
(use-package magit
  :straight t
  :commands (magit-process-git)
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
	  ("δ"        1 magit-repolist-column-flag ())
	  ("⇣"        3 magit-repolist-column-unpulled-from-upstream
	   ((:right-align t)
	    (:help-echo "Upstream changes not in branch")))
	  ("⇡"        3 magit-repolist-column-unpushed-to-upstream
	   ((:right-align t)
	    (:help-echo "Local changes not in upstream")))
	  ("Branch"  25 magit-repolist-column-branch ())
	  ("Path"    99 magit-repolist-column-path ())))

  ;; Have magit-status go full screen and quit to previous
  ;; configuration.  Taken from
  ;; http://whattheemacsd.com/setup-magit.el-01.html#comment-748135498
  ;; and http://irreal.org/blog/?p=2253
  ;; (defadvice magit-status (around magit-fullscreen activate)
  ;;   (window-configuration-to-register :magit-fullscreen)
  ;;   ad-do-it
  ;;   (delete-other-windows))
  ;; (defadvice magit-mode-quit-window (after magit-restore-screen activate)
  ;;   (jump-to-register :magit-fullscreen))
  :config
  ;; (use-package libgit :straight t)
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (defun jf/magit-browse-pull-request ()
    "In `magit-log-mode' open the associated pull request
  at point.

  Assumes that the commit log title ends in the PR #, which
  is the case when you use the Squash and Merge strategy.

  This implementation is dependent on `magit' and `s'."
    (interactive)
    (let* ((beg (line-beginning-position))
	   (end (line-end-position))
	   (summary
	    (buffer-substring-no-properties
	     beg end)))
      (jf/open-pull-request-for :summary summary)))
  (defun jf/git-current-remote-url ()
    "Get the current remote url."
    (s-trim
     (shell-command-to-string
      (concat
       "git remote get-url "
       (format "%s" (magit-get-current-remote))))))
  (cl-defun jf/open-pull-request-for (&key summary)
    "Given the SUMMARY open the related pull request.

  This method assumes you're using Github's Squash and Strategy."
    (let ((remote-url (jf/git-current-remote-url)))
      (save-match-data
	(and (string-match "(\\#\\([0-9]+\\))$" summary)
	     (browse-url
	      (concat
	       ;; I tend to favor HTTPS and the repos end in ".git"
	       (s-replace ".git" "" remote-url)
	       "/pull/"
	       (match-string 1 summary)))))))
  (defun jf/open-pull-request-for-current-line ()
    "For the current line open the applicable pull request."
    (interactive)
    (let ((summary
	   (s-trim
	    (shell-command-to-string
	     (concat "git --no-pager annotate "
		     "-w -L "
		     (format "%s" (line-number-at-pos))
		     ",+1 "
		     "--porcelain "
		     buffer-file-name
		     " | rg \"^summary\"")))))
      (jf/open-pull-request-for :summary summary)))
  :bind (("C-c m" . magit-status)
	 ("C-x g m" . magit-status)
	 ("C-x g f" . magit-file-dispatch)
	 ("C-x g d" . magit-dispatch))
  ;; In other situations I bind s-6 to `git-messenger:popup-message'
  :bind (:map magit-log-mode-map ("C-x g b" . 'jf/magit-browse-pull-request))
  :hook ((with-editor-post-finish-hook . magit-status)))

;; COMMENTED OUT FOR FUTURE REFERENCE
;; (transient-define-prefix jf/magit-aux-commands ()
;;   "My personal auxiliary magit commands."
;;   ["Auxiliary commands"
;;    ("d" "Difftastic Diff (dwim)" jf/magit-diff-with-difftastic)
;;    ("s" "Difftastic Show" jf/magit-show-with-difftastic)])

;; (require 'magit)
;; (transient-append-suffix 'magit-dispatch "!"
;;   '("#" "My Magit Cmds" jf/magit-aux-commands))

;; (define-key magit-status-mode-map (kbd "#") #'jf/magit-aux-commands)

;; With the time machine, travel back and forth through a files history.
(use-package git-timemachine
  :straight (:host github :repo "emacsmirror/git-timemachine"))

;; Show the current git state in the gutter.  As you edit a line in a file
;; track by git, the indicators change to reflect if this is a modification,
;; addition, or deletion.
(use-package git-gutter
  :straight t
  :custom (git-gutter:update-interval 0.25)
  :bind ("C-x g =" . git-gutter:popup-hunk)
  ("C-x g p" . git-gutter:previous-hunk)
  ("C-x g n" . git-gutter:next-hunk)
  :init (global-git-gutter-mode t)
  (setq git-gutter:modified-sign "Δ"
	git-gutter:added-sign "+"
	git-gutter:deleted-sign "-"))

;; Type ~M-x git-link~ and the function pushes the Git forge URL to the kill
;; ring; I’ve configured the URL to use the SHA of the commit of the line on
;; which I called `git-link'.  This is helpful for sharing links with other
;; folks.  I use this /all of the time./ See https://github.com/sshaw/git-link.
(use-package git-link
  :config
  (defun jf/git-browse-to-repository (remote)
    "Open in external browser the current repository's given REMOTE."
    (interactive (list (git-link--select-remote)))
    (git-link-homepage remote)
    (browse-url (car kill-ring)))
  (setq git-link-use-commit t) ;; URL will be SHA instead of branch
  :straight t)

;; Sometimes I want to see more ~git~ information regarding the current line.
;; `git-messenger' provides a popup that shows the information and provides
;; some additional options.
(use-package git-messenger
  :config (setq git-messenger:show-detail t)
  (defun jf/git-messenger-popup ()
    "Open `git-messenger' or github PR.

  With universal argument, open the github PR for current line.

  Without universal argument, open `git-messenger'."
    (interactive)
    (if (equal current-prefix-arg nil) ; no C-u
	(git-messenger:popup-message)
      (jf/open-pull-request-for-current-line)))
  :custom
  (git-messenger:use-magit-popup t)
  :bind (:map git-messenger-map (("p" . 'jf/open-pull-request-for-current-line)
				 ("l" . 'git-link)))
  :bind (("C-x g b" . jf/git-messenger-popup))
  :straight t)

;; When working in code, I want different ways to view the metadata around the
;; code.  This adds a quick annotation to the current line; When did the last
;; person touch this and what was the message.  It's most useful aspect is
;; seeing multiple lines without relying on the blame.
(use-package blamer
  :straight (blamer :host github :repo "Artawower/blamer.el")
  :custom
  ;; Set to 0 because I don’t enable by default.  So I’m in a mindset of show
  ;; me who and when.
  (blamer-idle-time 0.0)
  (blamer-author-formatter "✎ %s ")
  (blamer-datetime-formatter "[%s] ")
  (blamer-commit-formatter "● %s")
  (blamer-min-offset 40)
  (blamer-max-commit-message-length 20))

(use-package savehist
  :init
  (savehist-mode))

;; Write "kill" command inputs to disk.
(use-package savekill :straight t)

;; Provides a UI for undo trees.  I'm not certain what I want to do with this.
(use-package undo-tree
  :straight t
  :bind (("C-z" . undo)
         ("C-s-z" . undo-tree-redo))
  :config
  (global-undo-tree-mode +1))


(provide 'jf-versioning)
;;; jf-versioning.el ends here
