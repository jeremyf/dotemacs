;;; jf-versioning.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;;; Code:
(use-package git-modes
  ;; A mode for editing gitconfig files.
  :straight t)

(use-package emacsql
  :straight (:host github :repo "magit/emacsql"))

(use-package magit
  ;; A fantastic UI for git commands; the interactive rebase is an absolute
  ;; wonder tool (see
  ;; https://takeonrules.com/2023/01/12/using-the-git-interactive-staging-as-a-moment-to-facilitate-synthesis/).
  ;; Also the progenitor of `transient'
  :straight (:host github :repo "magit/magit")
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
  :preface
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
  :bind (:map magit-log-mode-map ("C-x g b" . #'jf/magit-browse-pull-request))
  :hook ((with-editor-post-finish . #'magit-status)
          (git-commit-mode . (lambda () (setq fill-column git-commit-fill-column)))))

(setq auth-sources (list "~/.authinfo.gpg" 'macos-keychain-internet 'macos-keychain-generic))

(use-package forge
  :after (magit emacsql)
  :straight (:host github :repo "magit/forge"))

(defvar jf/version-control/valid-commit-title-prefixes
  '("🎁: feature (A new feature)"
     "🐛: bug fix (A bug fix)"
     "📚: docs (Changes to documentation)"
     "💄: style (Formatting, missing semi colons, etc; no code change)"
     "♻️: refactor (Refactoring production code)"
     "☑️: tests (Adding tests, refactoring test; no production code change)"
     "🧹: chore (Updating build tasks, package manager configs, etc; no production code change)"
     "🛠: build"
     "🔄: revert"
     "🦄: spike (Indicates research task; usually creates more tickets)"
     "☄️: epic (Enumeration of lots of other issues/tasks)"
     "⚙️: config changes"
     "🎬: initial commit or setup of project/component"
     "🤖: continuous integration (CI) changes")
  ;; The following list was pulled from http://udacity.github.io/git-styleguide/
  ;;
  ;; '("feat: A new feature"
  ;;    "fix: A bug fix"
  ;;    "docs: Changes to documentation"
  ;;    "style: Formatting, missing semi colons, etc; no code change"
  ;;    "refactor: Refactoring production code"
  ;;    "test: Adding tests, refactoring test; no production code change"
  ;;    "chore: Updating build tasks, package manager configs, etc; no production code change")
  "Team 💜 Violet 💜 's commit message guidelines on <2023-05-12 Fri>.")

(cl-defun jf/git-commit-mode-hook (&key (splitter ":") (padding " "))
  "If the first line is empty, prompt for commit type and insert it.

Add PADDING between inserted commit type and start of title.  For
the `completing-read' show the whole message.  But use the
SPLITTER to determine the prefix to include."
  (when (and (eq major-mode 'text-mode)
          (string= (buffer-name) "COMMIT_EDITMSG")
          (local-set-key (kbd "TAB") #'completion-at-point)
          (setq-local completion-at-point-functions
            (cons #'jf/version-control/issue-capf
              (cons #'jf/version-control/project-capf
                completion-at-point-functions)))
          ;; Is the first line empty?
          (save-excursion
            (goto-char (point-min))
            (beginning-of-line-text)
            (looking-at-p "^$")))
    (jf/insert-task-type-at-point :at (point-min))))

(global-set-key (kbd "s-7") #'jf/insert-task-type-at-point)
(cl-defun jf/insert-task-type-at-point (&key (splitter ":") (padding " ") (at nil))
  "Select and insert task type.

Split result on SPLITTER and insert result plus PADDING.  When
provided AT, insert character there."
  (interactive)
  (let ((commit-type (completing-read "Commit title prefix: "
                       jf/version-control/valid-commit-title-prefixes nil t)))
    (when at (goto-char at))
    (insert (car (s-split splitter commit-type)) padding)))

(add-hook 'find-file-hook 'jf/git-commit-mode-hook)

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

(use-package git-timemachine
  ;; With the time machine, travel back and forth through a files history.
  :straight (:host github :repo "emacsmirror/git-timemachine"))

(use-package git-gutter
  ;; Show the current git state in the gutter.  As you edit a line in a file
  ;; track by git, the indicators change to reflect if this is a modification,
  ;; addition, or deletion.
  :straight t
  :custom (git-gutter:update-interval 0.25)
  :bind ("C-x g =" . git-gutter:popup-hunk)
  ("C-x g p" . git-gutter:previous-hunk)
  ("C-x g n" . git-gutter:next-hunk)
  :init (global-git-gutter-mode t)
  (setq git-gutter:modified-sign "Δ"
    git-gutter:added-sign "+"
    git-gutter:deleted-sign "-"))

(use-package git-link
  ;; Type ~M-x git-link~ and the function pushes the Git forge URL to the kill
  ;; ring; I’ve configured the URL to use the SHA of the commit of the line on
  ;; which I called `git-link'.  This is helpful for sharing links with other
  ;; folks.  I use this /all of the time./ See https://github.com/sshaw/git-link.
  :config
  (defun jf/git-browse-to-repository (remote)
    "Open in external browser the current repository's given REMOTE."
    (interactive (list (git-link--select-remote)))
    (git-link-homepage remote)
    (browse-url (car kill-ring)))
  (setq git-link-use-commit t) ;; URL will be SHA instead of branch
  :straight t)

(use-package git-messenger
  ;; Sometimes I want to see more ~git~ information regarding the current line.
  ;; `git-messenger' provides a popup that shows the information and provides
  ;; some additional options.
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
  :bind (("s-6" . jf/git-messenger-popup)
          ("C-x g b" . jf/git-messenger-popup))
  :straight t)

(use-package blamer
  ;; When working in code, I want different ways to view the metadata around the
  ;; code.  This adds a quick annotation to the current line; When did the last
  ;; person touch this and what was the message.  It's most useful aspect is
  ;; seeing multiple lines without relying on the blame.
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
  ;; Save my history.
  :init
  (savehist-mode))

;; (use-package savekill
;;   ;; Write "kill" command inputs to disk.
;;   :straight t)

(use-package undo-tree
  ;; Provides a UI for undo trees.  I'm not certain what I want to do with this.
  :straight t
  :bind (("C-z" . undo)
          ("C-s-z" . undo-tree-redo))
  :custom (undo-tree-history-directory-alist ("." . "~/.emacs.d/undo-tree/"))
  :init
  (unless (f-dir? "~/.emacs.d/undo-tree/") (mkdir "~/.emacs.d/undo-tree/"))
  :config
  (global-undo-tree-mode +1))

(provide 'jf-versioning)
;;; jf-versioning.el ends here
