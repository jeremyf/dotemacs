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
  ;; My "~/bin/editor" script was causing problems in that it was asking to wait.
  :init (use-package with-editor
          :straight t
          :custom (with-editor-emacsclient-executable (file-truename "~/bin/git_editor")))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function #'magit-restore-window-configuration)
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  :bind (("C-c m" . magit-status)
          ("C-x g m" . magit-status)
          ("C-x g f" . magit-file-dispatch)
          ("C-x g d" . magit-dispatch))
  :hook ((with-editor-post-finish . #'magit-status)))

(use-package auth-source
  :straight (:type built-in)
  :config
  (setq auth-sources (list "~/.authinfo.pgp" "~/.authinfo")))

(use-package forge
  :bind ("C-s-f" . #'forge-dispatch)
  :straight (:host github :repo "magit/forge"))

(use-package gh-notify
  ;; A super-fast overlay of forge notifications (something which I
  ;; haven't previously used).
  :straight t
  :bind (:map gh-notify-mode-map
          ;; C-c C-c is more and more the "do it" command.  So let's
          ;; "Make it so."
          ("C-c C-c" . gh-notify-forge-refresh))
  :config
  (setq gh-notify-exclude-repo-limit
    '("samvera-labs/geomash"
       "samvera-labs/hyku_knapsack"
       "samvera/bulkrax"
       "samvera/hyku"
       "samvera/valkyrie"
       "scientist-softserv/actions"
       "scientist-softserv/adventist-dl"
       "scientist-softserv/adventist_knapsack"
       "scientist-softserv/atla-hyku"
       "scientist-softserv/britishlibrary"
       "scientist-softserv/derivative_rodeo"
       "scientist-softserv/hykuup_knapsack"
       "scientist-softserv/iiif_print"
       "scientist-softserv/palni-palci"
       "scientist-softserv/palni_palci_knapsack"
       "scientist-softserv/utk-hyku"
       "harvard-lts/CURIOSity"
       "WGBH-MLA/ams")))

(use-package git-commit
  :straight t
  :hook ((git-commit-mode . jf/git-commit-mode-setup))
  :bind (:map git-commit-mode-map
          (("TAB" .  #'completion-at-point)))
  :bind ("s-7" . #'jf/insert-task-type-at-point)
  :config
  (defun jf/git-commit-mode-setup ()
    ;; Specify config capf
    (setq fill-column git-commit-fill-column)
    (setq-local completion-at-point-functions
      (cons #'jf/version-control/issue-capf
        (cons #'jf/version-control/project-capf
          completion-at-point-functions)))
    (goto-char (point-min))
    (beginning-of-line-text)
    (when (looking-at-p "^$")
      (jf/insert-task-type-at-point :at (point-min))))

  (defvar jf/version-control/valid-commit-title-prefixes
    '("🎁: feature (A new feature)"
       "🐛: bug fix (A bug fix)"
       "📚: docs (Changes to documentation)"
       "💄: style (Formatting, missing semi colons, etc; no code change)"
       "♻️: refactor (Refactoring production code)"
       "☑️: tests (Adding tests, refactoring test; no production code change)"
       "🧹: chore (Updating build tasks, package manager configs, etc; no production code change)"
       "🛠: build"
       "💸: minting a new version"
       "🔄: revert"
       "🦄: spike (Indicates research task; usually creates more tickets)"
       "☄️: epic (Enumeration of lots of other issues/tasks)"
       "⚙️: config changes"
       "🎬: initial commit or setup of project/component"
       "🚧: work in progress (WIP)"
       "🗡: stab in the dark"
       "🤖: continuous integration (CI) changes")
    "Team 💜 Violet 💜 's commit message guidelines on <2023-05-12 Fri>.")
  (cl-defun jf/insert-task-type-at-point (&key (splitter ":") (padding " ") (at nil))
    "Select and insert task type.

Split result on SPLITTER and insert result plus PADDING.  When
provided AT, insert character there."
    (interactive)
    (let ((commit-type (completing-read "Commit title prefix: "
                         jf/version-control/valid-commit-title-prefixes nil t)))
      (when at (goto-char at))
      (insert (car (s-split splitter commit-type)) padding))))

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
  (setq git-gutter:modified-sign "%"
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
  :bind (:map git-messenger-map (("l" . 'git-link)))
  :bind (("s-6" . jf/git-messenger-popup)
          ("C-x g b" . jf/git-messenger-popup))
  :straight t)

(use-package savehist
  ;; Save my history.
  :config
  (setq savehist-additional-variables '(register-alist kill-ring))
  :init
  (savehist-mode 1))

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
