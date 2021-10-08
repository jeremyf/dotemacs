;;; jnf-config.el --- Summary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  Provides some configuration information for Emacs.  And some
;;  generally assumed packages that are useful
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package diminish
  :straight t)

;; GCMH does GC when the user is idle.
(use-package gcmh
  :straight t
  :diminish 'gcmh-mode
  :init
  (setq gcmh-idle-delay 5
	gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
	:config (gcmh-mode))

;; Load keychain environment variables
(use-package keychain-environment
  :straight t
  :config
  (keychain-refresh-environment))

;; a modern API for working with files and directories in Emacs.
;; https://github.com/rejeep/f.el/
(use-package f :straight t)

;; The long lost Emacs string manipulation library.
;; https://github.com/magnars/s.el/
(use-package s :straight t)

;; A modern list API for Emacs. No 'cl required.
;; https://github.com/magnars/dash.el/
(use-package dash :straight t)

(defconst jnf/fixed-width-font-name
  "Hack Nerd Font"
  "The name of the fixed width font.
I have it sprinkled through too many places.

Alternatives:
- \"Monaco\"
- \"JetBrains Mono\"
- \"Hack Nerd Font\"")

(defconst jnf/tor-home-directory
  (file-truename "~/git/takeonrules.source")
  "The home directory of TakeOnRules.com Hugo repository.")

(defconst jnf/tor-default-local-hostname
  "http://localhost:1313"
  "The scheme, host name, and port for serving up a local TakeOnRules.com.")

(defvar jnf/data-directories
  (list
    jnf/tor-home-directory
    "~/git/takeonrules.source/themes/hugo-tufte"
    "~/git/burning_wheel_lifepaths/"
    "~/git/jnf-emacs-bookmarks"
    "~/git/dotzshrc/"
    "~/git/dotemacs/"
    "~/git/org/"
    "~/git/org/archive"
    "~/git/org/daily"
    "~/git/org/public"
    "~/git/org/personal"
    "~/git/org/personal/thel-sector"
    "~/git/org/hesburgh-libraries"
    "~/git/org/forem"
    )
  "Relevant data directories for my day to day work.")

(defconst jnf/forem-dashboard-filename
  "~/git/org/forem/dashboard.org"
  "The file to the dashboard documentation and links for Forem.")

(cl-defun jnf/open-dashboard (&key (filename jnf/forem-dashboard-filename))
  "Open the given FILENAME via the bin/dashboard command."
  (interactive)
  (async-shell-command "dashboard"))

(cl-defun jnf/git-data-sync (&optional (directories jnf/data-directories))
  "Synchronize DIRECTORIES with git pull/push.

By default the DIRECTORIES are `jnf/data-directories'"
  (interactive)
  (message "Synchronizing local git repos...")
  (dolist (path directories)
    (if (f-dir-p (file-truename path))
        (progn
          (message (concat "Syncing \"" path "\"..."))
          (shell-command-to-string
           (concat
            "cd " path
            " && git pull --rebase"
            " && git push -u --force-with-lease")))
      (message (concat "Skipping missing directory \"" path "\"...")))
    (message "Finished synchronizing local git repos.")))

;; I use this package to "configure" menus, hence this is in the
;; config section.
(use-package pretty-hydra
  :straight (pretty-hydra
             :type git :host github :repo "jerrypnz/major-mode-hydra.el"
             :files (:defaults (:exclude "major-mode-hydra.el"))))

;; I have found this package quite "helpful"; When I want to know the
;; name of a function or key or variable, I can use the helpful
;; package.
(use-package helpful
  :straight t
  :after (all-the-icons pretty-hydra)
  :pretty-hydra
  ((:title (with-material "help_outline" "Helpful Menus") :quit-key "q" :exit t)
   ("Helpful"
    (
     ("b" embark-bindings "bindings")
     ("c" helpful-command "command")
     ("f" helpful-callable "function (interactive)")
     ("F" helpful-function "function (all)")
     ("k" helpful-key "key")
     ("l" find-library "Find library")
     ("m" helpful-macro "macro")
     ("p" helpful-at-point "thing at point")
     ("v" helpful-variable "variable")
     ("t" describe-text-properties "text properties")
     )))
  :bind ("C-s-h" . helpful-hydra/body))

(provide 'jnf-config.el)
;;; jnf-config.el ends here
