;;; jnf-config.el --- Summary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  Provides some configuration information for Emacs.  And some
;;  generally assumed packages that are useful
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a modern API for working with files and directories in Emacs.
;; https://github.com/rejeep/f.el/
(use-package f :straight t)

;; The long lost Emacs string manipulation library.
;; https://github.com/magnars/s.el/
(use-package s :straight t)

;; A modern list API for Emacs. No 'cl required.
;; https://github.com/magnars/dash.el/
(use-package dash :straight t)

(defconst jnf/tor-home-directory
  (file-truename "~/git/takeonrules.github.io")
  "The home directory of TakeOnRules.com Hugo repository.")

(defconst jnf/tor-default-local-hostname
  (file-truename "http://localhost:1313")
  "The scheme, host name, and port for serving up a local TakeOnRules.com.")

(defconst jnf/tor-hostname-regexp
  "https?://takeonrules\.com"
  "A regular expression for checking if it's TakeOnRules.com.")

(defvar jnf/data-directories
  (list
    jnf/tor-home-directory
    ;; The themes directory
    "~/git/takeonrules.github.io/themes/hugo-tufte"
    ;; The tooling directory
    "~/git/dotzshrc/"
    ;; The personal configuration options
    "~/git/jnf-emacs-config/"
    ;; An org directory
    "~/git/org/"
    ;; An org directory
    "~/git/org/archive"
    ;; An org directory
    "~/git/org/daily"
    ;; An org directory
    "~/git/org/public"
    ;; An org directory
    "~/git/org/personal"
    ;; An org directory
    "~/git/org/personal/thel-sector"
    ;; An org directory
    "~/git/org/hesburgh-libraries"
    )
  "Relevant data directories for my day to day work.")

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

(provide 'jnf-config.el)
;;; jnf-config.el ends here
