;;; jnf-config.el --- Summary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  Provides some configuration information for Emacs.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq jnf/data-directories
      '(
        ;; The themes directory
        "~/git/takeonrules.github.io/themes/hugo-tufte"
        ;; The content directory
        "~/git/takeonrules.github.io/"
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
        "~/git/org/hesburgh-libraries"
        ))

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
