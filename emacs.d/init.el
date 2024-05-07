;;; init.el --- Summary:
  ;;; -*- lexical-binding: t; -*-
;;
;;  Emacs configuration for Jeremy Friesen
;;
  ;;; Commentary:
;;
;;  This is my journey into Emacs.  Let's see where we go!
;;
  ;;; CODE:
(add-to-list 'load-path "~/git/dotemacs/emacs.d")
(setq custom-file (make-temp-file "emacs-custom-"))
(load custom-file :noerror)
(set-frame-parameter nil 'fullscreen 'fullboth)

(require 'jf-launching)
(require 'jf-illuminating)
(require 'jf-navigating)
(require 'jf-fonts-and-iconography)
(require 'jf-windows)
(require 'jf-utility)
(require 'jf-completing)
(require 'jf-coding)
(require 'jf-organizing)
(require 'jf-framing)
(require 'jf-utility)
(require 'jf-writing)
(require 'jf-communicating)
(require 'jf-org-mode)
(require 'jf-denote)
(require 'jf-reading)
(require 'jf-versioning)
(require 'jf-quick-help)
(require 'jf-gaming)
(require 'jf-blogging)
(require 'jf-project)
(require 'jf-menus)
(require 'jf-capf-hacking)
(require 'jf-experiments)
(require 'git-related)
(require 'dig-my-grave)
;; (require 'jf-project-theme-colors)
(load "~/git/dotemacs/emacs.d/random-tables-data.el")

(setq jf/artist-mode-spraycan "🞄⁛◌🞆⊖⊗⁛●◯⦿⬤")
(load (concat user-emacs-directory "hide-comnt.el") :noerror)

(require 'server)
(setq server-client-instructions nil)
(unless (server-running-p)
  (server-start))

(add-hook 'after-init-hook #'jf/enable-indent-for-tab-command)

(setq safe-local-variable-values
  '((eval
      ;; setq-local org-export-with-properties
      ;; '("PRONOUNS" "ALIGNMENT" "BACKGROUND" "DEMEANOR" "ANCESTRY" "KEEPSAKE" "LOCATIONS" "FACTIONS" "ARCHETYPE" "SESSION_DATE" "START_LOCATION"  "CAMPAIGN_START_DATE" "CAMPAIGN_END_DATE" "END_LOCATION")
      (projectile-git-fd-args . "-H -0 -E hyrax-webapp -E .git -tf --strip-cwd-prefix -c never")
      (projectile-git-submodule-command . "")
      (jf/tor-minor-mode . 1)
      (projectile-require-project-root)
      (projectile-git-command . "git ls-files -zco --exclude-from=.projectile.gitignore")
      (org-insert-tilde-language . ruby)
      (org-insert-tilde-language . emacs-lisp)
      (encoding . utf-8))))

(provide 'init)
  ;;; init.el ends here
