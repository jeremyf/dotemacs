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

(defconst jnf-silence-loading-log t
  "When true log to stdout any ")

;; I have additional files that I require in the emacs directory
(add-to-list 'load-path (expand-file-name "~/git/dotemacs/jnf-emacs-packages"))

;; https://www.reddit.com/r/emacs/comments/mtb05k/emacs_init_time_decreased_65_after_i_realized_the/
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; This preamble is part of straight-use-package My understanding, in
;; reading straight documentation is that it has better load
;; times. However, the configuration options I often see leverage
;; "use-package" which is why most of my package declarations look as
;; they do.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil jnf-silence-loading-log))

(setq straight-repository-branch "develop")

;; I saw that straight loaded use-package to take advantage of the
;; use-package syntax which is often how things are documented.
(straight-use-package 'use-package)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file :noerror)

(load "jnf-basic-config.el" nil jnf-silence-loading-log)
(load "jnf-config.el" nil jnf-silence-loading-log)

(when (file-directory-p "~/git/dotzshrc/symlinks/.hammerspoon/Spoons/editWithEmacs.spoon")
  (load "~/git/dotzshrc/symlinks/.hammerspoon/Spoons/editWithEmacs.spoon/hammerspoon.el" nil jnf-silence-loading-log))

(load "jnf-display.el" nil jnf-silence-loading-log)

;; During loading of init file, disable checking filenames against the list of
;; filetype handlers. This speeds up startup, as otherwise this list would be
;; checked for every loaded .el and .elc file.
(let ((file-name-handler-alist nil))
  ;; Load the remainder of the configuration from the Org configuration file.
  (org-babel-load-file (concat user-emacs-directory "configuration.org")))

;; (load "jnf-vertico.el" nil jnf-silence-loading-log)
(load "jnf-consult.el" nil jnf-silence-loading-log)

(load "jnf-epub.el" nil jnf-silence-loading-log)
(load "jnf-modes.el" nil jnf-silence-loading-log)
(load "darwin-emacs-config.el" nil jnf-silence-loading-log)

;; I want a completion framework, and the 'company-org-block package
;; is enough to say "Yes to company" over the simpler corfu.
;; (load "jnf-company.el" nil jnf-silence-loading-log)
(load "jnf-corfu.el" nil jnf-silence-loading-log)

(load "jnf-in-buffer.el" nil jnf-silence-loading-log)
(load "jnf-macros.el" nil jnf-silence-loading-log)
(load "jnf-git.el" nil jnf-silence-loading-log)
(load "jnf-org.el" nil jnf-silence-loading-log)
(load "jnf-org-roam-v3.el" nil jnf-silence-loading-log)

(load "jnf-spelling.el" nil jnf-silence-loading-log)
(load "jnf-typography.el" nil jnf-silence-loading-log)
(load "jnf-enh-ruby.el" nil jnf-silence-loading-log)
(load "jnf-lsp-mode.el" nil jnf-silence-loading-log)
;; (load "jnf-beancount.el" nil jnf-silence-loading-log)
;; (load "jnf-tabs.el" nil jnf-silence-loading-log)
;; (load "jnf-stars-without-number.el" nil jnf-silence-loading-log)
(load "jnf-elfeed.el" nil jnf-silence-loading-log)
;; (load "jnf-fennel.el" nil jnf-silence-loading-log)
(load "jnf-dired.el" nil jnf-silence-loading-log)
(load "jnf-dice.el" nil jnf-silence-loading-log)
(load "jnf-forem.el" nil jnf-silence-loading-log)
;; (load "jnf-titlecase.el" nil jnf-silence-loading-log)
(load "jnf-blogging.el" nil jnf-silence-loading-log)
(load "jnf-help.el" nil jnf-silence-loading-log)
(load "jnf-menu.el" nil jnf-silence-loading-log)


(diminish 'eldoc-mode)

(provide 'init)
;;; init.el ends here
