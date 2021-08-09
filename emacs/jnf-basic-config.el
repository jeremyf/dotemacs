;;; -*- lexical-binding: t; -*-
;;; jnf-basic-config.el --- Summary
;;
;;; Commentary:
;;
;;  Some basic configurations that should be applicable for all emacs
;;  settings.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A chunk of code that allows me to pass multiple filenames to
;; emacsclient AND open those files in different frames within the
;; same window.(defvar server-visit-files-custom-find:buffer-count)
(defvar server-visit-files-custom-find:buffer-count)
(defadvice server-visit-files
    (around server-visit-files-custom-find
            activate compile)
  "Maintain a counter of visited files from a single client call."
  (let ((server-visit-files-custom-find:buffer-count 0))
    ad-do-it))
(defun server-visit-hook-custom-find ()
  "Arrange to visit the files from a client call in separate windows."
  (if (zerop server-visit-files-custom-find:buffer-count)
      (progn
        (delete-other-windows)
        (switch-to-buffer (current-buffer)))
    (let ((buffer (current-buffer))
          (window (split-window-sensibly)))
      (switch-to-buffer buffer)
      (balance-windows)))
  (setq server-visit-files-custom-find:buffer-count
        (1+ server-visit-files-custom-find:buffer-count)))
(add-hook 'server-visit-hook 'server-visit-hook-custom-find)

(global-so-long-mode)

;; Parenthesis matching is one of the flaws in my Emacs setup as of
;; this writing. I know that there are a lot of options out
;; there—paredit, smartparens, etc.—but I haven’t sat down and really
;; capital-L Learned a better solution than the TextMate-style bracket
;; completion (which Emacs calls, somewhat fancifully, ‘electric’).
;;
;; https://blog.sumtypeofway.com/posts/emacs-config.html
(electric-pair-mode)

(set-frame-font "JetBrains Mono")

;; Don't create lock files.  Emacs drops these files on the file
;; system.  I found this most cumbersome when working in hugo.  The
;; hugo build would file because it tried to handle a lock file.
(setq create-lockfiles nil)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
;; See https://snarfed.org/gnu_emacs_backup_files
;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; Stop ringing any bell
(setq ring-bell-function 'ignore)

;; When you open Emacs, grab all the space on the screen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Hide the icons of the Emacs toolbar
(tool-bar-mode -1)

;; Hide the scroll bar. Let's be clear, I don't use it.
(scroll-bar-mode -1)

;; Instead of typing "yes" or "no" short-circuit to "y" or "n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; The default is 60.  It is rare that I need more than 15 or 20.
;; However in my long use of Jumpcut there have been a few times where
;; I get into the 80s on previous pastes.  Given that the kill ring is
;; searchable, I think a larger value makes a lot of sense.
(setq kill-ring-max 120)

;; Given the number of symlinks, visit the "linked to" file.
(setq vc-follow-symlinks t)

(setq bookmark-default-file "~/git/jnf-emacs-config/bookmarks")

(setq-default indent-tabs-mode nil) ;; Ensure tabs are expanded, not inserted
(setq inhibit-startup-screen t) ;; Don't include the  emacs "start" window
(provide 'jnf-basic-config.el)
;;; jnf-basic-config.el ends here
