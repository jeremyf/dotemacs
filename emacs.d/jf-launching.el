;;; jf-launching.el --- For launching Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:


;; BEGIN Core Configuration
;; I have chosen to adopt \"straight.el\" for my package management.  The fact
;; that it seamlessly works with `use-package' has help me keep my code more
;; organized.

;;; Code:
;;
;; https://www.reddit.com/r/emacs/comments/mtb05k/emacs_init_time_decreased_65_after_i_realized_the/
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; This preamble is part of straight-use-package My understanding, in
;; reading straight documentation is that it has better load
;; times. However, the configuration options I often see leverage
;; "use-package" which is why most of my package declarations look as
;; they do.
(defvar bootstrap-version)
(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
       (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-repository-branch "develop")
(straight-use-package 'use-package)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  ;; https://xenodium.com/trying-out-gccemacs-on-macos/
  :straight t
  :config
  (exec-path-from-shell-initialize)
  (if (and (fboundp 'native-comp-available-p)
        (native-comp-available-p))
    (progn
      (message "Native comp is available")
      ;; Using Emacs.app/Contents/MacOS/bin since it was compiled with
      ;; ./configure --prefix="$PWD/nextstep/Emacs.app/Contents/MacOS"
      (add-to-list 'exec-path (concat invocation-directory "bin") t)
      (setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
                               (when (getenv "LIBRARY_PATH")
                                 ":")
                               ;; This is where Homebrew puts gcc libraries.
                               (car (file-expand-wildcards
                                      "/opt/homebrew/lib/gcc/*"))))
      ;; Only set after LIBRARY_PATH can find gcc libraries.
      (setq comp-deferred-compilation t))
    (message "Native comp is *not* available")))

;; These are some general configurations that I’ve slowly accumulated.  There’s
;; inline documentation in most cases.  There might be little bits worth
;; teasing out but for the most part, you can move along and reference this
;; later.

(setq user-full-name "Jeremy Friesen"
  user-mail-address "jeremy@jeremyfriesen.com")
(defconst jf/github-username "jeremyf"
  "My username on github.")

(defconst jf/silence-loading-log t
  "When t log to stdout load messages from this configuration.

     In a previous iteration, I loaded lots of separate '*.el' files.
     This flag allowed me to more easily troubleshoot those load
     attempts.")

(make-directory "~/.emacs.d/autosaves/" t) ;; Ensuring I have an autosave
;; directory.
(recentf-mode 1) ;; Track recent

;; Quietly save the recent file list every 10 minutes.
(run-at-time nil 600 (lambda () (let ((save-silently t)) (recentf-save-list))))
(global-auto-revert-mode)

(setq-default fill-column 80)
(setq-default cursor-type 'bar) ;; Doing a bit of configuration of my cursors
(blink-cursor-mode t)

(when (executable-find "rg")
  (setq grep-program "rg"))

(setq
  ;; Don't delink hardlinks
  backup-by-copying t

  backup-directory-alist '((".*" . "~/.emacs.d/backups/"))

  bookmark-default-file "~/emacs-bookmarks.el"

  bookmark-save-flag 1

  ;; I may as well trust themes.
  custom-safe-themes t

  ;; Don't create lock files.  It's only me on this maching.
  create-lockfiles nil

  ;; Instead of delete being gone forever, throw it in the trashbin which I must
  ;; take out
  delete-by-moving-to-trash t

  ;; Automatically delete excess backups
  delete-old-versions t

  ;; https://www.reddit.com/r/emacs/comments/102y0n4/weekly_tips_tricks_c_thread/
  dired-dwim-target t

  echo-key-strokes 0.2

  global-mark-ring-max 32

  help-window-select t

  ;; Show index and count of search results
  isearch-lazy-count t
  lazy-count-prefix-format "(%s/%s) "

  ;; Slow down the UI being updated to improve performance
  idle-update-delay 0.5

  ;; Ensure tabs are expanded, not inserted
  indent-tabs-mode nil

  ;; Don't include the  emacs "start" window
  inhibit-startup-screen t

  ;; how many of the newest versions to keep
  kept-new-versions 20

  ;; and how many of the old
  kept-old-versions 5

  ;; Set a generous kill ring size.
  kill-ring-max 120

  ;; Favor new bit code
  load-prefer-newer t

  ;; Increase read size per process
  read-process-output-max (* 6 512 1024)

  recentf-max-menu-items 50

  recentf-max-saved-items 200

  require-final-newline t

  ;; Make regular Isearch interpret empty space as regular expression
  search-whitespace-regexp ".*?"

  ;; Type C-u C-SPC to pop the mark, then C-SPC to pop again.
  ;; Without this variable, it's C-u C-SPC everytime
  set-mark-command-repeat-pop t

  show-trailing-whitespace t

  ;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
  switch-to-buffer-obey-display-actions t

  ;; https://github.com/maryrosecook/emacs/blob/6ef574e27f33f08a81b26970b5fb9b4c9c1f9eff/init.el#L745
  ;; make emacs only add vertical split panes
  ;; split-height-threshold 99999999999999999
  ;; Follow symlinks instead of prompting.
  vc-follow-symlinks t

  ;; Use version numbers on backups
  version-control t

  ;; Recommendation from https://protesilaos.com/emacs/modus-themes
  x-underline-at-descent-line t

  ;; Exposing one additional modifier key.
  ns-right-command-modifier 'hyper
  ns-right-alternate-modifier 'meta
  line-move-visual t)

(setq ediff-keep-variants nil)
(setq ediff-make-buffers-readonly-at-startup nil)
(setq ediff-merge-revisions-with-ancestor t)
(setq ediff-show-clashes-only t)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(bind-key "H-s" 'save-buffer)
;; With subword-mode, HelloWorld is two words for navigation.
(global-subword-mode)


;; When you open Emacs, grab all the space on the screen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(tool-bar-mode -1) ;; Hide the icons of the Emacs toolbar
(scroll-bar-mode -1) ;; Hide the scroll bar. Let's be clear, I don't use it.
(defalias 'yes-or-no-p 'y-or-n-p) ;; Always "y" or "n" for yes/no

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(add-function :after after-focus-change-function
  (defun jf/garbage-collect-maybe ()
    (unless (frame-focus-state)
      (garbage-collect))))

;; And I’m going to disable a few key bindings.  These were always messing me
;; up a bit.  Also enable a few that I find helpful.  (I’ll enable a lot more
;; later).
(unbind-key "C-z") ;; `suspend-frame'
(unbind-key "C-c o") ;; was bound to open a file externally
(unbind-key "C-x C-c") ;; was `save-buffers-kill-terminal'

(keymap-global-set "M-<delete>" 'kill-word)
(keymap-global-set "s-<down>" 'end-of-buffer)
(keymap-global-set "s-<up>" 'beginning-of-buffer)
(keymap-global-set "s-q" 'save-buffers-kill-terminal)
(keymap-global-set "s-w" 'kill-current-buffer)
(keymap-global-set "C-x C-b" 'ibuffer)
(keymap-global-set "M-RET" 'newline-and-indent)

(use-package dired
  :straight (:type built-in)
  :custom (dired-listing-switches "-laGhpX")
  (dired-use-ls-dired t)
  :hook (dired-mode . dired-hide-details-mode))

(with-eval-after-load 'dired
  ;; Bind dired-x-find-file.
  (setq dired-x-hands-off-my-keys nil)
  (require 'dired-x))


(use-package minions
  :straight t
  :custom (minions-prominent-modes '(flymake-mode))
  :config (minions-mode 1))

(use-package gcmh
  ;; *Gcmh* does garbage collection (GC) when the user is idle.
  :straight t
  :init
  (setq gcmh-idle-delay 5
    gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
  :config (gcmh-mode))

;;; Connective Tissue and oddity functions:
(defvar server-visit-files-custom-find:buffer-count
  "A counter for assisting with opening multiple files via a single
    client call.")

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


(defconst jf/tor-home-directory
  (file-truename "~/git/takeonrules.source")
  "The home directory of TakeOnRules.com Hugo repository.")

(defconst jf/tor-hostname-default-local
  "http://localhost:1313"
  "The scheme, host name, and port for serving up a local TakeOnRules.com.")

(defconst jf/tor-hostname-default-remote
  "https://takeonrules.com"
  "The scheme and host name for TakeOnRules.com.")

(defvar jf/tor-hostname-current
  jf/tor-hostname-default-local
  "What is the current hostname for serving TakeOnRules content.")

;; https://www.reddit.com/r/emacs/comments/112t0uo/comment/ja41lso/?utm_source=share&utm_medium=web2x&context=3
(require 'info)
(info-initialize)
(push "/opt/homebrew/share/info" Info-directory-list)

(provide 'jf-launching)
;;; jf-launching.el ends here
