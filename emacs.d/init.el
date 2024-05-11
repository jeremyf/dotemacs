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

;; https://www.reddit.com/r/emacs/comments/mtb05k/emacs_init_time_decreased_65_after_i_realized_the/
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; This preamble is part of straight-use-package My understanding, in
;; reading straight documentation is that it has better load
;; times. However, the configuration options I often see leverage
;; "use-package" which is why most of my package declarations look as
;; they do.
(defvar bootstrap-version nil)
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

;; *Gcmh* does garbage collection (GC) when the user is idle.
(use-package gcmh
  :straight t
  :init
  (setq gcmh-idle-delay 5
    gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
  :config (gcmh-mode)
  (add-function :after after-focus-change-function
    (defun jf/garbage-collect-maybe ()
      (unless (frame-focus-state)
        (garbage-collect)))))

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

(defconst jf/silence-loading-log t
  "When t log to stdout load messages from this configuration.

     In a previous iteration, I loaded lots of separate '*.el' files.
     This flag allowed me to more easily troubleshoot those load
     attempts.")
 ;; Ensuring I have an autosave directory.
(make-directory "~/.emacs.d/autosaves/" t)

(use-package recentf
  :straight (:type built-in)
  :config
  (setq recentf-max-menu-items 50
    recentf-max-saved-items 200)
  ;; Track recent
  (recentf-mode 1)
  ;; Quietly save the recent file list every 10 minutes.
  (run-at-time nil 600 (lambda ()
                         (let ((save-silently t))
                           (recentf-save-list)))))

(use-package autorevert
  :straight (:type built-in)
  :config
  (global-auto-revert-mode))

(setq-default fill-column 80)
;; Doing a bit of configuration of my cursors
(setq-default cursor-type 'bar)
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

  require-final-newline t

  ;; Make regular Isearch interpret empty space as regular expression
  ;; search-whitespace-regexp ".*?"

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
  line-move-visual t)

(use-package emacs
  :straight (:type built-in)
  :config
  ;; The following function facilitates a best of both worlds.  By
  ;; default, I want Option to be Meta (e.g. \"M-\") in Emacs.
  ;; However, I can toggle that setting.  That way if I need an umlaut
  ;; (e.g., \"¨\"), I can use MacOS’s native functions to type \"⌥\" +
  ;; \"u\".
  ;;
  ;; I like having MacOS’s native Option (e.g. =⌥=) modifier
  ;; available.  But using that default in Emacs would be a
  ;; significant hinderance.
  (defun jf/toggle-osx-alternate-modifier ()
    "Toggle native OS-X Option modifier setting.

    See `ns-alternate-modifier'."
    (interactive)
    (if ns-alternate-modifier
      (progn (setq ns-alternate-modifier nil)
        (message "Enabling OS X native Option modifier"))
      (progn (setq ns-alternate-modifier 'meta)
        (message "Disabling OX X native Option modifier (e.g. Option as Meta)"))))
  ;; Exposing one additional modifier key.
  (setq ns-right-command-modifier 'hyper
    ns-right-alternate-modifier 'meta))

(use-package ediff
  :straight (:type built-in)
  :config
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

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
  :config
  ;; When two dired buffers are open and you mark then rename a file, it
  ;; assume's you're moving the file from the one buffer to the other.
  ;; Very useful.
  (setq dired-dwim-target t)
  (with-eval-after-load 'dired
    ;; Bind dired-x-find-file.
    (setq dired-x-hands-off-my-keys nil)
    (require 'dired-x))
  :hook (dired-mode . dired-hide-details-mode))

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
(use-package info
  :straight (:type built-in)
  :config
  (info-initialize)
  (push "/opt/homebrew/share/info" Info-directory-list))

(use-package expand-region
  ;; A simple package that does two related things really well; expands and
  ;; contracts the current region.  I use this all the time.
  ;;
  ;; In writing, with the cursor at point, when I expand it selects the word.
  ;; The next expand the sentence, then paragraph, then page.  In programming it
  ;; leverages sexp.
  :straight (:host github :repo "jeremyf/expand-region.el")
  :bind (("C-=" . er/expand-region)
          ("C-+" . er/contract-region)))

(use-package display-fill-column-indicator
  ;; It's nice to have a gentle reminder showing me the recommended column width
  ;; for the current buffer.
  :straight (:type built-in)
  :hook (prog-mode . display-fill-column-indicator-mode))

(use-package kind-icon
  ;; This packages helps provide additional icons for functions and variables in
  ;; the completion candidates.
  :straight t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as
                                        ; `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and
                                        ; background colors ("blended")?
  (kind-icon-blend-frac 0.08)
  ;; directory that defaults to the `user-emacs-directory'. Here, I change that
  ;; directory to a location appropriate to `no-littering' conventions, a
  ;; package which moves directories of other packages to sane locations.
  ;; (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ;
  ;; Change cache dir
  :config
  ;; Enable-Recursive-Minibuffers `kind-icon'
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package lin
  ;;  “LIN locally remaps the hl-line face to a style that is optimal for major
  ;;  modes where line selection is the primary mode of interaction.”  In
  ;;  otherwords, ~lin.el~ improves the highlighted line behavior for the
  ;;  competing contexts.
  :straight (lin :host gitlab :repo "protesilaos/lin")
  :init (global-hl-line-mode)
  :config (lin-global-mode 1)
  (setq lin-face 'lin-blue))

(use-package pulsar
  ;; A little bit of visual feedback.  See
  ;; https://protesilaos.com/codelog/2022-03-14-emacs-pulsar-demo/
  :straight (pulsar :host gitlab :repo "protesilaos/pulsar")
  :hook
  (consult-after-jump . pulsar-recenter-top)
  (consult-after-jump . pulsar-reveal-entry)
  ;; integration with the built-in `imenu':
  (imenu-after-jump . pulsar-recenter-top)
  (imenu-after-jump . pulsar-reveal-entry)
  :config
  (pulsar-global-mode 1)
  (setq pulsar-face 'pulsar-magenta
    pulsar-delay 0.05)
  (setq ring-bell-function 'jf/pulse)
  :preface
  (defun jf/pulse (&optional parg)
    "Pulse the current line.

  If PARG (given as universal prefix), pulse between `point' and `mark'."
    (interactive "P")
    (if (car parg)
      (pulsar--pulse nil nil (point) (mark))
      (pulsar-pulse-line)))
  :bind (("C-c C-l" . jf/pulse)))

(use-package rainbow-mode
  ;; When I toggle on Rainbow mode, it colorizes the text that is color names
  ;; and hex declarations (e.g. "#0000ff" ).  Most useful when working with CSS,
  ;; but sometimes non-CSS files have those declarations as well.
  :straight t)

(use-package rainbow-delimiters
  ;; A quick and useful visual queue for paranthesis.
  :straight t
  :hook ((prog-mode) . rainbow-delimiters-mode))

(use-package recursion-indicator
  ;; I vascilate between yes and no; but invariably find myself stuck in a
  ;; recursed buffer.
  :straight t
  :config
  (setq enable-recursive-minibuffers t)
  (recursion-indicator-mode))

(use-package vi-tilde-fringe
  ;; Show tilde (e.g. ~\~~) on empty trailing lines.  This is a feature ported
  ;; from https://en.wikipedia.org/wiki/Vi
  :straight t
  :config (global-vi-tilde-fringe-mode))

(use-package whole-line-or-region
  ;; From the package commentary, “This minor mode allows functions to operate
  ;; on the current line if they would normally operate on a region and region
  ;; is currently undefined.”  I’ve used this for awhile and believe it’s not
  ;; baked into my assumptions regarding how I navigate Emacs.
  :straight t
  :config (whole-line-or-region-global-mode))

(use-package keycast
  ;; When I turn on `keycast-mode-line' each key press will echo in the
  ;; mode-line.  There are other options for logging which could be conceptually
  ;; useful for feeding a macro.
  :straight t)

(use-package vc-git
  :straight (:type built-in))

(use-package keycast
  :straight t
  :init
  (setq keycast-mode-line-insert-after
    'jf/mode-line-format/buffer-name-and-status)
  (setq keycast-mode-line-remove-tail-elements nil)
  (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
  (setq keycast-mode-line-format "%2s%k%2s(%c%R)"))

(use-package emacs
  :straight (:type built-in)
  :preface

  (defvar-local jf/mode-line-format/kbd-macro
    '(:eval
       (when (and (mode-line-window-selected-p) defining-kbd-macro)
         (propertize " KMacro " 'face 'mode-line-highlight))))

  (defvar-local jf/mode-line-format/buffer-name-and-status
    '(:eval
       (let ((name (buffer-name)))
         (propertize
           (if buffer-read-only
             ;; TODO: <2024-03-09 Sat> Create mouse clickability to review
             ;; filename
             (format " %s %s " (char-to-string #xE0A2) name)
             name)
           'face
           (if (mode-line-window-selected-p)
             'mode-line-buffer-id
             'mode-line-inactive)))))

  (defun jf/mode-line-format/major-mode-indicator ()
    "Return appropriate propertized mode line indicator for the major mode."
    (let ((indicator (cond
                       ((derived-mode-p 'text-mode) "§")
                       ((derived-mode-p 'prog-mode) "λ")
                       ((derived-mode-p 'comint-mode) ">_")
                       (t "◦"))))
      (propertize indicator
        'face
        (if (mode-line-window-selected-p)
          'jf/mode-line-format/face-shadow
          'mode-line-inactive))))

  (defun jf/mode-line-format/major-mode-name ()
    (propertize (capitalize (string-replace "-mode" "" (symbol-name major-mode)))
      'face (if (mode-line-window-selected-p) 'mode-line 'mode-line-inactive)))

  (defvar-local jf/mode-line-format/major-mode
    '(:eval
       (concat
         (jf/mode-line-format/major-mode-indicator)
         " "
         (jf/mode-line-format/major-mode-name))))

  (defvar-local jf/mode-line-format/narrow
    '(:eval
       (when (and (mode-line-window-selected-p)
               (buffer-narrowed-p)
               (not (derived-mode-p
                      'Info-mode
                      'help-mode
                      'special-mode
                      'message-mode)))
         (propertize " Narrow " 'face 'mode-line-highlight))))

  (defvar jf/mode-line-format/vterm-map
    (let ((map (make-sparse-keymap)))
      (define-key map [mode-line down-mouse-1] #'vterm-copy-mode)
      map)
    "Keymap to display on `vterm' copy indicator.")

  (defvar-local jf/mode-line-format/vterm
    '(:eval
       (when (derived-mode-p 'vterm-mode)
         (propertize
           (concat " " (if vterm-copy-mode "©" "O") " ")
           'face 'mode-line-highlight
           'local-map jf/mode-line-format/vterm-map
           'help-echo "mouse-1:vterm-copy-mode"))))

  (defvar-local jf/mode-line-format/misc-info
    '(:eval
       (when (mode-line-window-selected-p)
         mode-line-misc-info)))

  (with-eval-after-load 'eglot
    (setq mode-line-misc-info
      (delete '(eglot--managed-mode
                 (" [" eglot--mode-line-format "] "))
        mode-line-misc-info)))

  (defvar-local jf/mode-line-format/eglot
    `(:eval
       (when (and (featurep 'eglot) (mode-line-window-selected-p))
         '(eglot--managed-mode eglot--mode-line-format))))

  (defvar-local jf/mode-line-format/vc-branch
    '(:eval
       (when-let* (((mode-line-window-selected-p))
                    (file (if (equal major-mode 'dired-mode)
                            default-directory
                            (buffer-file-name)))
                    (backend (or (vc-backend file) 'git))
                    (branch (jf/mode-line-format/vc-branch-name file backend)))
         (jf/mode-line-format/vc-details file branch))))

  (defface jf/mode-line-format/face-shadow
    '((t :foreground "#d0ffe0" :inherit shadow))
    "A face for symbols in the `mode-line'.")

  (defun jf/mode-line-format/vc-details (file branch)
    "Return the FILE and BRANCH."
    (propertize
      (concat
        ;; 
        (propertize "" ;; (char-to-string #xE0A0)
          'face
          'jf/mode-line-format/face-shadow)
        " "
        branch)
      'local-map jf/mode-line-format/map-vc
      'help-echo "mouse-1: magit-status"))

  (defvar jf/mode-line-format/map-vc
    (let ((map (make-sparse-keymap)))
      (define-key map [mode-line down-mouse-1] #'magit-status)
      map)
    "Keymap to display on version control indicator.")

  (defun jf/mode-line-format/vc-branch-name (file backend)
    "Return VC branch name for FILE with BACKEND."
    (when-let ((rev (vc-working-revision file backend))
                (branch (or (vc-git--symbolic-ref file)
                          (substring rev 0 7))))
      branch))

  (defvar-local jf/mode-line-format/flymake
    '(:eval
       (when (and flymake--state
               (mode-line-window-selected-p))
         flymake-mode-line-format)))

  (defvar-local jf/mode-line-format/project
    '(:eval
       (when (projectile-project-p)
         (propertize
           (concat " " (project-name (project-current)))
           'face
           (if (mode-line-window-selected-p)
             'jf/mode-line-format/face-shadow
             'mode-line-inactive)))))

  (dolist (construct '(
                        jf/mode-line-format/buffer-name-and-status
                        jf/mode-line-format/eglot
                        jf/mode-line-format/flymake
                        jf/mode-line-format/kbd-macro
                        jf/mode-line-format/major-mode
                        jf/mode-line-format/misc-info
                        jf/mode-line-format/narrow
                        jf/mode-line-format/project
                        jf/mode-line-format/vc-branch
                        jf/mode-line-format/vterm
                        ))
    (put construct 'risky-local-variable t))

  (setq-default mode-line-format
    '("%e" " "
       jf/mode-line-format/vterm
       jf/mode-line-format/kbd-macro
       jf/mode-line-format/narrow
       jf/mode-line-format/buffer-name-and-status "  "
       jf/mode-line-format/major-mode "  "
       jf/mode-line-format/project "  "
       jf/mode-line-format/vc-branch "  "
       jf/mode-line-format/flymake "  "
       jf/mode-line-format/eglot "  "
       ;; jf/mode-line-format/misc-info
       )))

(use-package ace-window
  ;; Quick navigation from window to window.
  :straight t
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("C-x o" . ace-window)
          ("M-o" . ace-window)))

(use-package avy
  ;; Pick a letter, avy finds all words with that at the beginning of it.  Narrow
  ;; results from there.
  :bind (("C-j" . avy-goto-char-timer))
  :straight t
  :config
  (setq avy-dispatch-alist
    '((?. . jf/avy-action-embark)
       (?x . avy-action-kill-move)
       (?X . avy-action-kill-stay)
       (?y . avy-action-yank)
       (?Y . jf/avy-action-yank-whole-line)
       (?w . avy-action-copy)
       (?W . jf/avy-action-copy-whole-line)
       (?k . avy-action-kill)
       (?K . jf/avy-action-kill-whole-line)
       (?t . avy-action-teleport)
       (?T . jf/avy-action-teleport-whole-line)))
  :config
  ;; https://karthinks.com/software/avy-can-do-anything/#remembering-to-avy
  (defun jf/avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
      (cdr
        (ring-ref avy-ring 0)))
    t)

  (defun jf/avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
      (cdr
        (ring-ref avy-ring 0)))
    t)

  (defun jf/avy-action-yank-whole-line (pt)
    (jf/avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (defun jf/avy-action-teleport-whole-line (pt)
    (jf/avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (defun jf/avy-action-embark (pt)
    (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
      (select-window
        (cdr (ring-ref avy-ring 0))))
    t))

(use-package browse-at-remote
  ;; Because I sometimes want to jump to the source code.  And in looking at
  ;; this I learned about vc-annotate; a better blame than what I've had before.
  ;; `bar-browse' is faster than `browse-at-remote'.
  :straight t
  :bind
  ;; Note this is in the same prefix space as `link-hint'
  ("C-c l r" . browse-at-remote)
  ("C-c l a" . vc-annotate)
  ("C-c l n" . jf/project/jump-to/notes)
  ("C-c l t" . git-timemachine))

(use-package imenu-list
  ;; Show an outline summary of the current buffer.
  :custom (imenu-list-focus-after-activation t)
  (imenu-list-size 0.4)
  (imenu-list-position 'right)
  :bind ("s-4" . 'imenu-list-smart-toggle)
  :bind (:map imenu-list-major-mode-map ("o" . 'imenu-list-goto-entry))
  :straight t)

(use-package link-hint
  ;; I use this more and more and more.  Invoking `link-hint-open-link'
  ;; highlights the visible URLs, providing quick keys to then open those URLs.
  ;; If there's only one candidate, the function opens that URL.
  :straight t
  :bind
  ;; Note this is in the same prefix space as `browse-at-remote'
  ("C-c l o" . jf/link-hint-open-link)
  ("C-c l c" . link-hint-copy-link)
  :config
  (defun jf/link-hint-open-link (prefix)
    "Hint at then browse a URL.

When given PREFIX use `eww-browse-url'."
    (interactive "P")
    (let ((browse-url-browser-function
            (if prefix #'eww-browse-url browse-url-browser-function)))
      (link-hint-open-link))))


(use-package fontaine
  ;; A narrow focus package for naming font configurations and then selecting
  ;; them.
  :straight t
  :config
  (setq fontaine-presets
    ;; I'm naming the presets as "actions"; the mindset that I'm using when
    ;; wanting that font.
    '((smallest
        :default-height 100)
       (smaller
         :default-height 120)
       (default
         :default-height 130)
       (bigger
         :default-height 160)
       (coding
         :default-family "IntoneMono Nerd Font Mono"
         :default-weight light
         :bold-weight medium
         :default-height 130)
       (biggest
         :default-weight light
         :default-height 220
         :bold-weight bold)
       (reading
         :default-weight semilight
         :default-family "ETBembo"
         :default-height 220
         :bold-weight bold)
       (t
         :default-family "IntoneMono Nerd Font Mono"
         :default-weight light
         :default-height 130
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :fixed-pitch-serif-family nil ; falls back to :default-family
         :fixed-pitch-serif-weight nil ; falls back to :default-weight
         :fixed-pitch-serif-height 1.0
         :variable-pitch-family "IntoneMono Nerd Font Propo"
         :variable-pitch-weight nil
         :variable-pitch-height 1.0
         :bold-family nil ; use whatever the underlying face has
         :bold-weight medium
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)))
  (fontaine-set-preset 'default))

;;;; Icons

(use-package nerd-icons
  :straight t
  :config (setq nerd-icons-font-family "IntoneMono Nerd Font Mono"))

(use-package all-the-icons
  ;; It's nice to see icons as a quick visual helper.
  :straight t
  ;; :config
  ;; (cl-defmacro jf/all-the-icons--with(&key name)
  ;;   "A macro to provide functions for icon names."
  ;;   (let ((defun-fn (intern (concat "jf/all-the-icons--with-" name)))
  ;;          (icon-fn (intern (concat "all-the-icons-" name)))
  ;;          (docstring (concat
  ;;                       "Displays an ICON from `all-the-icons-" name "'.")))
  ;;     `(defun ,defun-fn (icon str &optional height v-adjust)
  ;;        ,docstring
  ;;        (s-concat (,icon-fn
  ;;                    icon
  ;;                    :v-adjust (or v-adjust 0)
  ;;                    :height (or height 1))
  ;;          " " str))))
  ;; (jf/all-the-icons--with :name "faicon")
  ;; (jf/all-the-icons--with :name "material")
  ;; (jf/all-the-icons--with :name "octicon")
  ;; (jf/all-the-icons--with :name "alltheicon")
  )

(use-package all-the-icons-dired
  ;; Incorporates file icons with file listings of dired.  /Note/: On 2021-04-11
  ;; I was getting the following error with this package: "*ERROR*: Symbol's
  ;; value as variable is void: file"
  :straight t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;;;; Typography
(use-package typopunct
  ;; A package that provides some automatic replacement of strings of keys.  For
  ;; example in text-mode, when I type three periods (e.g. “.”) typopunct
  ;; replaces that with an ellipsis (e.g. “…”)
  :straight t
  :custom (typopunct-buffer-language 'english)
  :config
  (add-hook 'org-mode-hook 'jf/typopunct-init)
  (defun jf/typopunct-init ()
    (require 'typopunct)
    (typopunct-change-language 'english)
    (typopunct-mode 1))
  ;; To insert a typographical ellipsis sign (…) on three consecutive
  ;; dots, or a middle dot (·) on ‘^.’
  (defconst typopunct-ellipsis (decode-char 'ucs #x2026))
  (defconst typopunct-middot   (decode-char 'ucs #xB7)) ; or 2219
  (defun typopunct-insert-ellipsis-or-middot (arg)
    "Change three consecutive dots to a typographical ellipsis mark."
    (interactive "p")
    (cond
      ((and (= 1 arg)
         (eq (char-before) ?^))
        (delete-char -1)
        (insert typopunct-middot))
      ((and (= 1 arg)
         (eq this-command last-command)
         (looking-back "\\.\\." 1))
        (replace-match "")
        (insert typopunct-ellipsis))
      (t
        (self-insert-command arg))))
  (define-key typopunct-map "." 'typopunct-insert-ellipsis-or-middot)
  ;; feet, arcminutes, derivatives
  (defconst typopunct-prime  (decode-char 'ucs #x2032))
  ;; inches, arcseconds, double derivatives
  (defconst typopunct-dprime (decode-char 'ucs #x2033))
  (defconst typopunct-tprime (decode-char 'ucs #x2034))
  ;; The minus sign (−) is separate from the hyphen (-), en dash (–) and
  ;; em dash (—). To build upon the clever behavior of the ‘-’ key
  (defconst typopunct-minus (decode-char 'ucs #x2212))
  (defconst typopunct-pm    (decode-char 'ucs #xB1))
  (defconst typopunct-mp    (decode-char 'ucs #x2213))
  (defadvice typopunct-insert-typographical-dashes
    (around minus-or-pm activate)
    (cond
      ((or (eq (char-before) typopunct-em-dash)
         (looking-back "\\([[:blank:]]\\|^\\)\\^" 2))
        (delete-char -1)
        (insert typopunct-minus))
      ((looking-back "[^[:blank:]]\\^" 1)
        (insert typopunct-minus))
      ((looking-back "+/" 1)
        (progn (replace-match "")
          (insert typopunct-pm)))
      (t ad-do-it)))
  (defun typopunct-insert-mp (arg)
    (interactive "p")
    (if (and (= 1 arg) (looking-back "-/" 2))
      (progn (replace-match "")
        (insert typopunct-mp))
      (self-insert-command arg)))
  (define-key typopunct-map "+" 'typopunct-insert-mp)
  (defconst typopunct-times (decode-char 'ucs #xD7))
  (defun typopunct-insert-times (arg)
    "Insert multiplication sign at ARG."
    (interactive "p")
    (if (and (= 1 arg) (looking-back "\\([[:blank:]]\\|^\\)\\^"))
      (progn (delete-char -1)
        (insert typopunct-times))
      (self-insert-command arg)))
  (define-key typopunct-map "x" 'typopunct-insert-times)
  (defadvice typopunct-insert-quotation-mark (around wrap-region activate)
    (let* ((lang (or (get-text-property (point) 'typopunct-language)
                   typopunct-buffer-language))
            (omark (if single
                     (typopunct-opening-single-quotation-mark lang)
                     (typopunct-opening-quotation-mark lang)))
            (qmark (if single
                     (typopunct-closing-single-quotation-mark lang)
                     (typopunct-closing-quotation-mark lang))))
      (cond
        (mark-active
          (let ((skeleton-end-newline nil)
                 (singleo (typopunct-opening-single-quotation-mark lang))
                 (singleq (typopunct-closing-single-quotation-mark lang)))
            (if (> (point) (mark))
              (exchange-point-and-mark))
            (save-excursion
              (while (re-search-forward (regexp-quote (string omark)) (mark) t)
                (replace-match (regexp-quote (string singleo)) nil nil)))
            (save-excursion
              (while (re-search-forward (regexp-quote (string qmark)) (mark) t)
                (replace-match (regexp-quote (string singleq)) nil nil)))
            (skeleton-insert (list nil omark '_ qmark) -1)))
        ((looking-at (regexp-opt (list (string omark) (string qmark))))
          (forward-char 1))
        (t ad-do-it)))))

(use-package window
  :preface (require 'prot-window)
  ;; Wrangle up how windows and buffers display.
  :straight (:type built-in)
  :bind (("s-q" . #'jf/bury-or-unbury-buffer)
          ("C-x 2" . #'jf/window/split-and-follow-below)
          ("C-x 3" . #'jf/window/split-and-follow-right)
          ("s-\\" . #'jf/nav-toggle-split-direction))
  :config
  (advice-add #'kill-buffer-and-window :after #'balance-windows)
  (defun jf/nav-toggle-split-direction ()
    "Toggle window split from vertical to horizontal.
This work the other way around as well.
Credit: https://github.com/olivertaylor/dotfiles/blob/master/emacs/init.el"
    (interactive)
    (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows")
      (let ((was-full-height (window-full-height-p)))
        (delete-other-windows)
        (if was-full-height
          (split-window-vertically)
          (split-window-horizontally))
        (save-selected-window
          (other-window 1)
          (switch-to-buffer (other-buffer))))))

  (defun jf/window/split-and-follow-below ()
    "Split the selected window in two with the new window below.

This uses `split-window-below' but follows with the cursor."
    (interactive)
    (split-window-below)
    (balance-windows)
    (other-window 1))
  (defun jf/window/split-and-follow-right ()
    "Split the selected window in two with the new window to the right.

This uses `split-window-right' but follows with the cursor."
    (interactive)
    (split-window-right)
    (balance-windows)
    (other-window 1))
  (defun jf/body-function/rspec-compilation (window)
    "Select the WINDOW and move to `end-of-buffer'."
    (select-window window)
    (end-of-buffer))
  (setq display-buffer-alist
    `(;; no window
       ("\\`\\*Async Shell Command\\*\\'"
         (display-buffer-no-window))
       ("\\`\\*\\(Warnings\\|Compile-Log\\|Org Links\\)\\*\\'"
         (display-buffer-no-window)
         (allow-no-window . t))
       ;; bottom side window
       ("\\*Org \\(Select\\|Note\\)\\*" ; the `org-capture' key selection and `org-add-log-note'
         (display-buffer-in-side-window)
         (dedicated . t)
         (side . bottom)
         (slot . 0)
         (window-parameters . ((mode-line-format . (" %b")))))
       ;; bottom buffer (NOT side window)
       ((or . ((derived-mode . flymake-diagnostics-buffer-mode)
                (derived-mode . flymake-project-diagnostics-mode)
                (derived-mode . messages-buffer-mode)
                (derived-mode . backtrace-mode)))
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . 0.3)
         (dedicated . t)
         (preserve-size . (t . t)))
       ("\\*Embark Actions\\*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . fit-window-to-buffer)
         (window-parameters . ((no-other-window . t)
                                (mode-line-format . (" %b")))))
       ("\\*\\(Output\\|Register Preview\\).*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom))
       ;; below current window
       ("\\(\\*Capture\\*\\|CAPTURE-.*\\)"
         (display-buffer-reuse-mode-window display-buffer-below-selected))
       ("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . 0.1)
         (dedicated . t)
         (preserve-size . (t . t)))
       ("\\*rspec-compilation\\*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (body-function . jf/body-function/rspec-compilation))
       ((derived-mode . reb-mode) ; M-x re-builder
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . 4) ; note this is literal lines, not relative
         (dedicated . t)
         (preserve-size . (t . t)))
       ((or . ((derived-mode . occur-mode)
                (derived-mode . grep-mode)
                (derived-mode . rg-mode)
                (derived-mode . Buffer-menu-mode)
                (derived-mode . log-view-mode)
                (derived-mode . help-mode) ; See the hooks for `visual-line-mode'
                "\\*\\(|Buffer List\\|Occur\\|vc-change-log\\).*"))
         (prot-window-display-buffer-below-or-pop)
         (dedicated . t)
         (body-function . prot-window-select-fit-size))
       ("\\*\\(Calendar\\|Bookmark Annotation\\|ert\\).*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (dedicated . t)
         (window-height . fit-window-to-buffer))
       ;; NOTE 2022-09-10: The following is for `ispell-word', though
       ;; it only works because I override `ispell-display-buffer'
       ;; with `prot-spell-ispell-display-buffer' and change the
       ;; value of `ispell-choices-buffer'.
       ("\\*ispell-top-choices\\*.*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . fit-window-to-buffer))
       ;; same window
       ;; NOTE 2023-02-17: `man' does not fully obey the
       ;; `display-buffer-alist'.  It works for new frames and for
       ;; `display-buffer-below-selected', but otherwise is
       ;; unpredictable.  See `Man-notify-method'.
       ((or . ((derived-mode . Man-mode)
                (derived-mode . woman-mode)
                "\\*\\(Man\\|woman\\).*"))
         (display-buffer-same-window))))
  (setq confirm-kill-emacs #'yes-or-no-p)
  :config
  ;; For some reason, the C-x 5 0 keybindings don't set in my brain.
  (defun jf/bury-or-unbury-buffer (&optional prefix)
    "Without PREFIX `bury-buffer' a buffer.

With one universal PREFIX, `unbury-buffer'.
With two universal PREFIX `delete-frame'.
With three or more universal PREFIX `save-buffers-kill-emacs'."
    (interactive "p")
    (cond
      ((eq prefix nil)
        (if buffer-read-only (kill-current-buffer) (bury-buffer)))
      ((>= prefix 64)
        (progn
          (let ((save-silently t)) (recentf-save-list))
          (save-buffers-kill-emacs t)))
      ((>= prefix 16)
        (delete-frame))
      ((>= prefix 4)
        (unbury-buffer))
      (t
        (if buffer-read-only (kill-current-buffer) (bury-buffer))))))

(use-package font-lock
  :straight (:type built-in)
  :config
  ;; Show tabs as they are tricky little creatures
  (defface jf/tabs-face
    '((default :inherit font-lock-misc-punctuation-face))
    "Help me see tabs; they are tricky creatures.")
  (defface jf/bom-face
    '((default :inherit font-lock-misc-punctuation-face))
    "Help me see BOM characters \"﻿\"; they are tricky!")

  (add-hook 'prog-mode-hook
    (lambda ()
      (unless (member major-mode '(go-mode go-ts-mode))
        (font-lock-add-keywords nil '(("\t" . 'jf/tabs-face)))
        (font-lock-add-keywords nil '(("﻿" . 'jf/bom-face))))))
  (add-hook 'text-mode-hook
    (lambda ()
      (font-lock-add-keywords nil '(("\t" . 'jf/tabs-face)))
      (font-lock-add-keywords nil '(("﻿" . 'jf/bom-face))))))

(use-package ef-themes
  :straight t
  :init
  (defvar jf/themes-plist '()
    "The named themes by pallette.")
  :config
  (setq ef-themes-headings ; read the manual's entry or the doc string
    '((0 . (bold 1.4))
       (1 . (variable-pitch bold 1.7))
       (2 . (overline semibold 1.5))
       (3 . (monochrome overline 1.4 background))
       (4 . (overline 1.3))
       (5 . (rainbow 1.2))
       (6 . (rainbow 1.15))
       (t . (rainbow 1.1))))
  ;; When these are non-nil, the mode line uses the proportional font
  (setq ef-themes-mixed-fonts t
    ef-themes-variable-pitch-ui t)

  (defun jf/theme-custom-faces ()
    "Set the various custom faces for both `treesit' and `tree-sitter'."
    (ef-themes-with-colors
      (setq hl-todo-keyword-faces
        `(("HOLD" . ,yellow)
           ("TODO" . ,red)
           ("BLOCKED" . ,yellow)
           ("NEXT" . ,blue)
           ("THEM" . ,magenta)
           ("PROG" . ,cyan-warmer)
           ("OKAY" . ,green-warmer)
           ("DONT" . ,yellow-warmer)
           ("FAIL" . ,red-warmer)
           ("BUG" . ,red-warmer)
           ("DONE" . ,green)
           ("NOTE" . ,blue-warmer)
           ("KLUDGE" . ,cyan)
           ("HACK" . ,cyan)
           ("TEMP" . ,red)
           ("FIXME" . ,red-warmer)
           ("XXX+" . ,red-warmer)
           ("REVIEW" . ,red)
           ("DEPRECATED" . ,yellow)))
      (custom-set-faces
        `(denote-faces-link
           ((,c (:inherit link
                  :box (:line-width (1 . 1)
                         :color ,border
                         :style released-button)))))
        `(ef-themes-fixed-pitch
           ((,c (:family "IntoneMono Nerd Font Mono"))))
        `(olivetti-fringe
           ((,c (:inherit fringe :background ,bg-dim))))
        `(jf/bom-face
           ((,c (:width ultra-expanded
                  :box (:line-width (2 . 2)
                         :color ,underline-err
                         :style released-button)))))
        `(jf/mode-line-format/face-shadow
           ((,c :foreground ,fg-mode-line)))
        `(jf/tabs-face
           ((,c :underline (:style wave :color ,bg-blue-intense))))
        `(jf/org-faces-date
           ((,c :underline nil :foreground ,cyan-faint)))
        `(jf/org-faces-epigraph
           ((,c :underline nil :slant oblique :foreground ,fg-alt)))
        `(jf/org-faces-abbr
           ((,c :underline t :slant oblique :foreground ,fg-dim)))
        `(org-list-dt
           ((,c :bold t :slant italic :foreground ,fg-alt)))
        `(font-lock-misc-punctuation-face
           ((,c :foreground ,green-warmer)))
        `(elixir-ts-comment-doc-identifier
           ((,c :foreground ,comment)))
        `(elixir-ts-comment-doc-attribute
           ((,c :foreground ,comment)))
        ;; `(mode-line
        ;;    ((,c :foreground ,cyan :background ,bg-cyan-subtle)))
        `(org-block
           ;; ((,c :background ,bg-yellow-subtle)))
           ((,c :background ,bg-added-faint)))
        `(org-block-begin-line
           ((,c :background ,bg-added-refine)))
        `(org-block-end-line
           ((,c :background ,bg-added-refine)))
        `(org-modern-priority
           ((,c :foreground ,fg-term-red-bright
              :box (:color ,fg-term-red-bright :line-width (-1 . -1)))))
        `(fill-column-indicator
           ((,c :width ultra-condensed
              :background ,bg-dim
              :foreground ,bg-dim)))
        `(font-lock-regexp-face
           ((,c :foreground ,red))))))
  (setq jf/themes-plist '(:dark ef-bio :light ef-cyprus)))

(use-package custom
  :straight (:type built-in)
  :preface
  (mapc #'disable-theme custom-enabled-themes)
  :config
  ;; In organizing the packages, I discovred that themes is part of the
  ;; `custom' package.
  (defun jf/emacs-theme-by-osx-appearance ()
    "Function to load named theme."
    (load-theme
      (plist-get jf/themes-plist (jf/current-macos-interface-style))))

  ;; Theming hooks to further customize colors
  (defvar after-enable-theme-hook nil
    "Normal hook run after enabling a theme.")

  (defun run-after-enable-theme-hook (&rest _args)
    "Run `after-enable-theme-hook'."
    (run-hooks 'after-enable-theme-hook))

  (advice-add 'enable-theme :after #'run-after-enable-theme-hook)

  (add-hook 'after-enable-theme-hook #'jf/theme-custom-faces)

  (defun jf/current-macos-interface-style ()
    (if (equal "Dark"
          (substring
            (shell-command-to-string
              "defaults read -g AppleInterfaceStyle") 0 4))
      :dark :light))

  (defun jf/dark ()
    "Toggle system-wide Dark or Light setting."
    (interactive)
    (shell-command
      (concat "osascript -e 'tell application \"System Events\" "
        "to tell appearance preferences "
        "to set dark mode to not dark mode'")
    (jf/emacs-theme-by-osx-appearance)))
  (jf/emacs-theme-by-osx-appearance))

(use-package tab-bar
  ;; I've been stepping away from multiple tabs, however as I further
  ;; explored.  I need to think of these tabs as contained frames.  They
  ;; can save window configuration.
  :straight (:type built-in)
  :bind ("C-c t t" . #'tab-bar-mode)
  :hook (tab-bar-mode . jf/tab-bar-mode-hook)
  :config
  (defun jf/tab-bar-mode-hook ()
    "Expose key binding for switching tabs."
    (define-key global-map
      (kbd "C-c t n")
      #'tab-bar-new-tab
      (not tab-bar-mode))
    (define-key global-map
      (kbd "C-c t k")
      #'tab-bar-close-tab
      (not tab-bar-mode))
    (define-key global-map
      (kbd "C-c t s")
      #'tab-bar-switch-to-tab
      ;; Remove the binding when we're NOT in tab-bar-mode.
      (not tab-bar-mode))))

(use-package xref
  :straight t
  :custom
  (xref-file-name-display 'project-relative)
  (xref-search-program 'ripgrep))

(use-package tmr
  ;; A timer package.
  ;;
  ;; My dbus install is not behaving so I'm cheating with a bit of AppleScript
  :preface
  (defun jf/tmr-notification-notify (timer)
    "Dispatch a notification for TIMER."
    (let ((title "TMR May Ring (Emacs tmr package)")
           (description (tmr--timer-description timer)))
      (ns-do-applescript (concat "display notification \""
                           description
                           "\" sound name \"Glass\""))))
  :custom (tmr-notify-function #'jf/notifications-notify)
  (tmr-timer-completed-functions
    (list #'tmr-print-message-for-completed-timer
      #'tmr-sound-play
      #'jf/tmr-notification-notify))
  (tmr-timer-finished-functions
    (list #'tmr-print-message-for-completed-timer #'tmr-sound-play #'jf/tmr-notification-notify) nil nil "Customized with use-package tmr")
  :straight (:host github :type git
              :repo "protesilaos/tmr"))

(use-package transient
  ;; A package for creating turbo-charged menus.  It is the backbone for the
  ;; menu-like dispatch of `magit' functionality.
  :straight t)

(use-package ts
  ;; Timestamp library (not typescript)
  :straight t)

(use-package keychain-environment
  ;; Help me manage my secrets via the OS
  ;; Load keychain environment
  :straight t
  :config (keychain-refresh-environment))

(use-package dash
  ;; A modern list API for Emacs. No 'cl required.
  ;; (See https://github.com/magnars/dash.el/)
  :straight t)

(use-package f
  ;; A modern API for working with files and directories in Emacs.
  ;; (See https://github.com/rejeep/f.el/)
  :straight t)

(use-package s
  ;; The long lost Emacs string manipulation library.
  ;; (See https://github.com/magnars/s.el/)
  :straight t)

(use-package wgrep
  ;; “Edit a grep buffer and apply those changes to the file buffer.”  In other
  ;; words, after “searching” for something, sending the results to a buffer
  ;; (via `embark-export' or such thing), you can edit that search results
  ;; buffer and propogate the changes to the locations of the elements that
  ;; matched the search.
  ;;
  ;;   1.  Call `consult-ripgrep' (via ~C-c f~) to search for something.
  ;;   2.  Call `embark-export' (via ~C-s-e~) to export to a grep buffer.
  ;;   3.  Call `wgrep-change-to-wgrep-mode' (via ~e~ or ~C-c C-p~)
  ;;   4.  Edit the grep buffer as you would anywhere else.
  ;;   5.  Save (via ~C-x C-s~) or Cancel (via ~C-c C-k~).
  :after (embark-consult ripgrep)
  :config (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  :straight t
  :bind (:map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          :map ripgrep-search-mode-map
          ("e" . wgrep-change-to-wgrep-mode)))

(use-package rg
  ;; A highly performant successor to the venerable grep.
  :after (wgrep)
  :custom (rg-keymap-prefix (kbd "C-c f"))
  :config (rg-enable-menu)
  ;; https://github.com/dajva/rg.el/issues/142 Give focus to *rg* buffer.
  (add-to-list 'rg-finish-functions (lambda (buffer _) (pop-to-buffer buffer)))

  ;; Override the baseline rg-project to include files
  (rg-define-search rg-project
    :dir project
    :files "*.*")

  ;; Prompt for file types
  (rg-define-search rg-project-prompt-for-files
    :dir project
    :menu ("Search" "P" "Project prompt file type"))

  (when (f-dir-p "~/git/dotemacs/")
    (rg-define-search rg-projects-dotemacs
      "Search Dotemacs"
      :dir "~/git/dotemacs/"
      :files "*.*"
      :menu ("Projects" "j d" "Dotemacs")))

  :init (setq ripgrep-arguments "--ignore-case --follow")
  ;; I want to ensure that I'm following symlinks.  This is important
  ;; for my Personal Knowledge Management file structure and ensuring
  ;; that private and public concepts don't bleed across machines.
  (add-to-list 'rg-required-command-line-flags "--follow")
  :straight t)

(use-package visual-regexp
  ;; I haven't used much search and replace but the visual queues are
  ;; useful.  And I learned about ,\ in this play.  ,\(upcase \1) will
  ;; upcase the first capture region.
  :straight t
  :bind ("C-c C-r r" . vr/replace)
  ("C-c C-r m" . vr/mc-mark)
  ("C-c C-r q" . vr/query-replace)
  ("C-c C-r p" . project-query-replace-regexp))

;; https://github.com/hokomo/query-replace-parallel
;; Presented at https://pad.emacsconf.org/2023-parallel
;; (use-package query-replace-parallel
;;   :straight (:host github :repo "hokomo/query-replace-parallel")
;;   :commands (query-replace-parallel query-replace-parallel-regexp))

(use-package crux
  ;; A mix of a few odd and useful functions.
  :straight t
  :bind (("C-a" . crux-move-beginning-of-line)
          ("<C-s-return>" . crux-smart-open-line-above)
          ("C-s-k" . crux-kill-line-backwards)
          ("<s-backspace>" . crux-kill-line-backwards)
          ("<f9>" . crux-kill-other-buffers)))

(use-package math-at-point
  ;; Sometimes you just want to do math
  :straight (math-at-point :type git :host github
              :repo "shankar2k/math-at-point")
  :bind ("C-c =" . math-at-point))


(use-package emacs-everywhere
  ;; Favor this over the hammerspoon editWithEmacs command.  Why?  This
  ;; command is more generalized and is getting more traction amongst
  ;; folks in the #emacs fediverse.  Hence something that is more likely
  ;; to see maintenance.
  :straight t
  :config
  (setq emacs-everywhere-frame-parameters
    '((name . "emacs-everywhere")
       (fullscreen)
       (width . 0.33)
       (height . 0.5)
       (top . 0)
       (left . 0))))

(require 'jf-utility)

(use-package ws-butler
  ;; Keep white space tidy.
  :straight t
  :hook (prog-mode . ws-butler-mode))

(use-package fill-sentences-correctly
  ;; `fill-sentences-correctly-mode' ensures that `fill-paragraph'
  ;; (e.g. M-q) preserves two spaces.
  :straight (fill-sentences-correctly
       :host github
       :repo "duckwork/fill-sentences-correctly.el")
  :config (fill-sentences-correctly-mode))

(use-package tomelr
  ;; Emacs-Lisp Library for converting S-expressions to TOML.  I'll
  ;; likely be using this as I move my Hugo front-matter from YAML to
  ;; TOML, as per the changes described by `ox-hugo'.
  :straight (tomelr :host github :repo "kaushalmodi/tomelr"))

(require 'jf-org-mode)

(use-package abbrev
  ;; The =abbrev= package is simple and powerful, providing an
  ;; auto-correct that I configure.  No more “teh” in my text.
  :straight (:type built-in)
  :custom (abbrev-file-name (file-truename
                              "~/git/dotemacs/emacs.d/abbrev_defs"))
  :hook (text-mode . abbrev-mode))

(use-package emacs
  :bind ("C-M-i" . completion-at-point)
  ("TAB" . indent-for-tab-command)
  :custom
  (global-display-line-numbers-mode t)
  (column-number-mode t)
  (global-display-fill-column-indicator-mode t)
  (delete-selection-mode t)
  (auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  :init
  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
    #'command-completion-default-include-p)
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (format "[%s %s] %s"
            (propertize "CRM" 'face 'error)
            (propertize
              (replace-regexp-in-string
                "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                crm-separator)
              'face 'success)
            (car args))
      (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
    '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package embark
  ;; The "missing" context menu; a bit like the right-click but more.
  :straight t
  :bind
  (("C-." . embark-act)       ;; pick some comfortable binding
    ("M-." . embark-dwim)
    ("C-s-e" . embark-export)
    ("H-e" . embark-export)
    ("C-h b" . embark-bindings))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (setq embark-action-indicator
    (lambda (map &optional _target)
      (which-key--show-keymap "Embark" map nil nil 'no-paging)
      #'which-key--hide-popup-ignore-command)
    embark-become-indicator embark-action-indicator))

(use-package consult
  ;; Extensions for the numerous `completing-read' functions.  Highly extensible
  ;; and customizable.
  :straight t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
          ("C-c h" . consult-history)
          ("C-c b" . consult-buffer)
          ("C-c k" . consult-kmacro)
          ;; C-x bindings (ctl-x-map)
          ("C-x M-:" . consult-complex-command)
          ("C-x b" . consult-bookmark)
          ("s-b" . consult-buffer)
          ("C-x 4 b" . consult-buffer-other-window)
          ;; Custom M-# bindings for fast register access
          ("M-#" . consult-register-load)
          ("M-'" . consult-register-store)
          ("M-`" . consult-register)
          ;; Other custom bindings
          ("C-y" . yank)
          ("C-c C-/" . #'consult-clock-in)
          ("M-y" . consult-yank-from-kill-ring)
          ("M-s k" . consult-keep-lines)
          ("M-s u" . consult-focus-lines)
          ;; M-g bindings (goto-map)
          ("M-g e" . consult-compile-error)
          ("M-g g" . consult-goto-line)
          ("H-o" . consult-org-agenda)
          ("M-g M-o" . consult-org-agenda)
          ("M-g M-g" . consult-goto-line)
          ("s-l" . consult-goto-line)
          ("C-l" . consult-goto-line)
          ("M-g o" . consult-outline)
          ("M-g m" . consult-mark)
          ("M-g M" . consult-global-mark)
          ("C-x C-SPC" . consult-global-mark)
          ("M-i" . jf/consult-imenu)
          ("M-g i" . consult-imenu)
          ("M-g I" . consult-imenu-multi)
          ;; M-s bindings (search-map)
          ("M-s f" . consult-find)
          ;; ("M-s L" . consult-locate)
          ;; ("M-s g" . consult-git-grep)
          ;; ("M-s G" . consult-git-grep)
          ;;
          ;; I keep this around because orderless search is great
          ("M-s r" . consult-ripgrep)
          ;; ("C-c f" . consult-ripgrep)
          ;; ("M-s l" . consult-line)
          ("M-s M-s" . consult-line-multi)
          ;; Customizations that map to ivy
          ("C-c r" . consult-recent-file)
          ;; ("C-c o" . consult-file-externally)
          ("C-s" . consult-line) ;; I've long favored Swiper mapped to c-s
          ;; Isearch integration
          ("M-s e" . consult-isearch-history)
          :map isearch-mode-map
          ("M-e" . consult-isearch-history)
          ("M-s e" . consult-isearch-history)
          ("M-s l" . consult-line))
  :commands (consult--read)
  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
    xref-show-definitions-function #'consult-xref)
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
    register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  :custom
  (consult-narrow-key "<")
  ;; Updating the default to include "--smart-case"
  ;; Leveraging ripgrep-all https://github.com/phiresky/ripgrep-all
  (consult-ripgrep-command
    (concat "rg --null --hidden --line-buffered --color=ansi --max-columns=1000 "
      "--smart-case --no-heading --line-number --no-ignore-vcs "
      "--follow "
      "--glob !vendor/ --glob !coverage/ --glob !**/tmp/ --glob !**/log/ "
      "--glob !public/ --glob !node_modules/ --glob !.git/ --glob !doc/ "
      "--glob !.yardoc/ --glob !.byebug_history "
      " . -e ARG OPTS"))
  (consult-ripgrep-args
    (concat "rg --null --hidden --line-buffered --color=never --max-columns=1000 "
      "--path-separator / --no-ignore-vcs --smart-case --no-heading "
      "--follow "
      "--glob !vendor/ --glob !coverage/ --glob !**/tmp/ --glob !**/log/ "
      "--glob !public/ --glob !node_modules/ --glob !.git/ --glob !doc/ "
      "--glob !.yardoc/ --glob !.byebug_history "
      "--line-number "))
  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :preface
  (defun jf/consult-imenu (prefix)
    "Call `consult-imenu' or when PREFIX is given call `consult-imenu-multi'."
    (interactive "P")
    (if (car prefix)
      (consult-imenu-multi)
      (consult-imenu)))
  (defun consult-clock-in (prefix &optional match)
    "Clock into an Org agenda heading picking from MATCH.

With a PREFIX jump to the agenda without starting the clock."
    (interactive "P")
    (let ((the-match (or match "TODO=\"STARTED\"|TODO=\"TODO\"")))
      (if prefix
        (consult-org-agenda the-match)
        (save-window-excursion
          (consult-org-agenda the-match)
          (org-clock-in)))))
  (defun jf/consult-buffer-kill ()
    "In `consult-buffer' kill the current candidate"
    (interactive)
    (let ((marker (string #x200002)) ;; probably some internal detail :(
           (candidate (vertico--candidate)))
      (when (s-ends-with? marker candidate)
        (kill-buffer (s-replace marker "" candidate))
        (vertico-next))))
  ;; Customizations
  :config
  (consult-customize
    consult-line consult-ripgrep consult-find
    :initial (when (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))))
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)

  (require 'consult-imenu)
  (dolist (python '(python-mode python-ts-mode))
    (add-to-list 'consult-imenu-config
      `(,python
         :toplevel "Method"
         :types ((?f "Field" font-lock-variable-name-face)
                  (?c "Class" font-lock-type-face)
                  (?m "Method" font-lock-function-name-face)
                  (?v "Variable" font-lock-variable-name-face)
                  ))))
  (dolist (ruby '(ruby-mode ruby-ts-mode))
    (add-to-list 'consult-imenu-config
      `(,ruby
         :toplevel "Method"
         :types ((?p "Property" font-lock-property-name-face)
                  (?c "Class" font-lock-type-face)
                  (?C "Constant" font-lock-constant-face)
                  (?e "Example" font-lock-doc-face)
                  (?M "Module" font-lock-type-face)
                  (?m "Method" font-lock-function-name-face))))))

(use-package embark-consult
  ;; I use ~embark.el~ and ~consult.el~, let’s add a little bit more connective
  ;;  tissue.
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-dir
  ;; This package helps ease traveling across directories by providing directory
  ;; candidates related to current buffers, bookmarks, and projects.  Further,
  ;; like other ~consult.el~ functions, you can use narrowing keys.  See
  ;; https://github.com/karthink/consult-dir.
  :straight t
  :after (consult)
  :bind (("C-x C-d" . consult-dir)
          :map minibuffer-local-completion-map
          ("C-x C-d" . consult-dir)
          ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-projectile
  ;; package provides a function I use everyday: ~M-x consult-projectile~.  When
  ;; I invoke ~consult-projectile~, I have the file completion for the current
  ;; project.  I can also type =b= + =SPACE= to narrow my initial search to open
  ;; buffers in the project.  Or =p= + =space= to narrow to other projects; and
  ;; then select a file within that project.
  :commands (consult-projectile)
  :bind (("C-x 4 p" . consult-projectile-find-file-other-window)
          ("M-s r" . consult-ripgrep)
          ("M-s f" . projectile-ripgrep))
  :straight (consult-projectile
              :type git
              :host gitlab
              :repo "OlMon/consult-projectile"
              :branch "master")


  :config
  ;; I want recent files as well as project files as well as recent project
  ;; files...Hence the override fb
  (setq jf/consult--source-recent-file consult--source-recent-file)
  (plist-put jf/consult--source-recent-file :narrow ?R)
  (plist-put jf/consult--source-recent-file :name "Recent File")
  (setq consult-projectile-sources
    '( ;; key b
       consult-projectile--source-projectile-buffer
       ;; key f
       consult-projectile--source-projectile-file
       ;; key p
       consult-projectile--source-projectile-project
       ;; key d
       consult-projectile--source-projectile-dir
       ;; key m
       consult--source-bookmark
       ;; key r
       consult-projectile--source-projectile-recentf
       ;; key R
       jf/consult--source-recent-file
       ;; key *
       consult--source-modified-buffer))


  (defun consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
    (interactive)
    (let ((default-directory (or dir default-directory)))
      (consult--read #'read-file-name-internal :state (consult--file-preview)
        :prompt prompt
        :initial initial
        :require-match mustmatch
        :predicate pred)))
  :bind
  ;;; This overwrite `ns-open-file-using-panel'; the operating system's "Finder"
  ;; ("C-c o" . consult-projectile)
  ;;; I have long had Cmd+t mapped to opening project files; however, I'm
  ;;; noticing the way I'm typing this and it is feeling wrong.  So now I won't
  ;;; have that way open.
  ("s-t" . consult-projectile)
  ("s-p" . consult-projectile)
  ("H-t" . consult-projectile)
  ("H-p" . consult-projectile))

(use-package corfu
  ;; Completion overlay; a narrower intreface than the more verbose company.
  :straight t
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
          ("<escape>". corfu-quit)
          ("<return>" . corfu-insert)
          ("M-d" . corfu-show-documentation)
          ("M-l" . 'corfu-show-location)
          ("TAB" . corfu-next)
          ([tab] . corfu-next)
          ("S-TAB" . corfu-previous)
          ([backtab] . corfu-previous))
  :custom
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)      ; Always show candidates in menu
  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)
  ;; (corfu-min-width 80)
  ;; (corfu-max-width corfu-min-width)     ; Always have the same width
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)
  ;; (corfu-echo-documentation nil)        ; Already use corfu-doc
  (corfu-separator ?\s)                 ; Necessary for use with orderless
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)       ; Preview current candidate?
  (corfu-preselect-first t)             ; Preselect first candidate?
  :preface
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (defun corfu-move-to-minibuffer ()
    "Move \"popup\" completion candidates to minibuffer.
Useful if you want a more robust view into the recommend candidates."
    (interactive)
    (let (completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map))
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))
  :config
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  :init
  ;; (corfu-indexed-mode)
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  (global-corfu-mode)
  (load "~/.emacs.d/straight/build/corfu/corfu-indexed.el"
    nil
    jf/silence-loading-log)
  (corfu-indexed-mode)
  (load "~/.emacs.d/straight/build/corfu/corfu-info.el"
    nil
    jf/silence-loading-log)
  (load "~/.emacs.d/straight/build/corfu/corfu-popupinfo.el"
    nil
    jf/silence-loading-log)
  (corfu-popupinfo-mode))

(use-package cape
  ;; Completion at point functions, with the amazing `cape-capf-super' for
  ;; granular configuration of specific mode completion behavior.
  :straight t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :bind (("C-c p d" . cape-dabbrev)
          ("C-c p e" . cape-elisp-block)
          ("C-c p f" . cape-file)
          ("C-c p h" . cape-history)
          ("C-c p s" . cape-symbol)
          ("C-c p w" . cape-dict)))

(use-package grab-mac-link
  ;; Grab a link from a variety of MacOS applications.
  :straight t
  ;; Ensuring we load these, as I'll need them later.
  :commands (grab-mac-link-safari-1 grab-mac-link-firefox-1)
  :config
  ;; A replacement function for existing grab-mac-link-make-html-link
  (defun jf/grab-mac-link-make-html-link (url name)
    "Using HTML syntax, link to and cite the URL with the NAME."
    (format (concat "<cite>"
              "<a href=\"%s\" class=\"u-url p-name\" rel=\"cite\">"
              "%s"
              "</a>"
              "</cite>")
      url name))
  ;; The function advice to override the default behavior
  (advice-add 'grab-mac-link-make-html-link
    :override 'jf/grab-mac-link-make-html-link
    '((name . "jnf")))
  :bind (("C-c g" . grab-mac-link)))

(use-package helpful
  ;; Help me lookup definitions and details.
  :init
  (use-package transient :straight t)
  ;; I'm going to talk about this later, but I'm adding this to the menu, so I
  ;; may as well state the dependency.
  (use-package embark :straight t)
  :straight t
  :config
  (transient-define-prefix jf/helpful-menu ()
    "Return a `transient' compliant list to apply to different transients."
    ["Help"
      ""
      ("Q" "Kill Helpful Buffers" helpful-kill-buffers)
      ""
      ("b" "Bindings" embark-bindings)
      ("c" "Command" helpful-command)
      ("d" "Definition" sdcv-search)
      ("D" "Docs" devdocs-lookup)
      ("f" "Function (interactive)" helpful-callable)
      ("F" "Function (all)" helpful-function)
      ("k" "Key" helpful-key)
      ("l" "Library" find-library)
      ("m" "Macro" helpful-macro)
      ("p" "Thing at point" helpful-at-point)
      ("." "Thing at point" helpful-at-point)
      ("t" "Text properties" describe-text-properties)
      ("v" "Variable" helpful-variable)])
  :bind ("H-h" . jf/helpful-menu)
  ("C-s-h" . jf/helpful-menu))

(use-package hippie-exp
  ;; A composable expansion tool that I find compliments `corfu' in that it
  ;; looks in a different manner for completions.
  ;;
  ;; TODO: Perhaps I should spend a bit time investigating removing `hippie-exp'
  ;; in favor of `corfu' and `cape' behavior.  Definitely spend a bit of time exploring
  ;; this option.
  :straight t
  :config
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev-visible
                                            try-expand-dabbrev
                                            try-expand-dabbrev-all-buffers
                                            try-expand-dabbrev-from-kill
                                            try-complete-file-name
                                            try-complete-file-name-partially
                                            try-expand-all-abbrevs
                                            try-expand-list
                                            try-expand-line
                                            try-complete-lisp-symbol-partially
                                            try-complete-lisp-symbol))
  :bind (("M-SPC" . hippie-expand))
  :init (global-set-key [remap dabbrev-expand] 'hippie-expand))

(use-package marginalia
  ;; Given that my blog has lots of "writing in the margins" this is the package
  ;; for me.
  ;;
  ;; It provides annotations for completions; in particular I rely on showing
  ;; the docstring of `M-x' results.
  :straight t
  :config (setq marginalia-max-relative-age 0) ;; Set absolute value
  ;; /Note:/ The declaration of `marginalia-mode' must be in the :init
  ;; section.This ensures that it is enabled right away.  It also forces the
  ;; loading of the package.
  :init (marginalia-mode))

(use-package orderless
  ;; The https://github.com/minad/orderless package provides completion tooling
  ;; for non-strict word order.  I spent considerable time reading through the
  ;; https://github.com/minad/consult/wiki
  ;;
  ;; As configured the orderless completion recognizes the following “switches”:
  ;;
  ;; - Flex (~\~~) :: Just start typing characters and you’ll get matches that
  ;;   have those characters
  ;; - File Extension (~\.ext~) :: Match files with this extension.
  ;; - Regexp ~^.$~ :: Use some regular expression syntax
  ;;   - ~^~ matching beginning
  ;;   - ~.~ any ol’ character
  ;;   - ~$~ matching ending
  ;; - Initialism (~`~) :: In ~M-x~ when I typed ~`pl~ the ~previous-line~
  ;;   function was a top match.  The initialism switch “explodes” the
  ;;   characters and says match methods who’s words start with those
  ;;   characters.
  ;; - Not Literal ~!~ :: Exclude candidates that match the literal
  ;;   (e.g. ~!previous~ won’t show ~previous-line~ in the ~M-x~ completion).
  ;; - Literal ~=~ :: No “fuzzy buziness”, just match exactly what I typed.
  ;;
  ;; There is another case (e.g. ~%~ character fold) that I don’t yet
  ;; understand.
  :straight t
  :config
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
       (?! . orderless-without-literal)
       (?`. orderless-initialism)
       (?= . orderless-literal)
       (?~ . orderless-flex)))
  (defun +orderless-dispatch (pattern index _total)
    (cond
      ;; Ensure that $ works with Consult commands, which add disambiguation
      ;; suffixes
      ((string-suffix-p "$" pattern)
        `(orderless-regexp . ,(concat (substring pattern 0 -1)
                                "[\x100000-\x10FFFD]*$")))
      ;; File extensions
      ((and
         ;; Completing filename or eshell
         (or minibuffer-completing-file-name
           (derived-mode-p 'eshell-mode))
         ;; File extension
         (string-match-p "\\`\\.." pattern))
        `(orderless-regexp . ,(concat "\\." (substring pattern 1)
                                "[\x100000-\x10FFFD]*$")))
      ;; Ignore single !
      ((string= "!" pattern) `(orderless-literal . ""))
      ;; Prefix and suffix
      ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
         (cons (cdr x) (substring pattern 1))
         (when-let (x (assq (aref pattern (1- (length pattern)))
                        +orderless-dispatch-alist))
           (cons (cdr x) (substring pattern 0 -1)))))))
  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism
                                  orderless-literal
                                  orderless-regexp)))
  ;; Certain dynamic completion tables (completion-table-dynamic) do not work
  ;; properly with orderless. One can add basic as a fallback.  Basic will only
  ;; be used when orderless fails, which happens only for these special tables.
  (setq completion-styles '(orderless basic)
    completion-category-defaults nil
          ;;; Enable partial-completion for files.
          ;;; Either give orderless precedence or partial-completion.
          ;;; Note that completion-category-overrides is not really an override,
          ;;; but rather prepended to the default completion-styles.
    ;; completion-category-overrides '((file (styles orderless
    ;; partial-completion))) ;; orderless is tried first
    completion-category-overrides '((file
                                      (styles partial-completion))
                                     ;; enable initialism by default for symbols
                                     (command
                                       (styles +orderless-with-initialism))
                                     (variable
                                       (styles +orderless-with-initialism))
                                     (symbol
                                       (styles +orderless-with-initialism))
                                     (eglot
                                       (styles orderless)))
    orderless-component-separator #'orderless-escapable-split-on-space
    orderless-style-dispatchers '(+orderless-dispatch)))

(use-package org-mac-link
  ;; Similar to `grab-mac-link' but a bit specific to `org-mode'.
  :straight (org-mac-link :type git :host github :repo "jeremyf/org-mac-link")
  :bind (:map org-mode-map (("C-c g" . org-mac-grab-link))))

(use-package tempel
  ;; For awhile, I'd used yasnippets; themselves inspired by my beloved
  ;; TextMate.  However, I've found `tempel' to be both more than adequate and
  ;; has a narrower implementation foot-print, cleaving closer to emacs-lisp;
  ;; thus likely easing it's maintenance burden.
  :straight (tempel :host github :repo "minad/tempel")
  :custom (tempel-path "~/git/dotemacs/templates")
  :config (global-tempel-abbrev-mode)
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
          ("M-*" . tempel-insert))
  :bind (:map tempel-map (([backtab] . tempel-previous)
        ("TAB" . tempel-next)))
  :preface
  (cl-defun jf/org-macro-value-list (macro-name &key (dir org-directory))
    "List the unique inner text of all uses of MACRO-NAME in given DIR."
    (let ((path (if current-prefix-arg dir (or (buffer-file-name (current-buffer)) dir))))
      (s-split
        "\n"
        (s-trim
          (shell-command-to-string
            (concat
              "rg \"\\{\\{\\{"
              macro-name
              "\\((.+?)\\)\\}\\}\\}"
              "\" --only-matching --no-filename -r '$1' "
              path
              " | sort | uniq"))))))
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'. `tempel-expand'
    ;; only triggers on exact matches. Alternatively use `tempel-complete' if
    ;; you want to see all matches, but then Tempel will probably trigger too
    ;; often when you don't expect it.
    ;; NOTE: We add `tempel-expand' *before* the main programming mode Capf,
    ;; such that it will be tried first.
    (setq-local completion-at-point-functions
      (cons #'tempel-expand
        completion-at-point-functions)))
  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (tempel-global-abbrev-mode)
  :init
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  ;; Hyper Macro!
  (tempel-key "H-m d" tsomb-date org-mode-map)
  (tempel-key "H-m u" update_block org-mode-map)
  (tempel-key "H-m c" macro-cite org-mode-map)
  (tempel-key "H-m i" macro-idiomatic org-mode-map)
  (tempel-key "H-m m" macro-mechanic org-mode-map)
  (tempel-key "H-m k" macro-keyboard org-mode-map))

(use-package vertico
  ;; Another one of minad's packages which improves my day to day experience.  I
  ;; find the user experience wonderful when pairing vertical candidate
  ;; selection with `marginalia' and then having the `vertico-indexed-mode'
  ;; option for quick numerical selection.
  :straight (:type git :host github :repo "minad/vertico")
  :bind (:map vertico-map
          (("<tab>" . #'vertico-insert)
            ("<escape>" . #'minibuffer-keyboard-quit)
            ("M-p" . #'previous-history-element)
            ("M-n" . #'next-history-element)
            ;; I've been using more groupings, and being able to move through
            ;; those is nice.
            ("C-M-n" . #'vertico-next-group)
            ("C-M-p" . #'vertico-previous-group)
            ("C-SPC" . #'jf/vertico-restrict-to-matches)))
  :preface
  ;; https://github.com/minad/vertico/wiki#restrict-the-set-of-candidates
  (defun jf/vertico-restrict-to-matches ()
    "Restrict set of candidates to visible candidates.

This will narrow the candidates to what matched.  Then clears the
prompt and allows further narrowing.

Useful when you want to mix selector semantics (e.g. start with a
literal then add a fuzzy search)."
    (interactive)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert " ")
      (add-text-properties (minibuffer-prompt-end) (point-max)
        '(invisible t
           read-only t
           cursor-intangible t
           rear-nonsticky t))))
  :config
  (define-key vertico-map (kbd "C-SPC") #'jf/vertico-restrict-to-matches)
  (vertico-mode 1)
  (setq read-file-name-completion-ignore-case t
    read-buffer-completion-ignore-case t
    completion-ignore-case t)
  (setq vertico-cycle t)
  :init
  ;; Type "C-3 return" and select the 3rd candidate in the list.
  (load "~/.emacs.d/straight/build/vertico/vertico-indexed.el"
    nil
    jf/silence-loading-log)
  (vertico-indexed-mode)
  (load "~/.emacs.d/straight/build/vertico/vertico-directory.el"
    nil
    jf/silence-loading-log)
  (load "~/.emacs.d/straight/build/vertico/vertico-repeat.el"
    nil
    jf/silence-loading-log)
  (keymap-global-set "M-r" #'vertico-repeat)
  ;; When I type ~/ in the `find-file' selector, then it will clear the existing
  ;; path and go to ~/
  ;; From Prot's video presentation
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package which-key
  ;; This package helps me begin typing a key sequence and seeing what options
  ;; are available to complete the sequence.
  ;;
  ;; For example, I type "C-c", wait a moment and get a menu that shows me what
  ;; key bindings start with "C-c"; and then I can type the following key and
  ;; execute that command.
  :straight t
  :custom
  (which-key-side-window-max-width 0.5)
  (which-key-min-column-description-width 60)
  (which-key-max-description-length nil)
  (which-key-show-docstrings t)
  (which-key-add-column-padding 2)
  (which-key-max-display-columns 2)
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (which-key-show-major-mode))

(require 'jf-denote)
(use-package emojify
  ;; All the people using emojiis; why not
  :straight t
  :config
  (defun --set-emoji-font (frame)
    "Adjust the font settings of FRAME so Emacs can display emoji properly."
    (if (eq system-type 'darwin)
  ;; For NS/Cocoa
  (set-fontset-font t
        'symbol
        (font-spec :family "Apple Color Emoji")
        frame
        'prepend)
      ;; For Linux
      (set-fontset-font t
      'symbol
      (font-spec :family "Symbola")
      frame
      'prepend)))
  ;; For when Emacs is started in GUI mode:
  (--set-emoji-font nil)
  ;; Hook for when a frame is created with emacsclient
  ;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
  (add-hook 'after-make-frame-functions '--set-emoji-font))

(use-package sdcv-mode
  ;; This follows from
  ;; http://mbork.pl/2017-01-14_I'm_now_using_the_right_dictionary
  ;;
  ;; Namely I want to use a more inspiring dictionary for the poetry and prose.
  :straight (sdcv-mode :type git :host github :repo "gucong/emacs-sdcv")
  :bind ("C-c C-'" . sdcv-search))


(use-package unicode-fonts
  ;; Before the emojii...
  :straight t
  :config (unicode-fonts-setup))

(use-package unfill
  ;; Provides the reverse of ~fill-paragraph~, and a toggle fill and unfill.  In
  ;; fact, the unfill/fill function of Emacs was the first editor function I saw
  ;; (shown to me by a friend in 2005) that had me strongly consider Emacs. Alas
  ;; I was not prepared for Emacs.
  :bind ("M-q" . unfill-toggle)
  :straight t)

(use-package hungry-delete
  ;; Delete multiple spaces in one delete stroke.
  :straight t
  :config (global-hungry-delete-mode))

(use-package move-text
  ;; A simple package ability to move lines up and down.
  :straight t
  :bind (([C-s-down] . move-text-down)
         ([C-s-up] . move-text-up)))

(use-package titlecase
  ;; The rules of “titlecase” are confounding.  The ~titlecase.el~ package
  ;; provides numerous ways to cast a string to “titlecase.”  I chose wikipedia
  ;; style as a quasi-opinionated compromise.
  :straight (titlecase :host github :repo "duckwork/titlecase.el")
  :custom (titlecase-style 'wikipedia))

(use-package multiple-cursors
  ;; Allow Emacs to work with multiple cursors.  See
  ;; https://melpa.org/#/multiple-cursors
  :bind (("C-M-SPC" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-s-<mouse-1>" . mc/add-cursor-on-click)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/edit-lines)) ;; CTRL+CMD+c
  :straight t)

(use-package iedit
  ;; Type \"C-;\" to select current symbol and all matches; Then edit at multiple
  ;; points.
  :straight t)

(use-package ispell
  :straight (:type built-in)
  :config
  (setq-default ispell-program-name "aspell"))

(require 'jf-coding)
(require 'jf-organizing)

(use-package edit-indirect
  ;; A nice package for editing regions in separate buffers.  It doesn't appear
  ;; to get the mode guess right.  I haven't used this as much as
  ;; `narrow-region'.  Perhaps it can go?
  :straight t)

(use-package logos
  ;; A `narrow-region' extension that moves towards providing a
  ;; presentation-type experience.
  :straight t
  :bind (:map logos-focus-mode-map
          ("M-]" . #'logos-forward-page-dwim)
          ("s-]" . #'logos-forward-page-dwim)
          ("M-[" . #'logos-backward-page-dwim)
          ("s-[" . #'logos-backward-page-dwim))
  :config
  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim))
  ;; (let ((map logos-focus-mode-map))
  ;;   (define-key map [remap next-line] #'logos-forward-page-dwim)
  ;;   (define-key map [remap previous-line] #'logos-backward-page-dwim))
  (setq logos-outlines-are-pages t)
  (setq-default logos-hide-cursor t
    logos-hide-mode-line t
    logos-hide-buffer-boundaries t
    logos-hide-fringe t
    logos-variable-pitch t
    logos-buffer-read-only t
    logos-scroll-lock nil
    logos-olivetti t
    logos-outline-regexp-alist
    `((emacs-lisp-mode . "^;;;+ ")
       (org-mode . "^\\*+ +")
       (markdown-mode . "^\\#+ +")))
  (defun logos--reveal-entry ()
    "Reveal Org or Outline entry."
    (cond
      ((and (eq major-mode 'org-mode)
         (org-at-heading-p))
        (org-show-subtree))
      ((or (eq major-mode 'outline-mode)
         (bound-and-true-p outline-minor-mode))
        (outline-show-subtree))))
  :init
  (add-hook 'logos-page-motion-hook #'logos--reveal-entry))

(use-package "nov.el"
  ;; A package to help in reading epubs.
  :straight t
  :init (use-package esxml :straight t)
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :custom (nov-text-width 80))

(use-package so-long
  ;; Switch to `so-long' when the file gets too long for normal processing.
  :straight t
  :bind
  (:map so-long-mode-map
    ("C-s" . isearch-forward)
    ("C-r" . isearch-backward))
  :config (global-so-long-mode 1))

;;;
;; A package to "narrow" focus; providing a visually appealing interface
(use-package olivetti
  :straight t
  :hook (olivetti-mode-on . jf/olivetti-mode-on-hook)
  (olivetti-mode-off . jf/olivetti-mode-off-hook)
  :config
  ;; I'm typically aiming for 80 character fill-column.
  (setq olivetti-body-width 80)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-style 'fancy)
  (setq olivetti-recall-visual-line-mode-entry-state t)
  :config
  (defun jf/olivetti-mode-on-hook ()
    "Remove some visual chatter."
    (setq-local original-flymake-fringe-indicator-position
      flymake-fringe-indicator-position)
    (setq-local original-vi-tilde-fringe-mode
      vi-tilde-fringe-mode)
    (setq-local original-display-fill-column-indicator-mode
      display-fill-column-indicator-mode)
    (setq-local original-git-gutter-mode
      git-gutter-mode)
    (setq-local original-display-line-numbers-mode
      display-line-numbers-mode)
    (setq-local original-org-modern-block-fringe
      org-modern-block-fringe)
    ;; The of org-modern blocks is not quite right with olivetti.
    (setq-local org-modern-block-fringe nil)
    (setq-local flymake-fringe-indicator-position nil)
    ;; By restarting org-modern-mode, I hide the expansive fringe; thus
    ;; preserving the "beauty" of Olivetti
    (when (eq major-mode 'org-mode)
      (org-modern-mode 1))
    (vi-tilde-fringe-mode -1)
    (display-line-numbers-mode -1)
    (display-fill-column-indicator-mode -1)
    (git-gutter-mode -1))
  (defun jf/olivetti-mode-off-hook ()
    "Restore some visual chatter."
    (setq-local flymake-fringe-indicator-position
      original-flymake-fringe-indicator-position)
    (when (eq major-mode 'org-mode)
      (org-modern-mode 1))
    (vi-tilde-fringe-mode
      original-vi-tilde-fringe-mode)
    (display-fill-column-indicator-mode
      original-display-fill-column-indicator-mode)
    (display-line-numbers-mode
      original-display-line-numbers-mode)
    (git-gutter-mode
      original-git-gutter-mode)
    (setq-local org-modern-block-fringe
      original-org-modern-block-fringe))
  (defun jf/olivetti-mode (&rest args)
    ;; I want to turn off org-modern-mode as it's drawing of the
    ;; overlays conflicts with Olivetti.  We'll turn it on later.
    (when (eq major-mode 'org-mode)
      (org-modern-mode -1)))
  (advice-add 'olivetti-mode :before #'jf/olivetti-mode))

;;; Presentation mode leveraging logos

(defvar jf/minor-mode/presenter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") #'next-line)
    (define-key map (kbd "C-p") #'previous-line)
    (dolist (key `("M-]" "s-]"))
      (define-key map (kbd key) 'logos-forward-page-dwim))
    (dolist (key `("M-[" "s-["))
      (define-key map (kbd key) 'logos-backward-page-dwim))
    map))

(define-minor-mode jf/minor-mode/presenter
  "Enter a `logos' and `olivetti' mode for showing things."
  :init-value nil
  :global nil
  :keymap jf/minor-mode/presenter-map
  :lighter " presenter")

(defcustom jf/minor-mode/presenter-on-hook
  (lambda ()
    (let ((logos-hide-cursor nil)
           (logos-buffer-read-only nil)
           (org-hide-emphasis-markers t))
      (call-interactively 'logos-narrow-dwim)
      (olivetti-mode t)
      (keycast-mode-line-mode t)
      (display-line-numbers-mode -1)
      (when (fboundp 'fontaine-set-preset) (fontaine-set-preset 'presenting))
      (when (fboundp 'vi-tilde-fringe-mode) (vi-tilde-fringe-mode -1))
      (when (fboundp 'git-gutter-mode) (git-gutter-mode -1))
      (when (fboundp 'centaur-tabs-local-mode) (centaur-tabs-local-mode -1))))
  "Hook when `jf/minor-mode/presenter' activated."
  :type 'hook)

(defcustom jf/minor-mode/presenter-off-hook
  (lambda ()
    (call-interactively 'widen)
    (olivetti-mode -1)
    (keycast-mode-line-mode -1)
    ;; (setq-local  org-hide-emphasis-markers nil)
    (display-line-numbers-mode t)
    (when (fboundp 'fontaine-set-preset) (fontaine-set-preset 'default))
    (when (fboundp 'vi-tilde-fringe-mode) (vi-tilde-fringe-mode t))
    (when (fboundp 'git-gutter-mode) (git-gutter-mode t))
    (when (fboundp 'centaur-tabs-local-mode) (centaur-tabs-local-mode t)))
  "Hook when `jf/minor-mode/presenter' deactivated."
  :type 'hook)

(require 'jf-utility)

(use-package mastodon
  :straight (:host github :repo "jeremyf/mastodon.el")
  :config (setq mastodon-instance-url "https://dice.camp"
            mastodon-active-user "takeonrules"))

(use-package doc-view
  ;; A package for improving the in Emacs viewing experience of PDFs.
  :straight (doc-view :type built-in)
  :bind (:map doc-view-mode-map
              ("C-c g" . doc-view-goto-page)))

(use-package elfeed
  ;; An Emacs RSS reader.  I’ve used Google Reader, Feedly, Inoreader, and
  ;; Newsboat.  I wrote about
  ;; https://takeonrules.com/2020/04/12/switching-from-inoreader-to-newsboat-for-rss-reader/,
  ;; and the principles apply for Elfeed.
  :straight t
  :after org
  :preface
  (defun jf/elfeed-show-entry-switch(buffer)
    (switch-to-buffer buffer)
    (setq-local shr-inhibit-images t)
    (olivetti-mode 1)
    (text-scale-set 2)
    (elfeed-show-refresh))
  :custom
  (elfeed-curl-timeout 90)
  (elfeed-db-directory "~/Documents/.elfeed")
  :config
  (setq elfeed-show-entry-switch #'jf/elfeed-show-entry-switch)
  (setq-default elfeed-search-filter "@2-days-ago +unread ")
  :bind ((:map elfeed-search-mode-map
           ("q" . jf/elfeed-save-db-and-bury)))
  :config
  (defun jf/elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer."
    ;;write to disk when quiting
    (interactive)
    (elfeed-db-save)
    (quit-window))

  (defun jf/elfeed-load-db-and-open ()
    "Load the elfeed db from disk before opening."
    (interactive)
    (elfeed)
    (elfeed-update)
    (elfeed-db-load)
    (elfeed-search-update--force))
  (defalias 'rss 'jf/elfeed-load-db-and-open)

  ;; From https://karthinks.com/blog/lazy-elfeed/
  (defun elfeed-search-show-entry-pre (&optional lines)
    "Return a function that will scroll n LINES in `elfeed' search results.

It will display entries without switching to them."
    (lambda (times)
      (interactive "p")
      (forward-line (* times (or lines 0)))
      (recenter)
      (call-interactively #'elfeed-search-show-entry)
      (select-window (previous-window))
      (unless elfeed-search-remain-on-entry (forward-line -1))))
  (eval-after-load 'elfeed-search
    '(define-key elfeed-search-mode-map (kbd "n")
       (elfeed-search-show-entry-pre +1)))
  (eval-after-load 'elfeed-search
    '(define-key elfeed-search-mode-map (kbd "p")
       (elfeed-search-show-entry-pre -1)))
  (eval-after-load 'elfeed-search
    '(define-key elfeed-search-mode-map (kbd "M-RET")
       (elfeed-search-show-entry-pre))))
;; End https://karthinks.com/blog/lazy-elfeed/

(use-package elfeed-org
  ;; Maintaining my RSS subscriptions in `org-mode' format.
  :straight t
  :after elfeed
  :config (elfeed-org)
  (defun jf/export-public-elfeed-opml ()
    "Export public OPML file."
    (let ((opml-body (cl-loop for org-file in '("~/git/org/denote/indices/public-elfeed.org")
                       concat
                       (with-temp-buffer
                         (insert-file-contents
                           (expand-file-name org-file org-directory))
                         (rmh-elfeed-org-convert-org-to-opml
                           (current-buffer))))))
      (with-current-buffer (find-file-noselect "~/git/takeonrules.source/static/blogroll.xml")
        (erase-buffer)
        (insert "<?xml version=\"1.0\"?>\n")
        (insert "<?xml-stylesheet type=\"text/xsl\" href=\"/blogroll.xsl\"?>\n")
        (insert "<opml version=\"1.0\">\n")
        (insert "  <head>\n")
        (insert "    <title>Take on Rules Public Blogroll</title>\n")
        (insert "  </head>\n")
        (insert "  <body>\n")
        (insert opml-body)
        (insert "  </body>\n")
        (insert "</opml>\n")
        (save-buffer))))
  (setq rmh-elfeed-org-files nil)
  (dolist (file '("~/git/org/denote/indices/public-elfeed.org"
                   "~/git/org/denote/indices/private-elfeed.org"))
    (when (f-exists? file)
      (add-to-list 'rmh-elfeed-org-files file))))

(use-package eww
  ;; A plain text browser.  Use this to see just how bad much of the web has
  ;; become.
  :straight t
  :custom (eww-auto-rename-buffer 'title)
  :config
  (setq shr-cookie-policy nil)
  (defun shr-tag-dfn (dom)
    (shr-fontize-dom dom 'italic))

  (defun shr-tag-cite (dom)
    (shr-fontize-dom dom 'italic))

  (defun shr-tag-q (dom)
    (shr-insert (car shr-around-q-tag))
    (shr-generic dom)
    (shr-insert (cdr shr-around-q-tag)))

  (defcustom shr-around-q-tag '("“" . "”")
    "The before and after quotes.  `car' is inserted before the Q-tag and `cdr' is inserted after the Q-tag.

Alternative suggestions are: - '(\"\\\"“\" . \"\\\"\")"
    :type (cons 'string 'string))

  (defface shr-small
    '((t :height 0.8))
    "Face for <small> elements.")

  ;; Drawing inspiration from shr-tag-h1
  (defun shr-tag-small (dom)
    (shr-fontize-dom dom (when shr-use-fonts 'shr-small)))

  (defface shr-time
    '((t :inherit underline :underline (:style wave)))
    "Face for <time> elements.")

  ;; Drawing inspiration from shr-tag-abbr
  (defun shr-tag-time (dom)
    (when-let* ((datetime (or
         (dom-attr dom 'title)
         (dom-attr dom 'datetime)))
    (start (point)))
      (shr-generic dom)
      (shr-add-font start (point) 'shr-time)
      (add-text-properties
       start (point)
       (list
  'help-echo datetime
         'mouse-face 'highlight))))

  ;; EWW lacks a style for article
  (defun shr-tag-article (dom)
    (shr-ensure-paragraph)
    (shr-generic dom)
    (shr-ensure-paragraph))

  ;; EWW lacks a style for section; This is quite provisional
  (defun shr-tag-section (dom)
    (shr-ensure-paragraph)
    (shr-generic dom)
    (shr-ensure-paragraph))

  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  (defun jf/reader-visual ()
    ;; A little bit of RSS beautification.
    "A method to turn on visual line mode and adjust text scale."
    (require 'olivetti)
    (olivetti-mode 1)
    (text-scale-set 2))
  :bind (:map eww-mode-map ("U" . eww-up-url))
  :bind (("C-s-o" . browse-url-at-point))
  :hook ((eww-mode . jf/reader-visual)))

(use-package stem-reading-mode
  ;; A package that emboldens word stems, helping read a bit faster.
  :straight t
  :custom (stem-reading-overlay t))

(require 'jf-versioning)
(require 'jf-quick-help)
(require 'jf-gaming)
(require 'jf-blogging)
(require 'jf-project)
(require 'jf-menus)

(use-package qrencode
  ;; https://github.com/ruediger/qrencode-el/
  ;;
  ;; Generate an plain text QRCode (or PNG but really why not use those
  ;; UTF characters)
  :straight t)

(use-package pdf-tools
  :pin manual ;; manually update
  :straight t
  :defer t
  :ensure t
  :config (pdf-tools-install)
   ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
   ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package org-noter
  :straight t
  :config (setq org-noter-doc-split-percentage '(0.67 . 0.33))
  (org-noter-enable-update-renames)
  (setq org-noter-notes-search-path '())
  (dolist (path '("~/Library/CloudStorage/ProtonDrive-jeremy@jeremyfriesen.com/"
                   "~/Documents/"))
    (when (f-dir-p path)
      ;; Add element to end of list.
      (add-to-list 'org-noter-notes-search-path path t)))
  (setq org-noter-default-notes-file-names
    '("Notes.org")))

(use-package avy-embark-collect
  :after (avy embark)
  :straight t)

(require 'dig-my-grave)

(load "~/git/dotemacs/emacs.d/random-tables-data.el")

(load (concat user-emacs-directory "hide-comnt.el") :noerror)

(use-package server
  :straight (:type built-in)
  :hook (server-visit . server-visit-hook-custom-find)
  :config
  (setq server-client-instructions nil)
  (unless (server-running-p)
    (server-start))

  ;; Connective Tissue and oddity functions:
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
      (1+ server-visit-files-custom-find:buffer-count))))

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

(use-package paragraphs
  :straight t
  :bind (("M-[" . #'backward-paragraph)
          ("s-[" . #'backward-paragraph)
          ("M-]" . #'forward-paragraph)
          ("s-]" . #'forward-paragraph)))

(use-package transient
  :straight (:type built-in)
  :config
  (transient-define-suffix jf/jump-to/agenda-local ()
    "Jump to local agenda"
    :description "Agenda (Local)"
    (interactive)
    (find-file jf/agenda-filename/local))

  (transient-define-suffix jf/shr/toggle-images ()
    "Toggle showing or hiding images"
    :description (lambda ()
                   (format "Show SHR Images (%s)"
                     (if shr-inhibit-images " " "*")))
    (interactive)
    (setq shr-inhibit-images (not shr-inhibit-images)))

  (transient-define-suffix jf/jump-to/code-backlog ()
    "Jump to coding backlog"
    :description "Capture Backlog"
    (interactive)
    (find-file jf/org-mode/capture/filename))

  (transient-define-suffix jf/org-mode/add-description (description)
    "Add Description to `org-mode'"
    :description "Add Description…"
    (interactive (list (read-string "Description: ")))
    (when (jf/org-mode/blog-entry?)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^$")
        (insert "\n#+DESCRIPTION: " description))))

  (transient-define-suffix jf/org-mode/add-session-report (date game location)
    "Add session report metadata (DATE, GAME, and LOCATION) to current buffer."
    :description "Add Session…"
    (interactive (list
                   (org-read-date
                     nil nil nil "Session Date")
                   (completing-read
                     "Game: " (jf/tor-game-list))
                   (completing-read
                     "Location: " jf/tor-session-report-location)))
    (when (jf/org-mode/blog-entry?)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^$")
        (insert "\n#+HUGO_CUSTOM_FRONT_MATTER: :sessionReport "
          "'((date . \"" date "\") (game . \"" game "\") "
          "(location . \"" location "\"))"))))

  (defun jf/org-mode/blog-entry? (&optional buffer)
    (when-let* ((buffer (or buffer (current-buffer)))
                 (file (buffer-file-name buffer)))
      (and (denote-file-is-note-p file)
        (string-match-p "\\/blog-posts\\/" file))))

  (transient-define-suffix jf/enable-indent-for-tab-command ()
    :description "Enable `indent-for-tab-command'"
    (interactive)
    (keymap-global-set "TAB" #'indent-for-tab-command))
  (transient-define-prefix jf/menu ()
    "A context specific \"mega\" menu."
    ;; Todo, can I get this section into a function so I can duplicate
    ;; it in the jf/menu--tor?
    [["Jump to"
       ("j a" jf/jump-to/agenda-local)
       ;; ("j c" "Capture Backlog" jf/jump-to/code-backlog)
       ("j d" "Denote File" jf/jump_to_corresponding_denote_file :if-derived markdown-mode)
       ("j g" "Global Mark" consult-global-mark)
       ("j h" "Hugo File" jf/jump_to_corresponding_hugo_file :if-derived org-mode)
       ("j m" "Mark" consult-mark)
       ("j r" "Jump to Git Related" consult-git-related-find-file)
       ("j l" "Jump to Magit Project Lists" magit-list-repositories)
       ;; ("j s" "Jump to Shortdoc" shortdoc-display-group)
       ;; ("j v" jf/jump-to/violet-board)
       ]
      ["Tasks"
        ("n" "Github Notifications…" gh-notify)
        ("s" "Search note content…" consult-notes-search-in-all-notes)
        ("S" "Search note filename…" consult-notes)
        ("C-t" "Start a timer…" tmr-with-description)
        ("u" jf/org-mode/agenda-files-update)]
      ["Denote"
        ("d a" jf/project/add-project-path :if jf/denote?)
        ("d c" jf/denote-org-capture/filename-set)
        ("d p" jf/project/convert-document-to-project :if jf/denote?)
        ]
      ["Blogging"
        ("b d" jf/org-mode/add-description :if jf/org-mode/blog-entry?)
        ("b r" jf/org-mode/add-session-report :if jf/org-mode/blog-entry?)
        ("b s" "Add Series…" jf/org-mode/add-series-to-file :if jf/org-mode/blog-entry?)
        ("b x" "Export to TakeOnRules…" jf/export-org-to-tor :if jf/org-mode/blog-entry?)]]
    [["Modes"
       ;; I could write functions for these, but this is concise enough
       ("m t" "Typopunct ( )" typopunct-mode :if-nil typopunct-mode)
       ("m t" "Typopunct (*)" typopunct-mode :if-non-nil typopunct-mode)
       ("m o" "MacOS Native Option ( )" jf/toggle-osx-alternate-modifier :if-non-nil ns-alternate-modifier)
       ("m o" "MacOS Native Option (*)" jf/toggle-osx-alternate-modifier :if-nil ns-alternate-modifier)
       ("m i" jf/shr/toggle-images)
       ("TAB" jf/enable-indent-for-tab-command)
       ]
      ["Grab Refs"
        ("g e" "Elfeed" jf/capture/denote/from/elfeed-show-entry :if-derived elfeed-show-mode)
        ("g f" "Firefox" jf/menu--org-capture-firefox)
        ("g s" "Safari" jf/menu--org-capture-safari)
        ("g w" "Eww" jf/capture/denote/from/eww-data :if-derived eww-mode)
        ]
      ["Bookmark"
        ("B s" "Safari" jf/menu--bookmark-safari)]])

  ;; this suffix provides a dynamic description of the current host I want to use
  ;; for my blog.  And the prefix’s function toggles the host.
  :bind ("s-1" . #'jf/menu))

(provide 'init)
  ;;; init.el ends here
