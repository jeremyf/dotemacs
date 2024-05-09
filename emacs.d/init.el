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

;; Track recent
(recentf-mode 1)

;; Quietly save the recent file list every 10 minutes.
(run-at-time nil 600 (lambda () (let ((save-silently t)) (recentf-save-list))))
(global-auto-revert-mode)

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

  ;; Exposing one additional modifier key.
  ns-right-command-modifier 'hyper
  ns-right-alternate-modifier 'meta
  line-move-visual t)

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

(require 'jf-modeline)

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

(require 'jf-windows)

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
(require 'jf-org-mode)

(use-package abbrev
  ;; The =abbrev= package is simple and powerful, providing an auto-correct
  ;; that I configure.  No more “teh” in my text.
  :straight (:type built-in)
  :custom (abbrev-file-name (file-truename
                              "~/git/dotemacs/emacs.d/abbrev_defs"))
  :hook (text-mode . abbrev-mode))

;; https://github.com/ChanderG/lam
;;
;; Create a buffer specific `abbrev'
(use-package lam
  :straight (:host github :repo "ChanderG/lam")
  :bind ("C-x l" . #'lam/control))

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
(require 'jf-framing)
(require 'jf-utility)
(require 'jf-communicating)
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

(provide 'init)
  ;;; init.el ends here
