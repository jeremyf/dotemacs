;;; init.el --- Summary:  ;;; -*- lexical-binding: t; -*-
;;
;;  Emacs configuration for Jeremy Friesen
;;
;;; Commentary:
;;
;;  This is my journey into Emacs.  Let's see where we go!  I have tried
;;  `org-mode' literate configuration, separate conceptual files, and am
;;  now creating a singular unholy init file of all the things.  I have
;;  made efforts to associate the functions/variables with their
;;  corresponding packages; either built-in or from places such as
;;  melpa.
;;
;;  How is this mess organized?  I have favored the `use-package' macro
;;  to fit most of the configuration within the `use-package' sexp.
;;  This has meant using several built-in packages, which provides some
;;  organizational meaning.  And as I get more familiar with those
;;  built-in packages, I can begin to explore their configurability.
;;
;;  This organization does mean that I'm declaring a function within the
;;  `use-package' macro that might depend on other packages.  Perhaps
;;  overtime I'll refactor accordingly.
;;
;;; Code:
(add-to-list 'load-path "~/git/dotemacs/emacs.d")

;; Let's just shunt those custom files into oblivion.
(setq custom-file (make-temp-file "emacs-custom-"))
(load custom-file :noerror)

;; https://www.reddit.com/r/emacs/comments/mtb05k/emacs_init_time_decreased_65_after_i_realized_the/
(setq straight-check-for-modifications
  '(check-on-save find-when-checking))

;; This preamble is part of straight-use-package My understanding, in
;; reading straight documentation is that it has better load
;; times. However, the configuration options I often see leverage
;; "use-package" which is why most of my package declarations look as
;; they do.
(defvar bootstrap-version nil)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el"
          user-emacs-directory))
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

;; When you open Emacs, grab all the space on the screen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(use-package gcmh
  ;; *Gcmh* does garbage collection (GC) when the user is idle.
  :straight t
  :init
  (setq
    gcmh-idle-delay 5
    gcmh-low-cons-threshold (* 1024 1024)
    gcmh-high-cons-threshold (* 16 1024 1024))
  :config (gcmh-mode)
  (add-function :after after-focus-change-function
    (defun jf/garbage-collect-maybe ()
      (unless (frame-focus-state)
        (garbage-collect)))))

(use-package emacs
  ;; Setting baseline behavior for Emacs.
  :straight (:type built-in)
  :init
  ;; And I’m going to disable a few key bindings.  These were always
  ;; messing me up a bit.  Also enable a few that I find helpful.  (I’ll
  ;; enable a lot more later).
  (unbind-key "C-z") ;; `suspend-frame'
  (unbind-key "s-o") ;; `ns-open-file-using-panel'
  (unbind-key "C-x C-c") ;; `save-buffers-kill-terminal'

  ;; Hide the icons of the Emacs toolbar
  (tool-bar-mode -1)

  ;; Ensuring I have an autosave directory.  On a few rare occassions
  ;; this has saved me from lost "work".
  (make-directory "~/.emacs.d/autosaves/" t)
  :bind (("M-[" . #'backward-paragraph)
          ("s-[" . #'backward-paragraph)
          ("H-s". #'save-buffer)
          ("M-]" . #'forward-paragraph)
          ("s-]" . #'forward-paragraph)
          ("C-k" . #'jf/kill-line-or-region)
          ("C-c n" . #'jf/yank-file-name-to-clipboard) ;; Deprecated
          ("C-c y n" . #'jf/yank-file-name-to-clipboard)
          ("M-<delete>" . #'kill-word)
          ("s-<down>" . #'end-of-buffer)
          ("s-<up>" . #'beginning-of-buffer)
          ("s-q" . #'save-buffers-kill-terminal)
          ("s-w" . #'kill-current-buffer)
          ("C-x C-b" . #'ibuffer)
          ("M-RET" . #'newline-and-indent))
  :bind (:map emacs-lisp-mode-map
          ("C-c C-c" . 'jf/eval-region-dwim))
  :config
  ;; Allow "y" or "n" to stand in for yes/no prompts
  (defalias 'yes-or-no-p 'y-or-n-p)

  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  ;; With 80 columns, I can always have two windows side-by-side.
  (setq-default fill-column 80)

  ;; Doing a bit of configuration of my cursors.  The blinking provides
  ;; the queue and on 2024-05-18, I thought I'd give a try with the
  ;; hollow box.  Why?  It feels more retro.
  (setq-default cursor-type 'bar)
  (blink-cursor-mode t)

  (setq
    ;; Don't delink hardlinks
    backup-by-copying t

    ;; On a few occassions, perhaps once every three months, I dive into
    ;; my backups to find something that 1) wasn't under version control
    ;; and 2) somehow got altered.
    backup-directory-alist '((".*" . "~/.emacs.d/backups/"))

    ;; I may as well trust themes.
    custom-safe-themes t

    ;; Don't create lock files.  It's only me on this machine...I hope.
    create-lockfiles nil

    ;; Instead of delete being gone forever, throw it in the trashbin
    ;; which I must take out
    delete-by-moving-to-trash t

    ;; Automatically delete excess backups
    delete-old-versions t

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
    kill-ring-max 128

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

  ;; These are some general configurations that I’ve slowly accumulated.
  ;; There’s inline documentation in most cases.  There might be little
  ;; bits worth teasing out but for the most part, you can move along
  ;; and reference this later.
  (setq user-full-name "Jeremy Friesen"
    user-mail-address "jeremy@jeremyfriesen.com")

  (defun jf/alist-prompt (prompt collection &rest args)
    "PROMPT from an alist COLLECTION returning a `cons'.

This is a simple wrapper around the `completing-read' function."
    (let ((string (completing-read prompt collection args)))
      (cons string (alist-get string collection nil nil #'string=))))

  (defun jf/eval-region-dwim ()
    "When region is active, evaluate it and kill the mark.

Else, evaluate the whole buffer."
    ;; I try to get quick feedback when writing emacs-lisp; the
    ;; `jf/eval-region-dwim' binds a mnemonic key sequence to an extend
    ;; `eval-region'.
    (interactive)
    (if (not (region-active-p))
      (progn
        (message "Evaluating buffer...")
        (eval-buffer))
      (progn
        (message "Evaluating region...")
        (eval-region (region-beginning) (region-end)))
      (setq-local deactivate-mark t)))

  (defun jf/kill-line-or-region (&optional arg)
    "Kill the selected region otherwise kill the ARG number of lines."
    (interactive "P")
    (if (use-region-p)
      (kill-region (region-beginning) (region-end))
      (kill-line arg)))

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
        (message "Disabling OX X native Option modifier"))))

  ;; Exposing one additional modifier key.  This opens up a significant
  ;; set of keys and no one (by default) binds to 'H-' while many bind
  ;; to 'C-c' or other 'C-' keys, leaving conflicts.
  (setq ns-right-command-modifier 'hyper
    ns-right-alternate-modifier 'meta)

  (defun jf/yank-file-name-to-clipboard (arg)
    "Nab, I mean copy, the current buffer file name to the clipboard.

  When you pass one universal prefix ARG, nab the project
  relative filename.  When you pass two or more prompt for
  different aspects of a file."
    ;; https://blog.sumtypeofway.com/posts/emacs-config.html
    (interactive "P")
    (let* ((prefix
             (car arg))
            (raw-filename
              (if (equal major-mode 'dired-mode)
                default-directory
                (buffer-file-name)))
            (filename
              (cond
                ((not prefix)
                  raw-filename)
                ((= prefix 4)
                  (concat "./"
                    (file-relative-name
                      raw-filename (projectile-project-root))))
                ((>= prefix 16)
                  (let ((options
                          '(("Filename, Basename" .
                              (lambda (f) (file-name-nondirectory f)))
                             ("Filename, Project Relative" .
                               (lambda (f)
                                 (concat "./"
                                   (file-relative-name f
                                     (projectile-project-root)))))
                             ("Filename, Full" .
                               (lambda (f) (f)))
                             ("Dirname" .
                               (lambda (f) (file-name-directory f)))
                             ("Dirname, Project Relative" .
                               (lambda (f)
                                 (concat "./"
                                   (file-relative-name
                                     (file-name-directory f)
                                     (projectile-project-root))))))))
                    (funcall
                      (alist-get
                        (completing-read "Option: " options nil t)
                        options nil nil #'string=)
                      raw-filename))))))
      (when filename
        (kill-new filename)
        (message
          "Copied buffer file name '%s' to the clipboard."
          filename)))))

(use-package subword
  ;; With subword-mode, HelloWorld is two words for navigation.
  :straight (:type built-in)
  :config
  (global-subword-mode))

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
      (setenv "LIBRARY_PATH"
        (concat (getenv "LIBRARY_PATH")
          (when (getenv "LIBRARY_PATH")
            ":")
          ;; This is where Homebrew puts gcc libraries.
          (car (file-expand-wildcards
                 "/opt/homebrew/lib/gcc/*"))))
      ;; Only set after LIBRARY_PATH can find gcc libraries.
      (setq comp-deferred-compilation t))
    (message "Native comp is *not* available")))

(use-package recentf
  ;; The probability of me needing to open a file I "recently" opened is
  ;; high.  Providing a list of those files for easy searching helps me
  ;; navigate tasks that I'm working on over the course of a week or
  ;; more.
  :straight (:type built-in)
  :config
  (setq recentf-max-menu-items 50
    recentf-max-saved-items 256)
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

(use-package grep
  ;; The most important switch is moving from 'grep' to 'ripgrep', a
  ;; fast near drop-in replacement for 'grep'; most notable on large
  ;; directories.
  :straight (:type built-in)
  :config
  (when (executable-find "rg")
    (setq grep-program "rg")))

(defun jf/sort-unique-lines (reverse beg end
                              &optional adjacent
                              keep-blanks
                              interactive)
  "Sort lines and delete duplicates.

  By default the sort is lexigraphically ascending.  To sort as
  descending set REVERSE to non-nil.  Specify BEG and END for the
  bounds of sorting.  By default, this is the selected region.

  I've included ADJACENT, KEEP-BLANKS, and INTERACTIVE so I can
  echo the method signature of `sort-lines' and
  `delete-duplicate-lines'"
  (interactive "P\nr")
  (sort-lines reverse beg end)
  (delete-duplicate-lines
    beg end reverse adjacent keep-blanks interactive))

(use-package ediff
  ;; I haven't used `ediff' much, but it's a good option for reviewing
  ;; file deltas.
  :straight (:type built-in)
  :config
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package dired
  ;; Oh `dired', you are a super powered directory editor.  For years I
  ;; avoided interacting with you, but overtime I've practiced and find
  ;; your utility great.  Never conflate a file with a buffer and
  ;; instead consider the power of what a buffer can do.
  :straight (:type built-in)
  :custom (dired-listing-switches "-laGhpX")
  (dired-use-ls-dired t)
  :config
  ;; When two dired buffers are open and you mark then rename a file, it
  ;; assume's you're moving the file from the one buffer to the other.
  ;; Very useful.
  (setq dired-dwim-target t)
  (setq dired-vc-rename-file t)
  (with-eval-after-load 'dired
    ;; Bind dired-x-find-file.
    (setq dired-x-hands-off-my-keys nil)
    (require 'dired-x))
  :hook (dired-mode . dired-hide-details-mode))

(use-package info
  ;; A lot of offline documentation resides in the venerable `info'
  ;; format.  This allows me to access the info docs that are part of
  ;; brew installed packages.
  :straight (:type built-in)
  :config
  (info-initialize)
  (push "/opt/homebrew/share/info" Info-directory-list))

(use-package expand-region
  ;; A simple package that does two related things really well; expands
  ;; and contracts the current region.  I use this all the time.
  ;;
  ;; In writing, with the cursor at point, when I expand it selects the
  ;; word.  The next expand the sentence, then paragraph, then page.  In
  ;; programming it leverages sexp.
  :straight (:host github :repo "jeremyf/expand-region.el")
  :bind (("C-=" . er/expand-region)
          ("C-+" . er/contract-region)))

(use-package display-fill-column-indicator
  ;; It's nice to have a gentle reminder showing me the recommended
  ;; column width for the current buffer.
  :straight (:type built-in)
  :hook (prog-mode . display-fill-column-indicator-mode))

(use-package kind-icon
  ;; This packages helps provide additional icons for functions and
  ;; variables in the completion candidates.
  :straight t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  ;; Have background color be the same as `corfu' face background
  (kind-icon-default-face 'corfu-default)
  ;; Use midpoint color between foreground and background colors
  ;; ("blended")?
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  ;; directory that defaults to the `user-emacs-directory'. Here, I
  ;; change that directory to a location appropriate to `no-littering'
  ;; conventions, a package which moves directories of other packages to
  ;; sane locations.  (svg-lib-icons-dir
  ;; (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change
  ;; cache dir
  :config
  ;; Enable-Recursive-Minibuffers `kind-icon'
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package lin
  ;;  “LIN locally remaps the hl-line face to a style that is optimal
  ;;  for major modes where line selection is the primary mode of
  ;;  interaction.”  In otherwords, ~lin.el~ improves the highlighted
  ;;  line behavior for the competing contexts.
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
  :bind (("C-c C-l" . jf/pulse)))

(defun jf/pulse (&optional parg)
  "Pulse the current line.

  When PARG pulse between `point' and `mark'."
  (interactive "P")
  (if (car parg)
    (pulsar--pulse nil nil (point) (mark))
    (pulsar-pulse-line)))

;; Silence that bell by pulsing the line instead
(setq ring-bell-function 'jf/pulse)

(use-package rainbow-mode
  ;; When I toggle on Rainbow mode, it colorizes the text that is color
  ;; names and hex declarations (e.g. "#0000ff" ).  Most useful when
  ;; working with CSS, but sometimes non-CSS files have those
  ;; declarations as well.
  :straight t)

(use-package rainbow-delimiters
  ;; A quick and useful visual queue for paranthesis.
  :straight t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(use-package recursion-indicator
  ;; I vascilate between yes and no; but invariably find myself stuck in
  ;; a recursed buffer.
  :straight t
  :demand t
  :init
  (setq enable-recursive-minibuffers t)
  :config
  (recursion-indicator-mode))

(use-package vi-tilde-fringe
  ;; Show tilde (e.g. ~\~~) on empty trailing lines.  This is a feature
  ;; ported from https://en.wikipedia.org/wiki/Vi
  :straight t
  :config (global-vi-tilde-fringe-mode))

(use-package whole-line-or-region
  ;; From the package commentary, “This minor mode allows functions to
  ;; operate on the current line if they would normally operate on a
  ;; region and region is currently undefined.”  I’ve used this for
  ;; awhile and believe it’s not baked into my assumptions regarding how
  ;; I navigate Emacs.
  :straight t
  :config (whole-line-or-region-global-mode))

(use-package keycast
  ;; It can be helpful when pairing or presenting to have a log of your
  ;; key cominations.
  :straight t
  :init
  (setq keycast-mode-line-insert-after
    'jf/mode-line-format/buffer-name-and-status)
  (setq keycast-mode-line-remove-tail-elements nil)
  (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
  (setq keycast-mode-line-format "%2s%k%2s(%c%R)"))

(use-package emacs
  :straight (:type built-in)
  :after (projectile)
  :preface
  (defvar-local jf/mode-line-format/kbd-macro
    '(:eval
       (when (and (mode-line-window-selected-p) defining-kbd-macro)
         (propertize " KMacro " 'face 'mode-line-highlight))))

  (defvar-local jf/mode-line-format/buffer-name-and-status
    '(:eval
       (let ((name
               (buffer-name)))
         (propertize
           (if buffer-read-only
             (format " %s %s " (char-to-string #xE0A2) name)
             name)
           'face
           (if (mode-line-window-selected-p)
             'mode-line-buffer-id
             'mode-line-inactive)))))

  (defun jf/mode-line-format/major-mode-indicator ()
    "Return propertized mode line indicator for the major mode."
    (let ((indicator
            (cond
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
    (propertize (capitalize
                  (string-replace "-mode" "" (symbol-name major-mode)))
      'face (if (mode-line-window-selected-p)
              'mode-line 'mode-line-inactive)))

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
    (let ((map
            (make-sparse-keymap)))
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
                    (file
                      (if (equal major-mode 'dired-mode)
                        default-directory
                        (buffer-file-name)))
                    (backend
                      (or (vc-backend file) 'git))
                    (branch
                      (jf/mode-line-format/vc-branch-name
                        file backend)))
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
    (let ((map
            (make-sparse-keymap)))
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
       (when (and (fboundp 'projectile-project-p) (projectile-project-p))
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
  :custom (aw-keys '(?a ?s ?d ?f ?g ?j ?h ?k ?l))
  :bind (("M-o" . ace-window) ;; deprecated
          ("s-o" . ace-window)))

(use-package avy
  ;; Pick a letter, avy finds all words with that at the beginning of
  ;; it.  Narrow results from there.
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
  ;; highlights the visible URLs, providing quick keys to then open
  ;; those URLs.  If there's only one candidate, the function opens that
  ;; URL.
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
      (link-hint-open-link)))

  (defun jf/link-hint--apply (advised-function func &rest args)
    "Hijack opening `org-mode' URLs by attempting to open local file.

The ADVISED-FUNCTION is the original `link-hint--apply'.

The FUNC is the dispatched function for handling the link
type (e.g. `link-hint--open-org-link').

The ARGS are the rest of the ARGS passed to the ADVISED-FUNCTION."
    (if (and
          (eq 'link-hint--open-org-link func)
          (eq :open (caddr args)))
      (progn
        (if-let* ((url
                    (car args))
                   (_match
                     (string-match
                       (concat "^https://github.com/"
                         "\\([^/]+\\)/\\([^/]+\\)" ;; org/repo
                         "/[^/]+/[^/]+/" ;; blob/sha
                         "\\([^#]+\\)" ;; path to fil
                         "\\(#L\\([0-9]+\\)\\)?") ;; line number
                       url)))
          ;; Due to my present file structure I have some repositories
          ;; in ~/git/ and others in ~/git/sub-dir
          ;;
          ;; In most every case, the Github org and repo match the
          ;; remote URL.
          (let ((filename-without-org
                  (format "~/git/%s/%s"
                    (match-string 2 url)
                    (match-string 3 url)))
                 (filename-with-org
                   (format "~/git/%s/%s/%s"
                     (match-string 1 url)
                     (match-string 2 url)
                     (match-string 3 url)))
                 (line-number
                   (match-string 5 url)))
            (cond
              ((f-exists? filename-without-org)
                (progn
                  (find-file-other-window filename-without-org)
                  (when line-number
                    (goto-char (point-min))
                    (forward-line
                      (1- (string-to-number line-number))))))
              ((f-exists? filename-with-org)
                (progn
                  (find-file-other-window filename-with-org)
                  (when line-number
                    (goto-char (point-min))
                    (forward-line
                      (1- (string-to-number line-number))))))
              (t (funcall func args))))

          (funcall func args)))
      (apply advised-function func args)))

  (advice-add 'link-hint--apply
    :around #'jf/link-hint--apply))

(use-package browse-at-remote
  ;; Because I sometimes want to jump to the source code.  And in
  ;; looking at this I learned about vc-annotate; a better blame than
  ;; what I've had before.  `bar-browse' is faster than
  ;; `browse-at-remote'.
  :straight t
  :after (link-hint)
  :bind
  ;; Note this is in the same prefix space as `link-hint'
  ("C-c l r" . browse-at-remote)
  ("C-c l a" . vc-annotate)
  ("C-c l n" . jf/project/jump-to/notes)
  ("C-c l t" . git-timemachine))

(use-package fontaine
  ;; A narrow focus package for naming font configurations and then
  ;; selecting them.
  :straight t
  :config
  (setq fontaine-presets
    ;; I'm naming the presets as "actions"; the mindset that I'm using
    ;; when wanting that font.
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

(use-package nerd-icons
  :straight t
  :config (setq nerd-icons-font-family "IntoneMono Nerd Font Mono"))

(use-package all-the-icons
  ;; It's nice to see icons as a quick visual helper.
  :straight t)

(use-package all-the-icons-dired
  ;; Incorporates file icons with file listings of dired.  /Note/: On
  ;; 2021-04-11 I was getting the following error with this package:
  ;; "*ERROR*: Symbol's value as variable is void: file"
  :straight t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package typopunct
  ;; A package that provides some automatic replacement of strings of
  ;; keys.  For example in text-mode, when I type three periods
  ;; (e.g. “.”) typopunct replaces that with an ellipsis (e.g. “…”)
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
          (let ((skeleton-end-newline
                  nil)
                 (singleo
                   (typopunct-opening-single-quotation-mark lang))
                 (singleq
                   (typopunct-closing-single-quotation-mark lang)))
            (if (> (point) (mark))
              (exchange-point-and-mark))
            (save-excursion
              (while (re-search-forward
                       (regexp-quote (string omark)) (mark) t)
                (replace-match
                  (regexp-quote (string singleo)) nil nil)))
            (save-excursion
              (while (re-search-forward
                       (regexp-quote (string qmark)) (mark) t)
                (replace-match
                  (regexp-quote (string singleq)) nil nil)))
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
      (let ((was-full-height
              (window-full-height-p)))
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
       ;; the `org-capture' key selection and `org-add-log-note'
       ("\\*Org \\(Select\\|Note\\)\\*"
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
         (display-buffer-reuse-mode-window
           display-buffer-below-selected)
         (window-height . fit-window-to-buffer)
         (window-parameters . ((no-other-window . t)
                                (mode-line-format . (" %b")))))
       ("\\*\\(Output\\|Register Preview\\).*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom))
       ;; below current window
       ("\\(\\*Capture\\*\\|CAPTURE-.*\\)"
         (display-buffer-reuse-mode-window
           display-buffer-below-selected))
       ("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
         (display-buffer-reuse-mode-window
           display-buffer-below-selected)
         (window-height . 0.1)
         (dedicated . t)
         (preserve-size . (t . t)))
       ("\\*rspec-compilation\\*"
         (display-buffer-reuse-mode-window
           display-buffer-below-selected)
         (body-function . jf/body-function/rspec-compilation))
       ((derived-mode . reb-mode) ; M-x re-builder
         (display-buffer-reuse-mode-window
           display-buffer-below-selected)
         (window-height . 4) ; note this is literal lines, not relative
         (dedicated . t)
         (preserve-size . (t . t)))
       ((or . ((derived-mode . occur-mode)
                (derived-mode . grep-mode)
                (derived-mode . rg-mode)
                (derived-mode . Buffer-menu-mode)
                (derived-mode . log-view-mode)
                (derived-mode . help-mode)
                "\\*\\(|Buffer List\\|Occur\\|vc-change-log\\).*"))
         (prot-window-display-buffer-below-or-pop)
         (dedicated . t)
         (body-function . prot-window-select-fit-size))
       ("\\*\\(Calendar\\|Bookmark Annotation\\|ert\\).*"
         (display-buffer-reuse-mode-window
           display-buffer-below-selected)
         (dedicated . t)
         (window-height . fit-window-to-buffer))
       ;; NOTE 2022-09-10: The following is for `ispell-word', though
       ;; it only works because I override `ispell-display-buffer'
       ;; with `prot-spell-ispell-display-buffer' and change the
       ;; value of `ispell-choices-buffer'.
       ("\\*ispell-top-choices\\*.*"
         (display-buffer-reuse-mode-window
           display-buffer-below-selected)
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

(mapc #'disable-theme custom-enabled-themes)

(use-package ef-themes
  :straight t
  :init
  (defvar jf/themes-plist '()
    "The named themes by pallette.")
  :config
  (setq ef-themes-common-palette-overrides
      '((bg-region bg-green-intense)
        (fg-region fg-main)))
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
        `(font-lock-regexp-face
           ((,c :foreground ,red))))))
  ;; I had '(:light ef-cyprus) but the differentiation between function
  ;; and comment was not adequate
  (setq jf/themes-plist '(:dark ef-bio :light ef-elea-light)))

(use-package custom
  :straight (:type built-in)
  :config
  ;; In organizing the packages, I discovred that themes is part of the
  ;; `custom' package.
  (defun jf/emacs-theme-by-osx-appearance ()
    "Function to load named theme."
    (ef-themes-select
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
        "to set dark mode to not dark mode'"))
    (jf/emacs-theme-by-osx-appearance))
  (jf/emacs-theme-by-osx-appearance))

(use-package xref
  ;; Cross-referencing commands.  I suspect there's a lot more that I
  ;; could use to improve my usage.
  :straight (:type built-in)
  :custom
  (xref-file-name-display 'project-relative)
  (xref-search-program 'ripgrep))

(use-package tmr
  ;; A timer package.
  ;;
  ;; My dbus install is not behaving so I'm cheating with a bit of
  ;; AppleScript
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
    (list #'tmr-print-message-for-completed-timer
      #'tmr-sound-play #'jf/tmr-notification-notify)
    nil nil "Customized with use-package tmr")
  :straight (:host github :type git
              :repo "protesilaos/tmr"))

(use-package transient
  ;; A package for creating turbo-charged menus.  It is the backbone for
  ;; the menu-like dispatch of `magit' functionality.
  :straight t
  ;; This exposes the --sign switch for git commit
  :config (setq transient-default-level 5))

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
  ;; “Edit a grep buffer and apply those changes to the file buffer.”
  ;; In other words, after “searching” for something, sending the
  ;; results to a buffer (via `embark-export' or such thing), you can
  ;; edit that search results buffer and propogate the changes to the
  ;; locations of the elements that matched the search.
  ;;
  ;;   1.  Call `consult-ripgrep' (via ~C-c f~) to search for something.
  ;;   2.  Call `embark-export' (via ~C-s-e~) to export to a grep
  ;;       buffer.
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
  ;; https://github.com/dajva/rg.el/issues/142 Give focus to *rg*
  ;; buffer.
  (add-to-list 'rg-finish-functions
    (lambda (buffer _) (pop-to-buffer buffer)))

  ;; Override the baseline rg-project to include files
  (rg-define-search rg-project
    :dir project
    :files "*.*")

  ;; Prompt for file types
  (rg-define-search rg-project-prompt-for-files
    :dir project
    :files (concat
             "`rg \"^// Code generated .* DO NOT EDIT\\.$\" "
             project
             " --files-without-matches --glob \\!vendor`")
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

(rg-define-search rg-project-not-generated
  "Search only within files that are not generated."
  :files (jf/rg-get-not-generated-file-names)
  :dir project
  :flags '("--glob=\"\\!vendor/\"")
  :menu ("Search" "n" "Not Generated"))

(defun jf/rg-get-not-generated-file-names ()
  "Create a custom type glob for files that were not generated."
  (format "{%s}"
    (string-trim
      (shell-command-to-string
        (concat
          "rg -e \"^// Code generated .*DO NOT EDIT\\.$\" . "
          "--files-without-match --glob=\\!vendor | tr '\\n' '\\,' "
          " | sed 's|,$||' | sed 's|\\./||g'" )))))


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
          ("C-M-d" . jf/duplicate-current-line-or-lines-of-region)
          ("C-c d" . jf/duplicate-current-line-or-lines-of-region)
          ("<f12>" . jf/create-scratch-buffer))
  :config
  (cl-defun jf/create-scratch-buffer (&optional arg)
    "Create `scratch' buffer; create `denote' scratch when ARG given."
    ;; A simple wrapper around scratch, that helps name it and sets the
    ;; major mode to `org-mode'.
    (interactive "P")
    (if (car arg)
      (jf/denote/create-scratch (format-time-string "%Y-%m-%d Scratch"))
      (progn
        (crux-create-scratch-buffer)
        (rename-buffer (concat "*scratch* at "
                         (format-time-string "%Y-%m-%d %H:%M")))
        (org-mode))))
  (defun jf/duplicate-current-line-or-lines-of-region (arg)
    "Duplicate current line region ARG times."
    ;; Sometimes I just want to duplicate an area without copy and
    ;; paste.  This helps that process.  It’s not as smart as TextMate’s
    ;; equivalent function, but it’s close enough.
    (interactive "p")
    (if (use-region-p)
      (progn
        (when (> (point) (mark))
          (exchange-point-and-mark))
        (beginning-of-line)
        (exchange-point-and-mark)
        (end-of-line)
        (goto-char (+ (point) 1))
        (exchange-point-and-mark)
        (let* ((end
                 (mark))
                (beg
                  (point))
                (region
                  (buffer-substring-no-properties beg end)))
          (dotimes (_i arg)
            (goto-char end)
            (insert region)
            (setq end (point)))))
      (crux-duplicate-current-line-or-region arg))))

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

(use-package htmlize
  ;; This package helps me convert an inline `org-mode' buffer to RTF
  ;; and paste into other contexts (Google Docs for example) without
  ;; worrying about an explicit export and copy.
  :straight t
  :bind ("C-M-s-c" . jf/formatted-copy-org-to-html)
  :config
  ;; The following functions build on both org and the htmlize package.
  ;; I define them as part of the config because without the package
  ;; these won't work.
  ;;
  ;; For this to work, I needed to permit my \"~/bin/emacsclient\" in
  ;; the Security & Privacy > Accessibility system preference.
  ;;
  ;; http://mbork.pl/2021-05-02_Org-mode_to_Markdown_via_the_clipboard
  (defun jf/org-copy-region-as-markdown ()
    "Copy the region (in Org) to the system clipboard as Markdown."
    (interactive)

    (if (use-region-p)
      (let* ((region
               (buffer-substring-no-properties
                 (region-beginning)
                 (region-end)))
              (markdown
                (org-export-string-as region 'md t '(:with-toc nil))))
        (gui-set-selection 'CLIPBOARD markdown))))

  ;; I have found that Slack resists posting rich content, so I often
  ;; need to open up TextEdit, paste into an empty file, copy the
  ;; contents, and then paste into Slack.
  (defun jf/formatted-copy-org-to-html (prefix)
    "Export region to HTML, and copy it to the clipboard.

When given the PREFIX arg, paste the content into TextEdit (for
future copy)."
    (interactive "P")
    (save-window-excursion
      (let* ((buf
               (org-export-to-buffer
                 'html "*Formatted Copy*" nil nil t t))
              (html (with-current-buffer buf (buffer-string))))
        (with-current-buffer buf
          (shell-command-on-region
            (point-min)
            (point-max)
            (concat "textutil -inputencoding UTF-8 -stdout -stdin "
              "-format html -convert rtf | pbcopy")))
        (kill-buffer buf)
        ;; Paste into TextEdit
        (when (car prefix)
          (ns-do-applescript
            (concat
              "tell application \"TextEdit\"\n"
              "\tactivate\n"
              "\tset myrtf to the clipboard as «class RTF »\n"
              "\tset mydoc to make new document\n"
              "\tset text of mydoc to myrtf\n"
              "end tell")))))))

(use-package org
  ;; Begin Org Mode (all it's glory)
  ;;
  ;; - Time tracking
  ;; - Note taking
  ;; - Exporting notes to different formats
  ;; - Capturing information
  ;; - Mixing code and prose to generate technical documentation
  ;;
  ;; And I'm sure much more
  :preface
  (require 'cl-lib)
  :straight (org :type git :host github :repo "emacsmirror/org")
  :hook (org-mode . jf/org-mode/configurator)
  :bind (("C-c C-j" . jf/project/jump-to-task)
          ("C-c C-x C-j" . org-clock-goto)
          ("s-5" . jf/org-insert-immediate-active-timestamp)
          )
  :bind (:map org-mode-map (("C-c j" . org-goto)
                             ("C-c C-j" . jf/project/jump-to-task)
                             ("C-x n t" . jf/org-mode/narrow-to-date)
                             ("C-j" . avy-goto-char-timer)))
  :config
  (org-clock-persistence-insinuate)
  (setq org-use-speed-commands t)
  (setq org-agenda-clockreport-parameter-plist
    '(:link t :maxlevel 2 :stepskip0 t
       :fileskip0 t :filetitle t :tags t))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-goto-interface #'outline-path-completion)
  (setq org-time-stamp-rounding-minutes '(0 15))
  (setq org-clock-rounding-minutes 15)
  (setq org-link-frame-setup
    '((vm . vm-visit-folder-other-frame)
       (vm-imap . vm-visit-imap-folder-other-frame)
       (gnus . org-gnus-no-new-news)
       (file . find-file)
       (wl . wl-other-frame)))
  (setq org-babel-ruby-wrapper-method
    (concat "results = self.instance_exec { %s } "
      "File.open('%s', 'w') { |f| "
      " f.write((results.class == String) ? results : results.inspect) "
      "}"))

  (setq org-babel-ruby-pp-wrapper-method "require 'pp'
results = self.instance_exec { %s }
File.open('%s', 'w') { |f| $stdout = f; pp results }")
  (setq org-clock-persist 'history)
  (setq org-export-headline-levels 4)
  ;; When I would load the agenda, I'd invariably type "l" to list the
  ;; entries.
  (setq org-agenda-start-with-log-mode t)
  ;; I continue to encounter issues with not properly generating table
  ;; of contents.  As such I used the following:
  ;;
  ;; https://emacs.stackexchange.com/questions/76255/why-is-the-toc-missing-in-org-mode-latex-output
  ;;
  ;; Note, I did change from pdflatex to lualatex as the LaTeX class I'm
  ;; often using are only available in Lua processing.
  ;;
  ;; See https://orgmode.org/worg/org-tutorials/org-latex-export.html#sec-12-3 for why 3
  (setq org-latex-pdf-process
    '("lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
       "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
       "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; The 'minted backend provides souce code highlighting.
  (setq
    org-latex-src-block-backend 'minted
    org-latex-compiler "lualatex"
    org-latex-custom-lang-environments
    '((emacs-lisp "common-lispcode"))
    org-latex-minted-options '(("frame" "lines")
                                ("fontsize" "\\footnotesize")
                                ("linenos" "")))

  (setq
    org-auto-align-tags nil
    org-tags-column nil

    org-confirm-babel-evaluate #'jf/org-confirm-babel-evaluate
    org-fontify-quote-and-verse-blocks t
    ;; I'd prefer to use the executable, but that doe not appear to be
    ;; the implementation of org-babel.
    org-plantuml-jar-path (concat
                            (string-trim
                              (shell-command-to-string
                                "brew-path plantuml"))
                            "/libexec/plantuml.jar")
    org-insert-heading-respect-content t
    org-catch-invisible-edits 'show-and-error
    org-use-fast-todo-selection 'expert
    org-log-into-drawer t
    org-imenu-depth 4
    org-hide-emphasis-markers t
    ;; turning off org-elements cache speeds up input latency
    ;; See https://www.reddit.com/r/emacs/comments/11ey9ft/weekly_tips_tricks_c_thread/
    org-element-use-cache nil
    org-export-with-sub-superscripts '{}
    org-pretty-entities t
    org-pretty-entities-include-sub-superscripts nil
    org-agenda-log-mode-items '(clock)
    org-directory (file-truename "~/git/org")
    ;; org-agenda-files (jf/org-mode/agenda-files)
    org-default-notes-file (concat
                             org-directory
                             "/captured-notes.org")
    org-log-done 'time
    org-todo-keywords '((type "TODO(t)"
                          "STARTED(s!)"
                          "BLOCKED(b@/!)"
                          "WAITING(w@/!)"
                          "|"
                          "CANCELED(c@/!)"
                          "DONE(d!)")))
  (defun jf/org-capf ()
    "The `completion-at-point-functions' I use for `org-mode'."
    (setq-local completion-at-point-functions
      (list (cape-capf-super
              #'jf/org-capf-links
              #'jf/org-capf-macros
              #'tempel-expand
              #'cape-file))))
  ;; Cribbed from `org-roam' org-roam-complete-link-at-point
  (defun jf/org-capf-links ()
    "Complete links."
    (when (and (thing-at-point 'symbol)
            (not (org-in-src-block-p))
            (not (save-match-data (org-in-regexp org-link-any-re))))
      ;; We want the symbol so that links such performing completion on
      ;; "org-mode" will look for links with the text of org-mode and
      ;; then replace the text "org-mode" with the returned link.
      (let ((bounds
              (bounds-of-thing-at-point 'symbol)))
        (list (car bounds) (cdr bounds)
          ;; Call without parameters, getting a links (filtered by CAPF
          ;; magic)
          (jf/org-links-with-text)
          :exit-function
          (lambda (text _status)
            ;; We want the properties of that link.  In the case of one
            ;; match, the provided text will have the 'link property.
            ;; However if the
            (let ((link
                    (car (jf/org-links-with-text text))))
              (delete-char (- (length text)))
              (insert
                "[[" (get-text-property 0 'link link) "]"
                "[" text "]]")))
          ;; Proceed with the next completion function if the returned
          ;; titles do not match. This allows the default Org capfs or
          ;; custom capfs of lower priority to run.
          :exclusive 'no))))

  (defun jf/org-capf-macros ()
    "Complete macros.

And 3 shall be the magic number."
    ;; We're looking backwards for one to three '{' characters.
    (when (and (looking-back "{+" 3)
            (not (org-in-src-block-p)))
      (list
        ;; The beginning of the match is between 1 and 3 characters
        ;; before `point'.  The math works out: `point' - 3 + number of
        ;; '{' we found
        (+ (- (point) 3)
          (string-match "{+" (buffer-substring (- (point) 3) (point))))
        (point)
        ;; Call without parameters, getting all of the macros.
        (jf/org-macros)
        :exit-function
        (lambda (text _status)
          (let ((macro
                  (car (jf/org-macros text))))
            (delete-char (- (length text)))
            (insert macro)))
        ;; Proceed with the next completion function if the returned
        ;; titles do not match. This allows the default Org capfs or
        ;; custom capfs of lower priority to run.
        :exclusive 'no)))

  (defun jf/org-macros (&optional given-macro)
    (let ((macros
            (jf/regexp-matches-for-text "{{{[^{}]+}}}")))
      (if given-macro
        (when (member given-macro macros) (list given-macro))
        macros)))

  (defun jf/regexp-matches-for-text (regexp &optional string)
    "Get a list of all REGEXP matches in the STRING."
    (save-match-data
      (seq-uniq
        (let ((string
                (or string (buffer-string)))
               (pos 0) matches)
          (while (string-match regexp string pos)
            (let ((text
                    (match-string 0 string)))
              (set-text-properties 0 (length text) nil text)
              (push text matches))
            (setq pos (match-end 0)))
          matches))))

  (defun jf/org-links-with-text (&optional given-link)
    "Return the `distinct-' `org-mode' links in the `current-buffer'.

Each element of the list will be a `propertize' string where the
string value is the text of the link and the \"link\" property
will be the :raw-link.

When provided a GIVEN-LINK stop processing when we encounter the
first matching link."
    (let ((links
            (org-element-map
              (org-element-parse-buffer)
              'link
              (lambda (link)
                (when-let* ((left
                              (org-element-property
                                :contents-begin link))
                             (right
                               (org-element-property
                                 :contents-end link)))
                  (let ((returning
                          (propertize
                            (buffer-substring-no-properties
                              left
                              right)
                            'link (org-element-property
                                    :raw-link link))))
                    (if given-link
                      (when (string= given-link returning) returning)
                      returning))))
              nil
              given-link)))
      ;; Ensure that we have a distinct list.
      (if (listp links)
        (seq-uniq links)
        (list links))))

  (defun jf/org-insert-immediate-active-timestamp (arg)
    "Insert an active date for today.

  One universal ARG prompts for date
  Two universal ARG inserts timestamp.
  then insertes active date."
    ;; Insert an active timestamp, with a few options.
    (interactive "P")
    (let ((prefix
            (car arg)))
      (cond
        ((not prefix)
          (org-insert-time-stamp nil nil nil))
        ((= prefix 4)
          (org-insert-time-stamp (org-read-date nil t nil "Date")
            nil nil))
        ((>= prefix 16)
          (org-insert-time-stamp nil t nil)))))

  (defun jf/org-mode/agenda-files ()
    "Return a list of note files containing 'agenda' tag.

Uses the fd command (see https://github.com/sharkdp/fd)

We want files to have the 'projects' `denote' keyword."
    (let ((projects
            (mapcar (lambda (el) (cdr el)) (jf/project/list-projects))))
      ;; (dolist (file (jf/journal/list-current-journals))
      ;;   (setq projects (cons file projects)))
      ;; (when (file-exists-p jf/agenda-filename/scientist)
      ;;   (setq projects (cons jf/agenda-filename/scientist projects)))
      (when (file-exists-p jf/agenda-filename/local)
        (setq projects (cons jf/agenda-filename/local projects)))
      projects))

  ;; (defun jf/journal/list-current-journals ()
  ;;   "Return the last 14 daily journal entries."
  ;;   (split-string-and-unquote
  ;;     (shell-command-to-string
  ;;       (concat
  ;;         "fd _journal --absolute-path "
  ;;         denote-journal-extras-directory " | sort | tail -14"))
  ;;   "\n"))

  (defun jf/org-mode/capture/project-task/find ()
    "Find the project file and position to the selected task."
    (let* ((project
             (completing-read "Project: " (jf/project/list-projects)))
            (filename
              (cdar (jf/project/list-projects :project project)))
            (name-and-task
              (jf/alist-prompt
                (format "Task for %s: " project)
                (jf/org-mode/existing-tasks filename)))
            (task-name
              (car name-and-task)))
      ;; Defer finding this file as long as possible.
      (find-file filename)

      (if-let ((task (cdr name-and-task)))
        ;; I like having the most recent writing close to the headline;
        ;; showing a reverse order.  This also allows me to have
        ;; sub-headings within a task and not insert content and clocks
        ;; there.  (if-let ((drawer (car (org-element-map task 'drawer
        ;; #'identity)))) (goto-char (org-element-property :contents-end
        ;; drawer)) (goto-char (org-element-property :contents-begin
        ;; task)))
        (let* ((name-and-subtask
                 (jf/alist-prompt
                   (format "Sub-Task for %s: " task-name)
                   (jf/org-mode/existing-sub-tasks :task task)))
                (subtask-name
                  (car name-and-subtask)))
          (if-let ((subtask (cdr name-and-subtask)))
            (goto-char (org-element-property :contents-end subtask))
            (if current-prefix-arg
              ;; We don't want to edit this thing
              (goto-char (org-element-property :begin task))
              (progn
                (goto-char (org-element-property :contents-end task))
                (insert "** " subtask-name "\n\n")))))
        (progn
          (goto-char (point-max))
          ;; Yes make this a top-level element.  It is easy to demote
          ;; and move around.
          (insert "* TODO " task-name " :tasks:\n\n")))))

  (defun jf/org-mode/existing-tasks (&optional filename)
    "Return an alist of existing tasks in given FILENAME.

Each member's `car' is title and `cdr' is `org-mode' element.

Members of the sequence either have a tag 'tasks' or are in a
todo state."
    (with-current-buffer (or (and filename
                               (find-file-noselect filename))
                           (current-buffer))
      (mapcar (lambda (headline)
                (cons (org-element-property :title headline) headline))
        (org-element-map
          (org-element-parse-buffer 'headline)
          'headline
          (lambda (headline)
            (and
              (or (eq (org-element-property :todo-type headline) 'todo)
                (member "tasks" (org-element-property :tags headline)))
              headline))))))

  (cl-defun jf/org-mode/existing-sub-tasks (&key task)
    "Return an alist of existing sub-tasks for the given TASK element.

Each member's `car' is title and `cdr' is `org-mode' element."
    (let ((subtask-level
            (+ 1 (org-element-property :level task))))
      (mapcar (lambda (headline)
                (cons (org-element-property :title headline) headline))
        (org-element-map
          task
          'headline
          (lambda (headline)
            (and
              (eq (org-element-property :level headline) subtask-level)
              headline))))))

  (defun jf/org-mode/configurator ()
    (add-hook 'org-mode-hook
      (lambda ()
        "Add localized hooks for `org-mode'."
        (add-hook 'before-save-hook
          #'jf/org-mode/recalculate-buffer-tables nil :local)
        (add-hook 'before-save-hook
          #'jf/org-add-ids-to-headlines-in-file nil 'local)
        (add-hook 'focus-out-hook
          #'org-save-all-org-buffers nil :local)))
    (jf/org-capf)
    (setq-local tab-width 8)
    (turn-on-visual-line-mode)
    (electric-pair-mode -1))
  (defun jf/denote-org-capture ()
    "An org-capture conformant function for capturing to a blog-post."
    (if denote-last-path
      denote-org-capture-specifiers
      (let ((denote-directory
              (f-join denote-directory "blog-posts")))
        (denote-org-capture))))

  ;; https://stackoverflow.com/questions/13340616/assign-ids-to-every-entry-in-org-mode
  (defun jf/org-add-ids-to-headlines-in-file ()
    "Add ID properties to all file's headlines without an ID."
    (interactive)
    (org-map-entries 'org-id-get-create))

  (org-babel-do-load-languages 'org-babel-load-languages
    (append org-babel-load-languages
      '((emacs-lisp . t)
         (shell . t)
         (plantuml . t)
         (ruby . t))))
  (add-to-list 'org-structure-template-alist '("m" . "marginnote"))
  (add-to-list 'org-structure-template-alist '("D" . "details"))
  (add-to-list 'org-structure-template-alist '("S" . "summary"))
  (add-to-list 'org-structure-template-alist '("U" . "update"))
  (add-to-list 'org-structure-template-alist '("i" . "inlinecomment"))
  ;; I grabbed from the following LaTeX class from
  ;; https://www.reddit.com/r/emacs/comments/3zcr43/nooborgmode_custom_latexpdf_export_custom_style/.
  ;; I’m trash with LaTeX, but like the layout thusfar.

  ;; \\hypersetup{colorlinks=false,pdfborderstyle={/S/U/W 1},pdfborder=0 0 1}"
  ;;
  ;; Make TAB act as if it were issued from the buffer of the
  ;; languages's major mode.
  :custom (org-src-tab-acts-natively t)
  (org-clock-clocktable-default-properties
    '(:maxlevel 5 :link t :tags t))
  :bind (:map org-mode-map
          ("C-c l u" . jf/org-mode/convert-link-type)
          ("C-c l i" . org-insert-link)
          ("C-c l h" . jf/org-link-to-headline)
          ("M-g o" . consult-org-heading))
  :bind (("s-8" . #'jf/org-mode/capture/insert-content-dwim)
          ("C-c l s" . org-store-link)
          ("C-c a" . org-agenda)
          ("C-c c" . org-capture)
          ("C-s-t" . org-toggle-link-display))

  :config
  (defvar jf/org-mode/capture/filename
    "~/git/org/denote/melange/20230210T184422--example-code__programming.org"
    "The file where I'm capturing content.

By default this is my example code project.")

  (defconst jf/agenda-filename/local
    "~/git/org/denote/indices/20200501T120000--agenda.org"
    "A local (to the machine) agenda.

Note, there's an assumption that a file of the given name will
exist on each machine, but its contents will be different based
on the needs/constraints of the locality.")

  (defconst jf/lore24-filename
    "~/git/org/denote/indices/20231225T130631--lore24-in-the-shadows-of-mont-brun__Lore24_campaigns_rpgs.org")

  (defvar jf/link-to-project nil)

  (defun jf/project-as-tag ()
    "Prompt for project and kill link to project."
    (let* ((project
             (completing-read "Project: " (jf/project/list-projects)))
            (keyword
              (denote-sluggify-keyword project))
            (file
              (cdar (jf/project/list-projects :project project)))
            (identifier (denote-retrieve-filename-identifier file)))
      (setq jf/link-to-project
        (format "[[denote:%s][%s]]" identifier project))
      keyword))

  (defun jf/project-as-link ()
    (let ((link
            jf/link-to-project))
      (setq jf/link-to-project nil)
      link))


  ;; Convert the data ":PROPERTY: VALUE" into a latex item or markdown
  ;; definition term and detail.
  (setq org-export-filter-node-property-functions
    '(jf/ox/transform-node-property-to-item))

  (defun jf/ox/transform-node-property-to-item (data back-end channel)
    "Convert DATA to appropriate markup for given BACK-END.

CHANNEL is ignored."
    (let* ((field-value
             (s-split ":" data))
            (term
              (s-titleize (s-replace "_" " " (car field-value))))
            (value
              (s-trim (cadr field-value))))
      (if (s-blank? value)
        ""
        (cond
          ((eq back-end 'latex)
            (format "\\item[{%s:}] %s\n" term value))
          ((eq back-end 'md)
            (format "%s\n: %s\n" term value))
          (t data)))))

  (defun jf/org-latex-property-drawer (_property-drawer contents _info)
    "Transcode a PROPERTY-DRAWER element from Org to LaTeX.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
    (and (org-string-nw-p contents)
      (format
        "\\begin{description}\n%s\\end{description}\n\\vspace{5mm}"
        contents)))

  (advice-add #'org-latex-property-drawer
    :override #'jf/org-latex-property-drawer)

  (defun jf/org-latex-format-basic-headline-function (_todo
                                                       _todo-type
                                                       _priority
                                                       text
                                                       _tags
                                                       _info)
    "Only render the TEXT of the headlin.
See `org-latex-format-headline-function' for details."
    text)

  ;; Without these, I've lost table of contents in PDF exports.
  (defun jf/org-export-change-options (plist backend)
    (cond
      ((equal backend 'html)
        (plist-put plist :with-toc nil)
        (plist-put plist :section-numbers nil))
      ((equal backend 'latex)
        (plist-put plist :with-toc 3)
        (plist-put plist :section-numbers nil)))
    plist)

  ;; From https://emacs.stackexchange.com/questions/22210/auto-update-org-tables-before-each-export
  ;; Recalculate all org tables in the buffer when saving.
  (defvar-local jf/org-mode/enable-buffer-wide-recalculation t
    "When non-nil, recalculate all dynamic regions when saving the file.

This variable is buffer local.")
  ;; Mark `jf/org-mode/enable-buffer-wide-recalculation' as a safe local
  ;; variable as long as its value is t or nil. That way you are not
  ;; prompted to add that to `safe-local-variable-values' in custom.el.
  (put 'jf/org-mode/enable-buffer-wide-recalculation
    'safe-local-variable #'booleanp)

  (defun jf/org-mode/recalculate-buffer-tables (&rest args)
    "Wrapper function for `org-table-recalculate-buffer-tables' and
`org-dblock-update' that runs that function only if
`jf/org-mode/enable-buffer-wide-recalculation' is non-nil.

Also, this function has optional ARGS that is needed for any
function that is added to
`org-export-before-processing-hook'. This would be useful if this
function is ever added to that hook."
    (when jf/org-mode/enable-buffer-wide-recalculation
      (progn
        (org-table-recalculate-buffer-tables)
        (org-dblock-update '(4)))))

  (with-eval-after-load 'org
    (use-package ox
      :straight (ox :type built-in))
    (add-to-list 'org-export-filter-options-functions
      #'jf/org-export-change-options)
   ;;; Org Export and Composition Functionality
    (setq org-export-global-macros (list))

    (add-to-list 'org-export-global-macros
      '("kbd" . "@@html:<kbd>@@$1@@html:</kbd>@@"))
    (add-to-list 'org-export-global-macros
      '("cite" . "@@html:<cite>@@$1@@html:</cite>@@"))
    (add-to-list 'org-export-global-macros
      '("dfn" . "@@html:<dfn>@@$1@@html:</dfn>@@"))
    (add-to-list 'org-export-global-macros
      '("mark" . "@@html:<mark>@@$1@@html:</mark>@@"))
    (add-to-list 'org-export-global-macros
      '("scene-date" . "#+begin_marginnote\nThe scene occurs on @@html:<span class=\"time\">@@$1@@html:</span>@@.\n#+end_marginnote"))
    (add-to-list 'org-export-global-macros
      '("mention" . "@@hugo:{{< glossary key=\"@@$1@@hugo:\" >}}@@"))
    (add-to-list 'org-export-global-macros
      '("abbr" . "@@hugo:{{< glossary key=\"@@$1@@hugo:\" abbr=\"t\" >}}@@"))
    (add-to-list 'org-export-global-macros
      '("abbr-plural" . "@@hugo:{{< glossary key=\"@@$1@@hugo:\" abbr=\"t\" plural=\"t\" >}}@@"))
    (add-to-list 'org-export-global-macros
      '("i" . "@@html:<i class=\"dfn\">@@$1@@html:</i>@@"))
    (add-to-list 'org-export-global-macros
      '("mechanic" . "@@html:<i class=\"mechanic\">@@$1@@html:</i>@@"))
    (add-to-list 'org-export-global-macros
      '("m" . "@@html:<i class=\"mechanic\">@@$1@@html:</i>@@"))
    (add-to-list 'org-export-global-macros
      '("newline" . "@@latex:\\@@ @@html:<br />@@"))
    (add-to-list 'org-export-global-macros
      '("newpage" . "@@latex:\newpage@@"))
    (add-to-list 'org-export-global-macros
      '("rune" . "@@hugo:<span class=\"rune\">@@$1@@hugo:</span>@@"))
    (add-to-list 'org-export-global-macros
      '("linkToSeries" . "@@hugo:{{< linkToSeries \"@@$1@@hugo:\" >}}@@"))
    (add-to-list 'org-latex-classes
      '("jf/article"
         "\\documentclass[11pt,a4paper]{article}"
         ("\\section{%s}" . "\\section{%s}")
         ("\\subsection{%s}" . "\\subsection{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection{%s}")
         ("\\paragraph{%s}" . "\\paragraph{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph{%s}")))
    (setq org-latex-default-class "jf/article")

    (use-package ox-gfm
      :straight t
      :init
      (require 'ox-gfm))

    (use-package igist
      :straight t
      :config
      (setq igist-current-user-name "jeremyf")
      (setq igist-auth-marker 'igist))

    ;; In
    ;; https://takeonrules.com/2022/02/26/note-taking-with-org-roam-and-transclusion/,

    ;; I wrote about ~org-transclusion~.  The quick version,
    ;; ~org-transclusion~ allows you to include text from one file into
    ;; another.  This allows for document composition.
    (use-package org-transclusion
      :straight t
      :init (setq org-transclusion-exclude-elements
              '(property-drawer keyword)))

    ;; I love the work of Daniel Mendler (https://github.com/minad).
    ;; This package gives a bit of visual chrome to org files.
    (use-package org-modern
      :straight (:host github :repo "minad/org-modern")
      :custom
      (org-modern-replace-stars
        '("➊" "➋" "➌" "➍" "➎" "➏" "➐" "➑" "➒" "•"))
      (org-modern-star 'replace)
        ;; Showing the depth of stars helps with the speed keys
        ;; as well as gives a clearer indicator of the depth of
        ;; the outline.
        (org-modern-hide-stars nil)
      :config (global-org-modern-mode))

    ;; For automatically showing the invisible parts of org-mode.
    (use-package org-appear
      :straight (:type git :host github :repo "awth13/org-appear")
      :hook (org-mode . org-appear-mode)))

  (defun jf/org-confirm-babel-evaluate (lang body)
    "Regardless of LANG and BODY approve it."
    nil)

  ;; Org Mode time tracking and task tracking adjustments

  (defun jf/org-mode/agenda-project-prompt ()
    "Prompt for project based on existing projects in agenda file.

    Note: I tried this as interactive, but the capture templates
    insist that it should not be interactive."
    (completing-read
      "Project: "
      (sort
        (seq-uniq
          (org-map-entries
            (lambda ()
              (org-element-property :raw-value (org-element-at-point)))
            "+LEVEL=4+projects" 'agenda))
        #'string<)))

  ;; When I jump to a new task for the day, I want to position that task
  ;; within the prompted project.  Inspiration from
  ;; https://gist.github.com/webbj74/0ab881ed0ce61153a82e.
  (cl-defun jf/org-mode/agenda-find-project-node (
                                                   &key
                                                   (tag "projects")
                                                   (project (jf/org-mode/agenda-project-prompt))
                                                   ;; The `file+olp+datetree` directive creates a headline like
                                                   ;; “2022-09-03 Saturday”.
                                                   (within_headline (format-time-string "%Y-%m-%d %A")))
    "Position `point' at the end of the given PROJECT WITHIN_HEADLINE.

And use the given TAG."
    ;; We need to be using the right agenda file.
    (with-current-buffer (find-file-noselect
                           jf/agenda-filename/local)
      (let ((existing-position
              (org-element-map
                (org-element-parse-buffer)
                'headline
                ;; Finds the end position of:
                ;; - a level 4 headline
                ;; - that is tagged as a :projects:
                ;; - is titled as the given project
                ;; - and is within the given headline
                (lambda (hl)
                  (and (=(org-element-property :level hl) 4)
                    ;; I can't use the :title attribute as it is a more
                    ;; complicated structure; this gets me the raw
                    ;; string.
                    (string= project
                      (plist-get (cadr hl) :raw-value))
                    (member tag
                      (org-element-property :tags hl))
                    ;; The element must have an ancestor with a headline
                    ;; of today
                    (string= within_headline
                      (plist-get
                        ;; I want the raw title, no styling nor tags
                        (cadr
                          (car
                            (org-element-lineage hl)))
                        :raw-value))
                    (org-element-property :end hl)))
                nil t)))
        (if existing-position
          ;; Go to the existing position for this project
          (goto-char existing-position)
          (progn
            ;; Go to the end of the file and append the project to the
            ;; end
            (goto-char (point-max))
            ;; Ensure we have a headline for the given day
            (unless (org-element-map
                      (org-element-parse-buffer)
                      'headline
                      (lambda (hl)
                        (string= within_headline
                          (plist-get
                            ;; I want the raw title, no styling nor tags
                            (cadr (car (org-element-lineage hl)))
                            :raw-value))))
              (insert (concat "\n\n*** "within_headline)))
            (insert (concat "\n\n**** " project " :" tag ":\n\n")))))))

  (cl-defun jf/org-mode/agenda-find-blocked-node ()
    "Add a blocker node to today."
    (jf/org-mode/agenda-find-project-node :tag "blockers"
      :project (concat
                 "Blockers for "
                 (format-time-string
                   "%Y-%m-%d"))))

  (cl-defun jf/org-mode/agenda-find-merge-request-node ()
    "Add a mergerequest node to today."
    (jf/org-mode/agenda-find-project-node :tag "mergerequests"
      :project (concat "Merge Requests for "
                 (format-time-string
                   "%Y-%m-%d"))))

  ;; Takes my notes for the day and formats them for a summary report.
  (defun jf/org-mode/agenda-to-stand-up-summary (parg)
    "Copy to the kill ring the day's time-tracked summary.

When given PARG, prompt for the day of interest.

NOTE: This follows the convention that projects are on headline 4 and
tasks within projects are headline 5."
    (interactive "P")
    (with-current-buffer (find-file-noselect
                           jf/agenda-filename/local)
      (save-excursion
        (let ((within_headline
                ;; Use the CCYY-MM-DD Dayname format and prompt for a
                ;; date if PREFIX-ARG given.
                (format-time-string "%Y-%m-%d %A"
                  (when (car parg)
                    (org-read-date nil t nil "Pick a day:" )))))
          (kill-new
            (concat "*Summary of " within_headline "*\n\n"
              (s-trim
                (s-join
                  "\n"
                  (org-element-map
                    (org-element-parse-buffer)
                    'headline
                    (lambda (hl)
                      (when (member
                              within_headline
                              (mapcar
                                (lambda (ancestor)
                                  (plist-get (cadr ancestor)
                                    :raw-value))
                                (org-element-lineage hl)))
                        (pcase (org-element-property :level hl)
                          (4
                            (concat "\n"
                              (plist-get (cadr hl) :raw-value)))
                          (5
                            (if (and
                                  (member "mergerequest"
                                    (org-element-property :tags hl))
                                  (eq 'done
                                    (org-element-property
                                      :todo-type hl)))
                              nil
                              (concat "- "
                                (plist-get (cadr hl) :raw-value))))
                          (_ nil)))))))))
          (jf/create-scratch-buffer)
          (yank)))))

  (defun jf/org-mode/narrow-to-date (date)
    "Narrow agenda to given DATE agenda subtree."
    (interactive (list (if current-prefix-arg
                         (org-read-date nil nil nil "Pick a day:")
                         (format-time-string "%Y-%m-%d"))))
    (widen)
    (goto-char (point-max))
    (re-search-backward (concat "^\*\*\* " date))
    (end-of-line)
    (org-narrow-to-subtree)
    (message "Narrowing to %s agenda" date))

  ;; I’m responsible for tracking my work time.  I want a way to quickly
  ;; see what that is for the current week.
  ;;
  ;; A utility function providing an overrview
  (cl-defun jf/org-mode/weekly-report ()
    "Jump to my weekly time tracker.

Useful for providing me with an overview of my total tracked time
for the week."
    (interactive)
    (find-file jf/agenda-filename/local)
    (require 'pulsar)
    (pulsar-pulse-line)
    (org-clock-report 4))

  ;; Another task at end of month is to transcribing my agenda’s
  ;; timesheet to entries in our time tracking software.  From the day’s
  ;; project link in the =org-clock-report=, I want to copy the
  ;; headlines of each of the tasks.  I fill out my time sheets one day
  ;; at a time.
  (defun jf/org-mode/time-entry-for-project-and-day ()
    "Function to help report time for Scientist.com.

Assumes that I'm on a :projects: headline.

- Sum the hours (in decimal form) for the tasks.
- Create a list of the tasks.
- Write this information to the message buffer.
- Then move to the next heading level."
    (interactive)
    (let* ((project
             (plist-get (cadr (org-element-at-point)) :raw-value))
            (tasks
              (s-join "\n"
                (org-with-wide-buffer
                  (when (org-goto-first-child)
                    (cl-loop collect (concat "- "
                                       (org-no-properties
                                         (org-get-heading t t t t)))
                      while (outline-get-next-sibling))))))
            (hours (/ (org-clock-sum-current-item) 60.0))
            (output (format "Tasks:\n%s\nProject: %s\nHours: %s\n"
                      tasks
                      project
                      hours)))
      (kill-new tasks)
      (message output)))

  ;; Org Mode has built-in capabilities for exporting to HTML (and other
  ;; languages).  The following function does just a bit more.  It
  ;; converts the org region to HTML and sends it to the clipboard as an
  ;; RTF datatype.
  ;;
  ;; Why is that nice?  As an RTF datatype, the paste receiver better
  ;; handles the HTML (e.g., I can more readily paste into an Email and
  ;; it pastes as expected).
  ;;
  ;; See
  ;; https://kitchingroup.cheme.cmu.edu/blog/2016/06/16/Copy-formatted-org-mode-text-from-Emacs-to-other-applications/
  ;; for more details.  One addition I made was to add the
  ;; ~-inputencoding UTF-8~ switch.  Without it, I would end up with
  ;; some weird characters from odd smartquote handling.
  (defun jf/org-mode/delete-link ()
    "Remove the link part of `org-mode' keeping only description."
    (interactive)
    (let ((elem
            (org-element-context)))
      (when (eq (car elem) 'link)
        (let* ((content-begin
                 (org-element-property :contents-begin elem))
                (content-end
                  (org-element-property :contents-end elem))
                (link-begin
                  (org-element-property :begin elem))
                (link-end
                  (org-element-property :end elem)))
          (when (and content-begin content-end)
            (let ((content
                    (buffer-substring-no-properties
                      content-begin content-end)))
              (delete-region link-begin link-end)
              (insert (concat content " "))))))))
  ;; ;; I have left the following for posterity.  It's something that
  ;; ;; I've stubmled upon in the past and might stumble again if I
  ;; ;; return to `org-roam'.
  ;;
  ;; (defun jf/force-org-rebuild-cache (prefix-arg)
  ;;   "Call functions to rebuild the applicable `org-mode' and `org-roam' cache(s).

  ;; When given PREFIX_ARG, clear the org-roam database (via
  ;;  `org-roam-db-clear-all') then sync.  This will slow down the sync."
  ;;   (interactive "P")
  ;;   (org-id-update-id-locations)
  ;;   (when (fboundp 'org-roam-db-clear-all)
  ;;     (progn
  ;;       (when (car prefix-arg) (org-roam-db-clear-all))
  ;;       (org-roam-db-sync)
  ;;       (org-roam-update-org-id-locations))))

  (cl-defun jf/org-agenda/send-forward-task ()
    "Send an `org-mode' task node forward."
    (interactive)
    (save-excursion
      (let* ((day-project-task
               (jf/org-agenda/timesheet/get-day-and-project-and-task-at-point))
              (from-project
                (plist-get day-project-task :project))
              (from-task
                (plist-get day-project-task :task)))
        ;; Narrowing the region to perform quicker queries on the
        ;; element
        (narrow-to-region (org-element-property :begin from-task)
          (org-element-property :end from-task))

        ;; Grab each section for the from-task and convert that into
        ;; text.
        ;;
        ;; Yes we have the from-task, however, we haven't parsed that
        ;; entity.  Without parsing that element, the
        ;; `org-element-contents' returns nil.
        (let ((content
                (s-join "\n"
                  (org-element-map (org-element-parse-buffer)
                    'section
                    (lambda (section)
                      (mapconcat
                        (lambda (element)
                          (pcase (org-element-type element)
                            ;; I want to skip my time entries
                            ('drawer nil)
                            (_ (buffer-substring-no-properties
                                 (org-element-property
                                   :begin element)
                                 (org-element-property
                                   :end element)))))
                        (org-element-contents section)
                        "\n"))))))
          (widen)
          (org-capture-string
            (format "%s %s :%s:\n\n%s %s %s :%s:\n%s"
              (s-repeat (org-element-property :level from-project) "*")
              (org-element-property :raw-value from-project)
              (s-join ":" (org-element-property :tags from-project))
              (s-repeat (org-element-property :level from-task) "*")
              (org-element-property :todo-keyword from-task)
              (org-element-property :raw-value from-task)
              (s-join ":" (org-element-property :tags from-task))
              content)
            "d"))
        ;; Now that we've added the content, let's tidy up the
        ;; from-task.
        (goto-char (org-element-property :contents-begin from-task))
        ;; Prompt for the todo state of the original task.
        (call-interactively 'org-todo))))

  (defun jf/org-agenda/timesheet/get-day-and-project-and-task-at-point ()
    "Return a plist of :day, :project, and :task for element at point."
    (let* ((task
             (jf/org-agenda-headline-for-level :level 5))
            (project (progn
                       (org-up-heading-safe)
                       (org-element-at-point)))
            (day (progn
                   (org-up-heading-safe)
                   (org-element-at-point))))
      (list :project project :task task :day day)))

  (cl-defun jf/org-agenda-headline-for-level (&key (level 5))
    "Find the `org-mode' ancestor headline with LEVEL."
    (let ((element
            (org-element-at-point)))
      (if (eq 'headline (org-element-type element))
        (let ((element-level
                (org-element-property :level element)))
          (cond
            ((= level element-level)
              (progn (message "Found %s" element) element))
            ((> level element-level)
              (user-error
                "Selected element %s is higher level"
                element-level))
            ((< level element-level)
              (progn
                (org-up-heading-safe)
                (jf/org-agenda-headline-for-level :level level)))))
        (progn
          (org-back-to-heading)
          (jf/org-agenda-headline-for-level :level level)))))

  ;; https://www.reddit.com/r/emacs/comments/yjobc2/what_method_do_you_use_to_create_internal_links/
  (defun jf/org-get-headlines (&optional max-depth)
    "Get `org-mode' headline text within current buffer."
    (org-element-map (org-element-parse-buffer 'headline nil t)
      'headline (lambda (headline)
                  (and
                    (if (integerp max-depth)
                      (>= max-depth
                        (org-element-property :level headline))
                      t)
                    (org-element-property :title headline)))))

  (defun jf/org-link-to-headline ()
    "Insert an internal link to a headline."
    (interactive)
    (let* ((headlines
             (jf/org-get-headlines))
            (choice
              (completing-read "Headings: " headlines nil t))
            (desc
              (read-string "Description: " choice)))
      (org-insert-link buffer-file-name (concat "*" choice) desc)))

  ;; If the example doesn't exist, create the example in the file

  (cl-defun jf/org-mode/capture/prompt-for-example (&optional
                                                     given-mode
                                                     &key
                                                     (tag "example"))
    "Prompt for the GIVEN-MODE example with given TAG."
    (let* ((mode
             (or given-mode (completing-read "Example:"
                              '("Existing" "New" "Stored")))))
      (cond
        ((string= mode "New")
          (let ((example
                  (read-string "New Example Name: "
                    nil
                    nil
                    (format-time-string "%Y-%m-%d %H:%M:%S"))))
            (with-current-buffer (find-file-noselect
                                   jf/org-mode/capture/filename)
              (jf/org-mode/capture/set-position-file :headline nil
                :tag "examples"
                :depth 1)
              (insert (s-format jf/org-mode/capture/example-template
                        'aget
                        (list (cons "example" example)
                          (cons "tag" tag))))
              example)))
        ((string= mode "Existing")
          (with-current-buffer (find-file-noselect
                                 jf/org-mode/capture/filename)
            (let ((examples
                    (org-map-entries
                      (lambda ()
                        (org-element-property
                          :title (org-element-at-point)))
                      (concat "+LEVEL=2+" tag) 'file)))
              (if examples
                (completing-read "Example: " examples nil t)
                (jf/org-mode/capture/prompt-for-example
                  "New" :tag tag)))))
        ((string= mode "Stored")
          (or jf/org-mode/capture/stored-context
            (jf/org-mode/capture/prompt-for-example "Existing"
              :tag tag))))))

  (defvar jf/org-mode/capture/example-template
    (concat "\n\n** TODO ${example} :${tag}:\n\n*** TODO Context\n\n"
      "*** Code :code:\n\n*** TODO Discussion\n\n"
      "*** COMMENT Refactoring\n\n"))

  (defvar jf/org-mode/capture/stored-context
    nil
    "A cached value to help quickly capture items.")

  (cl-defun jf/org-mode/capture/set-position-file (&key
                                                    (headline (jf/org-mode/capture/prompt-for-example))
                                                    (tag "code")
                                                    (depth 3))
    "Position `point' at the end of HEADLINE.

The HEADLINE must have the given TAG and be at the given DEPTH
and be a descendent of the given PARENT_HEADLINE.  If the
HEADLINE does not exist, write it at the end of the file."
    ;; We need to be using the right agenda file.
    (with-current-buffer (find-file-noselect
                           jf/org-mode/capture/filename)
      (setq jf/org-mode/capture/stored-context headline)
      (let* ((existing-position
               (org-element-map
                 (org-element-parse-buffer)
                 'headline
                 (lambda (hl)
                   (and (=(org-element-property :level hl) depth)
                     (member tag
                       (org-element-property :tags hl))
                     (if headline
                       (string= headline
                         (plist-get
                           (cadr
                             (car
                               (org-element-lineage hl)))
                           :raw-value))
                       t)
                     (org-element-property :end hl)))
                 nil t)))
        (goto-char existing-position))))

  ;; With Heavy inspiration from http://www.howardism.org/Technical/Emacs/capturing-content.html
  (defvar jf/org-mode/capture/template/default
    (concat "\n**** ${function-name}"
      "\n:PROPERTIES:"
      "\n:CAPTURED_AT: ${captured-at}"
      "\n:REMOTE_URL: [[${remote-url}][${function-name}]]"
      "\n:LOCAL_FILE: [[file:${file-name}::${line-number}]]"
      "\n:FUNCTION_NAME: ${function-name}"
      "\n:END:\n"
      "\n#+BEGIN_${block-type} ${block-mode}"
      "\n${block-text}"
      "\n#+END_${block-type}"))

  (defvar jf/org-mode/capture/template/while-clocking
    (concat "Reviewing [[${remote-url}][${function-name}]] "
      "\n\n#+BEGIN_${block-type} ${block-mode}"
      "\n${block-text}"
      "\n#+END_${block-type}"))

  (cl-defun jf/org-mode/capture/get-field-values (block-text)
    "Get the text between START and END returning a fields and values.

The return value is a list of `cons' with the `car' values of:

- function-name
- captured-at
- remote-url
- file-name
- line-number
- block-type
- block-mode
- block-text"
    (require 'magit)
    (require 'git-link)
    (let* ((file-name (buffer-file-name (current-buffer)))
            (org-src-mode (replace-regexp-in-string
                            "-\\(ts-\\)?mode"
                            ""
                            (format "%s" major-mode)))
            (func-name (which-function))
            (type (cond
                    ((eq major-mode 'nov-mode) "QUOTE")
                    ((derived-mode-p 'prog-mode) "SRC")
                    (t "SRC" "EXAMPLE")))
            (file-base (if file-name
                         (file-name-nondirectory file-name)
                         (format "%s" (current-buffer))))
            (line-number (line-number-at-pos (region-beginning)))
            (remote-link (when (magit-list-remotes)
                           (progn
                             (call-interactively 'git-link)
                             (car kill-ring)))))
      `(("function-name" . ,(or func-name "Unknown"))
         ("captured-at" . ,(format-time-string "%Y-%m-%d %H:%M"))
         ("remote-url" . ,remote-link)
         ("file-name" . ,file-name)
         ("line-number" . ,line-number)
         ("block-type" . ,type)
         ("block-mode" . ,org-src-mode)
         ("block-text" . , block-text))))

  (cl-defun jf/denote/capture-wrap (&key link content)
    "Given INK and CONTENT return a string to insert into the capture."
    ;; We must do funny business with the link to discern the type.
    (let* ((elements
             (s-split "::"
               (string-replace "]]" "" (string-replace "[[" "" link))))
            (parts (s-split ":" (car elements)))
            (type (car parts))
            (path (s-join ":" (cdr parts))))
      (cond
        ;; The 'eww-mode never fires :(
        ((eq 'eww-mode major-mode)
          (save-excursion
            (let* ((url
                     (plist-get eww-data :url))
                    (title
                      (plist-get eww-data :title)))
              (concat "#+attr_shortcode:"
                (when title (concat " :cite " title))
                (when url (concat " :cite_url " url))
                "\n#+begin_blockquote\n"
                content
                "\n#+end_blockquote\n%?"))))
        ((string= "elfeed" type)
          (save-excursion
            (funcall (org-link-get-parameter type :follow) path)
            (let ((url
                    (elfeed-entry-link elfeed-show-entry))
                   (title
                     (elfeed-entry-title elfeed-show-entry))
                   (author
                     (plist-get
                       (car (plist-get
                              (elfeed-entry-meta elfeed-show-entry)
                              :authors))
                       :name)))
              (concat (when (or author title url) "#+attr_shortcode:")
                (when author (concat " :pre " author))
                (when title (concat " :cite " title))
                (when url (concat " :cite_url " url))
                "\n#+begin_blockquote\n"
                content
                "\n#+end_blockquote\n%?"))))
        ((string= "file" type)
          (save-excursion
            ;; When capturing an HTML file, when the second parameter
            ;; was nil, the `org-link-open-as-file' would launch an
            ;; external web browser.  Not desired behvaior for a content
            ;; capture.
            (org-link-open-as-file path t)
            (s-format jf/org-mode/capture/template/while-clocking
              'aget
              (jf/org-mode/capture/get-field-values content))))
        ((or (string= "http" type) (string= "https" type))
          (save-excursion
            (concat "#+attr_shortcode: :cite_url " link
              "\n#+begin_blockquote\n"
              content
              "\n#+end_blockquote\n%?")))
        (t (concat "\n#+begin_example\n"
             content
             "\n#+end_example")))))

  (defun jf/org-mode/capture/parameters (prefix)
    "A logic lookup table by PREFIX."
    (cond
      ;; When we're clocking and no prefix is given...
      ((and
         (= 1 prefix)
         (fboundp 'org-clocking-p) (org-clocking-p))
        (list :key "i"
          :template jf/org-mode/capture/template/while-clocking))
      ;; We're not clocking or we provided a prefix.
      (t (list :key "c"
           :template jf/org-mode/capture/template/default))))

  (cl-defun jf/org-mode/capture/insert-content-dwim (start end prefix)
    "Capture the text between START and END.

Without PREFIX and not clocking capture clock otherwise capture
to Backlog."
    (interactive "r\np")
    ;; There is a data structure looking to exist.  That structure is:
    ;;
    ;; - org-capture-key (default "c")
    ;; - template jf/org-mode/capture/template/default
    (let ((params
            (jf/org-mode/capture/parameters prefix))
           (block-text
             (buffer-substring-no-properties start end)))
      (org-capture-string
        (s-format (plist-get params :template)
          'aget
          (jf/org-mode/capture/get-field-values block-text))
        (plist-get params :key))))

  (load "jf-campaign.el")
  (setq org-capture-templates
    '(("a" "To Agenda"
        entry (file jf/agenda-filename/local)
        "* TODO %?"
        :clock-keep t
        :empty-lines-before 1
        :jump-to-captured t)
       ("d" "To Denote"
         plain (file denote-last-path)
         #'jf/denote-org-capture
         :no-save t
         :immediate-finish nil
         :kill-buffer t
         :jump-to-captured t)
       ("c" "Content to Clock"
         plain (clock)
         "%(jf/denote/capture-wrap :link \"%L\" :content \"%i\")"
         :empty-lines 1)
       ("i" "Immediate to Clock"
         plain (clock)
         "%i%?"
         :immediate-finish t)
       ("k" "Kill to Clock"
         plain (clock)
         "%c" :immediate-finish t)
       ("l" "#Lore24 Entry"
         plain (file+olp+datetree jf/lore24-filename)
         "%?"
         :clock-in t
         :clock-keep t
         :empty-lines-before 1
         :jump-to-captured t)
       ("n" "NPC"
         entry (file+headline jf/campaign/file-name
                 "Non-Player Characters")
         "* %(jf/campaign/random-npc-as-entry)\n%?"
         :empty-lines-after 1
         :jump-to-captured t)
       ("t" "Today I Learned"
         plain (file+olp+datetree
                 "~/git/org/denote/glossary/20221009T115751--today-i-learned__TodayILearned.org")
         "[[date:%(format-time-string \"%Y-%m-%d\")][Today]] I learned %?"
         :empty-lines-before 1
         :empty-lines-after 1
         :clock-keep t)
       ("N" "Note for project task"
         plain (function jf/org-mode/capture/project-task/find)
         "%T\n\n%?"
         :empty-lines-before 1
         :empty-lines-after 1))))

(use-package org-web-tools
  ;; A package that I can pull down a web page and store its content as
  ;; an `org-mode' file.
  :straight t
  :config
  (setq org-web-tools-pandoc-sleep-time 1.5))

(use-package abbrev
  ;; The =abbrev= package is simple and powerful, providing an
  ;; auto-correct that I configure.  No more “teh” in my text.
  :straight (:type built-in)
  :custom (abbrev-file-name (file-truename
                              "~/git/dotemacs/emacs.d/abbrev_defs"))
  :hook (text-mode . abbrev-mode))

(use-package emacs
  :bind (("C-M-i" . completion-at-point)
          ("TAB" . indent-for-tab-command)
          ("C-w" . jf/delete-region-or-backward-word)
          ("M-DEL" . jf/delete-region-or-backward-word)
          ("C-M-<backspace>" . backward-kill-paragraph))
  :custom
  (global-display-line-numbers-mode t)
  (column-number-mode t)
  (global-display-fill-column-indicator-mode t)
  (delete-selection-mode t)
  (auto-save-file-name-transforms
    '((".*" "~/.emacs.d/autosaves/\\1" t)))
  :init
  ;; Emacs 28: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not supposed to be
  ;; used via M-x.
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
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  :config
  (defun jf/auto-create-missing-dirs ()
    "Ensure that we create directories along the new path."
    ;; Ensure that we create the directories along the path of a new
    ;; file I’m creating.  See
    ;; https://emacsredux.com/blog/2022/06/12/auto-create-missing-directories/
    (let ((target-dir
            (file-name-directory buffer-file-name)))
      (unless (file-exists-p target-dir)
        (make-directory target-dir t))))

  (add-to-list 'find-file-not-found-functions
    #'jf/auto-create-missing-dirs)

  (defun jf/filename/tilde-based (filename)
    "Return ~/ relative FILENAME."
    (string-replace (getenv "HOME") "~"
      (if (consp filename) (cadr filename) filename)))
  (defun jf/delete-region-or-backward-word (&optional arg)
    "Delete region or delete backwards the ARG number of words."
    (interactive "p")
    (if (region-active-p)
      (delete-region (region-beginning) (region-end))
      (jf/delete-word (- arg))))

  (defun jf/delete-word (arg)
    "Delete characters forward until encountering the end of a word.

    With ARG, do this that many times."
    (interactive "p")
    (if (use-region-p)
      (delete-region (region-beginning) (region-end))
      (delete-region (point) (progn (forward-word arg) (point))))))

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
  ;; Extensions for the numerous `completing-read' functions.  Highly
  ;; extensible and customizable.
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
          ("M-g s-o" . consult-org-agenda)
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
          ;; I've long favored Swiper mapped to c-s
          ("C-s" . consult-line)
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
  ;; Optionally configure the register formatting. This improves the
  ;; register preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
    register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.  This adds thin
  ;; lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  :custom
  (consult-narrow-key "<")
  ;; Updating the default to include "--smart-case"
  ;; Leveraging ripgrep-all https://github.com/phiresky/ripgrep-all
  (consult-ripgrep-command
    (concat "rg --null --hidden --line-buffered --color=ansi "
      "--max-columns=1000 --follow "
      "--smart-case --no-heading --line-number --no-ignore-vcs "
      "--glob !vendor/ --glob !coverage/ --glob !**/tmp/ "
      "--glob !public/ --glob !node_modules/ --glob !.git/ "
      "--glob !doc/ "
      "--glob !.yardoc/ --glob !.byebug_history --glob !**/log/ "
      " . -e ARG OPTS"))
  (consult-ripgrep-args
    (concat "rg --null --hidden --line-buffered --color=never "
      "--max-columns=1000 --follow"
      "--path-separator / --no-ignore-vcs --smart-case --no-heading "
      "--glob !vendor/ --glob !coverage/ --glob !**/tmp/ "
      "--glob !public/ --glob !node_modules/ --glob !.git/ "
      "--glob !doc/ "
      "--glob !.yardoc/ --glob !.byebug_history  --glob !**/log/ "
      "--line-number "))
  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :preface
  (defun jf/consult-imenu (prefix)
    "Call `consult-imenu' or `consult-imenu-multi' when PREFIX given."
    (interactive "P")
    (if (car prefix)
      (consult-imenu-multi)
      (consult-imenu)))
  (defun consult-clock-in (prefix &optional match)
    "Clock into an Org agenda heading picking from MATCH.

With a PREFIX jump to the agenda without starting the clock."
    (interactive "P")
    (let ((the-match
            (or match "TODO=\"STARTED\"|TODO=\"TODO\"")))
      (if prefix
        (consult-org-agenda the-match)
        (save-window-excursion
          (consult-org-agenda the-match)
          (org-clock-in)))))
  (defun jf/consult-buffer-kill ()
    "In `consult-buffer' kill the current candidate"
    (interactive)
    (let ((marker
            (string #x200002)) ;; probably some internal detail :(
           (candidate
             (vertico--candidate)))
      (when (s-ends-with? marker candidate)
        (kill-buffer (s-replace marker "" candidate))
        (vertico-next))))
  ;; Customizations
  :config
  (consult-customize
    consult-line consult-ripgrep consult-find
    :initial (when (use-region-p)
               (buffer-substring-no-properties
                 (region-beginning) (region-end))))
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
  ;; I use ~embark.el~ and ~consult.el~, let’s add a little bit more
  ;;  connective tissue.
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-dir
  ;; This package helps ease traveling across directories by providing
  ;; directory candidates related to current buffers, bookmarks, and
  ;; projects.  Further, like other ~consult.el~ functions, you can use
  ;; narrowing keys.  See https://github.com/karthink/consult-dir.
  :straight t
  :after (consult)
  :bind (("C-x C-d" . consult-dir)
          :map minibuffer-local-completion-map
          ("C-x C-d" . consult-dir)
          ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-projectile
  ;; package provides a function I use everyday: ~M-x
  ;; consult-projectile~.  When I invoke ~consult-projectile~, I have
  ;; the file completion for the current project.  I can also type =b= +
  ;; =SPACE= to narrow my initial search to open buffers in the project.
  ;; Or =p= + =space= to narrow to other projects; and then select a
  ;; file within that project.
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
  ;; I want recent files as well as project files as well as recent
  ;; project files...Hence the override fb
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


  (defun consult-find-file-with-preview (prompt
                                          &optional dir default
                                          mustmatch initial pred)
    (interactive)
    (let ((default-directory
            (or dir default-directory)))
      (consult--read #'read-file-name-internal
        :state (consult--file-preview)
        :prompt prompt
        :initial initial
        :require-match mustmatch
        :predicate pred)))
  :bind
  ;;; This overwrite `ns-open-file-using-panel'; the operating system's
  ;;; "Finder"
  ;; ("C-c o" . consult-projectile)
  ;;; I have long had Cmd+t mapped to opening project files; however,
  ;;; I'm noticing the way I'm typing this and it is feeling wrong.  So
  ;;; now I won't have that way open.
  ("s-t" . consult-projectile)
  ("s-p" . consult-projectile)
  ("H-t" . consult-projectile)
  ("H-p" . consult-projectile))

(use-package corfu
  ;; Completion overlay; a narrower intreface than the more verbose
  ;; company.
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
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent
  ;; when you want to perform completion
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)      ; Always show candidates in menu
  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)
  ;; (corfu-min-width 80)
  ;; (corfu-max-width corfu-min-width) ; Always have the same width
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)
  ;; (corfu-echo-documentation nil)  ; Already use corfu-doc
  (corfu-separator ?\s)              ; Necessary for use with orderless
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)    ; Preview current candidate?
  (corfu-preselect-first t)          ; Preselect first candidate?
  :preface
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (defun corfu-move-to-minibuffer ()
    "Move \"popup\" completion candidates to minibuffer.
Useful if you want a more robust view into the recommend candidates."
    (interactive)
    (let (completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region
        completion-in-region--data)))
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map))
      ;; Disable automatic echo and popup
      (setq-local corfu-echo-delay nil
        corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  :config
  (add-hook 'minibuffer-setup-hook
    #'corfu-enable-always-in-minibuffer 1)
  :init
  ;; (corfu-indexed-mode)
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  (global-corfu-mode)
  (load "~/.emacs.d/straight/build/corfu/corfu-indexed.el" nil t)
  (corfu-indexed-mode)
  (load "~/.emacs.d/straight/build/corfu/corfu-info.el" nil t)
  (load "~/.emacs.d/straight/build/corfu/corfu-popupinfo.el" nil t)
  (corfu-popupinfo-mode))

(use-package cape
  ;; Completion at point functions, with the amazing `cape-capf-super'
  ;; for granular configuration of specific mode completion behavior.
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
  ;; I'm going to talk about this later, but I'm adding this to the
  ;; menu, so I may as well state the dependency.
  (use-package embark :straight t)
  :straight t
  :config
  (transient-define-prefix jf/helpful-menu ()
    "Return a `transient' list to apply to different transients."
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
      ("i" "Info" info)
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
  ;; A composable expansion tool that I find compliments `corfu' in that
  ;; it looks in a different manner for completions.
  ;;
  ;; TODO: Perhaps I should spend a bit time investigating removing
  ;; `hippie-exp' in favor of `corfu' and `cape' behavior.  Definitely
  ;; spend a bit of time exploring this option.
  :straight t
  :config
  (setq hippie-expand-try-functions-list
    '(try-expand-dabbrev-visible
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
  ;; Given that my blog has lots of "writing in the margins" this is the
  ;; package for me.
  ;;
  ;; It provides annotations for completions; in particular I rely on
  ;; showing the docstring of `M-x' results.
  :straight t
  :config (setq marginalia-max-relative-age 0)
  ;; /Note:/ The declaration of `marginalia-mode' must be in the :init
  ;; section.This ensures that it is enabled right away.  It also forces
  ;; the loading of the package.
  :init (marginalia-mode))

(use-package orderless
  ;; The https://github.com/minad/orderless package provides completion
  ;; tooling for non-strict word order.  I spent considerable time
  ;; reading through the https://github.com/minad/consult/wiki
  ;;
  ;; As configured the orderless completion recognizes the following
  ;; “switches”:
  ;;
  ;; - Flex (~\~~) :: Just start typing characters and you’ll get
  ;;   matches that have those characters
  ;; - File Extension (~\.ext~) :: Match files with this extension.
  ;; - Regexp ~^.$~ :: Use some regular expression syntax
  ;;   - ~^~ matching beginning
  ;;   - ~.~ any ol’ character
  ;;   - ~$~ matching ending
  ;; - Initialism (~`~) :: In ~M-x~ when I typed ~`pl~ the
  ;;   ~previous-line~ function was a top match.  The initialism switch
  ;;   “explodes” the characters and says match methods who’s words
  ;;   start with those characters.
  ;; - Not Literal ~!~ :: Exclude candidates that match the literal
  ;;   (e.g. ~!previous~ won’t show ~previous-line~ in the ~M-x~
  ;;   completion).
  ;; - Literal ~=~ :: No “fuzzy buziness”, just match exactly what I
  ;; - typed.
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
      ;; Ensure that $ works with Consult commands, which add
      ;; disambiguation suffixes
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
  ;; Certain dynamic completion tables (completion-table-dynamic) do not
  ;; work properly with orderless. One can add basic as a fallback.
  ;; Basic will only be used when orderless fails, which happens only
  ;; for these special tables.
  (setq completion-styles '(orderless basic)
    completion-category-defaults nil
          ;;; Enable partial-completion for files.  Either give
          ;;; orderless precedence or partial-completion.  Note that
          ;;; completion-category-overrides is not really an override,
          ;;; but rather prepended to the default completion-styles.
    ;; completion-category-overrides '((file (styles orderless
    ;; partial-completion))) ;; orderless is tried first
    completion-category-overrides
    '((file
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
  ;;
  ;; I often use this when reading while writing blog posts.
  :after (grab-mac-link)
  :straight (org-mac-link :type git
              :host gitlab :repo "aimebertrand/org-mac-link")
  :bind (:map org-mode-map (("C-c g" . org-mac-grab-link))))

(use-package tempel
  ;; For awhile, I'd used yasnippets; themselves inspired by my beloved
  ;; TextMate.  However, I've found `tempel' to be both more than
  ;; adequate and has a narrower implementation foot-print, cleaving
  ;; closer to emacs-lisp; thus likely easing it's maintenance burden.
  :straight (tempel :host github :repo "minad/tempel")
  :custom (tempel-path "~/git/dotemacs/templates")
  :config (global-tempel-abbrev-mode)
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
          ("M-*" . tempel-insert))
  :bind (:map tempel-map (([backtab] . tempel-previous)
                           ("TAB" . tempel-next)))
  :preface
  (cl-defun jf/org-macro-value-list (macro-name
                                      &key (dir org-directory))
    "List the unique inner text of all uses of MACRO-NAME in given DIR."
    (let ((path
            (if current-prefix-arg
              dir
              (or (buffer-file-name (current-buffer)) dir))))
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
    ;; Add the Tempel Capf to
    ;; `completion-at-point-functions'. `tempel-expand' only triggers on
    ;; exact matches. Alternatively use `tempel-complete' if you want to
    ;; see all matches, but then Tempel will probably trigger too often
    ;; when you don't expect it.  NOTE: We add `tempel-expand' *before*
    ;; the main programming mode Capf, such that it will be tried first.
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
  ;; Another one of minad's packages which improves my day to day
  ;; experience.  I find the user experience wonderful when pairing
  ;; vertical candidate selection with `marginalia' and then having the
  ;; `vertico-indexed-mode' option for quick numerical selection.
  :straight (:type git :host github :repo "minad/vertico")
  :bind (:map vertico-map
          (("<tab>" . #'vertico-insert)
            ("<escape>" . #'minibuffer-keyboard-quit)
            ("M-p" . #'previous-history-element)
            ("M-n" . #'next-history-element)
            ;; I've been using more groupings, and being able to move
            ;; through those is nice.
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
    (let ((inhibit-read-only
            t))
      (goto-char (point-max))
      (insert " ")
      (add-text-properties (minibuffer-prompt-end) (point-max)
        '(invisible t
           read-only t
           cursor-intangible t
           rear-nonsticky t))))
  :config
  (define-key vertico-map (kbd "C-SPC")
    #'jf/vertico-restrict-to-matches)
  (vertico-mode 1)
  (setq read-file-name-completion-ignore-case t
    read-buffer-completion-ignore-case t
    completion-ignore-case t)
  (setq vertico-cycle t)
  :init
  ;; Type "C-3 return" and select the 3rd candidate in the list.
  (load "~/.emacs.d/straight/build/vertico/vertico-indexed.el" nil t)
  (vertico-indexed-mode)
  (load "~/.emacs.d/straight/build/vertico/vertico-directory.el" nil t)
  (load "~/.emacs.d/straight/build/vertico/vertico-repeat.el" nil t)
  (keymap-global-set "M-r" #'vertico-repeat)
  ;; When I type ~/ in the `find-file' selector, then it will clear the
  ;; existing path and go to ~/ From Prot's video presentation
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package which-key
  ;; This package helps me begin typing a key sequence and seeing what
  ;; options are available to complete the sequence.
  ;;
  ;; For example, I type "C-c", wait a moment and get a menu that shows
  ;; me what key bindings start with "C-c"; and then I can type the
  ;; following key and execute that command.
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

(use-package denote
  ;; Preamble
  ;;
  ;; Prior to `denote' I made extensive use of `org-roam'; I was
  ;; following `denote' development and appreciate Protesilaos's
  ;; pedagological approach to documentation.  I also appreciate the
  ;; design considerations; which I wrote about here:
  ;; https://takeonrules.com/2022/10/09/denote-emacs-configuration/
  ;;
  ;; I installed denote and began exploring.  I am a software developer
  ;; by trade, and found the code accessible and discernable; that with
  ;; it's sole dependency being `emacs' I felt warranted further
  ;; exploration.  Accessible, discernable, and no dependencies are
  ;; attractive attributes of software that I use as my tools of work
  ;; and play.  In my experience, the maintenance and enhancement is
  ;; easier for this kind of software.
  ;;
  ;; With further exploration, I migrated fully from `org-roam' to
  ;; `denote'.
  ;;
  ;; On Domains
  ;;
  ;; This package configures and extends `denote' by adding conceptual
  ;; domains to my note taking.  The domains are larger demarcations
  ;; than simple tags.  This is built on top of the `denote-directory'
  ;; variable and function.
  ;;
  ;; Further by leveraging domains, I have three means of searching:
  ;;
  ;; - "posts/" are all of my blog posts
  ;; - "-word" will find title's with "word" in them
  ;; - "_tag" will find the "tag" amongst the files keywords
  ;;
  ;; This allows me to leverage, if I want, Denote's siloing feature.
  ;;
  ;; On Org Mode integration
  ;;
  ;; I make extensive use of `org-mode'; it is the format I use for
  ;; crafting my blog posts (see https://takeonrules.com).  It is also
  ;; the tool I use for my day to day task tracking and time tracking.
  ;;
  ;; I have structured my workflow so that any of these day to day
  ;; activities can easily produce blog posts.  I want my internal
  ;; writing to have lots of connective references; to help me find
  ;; previous notes and perhaps look for interesting connections.
  ;;
  ;; I also want posts that I publish to provide a similar experience;
  ;; but the links need to only be for publicly available connections.
  ;; In other words, when I export a blog post, any internal links that
  ;; have an external proxy are rendered as links to those external
  ;; proxies.  Any internal links without an external proxy are rendered
  ;; without links.
  ;;
  ;; This is done via `org-link-set-parameters' and denote's
  ;; documentation (see https://protesilaos.com/emacs/denote) provides
  ;; excellent examples a `org-link-set-parameters'.
  :preface
  (require 'cl-lib)
  ;; A narrow focus tool for organizing notes.  I appreciate the design
  ;; constraints and lack of external dependencies.  This package
  ;; provides portability.  It sits as an alternate to the amazing
  ;; `org-roam' package.
  :straight (:host github :type git :repo "jeremyf/denote")
  :commands (denote-directory
              denote-file-prompt
              denote--title-prompt
              denote-get-path-by-id)
  :bind ("H-l" . 'jf/denote/link-or-create)
  ("H-i" . 'jf/denote/link-or-create)
  :hook (dired-mode . denote-dired-mode)
  (org-mode . denote-rename-buffer-mode)
  :init
  (setq denote-known-keywords
    (split-string-and-unquote
      (shell-command-to-string
        (concat
          "rg \"#\\+TAG:\\s([\\w-]+)\" "
          (expand-file-name "denote/glossary" org-directory)
          " --only-matching"
          " --no-filename "
          " --follow "
          " --replace '$1' | "
          "ruby -ne 'puts $_.gsub(/^(\\w)\\w+-/) { |m| "
          "  m[0].upcase + m[1..-1] "
          "}.gsub(/-(\\w)/) { |m| m[1].upcase }'"))
      "\n"))
  :preface
  (defun jf/blog-entry? (&optional buffer)
    "Return non-nil when BUFFER is a blog post."
    (when-let* ((buffer (or buffer (current-buffer)))
                 (file (buffer-file-name buffer)))
      (and (denote-file-is-note-p file)
        (string-match-p "\\/blog-posts\\/" file))))
  :config
  (cl-defun jf/denote? (&key (buffer (current-buffer)))
    "Return non-nil when BUFFER is for `denote'."
    (when-let* ((file (buffer-file-name buffer)))
      (denote-file-is-note-p file)))
  (require 'denote-org-extras)
  ;; (setq denote-journal-extras-title-format 'day-date-month-year)
  (setq denote-infer-keywords t)
  (setq denote-excluded-punctuation-regexp
    "[][{}!@#$%^&*()=+'\"?,.|;:~`‘’“”/—–]*")
  (setq denote-modules '(xref ffap))
  (setq denote-org-capture-specifiers
    "%(jf/denote/capture-wrap :link \"%L\" :content \"%i\")")
  (setq denote-directory (expand-file-name "denote" org-directory))
  ;; These are the minimum viable prompts for notes
  (setq denote-prompts '(title keywords))
  ;; I love ‘org-mode format; reading ahead I'm setting this
  (setq denote-file-type 'org)
  ;; And `org-read-date' is an amazing bit of tech
  (setq denote-date-prompt-denote-date-prompt-use-org-read-date t)
  (setq denote-file-name-slug-functions
    '((title . jf/denote-sluggify-title)
       (signature . jf/denote-sluggify-signature)
       (keyword . jf/denote-sluggify-keyword)))

  (setq denote-file-name-letter-casing '((title . downcase)
                                          (signature . downcase)
                                          (keywords . verbatim)
                                          (t . downcase)))
  (defvar jf/diacritics-to-non-diacritics-map
    '(("ž" . "z") ("Ž" . "Z")
       ("ý" . "y") ("ÿ" . "y") ("Ÿ" . "Y")
       ("š" . "s") ("Š" . "S")
       ("ñ" . "n") ("Ñ" . "N")
       ("ü" . "u") ("û" . "u") ("ú" . "u") ("ù" . "u")
       ("Ü" . "U") ("Û" . "U") ("Ú" . "U") ("Ù" . "U")
       ("ï" . "i") ("î" . "i") ("í" . "i") ("ì" . "i")
       ("Ï" . "I") ("Î" . "I") ("Í" . "I") ("Ì" . "I")
       ("Ð" . "D")
       ("ç" . "c") ("Ç" . "C")
       ("ð" . "e") ("ë" . "e") ("ê" . "e") ("é" . "e") ("è" . "e")
       ("Ë" . "E") ("Ê" . "E") ("É" . "E") ("È" . "E")
       ("ø" . "o") ("ö" . "o") ("õ" . "o") ("ô" . "o") ("ó" . "o")
       ("ò" . "o")
       ("Ø" . "O") ("Ö" . "O") ("Õ" . "O") ("Ô" . "O") ("Ó" . "O")
       ("Ò" . "O")
       ("å" . "a") ("ä" . "a") ("ã" . "a") ("â" . "a") ("á" . "a")
       ("à" . "a")
       ("Å" . "A") ("Ä" . "A") ("Ã" . "A") ("Â" . "A") ("Á" . "A")
       ("À" . "A")
       ("…" . "")  ;; Ellipsis
       ("—" . "-") ;; em dash
       ("–" . "-") ;; en dash
       )
    "Map of diacritic to non-diacritic form.")
  (defun jf/remove-diacritics-from (string)
    "Remove the diacritics from STRING."
    (when string
      (cl-reduce (lambda (text diacritic-map-element)
                   (s-replace (car diacritic-map-element)
                     (cdr diacritic-map-element) text))
        jf/diacritics-to-non-diacritics-map
        :initial-value string)))

  (defun jf/denote-sluggify-title (str)
    (denote-sluggify-title (jf/remove-diacritics-from str)))

  (defun jf/denote-sluggify-keyword (str)
    (jf/remove-diacritics-from str))

  (defun jf/denote-sluggify-signature (str)
    (denote-sluggify-signature
      (jf/remove-diacritics-from
        (s-replace "=" "_" (s-replace "-" "_" str)))))

  (defun jf/denote-link-ol-get-id ()
    "Use `org-id-get' to find/create ID."
    (org-id-get (point) 'create))

  (defun jf/denote-sluggify (args)
    (let ((type (car args))
           (text (cadr args)))
      (list type (cond
                   ((eq 'title type)
                     (jf/denote-sluggify-title text))
                   ((eq 'signature type)
                     (jf/denote-sluggify-signature text))
                   (t
                     text)))))

  (defun jf/denote-filename-is-note-p (filename)
    "Return non-nil if FILENAME is a valid name for a Denote note.
For our purposes, its path must be part of the variable
`denote-directory', it must have a Denote identifier in its name,
and use one of the extensions implied by `denote-file-type'."
    (and (or
           (string-match-p
             "/Documents/denote-"
             (expand-file-name filename))
           (string-prefix-p
             (denote-directory)
             (expand-file-name filename)))
      (denote-file-has-identifier-p filename)
      (denote-file-has-supported-extension-p filename)))
  (advice-add #'denote-sluggify
    :filter-args #'jf/denote-sluggify)
  (advice-add #'denote-link-ol-get-id
    :override #'jf/denote-link-ol-get-id)
  (advice-add #'denote-filename-is-note-p
    :override #'jf/denote-filename-is-note-p)

  (defun jf/dired-rename-files-to-denote-schema ()
    "Rename marked files in `dired-mode'."
    (interactive)
    (when (seq-find (lambda (file)
                      (member
                        (file-name-nondirectory file)
                        '("." "..")))
            (dired-get-marked-files))
      (user-error "Can't rename \".\" or \"..\" files"))
    (dolist (file (dired-get-marked-files))
      (let ((current-prefix-arg nil))
        (apply #'jf/rename-file-to-denote-schema
          (list :file file :signature :prompt)))))

  (defun jf/denote-sort-dired ()
    "Sort current directory using `denote-sort-dired'."
    (interactive)
    (if (eq major-mode 'dired-mode)
      (let ((denote-directory
              default-directory))
        (call-interactively #'denote-sort-dired)
        (user-error "Current buffer not dired-mode"))))

  (cl-defun jf/rename-file-to-denote-schema (&key file id title
                                              keywords dir signature
                                              force dry-run)
    "Rename FILE using `denote' schema.

When `current-prefix-arg' is non-nil prompt for many of the parameters.

When no FILE is provided use `buffer-file-name'.

- DIR: target directory to move the file to; by default put the
       new file in the same directory.

- ID: the identifier for this file, defaulting to one created via
      `denote-create-unique-file-identifier'.

- TITLE: The tile for this file; default's to a
         `s-titleized-words' of the given FILE's base name.

- KEYWORDS: A list of keywords to apply to the file.  When passed
            :none, skip prompting, via `denote-keywords-prompt'
            for a list of keywords.

- SIGNATURE: The optional signature of the file.  When passed
             :none, skip prompting for a signature.

- FORCE: When non-nil, rename the file without prompting to
         confirm the change.

- DRY-RUN: When non-nil, do not perform the name change but
           instead message the file's new name."
    (interactive)
    (let* ((file
             (or file (if current-prefix-arg
                        (call-interactively (lambda (f)
                                              (interactive "f") f))
                        (buffer-file-name))))
            (title
              (or title
                (read-string "Title: "
                  (denote-desluggify-title
                    (or (denote-retrieve-filename-title file)
                      (s-titleized-words (f-base file)))))))
            (id
              (or id
                (denote-retrieve-filename-identifier file)
                (denote-create-unique-file-identifier
                  file (denote--get-all-used-ids))))
            (keywords
              (if (equal keywords :none)
                '()
                (or keywords
                  (let ((kws
                          (denote-extract-keywords-from-path file)))
                    (completing-read-multiple "Keywords: "
                      (delete-dups (append kws (denote-keywords)))
                      nil nil (when kws
                                (concat (s-join "," kws) ",")))))))
            (signature
              (or (when (equal signature :none) "")
                (and signature (not (equal signature :prompt)))
                (when (or current-prefix-arg (equal signature :prompt))
		              (completing-read "Signature: "
                    (jf/tor-series-list) nil nil
                    (denote-retrieve-filename-signature file)))
                ""))
            (dir
              (f-join
                (or (and dir (not (equal dir :prompt)))
                  (if (or current-prefix-arg (equal dir :prompt))
                    (call-interactively (lambda (d)
                                          (interactive "D") d))
                    (f-dirname file)))
                "./"))
            (extension
              (f-ext file t))
            (new-file (denote-format-file-name
                        dir id (cl-sort keywords #'string<)
                        title extension signature)))
      (if dry-run
        (message "Changing %S to %S" file new-file)
        (when (or force (denote-rename-file-prompt file new-file))
          (denote-rename-file-and-buffer file new-file)
          (denote-update-dired-buffers)))
      new-file))


  (cl-defun jf/denote/org-property-from-id (&key identifier property)
    ;; This function helps me retrieve Org-Mode properties from the
    ;; given Denote ID.
    "Given an IDENTIFIER and PROPERTY return it's value or nil.

    Return nil when:

    - is not a `denote' file
    - IDENTIFIER leads to a non `org-mode' file
    - PROPERTY does not exist on the file"
    (when-let ((filename (denote-get-path-by-id identifier)))
      (when (string= (file-name-extension filename) "org")
        (with-current-buffer (find-file-noselect filename)
          (cadar (org-collect-keywords (list property)))))))

  (cl-defun jf/denote/org-keywords-from-id (&key identifier keywords)
    "Given an IDENTIFIER and KEYWORDS list return an a-list of values.

    Return nil when:

    - is not a denote file
    - IDENTIFIER leads to a non `org-mode' file
    - KEYWORD does not exist on the file.

This function is the plural version of
`jf/denote/org-property-from-id'."
    ;; ;; Testing jf/denote/org-property-from-id
    ;; (message "%s" (jf/denote/org-property-from-id
    ;;     :identifier "20220930T215235"
    ;;		 :property "ABBR"))
    ;; ;; Testing jf/denote/org-keywords-from-id
    ;; (message "%s" (jf/denote/org-keywords-from-id
    ;;     :identifier "20220930T215235"
    ;;     :properties '("TITLE" "ABBR")))
    (when-let ((filename (denote-get-path-by-id identifier)))
      (when (string= (file-name-extension filename) "org")
        (with-current-buffer (find-file-noselect filename)
          (org-collect-keywords keywords)))))

  (defun jf/denote/plist-for-export-of-id (identifier)
    "Given an IDENTIFIER export a `plist' with the following properties:

    - :title
    - :key
    - :url

    Return nil when:

    - is not a denote file
    - IDENTIFIER leads to a non `org-mode' file"
    ;; Testing
    ;; (message "%s" (jf/denote/plist-for-export-of-id "20221009T115949"))
    (when-let ((filename (denote-get-path-by-id identifier)))
      (when (string= (file-name-extension filename) "org")
        (with-current-buffer (find-file-noselect filename)
          (let ((kw-plist
                  (jf/org-keywords-as-plist
                    :keywords-regexp
                    (concat "\\(TITLE\\|GLOSSARY_KEY\\|OFFER"
                      "\\|ROAM_REFS\\|SAME_AS\\)"))))
            (list
              :title (lax-plist-get kw-plist "TITLE")
              :key (lax-plist-get kw-plist "GLOSSARY_KEY")
              :url (or
                     (lax-plist-get kw-plist "OFFER")
                     (when-let ((refs
                                  (lax-plist-get kw-plist "ROAM_REFS")))
                       (if (listp refs)
                         (first (s-split " " refs t))
                         refs))
                     (lax-plist-get kw-plist "SAME_AS"))))))))

  (defun jf/denote/link-or-create (target &optional id-only)
    "Use `denote-link' on TARGET file, creating it if necessary.

As `denote-link-or-create' but use `jf/denote/file-prompt'
instead of `denote-file-prompt'.

This function is intended for a global find of all notes.  With
ID-ONLY link without title."
    (interactive (list (jf/denote/file-prompt)
                   current-prefix-arg))
    (if (and target (file-exists-p target))
      (let ((type
              (denote-filetype-heuristics target)))
        (denote-link target type
          (denote--link-get-description target)
          id-only)
        )
      (denote--command-with-title-history
        #'denote-link-after-creating)))

  (defun jf/denote/file-prompt (&optional files-matching-regexp)
    "Prompt for a file based on subdirectories.

See `denote-file-prompt'"
    ;; I’m not looking at active silo-ing and want to be able to search
    ;; specifically from the top-level and all subdirectories.
    (when-let* ((vc-dirs-ignores (mapcar
                                   (lambda (dir)
                                     (concat dir "/"))
                                   vc-directory-exclusion-list))
                 (files (mapcan
                          (lambda (sub-dir)
                            (project--files-in-directory
                              (f-join
                                (denote-directory)
                                sub-dir)
                              vc-dirs-ignores))
                          jf/denote/subdirectories))
                 (file (funcall project-read-file-name-function
                         "Select note" files nil 'file-name-history)))
      (let ((completion-ignore-case
              read-file-name-completion-ignore-case))
        (add-to-history 'denote-file-history file)
        file)))

  (setq consult-notes-sources (list))
  (setq jf/denote/subdirectories (list))

  (defun jf/denote/find-file ()
    "Find file in the current denote directory."
    (interactive)
    (require 'consult-projectile)
    (require 'denote)
    ;; For this query, override the `projectile-git-command' so that I
    ;; can include my "denote/scientist" notes.
    (let ((projectile-git-command
            "git ls-files -zco --exclude-from=.projectile.gitignore"))
      (consult-projectile--file (denote-directory))))

  (cl-defmacro jf/denote/create-functions-for (&key domain
                                                key (create-fn nil))
    "A macro to CREATE-FN for the given DOMAIN.

          The KEY is the ASCII value of the binding key.

          Creates:

          - Wrapping function of `jf/denote/find-file' that
            narrows results to the given DOMAIN.

          - Create linking function for DOMAIN.
          - Add the domain to the `jf/denote/subdirectories'.
          - Adds DOMAIN to `consult-notes-sources'."
    (let* ((finder-fn
             (intern (concat "jf/denote/find-file--" domain)))
            (subdirectory
              (f-join "~/git/org/denote" domain))
            (finder-docstring
              (concat "Find file in \""
                domain
                "\" subdirectory of `denote-directory'."))
            (default-create-fn
              (intern (concat "jf/denote/create--"
                        domain
                        "--default")))
            (default-create-docstring
              (concat "Create denote in \""
                domain
                "\" subdirectory of "
                "`denote-directory'."))
            (link-or-creator-fn
              (intern (concat "jf/denote/link-or-create--" domain)))
            (link-or-creator-docstring
              (concat "Link to denote in \""
                domain
                "\" subdirectory of "
                "`denote-directory'.")))
      (when (f-exists? subdirectory)
        `(progn
           (add-to-list 'jf/denote/subdirectories ,domain)
           (when (boundp 'consult-notes-sources)
             (add-to-list
               'consult-notes-sources
               '(,domain ,key ,subdirectory)))
           (defun ,default-create-fn ()
             ,default-create-docstring
             (interactive)
             (let ((denote-directory
                     (f-join (denote-directory) ,domain)))
               (call-interactively #'denote)))
           (bind-key (format "H-d c %c" ,key)
             (or ,create-fn ',default-create-fn))
           (bind-key (format "H-d f %c" ,key)
             ',finder-fn)
           (defun ,finder-fn ()
             ,finder-docstring
             (interactive)
             (let ((denote-directory
                     (f-join (denote-directory) ,domain)))
               (call-interactively #'jf/denote/find-file)))
           (bind-key (format "H-d l %c" ,key) ',link-or-creator-fn)
           (defun ,link-or-creator-fn ()
             ,link-or-creator-docstring
             (interactive)
             (let ((denote-directory
                     (f-join (denote-directory) ,domain)))
               (call-interactively #'denote-link-or-create)))
           ))))

  ;; The blog-post domain is for things that I have, will, or might
  ;; publish to https://takeonrules.com
  (jf/denote/create-functions-for :domain "blog-posts"
    :key ?b)

  (defun jf/denote/find-file--blog-posts-draft (filename)
    "Find a draft FILENAME in the \"blog-posts\" denote sub-directory."
    (interactive
      (list (jf/find-file-via-matching
              :prompt "Draft filename: "
              :matching "^#\\+ROAM_REFS:"
              :switch "--files-without-match"
              :in (f-join (denote-directory) "blog-posts"))))
    (find-file filename))
  (bind-key "H-d f B" #'jf/denote/find-file--blog-posts-draft)

  (defun jf/denote/create-scratch (title)
    "Create a scratch note with TITLE."
    (interactive (list (read-string
                         "Scratch title: "
                         (format-time-string "%Y-%m-%d Scratch"))))
    (denote title
      nil
      'org
      (f-join (denote-directory) "scratch")))

  ;; The scratch domain is a place to capture random notes.  These can
  ;; be promoted to another directory or eventually discarded.
  (jf/denote/create-functions-for :domain "scratch"
    :create-fn #'jf/denote/create-scratch
    :key ?s)

  (jf/denote/create-functions-for :domain "work"
    :key ?w)

  (jf/denote/create-functions-for :domain "private"
    :key ?v)

  (cl-defun jf/denote/create-epigraph (&key
                                        (body
                                          (read-from-minibuffer
                                            "Epigraph Text: "))
                                        ;; Todo prompt for Author Name
                                        (author_name
                                          (read-from-minibuffer
                                            "Author Name: "))
                                        ;; Todo prompt for Work Title
                                        (work_title
                                          (read-from-minibuffer
                                            "Work Title: "))
                                        (nth-words 8))
    "Create an epigraph from BODY, AUTHOR_NAME, and WORK_TITLE.

Default the note’s title to the first NTH-WORDS of the BODY."
    (interactive)
    (let* ((body-as-list
             (s-split-words body))
            (title (s-join " " (if (> (length body-as-list) nth-words)
                                 (cl-subseq body-as-list 0 nth-words)
                                 body-as-list)))
            (template (concat
                        ;; The name of the author
                        "#+AUTHOR_NAME: " author_name "\n"
                        ;; Where can you “find” this author?
                        "#+AUTHOR_URL:\n"
                        ;; The GLOSSARY_KEY for the given author
                        "#+AUTHOR_KEY:\n"
                        ;; What’s the title of the work?
                        "#+WORK_TITLE: " work_title "\n"
                        ;; Where can you “get” this work?
                        "#+WORK_URL:\n"
                        ;; The GLOSSARY_KEY for the given work
                        "#+WORK_KEY:\n"
                        ;; Indicates if this is a poem (or not)
                        "#+POEM:\n"
                        ;; The page in which this passage appears in the
                        ;; given work.
                        "#+PAGE:\n"
                        ;; The name of the translator
                        "#+TRANSLATOR_NAME:\n")))
      (denote title
        nil
        'org
        (f-join (denote-directory) "epigraphs")
        nil
        template)))

  (jf/denote/create-functions-for :domain "epigraphs"
    :key ?e
    :create-fn 'jf/denote/create-epigraph)

  (cl-defun jf/denote/create-glossary-entry (&key
                                              (title (read-from-minibuffer "Name the Entry: "))
                                              (is-a-game (yes-or-no-p "Is this a game?"))
                                              (abbr (read-from-minibuffer "Abbreviation (empty to skip): ")))
    "Create a `denote' entry for the given TITLE and ABBR.

    And if this IS-A-GAME then amend accordingly.

    NOTE: At present there is no consideration for uniqueness."
    (interactive)
    (let* ((key
             (downcase (denote-sluggify-title title)))
            (template (concat "#+GLOSSARY_KEY: " key "\n"
                        (when (s-present? abbr)
                          (concat "#+ABBR: " abbr "\n"))
                        ;; TODO: Include a prompt of existing
                        ;; disclaimers
                        "#+CONTENT_DISCLAIMER:\n"
                        "#+DESCRIPTION:\n"
                        (when is-a-game (concat "#+GAME: " key "\n"))
                        "#+ITEMID:\n"
                        "#+ITEMTYPE:\n"
                        "#+NO_TITLE:\n"
                        (when (s-present? abbr)
                          "#+PLURAL_ABBR:\n#+PLURAL_TITLE:\n")
                        "#+ROAM_REFS:\n"
                        "#+TAG:\n" ;; TODO: Assert uniqueness
                        ))
            (keywords (list)))
      ;; Add both "abbr" and the abbr to the keywords; both help in
      ;; searching results
      (when (s-present? abbr) (add-to-list 'keywords "abbr"))
      (when is-a-game (add-to-list 'keywords "game"))
      (denote title
        keywords
        'org
        (f-join (denote-directory) "glossary")
        nil
        template
        (when (s-present? abbr)
          (progn (denote-sluggify-signature abbr))))))

  (jf/denote/create-functions-for :domain "glossary"
    :key ?g
    :create-fn 'jf/denote/create-glossary-entry)
  ;; Testing jf/denote/org-property-from-id
  ;; (message "%s" (jf/denote/org-property-from-id :id "20220930T215235"
  ;;                :property "ABBR"))

  ;; All the other things; perhaps they could become blog posts, but for
  ;; now they remain part of the mixture and medley.
  (jf/denote/create-functions-for :domain "melange"
    :key ?m)

  ;; I do write notes about people I interact with.  Technically I have
  ;; glossary entries for people.  But those entries are for folks I
  ;; don’t interact with.
  (jf/denote/create-functions-for :domain "people"
    :key ?p)

  ;; On my site I write https://takeonrules.com/series/.  I track this
  ;; data in a YAML file; I’d like to treat this data similar to my
  ;; glossary.
  (cl-defun jf/denote/create-indices-entry (&key
                                             (title
                                               (read-from-minibuffer
                                                 "Name the index: "))
                                             (is-a-series
                                               (yes-or-no-p
                                                 "Take on Rules series?")))
    "Create a `denote' index entry for the given TITLE.

Consider different logic if IS-A-SERIES."
    (interactive)
    (let* ((keywords
             (list))
            (template (concat (when (s-present? is-a-series)
                                "#+HIGHLIGHT: true\n"))))
      (when (s-present? is-a-series)
        (add-to-list 'keywords "series"))
      (denote title
        nil
        'org
        (f-join (denote-directory) "indices")
        nil
        template)))

  (jf/denote/create-functions-for :domain "references"
    :key ?r)

  (jf/denote/create-functions-for :domain "indices"
    :key ?i
    :create-fn 'jf/denote/create-indices-entry)

  (cl-defun jf/org-link-complete-link-for (parg &key
                                            scheme filter subdirectory)
    "Prompt for `denote' with filename FILTER in the given SUBDIRECTORY.

    Returns a string of format: \"SCHEME:<id>\" where <id> is
    an `denote' identifier.

PARG is part of the method signature for `org-link-parameters'."
    (let* ((denote-directory
             (if subdirectory
               (f-join (denote-directory)
                 (concat subdirectory "/"))
               (denote-directory)))
            (file (funcall project-read-file-name-function
                    "Select note: "
                    (denote-all-files)
                    ;; Filter might be nil; if so pass nil.  Otherwise
                    ;; the string.
                    (when filter
                      (lambda (fname) (s-contains? filter fname t)))
                    'denote-file-history)))
      ;; This leverages a post v1.0.0 parameter of Denote
      ;; See https://git.sr.ht/~protesilaos/denote/commit/c6c3fc95c66ba093a266c775f411c0c8615c14c7
      (concat scheme ":" (denote-retrieve-filename-identifier file))))

  (cl-defun jf/denote/link-ol-abbr-with-property (link
                                                   description
                                                   format
                                                   protocol
                                                   &key
                                                   keyword
                                                   additional-hugo-parameters)
    "Export a LINK with DESCRIPTION for the given PROTOCOL and FORMAT.

    FORMAT is an Org export backend.  We will discard the given
    DESCRIPTION.  PROTOCOL is ignored."
    (let* ((keyword-alist
             (jf/denote/org-keywords-from-id
               :identifier link
               :keywords (list "TITLE"
                           keyword
                           "GLOSSARY_KEY")))
            (title
              (car (alist-get "TITLE" keyword-alist nil nil #'string=)))
            (keyword-value
              (car (alist-get keyword keyword-alist nil nil #'string=)))
            (key
              (car (alist-get
                     "GLOSSARY_KEY" keyword-alist nil nil #'string=))))
      (cond
        ((or (eq format 'html) (eq format 'md))
          (if jf/exporting-org-to-tor
            (format "{{< glossary key=\"%s\" %s >}}"
              key
              additional-hugo-parameters)
            (format "<abbr title=\"%s\">%s</abbr>"
              title
              keyword-value)))
        ((or (eq format 'latex) (eq format 'beamer))
          (format "\\ac{%s}" keyword-value))
        (t (format "%s (%s)"
             title
             keyword-value)))))

  (org-link-set-parameters "abbr"
    :complete (lambda (&optional parg)
                (jf/org-link-complete-link-for
                  parg
                  :scheme "abbr"
                  :filter "_abbr"))
    :export (lambda (link description format protocol)
              (jf/denote/link-ol-abbr-with-property
                link description format protocol
                :keyword "ABBR"
                :additional-hugo-parameters "abbr=\"t\""))
    :face #'jf/org-faces-abbr
    :follow #'denote-link-ol-follow
    )

  (org-link-set-parameters "abbr-plural"
    :complete (lambda (&optional parg)
                (jf/org-link-complete-link-for
                  parg
                  :scheme "abbr-plural"
                  :filter "_abbr"))
    :export (lambda (link description format protocol)
              (jf/denote/link-ol-abbr-with-property
                link description format protocol
                :keyword "PLURAL_ABBR"
                :additional-hugo-parameters "abbr=\"t\" plural=\"t\""))
    :face #'jf/org-faces-abbr
    :follow #'denote-link-ol-follow
    ;;;; I'm unclear if/how I want to proceed with this
    ;; :store (lambda (jf/org-link-store-link-for :scheme "abbr-plural"))
    )

  (org-link-set-parameters "date"
    :complete #'jf/denote/link-complete-date
    :export #'jf/denote/link-export-date
    :face #'jf/org-faces-date
    :follow #'jf/denote/link-follow-date)

  (cl-defun jf/denote/link-complete-date (&optional parg)
    "Prompt for the given DATE.

While we are prompting for a year, month, and day; a reminder
that this is intended to be conformant with the TIME element.
But for my typical use I write these as either years; years and
months; and most often year, month, and days.

PARG is for conformant method signature."
    (format "date:%s" (org-read-date)))

  (cl-defun jf/denote/link-export-date (link description format protocol)
    "Export a date for given LINK, DESCRIPTION, FORMAT, and PROTOCOL."
    (cond
      ((or (eq format 'html) (eq format 'md))
        (format "<time datetime=\"%s\" title=\"%s\">%s</time>"
          link link description))
      ((eq format 'beamer)
        (format "%s" description))
      (t (format "%s (%s)" description link))))

  (cl-defun jf/denote/link-follow-date (date &optional parg)
    "Follow the given DATE; uncertain what that means.

PARG is for a conformant method signature."
    (message "TODO, implement link for %s" date))

  ;; I want to be able to link and export my epigraph entries.  For now,
  ;; I'm going to focus on the HTML and Markdown version; as most often
  ;; when I include an epigraph it is for my blog posts.
  (cl-defun jf/denote/link-ol-epigraph-link (link
                                              description format protocol
                                              &key
                                              additional-hugo-parameters
                                              (jf/exporting-org-to-tor
                                                jf/exporting-org-to-tor))
    "Export the epigraph for given LINK, DESCRIPTION, PROTOCOL, and FORMAT.

  NOTE: This only works for blog export.
  TODO: Consider how to expand beyond blog support."
    (cond
      ((and
         jf/exporting-org-to-tor
         (or (eq format 'html) (eq format 'md)))
        (format "{{< epigraph key=\"%s\" >}}" link))
      ((or (eq format 'html) (eq format 'md))
        (concat "<blockquote>\n"
          (jf/epigraph-text-for :identifier link)
          "\n</blockquote>"))
      (t nil)))

  (cl-defun jf/epigraph-text-for (&key identifier)
    "Return the epigraph text for `denote' IDENTIFIER."
    (let ((filename
            (denote-get-path-by-id identifier)))
      (with-current-buffer (find-file-noselect filename)
        (let ((text
                (s-join "\n\n" (org-element-map
                                 (org-element-parse-buffer)
                                 'paragraph
                                 (lambda (p) (caddr p))))))
          (if (cadar (org-collect-keywords '("POEM")))
            (format "<pre class=\"poem\">\n%s\n</pre>" text)
            (format "%s" text))))))

  (org-link-set-parameters "epigraph"
    :complete (lambda (&optional parg)
                (jf/org-link-complete-link-for
                  parg
                  :scheme "epigraph"
                  :subdirectory "epigraphs"))
    :export (lambda (link description format protocol)
              (jf/denote/link-ol-epigraph-link
                link description format protocol))
    :face #'jf/org-faces-epigraph
    :follow #'denote-link-ol-follow)

  (defface jf/org-faces-date '((default :inherit link))
    "Face used to style `org-mode' date links in the buffer."
    :group 'denote-faces
    :package-version '(denote . "0.5.0"))

  (defface jf/org-faces-epigraph '((default :inherit link))
    "Face used to style `org-mode' epigraph links in the buffer."
    :group 'denote-faces
    :package-version '(denote . "0.5.0"))

  (defface jf/org-faces-abbr '((default :inherit link))
    "Face used to style `org-mode' abbr links in the buffer."
    :group 'denote-faces
    :package-version '(denote . "0.5.0"))

  (defun jf/denote/link-ol-export (link description format)
    "Export a `denote:' link from Org files.

The LINK, DESCRIPTION, FORMAT, and PROTOCOL are handled by the
export backend.

When USE_HUGO_SHORTCODE is given use glossary based exporting."
    (let* ((path-id
             (denote-link--ol-resolve-link-to-target link :path-id))
            (path
              (file-name-nondirectory (car path-id)))
            (export-plist
              (jf/denote/plist-for-export-of-id link))
            (title
              (plist-get export-plist :title))
            (url
              (plist-get export-plist :url))
            (glossary_key
              (plist-get export-plist :key))
            (desc
              (or description title)))
      (if url
        (cond
          ((and jf/exporting-org-to-tor glossary_key)
            (format "{{< glossary key=\"%s\" >}}" glossary_key))
          ;; Use the TakeOnRules shortcode that leverages Hugo built-in
          ((and jf/exporting-org-to-tor
             (s-starts-with? "https://takeonrules.com/" url))
            (if (s-contains? "/series/" url)
              (format "{{< linkToSeries \"%s\" >}}"
                (nth 4
                  (s-split "/"
                    "https://takeonrules.com/series/one-two-three/")))
              (format "{{< linkToPath \"%s\" >}}"
                (s-trim
                  (s-replace "https://takeonrules.com/" "/" url)))))
          ((eq format 'html)
            (format "<a href=\"%s\">%s</a>" url desc))
          ((eq format 'md) (format "[%s](%s)" desc url))
          ((or (eq format 'latex) (eq format 'beamer))
            (format "\\href{%s}{%s}"
              (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" path)
              desc))
          ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
          ((eq format 'ascii) (format "[%s] <denote:%s>" desc path))
          (t path))
        desc)))

  (advice-add #'denote-link-ol-export
    :override #'jf/denote/link-ol-export
    '((name . "wrapper")))

  (defun jf/associate-blog-post-url-with-identifier (url identifier)
    "Associate given URL with the `denote' IDENTIFIER."
    (message "Associating URL: %s with IDENTIFIER: %s." identifier url)
    (let* ((filename
             (denote-get-path-by-id identifier))
            (buffer
              (find-file-noselect filename)))
      (with-current-buffer buffer
        (jf/export-org-to-tor--global-buffer-prop-ensure
          :key "ROAM_REFS"
          :plist (jf/org-keywords-as-plist :keywords-regexp "ROAM_REFS")
          :default url)
        (save-buffer))))

  (defun jf/org-mode/convert-link-type (&optional element)
    "Replace the given `org-mode' ELEMENT's link type and text."
    (interactive)
    (let* ((types
             '("abbr" "abbr-plural" "denote"))
            (element
              (or element (org-element-context))))
      (if (eq 'link (car element))
        (let ((type
                (org-element-property :type (org-element-context)))
               (denote-id
                 (plist-get (cadr element) :path)))
          (if (member type types)
            (when-let ((new-type
                         (completing-read "New link type: "
                           types nil t)))
              (if-let ((new-text
                         (jf/denote/org-property-from-id
                           :identifier denote-id
                           :property
                           (cond
                             ((string= "abbr" new-type)
                               "ABBR")
                             ((string= "abbr-plural" new-type)
                               "PLURAL_ABBR")
                             ((string= "denote" new-type)
                               "TITLE")))))
                (progn
                  (replace-regexp-in-region
                    (concat "\\[\\[\\([^:]+\\):\\([0-9A-Z]+\\)"
                      "\\]\\[\\([^]]+\\)\\]\\]")
                    (format "[[%s:%s][%s]]"
                      new-type denote-id new-text)
                    (org-element-property :begin element)
                    (org-element-property :end element))
                  (org-link-descriptive-ensure))
                (user-error "Expected denote-id %s to have a %s acceptable property" denote-id new-type)))
            (user-error "Current element is of type %s; it must be one of the following: %s" type types)))
        (user-error "Current element must be of type 'link; it is %S" (car element)))))

  (defun jf/menu--org-capture-firefox ()
    "Create an `denote' entry from Firefox page."
    (interactive)
    (require 'grab-mac-link)
    (jf/denote/capture-reference :url (car (grab-mac-link-firefox-1))))

  (defun jf/menu--org-capture-safari ()
    "Create an `denote' entry from Safari page."
    (interactive)
    (require 'grab-mac-link)
    (jf/denote/capture-reference :url (car (grab-mac-link-safari-1))))

  (defun jf/capture/denote/from/eww-data ()
    "Create an `denote' entry from `eww' data."
    (interactive)
    (jf/denote/capture-reference :url (plist-get eww-data :url)))

  (defun jf/capture/denote/from/elfeed-show-entry ()
    "Create `denote' entry from `elfeed-show-entry'."
    (interactive)
    (jf/denote/capture-reference
      :url (elfeed-entry-link elfeed-show-entry)))

  (defun jf/menu--bookmark-safari ()
    "Create `bookmark+' for current Safari page."
    (interactive)
    (require 'grab-mac-link)
    (let* ((url-and-title
             (grab-mac-link-safari-1))
            (title
              (read-string
                (concat "URL: " (car url-and-title) "\nTitle: ")
                (cadr url-and-title))))
      (bmkp-url-target-set (car url-and-title) nil title)))

  ;; I'd love to avoid re-fetching the content.
  (cl-defun jf/sanitized-dom (&key html)
    "Convert HTML to sanitized dom."
    (with-temp-buffer
      (insert html)
      (org-web-tools--sanitized-dom)
      (buffer-string)))

  (cl-defun jf/denote/capture-reference (&key url
                                          (keywords (denote-keywords-prompt))
                                          (domain "references"))
    "Create a `denote' entry in DOMAIN for URL with KEYWORDS.

The DOM could be as sanitized by `org-web-tools--sanitized-dom'."
    (let* ((url
             (or url (org-web-tools--get-first-url)))
            (dom
              (plz 'get url :as #'org-web-tools--sanitized-dom))
            (title-readable
              (org-web-tools--eww-readable dom))
            (title
              (org-web-tools--cleanup-title
                (or (car title-readable) "")))
            (article
              (org-web-tools--html-to-org-with-pandoc
                (cdr title-readable))))
      (denote title
        keywords
        'org
        (f-join (denote-directory) domain)
        nil
        (concat "#+ROAM_REFS: " url "\n\n" article))))

  (defun jf/denote/archive-timesheet-month ()
    "Cut the month agenda and create a `denote' note."
    (interactive)
    (let* ((headline
             (jf/org-agenda-headline-for-level :level 2))
            (title
              (org-element-property :title headline)))
      (org-cut-subtree)
      (denote (concat title " Time Sheet")
        '("timesheet" "scientist")
        'org
        (f-join (denote-directory) "scientist"))
      (yank)
      (save-buffer)))

  (cl-defun jf/org-mode/add-series-to-file (&key
                                             file series drop-tags all)
    "Add SERIES to FILE.

Optionally DROP-TAGS, as there may have been a TAG associated
with the series."
    (interactive)
    (with-current-buffer (if file
                           (find-file-noselect file)
                           (current-buffer))
      (when (or current-prefix-arg all (jf/blog-entry?))
        (let ((series
                (or series
                  (completing-read "Series: "
                    (jf/tor-series-list) nil t))))
          (unless (and (jf/blog-entry?)
                    (s-contains? "#+HUGO_CUSTOM_FRONT_MATTER: :series "
                      (buffer-substring-no-properties
                        (point-min) (point-max))))
            (save-excursion
              (goto-char (point-min))
              (re-search-forward "^$")
              (insert "\n#+HUGO_CUSTOM_FRONT_MATTER: :series " series)
              (save-buffer)))
          (let* ((file
                   (buffer-file-name))
                  (id
                    (denote-retrieve-filename-identifier file))
                  (file-type
                    'org)
                  (title
                    (denote-retrieve-title-value file file-type))
                  (keywords
                    (seq-difference
                      (denote-retrieve-keywords-value file file-type)
                      (flatten-list drop-tags)))
                  (extension
                    (denote-get-file-extension file))
                  (dir
                    (file-name-directory file))
                  (new-name
                    (denote-format-file-name
                      dir id keywords title extension series)))
            (denote-rename-file-and-buffer file new-name)
            (denote-update-dired-buffers))))))

  (transient-define-suffix jf/denote-org-capture/filename-set ()
    "Work with `jf/denote-org-capture/filename'"
    :description
    '(lambda ()
       (concat
         "Denote Capture Filename: "
         (propertize
           (format "%s"
             (and denote-last-path
               (file-exists-p denote-last-path)
               (denote-retrieve-filename-title denote-last-path)))
           'face 'transient-argument)))
    (interactive)
    (if denote-last-path
      (setq denote-last-path nil)
      (let ((fname
              (buffer-file-name (current-buffer))))
        (setq denote-last-path
          (and (denote-file-is-note-p  fname) fname))))))

(use-package consult-notes
  ;;Let’s add another way at looking up files.  I appreciate the ability
  ;;to search all files and start with a character (e.g. =b=) followed
  ;;by <space> to filter to the note source keyed as =s=
  ;;(e.g. Scientist).
  :straight (:type git :host github :repo "mclear-tools/consult-notes")
  ;; :after (consult denote)
  :bind
  ("H-d s" . 'consult-notes-search-in-all-notes)
  ("H-f" . 'consult-notes)
  ;; Ensuring that I search my denote/scientist sub-directory, which is
  ;; excluded from it's containing project's git repository.
  :custom (consult-notes-use-rg t)
  (consult-notes-ripgrep-args
    (concat
      "rg --null --line-buffered --color=never --max-columns=1000 "
      "--path-separator / --ignore-case --no-heading --line-number "
      "--follow --hidden --glob=!.git/ -L --sortr=accessed"))
  :commands (consult-notes
              consult-notes-search-in-all-notes))

(use-package emojify
  ;; All the people using emojiis; why not
  :straight t
  :config
  (defun --set-emoji-font (frame)
    "Adjust font settings of FRAME so Emacs can display emoji properly."
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
  ;; Hook for when a frame is created with emacsclient see
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
  (add-hook 'after-make-frame-functions '--set-emoji-font))

(use-package sdcv-mode
  ;; This follows from
  ;; http://mbork.pl/2017-01-14_I'm_now_using_the_right_dictionary
  ;;
  ;; Namely I want to use a more inspiring dictionary for the poetry and
  ;; prose.
  :straight (sdcv-mode :type git :host github :repo "gucong/emacs-sdcv")
  :bind ("C-c C-'" . sdcv-search))


(use-package unicode-fonts
  ;; Before the emojii...
  :straight t
  :config (unicode-fonts-setup))

(use-package unfill
  ;; Provides the reverse of ~fill-paragraph~, and a toggle fill and
  ;; unfill.  In fact, the unfill/fill function of Emacs was the first
  ;; editor function I saw (shown to me by a friend in 2005) that had me
  ;; strongly consider Emacs. Alas I was not prepared for Emacs.
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
  ;; The rules of “titlecase” are confounding.  The ~titlecase.el~
  ;; package provides numerous ways to cast a string to “titlecase.”  I
  ;; chose wikipedia style as a quasi-opinionated compromise.
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
  ;; Type \"C-;\" to select current symbol and all matches; Then edit at
  ;; multiple points.
  :straight t)

(use-package ispell
  :straight (:type built-in)
  :config
  (setq-default ispell-program-name "aspell"))

(use-package jinx
  ;; `brew install enchant`
  :straight t
  :bind ("M-$" . #'jink-correct)
  :hook (emacs-startup . #'global-jinx-mode)
  :config
  (add-to-list 'jinx-exclude-faces '(prog-mode font-lock-string-face))
  ;; From https://github.com/minad/jinx/wiki
  (defun jinx--add-to-abbrev (overlay word)
    "Add abbreviation to `global-abbrev-table'.
The misspelled word is taken from OVERLAY.  WORD is the corrected word."
    (let ((abbrev (buffer-substring-no-properties
                    (overlay-start overlay)
                    (overlay-end overlay))))
      (message "Abbrev: %s -> %s" abbrev word)
      (define-abbrev global-abbrev-table abbrev word)))


  (advice-add 'jinx--correct-replace :before #'jinx--add-to-abbrev))


(use-package crdt
  ;; For remote code sharing/pairing
  :straight t)

(use-package code-review
  ;; Shall we review code via Magit?  I believe the answer must be yes.
  :after magit
  :straight (code-review
              :type git
              :host github
              :repo "phelrine/code-review"
              :branch "fix/closql-update")
  :bind (:map forge-pullreq-mode-map
          (("C-c r" . #'code-review-forge-pr-at-point)))
  :config
  (add-hook 'code-review-mode-hook #'emojify-mode)
  (setq code-review-fill-column 80))

(use-package treesit
  :straight (:type built-in)
  :init
  (setq treesit-font-lock-level 4)
  :preface
  (defun jf/treesit/function-select ()
    "Select the current function at point."
    (interactive)
    (if-let ((func (treesit-defun-at-point)))
      (progn
        (goto-char (treesit-node-start func))
        (call-interactively #'set-mark-command)
        (goto-char (treesit-node-end func)))
      (user-error "No function to select")))

  (defun jf/treesit/wrap-rubocop (&optional given-cops)
    "Wrap the current ruby region by disabling/enabling the GIVEN-COPS."
    (interactive)
    (if (derived-mode-p 'ruby-ts-mode 'ruby-mode)
      (if-let ((region
                 (jf/treesit/derive-region-for-rubocop)))
        (let ((cops
                (or given-cops
                  (completing-read-multiple "Cops to Disable: "
                    jf/rubocop/list-all-cops nil t))))
          (save-excursion
            (goto-char (cdr region))
            (call-interactively #'crux-move-beginning-of-line)
            (let ((indentation
                    (s-repeat (current-column) " ")))
              (goto-char (cdr region))
              (insert "\n"
                (s-join "\n"
                  (mapcar
                    (lambda (cop)
                      (concat indentation "# rubocop:enable " cop))
                    cops)))
              (goto-char (car region))
              (beginning-of-line)
              (insert
                (s-join "\n"
                  (mapcar
                    (lambda (cop)
                      (concat indentation "# rubocop:disable " cop))
                    cops))
                "\n"))))
        (user-error "Not a region nor a function"))
      (user-error "%s is not derived from a ruby mode" major-mode)))

  (defun jf/treesit/derive-region-for-rubocop ()
    "Return `cons' of begin and end positions of region."
    (cond
      ;; When given, first honor the explicit region
      ((use-region-p)
        (cons (region-beginning) (region-end)))
      ;; Then honor the current function
      ((treesit-defun-at-point)
        (cons (treesit-node-start (treesit-defun-at-point))
          (treesit-node-end (treesit-defun-at-point))))
      ;; Then fallback to attempting to find the containing
      ;; class/module.
      (t
        (when-let ((node
                     (treesit-parent-until
                       (treesit-node-at (point))
                       (lambda (n) (member (treesit-node-type n)
                                     '("class" "module"))))))
          (cons (treesit-node-start node) (treesit-node-end node))))))

  ;; This function, tested against Ruby, will return the module space
  ;; qualified method name (e.g. Hello::World#method_name).
  (defun jf/treesit/yank-qualified-method-fname ()
    "Return the fully qualified name of method at point.  If not on a
method, get the containing class."
    (if-let ((func (treesit-defun-at-point)))
      ;; Instance method or class method?
      (let* ((method_type
               (if (string= "method"
                     (treesit-node-type func))
                 "#" "."))
              (method_name
                (treesit-node-text
                  (car (treesit-filter-child
                         func
                         (lambda (node)
                           (string= "identifier"
                             (treesit-node-type node)))))))
              (module_space
                (s-join "::" (jf/treesit/module_space func))))
        (if current-prefix-arg
          module_space
          (concat module_space method_type method_name)))
      (let ((current-node
              (treesit-node-at (point))))
        (s-join "::" (jf/treesit/module_space current-node)))))

  ;; Handles the following Ruby code:
  ;;
  ;;   module A::B
  ;;     module C
  ;;     end
  ;;     C::D = Struct.new do
  ;;       def call
  ;;       end
  ;;     end
  ;;   end
  ;; Special thanks to https://eshelyaron.com/posts/2023-04-01-take-on-recursion.html
  (defun jf/treesit/module_space (node &optional acc)
    (if-let ((parent
               (treesit-parent-until
                 node
                 (lambda (n) (member (treesit-node-type n)
                               '("class" "module" "assignment")))))
              (parent_name
                (treesit-node-text
                  (car
                    (treesit-filter-child
                      parent
                      (lambda (n)
                        (member (treesit-node-type n)
                          '("constant" "scope_resolution"))))))))
      (jf/treesit/module_space parent (cons parent_name acc))
      acc)))

(use-package treesit-auto
  :straight (:host github :repo "renzmann/treesit-auto")
  :config (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(use-package scopeline
  ;; Show the scope info of methods, blocks, if/case statements.  This
  ;; is done via an overlay for "blocks" that are more than 5 (default)
  ;; lines
  :straight (:host github :repo "jeremyf/scopeline.el")
  ;; The original `scopeline' prefix was creating line height issues for
  ;; my font of choice.  Namely adding just a bit more spacing for the
  ;; `scopeline' overlay, thus making line heights inconsistent.
  :config (setq scopeline-overlay-prefix "  ~ ")
  :hook ((ruby-mode ruby-ts-mode) . scopeline-mode))

(use-package bundler
  ;; For Ruby package management
  :straight (bundler
              :type git
              :host github
              :repo "endofunky/bundler.el"))

(use-package csv-mode
  :straight t
  ;; By default I want to show the separator character.
  :custom (csv-invisibility-default nil)
  ;; Always enter CSV mode in align mode; makes it easier to read.
  :hook (csv-mode . csv-align-mode))

(use-package docker
  ;; https://github.com/Silex/docker.el
  ;; A reality of modern development is that things happen in Docker.
  :straight t)

(use-package dockerfile-mode
  ;; Given that I interact with docker files, I should have some syntax
  ;; awareness.
  :straight t)

(use-package editorconfig
  ;; “EditorConfig helps maintain consistent coding styles for multiple
  ;; developers working on the same project across various editors and
  ;; IDEs.”  See https://editorconfig.org/#overview for more details.
  :straight t
  :config
  (editorconfig-mode 1))

(use-package heex-ts-mode
  :straight t)

(use-package mix
  :straight t
  :config
  (keymap-set mix-minor-mode-map "C-c e"
    #'mix-minor-mode-command-map)
  (keymap-set mix-minor-mode-map "C-c d"
    #'jf/duplicate-current-line-or-lines-of-region)
  (add-hook 'elixir-ts-mode-hook 'mix-minor-mode))

(use-package elixir-ts-mode
  :after heex-ts-mode
  :straight t)

;; (use-package flymake-elixir
;;   :straight t
;;   :config
;;   (add-hook 'elixir-ts-mode-hook 'flymake-elixir-load))

(use-package emacs
  :hook (emacs-lisp-mode . jf/emacs-lisp-mode-hook)
  :preface
  (defun jf/emacs-lisp-mode-hook ()
    ;; 72 is what I've found works best on exporting to my blog.
    (setq-local fill-column 72)))

(use-package go-mode
  :straight t
  :hook ((go-mode go-ts-mode) . jf/go-mode)
  :config
  ;; See https://pkg.go.dev/golang.org/x/tools/cmd/goimports
  (setq gofmt-command "goimports")
  (defun jf/go-mode ()
    (setq-local tab-width 2))
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-ts-mode
  :straight (:type built-in)
  :config
  (setq go-ts-mode-indent-offset 2)
  ;; Copied from
  ;; https://github.com/Homebrew/brew/blob/c2ed3327c605c3e738359c9807b8f4cd6fec09eb/Cellar/emacs-plus%4029/29.3/share/emacs/29.3/lisp/progmodes/go-ts-mode.el#L115-L206
  ;;
  ;; Modifications made to remove parse error.  Someday I'll get rid of
  ;; this.
  (setq go-ts-mode--font-lock-settings
    (treesit-font-lock-rules
      :language 'go
      :feature 'bracket
      '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

      :language 'go
      :feature 'comment
      '((comment) @font-lock-comment-face)

      :language 'go
      :feature 'constant
      `([(false) (nil) (true)] @font-lock-constant-face
         ,@(when (go-ts-mode--iota-query-supported-p)
             '((iota) @font-lock-constant-face))
         (const_declaration
           (const_spec name: (identifier) @font-lock-constant-face)))

      :language 'go
      :feature 'delimiter
      '((["," "." ";" ":"]) @font-lock-delimiter-face)

      :language 'go
      :feature 'definition
      '((function_declaration
          name: (identifier) @font-lock-function-name-face)
         (method_declaration
           name: (field_identifier) @font-lock-function-name-face)
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;; method_spec is, as of 2024-05-01 and Emacs v29.3, something
         ;;; that breaks with the installed tree-sitter language.
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;; (method_spec
         ;;  name: (field_identifier) @font-lock-function-name-face)
         (field_declaration
           name: (field_identifier) @font-lock-property-name-face)
         (parameter_declaration
           name: (identifier) @font-lock-variable-name-face)
         (short_var_declaration
           left: (expression_list
                   (identifier) @font-lock-variable-name-face
                   ("," (identifier) @font-lock-variable-name-face)*))
         (var_spec name: (identifier) @font-lock-variable-name-face
           ("," name: (identifier) @font-lock-variable-name-face)*))

      :language 'go
      :feature 'function
      '((call_expression
          function: (identifier) @font-lock-function-call-face)
         (call_expression
           function: (selector_expression
                       field: (field_identifier)
                       @font-lock-function-call-face)))

      :language 'go
      :feature 'keyword
      `([,@go-ts-mode--keywords] @font-lock-keyword-face)

      :language 'go
      :feature 'label
      '((label_name) @font-lock-constant-face)

      :language 'go
      :feature 'number
      '([(float_literal)
          (imaginary_literal)
          (int_literal)] @font-lock-number-face)

      :language 'go
      :feature 'string
      '([(interpreted_string_literal)
          (raw_string_literal)
          (rune_literal)] @font-lock-string-face)

      :language 'go
      :feature 'type
      '([(package_identifier) (type_identifier)] @font-lock-type-face)

      :language 'go
      :feature 'property
      '((selector_expression field: (field_identifier)
          @font-lock-property-use-face)
         (keyed_element (_ (identifier) @font-lock-property-use-face)))

      :language 'go
      :feature 'variable
      '((identifier) @font-lock-variable-use-face)

      :language 'go
      :feature 'escape-sequence
      :override t
      '((escape_sequence) @font-lock-escape-face)

      :language 'go
      :feature 'error
      :override t
      '((ERROR) @font-lock-warning-face))))

(use-package go-imenu
  :straight t
  :hook (go-mode . go-imenu-setup))

;; (use-package flymake-go
;;   :straight t)

(use-package ruby-mode
  ;; My language of choice for professional work.
  :straight (:type built-in)
  :custom (ruby-flymake-use-rubocop-if-available nil)
  :bind
  (:map ruby-mode-map
    (("C-M-h" . jf/treesit/function-select)
      ("C-c y f" . jf/treesit/yank-qualified-method-fname)
      ("C-c w r" . jf/treesit/wrap-rubocop)
      ("M-{" . ruby-beginning-of-block)
      ("M-}" . ruby-end-of-block)))
  :hook ((ruby-mode ruby-ts-mode) . #'jf/ruby-mode-configurator)
  :config
  (defun jf/ruby-mode-configurator ()
    (eldoc-mode t)
    (setq-local fill-column 80))
  (defun jf/require-debugger ()
    "Determine the correct debugger based on the Gemfile."
    (let ((gemfile-lock
            (f-join (projectile-project-root) "Gemfile.lock")))
      (if-let* ((f-exists? gemfile-lock)
                 (debuggers
                   (s-split "\n"
                     (shell-command-to-string
                       (concat
                         "rg \"^ +(byebug|debugger|pry-byebug|debug) \""
                         " " gemfile-lock
                         " -r '$1' --only-matching | uniq")))))
        (cond
          ((member "byebug" debuggers)
            "require 'byebug'; byebug")
          ((member "debug" debuggers)
            "require 'debug'; binding.break")
          ((member "debugger" debuggers)
            "require 'debugger'; debugger")
          ((member "pry-byebug" debuggers)
            "require 'pry-byebug'; binding.pry")
          (t "require 'debug'; binding.break"))
        "require 'debug'; binding.break"))))

(use-package python
  :straight (:type built-in)
  :hook (python-mode . jf/python-mode-configurator)
  :bind (:map python-mode-map ("M-." . xref-find-definitions))
  :config
  (defun jf/python-mode-configurator ()
    (eldoc-mode t)
    (python-docstring-mode t)
    (setq-default python-indent-offset 4)
    (setq-local fill-column 80))
  (defun jf/python-ts-mode-configurator ()
    (define-key python-ts-mode-map
      (kbd "M-.") #'xref-find-definitions)
    (jf/python-mode-configurator))
  (add-hook 'python-ts-mode-hook #'jf/python-ts-mode-configurator))

(use-package flymake-ruff
  :straight t)
;; :hook (eglot-managed-mode . flymake-ruff-load))

(use-package python-docstring
  :straight t)

(use-package pydoc-info
  :straight t
  :config
  (dolist (python '(python-mode python-ts-mode))
    (info-lookup-add-help
      :mode python
      :parse-rule 'pydoc-info-python-symbol-at-point
      :doc-spec
      '(("(python)Index" pydoc-info-lookup-transform-entry)
         ("(sphinx)Index" pydoc-info-lookup-transform-entry)))))

(use-package virtualenvwrapper
  :straight t
  :config
  ;; if you want interactive shell support
  (venv-initialize-interactive-shells)
  ;; if you want eshell support note that setting `venv-location` is not
  ;; necessary if you use the default location (`~/.virtualenvs`), or if
  ;; the the environment variable `WORKON_HOME` points to the right
  ;; place
  (venv-initialize-eshell)
  (setq projectile-switch-project-action
    '(lambda ()
       (venv-projectile-auto-workon)
       (projectile-find-file))))

(use-package json-mode
  ;; The web's data structure of choice is JSON.
  :straight t)

(use-package json-reformat
  ;; Because JSON can be quite ugly, I want something to help tidy it
  ;; up.
  :straight t
  :after json-mode
  :init (setq json-reformat:indent-width 2))

(use-package hl-todo
  :straight t
  :config (global-hl-todo-mode))


(use-package magit-todos
  ;; Package that adds a `magit' section highlighting todos in the
  ;; current repository; and even highlighting what todos were added in
  ;; the branch but not in main.
  :config (magit-todos-mode)
  :commands (magit-todos-list)
  :custom (magit-todos-exclude-globs '(".git/" "public/"))
  (magit-todos-insert-after
    '(bottom) nil nil
    "Changed by setter of obsolete option `magit-todos-insert-at'")
  :straight (:host github :repo "alphapapa/magit-todos"))

(use-package lua-mode
  ;; For working with https://www.hammerspoon.org/
  :straight t)

(use-package markdown-mode
  :straight t
  :bind (:map markdown-mode-map ("C-c C-j" . jf/project/jump-to-task))
  :hook (((markdown-mode markdown-ts-mode) . jf/markdown-mode-configurator))
  :mode (("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . gfm-mode)
          ("\\.markdown\\'" . gfm-mode))
  :config
  (defun jf/markdown-mode-configurator ()
    (setq-local markdown-hide-urls t)
    (visual-line-mode 1))
  (setq markdown-command
    ;; In the early days of Apple Silicon, Pandoc was only available
    ;; through an odd installation.  As those early days have passed,
    ;; Pandoc is now available in a native form for Apple Silicon.
    (if (file-exists-p "/opt/homebrew/bin/pandoc")
      "/opt/homebrew/bin/pandoc"
      "/usr/local/bin/pandoc"))
  (font-lock-add-keywords 'markdown-mode
    '(("{{[^}]+}}" . 'font-lock-function-name-face))))

(defun jf/markdown-toc (&optional depth)
  "Extract DEPTH of headings from the current Markdown buffer.

The generated and indented TOC will be inserted at point."
  (interactive "P")
  (let ((max-depth (or depth 3)) toc-list)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\(##+\\)\\s-+\\(.*\\)" nil t)
        (let* ((level
                 (length (match-string 1)))
                (heading-text
                  (match-string 2))
                (heading-id
                  (downcase (replace-regexp-in-string
                              "[[:space:]]+" "-" heading-text))))
          (when (<= level max-depth)
            (push (cons level
                    (cons heading-text heading-id))
              toc-list)))))
    (setq toc-list (reverse toc-list))
    (dolist (item toc-list)
      (let* ((level
               (car item))
              (heading-text
                (cadr item))
              (heading-id
                (cddr item))
              (indentation
                (make-string (- (* 2 (1- level)) 2) ?\ ))
              (line
                (format "- [%s](#%s)\n" heading-text heading-id)))
        (setq markdown-toc
          (concat markdown-toc (concat indentation line)))))
    (insert markdown-toc)))

(use-package plantuml-mode
  ;; A mode for working with PlantUML.  See https://plantuml.com
  :config (setq plantuml-executable-path (concat
                                           (getenv "HB_PATH")
                                           "/bin/plantuml")
            plantuml-default-exec-mode 'executable
            org-plantuml-executable-path (concat
                                           (getenv "HB_PATH")
                                           "/bin/plantuml")
            org-plantuml-exec-mode 'executable)
  :mode (("\\.plantuml\\'" . plantuml-mode))
  :mode (("\\.puml\\'" . plantuml-mode))
  :straight t)

(use-package rspec-mode
  ;; I write most of my Ruby tests using rspec.  This tool helps manage
  ;; that process.
  :straight t
  ;; Ensure that we’re loading ruby-mode before we do any rspec loading.
  :after ruby-mode
  :custom
  (rspec-docker-container "web")
  (rspec-use-spring-when-possible t)
  (rspec-use-docker-when-possible t)
  (rspec-docker-cwd "./")
  (rspec-docker-command "docker compose exec")
  :hook ((dired-mode . rspec-dired-mode)
          (rspec-mode . jf/rspec-mode-hook))
  ;; Dear reader, make sure that you can jump from spec and definition.
  ;; And in Ruby land when you have lib/my_file.rb, the corresponding
  ;; spec should be in spec/my_file_spec.rb; and when you have
  ;; app/models/my_file.rb, the spec should be in
  ;; spec/models/my_file_spec.rb
  :bind (:map rspec-mode-map
          (("s-." .
             'rspec-toggle-spec-and-target)
            ("C-c y r" .
              'jf/yank-bundle-exec-rspec-to-clipboard)))
  :bind (:map ruby-mode-map (("s-." . 'rspec-toggle-spec-and-target)))
  :preface
  (defun jf/rspec-mode-hook ()
    (setq imenu-generic-expression
      '(("Method" "^\\s *def\\s +\\([^\(\n; ]+\\)" 1)
         ("Describe" "^\\( *\\(its?\\|specify\\|example\\|describe\\|context\\|feature\\|scenario\\) +.+\\)" 1))))
  (defun jf/yank-bundle-exec-rspec-to-clipboard ()
    "Grab a ready to run rspec command."
    (interactive)
    (let* ((filename
             (file-relative-name (buffer-file-name)
               (projectile-project-root)))
            (text
              (format "bundle exec rspec %s:%s"
                filename (line-number-at-pos))))
      (kill-new text)
      (message "Killed: %s" text)
      text))
  (defun jf/rspec-spring-p ()
    "Check the project for spring as part of the Gemfile.lock."
    (let ((gemfile-lock
            (f-join (projectile-project-root) "Gemfile.lock")))
      (and (f-exists? gemfile-lock)
        (s-present?
          (shell-command-to-string
            (concat "rg \"^ +spring-commands-rspec \" "
              gemfile-lock))))))
  ;; Out of the box, for my typical docker ecosystem, the
  ;; `rspec-spring-p' function does not work.  So I'm overriding the
  ;; default behavior to match my ecosystem.
  (advice-add #'rspec-spring-p :override #'jf/rspec-spring-p))

(use-package dotenv-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)))

(use-package ruby-interpolation
  ;; Nice and simple package for string interpolation.
  :straight t
  :hook (ruby-mode . ruby-interpolation-mode))

(use-package sql-indent
  ;; SQL, oh how I love thee and wish I worked more with thee.
  :straight t
  :hook (sql-mode . sqlind-minor-mode))

(use-package string-inflection
  ;; A quick way to change case and separators for words.
  :straight t)

(use-package typescript-mode
  ;; I have this for the work I once did a few years ago.  I am happiest
  ;; when I'm not working in Javascript.
  :straight t)

(use-package vterm
  ;; A terminal in Emacs.
  :straight t)

(use-package web-mode
  ;; Help consistently edit web documents of SGML markup dialetcs.
  :straight t
  :config (setq web-mode-markup-indent-offset 2
            web-mode-css-indent-offset 2
            web-mode-code-indent-offset 2)
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list `auto-mode-alist '("\\.svg\\'" . xml-mode)))

(use-package xml-format
  ;; Encountering unformatted XML is jarring; this package helps format
  ;; it for human legibility.
  :straight t
  :after nxml-mode)

(use-package yaml-mode
  ;; Oh yaml, I once thought you better than XML.  Now, you are
  ;; ubiquitous and a bit imprecise.  Still better than JSON; which
  ;; doesn't allow for comments.
  :straight t)

(use-package yaml-pro
  :hook (yaml-ts-mode . jf/yaml-mode-configurator)
  :bind (:map yaml-ts-mode-map
          ("C-c y f" . yaml-pro-copy-node-path-at-point))
  :config
  (defun jf/yaml-mode-configurator ()
    ;; (which-function-mode)
    (setq-local add-log-current-defun-function
      #'jf/yaml-node-path-at-point))
  (defun jf/yaml-node-path-at-point ()
    (yaml-pro-ts--imenu-node-label (treesit-node-at (point) 'yaml)))
  :straight t)

;; (use-package combobulate
;;   :straight (:host github :repo "mickeynp/combobulate")
;;   :hook ((json-ts-mode . combobulate-mode)
;;           (html-ts-mode . combobulate-mode)
;;           (yaml-ts-mode . combobulate-mode)))

(use-package yard-mode
  ;; My prefered Ruby documentation syntax
  :straight t
  :preface
  ;; This is not working as I had tested; it's very dependent on the
  ;; little details.  I think I may want to revisit to just work on the
  ;; current line.
  (defun jf/ruby-mode/yank-yardoc ()
    "Add parameter yarddoc stubs for the current method."
    (interactive)
    (save-excursion
      (when-let* ((func (treesit-defun-at-point))
                   (method_parameters_text
                     (treesit-node-text
                       (car
                         (treesit-filter-child
                           func
                           (lambda (node)
                             (string= "method_parameters"
                               (treesit-node-type node))))))))
        (goto-char (treesit-node-start func))
        ;; Grab the parameter names.
        (let* ((identifiers
                 (mapcar (lambda (token)
                           (replace-regexp-in-string
                             "[^a-z|_]" ""
                             (car (s-split " "
                                    (s-trim token)))))
                   (s-split "," method_parameters_text)))
                (indentation
                  (s-repeat (current-column) " ")))
          (previous-line)
          (end-of-line)
          (insert
            (concat "\n" indentation "##\n")
            (s-join "\n" (mapcar
                           (lambda (param)
                             (concat indentation "# @param "
                               param
                               " [Object]"))
                           identifiers)))))))
  :bind* (:map ruby-mode-map
           (("C-c y f" . jf/yank-current-scoped-function-name)
             ("C-c y y" . jf/ruby-mode/yank-yardoc)))
  :hook ((ruby-mode ruby-ts-mode) . yard-mode))

(use-package devdocs
  ;; Download and install documents from https://devdocs.io/ Useful for
  ;; having local inline docs.  Perhaps not always in the format that I
  ;; want, but can't have everything.
  :straight t
  :commands (devdocs-install))

(use-package dash-docs
  ;; An alternate to devdocs.  Facilitates downloading HTML files and
  ;; index.
  :straight t)

;; (use-packaqge consult-dash
;;   :straight t)

(use-package flymake
  :straight t
  ;; Don't be so hasty in syntax checking.
  :custom (flymake-no-changes-timeout 2))

(use-package prog-mode
  :straight (:type built-in)
  :hook (prog-mode . jf/prog-mode-configurator)
  :config
  ;; I didn't know about `add-log-current-defun-function' until a blog
  ;; reader reached out.  Now, I'm making a general function for
  ;; different modes.
  (defun jf/yank-current-scoped-function-name ()
    "Echo and kill the current scoped function name.

See `add-log-current-defun-function'."
    (interactive)
    (if-let ((text
               (funcall add-log-current-defun-function)))
      (progn
        (message "%s" text)
        (kill-new (substring-no-properties text)))
      (user-error "Warning: Point not on function")))
  (bind-key "C-c y f"
    #'jf/yank-current-scoped-function-name prog-mode-map)
  (bind-key "C-c y f"
    #'jf/yank-current-scoped-function-name emacs-lisp-mode-map)

  (defvar jf/comment-header-regexp/major-modes-alist
    '((emacs-lisp-mode . "^;;;+$")
       (ruby-mode . "^[[:space:]]*##+$")
       (ruby-ts-mode . "^[[:space:]]*##+$"))
    "AList of major modes and their comment headers.")

  (defun jf/comment-header-forward ()
    "Move to previous line that starts a comment block.

See `jf/comment-header-regexp/major-modes-alis'."
    (interactive)
    (let ((regexp
            (alist-get major-mode
              jf/comment-header-regexp/major-modes-alist)))
      (when (string-match-p
              regexp
              (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))
        (forward-line))
      (condition-case err
        (progn
          (search-forward-regexp regexp)
          (beginning-of-line)
          (recenter scroll-margin t)
          (pulsar-pulse-line))
        (error (goto-char (point-max))))))

  (defun jf/comment-header-backward ()
    "Move to previous line that starts a comment block.
 See `jf/comment-header-regexp/major-modes-alis'."
    (interactive)
    (let ((regexp
            (alist-get major-mode
              jf/comment-header-regexp/major-modes-alist)))
      (when (string-match-p
              regexp
              (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))
        (previous-line)
        (recenter scroll-margin t)
        (pulsar-pulse-line))
      (condition-case err
        (progn
          (search-backward-regexp regexp)
          (beginning-of-line)
          (recenter scroll-margin t)
          (pulsar-pulse-line))
        (error (goto-char (point-min))))))

  (dolist (el jf/comment-header-regexp/major-modes-alist)
    (let ((jf-map
            (intern (format "%s-map" (car el)))))
      ;; The treesitter mode maps don't seem to exist at this point
      (unless (s-contains? "-ts-" (format "%s" (car el)))
        (progn
          (define-key (symbol-value jf-map)
            (kbd "s-ESC") #'jf/comment-header-backward)
          (define-key (symbol-value jf-map)
            (kbd "C-s-]") #'jf/comment-header-forward)))))

  (defun jf/prog-mode-configurator ()
    "Do the configuration of all the things."
    ;; I'll type my own parenthesis thank you very much.
    ;; (electric-pair-mode)
    (flymake-mode 1)
    (setq truncate-lines t)
    ;; (which-function-mode)
    ))

(use-package copilot
  ;; I want to explore this a bit, but by default want it "off" and to
  ;; be as unobtrusive.
  :straight (:host github
              :repo "zerolfx/copilot.el"
              :files ("dist" "*.el"))
  :bind (:map copilot-mode-map
          (("C-c 0 <return>" . copilot-accept-completion)
            ("C-c 0 <down>" .  copilot-next-completion)
            ("C-c 0 <up>" . copilot-previous-completion)
            ("C-c 0 DEL" . copilot-clear-overlay)
            ("C-c 0 TAB" . copilot-panel-complete)
            ("C-c 0 ESC" . copilot-mode)))
  :bind ("C-c 0 ESC" . copilot-mode)
  :custom
  ;; Copilot...never give me code comment recommendations.
  (copilot-disable-predicates '(er--point-is-in-comment-p))
  (copilot-idle-delay 1.5)
  :ensure t)

(use-package ruby-ts-mode
  :straight (:type built-in)
  :config
  (defun jf/ruby-ts-mode-configurator ()
    "Configure the `treesit' provided `ruby-ts-mode'."
    ;; I encountered some loading issues where ruby-ts-mode was not
    ;; available during my understanding of the use-package life-cycle.
    (cond ((string-match "_spec.rb$" buffer-file-name)
            (rspec-mode 1)))
    (setq-local add-log-current-defun-function
      #'jf/treesit/yank-qualified-method-fname)
    (define-key ruby-ts-mode-map (kbd "C-M-h")
      #'jf/treesit/function-select)
    (define-key ruby-ts-mode-map (kbd "M-.")
      #'xref-find-definitions)
    (define-key ruby-ts-mode-map (kbd "s-.")
      #'rspec-toggle-spec-and-target)
    (define-key ruby-ts-mode-map
      (kbd "C-c y f") #'jf/yank-current-scoped-function-name)
    (define-key ruby-ts-mode-map
      (kbd "C-c y y") #'jf/ruby-mode/yank-yardoc)
    (define-key ruby-ts-mode-map
      (kbd "s-ESC") #'jf/comment-header-backward)
    (define-key ruby-ts-mode-map
      (kbd "C-s-]") #'jf/comment-header-forward)
    (define-key ruby-ts-mode-map
      (kbd "C-c w r") #'jf/treesit/wrap-rubocop)
    (define-key ruby-ts-mode-map
      (kbd "M-{") #'ruby-beginning-of-block)
    (define-key ruby-ts-mode-map
      (kbd "M-}") #'ruby-end-of-block))
  :hook (ruby-ts-mode . jf/ruby-ts-mode-configurator))

(use-package emacs
  :straight (:type built-in)
  :config
  ;; From
  ;; https://emacs.dyerdwelling.family/emacs/20230414111409-emacs--indexing-emacs-init/
  ;;
  ;; Creating some outline modes.  Which has me thinking about an
  ;; outline mode for my agenda file.
  (defun jf/emacs-lisp-mode-configurator ()
    (setq imenu-sort-function 'imenu--sort-by-name)
    (setq imenu-generic-expression
      '((nil "^;;[[:space:]]+-> \\(.*\\)$" 1)
         ("Variables"
           "^[[:space:]]*([[:space:]]*\\(cl-\\)?defvar[[:space:]]+\\([^ ]*\\)$" 2)
         ("Variables"
           "^[[:space:]]*([[:space:]]*\\(cl-\\)?defconst[[:space:]]+\\([^ ]*\\)$" 2)
         ("Variables"
           "^[[:space:]]*([[:space:]]*\\(cl-\\)?defcustom[[:space:]]+\\([^ ]*\\)$" 2)
         ("Functions"
           "^[[:space:]]*([[:space:]]*\\(cl-\\)?defun[[:space:]]+\\([^(]+\\)" 2)
         ("Macros"
           "^[[:space:]]*([[:space:]]*\\(cl-\\)?defmacro[[:space:]]+\\([^(]+\\)" 2)
         ("Types"
           "^[[:space:]]*([[:space:]]*\\(cl-\\)?defstruct[[:space:]]+\\([^(]+\\)" 2)
         ("Packages"
           "^.*([[:space:]]*use-package[[:space:]]+\\([[:word:]-]+\\)" 1)))
    (imenu-add-menubar-index))
  :hook (emacs-lisp-mode . jf/emacs-lisp-mode-configurator))

;; An odd little creature, hide all comment lines.  Sometimes this can
;; be a useful tool for viewing implementation details.
(require 'hide-comnt)

;; I've been exploring either `lsp-mode' or `eglot' and thusfar prefer
;; the lightweight nature of `eglot'.
(if t
  (progn
    (use-package eglot
      :straight t
      ;; :straight (:type built-in) The Language Server Protocol (LSP)
      ;; is a game changer; having access to that tooling is very much a
      ;; nice to have.
      :hook ((
               yaml-mode yaml-ts-mode
               angular-mode angular-ts-mode ;; npm install -g @angular/language-service@next typescript @angular/language-server
               css-mode css-ts-mode
               elixir-ts-mode
               go-mode go-ts-mode ;; https://github.com/golang/tools/tree/master/gopls
               html-mode html-ts-mode
               js-mode js-ts-mode
               json-mode json-ts-mode ;; npm install -g vscode-json-languageserver
               python-mode python-ts-mode
               ruby-mode ruby-ts-mode
               scss-mode scss-ts-mode
               typescript-ts-mode typescript-mode ;; https://github.com/typescript-language-server/typescript-language-server
               )
              . eglot-ensure)
      :config
      (defun jf/eglot-capf ()
        "Ensure `eglot-completion-at-point' preceeds everything."
        ;; I don't want `eglot-completion-at-point' to trample my other
        ;; completion options.
        ;;
        ;; https://stackoverflow.com/questions/72601990/how-to-show-suggestions-for-yasnippets-when-using-eglot
        (setq-local completion-at-point-functions
          (list (cape-capf-super
                  #'eglot-completion-at-point
                  #'tempel-expand
                  #'cape-file
                  #'cape-keyword))))
      ;; https://github.com/elixir-lsp/elixir-ls?tab=readme-ov-file
      (add-to-list 'eglot-server-programs
        '(elixir-ts-mode "~/elixir-ls/v0.20.0/language_server.sh"))
      ;; https://github.com/emacs-lsp/lsp-mode/wiki/Install-Angular-Language-server
      ;; with modifications for homebrew
      (add-to-list 'eglot-server-programs
        '(angular-mode
           "node /opt/homebrew/lib/node_modules/@angular/language-server --ngProbeLocations /opt/homebrew/lib/node_modules --tsProbeLocations /opt/homebrew/lib/node_modules --stdio"))
      (add-to-list 'eglot-servier-programs
        '(angular-ts-mode
           "node /opt/homebrew/lib/node_modules/@angular/language-server --ngProbeLocations /opt/homebrew/lib/node_modules --tsProbeLocations /opt/homebrew/lib/node_modules --stdio"))
      :hook ((eglot-managed-mode . jf/eglot-capf)))


    ;; See https://elixir-lsp.github.io/elixir-ls/getting-started/emacs/

    (use-package eglot-booster
      :straight (:host github :repo "jdtsmith/eglot-booster")
      :after eglot
      :config	(eglot-booster-mode)
      (advice-add 'eglot-completion-at-point
        :around #'cape-wrap-buster))

    (use-package eldoc
      ;; Helps with rendering documentation
      ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
      :config
      (setq eldoc-documentation-strategy
        ;; 'eldoc-documentation-enthusiast))
        'eldoc-documentation-compose-eagerly)
      (add-to-list 'display-buffer-alist
        '("^\\*eldoc"
           (display-buffer-reuse-mode-window
             display-buffer-below-selected)
           (dedicated . t)
           (body-function . prot-window-select-fit-size)))
      :straight t))
  (progn
    (use-package lsp-mode
      :straight t
      :hook ((elixir-ts-mode . lsp)
              (angular-ts-mode . lsp)
              (ruby-ts-mode . lsp)
              (python-ts-mode . lsp)
              (go-ts-mode . lsp)
              (lsp-mode . lsp-enable-which-key-integration))
      :commands lsp)

    (use-package lsp-ui
      :straight t
      :commands lsp-ui-mode)

    (use-package dap-mode
      :straight t)))

(require 'gherkin-mode)

;; I'm not working in Ruby much these days, so into the comment dustbin.
;; (load "jf-rubocop-cops.el")

(use-package project
  ;; I'm unclear why I have this and projectile declared/required.
  ;;
  ;; TODO: Can I not require this?
  :straight t)

(use-package projectile
  ;; Convenient organization and commands for projects.
  :straight t
  :custom
  (projectile-project-search-path
    '("~/git/" "~/git/converge-cloud"))
  :bind ("s-." . projectile-toggle-between-implementation-and-test)
  :config
  (projectile-mode 1)
  ;; The default relevant `magit-list-repositories'
  ;; The following command shows all "project" directories
  (defvar jf/git-project-paths
    (mapcar (lambda (el) (cons el 1)) projectile-known-projects)
    "An alist of project directories.")

  (dolist (dir (f-directories "~/git/org/denote/"))
    (add-to-list 'jf/git-project-paths
      (cons dir 1)))

  (setq magit-repository-directories jf/git-project-paths))

(use-package bookmark+
  ;; https://www.emacswiki.org/emacs/BookmarkPlus
  ;;
  ;; Enhancements to the built-in Emacs bookmarking feature.
  :straight t
  :demand t
  :init
  (use-package bookmark
    :straight (:type built-in)
    :config
    ;; On each machine I use, I have different bookmarks, yet they all
    ;; point to the same location.
    (setq bookmark-default-file "~/emacs-bookmarks.el")

    ;; Save the `bookmark-file' each time I modify a bookmark.
    (setq bookmark-save-flag 1))
  :config
  ;; (define-key bookmark-bmenu-mode-map (kbd "s-o") #'ace-window)

  ;; when this is not set to `nil' explicitly, auto-save bookmarks
  ;; gets itself into an infinite loop attempting to autosave and
  ;; write the custom value to custom-file.el.  this happens only when
  ;; the buffer associated with the bookmark has not been saved. (to
  ;; reproduce the issue, remove the customize-set-value sexp, find a
  ;; new file, and wait 30 seconds; it'll start printing messages like
  ;; mad.  C-g will eventually break the loop.)  i only use one
  ;; bookmark file so this isn't a problem but it really does seem
  ;; like a bmkp bug.
  (customize-set-value 'bmkp-last-as-first-bookmark-file nil)
  ;; auto-set bookmarks.
  (setq bmkp-automatic-bookmark-mode-delay 30))

(use-package activities
  ;; https://takeonrules.com/2024/05/18/a-quiet-morning-of-practice-to-address-an-observed-personal-computering-workflow-snag/
  ;;
  ;; https://github.com/alphapapa/activities.el
  ;;
  ;; On <2024-05-17 Fri> while working to orient to a code-base, I was
  ;; reviewing several different files.  I found it helpful to have
  ;; those files open in a specific window configuration.
  ;;
  ;; I did this exploration in between a pairing session in which a
  ;; colleague stepped away to work on something and then came back.
  ;; When she came back, I needed to set down my windwo configuration
  ;; and work on the same code from a different perspective.
  ;;
  ;; It turns out "saving" that activity would have been useful; as I
  ;; could then come back to that window state and resume that work.
  ;;
  ;; The `activities' package provides that capability.
  :straight t
  :init
  (activities-mode)
  (activities-tabs-mode)
  :bind
  (("H-a n" . activities-new)
    ("H-a d" . activities-define)
    ("H-a D" . activities-discard)
    ("H-a a" . activities-resume)
    ("H-a z" . activities-suspend)
    ("H-a k" . activities-kill)
    ("H-a RET" . #'tab-bar-switch-to-tab)
    ("H-a S" . activities-save-all)
    ("H-a s" . activities-switch)
    ("H-a b" . activities-switch-buffer)
    ("H-a g" . activities-revert)
    ("H-a l" . activities-list)))

(use-package org-bookmark-heading
  ;; Emacs bookmark support for Org-mode.
  ;;
  ;; Without this package, the default bookmarking for an `org-mode'
  ;; file is to use a regular expression (which might include things
  ;; like TODO state).  This approach is precarious.  With this package
  ;; we now store a bookmark to an `org-mode' file as the filename and
  ;; the UUID.  This does not quite solve the precarity in regards to
  ;; `denote' and it's filename convention.
  ;;
  ;; As indicated in the configuration, this is a drop-in no additional
  ;; configuration package.  Super sweet!
  :straight t)

(use-package edit-indirect
  ;; A nice package for editing regions in separate buffers.  It doesn't
  ;; appear to get the mode guess right.  I haven't used this as much as
  ;; `narrow-region'.  Perhaps it can go?
  :straight t
  :bind ("C-x n e" . #'jf/edit-indirect-region-or-function)
  :config
  (defun jf/edit-indirect-region-or-function ()
    "Create indirect buffer to edit current region or function."
    (interactive)
    (if (use-region-p)
      (edit-indirect-region (region-beginning) (region-end))
      (cond
        ;; As of <2024-05-18 Sat> emacs-lisp does not work with treesit.
        ((or (derived-mode-p 'emacs-lisp-mode)
           (derived-mode-p 'text-mode))
          (let ((beg nil)
                 (end nil))
            (save-excursion
              (if (derived-mode-p 'emacs-lisp-mode)
                  (mark-defun)
                (mark-paragraph))
              (setq beg (point))
              (setq end (mark)))
            (edit-indirect-region beg end t)))
        ((derived-mode-p 'prog-mode)
          (if-let (func (treesit-defun-at-point))
            (edit-indirect-region
              (treesit-node-start func)
              (treesit-node-end func))
            (user-error "Cannot indirect edit; "
              "Select region or be within a function")))
        (t (user-error "Unable to indirect edit current context"))))))


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
  (let ((map
          global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim))
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
  ;; Switch to `so-long' when the file gets too long for normal
  ;; processing.
  :straight t
  :bind
  (:map so-long-mode-map
    ("C-s" . isearch-forward)
    ("C-r" . isearch-backward))
  :config (global-so-long-mode 1))

(use-package olivetti
  ;; A package to "narrow" focus; providing a visually appealing
  ;; interface
  :straight t
  :hook (olivetti-mode-on . jf/olivetti-mode-on-hook)
  (olivetti-mode-off . jf/olivetti-mode-off-hook)
  :config
  ;; I'm typically aiming for 80 character `fill-column'.
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

(use-package easy-mmode
  ;; The built in package for crafting minor-modes.  I know their
  ;; utility, and continue to consider how I might apply them as an
  ;; overlay to existing modes.  I do have my one minor mode
  ;; `jf/minor-mode/presenter'.
  :straight (:type built-in)
  :config
  (defvar jf/minor-mode/presenter-map
    (let ((map
            (make-sparse-keymap)))
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
      (let ((logos-hide-cursor
              nil)
             (logos-buffer-read-only
               nil)
             (org-hide-emphasis-markers
               t))
        (call-interactively 'logos-narrow-dwim)
        (olivetti-mode t)
        (keycast-mode-line-mode t)
        (display-line-numbers-mode -1)
        (when (fboundp 'fontaine-set-preset)
          (fontaine-set-preset 'presenting))
        (when (fboundp 'vi-tilde-fringe-mode)
          (vi-tilde-fringe-mode -1))
        (when (fboundp 'git-gutter-mode)
          (git-gutter-mode -1))
        (when (fboundp 'centaur-tabs-local-mode)
          (centaur-tabs-local-mode -1))))
    "Hook when `jf/minor-mode/presenter' activated."
    :type 'hook)

  (defcustom jf/minor-mode/presenter-off-hook
    (lambda ()
      (call-interactively 'widen)
      (olivetti-mode -1)
      (keycast-mode-line-mode -1)
      ;; (setq-local  org-hide-emphasis-markers nil)
      (display-line-numbers-mode t)
      (when (fboundp 'fontaine-set-preset)
        (fontaine-set-preset 'default))
      (when (fboundp 'vi-tilde-fringe-mode)
        (vi-tilde-fringe-mode t))
      (when (fboundp 'git-gutter-mode)
        (git-gutter-mode t))
      (when (fboundp 'centaur-tabs-local-mode)
        (centaur-tabs-local-mode t)))
    "Hook when `jf/minor-mode/presenter' deactivated."
    :type 'hook))

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
  ;; An Emacs RSS reader.  I’ve used Google Reader, Feedly, Inoreader,
  ;; and Newsboat.  I wrote about
  ;; https://takeonrules.com/2020/04/12/switching-from-inoreader-to-newsboat-for-rss-reader/,
  ;; and the principles apply for Elfeed.
  :straight t
  ;; Without this, I was not seeing `rss' command.
  :demand 3
  :custom
  (elfeed-curl-timeout 90)
  (elfeed-db-directory "~/Documents/.elfeed")
  :bind ((:map elfeed-search-mode-map
           ("q" . jf/elfeed-save-db-and-bury)))
  :config
  (setq elfeed-show-entry-switch #'jf/elfeed-show-entry-switch)
  (setq-default elfeed-search-filter "@2-days-ago +unread ")
  (defun jf/elfeed-show-entry-switch(buffer)
    (switch-to-buffer buffer)
    (setq-local shr-inhibit-images t)
    (olivetti-mode 1)
    (text-scale-set 2)
    (elfeed-show-refresh))
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
    "Return a function that scrolls n LINES in `elfeed' search results.

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
    (let ((opml-body
            (cl-loop for org-file in '("~/git/org/denote/indices/public-elfeed.org")
              concat
              (with-temp-buffer
                (insert-file-contents
                  (expand-file-name org-file org-directory))
                (rmh-elfeed-org-convert-org-to-opml
                  (current-buffer))))))
      (with-current-buffer
        (find-file-noselect "~/git/takeonrules.source/static/blogroll.xml")
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
  ;; A plain text browser.  Use this to see just how bad much of the web
  ;; has become.
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
    "The before and after quotes.

`car' is inserted before the Q-tag and `cdr' is inserted after
the Q-tag.

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
    (when-let* ((datetime
                  (or
                    (dom-attr dom 'title)
                    (dom-attr dom 'datetime)))
                 (start
                   (point)))
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

(use-package git-modes
  ;; A mode for editing gitconfig files.
  :straight t)

(use-package emacsql
  :straight (:host github :repo "magit/emacsql"))

(use-package magit
  ;; A fantastic UI for git commands; the interactive rebase is an
  ;; absolute wonder tool (see
  ;; https://takeonrules.com/2023/01/12/using-the-git-interactive-staging-as-a-moment-to-facilitate-synthesis/).
  ;; Also the progenitor of `transient'
  :straight (:host github :repo "magit/magit")
  :commands (magit-process-git)
  ;; My "~/bin/editor" script was causing problems in that it was asking
  ;; to wait.
  :init (use-package with-editor
          :straight t
          :custom (with-editor-emacsclient-executable
                    (file-truename "~/bin/git_editor")))
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
  (setq magit-display-buffer-function
    #'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function
    #'magit-restore-window-configuration)
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  :hook ((with-editor-post-finish . #'magit-status))
  :bind (("C-c m" . magit-status)
          ("C-x g m" . magit-status)
          ("C-x g f" . magit-file-dispatch)
          ("C-x g d" . magit-dispatch)))

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
       "samvera/hyku-next"
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
       "scientist-softserv/space_stone-serverless"
       "scientist-softserv/utk-hyku"
       "harvard-lts/CURIOSity"
       "WGBH-MLA/ams")))

(use-package git-commit
  :straight t
  :hook ((git-commit-mode . jf/git-commit-mode-configurator))
  :bind (:map git-commit-mode-map
          (("TAB" .  #'completion-at-point)))
  :bind ("s-7" . #'jf/insert-task-type-at-point)
  :config
  (defun jf/git-commit-mode-configurator ()
    "Prepare all of the commit buffer structure"
    (setq fill-column git-commit-fill-column)
    (goto-char (point-min))
    (beginning-of-line-text)
    (when (looking-at-p "^$")
      (structured-commit/write-message))))

(use-package structured-commit
  :straight (:type git :host github
              :repo "bunnylushington/structured-commit")
  :config
  (advice-add #'structured-commit/project
    :override #'jf/structured-commit/project)
  (defun jf/structured-commit/project ()
    "Return root directory base name for project.

The `magit-gitdir' is the project's .git directory."
    (require 'f)
    (file-name-base (f-dirname (magit-gitdir)))))

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
  ;; With the time machine, travel back and forth through a files
  ;; history.
  :straight (:host github :repo "emacsmirror/git-timemachine"))

(use-package git-gutter
  ;; Show the current git state in the gutter.  As you edit a line in a
  ;; file track by git, the indicators change to reflect if this is a
  ;; modification, addition, or deletion.
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
  ;; Type ~M-x git-link~ and the function pushes the Git forge URL to
  ;; the kill ring; I’ve configured the URL to use the SHA of the commit
  ;; of the line on which I called `git-link'.  This is helpful for
  ;; sharing links with other folks.  I use this /all of the time./ See
  ;; https://github.com/sshaw/git-link.
  :config
  (defun jf/git-browse-to-repository (remote)
    "Open in external browser the current repository's given REMOTE."
    (interactive (list (git-link--select-remote)))
    (git-link-homepage remote)
    (browse-url (car kill-ring)))
  (setq git-link-use-commit t) ;; URL will be SHA instead of branch
  :straight t)

(use-package git-messenger
  ;; Sometimes I want to see more ~git~ information regarding the
  ;; current line.  `git-messenger' provides a popup that shows the
  ;; information and provides some additional options.
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
  ;; Provides a UI for undo trees.  I'm not certain what I want to do
  ;; with this.
  :straight t
  :bind (("C-z" . undo)
          ("C-s-z" . undo-tree-redo))
  :custom (undo-tree-history-directory-alist
            ("." . "~/.emacs.d/undo-tree/"))
  :init
  (unless (f-dir? "~/.emacs.d/undo-tree/")
    (mkdir "~/.emacs.d/undo-tree/"))
  :config
  (global-undo-tree-mode +1))

(with-eval-after-load 'org
  (use-package ox
    :straight (:type built-in))
  (use-package ox-hugo
    :straight t
    :after ox
    :custom
    ;; - blockquote :: for chunks of text that I attribute to other
    ;;   folks.
    ;; - marginnote :: a "dangling" note that is only partially part of
    ;;                 the conversation.
    ;; - poem :: because poetic spacing is critical.
    ;; - inline_comments :: a concession that I need different comments
    ;;                      based on context; and that marginalia may be
    ;;                      too much in some cases.
    ;; - update :: I write updates for my blog posts; corrections or
    ;;             additions based on new information.
    (org-hugo-paired-shortcodes
      "blockquote marginnote poem inlinecomment update")
    (hugo-use-code-for-kbd t)
    :config
    (defconst jf/tor-home-directory
      (file-truename "~/git/takeonrules.source")
      "The home directory of TakeOnRules.com Hugo repository.")
    ;; I want to have backticks instead of indentations; The backticks
    ;; also
    (advice-add #'org-md-example-block
      :override #'org-blackfriday-src-block)

    ;; These functions work too aggressively.  The types of lists
    ;; (ordered, definition, and unordered) are co-mingled.  This
    ;; co-mingling means that I'm not getting the behavior I want.  So
    ;; I'll proceed with the default ox-hugo behavior.
    ;;
    ;; (advice-add #'org-blackfriday-plain-list
    ;;   :override #'org-html-plain-list '((name . "wrapper")))
    ;; (advice-add #'org-blackfriday-item
    ;;   :override #'org-html-item '((name . "wrapper")))

    ;; Convert footnote to sidenote for HTML export
    (defun jf/org-hugo-sidenote (footnote-reference _contents info)
      "Transcode FOOTNOTE-REFERENCE element to Hugo sidenote shortcode.
CONTENTS is nil.  INFO is a plist holding contextual information."
      (let* ((element
               (car (org-export-get-footnote-definition
                      footnote-reference info)))
              (beg
                (org-element-property :contents-begin element))
              (end
                (org-element-property :contents-end element))
              (content
                (s-trim
                  (org-export-string-as
                    (buffer-substring-no-properties beg end)
                    'md t '(:with-toc nil)))))
        (format "{{< sidenote >}}%s{{< /sidenote >}}" content)))

    (advice-add #'org-blackfriday-footnote-reference
      :override #'jf/org-hugo-sidenote
      '((name . "wrapper")))
    (advice-add #'org-blackfriday-footnote-section
      :override (lambda (&rest rest) ())
      '((name . "wrapper")))

    (advice-add #'org-hugo−-get-front-matter
      :filter-return #'jf/org-hugo-rewrite-tags)
    (defun jf/org-hugo-rewrite-tags (info)
      "Turn OneWordTags into one-word-tags."
      (require 's)
      (dolist (field '(:categories :tags))
        (when (plist-get info field)
          (plist-put info field
            (mapcar #'s-dashed-words (plist-get info field)))))
      info)

    (defun jf/org-md-quote-block (quote-block contents info)
      "Render a QUOTE-BLOCK with CONTENTS and INFO.

Either render via the standard markdown way or when exporting to
Take on Rules using the \"blockquote\" special block."
      (if jf/exporting-org-to-tor
        (progn
          (org-element-put-property quote-block :type "blockquote")
          (org-hugo-special-block quote-block contents info))
        ;; The original md quote block method; probably a better way to
        ;; do this.
        (replace-regexp-in-string
          "^" "> "
          (replace-regexp-in-string "\n\\'" "" contents))))
    (advice-add #'org-md-quote-block :override #'jf/org-md-quote-block)

    (setq org-hugo-base-dir "~/git/takeonrules.source")

    (defvar jf/org-macros-setup-filename
      "~/git/dotemacs/lib/org-macros.setup"
      "The path to the file that has inline org macros.")

    (defvar jf/exporting-org-to-tor nil
      "Not nil while performing export of org file to Take on Rules.")

    (cl-defun jf/export-org-to-tor (&key (buffer (current-buffer)))
      "Export current org BUFFER for TakeOnRules post."
      (interactive)
      ;; Ensure that we have an ID property.
      (setq jf/exporting-org-to-tor t)
      (with-current-buffer buffer
        (save-excursion
          (let* ((export-global-plist
                   (jf/org-keywords-as-plist))
                  (section
                    (jf/export-org-to-tor--global-buffer-prop-ensure
                      :key "HUGO_SECTION"
                      :plist export-global-plist
                      :default (format-time-string "posts/%Y")))
                  (base_dir
                    (jf/export-org-to-tor--global-buffer-prop-ensure
                      :key "HUGO_BASE_DIR"
                      :plist export-global-plist
                      :default "~/git/takeonrules.source"))
                  (format
                    (jf/export-org-to-tor--global-buffer-prop-ensure
                      :key "HUGO_FRONT_MATTER_FORMAT"
                      :plist export-global-plist
                      :default "yaml"))
                  (title
                    (lax-plist-get export-global-plist "TITLE"))
                  (identifier
                    (lax-plist-get export-global-plist "IDENTIFIER")))
            (save-buffer)
            (jf/export-org-to-tor--inject-additional-front-matter
              :title title
              :identifier identifier)
            ;; Write metadata
            (save-buffer)
            (unless org-transclusion-mode (org-transclusion-mode))
            (org-open-file (org-hugo-export-wim-to-md nil nil t)))))
      (setq jf/exporting-org-to-tor nil))

    (cl-defun jf/export-org-to-tor--inject-additional-front-matter (&key identifier title)
      "Export additional front matter.

    We want to ensure that we export the IDENTIFIER and TITLE.
    And add relevant metadata."
      (goto-char (point-min))
      (search-forward-regexp "#\\+HUGO_FRONT_MATTER_FORMAT: yaml")
      (insert (concat
                "\n#+HUGO_SLUG: " (denote-sluggify-title title)
                ;; 2022-02-26 07:46:15.000000000 -04:00
                "\n#+HUGO_PUBLISHDATE: "
                (format-time-string "%Y-%m-%d %H:%M:%S %z")
                "\n#+HUGO_TYPE: post"
                "\n#+HUGO_LAYOUT: post"
                "\n#+HUGO_DRAFT: true"
                "\n#+HUGO_CUSTOM_FRONT_MATTER: :licenses '(by-nc-nd-4_0)"
                "\n#+HUGO_CUSTOM_FRONT_MATTER: :org_id " identifier))
      (when-let ((kw-plist (jf/org-keywords-as-plist
                             :keywords-regexp "\\(SESSION_REPORT_DATE\\|SESSION_REPORT_LOCATION\\|SESSION_REPORT_GAME\\)")))
        (insert
          (format
            (concat "\n#+HUGO_CUSTOM_FRONT_MATTER: :sessionReport "
              "'((date . \"%s\") (game . \"%s\") (location . \"%s\"))")
            (plist-get kw-plist "SESSION_REPORT_DATE")
            (plist-get kw-plist "SESSION_REPORT_GAME")
            (plist-get kw-plist "SESSION_REPORT_LOCATION")))))

    (cl-defun jf/export-org-to-tor--global-buffer-prop-ensure (&key key plist (default nil))
      "Ensure current buffer has given KEY in global PLIST.

If not set  DEFAULT or prompt for it."
      (let ((value (plist-get plist key #'string=)))
        (if value value
          (jf/export-org-to-tor--global-buffer-prop-set
            :key key
            :value (or default
                     (read-from-minibuffer (format "%s: " key)))))))

    (cl-defun jf/export-org-to-tor--global-buffer-prop-set (&key key value)
      "Set global property named KEY to VALUE for current buffer."
      (goto-char (point-min))
      (forward-line 4)
      (insert (format "\n#+%s: %s" (upcase key) value)))

    (defvar jf/tor-session-report-location
      '("around the table"
         "via Zoom"
         "via Discord and Roll20"
         "via Discord"
         "in my living room")
      "TakeOnRules session report locations.")

    (cl-defun jf/org-keywords-as-plist (&key (keywords-regexp "\\(IDENTIFIER\\|FILETAGS\\|HUGO_FRONT_MATTER_FORMAT\\|HUGO_SECTION\\|HUGO_BASE_DIR\\|TITLE\\|SUBTITLE\\)"))
      (flatten-list (mapcar (lambda (prop)
                              (list (org-element-property :key prop)
                                (org-element-property :value prop)))
                      (jf/org-global-props keywords-regexp))))

    (defun jf/org-global-props (&optional property)
      "Get the plist of global org PROPERTY of current buffer."
      (unless property (setq property "PROPERTY"))
      (org-element-map
        (org-element-parse-buffer)
        'keyword
        (lambda (el)
          (when (string-match property
                  (org-element-property :key el))
            el))))

    (cl-defun jf/blog-post/tootify ()
      "Create a toot from the current buffer."
      (interactive)
      (if (jf/blog-entry?)
        (let* ((metadata
                 (jf/org-keywords-as-plist
                   :keywords-regexp "\\(ROAM_REFS\\|DESCRIPTION\\)"))
                (url
                  (lax-plist-get metadata "ROAM_REFS"))
                (description
                  (lax-plist-get metadata "DESCRIPTION")))
          (call-interactively #'mastodon-toot)
          (end-of-buffer)
          (insert (s-join "\n\n"
                    (flatten-list (list description url)))))
        (user-error "Current buffer is not a blog post")))

    (cl-defun jf/jump_to_corresponding_hugo_file (&key (buffer (current-buffer)))
      "Find the TakeOnRules.com url in BUFFER and jump to Hugo file."
      (interactive)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (save-match-data
            (if (re-search-forward
                  (concat "^\\#\\+ROAM_REFS:.+"
                    "\\(https?://takeonrules\.com[^ \n]*\\)" nil t))\]=

              (jf/tor-find-hugo-file-by-url (match-string 1))
              (message "Unable to find Take on Rules URL in buffer."))))))

    (cl-defun jf/jump_to_corresponding_denote_file (&key (buffer (current-buffer)))
      "Find org_id in BUFFER and jump to corresponding `denote' file."
      (interactive)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (save-match-data
            (if (re-search-forward
                  "^org_id: \\([[:digit:]]+T[[:digit:]]+\\)$" nil t)
              (find-file (denote-get-path-by-id (match-string 1)))
              (message "Unable to find Denote ID in buffer."))))))

    (defun jf/org-mode-get-keyword-key-value (kwd)
      "Map KWD to list."
      (let ((data
              (cadr kwd)))
        (list (plist-get data :key)
          (plist-get data :value))))

    (cl-defun jf/org-mode-extract-body-and-properties (node-id)
      "Extract quotable body and properties from NODE-ID."
      (with-current-buffer (find-file-noselect
                             (org-id-find-id-file node-id))
        (list :properties (org-element-map
                            (org-element-parse-buffer 'object)
                            '(keyword node-property)
                            #'jf/org-mode-get-keyword-key-value)
          :body (jf/org-mode-extract-body-from-current-buffer))))


    (defun jf/org-mode-extract-body-from-current-buffer ()
      "Extract the body from the current `org-mode' body."
      (buffer-substring (save-excursion
                          (jf/org-mode-find-point-that-starts-body t)
                          (point))
        (org-entry-end-position)))

    (defun jf/org-mode-find-point-that-starts-body (&optional unsafe)
      "Skip headline, planning line, and all drawers in current entry.

If UNSAFE is non-nil, assume point is on headline."
      (unless unsafe
        ;; To improve performance in loops (e.g. with `org-map-entries')
        (org-back-to-heading))
      (cl-loop for element = (org-element-at-point)
        for pos = (pcase element
                    (`(headline . ,_)
                      (org-element-property :contents-begin element))
                    (`(,(or 'planning 'property-drawer
                          'node-property 'keyword 'drawer)
                        . ,_)
                      (org-element-property :end element)))
        while pos
        do (goto-char pos)))

    (defun jf/tor-convert-text-to-post-title (title)
      "Convert TITLE to correct format."
      (message "Titleizing...")
      (replace-regexp-in-string
        ;; Replace "Hello World" with “Hello World”
        "\"\\([^\"]+\\)\""
        "“\\1”"
        (s-replace "'" "’" title)))

    (defun jf/tor-convert-text-to-slug (&optional string)
      "Convert STRING to appropriate slug."
      (s-replace "'" "" (s-dashed-words (s-downcase string))))

    (cl-defun jf/tor-toggle-hugo-server (&key
                                          (directory jf/tor-home-directory)
                                          (buffer-name "*Hugo Server*"))
      "This will start or stop a Hugo server in the given DIRECTORY.

  The BUFFER-NAME is where we'll run the Hugo process."
      (interactive)
      (if (get-buffer buffer-name)
        (progn
          (kill-buffer buffer-name)
          (message
            (concat "Stopping Hugo in \"" buffer-name "\" buffer…")))
        (let* ((default-directory
                 directory))
          (start-process
            "hugo-server" buffer-name "hugo" "server" "-D")
          (message
            (concat "Starting Hugo in \"" buffer-name "\" buffer…")))))

    (defvar jf/tor-hostname-regexp
      "^https?://takeonrules\.com"
      "A regular expression for checking if it's TakeOnRules.com.")

    (defvar jf/tor-hugo-regexp-for-post-path
      (concat jf/tor-hostname-regexp
        "/[0-9]\\{4\\}/[0-9]\\{2\\}/[0-9]\\{2\\}/\\([^/]+\\)/?$")
      "A regular expression for matching blog posts.")

    (defvar jf/tor-hugo-regexp-for-pages-path
      (concat jf/tor-hostname-regexp "/\\([^z-a]*[^/]\\)/?$")
      "A regular expression for matching pages.")

    (defun jf/tor-find-hugo-file-by-url (url)
      "Find the associated TakeOnRules.com file for the given URL."
      (interactive (list
                     (jf/prompt-for-url-dwim
                       :url-regexp jf/tor-hostname-regexp)))
      (cond
        ;; Blog post
        ((string-match jf/tor-hugo-regexp-for-post-path url)
          (let* ((slug
                   (match-string-no-properties 1 url))
                  (filename
                    (car
                      (jf/list-filenames-with-file-text
                        :matching (concat "^slug: " slug "$")
                        :in "content"))))
            (find-file
              (f-join jf/tor-home-directory "content" filename))))
        ;; Pages
        ((string-match jf/tor-hugo-regexp-for-pages-path url)
          (let* ((permalink
                   (match-string-no-properties 1 url))
                  (filename
                    (car
                      (jf/list-filenames-with-file-text
                        :matching (concat "^permalink: ['\\\"]?/?"
                                    permalink "/?['\\\"]?$")
                        :in "content"))))
            (find-file
              (f-join jf/tor-home-directory "content" filename))))
        ;; No match found
        (t (message "Unable to find post for \"%s\"" url))))

    (defun jf/tor-find-changelog-and-insert-entry ()
      "Find TakeOnRules glossary and begin entering a changelog entry."
      (interactive)
      (find-file (f-join jf/tor-home-directory "data" "changelog.yml"))
      ;; The changelog is structured in date descending order.  The
      ;; first line is the YAML preamble indicating a data object
      ;; (e.g. "---")
      (goto-char (point-min))
      (end-of-line)
      (insert (concat "\n- date: "
                (format-time-string "%Y-%m-%d")
                "\n  entries:\n    - ")))

    (defun jf/tor-find-series-and-insert-entry (title)
      "Find TakeOnRules series and add an entry with TITLE."
      (interactive "sSeries Entry's Title: ")
      (find-file (f-join jf/tor-home-directory "data" "series.yml"))
      (let ((key
              (downcase (s-dashed-words title))))
        (goto-char (point-max))
        (insert (concat
                  (if (looking-at-p "^$") "" "\n")
                  "- title: " title
                  "\n  key: " key))))

    ;; Note: I needed to use `fboundp' because if I invoked this
    ;; functions before other consult functions I got a method void
    ;; error.
    (cl-defun jf/find-file-via-matching (&key prompt matching in
                                          (switch "--files-with-matches"))
      "PROMPT for files IN directory with MATCHING content for SWITCH.

If `consult--read' is defined, use that.  Otherwise fallback to
`completing-read'."
      (if (fboundp 'consult--read)
        (consult--read
          (consult--with-increased-gc
            (jf/list-full-filenames-with-file-text
              :matching matching
              :in in
              :switch switch))
          :prompt prompt
          :sort nil
          :require-match t
          :category 'file
          :history 'file-name-history
          :state (consult--file-preview))
        (list (completing-read
                prompt
                (jf/list-filenames-with-file-text
                  :matching matching
                  :in in)))))


    (defun jf/tor-tags-list ()
      "Return a list of tags from TakeOnRules.com."
      (jf/tor-list-by-key-from-filename
        :key "tag" :filename "data/glossary.yml"))

    (defun jf/tor-epigraph-list ()
      "Return a list of epigraph keys from TakeOnRules.com."
      (jf/tor-list-by-key-from-filename
        :key "key" :filename "data/epigraphs.yml"))

    (defun jf/tor-game-list ()
      "Return a list of games from TakeOnRules.com."
      (jf/tor-list-by-key-from-filename
        :key "game" :filename "data/glossary.yml"))

    (defun jf/tor-glossary-title-list ()
      "Return a list of titles from TakeOnRules.com."
      (jf/tor-list-by-key-from-filename
        :key "title" :filename "data/glossary.yml"))

    (defun jf/tor-glossary-key-list ()
      "Return a list of keys from TakeOnRules.com glossary."
      (jf/tor-list-by-key-from-filename
        :key "key" :filename "data/glossary.yml"))

    (defun jf/tor-series-list ()
      "Return a list of series from TakeOnRules.com."
      (jf/tor-list-by-key-from-filename
        :key "key" :filename "data/series.yml"))

    (defun jf/tor-licenses-list ()
      "Return a list of available licenses for TakeOnRules.com."
      (jf/tor-list-by-key-from-filename
        :key "Key" :filename "data/licenses.yml"))

    (cl-defun jf/tor-list-by-key-from-filename (&key
                                                 key
                                                 filename
                                                 (directory jf/tor-home-directory))
      "Build list of entries of the KEY from the FILENAME in DIRECTORY."
      (split-string-and-unquote
        (shell-command-to-string
          (concat
            "rg \"^[- ] " key ": .*$\" "
            (f-join directory filename)
            " --only-matching --no-filename | sed 's/^[ -] " key
            ": //' | sort | tr '\n' '@'"))
        "@"))

    (cl-defun jf/list-filenames-with-file-text (&key matching in)
      "Build list of filenames MATCHING pattern IN the given directory."
      (let ((default-directory
              (f-join jf/tor-home-directory in)))
        (split-string-and-unquote
          (shell-command-to-string
            (concat
              "rg \""
              matching "\" --only-matching --files-with-matches"
              " --sortr modified | tr '\n' '@'"))
          "@")))

    (cl-defun jf/list-full-filenames-with-file-text (&key matching in
                                                      (switch "--files-with-matches"))
      "List filenames MATCHING with SWITCH pattern IN the directory."
      (split-string-and-unquote
        (shell-command-to-string
          (concat
            "rg \""
            matching "\" " in " --only-matching " switch
            " --sortr modified | tr '\n' '@'"))
        "@"))

    (defun jf/tor-page-relative-pathname-list ()
      "Return a list of pages for TakeOnRules.com."
      (jf/list-filenames-with-file-tex
        :matching "^title: " :in "content"))

    (defun jf/tor-asset-relative-pathname-list ()
      "Return a list of image filenames for TakeOnRules.com."
      (let ((default-directory
              (f-join jf/tor-home-directory "assets" "images")))
        (split-string-and-unquote
          (shell-command-to-string "ls"))))

    (defun jf/matches-in-buffer (regexp &optional buffer)
      "Return list of REGEXP matches in BUFFER or the current buffer."
      (let ((matches))
        (save-match-data
          (save-excursion
            (with-current-buffer (or buffer (current-buffer))
              (save-restriction
                (widen)
                (goto-char 1)
                (while (search-forward-regexp regexp nil t 1)
                  (push (match-string 0) matches)))))
          matches)))

    (defun jf/kill-new-markdown-heading-as-slug (heading)
      "Push onto the `kill-ring' a slugified version of HEADING."
      (interactive
        (list (completing-read
                "Heading: "
                (jf/matches-in-buffer "^#+ +.*$"))))
      (kill-new (jf/tor-convert-text-to-slug
                  (replace-regexp-in-string "^#+ +" "" heading))))

    (cl-defun jf/create-lore-24-blog-entry (&key
                                             (series "in-the-shadows-of-mont-brun")
                                             (keywords '("lore24" "rpgs")))
      "Create #Lore24 entry from current node.

Add the blog post to the given SERIES with the given KEYWORDS."
      (interactive)
      ;; Guard against running this on non- `jf/lore24-filename'.
      (unless (string=
                (jf/filename/tilde-based
                  (buffer-file-name (current-buffer)))
                jf/lore24-filename)
        (user-error "You must be in %S" jf/lore24-filename))
      ;; Now that we know we're on the right buffer...
      (let* (
              ;; Get the node of the current entry I'm working from
              (node-id (org-id-get-create))

              ;; Prompt for a name.
              (name (read-string "#Lore24 Blog Post Name: "))

              ;; Determine the last blog post created.
              (previous-post-basename
                (s-trim (shell-command-to-string
                          (concat
                            "cd ~/git/org/denote/blog-posts; "
                            "find *--lore24-entry-* | sort | "
                            "tail -1"))))

              ;; From the last blog post, derive the next index value.
              (next-index (format "%03d"
                            (+ 1
                              (string-to-number
                                (progn
                                  (string-match
                                    "--lore24-entry-\\([[:digit:]]+\\)-"
                                    previous-post-basename)
                                  (match-string-no-properties 1
                                    previous-post-basename))))))

              ;; We must name the post.  "Lore 24 - Entry NNN: Name"
              (title (format "Lore24 - Entry %s: %s" next-index name ))

              ;; The body of the blog post; by default I leverage
              ;; `org-transclusion'.
              (template
                (format
                  (concat "#+HUGO_CUSTOM_FRONT_MATTER: :series %s"
                    "\n\n#+TRANSCLUDE: [[id:%s]] :only-contents "
                    ":exclude-elements \"drawer keyword headline\"")
                  series
                  node-id))
              ;; This will be a blog post.
              (directory (f-join (denote-directory) "blog-posts"))

              ;; Series are added as signature.
              (signature (denote-sluggify-signature series)))

        ;; Create the blog post
        (denote title keywords 'org directory nil template signature)))

    (defun jf/path-to-table-number (table-number)
      (let* ((table-filename
               (f-join jf/tor-home-directory
                 "data/list_of_all_tables.yml"))
              (path (s-trim (shell-command-to-string
                              (concat "rg \"^- table_number:\\s"
                                table-number "$\" -A4 "
                                table-filename
                                " | rg \"^ +path: +\\\"([^\\\"]+)\\\"\""
                                " -r '$1' --only-matching")))))
        (s-trim (shell-command-to-string
                  (concat
                    "rg \"^#\\+ROAM_REFS: .*(https://takeonrules.com"
                    path ")\" -r '$1' --files-with-matches "
                    org-directory)))))

    (defun jf/list-blog-tables (&optional table-number)
      "Return list of blog tables with form \"TABLE-NUMBER: CAPTION\"."
      (let* ((table-filename
               (f-join jf/tor-home-directory
                 "data/list_of_all_tables.yml"))
              (match (or table-number "\\d+")))
        (s-split
          "\n"
          (s-trim
            (shell-command-to-string
              (concat "rg \"^- table_number: ("
                match ")\\n\\s+caption: (.*)\""
                " --multiline --replace '$1: $2' --only-matching "
                table-filename))))))

    (defun jf/org/link-table-complete ()
      "Complete a file from table."
      (completing-read "Table: " (jf/list-blog-tables) nil t))

    (defun jf/org/link-table-follow (link _)
      "Open a file from table for LINK."
      (find-file (jf/path-to-table-number link)))

    (defun jf/org/link-table-export (table-number desc format _)
      "Export TABLE-NUMBER with DESC for FORMAT."
      (let ((desc
              (concat "Table " table-number ": "
                (string-trim
                  (cadr
                    (s-split ":"
                      (car (jf/list-blog-tables table-number)))))))
             (link
               (format "https://takeonrules.com/tables/%s"
                 table-number)))
        (pcase format
          ((or 'html '11ty)
            (if jf/exporting-org-to-tor
              (format "{{< linkToTable %s >}}" table-number)
              (format "<a href=\"%s\">%s</a>" link desc)))
          ('md (if jf/exporting-org-to-tor
                 (format "{{< linkToTable %s >}}" table-number)
                 (format "[%s](%s)" desc link)))
          ('latex (format "\\href{%s}{%s}" link desc))
          ('texinfo (format "@uref{%s,%s}" link desc))
          ('ascii (format "%s (%s)" desc link))
          (_ (format "%s (%s)" desc link)))))

    (org-link-set-parameters
      "table"
      :complete #'jf/org/link-table-complete
      :export #'jf/org/link-table-export
      :follow #'jf/org/link-table-follow)))

(use-package qrencode
  ;; https://github.com/ruediger/qrencode-el/
  ;;
  ;; Generate an plain text QRCode (or PNG but really why not use those
  ;; UTF characters)
  :straight t)

(use-package pdf-tools
  ;; I appreciate having a good PDF reading experience within Emacs.
  :straight t
  :config
  ;; For some reason when I call `embark-dwim' on `pdf-loader-install'
  ;; It resets my theme and puts me at the beginning of this file.
  ;;
  ;; Also when I attempt to describ
  (pdf-loader-install)
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
  (dolist (path
            '("~/Library/CloudStorage/ProtonDrive-jeremy@jeremyfriesen.com/"
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
(use-package insert-random
  :straight t)

(use-package server
  :straight (:type built-in)
  :hook (server-visit . server-visit-hook-custom-find)
  :config
  (setq server-client-instructions nil)
  (unless (server-running-p)
    (server-start))

  ;; Connective Tissue and oddity functions:
  (defvar server-visit-files-custom-find:buffer-count
    nil
    "A counter for assisting with opening multiple files via a single
    client call.")

  (defadvice server-visit-files
    (around server-visit-files-custom-find
      activate compile)
    "Maintain a counter of visited files from a single client call."
    (let ((server-visit-files-custom-find:buffer-count
            0))
      ad-do-it))

  (defun server-visit-hook-custom-find ()
    "Arrange to visit the files from client call in separate windows."
    (if (zerop server-visit-files-custom-find:buffer-count)
      (progn
        (delete-other-windows)
        (switch-to-buffer (current-buffer)))
      (let ((buffer
              (current-buffer))
             (window
               (split-window-sensibly)))
        (switch-to-buffer buffer)
        (balance-windows)))
    (setq server-visit-files-custom-find:buffer-count
      (1+ server-visit-files-custom-find:buffer-count))))

(add-hook 'after-init-hook #'jf/enable-indent-for-tab-command)

(setq safe-local-variable-values
  '((eval
      (projectile-git-fd-args .
        "-H -0 -E hyrax-webapp -E .git -tf --strip-cwd-prefix -c never")
      (projectile-git-submodule-command . "")
      (jf/tor-minor-mode . 1)
      (projectile-require-project-root)
      (projectile-git-command .
        "git ls-files -zco --exclude-from=.projectile.gitignore")
      (org-insert-tilde-language . ruby)
      (org-insert-tilde-language . emacs-lisp)
      (encoding . utf-8))))

(use-package org
  ;; For projects and all
  :straight (:type built-in)
  :config
  (defun jf/org-mode/buffer-headline-tags ()
    "Return a list of `org-mode' tags excluding filetags.

  In the present implementation, I'm relying on `denote'
  conventions.  However, by creating a function I'm hiding the
  implementation details on how I get that."

    ;; This is here to indicate the dependency
    (require 'denote)
    (let* ((all-tags
             (org-get-buffer-tags))
            (file-level-tags
              (denote-extract-keywords-from-path (buffer-file-name))))
      ;; Given that I want inherited tags and the filetags are
      ;; considered to be on all headlines, I want to remove those tags.
      (cl-reduce (lambda (mem el)
                   (if (member
                         (substring-no-properties (car el))
                         file-level-tags)
                     mem
                     (add-to-list 'mem el)))
        all-tags :initial-value '())))

  (defun jf/org-mode/summarize-tags (&optional tags)
    "Create `org-mode' buffer that summarizes the headlines for TAGS.

This reducing function \"promotes\" the property drawer elements
to list elements while providing the same functionality of an
`org-mode' buffer.

Some of this could be accomplished with column/table declarations
but the headlines and content might not fit so well in the
buffer."
    (interactive (list
                   (completing-read-multiple
                     "Tags: "
                     (jf/org-mode/buffer-headline-tags) nil t)))

    (require 's)
    ;; With the given tags map the headlines and their properties.
    (let* ((prop-names-to-skip
             ;; This is a list of headline properties that I really
             ;; don't want to report.  I suspect some may be buffer
             ;; specific.  But for now, this should be adequate.
             ;;
             ;; Perhaps in later iterations we'll prompt for additional
             ;; ones to ignore.
             '("ID" "ALLTAGS" "FILE" "PRIORITY" "ITEM" "TIMESTAMP"
                "TIMESTAMP_IA" "CATEGORY" "TAGS"
                "BLOCKED" "TODO" "CLOSED"))
            (text-chunks
              ;; In using `org-map-entries' I can access inherited tags,
              ;; which I find structurally useful
              (org-map-entries
                ;; Yes this could be its own function but for now, we'll
                ;; leave it at that.
                (lambda ()
                  (let* ((h (org-element-at-point))
                          ;; Rebuild a terse header: depth, todo, title
                          ;; only
                          (header-text
                            (format
                              "%s%s %s\n"
                              (s-repeat
                                (org-element-property :level h) "*")
                              (if-let ((todo
                                         (org-element-property
                                           :todo-keyword h)))
                                (format " %s" todo) "")
                              ;; Turn the headline into a link to the
                              ;; document.
                              (format "%s [[file:%s::*%s][%s]]"
                                (org-element-property :title h)
                                (buffer-file-name)
                                ;; Strip out the progress cookie
                                (replace-regexp-in-string
                                  "\\[[^\]]*\\] +" ""
                                  (org-element-property :title h))
                                "link")))

                          ;; Only select relevant properties, converting
                          ;; those properties into a list of strings.
                          (properties-text
                            (seq-sort #'string<
                              (cl-reduce
                                (lambda (mem prop-value)
                                  (if (member (car prop-value)
                                        prop-names-to-skip)
                                    ;; For awhile I was forgetting to
                                    ;; always return the mem; which
                                    ;; would clobber my results.
                                    mem
                                    (add-to-list 'mem
                                      (format "- %s :: %s"
                                        (car prop-value)
                                        (cdr prop-value))
                                      t)))
                                (org-entry-properties h)
                                :initial-value nil))))

                    ;; If we have properties we want to render, we'll
                    ;; have one format.
                    (if properties-text
                      (format "%s\n%s\n" header-text
                        (s-join "\n" properties-text))
                      header-text)))

                ;; Select headlines matching any of the tags and that
                ;; are not "DONE".
                (format "+TODO<>\"DONE\"+%s"
                  (s-join "|" tags))
                'file 'comment))
            ;; Let's have only one of these
            (buffer-name "*Org Mode Tag Summary*")
            (display-buffer-mark-dedicated t))

      ;; When we've run this command again, so let's destroy what we had
      ;; and start anew.
      (when (get-buffer buffer-name) (kill-buffer buffer-name))
      (get-buffer-create buffer-name)
      (with-current-buffer buffer-name
        ;; Minimize the chatter of the mode-line
        (let ((mode-line
                (concat
                  (propertize (format " %s Tags: #%s"
                                ;; Show a lock icon
                                (char-to-string #xE0A2)
                                (s-join " #" tags))
                    'face 'mode-line-buffer-id)
                  "  "
                  (propertize
                    "C-c C-k to exit"
                    'face 'jf/mode-line-format/face-shadow))))
          ;; This came from `org-mode' so let's continue to keep it that
          ;; way.
          (org-mode)
          (insert (s-join "\n" text-chunks))
          (goto-char (point-min))
          ;; Let's not have the illusion that we're allowing ourselves
          ;; to edit this text
          (read-only-mode)
          (pop-to-buffer buffer-name
            `((display-buffer-in-side-window)
               (side . right)
               (window-width 72)
               (window-parameters
                 (tab-line-format . none)
                 (mode-line-format . ,mode-line)
                 (no-delete-other-windows . t))))))))

  (cl-defun jf/project/jump-to/notes (&key project)
    "Jump to the given PROJECT's notes file.

Determine the PROJECT by querying `jf/project/list-projects'."
    (interactive)
    (let* ((project
             (or (s-presence project)
               (jf/project/find-dwim)))
            (filename
              (cdar (jf/project/list-projects :project project))))
      (find-file filename)))

  ;; I work on several different projects each day; helping folks get
  ;; unstuck.  I also need to track and record my time.
  (bind-key "C-c C-j" 'jf/project/jump-to-task)
  (cl-defun jf/project/jump-to-task (&optional prefix)
    "Jump to task.

With one PREFIX go to place where we would jump on capture."
    (interactive "p")
    (require 'org-capture)
    (require 'pulsar)
    (cond
      ;; ((>= prefix 16)
      ;;   (if-let ((filename (f-join denote-journal-extras-directory "20240131T000000--time-reporting.org")))
      ;;     (progn
      ;;       (org-link-open-as-file (concat filename "::*Timeblock") nil)
      ;;       (org-next-visible-heading 1)
      ;;       (search-forward "#+BEGIN:")
      ;;       (org-dblock-update))
      ;;     (org-capture-goto-target "t")))
      ((>= prefix 4)
        (org-capture-goto-target "t"))
      (t (progn
           (call-interactively #'set-mark-command)
           (if (when (and (fboundp 'org-clocking-p) (org-clocking-p)) t)
             (progn
               (org-clock-goto)
               (goto-char (org-element-property
                            :contents-begin (org-element-at-point))))
             ;; Jump to where we would put a project were we to capture
             ;; it.
             (org-capture-goto-target "t")))))
    (pulsar-pulse-line))

  (bind-key "s-2" 'jf/project/jump-to/project-work-space)
  (defun jf/project/jump-to/project-work-space (project)
    "Prompt for PROJECT then workspace and open that workspace."
    (interactive (list (jf/project/find-dwim)))
    (let*
      ;; Get the project's file name
      ((filename
         (cdar (jf/project/list-projects :project project)))
        (paths-cons-list
          (jf/project/project-paths-for filename))
        (path-name
          (completing-read (format "Links for %s: " project)
            paths-cons-list nil t))
        (path
          (alist-get path-name paths-cons-list nil nil #'string=)))
      (cond
        ((s-starts-with? "http" path)
          (eww-browse-with-external-browser path))
        ((f-dir-p path)
          (dired path))
        ((f-file-p path)
          (if (string= "pdf" (f-ext path))
            (shell-command (concat "open " path))
            (find-file path)))
        ;; Try the path as an org-link (e.g. path ==
        ;; "denote:20230328T093100")
        (t (when-let* ((type-target (s-split ":" path))
                        ;; There's a registered handler for the protocol
                        ;; (e.g. "denote")
                        (follow-func (org-link-get-parameter
                                       (car type-target) :follow)))
             (funcall follow-func (cadr type-target))
             ;; We tried...and don't know how to handle this.
             (progn
               (message
                 (concat "WARNING: Project %s missing path name "
                   "\"%s\" (with path %s)")
                 project path-name path)
               (jf/project/jump-to/notes :project project)))))))

  (defun jf/project/project-paths-for (filename)
    "Find the project paths for the given FILENAME.

Added in cases where we want to inject the actual file."
    (with-current-buffer (find-file-noselect filename)
      (let ((paths
              (cl-maplist #'read
                (cdar (org-collect-keywords '("PROJECT_PATHS"))))))
        (setq paths (cons (cons "Notes" filename) paths)))))

  (cl-defun jf/project/list-projects (&key (project ".+")
                                       (directory org-directory))
    "Return a list of `cons' that match the given PROJECT.

The `car' of the `cons' is the project (e.g. \"Take on Rules\").
The `cdr' is the fully qualified path to that projects notes file.

The DIRECTORY defaults to `org-directory' but you can specify
otherwise."
    (mapcar (lambda (line)
              (let* ((slugs
                       (s-split ":" line))
                      (proj
                        (s-trim (car (cdr slugs))))
                      (filename
                        (s-trim (car slugs))))
                (cons proj filename)))
      (split-string-and-unquote
        (shell-command-to-string
          (concat
            "rg \"^#\\+PROJECT_NAME: +(" project ") *$\" " directory
            " --follow --only-matching --no-ignore-vcs --with-filename "
            "-r '$1' | tr '\n' '@'"))
        "@")))

  (cl-defun jf/project/get-project/project-source-code (&key (directory org-directory))
    "Return the current \"noted\" project name.

Return nil if the current buffer is not part of a noted project.

Noted projects would be found within the given DIRECTORY."
    (when-let ((project_path_to_code_truename (cdr (project-current))))
      (let ((project_path_to_code
              (jf/filename/tilde-based
                project_path_to_code_truename)))
        ;; How to handle multiple projects?  Prompt to pick one
        (let ((filename
                (s-trim (shell-command-to-string
                          (concat
                            "rg \"^#\\+PROJECT_PATHS: .*"
                            project_path_to_code " *\\\"\" "
                            directory " --files-with-matches "
                            " --no-ignore-vcs --ignore-case")))))
          (unless (string-equal "" filename)
            (with-current-buffer (find-file-noselect
                                   (file-truename filename))
              (jf/project/get-project/current-buffer)))))))

  (defun jf/project/get-project/current-clock ()
    "Return the current clocked project's name or nil."
    ;; This is a naive implementation that assumes a :task: has the
    ;; clock.  A :task:'s immediate ancestor is a :projects:.
    (when-let ((m (and
                    ;; If this isn't set, we ain't clocking.
                    (fboundp 'org-clocking-p)
                    (org-clocking-p)
                    org-clock-marker)))
      (with-current-buffer (marker-buffer m)
        (goto-char m)
        (jf/project/get-project/current-buffer))))

  (defun jf/project/get-project/current-buffer ()
    "Return the PROJECT_NAME keyword of current buffer."
    (cadar (org-collect-keywords (list "PROJECT_NAME"))))

  (defun jf/project/find-dwim ()
    "Find the current project based on context.

When the `current-prefix-arg' is set always prompt for the project."
    ;; `jf/project/get-project/current-agenda-buffer'
    (or
      (and (not current-prefix-arg)
        (or
          (jf/project/get-project/current-buffer)
          (jf/project/get-project/current-clock)
          (jf/project/get-project/project-source-code)))
      (completing-read "Project: " (jf/project/list-projects)))))

(require 'org-charsheet)

(use-package transient
  ;; Declaration for personal menu
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
    (when (jf/blog-entry?)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^$")
        (insert "\n#+DESCRIPTION: " description))))

  (transient-define-suffix jf/org-mode/add-session-report (date game location)
    "Add metadata (DATE, GAME, and LOCATION) to current buffer."
    :description "Add Session…"
    (interactive (list
                   (org-read-date
                     nil nil nil "Session Date")
                   (completing-read
                     "Game: " (jf/tor-game-list))
                   (completing-read
                     "Location: " jf/tor-session-report-location)))
    (when (jf/blog-entry?)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^$")
        (insert "\n#+HUGO_CUSTOM_FRONT_MATTER: :sessionReport "
          "'((date . \"" date "\") (game . \"" game "\") "
          "(location . \"" location "\"))"))))

  ;; My agenda files are a bit dynamic.  At one point I was using daily
  ;; journals and kept the latest 14 in my agenda files.  I don't that
  ;; now but I do add "projects" (see `jf/project/add-project-path').
  (transient-define-suffix jf/org-mode/agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    :description "Update agenda files…"
    (interactive)
    (message "Updating `org-agenda-files'")
    (setq org-agenda-files (jf/org-mode/agenda-files)))
  (add-hook 'after-init-hook #'jf/org-mode/agenda-files-update)

  (transient-define-suffix jf/enable-indent-for-tab-command ()
    :description "Enable `indent-for-tab-command'"
    (interactive)
    (keymap-global-set "TAB" #'indent-for-tab-command))

  (transient-define-suffix jf/project/convert-document-to-project (&optional buffer)
    "Conditionally convert the current BUFFER to a project.

This encodes the logic for creating a project."
    :description "Convert to project…"
    (interactive)
    (let ((buffer
            (or buffer (current-buffer))))
      (with-current-buffer buffer
        (if-let* ((file
                    (buffer-file-name buffer))
                   (_proceed
                     (and
                       (denote-file-is-note-p file)
                       (derived-mode-p 'org-mode)
                       (not (jf/project/get-project/current-buffer))))
                   (existing-title
                     (org-get-title))
                   (file-type
                     (denote-filetype-heuristics file)))
          (let ((keywords
                  (denote-retrieve-keywords-value file file-type)))
            ;; The 5th line is after the `denote' file metadata
            (goto-line 5)
            (insert "\n#+PROJECT_NAME: " existing-title
              "\n#+CATEGORY: " existing-title)
            (setq keywords (cons "projects" keywords))
            (denote-rewrite-keywords file keywords file-type)
            (call-interactively
              #'denote-rename-file-using-front-matter))
          (user-error "Unable to convert buffer to project")))))

  (transient-define-suffix jf/project/add-project-path (label path)
    "Add a PROJECT_PATH `org-mode' keyword to buffer.

This encodes the logic for creating a project."
    :description "Add project path…"
    (interactive (list
                   (read-string "Label: ")
                   (read-string "Path: ")))
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search
              t))
        (if (or
              (re-search-forward "^#\\+PROJECT_PATHS:" nil t)
              (re-search-forward "^#\\+PROJECT_NAME:" nil t))
          (end-of-line)
          (progn (goto-line 6) (re-search-forward "^$" nil t)))
        (insert
          "\n#+PROJECT_PATHS: (\""
          (s-trim label) "\" . \""
          (s-trim path)
          "\")"))))

  (transient-define-prefix jf/menu ()
    "A context specific \"mega\" menu."
    ;; Todo, can I get this section into a function so I can duplicate
    ;; it in the jf/menu--tor?
    [["Jump to"
       ("j a" jf/jump-to/agenda-local)
       ;; ("j c" "Capture Backlog" jf/jump-to/code-backlog)
       ("j d" "Denote File" jf/jump_to_corresponding_denote_file
         :if-derived markdown-mode)
       ("j g" "Global Mark" consult-global-mark)
       ("j h" "Hugo File" jf/jump_to_corresponding_hugo_file
         :if-derived org-mode)
       ("j m" "Mark" consult-mark)
       ;; ("j r" "Jump to Git Related" consult-git-related-find-file)
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
        ("B d" jf/org-mode/add-description
          :if jf/blog-entry?)
        ("B r" jf/org-mode/add-session-report
          :if jf/blog-entry?)
        ("B s" "Add Series…" jf/org-mode/add-series-to-file
          :if jf/blog-entry?)
        ("B x" "Export to TakeOnRules…" jf/export-org-to-tor
          :if jf/blog-entry?)]]
    [["Modes"
       ;; I could write functions for these, but this is concise enough
       ("m t" "Typopunct ( )" typopunct-mode
         :if-nil typopunct-mode)
       ("m t" "Typopunct (*)" typopunct-mode
         :if-non-nil typopunct-mode)
       ("m o" "MacOS Native Option ( )" jf/toggle-osx-alternate-modifier
         :if-non-nil ns-alternate-modifier)
       ("m o" "MacOS Native Option (*)" jf/toggle-osx-alternate-modifier
         :if-nil ns-alternate-modifier)
       ("m i" jf/shr/toggle-images)
       ;; I find that in all of my shuffling that sometimes the TAB for
       ;; command gets lost.  This is my "Yup that happens and here's
       ;; how to make that unhappen."
       ("TAB" jf/enable-indent-for-tab-command)
       ]
      ["Grab Refs"
        ("g e" "Elfeed" jf/capture/denote/from/elfeed-show-entry
          :if-derived elfeed-show-mode)
        ("g f" "Firefox" jf/menu--org-capture-firefox)
        ("g s" "Safari" jf/menu--org-capture-safari)
        ("g w" "Eww" jf/capture/denote/from/eww-data
          :if-derived eww-mode)
        ]
      ["Bookmark"
        ("b b" "Bookmarks" bookmark-bmenu-list)
        ("b s" "Safari" jf/menu--bookmark-safari)]])
  :bind ("s-1" . #'jf/menu))


;; (use-package detached
;;   ;; Run detached shell commands that can keep running even after
;;   ;; I quit emacs.
;;   ;;
;;   ;; We will need the 'dtach' command installed for the detached
;;   ;; package to work.
;;   ;;
;;   ;; '$ brew install dtach'
;;   :straight t
;;   (unless (executable-find "dtach")
;;     (async-shell-command "brew install dtach")))

;; (let ((ip4g-dir
;;        (expand-file-name "~/git/morpho-utils/emacs")))
;;   (when (file-directory-p ip4g-dir)
;;     (unless (executable-find "openstack")
;;       (async-shell-command "brew install openstackclient"))
;;     (unless (executable-find "autossh")
;;       (async-shell-command "brew install autossh"))))

(provide 'init)
;;; init.el ends here
