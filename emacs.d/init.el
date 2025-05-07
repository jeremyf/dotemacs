;;; init.el --- Emacs configuration for Jeremy Friesen -*- lexical-binding: t; -*-
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
(setopt custom-file (make-temp-file "emacs-custom-"))
(load custom-file :noerror)

;; https://www.reddit.com/r/emacs/comments/mtb05k/emacs_init_time_decreased_65_after_i_realized_the/
(setopt straight-check-for-modifications
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

(setopt straight-repository-branch "develop")
(straight-use-package 'use-package)
(setopt use-package-always-ensure t)

;; See https://github.com/radian-software/straight.el/issues/1146
(use-package straight
  :straight t
  :custom
  ;; add project and flymake to the pseudo-packages variable so straight.el doesn't download a separate version than what eglot downloads.
  (straight-built-in-pseudo-packages '(emacs nadvice python image-mode project flymake))
  (straight-use-package-by-default t))

;; When you open Emacs, grab all the space on the screen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(use-package gcmh
  ;; *Gcmh* does garbage collection (GC) when the user is idle.
  :straight t
  :custom
  (gcmh-idle-delay 5
    gcmh-low-cons-threshold (* 1024 1024)
    gcmh-high-cons-threshold (* 16 1024 1024))
  :config (gcmh-mode)
  (add-function :after after-focus-change-function
    (defun jf/garbage-collect-maybe ()
      (unless (frame-focus-state)
        (garbage-collect)))))


;; From https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
(defun  prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
    ((region-active-p)
      (keyboard-quit))
    ((derived-mode-p  'completion-list-mode)
      (delete-completion-window))
    ((>  ( minibuffer-depth)  0)
      (abort-recursive-edit))
    (t
      (keyboard-quit))))

(define-key global-map (kbd "C-g")  #'prot/keyboard-quit-dwim)

(use-package emacs
  ;; Setting baseline behavior for Emacs.
  :straight (:type built-in)
  :init
  ;; And I’m going to disable a few key bindings.  These were always
  ;; messing me up a bit.  Also enable a few that I find helpful.  (I’ll
  ;; enable a lot more later).
  (unbind-key "C-z") ;; `suspend-frame'
  (unbind-key "M-z") ;; `zap-to-char'
  (unbind-key "<f2>") ;; `suspend-frame'
  (unbind-key "s-o") ;; `ns-open-file-using-panel'
  (unbind-key "C-x C-c") ;; `save-buffers-kill-terminal'

  ;; Ensuring I have an autosave directory.  On a few rare occassions
  ;; this has saved me from lost "work".
  (make-directory "~/.emacs.d/autosaves/" t)
  :bind (("M-[" . #'backward-paragraph)
          ("s-[" . #'backward-paragraph)
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
          ("s-i" . #'ibuffer)
          ("M-RET" . #'newline-and-indent))
  :bind (:map emacs-lisp-mode-map
          ("H-e m" . 'symbol-overlay-rename)
          ("C-c C-c" . 'jf/eval-region-dwim))
  :config
  ;; Allow "y" or "n" to stand in for yes/no prompts
  (defalias 'yes-or-no-p 'y-or-n-p)

  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  ;; With 80 columns, I can always have two windows side-by-side.
  (setopt fill-column 80)

  ;; Doing a bit of configuration of my cursors.  The blinking provides
  ;; the queue and on 2024-05-18, I thought I'd give a try with the
  ;; hollow box.  Why?  It feels more retro.
  (setopt cursor-type 'bar)
  (blink-cursor-mode t)
  (setopt scroll-bar-mode 'right)
  (tool-bar-mode -1)
  ;; (setopt
  ;;   scroll-bar-mode 'right)
  (setopt
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
  (setopt user-full-name "Jeremy Friesen"
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

(when (eq system-type 'gnu/linux)
  (use-package grab-x-link
    :straight (:host github :repo "jeremyf/dotemacs" :files ("emacs.d/grab-x-link")))

  (cl-defmacro jf/linux:gsettings-toggler (name &key property match on_match on_miss)
    "Toggle the NAME setting via the PROPERTY.

When the PROPERTY is a MATCH set the property to ON_MATCH; otherwise set
it to the ON_MISS value."
    (let ((docstring
            (concat "Toggle " name " for Gnome desktop."))
           (func-name
             (intern (concat "jf/linux:toggle-" name))))
      `(defun ,func-name ()
         ,docstring
         (interactive)
         (let ((value
                 (if (string= ,match
                       (s-trim
                         (shell-command-to-string
                           (concat "gsettings get " ,property))))
                   ,on_match ,on_miss)))
           (shell-command
             (concat "gsettings set " ,property " " value))
           (message "%s: %s" ,name value)))))

  (jf/linux:gsettings-toggler "Trackpad"
    :property "org.gnome.desktop.peripherals.touchpad send-events"
    :match "'enabled'"
    :on_match "disabled"
    :on_miss "enabled")

  (jf/linux:gsettings-toggler "Night Light"
    :property "org.gnome.settings-daemon.plugins.color night-light-enabled"
    :match "true"
    :on_match "false"
    :on_miss "true")

  (defun jf/linux:radio-silence ()
    "Soft block laptop radios (e.g. bluetooth and wlan).

Related to `jf/linux:radio-broadcast'."
    (interactive)
    (shell-command "rfkill block all"))

  (defun jf/linux:radio-broadcast (&optional all identifiers)
    "Soft unblock laptop radios (e.g. bluetooth and wlan)

When ALL is non-nil unblock all radios.  Other unblock only the wlan.

Related to `jf/linux:radio-silence'."
    (interactive "P")
    (let ((identifiers
            (mapconcat
              (lambda (el) el)
              (or identifiers '("wlan"))
              " ")))
      (shell-command
        (concat "rfkill unblock "
          (if all "all" identifiers)))))

  ;; Favoring this over `cua-mode'
  (bind-key "s-c" 'copy-region-as-kill)
  (bind-key "s-a" 'mark-whole-buffer)
  (bind-key "s-s" 'save-buffer))

(when (eq system-type 'darwin)
  (progn
    (load "~/git/dotemacs/emacs.d/grab-mac-link-revised.el")
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
      (if (eq ns-right-alternate-modifier 'meta)
        (progn (setopt ns-right-alternate-modifier nil)
          (message "Enabling OS X native Option modifier"))
        (progn (setopt ns-right-alternate-modifier 'meta)
          (message "Disabling OX X native Option modifier")))
      (force-mode-line-update))

    ;; Exposing one additional modifier key.  This opens up a significant
    ;; set of keys and no one (by default) binds to 'H-' while many bind
    ;; to 'C-c' or other 'C-' keys, leaving conflicts.
    (setopt
      ns-function-modifier 'alt
      ns-right-command-modifier 'hyper
      ns-right-alternate-modifier 'meta)))

(defmacro jf/grab-browser-links (browser)
  (let ((bmk-fn
          (intern (format "jf/menu--bookmark-%s" browser)))
         (bmk-doc
           (format "Create `bookmark' for current %s page." browser))
         (ref-fn
           (intern (format "jf/menu--org-capture-%s" browser)))
         (ref-doc
           (format "Create an `denote' entry from %s page." browser))
         (grabber-fn
           (intern (format "grab-mac-link-%s-1" browser))))
    `(progn
       (defun ,bmk-fn ()
         ,bmk-doc
         (interactive)
         (if (eq system-type 'darwin)
           (progn
             (require 'grab-mac-link)
             (let* ((url-and-title
                      (,grabber-fn))
                     (title
                       (read-string
                         (concat "URL: " (car url-and-title) "\nTitle: ")
                         (cadr url-and-title))))
               (jf/bookmark-url (car url-and-title) title)))
           (user-error "My emacs for %s OS cannot yet bookmark %s page"
             system-type
             ,browser)))
       (defun ,ref-fn ()
         ,ref-doc
         (interactive)
         (if (eq system-type 'darwin)
           (progn
             (require 'grab-mac-link)
             (jf/denote/capture-reference :url (car (,grabber-fn))))
           (user-error "My emacs for %s OS cannot yet bookmark %s page"
             system-type
             ,browser))))))

(jf/grab-browser-links "safari")
(jf/grab-browser-links "firefox")
(jf/grab-browser-links "chrome")

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
  :straight t
  :after (org)
  :commands (denote-directory
              denote-file-prompt
              denote--title-prompt
              denote-get-path-by-id)
  :bind ("H-i" . 'denote-link-or-create)
  :hook (dired-mode . denote-dired-mode)
  (org-mode . denote-rename-buffer-mode)
  :init
  (setopt denote-known-keywords
    (split-string-and-unquote
      (shell-command-to-string
        (concat
          "rg \"#\\+TAG:\\s([\\w-]+)\" "
          jf/denote-base-dir
          " --only-matching"
          " --no-filename "
          " --follow "
          " --replace '$1' | "
          "ruby -ne 'puts $_.gsub(/^(\\w)\\w+-/) { |m| "
          "  m[0].upcase + m[1..-1] "
          "}.gsub(/-(\\w)/) { |m| m[1].upcase }'"))
      "\n"))
  :preface
  (defconst jf/denote/keywords/blogPosts
    "blogPosts"
    "The keyword used to indicate the note is slated to be a blogPost.")
  (defconst jf/denote/keywords/games
    "games"
    "The keyword used to indicate the note is a game.")
  (defconst jf/denote/keywords/abbr
    "abbr"
    "The keyword used to indicate the note is for an abbreviation.")
  (defconst jf/denote/keywords/glossary
    "glossary"
    "The keyword used to indicate the note is part of my glossary.")
  (defun jf/blog-entry? (&optional buffer)
    "Return non-nil when BUFFER is a blog post."
    (when-let* ((buffer (or buffer (current-buffer)))
                 (file (buffer-file-name buffer)))
      (and (denote-file-is-note-p file)
        (string-match-p
          (format "_%s" jf/denote/keywords/blogPosts)
          file))))
  :config
  (use-package denote-org
    :straight (:host github :type git :repo "protesilaos/denote-org")
    :after (denote org))
  ;; (setq denote-journal-extras-title-format 'day-date-month-year)
  (setopt denote-link-description-format "%t")
  (setopt denote-org-store-link-to-heading 'context)
  (setopt denote-rename-buffer-format "⟄ %D%b")
  (setopt denote-rename-buffer-backlinks-indicator " ↜")
  (setopt denote-infer-keywords nil)
  (setopt denote-org-capture-specifiers
    "%(jf/denote/capture-wrap :link \"%L\" :content \"%i\")")
  (setopt denote-directory jf/denote-base-dir)
  ;; These are the minimum viable prompts for notes
  (setopt denote-prompts '(title keywords))
  ;; I love ‘org-mode format; reading ahead I'm setting this
  (setopt denote-file-type 'org)
  ;; And `org-read-date' is an amazing bit of tech
  (setopt denote-file-name-slug-functions
    '((title . jf/denote-sluggify-title)
       (signature . jf/denote-sluggify-signature)
       (keyword . jf/denote-sluggify-keyword)))

  ;; (setq denote-file-name-letter-casing '((title . downcase)
  ;;                                         (signature . downcase)
  ;;                                         (keywords . verbatim)
  ;;                                         (t . downcase)))
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
       ("«" . "")
       ("»" . "")
       ("…" . "")  ;; Ellipsis
       ("—" . "-") ;; em dash
       ("–" . "-") ;; en dash
       (":" . "-")
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

  (advice-add #'denote-sluggify
    :filter-args #'jf/denote-sluggify)

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
    ;;     :property "ABBR"))
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

  (cl-defun jf/org-link-complete-link-for (parg &key
                                            scheme filter)
    "Prompt for `denote' with filename FILTER.

    Returns a string of format: \"SCHEME:<id>\" where <id> is
    an `denote' identifier.

PARG is part of the method signature for `org-link-parameters'."
    (let* ((file
             (denote-file-prompt (concat ".*" filter ".*"))))
      ;; This leverages a post v1.0.0 parameter of Denote
      ;; See https://git.sr.ht/~protesilaos/denote/commit/c6c3fc95c66ba093a266c775f411c0c8615c14c7
      (concat scheme ":" (denote-retrieve-filename-identifier file))))

  (cl-defun jf/denote/link-ol-abbr-with-property (link
                                                   description
                                                   format
                                                   info
                                                   &key
                                                   keyword
                                                   additional-hugo-parameters)
    "Export a LINK with DESCRIPTION for the given FORMAT.

    FORMAT is an Org export backend.  We will discard the given
    DESCRIPTION.  We use the INFO to track which abbreviations we've
    encountered; later using `jf/ox/filter-body/latex' to add an
    abbreviations section."
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
      ;; When we encounter an abbreviation, add that to the list.  We'll
      ;; later use that list to build a localized abbreviation element.
      (let ((abbr-links
              (plist-get info :abbr-links)))
        (unless (alist-get keyword-value abbr-links nil nil #'string=)
          (progn
            (add-to-list 'abbr-links (cons keyword-value title))
            (plist-put info :abbr-links abbr-links))))
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

  (defun jf/org/capture/finalize-work ()
    "Finalize works after capture."
    (jf/bibliography/export-shopping-list)
    (save-restriction
      (widen)
      (save-excursion
        (call-interactively #'org-up-heading nil)
        (jf/org-sort-entries/ignoring-stop-words))))

  (defun jf/bibliography/export-shopping-list (&optional file)
    "Export my book shopping list to the given FILE."
    (interactive)
    (let* ((works
             (save-restriction
               (widen)
               (save-excursion
                 (with-current-buffer
                   (find-file-noselect jf/filename/bibliography)
                   (org-map-entries
                     (lambda ()
                       (list
                         :title
                         (org-element-property
                           :title (org-element-at-point))
                         :author
                         (org-entry-get
                           (org-element-at-point) "AUTHOR")
                         :editor
                         (org-entry-get
                           (org-element-at-point) "EDITOR")
                         ))
                     "+LEVEL=2+books+shoppingList" 'file)))))
            (sorted-works
              (sort works
                :key (lambda (work)
                       (if-let ((author
                                  (plist-get work :author)))
                         (car (last
                                (s-split " "
                                  (car (s-split " and " author))) 1))
                         (if-let ((editor
                                    (plist-get work :editor)))
                           (car (last
                                  (s-split " "
                                    (car (s-split " and " editor))) 1))
                           ""))))))
      (let ((buffer
              (find-file-noselect (or file jf/filename/shopping-list))))
        (with-current-buffer buffer
          (delete-region (point-min) (point-max))
          (insert "Books from Jeremy's “shopping list” that he’s considering:\n\n")
          (dolist (work sorted-works)
            (insert (format "- “%s”%s%s\n"
                      (plist-get work :title)
                      (if-let ((author (plist-get work :author)))
                        (concat " by " author)
                        "")
                      (if-let ((editor (plist-get work :editor)))
                        (concat " edited by " editor)
                        ""))))
          (save-buffer)))))


  (defun jf/bibliography/export-epigraphs (&optional file)
    "Export epigraphs to my blog."
    (interactive)
    (let* ((epigraphs
             (save-restriction
               (widen)
               (save-excursion
                 (with-current-buffer
                   (find-file-noselect jf/filename/bibliography)
                   (elfeed--shuffle
                     (org-element-map
                       (org-element-parse-buffer)
                       '(quote-block verse-block)
                       (lambda (el)
                         ;; Skip un-named blocks as we can’t link to them.
                         (when-let* ((id
                                       (org-element-property :name el)))
                           (let* ((lineage
                                    (org-element-lineage el))
                                   (h-node
                                     (car
                                       (seq-filter
                                         (lambda (el)
                                           (and
                                             (eq (org-element-type el) 'headline)
                                             (= (org-element-property :level el) 2)))
                                         lineage)))
                                   (people?
                                     (member "people"
                                       (org-element-property :tags h-node))))
                             (list
                               :id id
                               :type (org-element-type el)
                               :work (if people?
                                       ""
                                       (car
                                         (org-element-property
                                           :title h-node)))
                               :author
                               (if people?
                                 (car
                                   (org-element-property :title h-node))
                                 (org-entry-get h-node "AUTHOR"))
                               :text
                               (buffer-substring-no-properties
                                 (org-element-property
                                   :contents-begin el)
                                 (org-element-property
                                   :contents-end el)))))))))))))
      (let* ((buffer
               (find-file-noselect
                 (or file jf/filename/epigraphy-takeonrules))))
        (with-current-buffer buffer
          (delete-region (point-min) (point-max))
          (insert
            "---\n"
            "date: 2021-07-22 19:23:43.883686000 -04:00 \n"
            "full_width: true\n"
            "images: []\n"
            "lastmod: " (format-time-string "%Y-%m-%d %H:%M:%S.%N %z") "\n"
            "layout: page\n"
            "permalink: \"/site-map/epigraphs/\"\n"
            "title: Epigraphs\n"
            "type: page\n"
            "---\n"
            "\n"
            "Ever since reading {{< glossary key=\"DUNE-NOVEL\" >}} by {{< glossary key=\"FRANK-HERBERT\" >}} I've loved epigraphs.  "
            "In that novel, the epigraphs are quotes from fictional works written within the Dune universe.  "
            "Below are quotes that I've gathered, and in some cases, I've used as epigraphs throughout <cite>Take on Rules</cite>.\n")
          (dolist (epigraph epigraphs)
            (let ((work
                    (plist-get epigraph :work))
                   (author
                     (plist-get epigraph :author))
                   (text
                     (plist-get epigraph :text)))
              (insert
                (format "<section class=\"epigraphs\"><blockquote data-id=\"%s\">%s%s\n</blockquote></section>\n"
                  (plist-get epigraph :id)
                  (if (eq (plist-get epigraph :type) 'verse-block)
                    (concat "<pre class=\"verse\">"  text "</pre>")
                    (org-export-string-as (s-trim text) 'html t))
                  (cond
                    ((and (s-present? work) (s-present? author))
                      (format "\n<footer>&#8213;%s, <cite>%s</cite></footer>"
                        author work))
                    ((s-present? work)
                      (format "\n<footer>&#8213; <cite>%s</cite></footer>"
                        work))
                    ((s-present? author)
                      (format "\n<footer>&#8213; %s</footer>"
                        author))
                    (t ""))))))
          (save-buffer))
        (message "Done exporting epigraphs to blog"))))

  (org-link-set-parameters "epigraph"
    :complete #'jf/org-link-ol-complete/epigraph
    :export #'jf/org-link-ol-export/epigraph
    :face #'jf/org-faces-epigraph
    :follow #'jf/org-link-ol-follow/epigraph)

  (defun jf/org-link-ol-export/epigraph (link description format protocol)
    "Export the text of the LINK epigraph in the corresponding FORMAT.

We ignore the DESCRIPTION and probably the PROTOCOL."
    (let ((buffer
            (find-file-noselect jf/filename/bibliography)))
      (save-restriction
        (widen)
        (save-excursion
          (with-current-buffer buffer
            (let* ((epigraph
                     (car
                       (org-element-map
                         (org-element-parse-buffer)
                         '(quote-block verse-block)
                         (lambda (el)
                           ;; Skip un-named blocks as we can’t link to
                           ;; them.
                           (when (string=
                                   (org-element-property :name el)
                                   link)
                             el)))))
                    (id
                      (org-element-property :name epigraph))
                    (class
                      (if (eq 'verse-block (org-element-type epigraph))
                        "verse"
                        "quote"))
                    (lineage
                      (org-element-lineage epigraph))
                    (context
                      (car
                        (seq-filter
                          (lambda (el)
                            (and
                              (eq (org-element-type el) 'headline)
                              (= (org-element-property :level el) 2)))
                          lineage)))
                    (people?
                      (member "people"
                        (org-element-property :tags context)))
                    (work
                      (if people?
                        ""
                        (car (org-element-property :title context))))
                    (author
                      (if people?
                        (car (org-element-property :title context))
                        (org-entry-get context "AUTHOR")))
                    (text
                      (buffer-substring-no-properties
                        (org-element-property
                          :contents-begin epigraph)
                        (org-element-property
                          :contents-end epigraph))))
              (cond
                ((or (eq format 'html) (eq format 'md))
                  (format "<blockquote class=\"%s epigraph\" data-id=\"%s\">\n%s%s</blockquote>\n"
                    class
                    id
                    (if (string= class "verse")
                      (s-replace "\n" "<br />\n" text)
                      (org-export-string-as text 'html t))
                    (cond
                      ((and (s-present? work) (s-present? author))
                        (format "\n<footer>&#8213;%s, <cite>%s</cite></footer>"
                          author work))
                      ((s-present? work)
                        (format "\n<footer>&#8213; <cite>%s</cite></footer>"
                          work))
                      ((s-present? author)
                        (format "\n<footer>&#8213; %s</footer>"
                          author))
                      (t ""))))
                ((eq format 'latex)
                  (format "\\begin{%s}\n%s%s\n\\end{%s}\n"
                    class
                    (if (string= "verse" class)
                      (s-replace "\n" "\\\\\n" text)
                      text)
                    (cond
                      ((and (s-present? work) (s-present? author))
                        (format "---%s, \\textit{%s}" author work))
                      ((s-present? work)
                        (format "---\\textit{%s}" work))
                      ((s-present? author)
                        (format "---%s" author))
                      (t ""))
                    class))
                (t
                  (let* ((use-hard-newlines t))
                    (s-replace
                      "\n" hard-newline
                      (format "%s%s"
                        text
                        (cond
                          ((and (s-present? work) (s-present? author))
                            (format "\n\n—%s, “%s”" author work))
                          ((s-present? work)
                            (format "\n\n—“%s”" work))
                          ((s-present? author)
                            (format "\n\n—%s" author))
                          (t "")))))))))))))

  (defun jf/org-link-ol-complete/epigraph ()
    "Find and insert an epigraph for export.

Wires into `org-insert-link'."
    (let* ((buffer
             (find-file-noselect jf/filename/bibliography))
            (candidates
              (save-restriction
                (widen)
                (save-excursion
                  (with-current-buffer buffer
                    (org-element-map
                      (org-element-parse-buffer)
                      '(quote-block verse-block)
                      (lambda (el)
                        ;; Skip un-named blocks as we can’t link to them.
                        (when-let* ((id
                                      (org-element-property :name el))
                                     (left
                                       (org-element-property
                                         :contents-begin el))
                                     (right
                                       (org-element-property
                                         :contents-end el))
                                     (text
                                       (s-trim
                                         (s-replace "\n" " "
                                           (buffer-substring-no-properties
                                             left
                                             (if (< (- right left) 72)
                                               right
                                               (+ left 72)))))))
                          (cons text id))))))))
            (candidate
              (completing-read "Epigraph: " candidates nil t))
            (id
              (alist-get candidate candidates nil nil #'string=)))
      (when id
        (progn
          (message "Added %S to the kill ring" candidate)
          (kill-new candidate)
          (format "epigraph:%s" id)))))


  (defun jf/org-link-ol-follow/epigraph (name)
    "Follow the NAME to the epigraph."
    (let* ((file
             jf/filename/bibliography)
            (case-fold-search
              t))
      (find-file file)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (search-forward-regexp (format "^#\\+name: +%s$" name)))
      (pulsar--pulse)))

  (org-link-set-parameters "work"
    ;; TODO: Allow link to specify to include author.
    :follow #'jf/org-link-ol-follow/work
    :complete #'jf/org-link-ol-complete/work
    :export #'jf/org-link-ol-export/work
    :face #'jf/org-faces-work)

  (defun jf/org-link-ol-follow/work (name)
    "Follow the NAME to the work."
    (let* ((file
             jf/filename/bibliography)
            (case-fold-search
              t))
      (find-file file)
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search
              t)
             (name
               (car (s-split "::" name))))
        (search-forward-regexp
          (format "^:custom_id:[[:space:]]+%s$" name))
        (call-interactively #'org-previous-visible-heading))
      (pulsar--pulse)))

  (defun jf/book-make-label (title subtitle author)
    "From the given TITLE, SUBTITLE and AUTHOR return it's formatted label."
    (format "«%s»%s"
                              (if (s-present? subtitle)
                                (concat title ": " subtitle)
                                title)
                              (if (s-present? author)
                                (concat " by " author) "")))

  (defun jf/org-link-ol-complete/work ()
    "Prompt for a work from my bibliography"
    (interactive)
    (let* ((buffer
             (find-file-noselect jf/filename/bibliography))
            (works
              (save-restriction
                (widen)
                (save-excursion
                  (with-current-buffer buffer
                    ;; With the given tag, find all associated headlines
                    ;; that match that tag.
                    (org-map-entries
                      (lambda ()
                        (let* ((headline
                                 (org-element-at-point))
                                (title
                                  (org-element-property :title headline))
                                (subtitle
                                  (org-entry-get headline "SUBTITLE"))
                                (author
                                  (org-entry-get headline "AUTHOR")))
                          (cons
                            (jf/book-make-label title subtitle author)
                            (list
                              :id (org-entry-get headline "CUSTOM_ID")
                              :title title
                              :subtitle subtitle
                              :author author))))
                      "+LEVEL=2+!people" 'file)))))
            (work
              (completing-read "Citable: " works nil t)))
      (when-let ((work-data
                   (alist-get work works nil nil #'string=)))
        (let* ((include-author
                 (and (plist-get work-data :author)
                   (yes-or-no-p "Include Author: ")))
                (include-subtitle
                  (and (plist-get work-data :subtitle)
                    (yes-or-no-p "Include Subtitle: ")))
                (desc
                  (jf/book-make-label
                    (plist-get work-data :title)
                    (when include-subtitle (plist-get work-data :subtitle))
                    (when include-author (plist-get work-data :author)))))
          (message "Added %S to the kill ring" desc)
          (kill-new desc)
          (format "work:%s%s%s"
            (plist-get work-data :id)
            (if include-author "::author" "")
            (if include-author "::subtitle" ""))))))

  (defun jf/org-link-ol-export/work (link description format protocol)
    "Export the text of the LINK work in the corresponding FORMAT.

We ignore the DESCRIPTION and probably the PROTOCOL."
    (let* ((buffer
            (find-file-noselect jf/filename/bibliography))
           (link-with-properties
             (s-split "::" link))
            (link
              (car link-with-properties))
            (with-author
              (member "author" link-with-properties))
            (with-subtitle
              (member "subtitle" link-with-properties)))
      (save-restriction
        (widen)
        (save-excursion
          (with-current-buffer buffer
            ;; First we find the corresponding work and possible URL.
            (when-let* ((work
                          (car
                            (org-element-map
                              (org-element-parse-buffer)
                              '(headline)
                              (lambda (el)
                                ;; Skip un-named blocks as we can’t link to
                                ;; them.
                                (when (string=
                                        (org-entry-get el "CUSTOM_ID")
                                        link)
                                  (let ((title
                                          (car
                                            (org-element-property :title el))))
                                    (list
                                      :title
                                      title
                                      :subtitle
                                      (if-let ((subtitle
                                                 (org-entry-get el "SUBTITLE")))
                                        (if with-subtitle
                                          title
                                          (format "%s: %s" title subtitle))
                                        title)
                                      :author
                                      (org-entry-get el "AUTHOR")
                                      :url
                                      (org-entry-get el "ROAM_REFS")))))))))
              (let ((author-suffix
                      (if (and
                            with-author
                            (s-present? (plist-get work :author)))
                        (format " by %s" (plist-get work :author))
                        "")))
                ;; Then we create the corresponding format.
                (cond
                  ((or (eq format 'html) (eq format 'md))
                    (format "<cite data-id=\"%s\">%s</cite>%s"
                      link
                      (if-let ((url
                                 (plist-get work :url)))
                        (format "<a href=\"%s\">%s</a>"
                          url (plist-get work :title))
                        (plist-get work :title))
                      author-suffix))
                  ((eq format 'latex)
                    (format "\\textit{%s}%s"
                      (if-let ((url
                                 (plist-get work :url)))
                        (format "\\href{%s}{%s}"
                          url (plist-get work :title))
                        (plist-get work :title))
                      author-suffix))
                  ((eq format 'odt)
                    (format
                      "<text:span text:style-name=\"%s\">%s</text:span>%s"
                      "Emphasis"
                      (if-let ((url
                                 (plist-get work :url)))
                        (format "<text:a xlink:type=\"simple\" xlink:href=\"%s\">%s</text:a>"
                          url (plist-get work :title))
                        (plist-get work :title))
                      author-suffix))
                  (t
                    (format "“%s”%s"
                      (plist-get work :title)
                      author-suffix))))))))))

  (org-link-set-parameters "elfeed"
    :follow #'elfeed-link-open
    :store #'elfeed-link-store-link
    :export #'elfeed-link-export-link)

  (defun elfeed-link-export-link (link desc format _protocol)
    "Export `org-mode' `elfeed' LINK with DESC for FORMAT."
    (if (string-match "\\([^#]+\\)#\\(.+\\)" link)
      (if-let* ((entry
                  (elfeed-db-get-entry
                    (cons (match-string 1 link)
                      (match-string 2 link))))
                 (url
                   (xml-escape-string (elfeed-entry-link entry)))
                 (title
                   (elfeed-entry-title entry)))
        (pcase format
          ('html (format "<a href=\"%s\">%s</a>" url desc))
          ('md (format "[%s](%s)" desc url))
          ('latex (format "\\href{%s}{%s}" url desc))
          ('odt
            (format "<text:a xlink:type=\"simple\" xlink:href=\"%s\">%s</text:a>"
              url desc))
          ('texinfo (format "@uref{%s,%s}" url desc))
          (_ (format "%s (%s)" desc url)))
        (format "%s (%s)" desc url))
      (format "%s (%s)" desc link)))

  (defface jf/org-faces-date '((default :inherit link))
    "Face used to style `org-mode' date links in the buffer."
    :group 'denote-faces
    :package-version '(denote . "0.5.0"))

  (defface jf/org-faces-epigraph '((default :inherit link))
    "Face used to style `org-mode' epigraph links in the buffer."
    :group 'denote-faces
    :package-version '(denote . "0.5.0"))

  (defface jf/org-faces-work '((default :inherit link))
    "Face used to style `org-mode' work links in the buffer."
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
              (when-let ((u (plist-get export-plist :url)))
                (xml-escape-string u)))
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
              (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" url)
              desc))
          ((eq format 'texinfo) (format "@uref{%s,%s}" url desc))
          ((eq format 'odt)
            (format "<text:a xlink:type=\"simple\" xlink:href=\"%s\">%s</text:a>"
              url
              desc))
          (t (format "[%s](%s)" desc url))
          ;; ((eq format 'odt) (org-odt-link url (format "%s" desc) (list)))
          ;; (t path)
          )
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
                (replace-regexp-in-region
                  (concat "\\[\\[\\([^:]+\\):\\([0-9A-Z]+\\)"
                    "\\]\\[\\([^]]+\\)\\]\\]")
                  (format "[[%s:%s][%s]]"
                    new-type denote-id new-text)
                  (org-element-property :begin element)
                  (org-element-property :end element))
                (user-error "Expected denote-id %s to have a %s acceptable property" denote-id new-type)))
            (user-error "Current element is of type %s; it must be one of the following: %s" type types)))
        (user-error "Current element must be of type 'link; it is %S" (car element)))))

  (defun jf/capture/denote/from/eww-data ()
    "Create an `denote' entry from `eww' data."
    (interactive)
    (jf/denote/capture-reference :url (plist-get eww-data :url)))

  (defun jf/capture/denote/from/elfeed-show-entry ()
    "Create `denote' entry from `elfeed-show-entry'."
    (interactive)
    (jf/denote/capture-reference
      :url (elfeed-entry-link elfeed-show-entry)))


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
            (denote-update-dired-buffers)))))))
(require 'denote)

(defvar jf/denote-base-dir
  (file-truename
    (if (file-exists-p (expand-file-name "~/.my-computer"))
      "~/git/org/denote/" "~/Documents/denote/"))
  "Where I put my notes; I need to provision differently for personal and
work computers.")

(use-package files
  :straight (:type built-in)
  :custom (make-backup-files nil))

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
  :custom (recentf-max-menu-items 50
    recentf-max-saved-items 256)
  ;; Track recent
  (recentf-mode 1)
  :config
  ;; Quietly save the recent file list every 5 minutes.
  (run-at-time (current-time) 300 (lambda ()
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
    (setopt grep-program "rg")))

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
  :custom
  (ediff-keep-variants nil)
  (ediff-make-buffers-readonly-at-startup nil)
  (ediff-merge-revisions-with-ancestor t)
  (ediff-show-clashes-only t)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package dired
  ;; Oh `dired', you are a super powered directory editor.  For years I
  ;; avoided interacting with you, but overtime I've practiced and find
  ;; your utility great.  Never conflate a file with a buffer and
  ;; instead consider the power of what a buffer can do.
  :straight (:type built-in)
  :custom (dired-listing-switches "-laGhpX")
  (dired-use-ls-dired t)
  ;; When two dired buffers are open and you mark then rename a file, it
  ;; assume's you're moving the file from the one buffer to the other.
  ;; Very useful.
  (dired-dwim-target t)
  (dired-vc-rename-file t)
  :config
  (with-eval-after-load 'dired
    ;; Bind dired-x-find-file.
    (require 'dired-x)
    (setopt dired-x-hands-off-my-keys nil))
  :hook (dired-mode . dired-hide-details-mode))

(use-package dired-filter
  :straight t)

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
  :custom (lin-face 'lin-blue))

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
  :custom
  (pulsar-face 'pulsar-magenta
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
(setopt ring-bell-function 'jf/pulse)

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
  :custom
  (enable-recursive-minibuffers t)
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
  :custom
  (keycast-mode-line-insert-after 'jf/mode-line-format/buffer-name-and-status
  keycast-mode-line-remove-tail-elements nil
  keycast-mode-line-window-predicate 'mode-line-window-selected-p
  keycast-mode-line-format "%2s%k%2s(%c%R)"))

(use-package emacs
  :straight (:type built-in)
  :after (projectile)
  :preface
  (defvar-local jf/mode-line-format/kbd-macro
    '(:eval
       (when (and (mode-line-window-selected-p) defining-kbd-macro)
         (concat
           (propertize " 𝕄 " 'face 'mode-line-highlight) " "))))

  (defvar jf/mode-line-format/major-mode-indicator-map
    (let ((map
            (make-sparse-keymap)))
      (define-key map [mode-line down-mouse-1] #'jf/toggle-osx-alternate-modifier)
      map)
    "Keymap to display when MacOS native keys are on.")

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
              ((derived-mode-p 'text-mode) "¶")
              ((derived-mode-p 'prog-mode) "λ")
              ((derived-mode-p 'comint-mode) ">_")
              (t "◦"))))
      (propertize indicator
        'face
        (if (mode-line-window-selected-p)
          (if (and (boundp 'ns-right-alternate-modifier)
                (eq ns-right-alternate-modifier nil))
            'jf/mode-line-format/face-shadow-highlight
            'jf/mode-line-format/face-shadow)
          'mode-line-inactive)
        'local-map jf/mode-line-format/major-mode-indicator-map
        'help-echo
        (concat "mouse-1:  #'jf/toggle-osx-alternate-modifier"))))

  (defvar jf/mode-line-format/major-mode-map
    (let ((map
            (make-sparse-keymap)))
      (define-key map [mode-line down-mouse-1]
        #'jf/toggle-typopunct-mode)
      map)
    "Keymap to display `typopunct-mode'.")

  (defun jf/mode-line-format/major-mode-name ()
    "Render the major mode as text.

When `typopunct-mode' is active, provide an actionable indicator of its
active nature."
    (let ((fmt
            (if typopunct-mode "“%s”" "%s")))
      (propertize
        (format fmt
          (capitalize
            (string-replace "-mode" "" (symbol-name major-mode))))
        'face
        (if (mode-line-window-selected-p)
          'mode-line 'mode-line-inactive)
        'local-map
        jf/mode-line-format/major-mode-map
        'help-echo
        (concat "mouse-1: #'jf/toggle-typopunct-mode"))))

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
         (concat
           (propertize " ⊆ " 'face 'mode-line-highlight) " "))))

  (defvar-local jf/mode-line-format/org-clock
    '(:eval
       (when
         (and
           (mode-line-window-selected-p)
           org-clock-current-task
           (not (derived-mode-p
                  'Info-mode
                  'help-mode
                  'special-mode
                  'message-mode)))
         (concat
           ;; The symbol that most looks like an analogue stop watch.
           (propertize " ⨶ " 'face 'mode-line-highlight) " "))))

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

  (defvar jf/mode-line-format/which-function-map
    (let ((map
            (make-sparse-keymap)))
      (define-key map [mode-line mouse-1]
        #'jf/yank-current-scoped-function-name)
      (define-key map [mode-line M-mouse-1]
        #'mark-defun)
      map))

  (defvar-local jf/mode-line-format/which-function
    '(:eval
       (when (and which-function-mode (mode-line-window-selected-p))
         (when-let ((func (which-function)))
           (propertize
             (concat " ⨍ := " func)
             'face
             'mode-line-emphasis
             'local-map
             jf/mode-line-format/which-function-map
             'help-echo
             (concat
               "mouse-1:    #'jf/yank-current-scoped-function-name\n"
               "M-mouse-1:  #'mark-defun (C-M-h)"))))))

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

  (defface jf/mode-line-format/face-shadow-highlight
    '((t :foreground "#d0ffe0" :inherit shadow))
    "A face for highlighting symbols in the `mode-line'.")

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

  ;; We need to acknowledge and accept that these are risky variables.
  ;; If we do not, then Emacs will not render the variable in the
  ;; modeline.
  (dolist (construct '(
                        jf/mode-line-format/buffer-name-and-status
                        jf/mode-line-format/eglot
                        jf/mode-line-format/flymake
                        jf/mode-line-format/kbd-macro
                        jf/mode-line-format/major-mode
                        jf/mode-line-format/misc-info
                        jf/mode-line-format/narrow
                        jf/mode-line-format/org-clock
                        jf/mode-line-format/project
                        jf/mode-line-format/vc-branch
                        jf/mode-line-format/vterm
                        jf/mode-line-format/which-function
                        ))
    (put construct 'risky-local-variable t))

  (setq-default mode-line-format
    '("%e" " "
       jf/mode-line-format/org-clock
       jf/mode-line-format/vterm
       jf/mode-line-format/kbd-macro
       jf/mode-line-format/narrow
       jf/mode-line-format/buffer-name-and-status " "
       jf/mode-line-format/major-mode " "
       jf/mode-line-format/project " "
       jf/mode-line-format/vc-branch " "
       jf/mode-line-format/flymake " "
       jf/mode-line-format/eglot
       jf/mode-line-format/which-function
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
  :custom
  (avy-dispatch-alist
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
  :bind ("s-i" . 'imenu-list-smart-toggle)
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
    (if prefix
      (link-hint-open-link)
      (let ((browse-url-browser-function
              #'eww-browse-url))
        (link-hint-open-link))))

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
              ;; (t (funcall func args))))
              (t (apply func (list url)))))

          ;; (funcall func args)))
          (apply func (list url))))
      (apply advised-function func args)))

  (advice-add 'link-hint--apply
    :around #'jf/link-hint--apply))

;; Allow for opening the URLs provided in eldoc.  This is rough and
;; minimal error handling.
(link-hint-define-type 'eldoc-url
  :next #'link-hint--next-eldoc-url
  :at-point-p #'link-hint--eldoc-url-at-point-p
  :open #'browse-url
  :copy #'kill-new)

(defun link-hint--next-eldoc-url (bound)
  "Get position of next `face' at or after BOUND."
  ;; While we're interested in the 'help-echo value, we need to see if
  ;; we can't solely work from that.  Instead we need to check if we
  ;; have a link face.
  (link-hint--next-property-with-value 'face 'markdown-link-face bound))

(defun link-hint--eldoc-url-at-point-p ()
  "Return the name of the eldoc link at the point or nil."
  ;; Mirroring `link-hint--next-eldoc-url' logic, when we have a link
  ;; face look to the help-echo for the URL.
  (when (eq (get-text-property (point) 'face)
          'markdown-link-face)
    (get-text-property (point) 'help-echo)))

(push 'link-hint-eldoc-url link-hint-types)

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
  (defvar variable-pitch-name
    (if (eq system-type 'darwin)
      "IntoneMono Nerd Font Propo"
      "IntoneMono Nerd Font Propo Light")
    "Provide an inflection point for OS fonts.  Namely they are named just a
bit differently.")
  (setopt fontaine-presets
    ;; I'm naming the presets as "actions"; the mindset that I'm using
    ;; when wanting that font.
    `((smallest
        :default-height 100)
       (smaller
         :default-height 110)
       (default
         :default-family "IntoneMono Nerd Font Mono"
         :default-weight light
         :bold-weight medium
         :default-height 125)
       (bigger
         :default-height 160)
       (coding
         :default-family "IntoneMono Nerd Font Mono"
         :default-weight light
         :bold-weight medium
         :default-height 125)
       (biggest
         :default-weight light
         :default-height 220
         :bold-weight medium)
       (reading
         :default-weight semilight
         :default-family "ETBembo"
         :default-height 220
         :bold-weight medium)
       (t
         :default-family "IntoneMono Nerd Font Mono"
         :default-weight light
         :default-height 1
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :fixed-pitch-serif-family nil ; falls back to :default-family
         :fixed-pitch-serif-weight nil ; falls back to :default-weight
         :fixed-pitch-serif-height 1.0
         :variable-pitch-family ,variable-pitch-name
         :variable-pitch-weight light
         :variable-pitch-height 1.0
         :bold-family nil ; use whatever the underlying face has
         :bold-weight medium
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)))
  (fontaine-set-preset 'default))

(use-package nerd-icons
  :straight t
  :custom (nerd-icons-font-family "IntoneMono Nerd Font Mono"))

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
  (defun jf/toggle-typopunct-mode ()
    "Toggle `typopunct-mode'."
    (interactive)
    (if typopunct-mode
      (typopunct-mode -1)
      (typopunct-mode 1))
    (force-mode-line-update))
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

(define-key global-map (kbd "C-M-s-SPC") #'jf/insert-non-breaking-space)

(defun jf/insert-non-breaking-space (&optional prefix)
  "Insert a non-breaking space PREFIX number of times"
  (interactive "p")
  (let ((times
         (or prefix 1))
        (char
         (if (derived-mode-p 'html-mode)
             "&nbsp;" " ")))
    (insert (s-repeat times char))))

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
  (setq display-buffer-alist
    `(;; no window
       ("\\`\\*Async Shell Command\\*\\'"
         (display-buffer-no-window))
       ("^\\*syncthing-"
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
       ("\\*\\.<gocov\\>$"
         (display-buffer-pop-up-window)
         (dedicated . t)
         (body-function . prot-window-select-fit-size))
       ((or "\\*rspec-compilation\\*")
         (display-buffer-reuse-mode-window
           display-buffer-below-selected))
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
  (setopt confirm-kill-emacs #'yes-or-no-p)
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
  :straight (ef-themes :host github :repo "protesilaos/ef-themes")
  :init
  (defvar jf/themes-plist '()
    "The named themes by pallette.")
  :config
  (setopt ef-themes-common-palette-overrides
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
  (setopt ef-themes-mixed-fonts t
    ef-themes-variable-pitch-ui t)

  (defun jf/theme-custom-faces ()
    "Set the various custom faces for both `treesit' and `tree-sitter'."
    (ef-themes-with-colors
      (setq hl-todo-keyword-faces
        `(
           ;; The first five are from Github:
           ;; https://github.com/orgs/community/discussions/16925
           ("NOTE" . ,blue-warmer)
           ("TIP" . ,green)
           ("IMPORTANT" . ,magenta-cooler)
           ("WARNING" . ,yellow)
           ("CAUTION" .,red-warmer)
           ;; Other keywords that I'm using
           ("HACK" . ,blue-warmer)
           ("TODO" . ,red-warmer)
           ("FIXME" . ,red-warmer)
           ("DONE" . ,green)
           ("ASSUMPTION" .,yellow)
           ("QUESTION" .,yellow)
           ("BLOCKED" . ,yellow)
           ("WAITING" . ,yellow)))
      (custom-set-faces
        `(shr-blockquote
           ((,c :foreground ,docstring)))
        `(shr-dfn
           ((,c :slant italic :foreground ,keyword)))
        `(shr-dt
           ((,c :slant italic :foreground ,keyword)))
        `(shr-cite
           ((,c :underline nil :slant italic :bold t :foreground ,mail-cite-0)))
        `(shr-aside
           ((,c :foreground ,comment :background ,bg-dim)))
        `(amread-highlight-face
           ((,c :foreground ,fg-main :background ,bg-search-lazy)))
        `(go-coverage-untracked
           ((,c :foreground ,fg-dim :background ,bg-dim)))
        `(go-coverage-8
           ((,c :foreground ,fg-added :background ,bg-added)))
        `(go-coverage-0
           ((,c :foreground ,fg-removed :background ,bg-removed)))
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
        `(go-test--standard-face
           ((,c :foreground ,info :background ,bg-info)))
        `(go-test--ok-face
           ((,c :foreground ,info)))
        `(go-test--error-face
           ((,c :foreground ,err)))
        `(jf/mode-line-format/face-shadow
           ((,c :foreground ,fg-mode-line)))
        `(jf/mode-line-format/face-shadow-highlight
           ((,c :foreground ,fg-mode-line :background ,bg-hover)))
        `(jf/tabs-face
           ((,c :underline (:style wave :color ,bg-blue-intense))))
        `(jf/org-faces-date
           ((,c :underline nil
              :foreground ,cyan-faint
              :underline (:color ,cyan-faint
                           :style dots))))
        `(jf/org-faces-epigraph
           ((,c :underline nil :slant italic :foreground ,fg-alt)))
        `(jf/org-faces-work
           ((,c :underline nil :slant italic :bold t :foreground ,mail-cite-0)))
        `(jf/org-faces-abbr
           ((,c :underline t :slant italic :foreground ,fg-dim)))
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
  ;; (setq jf/themes-plist '(:dark ef-bio :light ef-elea-light))
  (setq jf/themes-plist '(:dark ef-symbiosis :light ef-elea-light)))

(use-package custom
  :straight (:type built-in)
  :config
  ;; In organizing the packages, I discovred that themes is part of the
  ;; `custom' package.
  (defun jf/color-scheme-set-for-emacs (&optional given-scheme)
    "Function to load named theme."
    (let ((scheme
            (or given-scheme (funcall jf/color-scheme-func))))
      (ef-themes-select (plist-get jf/themes-plist scheme))))

  ;; Theming hooks to further customize colors
  (defvar after-enable-theme-hook nil
    "Normal hook run after enabling a theme.")

  (defun run-after-enable-theme-hook (&rest _args)
    "Run `after-enable-theme-hook'."
    (run-hooks 'after-enable-theme-hook))

  (advice-add 'enable-theme :after #'run-after-enable-theme-hook)

  (add-hook 'after-enable-theme-hook #'jf/theme-custom-faces)

  (defvar jf/color-scheme-func
    (if (eq system-type 'darwin)
      #'jf/current-macos-interface-style
      #'jf/current-color-scheme-gnome)
    "Function that returns :dark or :light, depending on current color scheme.")

  (defun jf/current-color-scheme-gnome ()
    "Determine Gnome preferred theme."
    (if (equal
          "'prefer-light'"
          (s-trim
            (shell-command-to-string
              "gsettings get org.gnome.desktop.interface color-scheme")))
      :light :dark))

  (defun jf/current-macos-interface-style ()
    "Determine MacOS preferred theme."
    (if (equal "Dark"
          (substring
            (shell-command-to-string
              "defaults read -g AppleInterfaceStyle") 0 4))
      :dark :light))

  (defvar jf/color-scheme-system-toggle-functions
    '(jf/color-scheme:gnome-color-scheme
       jf/color-scheme:gnome-gtk-theme
       jf/color-scheme:copyq-theme
       jf/color-scheme:emacs-theme)
    "A list of arity one functions that set component schemes based on the
input parameter.

When the parameter is non-nil, favor the dark option.  Otherwise favor
the light option.")

  (defun jf/color-scheme:gnome-color-scheme (lightp)
    "Set the gnome color scheme based on LIGHTP (e.g. light/dark)."
    (shell-command
      (format
        "gsettings set org.gnome.desktop.interface color-scheme prefer-%s"
        (if lightp "light" "dark"))))

  (defun  jf/color-scheme:gnome-gtk-theme (lightp)
    "Set the gnome gtk theme based on LIGHTP (e.g. light/dark)."
    (let ((theme
            (if lightp "Adwaita" "Adwaita-dark")))
      (shell-command
        (format
          "gsettings set org.gnome.desktop.interface gtk-theme %s"
          theme))))

  (defun jf/color-scheme:copyq-theme (lightp)
    "Set the copyq theme based on LIGHTP (e.g. light/dark)."
    (shell-command
      (format
        "copyq loadTheme %s/solarized-%s.ini"
        (s-trim
          (shell-command-to-string "copyq info themes"))
        (if lightp "light" "dark"))))

  (defun jf/color-scheme:emacs-theme (lightp)
    "Set the emacs theme based on LIGHTP (e.g. light/dark)."
    (ef-themes-select
      (plist-get jf/themes-plist
        (if lightp :light :dark))))

  (defun jf/color-scheme-system-toggle ()
    "Toggle system-wide Dark or Light setting."
    (interactive)
    (pcase system-type
      ('darwin
        (progn
          (shell-command
            (concat "osascript -e 'tell application \"System Events\" "
              "to tell appearance preferences "
              "to set dark mode to not dark mode'"))
          (jf/color-scheme-set-for-emacs)))
      (_
        (let ((lightp
                (eq :dark (jf/current-color-scheme-gnome))))
          (dolist (fn jf/color-scheme-system-toggle-functions)
            (funcall fn lightp))))))
  (defalias 'jf/dark 'jf/color-scheme-system-toggle)

  ;; Set the color scheme of emacs based on existing system function.
  (jf/color-scheme-set-for-emacs))

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
    (let ((title
            "TMR May Ring (Emacs tmr package)")
           (description
             (tmr--timer-description timer)))
      (if (eq system-type 'darwin)
        (ns-do-applescript (concat "display notification \""
                             description
                             "\" sound name \"Glass\""))
        (user-error "Unable to send tmr notification for OS"))))
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
  :straight (:host github :repo "magit/transient")
  ;; This exposes the --sign switch for git commit
  :config (setopt transient-default-level 5))

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
  (setopt wgrep-change-readonly-file t)
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
    :menu ("Search" "P" "Project prompt file type"))

  (when (f-dir-p "~/git/dotemacs/")
    (rg-define-search rg-projects-dotemacs
      "Search Dotemacs"
      :dir "~/git/dotemacs/"
      :files "*.*"
      :menu ("Projects" "j ." "Dotemacs")))

  :init (setq ripgrep-arguments "--ignore-case --follow")
  ;; I want to ensure that I'm following symlinks.  This is important
  ;; for my Personal Knowledge Management file structure and ensuring
  ;; that private and public concepts don't bleed across machines.
  (add-to-list 'rg-required-command-line-flags "--follow ")
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
  :custom
  (emacs-everywhere-major-mode-function #'emacs-everywhere-major-mode-org-or-markdown)
  (emacs-everywhere-frame-parameters
    '((name . "emacs-everywhere")
       (width . 80)
       (height . 12)
       (user-position . t)
       (top . 0)
       (left . 0)))
  :config
  ;; Create a nice editing experience, in which we have a well maximized
  ;; screen.  The position and fullscreen don't work.  So I'm relying on
  ;; setting a small frame, then expanding it to maximum.
  (add-hook 'emacs-everywhere-mode-hook #'olivetti-mode)
  (add-hook 'emacs-everywhere-mode-hook #'toggle-frame-maximized)
  )

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
          (if (eq (system-type 'darwin))
            (ns-do-applescript
              (concat
                "tell application \"TextEdit\"\n"
                "\tactivate\n"
                "\tset myrtf to the clipboard as «class RTF »\n"
                "\tset mydoc to make new document\n"
                "\tset text of mydoc to myrtf\n"
                "end tell"))
            (user-error "Unable to open RTF editor on OS")))))))

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
  :straight (org :github "bzg/org-mode")
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
  ;; (defun my-assign-custom-ids ()
  ;;     "Derive custom ID's based on `org-mode' headline.

  ;;   When duplicates encountered prompt for an alternate.

  ;; From Sacha's Post."
  ;;     (interactive)
  ;;     (let ((custom-ids
  ;;             (org-map-entries (lambda () (org-entry-get (point) "CUSTOM_ID")) "CUSTOM_ID={.}")))
  ;;       (org-map-entries
  ;;         (lambda ()
  ;;           (let ((slug
  ;;                   (replace-regexp-in-string
  ;;                     "^-\\|-$" ""
  ;;                     (replace-regexp-in-string "[^A-Za-z0-9]+" "-"
  ;;                       (downcase (string-join (org-get-outline-path t) " "))))))
  ;;             (while (member slug custom-ids)
  ;;               (setq slug (read-string "Manually set custom ID: " slug)))
  ;;             (org-entry-put (point) "CUSTOM_ID" slug)))
  ;;         "-CUSTOM_ID={.}")))
  (setopt org-closed-keep-when-no-todo t)
  (setopt org-agenda-include-inactive-timestamps t)
  (org-clock-persistence-insinuate)
  (setopt org-use-speed-commands t)
  (setopt org-agenda-clockreport-parameter-plist
    '(:link t :maxlevel 2 :stepskip0 t
       :fileskip0 t :filetitle t :tags t))
  (setopt org-outline-path-complete-in-steps nil)
  (setopt org-goto-interface #'outline-path-completion)
  (setopt org-time-stamp-rounding-minutes '(0 15))
  (setopt org-clock-rounding-minutes 15)
  (setopt org-link-frame-setup
    '((vm . vm-visit-folder-other-frame)
       (vm-imap . vm-visit-imap-folder-other-frame)
       (gnus . org-gnus-no-new-news)
       (file . find-file)
       (wl . wl-other-frame)))
  (setopt org-babel-ruby-wrapper-method
    (concat "results = self.instance_exec { %s } "
      "File.open('%s', 'w') { |f| "
      " f.write((results.class == String) ? results : results.inspect) "
      "}"))

  (setopt org-babel-ruby-pp-wrapper-method "require 'pp'
results = self.instance_exec { %s }
File.open('%s', 'w') { |f| $stdout = f; pp results }")
  (setopt org-clock-persist 'history)
  (setopt org-export-headline-levels 4)
  ;; When I would load the agenda, I'd invariably type "l" to list the
  ;; entries.
  (setopt org-agenda-start-with-log-mode 'only)
  ;; I continue to encounter issues with not properly generating table
  ;; of contents.  As such I used the following:
  ;;
  ;; https://emacs.stackexchange.com/questions/76255/why-is-the-toc-missing-in-org-mode-latex-output
  ;;
  ;; Note, I did change from pdflatex to lualatex as the LaTeX class I'm
  ;; often using are only available in Lua processing.
  ;;
  ;; See https://orgmode.org/worg/org-tutorials/org-latex-export.html#sec-12-3 for why 3
  (setopt org-latex-pdf-process
    '("lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
       "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
       "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; The 'minted backend provides souce code highlighting.
  (setopt
    org-latex-src-block-backend 'minted
    org-latex-compiler "lualatex"
    org-latex-custom-lang-environments
    '((emacs-lisp "common-lispcode"))
    org-latex-minted-options '(("frame" "lines")
                                ("fontsize" "\\footnotesize")
                                ("linenos" "")))

  (setopt
    org-auto-align-tags nil
    org-tags-column 0

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
    "Configure `org-mode' to my particulars."
    (setq-local tab-width 8)
    (jinx-mode 1)
    (add-hook 'before-save-hook
      #'jf/org-mode/recalculate-buffer-tables nil :local)
    ;; (add-hook 'before-save-hook
    ;;   #'jf/org-add-ids-to-headlines-in-file nil 'local)
    (add-hook 'focus-out-hook
      #'org-save-all-org-buffers nil :local)
    (jf/org-capf)
    (turn-on-visual-line-mode)
    (electric-pair-mode -1))

  ;; https://stackoverflow.com/questions/13340616/assign-ids-to-every-entry-in-org-mode
  (defun jf/org-add-ids-to-headlines-in-file ()
    "Add ID properties to all file's headlines without an ID."
    (interactive)
    (org-map-entries 'org-id-get-create))

  (org-babel-do-load-languages 'org-babel-load-languages
    (append org-babel-load-languages
      '((emacs-lisp . t)
         (shell . t)
         (verb . t)
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
  (org-latex-tables-booktabs t)
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
    (denote-get-path-by-id "20230210T184422")
    "The file where I'm capturing content.

By default this is my example code project.")

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
            (format "\\item[\\textbf{{%s:}}] %s\n" term value))
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

  (advice-add #'org-latex-special-block
    :around #'jf/org-latex-special-block)

  (defun jf/org-latex-special-block (orig-fun &rest args)
    "Transcode a SPECIAL-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
    (let ((type (org-element-property :type (car args))))
      (if (string= "marginnote" (downcase type))
        (let ((contents (cadr args)))
          (format "\n\\marginnote{%s}\n" contents))
        (apply orig-fun args))))

  (advice-add #'org-latex-property-drawer
    :override #'jf/org-latex-property-drawer)

  (defun jf/org-latex-format-basic-headline-function (_todo
                                                       _todo-type
                                                       _priority
                                                       text
                                                       _tags
                                                       _info)
    "Only render the TEXT of the headline.
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
        (org-dblock-update '(4))))))

(with-eval-after-load 'org
  (use-package ox
    :straight (ox :type built-in))
  (setq org-export-global-macros (list))

  (add-to-list 'org-export-global-macros
    '("kbd" . "@@html:<kbd>@@$1@@html:</kbd>@@"))
  (add-to-list 'org-export-global-macros
    '("cite" . "@@latex:\\textit{@@@@html:<cite>@@$1@@html:</cite>@@@@latex:}@@ "))
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
    '("i" . "@@latex:\\textit{@@@@html:<i class=\"dfn\">@@$1@@html:</i>@@@@latex:}@@"))
  (add-to-list 'org-export-global-macros
    '("em" . "@@latex:\\textit{@@@@html:<em>@@$1@@html:</em> @@@@latex:}@@"))
  (add-to-list 'org-export-global-macros
    '("mechanic" . "@@latex:\\textit{@@@@html:<i class=\"mechanic\">@@$1@@html:</i>@@@@latex:}@@"))
  (add-to-list 'org-export-global-macros
    '("m" . "@@latex:\\textit{@@@@html:<i class=\"mechanic\">@@$1@@html:</i>@@@@latex:}@@"))
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
       "\\documentclass[11pt,letter]{article}
\\usepackage[letter]{anysize}
\\usepackage{minted}
\\usepackage{array, booktabs, caption}
\\usemintedstyle{emacs}
\\usepackage[colorlinks = true,
        urlcolor  = blue]{hyperref}"
       ("\\section{%s}" . "\\section{%s}")
       ("\\subsection{%s}" . "\\subsection{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection{%s}")
       ("\\paragraph{%s}" . "\\paragraph{%s}")
       ("\\subparagraph{%s}" . "\\subparagraph{%s}")))
  (add-to-list 'org-latex-classes
    '("jf/landscape"
       "\\documentclass[11pt,letter,landscape]{article}
\\usepackage[letter]{anysize}
\\usepackage{minted}
\\usepackage{array, booktabs, caption}
\\usemintedstyle{emacs}"
       ("\\section{%s}" . "\\section{%s}")
       ("\\subsection{%s}" . "\\subsection{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection{%s}")
       ("\\paragraph{%s}" . "\\paragraph{%s}")
       ("\\subparagraph{%s}" . "\\subparagraph{%s}")))
  (add-to-list 'org-latex-classes
    '("tuftebook"
       "\\documentclass[notoc,sfsidenotes]{tufte-book}
\\usepackage{array, booktabs, caption}
\\hypersetup{pdftex,colorlinks=true,allcolors=blue}
\\usepackage{hypcap}
\\usepackage{color}
\\usepackage{amssymb}
\\usepackage{gensymb}
\\usepackage{nicefrac}
\\usepackage{units}"
       ("\\section{%s}" . "\\section*{%s}")
       ("\\subsection{%s}" . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}" . "\\paragraph*{%s}")
       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;; tufte-handout class for writing classy handouts and papers
  ;; (setq org-latex-classes nil)
  (add-to-list 'org-latex-classes
    '("tufte-handout"
       "\\documentclass{tufte-handout}
\\usepackage{array, booktabs, caption}
\\hypersetup{pdftex,colorlinks=true,allcolors=blue}
\\usepackage{color}
\\usepackage{amssymb}
\\usepackage{amsmath}
\\usepackage{gensymb}
\\usepackage{nicefrac}
\\usepackage{units}"
       ("\\section{%s}" . "\\section*{%s}")
       ("\\subsection{%s}" . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}" . "\\paragraph*{%s}")
       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setopt org-latex-default-class "jf/article")

  (defun jf/org-export-change-options (plist backend)
    "Conditinally add filter functions to our org-export."
    (cond
      ((equal backend 'latex)
        (if-let ((filter-body
                   (plist-get plist :filter-body)))
          (progn
            (add-to-list 'filter-body jf/ox/filter-body/latex)
            (plist-put plist :filter-body filter-body))
          (plist-put plist :filter-body '(jf/ox/filter-body/latex)))
        (if-let ((filter-final-output
                   (plist-get plist :filter-final-output)))
          (progn
            (add-to-list 'filter-final-output jf/ox/filter-final-output/latex)
            (plist-put plist :filter-final-output filter-final-output))
          (plist-put plist :filter-final-output '(jf/ox/filter-final-output/latex)))))
    plist)

  (add-to-list 'org-export-filter-options-functions
    'jf/org-export-change-options)

  (defun jf/ox/filter-final-output/latex (body backend info)
  "Conditionally add an acronym package to exported LaTeX document."
  (if-let ((abbr-links (plist-get info :abbr-links)))
    (replace-regexp-in-string
      "^\\\\documentclass\\(.*\\)"
      (lambda (md)
        "Acronym package to matching line."
        (concat "\\\\documentclass" (match-string 1 md)
          "\n\\\\usepackage[printonlyused,withpage]{acronym}"))
      body)
    body))

(defun jf/ox/filter-body/latex (body backend info)
  "Conditionally add a list of acronyms to the exported LaTeX document.

To have a meaningful render, this requires using the acronym LaTeX
package.  The `jf/ox/filter-final-output/latex' handles injecting that
LaTeX package."
  (if-let ((abbr-links (plist-get info :abbr-links)))
    ;; We encountered some links, let's add a section.
    (progn
      (concat
        body
        "\n\\section{List of Acronyms}\n"
        "\\begin{acronym}\n"
        (mapconcat
          (lambda (cell)
            "Create an acro for link."
            (format "\\acro{%s}{%s}" (car cell) (cdr cell)))
          ;; Sort the keys alphabetically.  Otherwise they are rendered
          ;; in the reverse order in which they are encountered.
          (sort abbr-links :key #'car)
          "\n")
        "\n\\end{acronym}\n"))
    body))

  (use-package ox-gfm
    :straight t
    :init
    (require 'ox-gfm))

  ;; (use-package igist
  ;;   :straight t
  ;;   :config
  ;;   (setq igist-current-user-name "jeremyf")
  ;;   (setq igist-auth-marker 'igist))

  ;; In
  ;; https://takeonrules.com/2022/02/26/note-taking-with-org-roam-and-transclusion/,

  ;; I wrote about ~org-transclusion~.  The quick version,
  ;; ~org-transclusion~ allows you to include text from one file into
  ;; another.  This allows for document composition.
  (use-package org-transclusion
    :straight t
    :init (setopt org-transclusion-exclude-elements
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

;; (cl-defun jf/org-agenda/send-forward-task ()
;;   "Send an `org-mode' task node forward."
;;   (interactive)
;;   (save-excursion
;;     (let* ((day-project-task
;;              (jf/org-agenda/timesheet/get-day-and-project-and-task-at-point))
;;             (from-project
;;               (plist-get day-project-task :project))
;;             (from-task
;;               (plist-get day-project-task :task)))
;;       ;; Narrowing the region to perform quicker queries on the
;;       ;; element
;;       (narrow-to-region (org-element-property :begin from-task)
;;         (org-element-property :end from-task))

;;       ;; Grab each section for the from-task and convert that into
;;       ;; text.
;;       ;;
;;       ;; Yes we have the from-task, however, we haven't parsed that
;;       ;; entity.  Without parsing that element, the
;;       ;; `org-element-contents' returns nil.
;;       (let ((content
;;               (s-join "\n"
;;                 (org-element-map (org-element-parse-buffer)
;;                   'section
;;                   (lambda (section)
;;                     (mapconcat
;;                       (lambda (element)
;;                         (pcase (org-element-type element)
;;                           ;; I want to skip my time entries
;;                           ('drawer nil)
;;                           (_ (buffer-substring-no-properties
;;                                (org-element-property
;;                                  :begin element)
;;                                (org-element-property
;;                                  :end element)))))
;;                       (org-element-contents section)
;;                       "\n"))))))
;;         (widen)
;;         (org-capture-string
;;           (format "%s %s :%s:\n\n%s %s %s :%s:\n%s"
;;             (s-repeat (org-element-property :level from-project) "*")
;;             (org-element-property :raw-value from-project)
;;             (s-join ":" (org-element-property :tags from-project))
;;             (s-repeat (org-element-property :level from-task) "*")
;;             (org-element-property :todo-keyword from-task)
;;             (org-element-property :raw-value from-task)
;;             (s-join ":" (org-element-property :tags from-task))
;;             content)
;;           "d"))
;;       ;; Now that we've added the content, let's tidy up the
;;       ;; from-task.
;;       (goto-char (org-element-property :contents-begin from-task))
;;       ;; Prompt for the todo state of the original task.
;;       (call-interactively 'org-todo))))

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
  (concat "[[${remote-url}][${function-name}]] "
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
  "Given LINK and CONTENT return a string to insert into the capture."
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

(defun jf/org/capture/dictionary/sort ()
  "Sort the dictionary."
  (save-restriction
    (widen)
    (save-excursion
      (org-backward-paragraph)
      (forward-line)
      (org-sort-list nil ?a))))

(defun jf/org/capture/quote/name-that-block ()
  "Name a quote/verse block from capture block.

Use `denote-sluggify' as the naming function for the quote"
  (let* (
          ;; The evaluated text thus far.  With prompts completed.
          (template
            (plist-get org-capture-current-plist :template))
          ;; By convention the second line of the template is the
          ;; first line of the content block
          (first-line-of-block-content
            (nth 1 (s-split "\n" template)))
          ;; Derive the name of the block by leveraging
          ;; `denote-sluggify'.
          (name
            (s-join "-"
              (seq-take
                (s-split "-"
                  (denote-sluggify 'title
                    first-line-of-block-content))
                8))))
    (save-excursion
      ;; Place the name of the quote just above the start of the
      ;; block.
      (goto-char (point-min))
      (insert (format "#+NAME: %s\n" name)))))

(defun jf/org/capture/quote-location ()
  "Position to a selected “quotable” element base on prompts.

Narrow focus to a tag, then a named element."
  (let* (
          ;; With the given tag, find all associated headlines that match that
          ;; tag.
          (headline-alist
            (org-element-map
              (org-element-parse-buffer 'headline)
              'headline
              (lambda (headline)
                (when
                  (eq (org-element-property :level headline) 2)
                  (let ((title
                          (org-element-property :title headline))
                         (subtitle
                           (org-entry-get headline "SUBTITLE"))
                         (author
                           (org-entry-get headline "AUTHOR")))
                    (cons
                      (jf/book-make-label title subtitle author)
                      (org-element-property :contents-begin headline)))))))
          ;; Prompt me to pick one of those headlines.
          (headline
            (completing-read
              "Quotable: "  headline-alist nil t)))
    (goto-char (alist-get headline headline-alist nil nil #'string=))
    (while (org-element-type-p (org-element-at-point) '(drawer property-drawer keyword planning))
      (goto-char (org-element-property :end (org-element-at-point))))))
(defvar jf/filename/shopping-list
  "~/SyncThings/source/books-to-get-from-bibliography.txt"
  "Dude, these are the books I'm curious about.")

(defvar jf/filename/epigraphy-takeonrules
  "~/git/takeonrules.source/content/site-map/epigraphs/index.md"
  "Page that I generate and push to the
https://takeonrules.com/site-map/epigraphs url.")

(defun jf/org-sort-entries/ignoring-stop-words ()
  "Sort org entries while ignoring stop words."
  (interactive)
  (org-sort-entries
    nil
    ?f
    (lambda ()
      "Remove the leading stop words from the title."
      (let ((heading
              (nth 4 (org-heading-components)))
             (case-fold-search t))
        (replace-regexp-in-string
          "^\\(The\\|A\\|An\\) " "" heading)))
    #'string<))

(defvar jf/elfeed-reading-priorities
  '("1st" "2nd" "3rd" "4th" "5th")
  "The priorities for RSS feeds, from which we can derive their index and
sort accordingly.")

(defun jf/org-sort-entries/by-elfeed-tag-priority ()
  "Sort org entries by elfeed tag priority."
  (interactive)
  (org-sort-entries
    nil
    ?f
    (lambda ()
      (format "%s-%s"
        ;; Find the priority elfeed tag.
        (or
          (cl-position
            (car
              (cl-intersection
                (org-element-property :tags (org-element-at-point))
                jf/elfeed-reading-priorities
                :test
                #'string=))
            jf/elfeed-reading-priorities
            :test #'string=)
          9)
        ;; And the title.
        (org-element-property :title (org-element-at-point))))
    #'string<))

(setq org-capture-templates
  `(("c" "Content to Clock"
      plain (clock)
      "%(jf/denote/capture-wrap :link \"%L\" :content \"%i\")"
      :empty-lines 1)
     ("d" "Dictionary"
       plain (file
               ,(denote-get-path-by-id "20230108T083359"))
       "- %^{Term} :: %^{Description}; %a"
       :after-finalize jf/org/capture/dictionary/sort)
     ("p" "Person to Quote"
       entry
       (file+headline jf/filename/bibliography "People")
       "%^{Name} :people:\n:PROPERTIES:\n:CUSTOM_ID: %(org-id-new)\n:END:\n%?"
       :jump-to-captured t)
     ("q" "Quote"
       plain
       (file+function jf/filename/bibliography
         jf/org/capture/quote-location)
       "#+begin_%^{Type|quote|quote|verse}\n%^{Text}\n#+end_%\\1"
       :prepare-finalize jf/org/capture/quote/name-that-block
       :jump-to-captured t
       :empty-lines 1)
     ("w" "Work"
       entry
       (file+headline jf/filename/bibliography "Works")
       "%^{Title} %^g\n:PROPERTIES:\n:CUSTOM_ID: %(org-id-new)\n:SUBTITLE: %^{Subtitle}\n:AUTHOR: %^{Author}\n:END:\n%?"
       :jump-to-captured t
       :after-finalize jf/org/capture/finalize-work)))

(use-package verb
  ;; https://github.com/federicotdn/verb
  :after org
  :straight t)

(use-package org-web-tools
  ;; A package that I can pull down a web page and store its content as
  ;; an `org-mode' file.
  :straight t
  :config
  (setopt org-web-tools-pandoc-sleep-time 1.5))

(defun jf/md-to-org-region (start end)
  "Convert region from markdown to org."
  ;; From http://yummymelon.com/devnull/converting-a-markdown-region-to-org-revisited.html
  (interactive "r")
  (shell-command-on-region start end "pandoc -f markdown -t org" t t))

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
  (menu-bar-mode)
  (column-number-mode t)
  (global-display-fill-column-indicator-mode t)
  (delete-selection-mode t)
  (auto-save-file-name-transforms
    '((".*" "~/.emacs.d/autosaves/\\1" t)))
  :init
  ;; Emacs 28: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not supposed to be
  ;; used via M-x.
  (setopt read-extended-command-predicate
    #'command-completion-default-include-p)
  ;; TAB cycle if there are only few candidates
  (setopt completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setopt tab-always-indent 'complete)
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
  (setopt minibuffer-prompt-properties
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
          ;; ("C-l" . consult-goto-line)
          ("M-g o" . consult-outline)
          ("M-g m" . consult-mark)
          ("M-g M" . consult-global-mark)
          ("C-x C-SPC" . consult-global-mark)
          ("M-i" . jf/consult-imenu)
          ("M-g i" . consult-imenu)
          ("M-g x" . jf/consult-recent-xref)
          ("M-g I" . consult-imenu-multi)
          ;; M-s bindings (search-map)
          ("M-s f" . consult-find)
          ;; ("M-s L" . consult-locate)
          ;; ("M-s g" . consult-git-grep)
          ;; ("M-s G" . consult-git-grep)
          ;;
          ;; I keep this around because orderless search is great
          ("M-s r r" . consult-ripgrep)
          ;; ("C-c f" . consult-ripgrep)
          ;; ("M-s l" . consult-line)
          ("M-s M-s" . consult-line-multi)
          ;; Customizations that map to ivy
          ("C-c r" . consult-recent-file)
          ;; ("C-c o" . consult-file-externally)
          ;; I've long favored Swiper mapped to c-s
          ("M-s s" . consult-line)
          ("C-s" . isearch-forward)
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
  (setopt xref-show-xrefs-function #'consult-xref
    xref-show-definitions-function #'consult-xref)
  ;; Optionally configure the register formatting. This improves the
  ;; register preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setopt register-preview-delay 0.5
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
      "--glob !doc/ --glob !vendor/  --glob !**/log/ "
      " . -e ARG OPTS"))
  (consult-ripgrep-args
    (concat "rg --null --hidden --line-buffered --color=never "
      "--max-columns=1000 --follow "
      "--path-separator / --no-ignore-vcs --smart-case --no-heading "
      "--glob !vendor/ --glob !coverage/ --glob !**/tmp/ "
      "--glob !public/ --glob !node_modules/ --glob !.git/ "
      "--glob !doc/ --glob !vendor/ --glob !**/log/ "
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
                  (?m "Method" font-lock-function-name-face)))))
  (dolist (go '(go-ts-mode go-mode))
    (add-to-list 'consult-imenu-config
      `(,go
         :toplevel "Function"
         :types ((?f "Function" font-lock-function-name-face)
                  (?m "Method" font-lock-function-name-face)
                  (?s "Struct" font-local-type-face)
                  (?i "Interface" font-local-type-face)
                  (?r "t.Run" font-lock-doc-face)
                  (?t "Type" font-local-type-face)
                  (?a "Alias" font-local-type-face))))))

(defun go-ts-mode--testing-run-node-p (node)
  "Return t when NODE is a testing.T.Run declaration."
  (and
    (string-equal "call_expression" (treesit-node-type node))
    (when-let* ((fnNode (treesit-node-child-by-field-name node "function"))
                 (operand (treesit-node-child-by-field-name fnNode "operand"))
                 (field (treesit-node-child-by-field-name fnNode "field")))
      (and
        (string= (treesit-node-type operand) "identifier")
        (string= (treesit-node-text operand) "t")
        (string= (treesit-node-type field) "field_identifier")
        (string= (treesit-node-text field) "Run")))))

(defun go-ts-mode--testing-run-name (node)
  "Get imenu t.Run NODE name."
  (let* ((args
           (treesit-node-child-by-field-name node "arguments")))
    (format "%s"
      (treesit-node-text (car (treesit-node-children args "argument_list"))))))

(defvar consult--xref-history nil)

(setq xref-marker-ring-length 32)

(defun jf/consult-recent-xref (&optional markers)
  "Jump to a marker in MARKERS list (defaults to `xref--history'.

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history."
  (interactive)
  (consult--read
    (consult--global-mark-candidates
      (or markers (flatten-list xref--history)))
    :prompt "Go to Xref: "
    :annotate (consult--line-prefix)
    :category 'consult-location
    :sort nil
    :require-match t
    :lookup #'consult--lookup-location
    :history '(:input consult--xref-history)
    :add-history (thing-at-point 'symbol)
    :state (consult--jump-state)))


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
  ;; This package provides a function I use everyday: ~M-x
  ;; consult-projectile~.  When I invoke ~consult-projectile~, I have
  ;; the file completion for the current project.  I can also type =b= +
  ;; =SPACE= to narrow my initial search to open buffers in the project.
  ;; Or =p= + =space= to narrow to other projects; and then select a
  ;; file within that project.  And so much more.
  :commands (consult-projectile)
  :bind (("M-s r r" . consult-ripgrep)
          ("M-s r n" . jf/consult-ripgrep-no-generated))
  :straight (consult-projectile
              :type git
              :host gitlab
              :repo "OlMon/consult-projectile"
              :branch "master")
  :config
  (defun jf/consult-ripgrep-no-generated ()
    "As `consult-ripgrep' but omit generated files."
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (let ((consult-rigrep-args
              (concat "rg -e \"^// Code generated .*DO NOT EDIT\\.$\" "
                ". --files-without-match --glob=\\!vendor | "
                "xargs " consult-ripgrep-args)))
        (call-interactively #'consult-ripgrep))))
  (defvar jf/consult--source-draft-blog-posts
    `(:name "Draft Blog Posts"
       :narrow ?D
       :cateogry 'file
       :face 'consult-file
       :history file-name-history
       :enabled (lambda ()
                  (file-exists-p (expand-file-name "~/.my-computer")))
       :action (lambda (f)
                 (consult--file-action (f-join (denote-directory) f)))
       :items (lambda ()
                (split-string-and-unquote
                  (shell-command-to-string
                    ;; First narrow to files with tags
                    (concat
                      "cd " (denote-directory) "; fd \"_" jf/denote/keywords/blogPosts ".*\\."
                      (symbol-name denote-file-type) "\" | "
                      "xargs rg \"^#\\+ROAM_REFS:\" -i --files-without-match --sortr modified"))
                  "\n")))
    "A `consult--read' conformant structure for draft blog posts.")

  (defvar jf/consult--source-recent-file
    (let ((data consult--source-recent-file))
      (plist-put data :narrow '(?R . "All Recent Files"))
      (plist-put data :name "All Recent Files"))
    "A `consult--read' conformant structure for all recent files.
In my general finder function, I to have both recent project files as
well as recent files spanning projects.  This adjustment makes that
possible.")
  (setopt consult-projectile-sources
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
       ;; key D
       jf/consult--source-draft-blog-posts
       ;; key *
       consult--source-modified-buffer))
  :bind
  ("s-p" . consult-projectile)
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

(use-package helpful
  ;; Help me lookup definitions and details.
  :init
  (use-package transient :straight (:host github :repo "magit/transient"))
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
                                      &key (dir jf/denote-base-dir))
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
  (tempel-key "H-m v" verb_block org-mode-map)
  (tempel-key "H-m c" macro-cite org-mode-map)
  (tempel-key "H-m e" macro-emphatic org-mode-map)
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
  (setopt read-file-name-completion-ignore-case t
    read-buffer-completion-ignore-case t
    completion-ignore-case t)
  (setopt vertico-cycle t)
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
  :straight (:type built-in)
  :custom
  (which-key-side-window-max-width 0.5)
  (which-key-min-column-description-width 60)
  (which-key-max-description-length nil)
  (which-key-show-docstrings t)
  (which-key-add-column-padding 2)
  (which-key-separator " :: ")
  (which-key-max-display-columns 2)
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (which-key-show-major-mode))

(defvar jf/filename/bibliography
  (denote-get-path-by-id "20241124T080648")
  "Dude, you can put your books in here.")

(cl-defun jf/denote? (&key (buffer (current-buffer)))
  "Return non-nil when BUFFER is for `denote'."
  (require 'denote)
  (when-let* ((file (buffer-file-name buffer)))
    (denote-file-is-note-p file)))

(use-package consult-denote
  ;; I had been using consult-notes, even writing about that package in
  ;; http://takeonrules.com/2025/04/11/extending-consult-notes-package-to-add-draft-blog-post-candidates/
  ;;
  ;; The writing about it helped clarify what I was after, and I did
  ;; learn something.  However, the `consult-denote' package is far more
  ;; "compact" and serves my needs quite well.
  :after (consult denote)
  :straight t
  :bind
  ("H-f" . #'consult-denote-find)
  ("H-s" . #'consult-denote-grep)
  :custom
  (consult-denote-grep-command #'consult-ripgrep)
  :config
  (setopt consult-denote-find-command
    ;; fd version 8 does not work with consult's parameters.  I know
    ;; that fd 10 works.
    (if (and
          (executable-find "fd")
          (<= 10
            (string-to-number
              (shell-command-to-string
                "fd --version | rg \"fd (\\d+)\" -r '\$1' --only-matching"))))
      #'consult-fd
    #'consult-find)))

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

(use-package synosaurus
  ;; brew install wordnet
  :straight (synosaurus :type git :host github :repo "hpdeifel/synosaurus")
  :custom (synosaurus-choose-method 'default))

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

(use-package jinx
  ;; `brew install enchant`
  :straight t
  :bind ("M-$" . #'jinx-correct)
  :bind (:map jinx-mode-map
          (("M-e n" . jinx-next)
            ("M-e n" . jinx-previous)))
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

(use-package treesit
  :straight (:type built-in)
  :init
  (setopt treesit-font-lock-level 4)
  :config
  (defvar jf/treesit-lang-cache
    (make-hash-table :test 'equal)
    "Cache the expensive computation of treelit language availability.

See `jf/treesit-language-available-p' for usage.")

  (defun jf/treesit-language-available-p (fn lang &rest rest)
    "Caching around the CPU expensive `treesit-language-available-p'."
    ;; I did some profiling of `treesit-language-available-p', and found
    ;; that when moving around via consult (and therefore preview) this
    ;; function was contributing to 75% of the CPU time.  And it was run
    ;; each time.
    (let ((cached-value
            (gethash lang jf/treesit-lang-cache 'miss)))
      (if (eq 'miss cached-value)
        (let ((value
                (apply fn lang rest)))
          (puthash lang value jf/treesit-lang-cache)
          value)
        cached-value)))
  (advice-add #'treesit-language-available-p
    :around #'jf/treesit-language-available-p)

  (add-to-list 'treesit-language-source-alist
             '(gitcommit . ("https://github.com/gbprod/tree-sitter-gitcommit")))
  :preface
  (defun jf/treesit/func-signature/dwim ()
    "Kill current function signature at point."
    (interactive)
    (when-let ((node
                 (treesit-parent-until
                   (treesit-node-at (point))
                   (lambda (n)
                     (or
                       (string= "function_declaration" (treesit-node-type n))
                       (string= "call_expression" (treesit-node-type n))))
                   t)))
      (pcase (treesit-node-type node)
        ("function_declaration"
          (jf/treesit/get-signature/function node))
        ("call_expression"
          (save-excursion
            (call-interactively #'xref-find-definitions)
            (jf/treesit/get-signature/function
              (treesit-parent-until
                (treesit-node-at (point))
                (lambda (n)
                  (string= "function_declaration" (treesit-node-type n)))
                t)))))))

  (defun jf/treesit/get-signature/function (node)
    "For the given NODE add its parameters and result to kill ring.

This function is to \"copy\" the implementation details of the node."
    (let ((node-type (treesit-node-type node)))
      (if (string= "function_declaration" node-type)
        (let* ((strings
                 (list
                   (treesit-node-text
                     (treesit-node-child-by-field-name node "parameters"))
                   (treesit-node-text
                     (treesit-node-child-by-field-name node "result"))))
                (msg
                  (concat "func" (s-join " " (-non-nil strings)))))
          (message
            "%s func dsignature: %s"
            (treesit-node-text
              (treesit-node-child-by-field-name node "name"))
            msg)
          (kill-new msg))
        (user-error "given node %s (type %s) not function_declaration"
          node node-type))))

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

(use-package cognitive-complexity
  :straight (:host github :repo "abougouffa/cognitive-complexity"))

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

  (add-to-list 'scopeline-targets
    '(go-mode "function_declaration" "func_literal" "method_declaration" "if_statement" "for_statement" "type_declaration" "call_expression"))
  (add-to-list 'scopeline-targets
    '(go-ts-mode "function_declaration" "func_literal" "method_declaration" "if_statement" "for_statement" "type_declaration" "call_expression"))
  :hook ((go-ts-mode go-mode) . scopeline-mode))

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

(use-package flymake-go-staticcheck
  ;; Adding a linter to go files, leveraging the staticcheck binary.
  ;; See: https://github.com/dominikh/go-tools/tree/master/cmd/staticcheck
  :straight t)

(use-package go-mode
  :straight t)

(use-package go-ts-mode
  :straight (:type built-in)
  :hook (go-ts-mode . jf/go-ts-mode-configurator)
  :bind (:map go-ts-mode-map
          (("s-." . 'jf/go/toggle-test-impl)
            ("H-e c" . 'jf/go/show-coverage)))
  :config
  (defun jf/go/show-coverage (&optional coverage-file)
    (interactive)
    (let* ((file (or coverage-file "test.coverage")))
      (if (f-file-p file)
        (let ((pos (point)))
          (go-coverage file)
          (when (< pos (max-char))
            (goto-char pos)))
        (go-test-current-project))))

  (defun jf/go/toggle-test-impl ()
    "As `projectile-toggle-between-implementation-and-test'."
    (interactive)
    (if (s-ends-with? "_test.go" (buffer-file-name))
      (projectile-toggle-between-implementation-and-test)
      (find-file (format "%s_test.go" (file-name-base (buffer-file-name))))))
  (defun go-format ()
    "Formats the current buffer according to the goimports tool."
    (interactive)
    (let ((tmpfile (make-temp-file "gofmt" nil ".go"))
           (patchbuf (get-buffer-create "*Gofmt patch*"))
           (errbuf (get-buffer-create "*Gofmt Errors*"))
           (coding-system-for-read 'utf-8)
           (coding-system-for-write 'utf-8))

      (with-current-buffer errbuf
        (setq buffer-read-only nil)
        (erase-buffer))
      (with-current-buffer patchbuf
        (erase-buffer))

      (write-region nil nil tmpfile)

      ;; We're using errbuf for the mixed stdout and stderr output. This
      ;; is not an issue because gofmt -w does not produce any stdout
      ;; output in case of success.
      (if (zerop (call-process "goimports" nil errbuf nil "-w" tmpfile))
        (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
          (progn
            (kill-buffer errbuf)
            (message "Buffer is already gofmted"))
          (go--apply-rcs-patch patchbuf)
          (kill-buffer errbuf)
          (message "Applied gofmt"))
        (message "Could not apply gofmt. Check errors for details")
        (gofmt--process-errors (buffer-file-name) tmpfile errbuf))

      (kill-buffer patchbuf)
      (delete-file tmpfile)))
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

(use-package go-tag
  ;; go install github.com/fatih/gomodifytags@latest
  :straight t
  :after go-ts-mode
  :bind (:map go-ts-mode-map
          (("H-g d" . #'go-browse-doc)
            ("H-g t" . #'go-tag-add)
            ("H-g H-g f" . #'go-goto-function)
            ("H-g H-g d" . #'go-goto-docstring)
            ("H-g H-g a" . #'go-goto-arguments)
            ("H-g T" . #'go-tag-remove))))

(use-package go-fill-struct
  ;; go install github.com/davidrjenni/reftools/cmd/fillstruct@latest
  :straight t
  :after go-ts-mode
  :bind (:map go-ts-mode-map
          (("H-g f" . #'go-fill-struct))))

(use-package gotest
  :straight t
  :after go-ts-mode
  :config
  (setq-default go-run-go-command "LOGGING_LEVEL=22 go")
  (setq-default go-test-go-command "LOGGING_LEVEL=22 go")
  (setq-default go-test-args (concat "-count 1 -v  --failfast -coverprofile=test.coverage")))

(use-package dape
  :straight t)

(use-package repeat
  :straight (:type built-in)
  :config
  (repeat-mode))

(use-package gotest-ts
  :straight (:host github :repo "chmouel/gotest-ts.el")
  :custom (dape-cwd-fn #'jf/dape-cwd-fn)
  :config
  (defun jf/dape-cwd-fn ()
    default-directory)
  :init
  (defun jf/dape/go-test-at-point ()
    "Run `dape' for go test at point.

See https://github.com/chmouel/gotest-ts.el"
    (interactive)
    (dape (dape--config-eval-1
            `(modes (go-ts-mode)
               ensure dape-ensure-command
               fn dape-config-autoport
               command "dlv"
               command-args ("dap" "--listen" "127.0.0.1::autoport")
               command-cwd dape-cwd-fn
               port :autoport
               :type "debug"
               :request "launch"
               :mode "test"
               :cwd dape-cwd-fn
               :program (lambda () (concat "./" (file-relative-name default-directory (funcall dape-cwd-fn))))
               :args (lambda ()
                       (when-let* ((test-name (gotest-ts-get-subtest-ts)))
                         (if test-name `["-test.run" ,test-name]
                           (error "No test selected")))))))))

(defun jf/go-ts-mode-configurator ()
  ;; From go-mode
  (setq-local paragraph-start
    (concat "[[:space:]]*\\(?:"
      comment-start-skip
      "\\|\\*/?[[:space:]]*\\|\\)$"))
  (setq-local paragraph-separate paragraph-start)
  (setq-local fill-paragraph-function #'go-fill-paragraph)
  (setq-local fill-forward-paragraph-function #'go--fill-forward-paragraph)
  (setq-local adaptive-fill-function #'go--find-fill-prefix)
  (setq-local adaptive-fill-first-line-regexp "")
  (setq-local comment-line-break-function #'go--comment-indent-new-line)

  (add-to-list
    'treesit-simple-imenu-settings
    `("t.Run" "\\`call_expression\\'" go-ts-mode--testing-run-node-p go-ts-mode--testing-run-name))
  (setq-local tab-width 2)
  (setq-local eglot-stay-out-of '(imenu))
  (if (s-ends-with? "_test.go" (buffer-file-name))
    (jf/minor-mode/go-ts-test-mode)
    (jf/minor-mode/go-ts-implementation-mode))
  ;; (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
  (add-hook 'before-save-hook #'go-format -15 t))

(defvar jf/minor-mode/go-ts-test-mode-map
  (let ((map
          (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'jf/go-test-current)
    map))

(defun jf/go-test-current (prefix)
  "Run current test.  When PREFIX given run using `dape`."
  (interactive "p")
  (cond
    ((>= prefix 16)
      (let ((current-prefix-arg nil))
        (go-test-current-file)))
    ((>= prefix 4)
      (let ((current-prefix-arg nil))
        (jf/dape/go-test-at-point)))
    (t
      (gotest-ts-run-dwim))))

(defvar jf/minor-mode/go-ts-implementation-mode-map
  (let ((map
          (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'go-test-current-project)
    map))

(define-minor-mode jf/minor-mode/go-ts-test-mode
  "A minor mode to augment test files in `go-ts-mode'."
  :init-value nil
  :global nil
  :keymap jf/minor-mode/go-ts-test-mode-map)

(define-minor-mode jf/minor-mode/go-ts-implementation-mode
  "A minor mode to augment implementation files in `go-ts-mode'."
  :init-value nil
  :global nil
  :keymap jf/minor-mode/go-ts-implementation-mode-map)

(use-package outline-indent
  ;; Simple and basic collapsable outline modes.
  :straight t
  :hook (prog-mode . outline-indent-minor-mode))

(use-package ruby-mode
  ;; My language of choice for professional work.
  :straight (:type built-in)
  :custom (ruby-flymake-use-rubocop-if-available nil)
  :bind
  (:map ruby-mode-map
    (("C-M-h" . jf/treesit/function-select)
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
  (setopt projectile-switch-project-action
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
  ;; A simple package to highlight todos identify by the
  ;; `hl-todo-keyword-faces'.
  :straight t
  :bind (("H-t n" . hl-todo-next)
          ("H-t p" . hl-todo-previous))
  :config (global-hl-todo-mode))


(use-package magit-todos
  ;; Package that adds a `magit' section highlighting todos in the
  ;; current repository; and even highlighting what todos were added in
  ;; the branch but not in main.
  :config (magit-todos-mode)
  :commands (magit-todos-list)
  :bind (("H-t H-t" . magit-todos-list))
  :custom (magit-todos-exclude-globs '(".git/" "public/" "vendor/"))
  ;; (magit-todos-keywords-list
  ;;   '("TODO" "HACK" "QUESTION" "BLOCKED" "WAITING" "FIXME"))
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
  :custom
  (markdown-fontify-code-blocks-natively t)
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
  (dolist (m '(markdown-mode gfm-mode))
    (font-lock-add-keywords m
      '(("{{[^}]+}}" . font-lock-function-name-face)))))

(defun jf/markdown-toc (&optional depth)
  "Extract DEPTH of headings from the current Markdown buffer.

The generated and indented TOC will be inserted at point."
  (interactive "P")
  (let ((max-depth (or depth 3)) toc-list)
    (setq-local markdown-toc nil)
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
        (setq-local markdown-toc
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
  :config
  (add-hook 'prog-mode-hook #'jf/prog-mode-configurator)
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
  (defun jf/yank-current-scoped-function-as-org-mode-link ()
    "Yank the current function and region as an `org-mode' link."
    (interactive)
    (if-let ((text
               (funcall add-log-current-defun-function)))
      (let ((link
              (format "[[%s][%s]]"
                (call-interactively #'git-link)
                text)))
        (message link)
        (kill-new (substring-no-properties link)))
      (user-error "Warning: Point not on function")))
  (bind-key "C-M-e"
    #'end-of-defun prog-mode-map)
  (bind-key "C-M-a"
    #'beginning-of-defun prog-mode-map)
  (bind-key "C-c y f"
    #'jf/yank-current-scoped-function-name prog-mode-map)
  (bind-key "C-c y o"
    #'jf/yank-current-scoped-function-as-org-mode-link prog-mode-map)
  (bind-key "C-c y f"
    #'jf/yank-current-scoped-function-name emacs-lisp-mode-map)
  (bind-key "C-c y o"
    #'jf/yank-current-scoped-function-as-org-mode-link emacs-lisp-mode-map)

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

    ;; CVE-2024-53920
    ;; https://eshelyaron.com/posts/2024-11-27-emacs-aritrary-code-execution-and-how-to-avoid-it.html
    (unless (derived-mode-p 'emacs-lisp-mode)
      (flymake-mode 1))
    (hl-todo-mode t)
    (setq show-trailing-whitespace t)
    (setq truncate-lines t)
    (which-function-mode)))

(use-package text-mode
  :straight (:type built-in)
  :config
  (add-hook 'text-mode-hook #'jf/text-mode-configurator)
  (defun jf/text-mode-configurator ()
    (setq show-trailing-whitespace t)))

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

;; I've been exploring either `lsp-mode' or `eglot' and thusfar prefer
;; the lightweight nature of `eglot'.
(use-package eglot
  :straight t
  :bind
  ("H-e h" . flymake-show-buffer-diagnostics)
  ("H-e n" . flymake-goto-next-error)
  ("H-e p" . flymake-goto-prev-error)
  (:map eglot-mode-map
    ("H-e s" . consult-eglot-symbols)
    ("H-e r" . eglot-reconnect)
    ;; Type M-/ then M-? to jump to definition and then find its usage
    ("M-/" . jf/treesit/jump-to-declaration-identifier)
    ;; ("H-e s" . jf/treesit/func-signature/dwim)
    ("H-e m" . eglot-rename)
    ("H-e a" . eglot-code-actions)
    ("H-e i" . eglot-find-implementation))
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
           json-mode json-ts-mode ;; npm i -g vscode-langservers-extracted
           python-mode python-ts-mode
           ruby-mode ruby-ts-mode
           scss-mode scss-ts-mode
           typescript-ts-mode typescript-mode ;; https://github.com/typescript-language-server/typescript-language-server
           )
          . eglot-ensure)
  :config
  (defun jf/treesit/jump-to-declaration-identifier ()
    "Within a defun context, jump to the identifier.

Useful for Eglot."
    (interactive)
    (if-let* ((dfn
                (treesit-defun-at-point))
               (dfn-identifier
                 (car (treesit-filter-child
                        dfn
                        (lambda (node)
                          (string= "identifier"
                            (treesit-node-type node)))))))
      (goto-char (treesit-node-start dfn-identifier))))
  ;; Cribbed from:
  ;; https://codeberg.org/mekeor/init/src/commit/11e3d86aa18090a5e3a6f0d29373c24373f29aaf/init.el#L666-L811
  ;;
  ;; References:
  ;; https://github.com/golang/tools/blob/master/gopls/doc/settings.md
  (setf (plist-get eglot-workspace-configuration :gopls)
    '(;; :allExperiments ;; bool false

       ;; BUILD

       ;; :buildFlags                 ;; []string                 []
       ;; :env                        ;; map[string]string        {}
       ;; :directoryFilters           ;; []string                 ["-**/node_modules"]
       ;; :templateExtensions         ;; []string                 []
       ;; :memoryMode                 ;; "DegradeClosed"|"Normal" "Normal"
       ;; :expandWorkspaceToModule    ;; bool                     true
       ;; :allowModfileModifications  ;; bool                     false
       ;; :allowImplicitNetworkAccess ;; bool                     false
       ;; :standaloneTags             ;; []string                 ["ignore"]

       ;; FORMATTING

       ;; :local   ;; string ""
       :gofumpt t ;; bool   false

       ;; UI

       ;; :codelenses (
       ;; :gc_details         ;; bool false
       ;; :generate           ;; bool true
       ;; :regenerate_cgo     ;; bool true
       ;; :run_govulncheck    ;; bool undocumented
       ;; :test               ;; bool undocumented
       ;; :tidy               ;; bool true
       ;; :upgrade_dependency ;; bool true
       ;; :vendor             ;; bool true
       ;; )

       ;; :semanticTokens   ;; bool false
       ;; :noSemanticString ;; bool false
       ;; :noSemanticNumber ;; bool false

       ;; COMPLETION

       ;; :usePlaceholders                ;; bool                                      false
       ;; :completionBudget               ;; time.Duration                             "100ms"
       ;; :matcher                        ;; "CaseInsensitive"|"CaseSensitive"|"Fuzzy" "Fuzzy"
       ;; :experimentalPostfixCompletions ;; bool                                      true
       ;; :completeFunctionCalls          ;; bool                                      true

       ;; DIAGNOSTIC

       :analyses
       (
         ;; :appends              ;; bool true
         ;; :asmdecl              ;; bool true
         ;; :assign               ;; bool true
         ;; :atomic               ;; bool true
         ;; :atomicalign          ;; bool true
         ;; :bools                ;; bool true
         ;; :buildtag             ;; bool true
         ;; :cgocall              ;; bool true
         ;; :composites           ;; bool true
         ;; :copylocks            ;; bool true
         ;; :deepequalerrors      ;; bool true
         ;; :defers               ;; bool true
         ;; :deprecated           ;; bool true
         ;; :directive            ;; bool true
         ;; :embed                ;; bool true
         ;; :errorsas             ;; bool true
         ;; :fieldalignment       ;; bool false
         ;; :fillreturns          ;; bool true
         ;; :fillstruct           ;; bool true
         ;; :httpresponse         ;; bool true
         ;; :ifaceassert          ;; bool true
         ;; :infertypeargs        ;; bool true
         ;; :loopclosure          ;; bool true
         ;; :lostcancel           ;; bool true
         ;; :nilfunc              ;; bool true
         ;; :nilness              ;; bool true
         ;; :nonewvars            ;; bool true
         ;; :noresultvalues       ;; bool true
         ;; :printf               ;; bool true
         :shadow t                ;; bool false
         ;; :shift                ;; bool true
         ;; :simplifycompositelit ;; bool true
         ;; :simplifyrange        ;; bool true
         ;; :simplifyslice        ;; bool true
         ;; :slog                 ;; bool true
         ;; :sortslice            ;; bool true
         ;; :stdmethods           ;; bool true
         ;; :stringintconv        ;; bool true
         ;; :structtag            ;; bool true
         ;; :stubmethods          ;; bool true
         ;; :testinggoroutine     ;; bool true
         ;; :tests                ;; bool true
         ;; :timeformat           ;; bool true
         ;; :undeclaredname       ;; bool true
         ;; :unmarshal            ;; bool true
         ;; :unreachable          ;; bool true
         ;; :unsafeptr            ;; bool true
         :unusedparams t          ;; bool false
         ;; :unusedresult         ;; bool true
         :unusedvariable t        ;; bool false
         :unusedwrite t           ;; bool false
         :useany t                ;; bool false
         )

       :staticcheck t ;; bool            false

       ;; :annotations (
       ;; :bounds ;; bool true
       ;; :escape ;; bool true
       ;; :inline ;; bool true
       ;; :nil    ;; bool true
       ;; )

       ;; :vulncheck                 ;; "Imports"|"Off" "Off"
       :diagnosticsDelay "250ms"     ;; time.Duration   "1s"
       ;; :diagnosticsTrigger        ;; "Edit"|"Save"   "Edit"
       ;; :analysisProgressReporting ;; bool            true

       ;; DOCUMENTATION

       ;; :hoverKind    ;; "FullDocumentation"|"NoDocumentation"|"SingleLine"|"Structured"|"SynopsisDocumentation" "FullDocumentation"
       ;; :linkTarget   ;; "godoc.org"|"pkg.go.dev"|string                                                         "pkg.go.dev"
       :linksInHover :json-false ;; bool                                                                           true

       ;; INLAY HINTS

       :hints
       (
         :assignVariableTypes    t           ;; bool false
         :compositeLiteralFields t           ;; bool false
         :compositeLiteralTypes  t           ;; bool false
         :constantValues         t           ;; bool false
         :functionTypeParameters t           ;; bool false
         :parameterNames         :json-false ;; bool false
         :rangeVariableTypes     t           ;; bool false
         )

       ;; NAVIGATION

       ;; :importShortcut ;; "Both"|"Definition"|"Link"                            "Both"
       ;; :symbolMatcher  ;; "CaseInsensitive"|"CaseSensitive"|"FastFuzzy"|"Fuzzy" "FastFuzzy"
       ;; :symbolStyle    ;; "Dynamic"|"Full"|"Package"                            "Dynamic"
       ;; :symbolScope    ;; "all"|"workspace"                                     "all"
       ;; :verboseOutput  ;; bool                                                  false
       ;; :newDiff        ;; "both"|"old"|"new"                                    "both"
       ))
  (defun jf/eglot-managed-mode ()
    "Ensure `eglot-completion-at-point' preceeds everything."
    ;; I don't want `eglot-completion-at-point' to trample my other
    ;; completion options.
    ;;
    ;; https://stackoverflow.com/questions/72601990/how-to-show-suggestions-for-yasnippets-when-using-eglot
    (setq-local completion-at-point-functions
      (list (cape-capf-super
              #'eglot-completion-at-point
              #'tempel-expand)))
    ;; I could configure `eglot-stay-out-of' to skip fly-make; or I
    ;; could simply check here if I'm in `go-ts-mode' and if so
    ;; enable the linting `flymake-go-staticcheck-enable'
    (when (and (derived-mode-p 'go-ts-mode)
            (fboundp 'flymake-go-staticcheck-enable))
      (progn
        (flymake-go-staticcheck-enable)
        (flymake-start))))
  ;; https://github.com/elixir-lsp/elixir-ls?tab=readme-ov-file
  (add-to-list 'eglot-server-programs
    ;; By personal convention I'm having the most up to date version
    ;; added to the v0 directory
    '(elixir-ts-mode "~/elixir-ls/elixir-ls-v0.27.2/language_server.sh"))
  ;; https://github.com/emacs-lsp/lsp-mode/wiki/Install-Angular-Language-server
  ;; with modifications for homebrew
  (add-to-list 'eglot-server-programs
    '(angular-mode
       "node /opt/homebrew/lib/node_modules/@angular/language-server --ngProbeLocations /opt/homebrew/lib/node_modules --tsProbeLocations /opt/homebrew/lib/node_modules --stdio"))
  (add-to-list 'eglot-server-programs
    '(angular-ts-mode
       "node /opt/homebrew/lib/node_modules/@angular/language-server --ngProbeLocations /opt/homebrew/lib/node_modules --tsProbeLocations /opt/homebrew/lib/node_modules --stdio"))
  :hook ((eglot-managed-mode . jf/eglot-managed-mode)))

(add-hook 'go-ts-mode-hook 'flymake-mode 8)
;; (add-hook 'go-ts-mode-hook 'flymake-show-buffer-diagnostics 9)
;; (add-hook 'go-ts-mode-hook 'eglot-ensure 10)
(add-hook 'elixir-ts-mode-hook 'eglot-ensure 10)

;; Optional: install eglot-format-buffer as a save hook.
;; The depth of -10 places this before eglot's willSave notification,
;; so that that notification reports the actual contents that will be saved.
;; (defun eglot-format-buffer-on-save ())
;; (add-hook 'go-ts-mode-hook #'eglot-format-buffer-on-save)

;; See https://elixir-lsp.github.io/elixir-ls/getting-started/emacs/

;; (use-package eglot-booster
;;   :straight (:host github :repo "jdtsmith/eglot-booster")
;;   :after eglot
;;   :hook ((eglot-managed-mode . eglot-booster-mode))
;;   :config
;;   (advice-add 'eglot-completion-at-point
;;     :around #'cape-wrap-buster))

(use-package eglot-hierarchy
  :straight (:host github :repo "dolmens/eglot-hierarchy")
  :bind (:map eglot-mode-map ("H-e ." . eglot-hierarchy-call-hierarchy))
  :after eglot)

(use-package consult-eglot
  :after (consult eglot)
  :straight t)

(use-package consult-eglot-embark
  :straight t
  :config
  (with-eval-after-load 'embark
    (with-eval-after-load 'consult-eglot
      (require 'consult-eglot-embark)
      (consult-eglot-embark-mode))))

(use-package eldoc
  ;; Helps with rendering documentation
  ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
  :config
  (setopt eldoc-documentation-strategy
    ;; 'eldoc-documentation-enthusiast))
    'eldoc-documentation-compose-eagerly)
  (add-to-list 'display-buffer-alist
    '("^\\*eldoc"
       (display-buffer-reuse-mode-window
         display-buffer-below-selected)
       (dedicated . t)
       (body-function . prot-window-select-fit-size)))
  :straight t)

(use-package emacs
  :straight (:type built-in)
  :config
  (add-to-list 'save-some-buffers-action-alist
    (list "d"
      (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
      "show diff between the buffer and its file"))
  ;; From
  ;; https://emacs.dyerdwelling.family/emacs/20230414111409-emacs--indexing-emacs-init/
  ;;
  ;; Creating some outline modes.  Which has me thinking about an
  ;; outline mode for my agenda file.
  (defun jf/emacs-lisp-mode-configurator ()
    (setopt imenu-sort-function 'imenu--sort-by-name)
    (setq imenu-generic-expression
      '((nil "^;;[[:space:]]+-> \\(.*\\)$" 1)
         ("Variables"
           "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\|var-keymap\\)\\)\\s-+\\(\\(?:\\w\\|\\s_\\|\\\\.\\)+\\)"
           2)
         ("Variables"
           "^\\s-*(defvar\\(?:-local\\)?\\s-+\\(\\(?:\\w\\|\\s_\\|\\\\.\\)+\\)[[:space:]\n]+[^)]"
           1)
         ("Functions"
           "^[[:space:]]*([[:space:]]*\\(cl-\\)?defun[[:space:]]+\\([^(]+\\)" 2)
         ("Macros"
           "^[[:space:]]*([[:space:]]*\\(cl-\\)?defmacro[[:space:]]+\\([^(]+\\)" 2)
         ("Types"
           "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\w\\|\\s_\\|\\\\.\\)+\\)"
           2)
         ("Packages"
           "^.*([[:space:]]*use-package[[:space:]]+\\([[:word:]-]+\\)" 1)))
    (imenu-add-menubar-index))
  :hook (emacs-lisp-mode . jf/emacs-lisp-mode-configurator))

;; An odd little creature, hide all comment lines.  Sometimes this can
;; be a useful tool for viewing implementation details.
(require 'hide-comnt)

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
  (setopt projectile-create-missing-test-files t)
  (setopt projectile-git-command
    "git ls-files -zco --exclude-standard -- ':!vendor/' ':!pkg/'")
  (setopt projectile-git-fd-args
    "-H -0 -tf --strip-cwd-prefix -c never -E vendor/ -E pkg/ -E docs/ -E .git")
  (projectile-mode 1)
  ;; The default relevant `magit-list-repositories'
  ;; The following command shows all "project" directories
  (defvar jf/git-project-paths
    (mapcar (lambda (el) (cons el 1)) projectile-known-projects)
    "An alist of project directories.")

  (setopt magit-repository-directories jf/git-project-paths)
  (projectile-register-project-type 'go
    '("go.mod")
    :project-file "go.mod"
    :test-suffix "_test"
    :related-files-fn #'jf/projectile/go-related-files)

  (defun jf/projectile/go-related-files (path)
    (when (string-match "\\.go$" path)
      (if (s-ends-with? "_test.go" path)
        (list :impl (replace-regexp-in-string "_test\\.go$" ".go" path))
        (list :test (replace-regexp-in-string "\\.go$" "_test.go" path))))))

(use-package eplot :straight
  (:host github :repo "larsmagne/eplot"))


(use-package casual-suite
  :straight t
  :config
  (use-package casual-avy
    :straight t)
  (keymap-set calc-mode-map "H-c" #'casual-calc-tmenu)
  (keymap-set dired-mode-map "H-c" #'casual-dired-tmenu)
  (keymap-set isearch-mode-map "H-c" #'casual-isearch-tmenu)
  (keymap-set ibuffer-mode-map "H-c" #'casual-ibuffer-tmenu)
  (keymap-set ibuffer-mode-map "H-f" #'casual-ibuffer-filter-tmenu)
  (keymap-set ibuffer-mode-map "H-s" #'casual-ibuffer-sortby-tmenu)
  (keymap-set Info-mode-map "H-c" #'casual-info-tmenu)
  (keymap-set reb-mode-map "H-c" #'casual-re-builder-tmenu)
  (keymap-set reb-lisp-mode-map "H-c" #'casual-re-builder-tmenu)
  (keymap-set bookmark-bmenu-mode-map "H-c" #'casual-bookmarks-tmenu)
  (keymap-set org-agenda-mode-map "H-c" #'casual-agenda-tmenu)
  (keymap-set symbol-overlay-map "H-c" #'casual-symbol-overlay-tmenu)
  (keymap-global-set "H-c a" #'casual-avy-tmenu)
  (keymap-global-set "H-c e" #'casual-editkit-main-tmenu))

(use-package bookmark
  :straight (:type built-in)
  :config
  ;; On each machine I use, I have different bookmarks, yet they all
  ;; point to the same location.
  (setq bookmark-default-file "~/emacs-bookmarks.el")

  ;; Save the `bookmark-file' each time I modify a bookmark.
  (setq bookmark-save-flag 1)

  (defvar jf/bookmark-make-record-function/browse-url/title ""
    "Used for capturing the title from the browser current tab.")

  (defvar jf/bookmark-make-record-function/browse-url/url ""
    "Used for capturing the url from the browser current tab.")

  (defun jf/bookmark-url (url title)
    "Create a `browse-url' bookmark for URL and TITLE."
    (let* ((jf/bookmark-make-record-function/browse-url/title
             title)
            (jf/bookmark-make-record-function/browse-url/url
              url)
            (bookmark-make-record-function
              #'jf/bookmark-make-record-function/browse-url))
      (bookmark-set-internal nil title nil)))

  (defun jf/bookmark-make-record-function/browse-url()
    "Function to build the bookmark structure."
    `(,jf/bookmark-make-record-function/browse-url/title
       (location . ,jf/bookmark-make-record-function/browse-url/url)
       (handler . jf/browse-url-bookmark-jump)))

  (defun jf/browse-url-bookmark-jump (bookmark)
    "Default bookmark handler for browser."
    (browse-url (bookmark-prop-get bookmark 'location)))

  ;; In the bookmark list page, identify the type as BROWSE for those
  ;; captured by `jf/bookmark-url'
  (put 'jf/browse-url-bookmark-jump 'bookmark-handler-type "BROWSE"))

;; (use-package activities
;;   ;; https://takeonrules.com/2024/05/18/a-quiet-morning-of-practice-to-address-an-observed-personal-computering-workflow-snag/
;;   ;;
;;   ;; https://github.com/alphapapa/activities.el
;;   ;;
;;   ;; On <2024-05-17 Fri> while working to orient to a code-base, I was
;;   ;; reviewing several different files.  I found it helpful to have
;;   ;; those files open in a specific window configuration.
;;   ;;
;;   ;; I did this exploration in between a pairing session in which a
;;   ;; colleague stepped away to work on something and then came back.
;;   ;; When she came back, I needed to set down my windwo configuration
;;   ;; and work on the same code from a different perspective.
;;   ;;
;;   ;; It turns out "saving" that activity would have been useful; as I
;;   ;; could then come back to that window state and resume that work.
;;   ;;
;;   ;; The `activities' package provides that capability.
;;   :straight t
;;   :init
;;   (activities-mode)
;;   (activities-tabs-mode)
;;   :bind
;;   (("H-a n" . activities-new)
;;     ("H-a d" . activities-define)
;;     ("H-a D" . activities-discard)
;;     ("H-a a" . activities-resume)
;;     ("H-a z" . activities-suspend)
;;     ("H-a k" . activities-kill)
;;     ("H-a RET" . #'tab-bar-switch-to-tab)
;;     ("H-a S" . activities-save-all)
;;     ("H-a s" . activities-switch)
;;     ("H-a b" . activities-switch-buffer)
;;     ("H-a g" . activities-revert)
;;     ("H-a l" . activities-list)))

;; (use-package intuitive-tab-line
;;   :straight (:host github :repo "thread314/intuitive-tab-line-mode")
;;   :custom
;;   (tab-line-tabs-function 'intuitive-tab-line-buffers-list)
;;   (tab-line-switch-cycling t)
;;   :config
;;   (global-tab-line-mode 1)
;;   (recentf-mode 1)
;;   (setq
;;     tab-line-new-button-show nil  ;; do not show add-new button
;;     tab-line-close-button-show nil  ;; do not show close button
;;     tab-line-separator " "  ;; delimitation between tabs
;;     )


;;   ;; (global-set-key (kbd "C-<prior>") 'tab-line-switch-to-prev-tab)
;;   (global-set-key (kbd "s-{") 'tab-line-switch-to-prev-tab)
;;   ;; (global-set-key (kbd "C-<next>") 'tab-line-switch-to-next-tab)
;;   (global-set-key (kbd "s-}") 'tab-line-switch-to-next-tab)
;;   ;; (global-set-key (kbd "C-S-<prior>") 'intuitive-tab-line-shift-tab-left)
;;   ;; (global-set-key (kbd "C-S-<next>") 'intuitive-tab-line-shift-tab-right)
;;   (global-set-key (kbd "C-S-t") 'recentf-open-most-recent-file))

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
    `((mastodon-mode . "^🐘")
       (emacs-lisp-mode . "^;;;+ ")
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
  (setq olivetti-body-width 60)
  (setq olivetti-minimum-body-width 60)
  (setq olivetti-style 'fancy)
  (setq olivetti-recall-visual-line-mode-entry-state t)
  :config
  (defun jf/olivetti-mode-on-hook ()
    (text-scale-adjust 0)
    (text-scale-adjust 2)
    "Remove some visual chatter."
    (setq-local original-flymake-fringe-indicator-position
      flymake-fringe-indicator-position)
    (setq-local original-vi-tilde-fringe-mode
      vi-tilde-fringe-mode)
    (setq-local original-display-fill-column-indicator-mode
      display-fill-column-indicator-mode)
    (when (fboundp 'git-gutter-mode)
      (setq-local original-git-gutter-mode
        git-gutter-mode))
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
    (when (fboundp 'git-gutter-mode)
      (git-gutter-mode -1))
    )
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
    (when (fboundp 'git-gutter-mode)
      (git-gutter-mode
       original-git-gutter-mode))
    (setq-local org-modern-block-fringe
      original-org-modern-block-fringe))
  (defun jf/olivetti-mode (&rest args)
    ;; I want to turn off org-modern-mode as it's drawing of the
    ;; overlays conflicts with Olivetti.  We'll turn it on later.
    (when (eq major-mode 'org-mode)
      (org-modern-mode -1)))
  (advice-add 'olivetti-mode :before #'jf/olivetti-mode))

(setq jf/toot-prefix "🐘")

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


(use-package doc-view
  ;; A package for improving the in Emacs viewing experience of PDFs.
  :straight (doc-view :type built-in)
  :bind (:map doc-view-mode-map
          ("C-c g" . doc-view-goto-page)))

(use-package url-vars
  :straight (:type built-in)
  :custom
  (url-privacy-level '(email lastloc os emacs))
  (url-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:128.0) Gecko/20100101 Firefox/128.0"))

(use-package elfeed
  ;; An Emacs RSS reader.  I’ve used Google Reader, Feedly, Inoreader,
  ;; and Newsboat.  I wrote about
  ;; https://takeonrules.com/2020/04/12/switching-from-inoreader-to-newsboat-for-rss-reader/,
  ;; and the principles apply for Elfeed.
  :straight t
  ;; Without this, I was not seeing `rss' command.
  :demand 3
  :custom
  (shr-inhibit-images t)
  (elfeed-curl-timeout 90)
  (elfeed-db-directory "~/.elfeed")
  (elfeed-user-agent url-user-agent)
  :bind ((:map elfeed-search-mode-map
           (("/" . jf/elfeed-filter-by-ordinality)
             ("+" . jf/elfeed-search-tag-all)
             ("q" . jf/elfeed-save-db-and-bury)))
          (:map elfeed-show-mode-map
            (("+" . jf/elfeed-show-tag)
              ("-" . elfeed-show-untag))))
  :config
  (defun jf/elfeed-filter-by-ordinality ()
    "Select a default filter and update elfeed."
    (interactive)
    (let* ((filters
            '(("First (1st)" . "+1st +unread")
               ("Second (2nd)" . "@7-days-ago +2nd +unread")
               ("Third (3rd)" . "@7-days-ago +3rd +unread")
               ("Outlets" . "@2-days-ago +outlet")))
            (filter
              (completing-read "Elfeed Filter: " filters nil t)))
      (setq elfeed-search-filter
        (alist-get filter filters nil nil #'string=))
      (elfeed-search-update :force)))
  (defun jf/elfeed-search-tag-all ()
    "Apply TAG to all selected entries."
    (interactive)
    (elfeed-search-tag-all
      (intern (completing-read "Tag: "
                  (elfeed-db-get-all-tags)))))
  (defun jf/elfeed-show-tag ()
    "Add TAGS to the displayed entry."
    (interactive)
    (apply #'elfeed-show-tag
      (mapcar
        #'intern
        (completing-read-multiple "Tag(s): "
          (elfeed-db-get-all-tags)))))
  (setq elfeed-show-entry-switch #'jf/elfeed-show-entry-switch)
  ;; I want to keep pulling down this feed, but not have it at the front
  ;; of my reading
  (setq-default elfeed-search-filter "@2-days-ago +1st +unread")
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
  (defalias 'rss 'jf/elfeed-load-db-and-open
    "Fetch RSS Feed.")

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
            (cl-loop for org-file in `(,(denote-get-path-by-id "20110202T000001"))
              concat
              (with-temp-buffer
                (insert-file-contents
                  (expand-file-name org-file jf/denote-base-dir))
                (jf/elfeed-org-convert-org-to-opml
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

  (defvar jf/elfeed-blogroll-tag "blogroll"
    "The tag I give to RSS feed items that I'll share on my blogroll.")

  (defun jf/elfeed-org-convert-org-to-opml (org-buffer)
    "Convert Org buffer content to OPML format.
Argument ORG-BUFFER the buffer to write the OPML content to.

Modification of `rmh-elfeed-org-convert-org-to-opml'."
    (let (need-ends
           opml-body)
      (with-current-buffer org-buffer
        (let ((org-inhibit-startup t)
               (org-mode-hook nil))
          (org-mode))
        (org-element-map (rmh-elfeed-org-import-trees
                           rmh-elfeed-org-tree-id) 'headline
          (lambda (h)
            (let* ((current-level (org-element-property :level h))
                    (tags (org-element-property :tags h))
                    (heading (org-element-property :raw-value h))
                    (link-and-title (and (string-match "^\\[\\[\\(http.+?\\)\\]\\[\\(.+?\\)\\]\\]" heading)
                                      (list (match-string-no-properties 0 heading)
                                        (match-string-no-properties 1 heading)
                                        (match-string-no-properties 2 heading))))
                    (hyperlink (and (string-match "^\\[\\[\\(http.+?\\)\\]\\(?:\\[.+?\\]\\)?\\]" heading)
                                 (list (match-string-no-properties 0 heading)
                                   (match-string-no-properties 1 heading))))
                    url
                    title
                    opml-outline)
              ;; fill missing end outlines
              (while (and (car need-ends) (>= (car need-ends) current-level))
                (let* ((level (pop need-ends)))
                  (setq opml-body (concat opml-body (format "  %s</outline>\n"
                                                      (make-string (* 2 level) ? ))))))

              (cond ((string-prefix-p "http" heading)
                      (setq url heading)
                      (setq title (or (elfeed-feed-title (elfeed-db-get-feed heading)) "Unknown")))
                (link-and-title (setq url (nth 1 link-and-title))
                  (setq title (nth 2 link-and-title)))
                (hyperlink (setq url (nth 1 hyperlink))
                  (setq title (or (elfeed-feed-title (elfeed-db-get-feed (nth 1 hyperlink))) "Unknown")))
                (t (setq title heading)))
              (if url
                (setq opml-outline (format "  %s<outline title=\"%s\" xmlUrl=\"%s\"/>\n"
                                     (make-string (* 2 current-level) ? )
                                     (xml-escape-string title)
                                     (xml-escape-string url)))
                (unless (string-prefix-p "entry-title" heading)
                  (unless (member rmh-elfeed-org-tree-id tags)
                    ;; insert category title only when it is neither the top
                    ;; level elfeed node nor the entry-title node
                    (progn
                      (push current-level need-ends)
                      (setq opml-outline (format "  %s<outline title=\"%s\">\n"
                                           (make-string (* 2 current-level) ? )
                                           (xml-escape-string title)))))))
              (when (member jf/elfeed-blogroll-tag tags)
                (setq opml-body (concat opml-body opml-outline)))))))

      ;; fill missing end outlines at end
      (while (car need-ends)
        (let* ((level (pop need-ends)))
          (setq opml-body (concat opml-body (format "  %s</outline>\n"
                                              (make-string (* 2 level) ? ))))))
      opml-body))
  (setq rmh-elfeed-org-files nil)
  (dolist (file `(,(denote-get-path-by-id "20110202T000001")))
    (when (and file (f-exists? file))
      (add-to-list 'rmh-elfeed-org-files file))))

(use-package eww
  ;; A plain text browser.  Use this to see just how bad much of the web
  ;; has become.
  :straight t
  :custom (eww-auto-rename-buffer 'title)
  :config
  (setopt shr-cookie-policy nil)
  (defun shr-tag-dfn (dom)
    (shr-generic dom))

  (defun shr-tag-cite (dom)
    (shr-generic dom))

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

  (defmacro shr-display-block (tag &optional face)
    "Register TAG a paragraph (in CSS parlance \"display:block;\").

See https://developer.mozilla.org/en-US/docs/Glossary/Block-level_content"
    (let ((fname
            (intern (format "shr-tag-%s" tag)))
           (docstring
             (format "Render \"%s\" tag as paragraph." tag)))
      `(defun ,fname (dom)
         ,docstring
         (shr-ensure-paragraph)
         (shr-generic dom)
         (shr-ensure-paragraph))))

  (shr-display-block "article")
  (shr-display-block "aside")
  (shr-display-block "footer")
  (shr-display-block "header")
  (shr-display-block "nav")
  (shr-display-block "section")

  (defmacro jf/shr-facify (tag)
    "Create and apply the face to the given TAG."
    (let ((face
            (intern (concat "shr-" tag)))
           (docstring-face
            (format "Face for <%s> elements." tag)))
    `(progn
       (defface ,face
         '((default :inherit shr-text))
         ,docstring-face)
       (advice-add (intern (concat "shr-tag-" ,tag))
         :around
         (lambda (advised-function func &rest args)
           (let ((start (point)))
             (apply advised-function func args)
             (shr-add-font start (point) (intern (concat "shr-" ,tag)))))))))

  (jf/shr-facify "blockquote")
  (jf/shr-facify "aside")
  (jf/shr-facify "cite")
  (jf/shr-facify "dfn")
  (jf/shr-facify "dt")

  (defun eww-first-url ()
    "Go to the page marked `first'.
A page is marked `first' if rel=\"first\" appears in a <link> or <a> tag."
    (interactive nil eww-mode)
    (let ((best-url
            (plist-get eww-data :first)))
      (if best-url
        (eww-browse-url (shr-expand-url best-url (plist-get eww-data :url)))
        (user-error "No `first' for this page"))))

  (defun eww-last-url ()
    "Go to the page marked `last'.
A page is marked `last' if rel=\"last\" appears in a <link> or <a> tag."
    (interactive nil eww-mode)
    (let ((best-url
            (plist-get eww-data :last)))
      (if best-url
        (eww-browse-url (shr-expand-url best-url (plist-get eww-data :url)))
        (user-error "No `last' for this page"))))

  ;; Favor librewolf as default firefox browser, failing that mullvad.
  (if (executable-find "librewolf")
    (setopt browse-url-firefox-program "librewolf")
    (when (executable-find "mullvad-browser")
      (setopt browse-url-firefox-program "mullvad-browser")))

  (defun jf/reader-visual ()
    ;; A little bit of RSS beautification.
    "A method to turn on visual line mode and adjust text scale."
    (require 'olivetti)
    (olivetti-mode 1)
    (text-scale-set 2))
  (unbind-key "u" shr-map)
  :bind (:map eww-mode-map
          ("u" . eww-up-url)
          ("M-<left>" . eww-first-url)
          ("M-<right>" . eww-last-url)
          ("h" . eww-home-url))
  :bind (("C-s-o" . browse-url-at-point))
  :hook ((eww-mode . jf/reader-visual)))

;; Given that I have tools to grab results from the browser; it makes
;; sense to have tools to launch a search in my browser.
(use-package emacs-websearch
  :straight '(emacs-websearch :host github :repo "zhenhua-wang/emacs-websearch"))

(use-package stem-reading-mode
  ;; A package that emboldens word stems, helping read a bit faster.
  :straight t
  :custom (stem-reading-overlay t))

(use-package amread-mode
  ;; A speadreading package.
  :straight t
  :custom
  (amread-word-speed 9.5)
  (amread-scroll-style 'word)
  (amread-voice-reader-language 'english)
  :commands (amread-mode))

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
  :config
  ;; git-commit is a package shipped with transient.
  (require 'git-commit)
  (setopt git-commit-major-mode #'git-commit-ts-mode)
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
       "samvera-labs/bot_challenge_page"
       "samvera-labs/hyku_knapsack"
       "samvera/bulkrax"
       "samvera/maintenance"
       "samvera/hydra-works"
       "samvera/hydra-head"
       "samvera/hyku"
       "samvera/hyku-next"
       "samvera/valkyrie"
       "samvera/avalon-next"
       "notch8/actions"
       "notch8/adventist-dl"
       "notch8/adventist_knapsack"
       "notch8/atla-hyku"
       "notch8/britishlibrary"
       "notch8/derivative_rodeo"
       "notch8/hykuup_knapsack"
       "notch8/iiif_print"
       "notch8/palni-palci"
       "notch8/palni_palci_knapsack"
       "notch8/space_stone-serverless"
       "notch8/utk-hyku"
       "harvard-lts/CURIOSity"
       "WGBH-MLA/ams")))

;; (use-package git-commit
;;   :straight t
;;   :config
;;   )

(use-package git-commit-ts-mode
  :straight t
  :after (magit git-commit)
  :mode "\\COMMIT_MESSAGE\\'"
  :hook ((git-commit-ts-mode . jf/git-commit-mode-configurator))
  :bind (:map git-commit-ts-mode-map
          (("TAB" .  #'completion-at-point)))
  :bind ("s-7" . #'structured-commit/write-message)
  :config
  (defun jf/git-commit-mode-configurator ()
    "Prepare all of the commit buffer structure"
    (setq-local fill-column git-commit-fill-column)
    (goto-char (point-min))
    (beginning-of-line-text)
    (when (looking-at-p "^$")
      (structured-commit/write-message))))

(use-package structured-commit
  :straight (:type git :host github
              :repo "jeremyf/structured-commit")
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

(use-package diff-hl
  :config (global-diff-hl-mode)
  :straight t)
;; (use-package git-gutter
;;   ;; Show the current git state in the gutter.  As you edit a line in a
;;   ;; file track by git, the indicators change to reflect if this is a
;;   ;; modification, addition, or deletion.
;;   :straight t
;;   :custom (git-gutter:update-interval 0.25)
;;   :bind ("C-x g =" . git-gutter:popup-hunk)
;;   ("C-x g p" . git-gutter:previous-hunk)
;;   ("C-x g n" . git-gutter:next-hunk)
;;   :init (global-git-gutter-mode t)
;;   (setq git-gutter:modified-sign "%"
;;     git-gutter:added-sign "+"
;;     git-gutter:deleted-sign "-"))

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
  (setopt git-link-use-commit t) ;; URL will be SHA instead of branch
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
  (let ((query
          (format "TODO=\"DONE\"+CLOSED>=\"<%s>\""
            (format-time-string "%Y-01-01"))))
    (add-to-list 'org-agenda-custom-commands
      `("d" "Done this year"
         ((tags ,query))))
    (let ((query (concat "books+" query)))
      (add-to-list 'org-agenda-custom-commands
        `("b" "Books read this year"
           ((tags ,query))))))
  ;; https://sachachua.com/blog/2024/11/changing-org-mode-underlines-to-the-html-mark-element/
  (setf (alist-get 'underline org-html-text-markup-alist)
    "<mark>%s</mark>")
  ;; These are more of the recommended markup for HTML.  And leaves open
  ;; the I-tag for idiomatic.
  (setf (alist-get 'bold org-html-text-markup-alist)
    "<strong>%s</strong>")
  ;; This will allow me to skip the i macro.
  (setf (alist-get 'italic org-html-text-markup-alist)
    "<i class=\"dfn\">%s</i>")
  (use-package ox
    :straight (:type built-in)
    :config
    (defun jf/org-odt-verse-block (_verse-block contents _info)
      "Transcode a VERSE-BLOCK element from Org to ODT.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
      (let ((contents
              (caddr _verse-block)))
        (format "\n<text:p text:style-name=\"OrgVerse\">%s</text:p>"
          ;; Add line breaks to each line of verse.
          (s-join
            "<text:line-break/>"
            (s-split "\n"
              (replace-regexp-in-string
                ;; Replace leading tabs and spaces.
                "^[ \t]+" #'org-odt--encode-tabs-and-spaces
                contents))))))
    (advice-add #'org-odt-verse-block :override #'jf/org-odt-verse-block))
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
      ;; Rebuild the doc cache based on set values.
      (org-set-regexps-and-options)
      ;; Ensure that we have an ID property.
      (jf/bibliography/export-to-takeonrules)
      (with-current-buffer buffer
        (save-excursion
          (let* ((jf/exporting-org-to-tor
                   t)
                  (org-export-exclude-tags
                   '("noexport" "private"))
                  (export-global-plist
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
            (org-open-file (org-hugo-export-wim-to-md nil nil t))))))

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
                "\n#+HUGO_CUSTOM_FRONT_MATTER: :org_id " identifier
                "\n#+HUGO_TAGS: "
                (s-join " "
                  (mapcar
                    #'string-inflection-kebab-case-function
                    (seq-intersection
                      (org-get-tags)
                      denote-known-keywords #'string=)))))
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
      (search-forward-regexp "^$")
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
                   :keywords-regexp "\\(ROAM_REFS\\|DESCRIPTION\\|TITLE\\)"))
                (url
                  (lax-plist-get metadata "ROAM_REFS"))
                (title
                  (lax-plist-get metadata "TITLE"))
                (description
                  (lax-plist-get metadata "DESCRIPTION")))
          (call-interactively #'mastodon-toot)
          (end-of-buffer)
          (insert (s-join "\n\n"
                    (flatten-list
                      (list
                        (when title (format "“%s”" title))
                        description
                        url)))))
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
                    "\\(https?://takeonrules\.com[^ \n]*\\)")
                  nil t)
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
              (error "Unable to find Denote ID in buffer."))))))

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

    ;; (defun jf/tor-epigraph-list ()
    ;;   "Return a list of epigraph keys from TakeOnRules.com."
    ;;   (jf/tor-list-by-key-from-filename
    ;;     :key "key" :filename "data/epigraphs.yml"))

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
            ": //' | sort"))))

    (cl-defun jf/list-filenames-with-file-text (&key matching in)
      "Build list of filenames MATCHING pattern IN the given directory."
      (let ((default-directory
              (f-join jf/tor-home-directory in)))
        (split-string-and-unquote
          (shell-command-to-string
            (concat
              "rg \""
              matching "\" --only-matching --files-with-matches"
              " --sortr modified")))))

    (cl-defun jf/list-full-filenames-with-file-text (&key matching in
                                                      (switch "--files-with-matches"))
      "List filenames MATCHING with SWITCH pattern IN the directory."
      (split-string-and-unquote
        (shell-command-to-string
          (concat
            "rg \""
            matching "\" " in " --only-matching " switch
            " --sortr modified"))))

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
          (save-restriction
            (widen)
            (save-excursion
              (with-current-buffer (or buffer (current-buffer))
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
                    jf/denote-base-dir)))))

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
  (setopt org-noter-notes-search-path '())
  (dolist (path
            '("~/Documents/"))
    (when (f-dir-p path)
      ;; Add element to end of list.
      (add-to-list 'org-noter-notes-search-path path t))))

(use-package avy-embark-collect
  :after (avy embark)
  :straight t)

(require 'dig-my-grave)

(use-package insert-random
  :straight t)

(use-package server
  :straight (:type built-in)
  :hook (server-visit . server-visit-hook-custom-find)
  :config
  (setopt server-client-instructions nil)
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
(add-hook 'after-init-hook #'global-display-line-numbers-mode)

(use-package org
  ;; For projects and all
  :straight (org :source org-elpa)
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
              (-map #'read
                (cdar (org-collect-keywords '("PROJECT_PATHS"))))))
        (setq paths (cons (cons "Notes" filename) paths)))))

  (cl-defun jf/project/list-projects (&key (project ".+")
                                       (directory jf/denote-base-dir))
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
            "fd _projects " directory " | xargs rg \"^#\\+PROJECT_NAME: +(" project ") *$\" "
            " --follow --only-matching --no-ignore-vcs --with-filename "
            "-r '$1' | tr '\n' '#'"))
        "#")))

  (cl-defun jf/project/get-project/project-source-code (&key (directory jf/denote-base-dir))
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
  :straight (:host github :repo "magit/transient")
  :config
  (transient-define-suffix jf/shr/toggle-images ()
    "Toggle showing or hiding images"
    :description (lambda ()
                   (format "Show SHR Images (%s)"
                     (if shr-inhibit-images " " "*")))
    (interactive)
    (setopt shr-inhibit-images (not shr-inhibit-images)))

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
    (setopt org-agenda-files (jf/org-mode/agenda-files))
    (when (symbolp 'consult-notes-org-headings-files)
      (setopt consult-notes-org-headings-files org-agenda-files)))
  (add-hook 'after-init-hook #'jf/org-mode/agenda-files-update)
  ;; When I add or remove a tag, this could trigger an agenda update.
  (add-hook 'denote-after-rename-file-hook #'jf/org-mode/agenda-files-update)
  ;; When I'm adding and removing tags, I may very well wish to have an
  ;; updated org-mode document.  The `org-set-regexps-and-options'
  ;; rebuilds some of that cache.
  (add-hook 'denote-after-rename-file-hook #'org-set-regexps-and-options)

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
       ;; ("j c" "Capture Backlog" jf/jump-to/code-backlog)
       ("j d" "Denote File" jf/jump_to_corresponding_denote_file
         :if-derived markdown-mode)
       ("j g" "Global Mark" consult-global-mark)
       ("j h" "Hugo File" jf/jump_to_corresponding_hugo_file
         :if-derived org-mode)
       ("j m" "Mark" consult-mark)
       ]
      ["Tasks"
        ("n" "Github Notifications…" gh-notify)
        ("C-t" "Start a timer…" tmr-with-details)
        ("u" jf/org-mode/agenda-files-update)
        ]
      ["Denote"
        ("d a" jf/project/add-project-path :if jf/denote?)
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
       ("m i" jf/shr/toggle-images)
       ;; I find that in all of my shuffling that sometimes the TAB for
       ;; command gets lost.  This is my "Yup that happens and here's
       ;; how to make that unhappen."
       ("TAB" jf/enable-indent-for-tab-command)
       ]
      ["Grab Refs"
        ("g e" "Elfeed" jf/capture/denote/from/elfeed-show-entry
          :if-derived elfeed-show-mode)
        ("g c" "Chrome" jf/menu--org-capture-chrome)
        ("g f" "Firefox" jf/menu--org-capture-firefox)
        ("g s" "Safari" jf/menu--org-capture-safari)
        ("g w" "Eww" jf/capture/denote/from/eww-data
          :if-derived eww-mode)
        ]
      ["Bookmark"
        ("b b" "Bookmarks" bookmark-bmenu-list)
        ("b c" "Chrome" jf/menu--bookmark-chrome)
        ("b f" "Firefox" jf/menu--bookmark-firefox)
        ("b s" "Safari" jf/menu--bookmark-safari)
        ]])
  :bind ("s-1" . #'jf/menu))

(when (f-file-p "~/git/random-table.el/random-table.el")
  (progn
    (load "~/git/random-table.el/random-table.el")
    (load "~/git/dotemacs/emacs.d/random-tables-data.el")))

(with-eval-after-load 'org
  (use-package edraw-org
    :straight (edraw-org :host github :repo "misohena/el-easydraw")
    :config (edraw-org-setup-default)))

;; When using the org-export-in-background option (when using the
;; asynchronous export function), the following settings are
;; required. This is because Emacs started in a separate process does
;; not load org.el but only ox.el.
(with-eval-after-load "ox"
  (require 'edraw-org)
  (edraw-org-setup-exporter))

(if (file-exists-p (expand-file-name "~/.my-computer"))
  (load "personal.el")
  (load "work.el"))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(safe-local-variable-values
     '((projectile-git-fd-args .
         "-H -0 -tf --strip-cwd-prefix -c never -E vendor/ -E pkg/ -E docs/ -E .git")
        (projectile-git-command .
          "git ls-files -zco --exclude-from=.projectile.gitignore")
        )))

(provide 'init)
;;; init.el ends here
