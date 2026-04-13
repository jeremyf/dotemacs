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
(add-to-list 'load-path "~/git/dotemacs/emacs.d/modules")

;; Let's just shunt those custom files into oblivion.
(setopt custom-file (make-temp-file "emacs-custom-"))
(load custom-file :noerror)

;; When you open Emacs, grab all the space on the screen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

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
          ("M-]" . #'forward-paragraph)
          ("M-<up>" . #'beginning-of-defun)
          ("M-<down>" . #'end-of-defun)
          ("M-C-t" . #'transpose-sentences)
          ("C-k" . #'jf/kill-line-or-region)
          ("C-c n" . #'jf/yank-file-name-to-clipboard) ;; Deprecated
          ("C-c y n" . #'jf/yank-file-name-to-clipboard)
          ("M-<delete>" . #'kill-word)
          ;; ("s-<down>" . #'end-of-buffer)
          ;; ("s-<up>" . #'beginning-of-buffer)
          ;; ("s-q" . #'save-buffers-kill-terminal)
          ;; ("s-w" . #'kill-current-buffer)
          ;; ("s-i" . #'ibuffer)
          ("M-RET" . #'newline-and-indent)
          ("<f12>" . scratch-buffer))
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
    kill-ring-max 512

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

(use-package project
  :straight (:type built-in))

(use-package ibuffer
  :straight (:type built-in)
  :custom (ibuffer-formats
      '((mark modified read-only " "
              (name 40 40 :left :elide) ; Change '30 30' to your preferred width
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " " (name 16 -1) " " filename))))

(use-package ibuffer-vc
  :straight t
  :after (ibuffer)
  :config
  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-vc-set-filter-groups-by-vc-root)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))))

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

  (defun jf/linux:xmodmap ()
    "Rebind xmodmap."
    (interactive)
    (shell-command "xmodmap ~/.Xmodmap"))

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

  (bind-key "C-c C-a" 'mark-whole-buffer))

(require 'setup-completion)

(when (eq system-type 'darwin)
  (progn
    (bind-key "s-w" 'jf/bury-or-unbury-buffer)
    (bind-key "s-p" 'consult-projectile)
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

    (setopt
      ns-function-modifier 'hyper
      ns-control-modifier 'control
      ns-right-control-modifier 'control
      ns-command-modifier 'super
      ns-right-command-modifier 'hyper
      ns-alternate-modifier 'meta
      ns-right-alternate-modifier 'meta)))

(defmacro jf/grab-browser-links (browser config)
  "For the given BROWSER and CONFIG define helper commands.

Interacting with the current tab/page of the BROWSER we can:
- grab the URL and page title as a link
- bookmark the URL with page title for description
- download a copy of the contents for the URL."
  (when-let* ((os-config (alist-get system-type config)))
    (when-let* ((grabber (plist-get os-config :grabber)))
      ;; As the grabber is a macro, we need to `eval' it instead of
      ;; `apply'.
      (eval grabber))
    (let* ((browser (downcase browser))
            (bmk-fn
              (intern (format "jf/menu--bookmark-%s" browser)))
            (bmk-doc
              (format "Create `bookmark' for current %s page." browser))
            (ref-fn
              (intern (format "jf/menu--org-capture-%s" browser)))
            (ref-doc
              (format "Create an `denote' entry from %s page." browser))
            (predicate-fn
              (intern (format "jf/browser-%s?" browser)))
            (predicate-var
              (intern (format "jf/browser-%s-p" browser)))
            (predicate-var-doc
              (format "Non-nil when %s browser is installed." browser))
            (predicate-fn-doc
              (format "When %s browser is installed 1, else -1." browser))
            (browser-finder-fn
              (plist-get os-config :when))
            (grabber-fn
              (if (eq system-type 'darwin)
                (progn
                  (require 'grab-mac-link)
                  (intern (format "grab-mac-link-%s-1" browser)))
                (progn
                  (require 'grab-x-link)
                  (intern (format "grab-x-link-%s" browser))))))
      `(progn
         (defun ,bmk-fn ()
           ,bmk-doc
           (interactive)
           (let* ((url-and-title
                    (,grabber-fn))
                   (url
                     (car url-and-title))
                   (title
                     (read-string
                       (concat "URL: " url "\nTitle: ")
                       (cdr url-and-title))))
             (jf/bookmark-url url title)))
         (defvar ,predicate-var nil ,predicate-var-doc)
         (defun ,predicate-fn ()
           ,predicate-fn-doc
           (if ,predicate-var
             (= ,predicate-var 1)
             (if-let* ((installed ,browser-finder-fn))
               (progn
                 (setf ,predicate-var 1)
                 t)
               (progn
                 (setf ,predicate-var -1)
                 nil))))
         (defun ,ref-fn ()
           ,ref-doc
           (interactive)
           (jf/denote/capture-reference :url (car (,grabber-fn))))))))

(jf/grab-browser-links "vivaldi"
  ((gnu/linux .
     (:when (executable-find "vivaldi")
       :grabber (grab-x-link:register-app
                  :menu-key ?v
                  :menu-label "[v]ivaldi"
                  :name "Vivaldi"
                  :classname "Vivaldi-stable"
                  :key "ctrl+l Escape ctrl+l ctrl+c Escape"
                  :suffix " - Vivaldi")))
     (darwin . (:when (f-dir? "/Applications/Vivaldi.app")))))
(jf/grab-browser-links "safari"
  ((gnu/linux . (:when (executable-find "safari")))
    (darwin . (:when (f-dir? "/Applications/Safari.app")))))
(jf/grab-browser-links "firefox"
  ((gnu/linux . (:when (executable-find "firefox")
                  :grabber (grab-x-link:register-app
                             :menu-key ?f
                             :menu-label "[f]irefox"
                             :name "Firefox"
                             :classname "Navigator"
                             :key "ctrl+l Escape ctrl+l ctrl+c Escape"
                             :suffix " - Firefox")))
    (darwin . (:when (f-dir? "/Applications/Firefox.app")))))
(jf/grab-browser-links "librewolf"
  ((gnu/linux . (:when (executable-find "librewolf")
                  :grabber (grab-x-link:register-app
                             :menu-key ?l
                             :menu-label "[l]ibrewolf"
                             :name "Librewolf"
                             :classname "Navigator"
                             :key "ctrl+l Escape ctrl+l ctrl+c Escape"
                             :suffix " — LibreWolf")))
    (darwin . (f-dir? "/Applications/Librewolf.app"))))
(jf/grab-browser-links "mullvad"
  ((gnu/linux . (:when (executable-find "mullvad-browser")
                  :grabber (grab-x-link:register-app
                             :menu-key ?m
                             :menu-label "[m]ullvad"
                             :name "Mullvad"
                             :classname "Navigator"
                             :key "ctrl+l Escape ctrl+l ctrl+c Escape"
                             :suffix " - Mullvad Browser")))
    (darwin . (:when (f-dir? "/Applications/Mullvad Browser.app")))))
(jf/grab-browser-links "chrome"
  ((gnu/linux . (:when (executable-find "chrome")))
    (darwin . (:when (f-dir? "/Applications/Google Chrome.app")))))

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
  :hook (dired-mode . denote-dired-mode)
  (org-mode . denote-rename-buffer-mode)
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
  (setopt denote-org-store-link-to-heading 'id)
  (setopt denote-rename-buffer-format "%D%b")
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
    (when-let* ((filename (denote-get-path-by-id identifier)))
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
    (when-let* ((filename (denote-get-path-by-id identifier)))
      (when (string= (file-name-extension filename) "org")
        (with-current-buffer (find-file-noselect filename)
          (org-collect-keywords keywords)))))

  (cl-defun jf/org-link-complete-link-for (parg &key
                                            property
                                            scheme
                                            (file jf/filename/glossary))
    "Prompt for glossary entry in FILE with PROPERTY

    Returns a string of format: \"SCHEME:<id>\" where <id> is
    an `denote' identifier linking to a custom header.

PARG is part of the method signature for `org-link-parameters'."
    (let* ((buffer
             (find-file-noselect file))
            (property (or property (upcase scheme)))
            (entries
              (with-current-buffer buffer
                (save-restriction
                  (widen)
                  (org-map-entries
                    (lambda()
                      (let* ((entry
                               (org-element-at-point))
                              (value
                                (org-entry-get
                                  entry (upcase property))))
                        (cons
                          (format "%s [%s]"
                            (org-element-property :title entry)
                            value)
                          (org-entry-get entry "CUSTOM_ID"))))
                    (format "+glossary+%s={.+}" (upcase property))
                    'file))))
            (selection
              (completing-read (format "Link to %s:" property)
                entries))
            (custom_id
              (alist-get selection entries nil nil #'string=)))
      ;; TODO: kill the correct value so it can be applied to the
      ;; description.
      (format "%s:%s::#%s" scheme
        (denote-retrieve-filename-identifier file)
        custom_id)))

  (org-link-set-parameters "glossary"
    :complete (lambda (&optional parg)
                (jf/org-link-complete-link-for
                  parg
                  :scheme "denote"
                  :property "GLOSSARY_KEY")))

  (org-link-set-parameters "abbr"
    :insert-description #'jf/org-link-description/glossary
    :complete (lambda (&optional parg)
                (jf/org-link-complete-link-for
                  parg
                  :scheme "abbr"
                  :property "ABBR"))
    :export (lambda (link description format channel)
              (jf/denote/link-ol-abbr-with-property
                link description format channel
                :keyword "ABBR"
                :additional-hugo-parameters "abbr=\"t\""))
    :face #'jf/org-faces-abbr
    :follow #'denote-link-ol-follow
    )

  (defun jf/org-link-description/glossary (link desc)
    "Provide default DESC for a glossary LINK.

Types can be: abbr, abbr-plural, and denote"
    (if desc
      desc
      (let* ((_match
               (string-match "^\\([^:]+\\):\\([^:]+\\)\\(::#\\(.*\\)\\)?"
                 link))
              (type
                (match-string 1 link))
              (denote-identifier
                (match-string 2 link))
              (custom_id
                (match-string 4 link))
              (entry
                (car (org-map-entries
                       #'org-element-at-point
                       (format "CUSTOM_ID=\"%s\"" custom_id)
                       (list (denote-get-path-by-id denote-identifier))))))
        (if entry
          (pcase type
            ("abbr"
              (or (org-entry-get entry "ABBR")
                (org-element-property :title entry)))
            ("abbr-plural"
              (or (org-entry-get entry "PLURAL_ABBR")
                (org-element-property :title entry)))
            ("denote"
              (org-element-property :title entry)))
          nil))))

  (org-link-set-parameters "abbr-plural"
    :insert-description #'jf/org-link-description/glossary
    :complete (lambda (&optional parg)
                (jf/org-link-complete-link-for
                  parg
                  :scheme "abbr-plural"
                  :property "PLURAL_ABBR"))
    :export (lambda (link description format channel)
              (jf/denote/link-ol-abbr-with-property
                link description format channel
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

  (cl-defun jf/denote/link-export-date (link description format channel)
    "Export a date for given LINK, DESCRIPTION, FORMAT, and CHANNEL."
    (cond
      ((or (eq format 'html) (eq format 'md) (eq format 'takeonrules))
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
    (jf/works/populate t))

  (org-link-set-parameters "work"
    :insert-description #'jf/org-link-description/work
    :follow #'jf/org-link-ol-follow/work
    :complete #'jf/org-link-ol-complete/work
    :export #'jf/org-link-ol-export/work
    :face #'jf/org-faces-work)

  (defun jf/org-link-description/work (link desc)
    "Provide default DESC for a work LINK."
    (if desc
      desc
      (let* ((slugs
               (s-split "::" link))
              (custom-id
                (cadr (s-split ":" (car slugs))))
              (include-author
                (member "author" slugs))
              (include-subtitle
                (member "subtitle" slugs))
              (include-translator
                (member "translator" slugs))
              (book-label
                (car (org-map-entries
                       (lambda ()
                         (jf/book-make-label
                           :title (org-element-property
                                    :title (org-element-at-point))
                           :subtitle (when include-subtitle
                                       (org-entry-get
                                         (org-element-at-point) "SUBTITLE"))
                           :author (when include-author
                                     (org-entry-get
                                       (org-element-at-point) "AUTHOR"))
                           :translator (when include-translator
                                         (org-entry-get
                                           (org-element-at-point) "TRANSLATOR"))))
                       (format "CUSTOM_ID=\"%s\"" custom-id)
                       `(,jf/filename/bibliography)))))
        book-label)))

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

  (defun jf/org-link-ol-complete/work ()
    "Prompt for a work from my bibliography"
    (interactive)
    (let* ((works
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
                           (org-entry-get headline "AUTHOR"))
                         (translator
                           (org-entry-get headline "TRANSLATOR")))
                   (cons
                     (jf/book-make-label
                       :title title :subtitle subtitle
                       :author author :translator translator)
                     (list
                       :id (org-entry-get headline "CUSTOM_ID")
                       :title title
                       :subtitle subtitle
                       :author author
                       :translator translator))))
               "+LEVEL=2+!people"
               (list jf/filename/bibliography)))
            (work
              (completing-read "Citable: " works nil t)))
      (when-let* ((work-data
                    (alist-get work works nil nil #'string=)))
        (let* ((include-author
                 (and (plist-get work-data :author)
                   (yes-or-no-p "Include Author: ")))
                (include-subtitle
                  (and (plist-get work-data :subtitle)
                    (yes-or-no-p "Include Subtitle: ")))
                (include-translator
                  (and (plist-get work-data :translator)
                    (yes-or-no-p "Include translator: "))))
          (format "work:%s%s%s%s"
            (plist-get work-data :id)
            (if include-author "::author" "")
            (if include-subtitle "::subtitle" "")
            (if include-translator "::translator" ""))))))

  (defvar jf/works/cache
    (make-hash-table :test 'equal)
    "A cache of all works ready for exporting.

See `jf/works/populate'.")

  (defun jf/org-link-ol-export/work (link description format channel)
    "Export the text of the LINK work in the corresponding FORMAT.

We ignore the DESCRIPTION and probably the CHANNEL."
    (let* ((link-with-properties
             (s-split "::" link))
            (link
              (car link-with-properties))
            (with-author
              (member "author" link-with-properties))
            (with-subtitle
              (member "subtitle" link-with-properties))
            (with-translator
              (member "translator" link-with-properties))
            (works
              (jf/works/populate)))
      (when-let* ((work
                    (gethash link works)))
        (let ((author-suffix
                (if (and
                      with-author
                      (s-present? (plist-get work :author)))
                  (format " by %s" (plist-get work :author))
                  ""))
               (translator-suffix
                 (if (and
                       with-translator
                       (s-present? (plist-get work :translator)))
                   (format " translated by %s" (plist-get work :translator))
                   "")))
          ;; Then we create the corresponding format.
          (cond
            ((or (eq format 'html) (eq format 'md))
              (format "<cite data-id=\"%s\">%s</cite>%s%s"
                link
                (if-let* ((url
                            (plist-get work :url)))
                  (format "<a href=\"%s\">%s</a>"
                    url (plist-get work :title))
                  (plist-get work :title))
                author-suffix
                translator-suffix))
            ((eq format 'latex)
              (format "\\textit{%s}%s%s"
                (if-let* ((url
                            (plist-get work :url)))
                  (format "\\href{%s}{%s}"
                    url (plist-get work :title))
                  (plist-get work :title))
                author-suffix
                translator-suffix))
            ((eq format 'odt)
              (format
                "<text:span text:style-name=\"%s\">%s</text:span>%s%s"
                "Emphasis"
                (if-let* ((url
                            (plist-get work :url)))
                  (format "<text:a xlink:type=\"simple\" xlink:href=\"%s\">%s</text:a>"
                    url (plist-get work :title))
                  (plist-get work :title))
                author-suffix
                translator-suffix))
            (t
              (format "“%s”%s%s"
                (plist-get work :title)
                author-suffix
                translator-suffixq)))))))

  (defun jf/works/populate (&optional clear-cache)
    "Populates and returns the `jf/works/cache'.

When CLEAR-CACHE is non-nil, clobber the cache and rebuild."
    (when clear-cache (clrhash jf/works/cache))
    (when (hash-table-empty-p jf/works/cache)
      (message "Rebuilding `jf/works/cache'...")
      (org-map-entries
        (lambda ()
          (when-let* ((el
                        (org-element-at-point))
                       ;; Skip un-named blocks as we can’t link to
                       ;; them.
                       (id
                         (org-entry-get el "CUSTOM_ID")))
            (puthash id
              (list
                :title
                (org-element-property :title el)
                :subtitle
                (org-entry-get el "SUBTITLE")
                :author
                (org-entry-get el "AUTHOR")
                :translator
                (org-entry-get el "TRANSLATOR")
                :url
                (org-entry-get el "ROAM_REFS"))
              jf/works/cache)))
        (concat "+level=2+works")
        `(,jf/filename/bibliography)))
    jf/works/cache)

  (org-link-set-parameters "elfeed"
    :follow #'elfeed-link-open
    :store #'elfeed-link-store-link
    :export #'elfeed-link-export-link)

  (defun elfeed-link-export-link (link desc format _channel)
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
            (when-let* ((new-type
                          (completing-read "New link type: "
                            types nil t)))
              (if-let* ((new-text
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
        (concat "#+ROAM_REFS: " url "\n\n" article)))))

(with-eval-after-load 'denote-org
  (let ((config
          (alist-get "denote" org-link-parameters nil nil #'string=)))
    (plist-put config :insert-description
      #'jf/org-link-description/glossary)
    (org-link-set-parameters "denote" config)))

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
  (let* ((entry
           (if (s-contains? "::#" link)
             (let ((custom_id
                     (car (last (s-split "::#" link)))))
               (with-current-buffer (find-file-noselect jf/filename/glossary)
                 (seq-first
                   (org-map-entries
                     #'org-element-at-point
                     (format
                       "CUSTOM_ID=\"%s\""
                       custom_id)
                     'file))))
             (user-error "Attempting to process non-conformant ABBR link %S" link)))
          (title
            (org-element-property :title entry))
          (keyword-value
            (org-entry-get entry keyword))
          (description
            (org-entry-get entry "DESCRIPTION"))
          (key
            (org-entry-get entry "CUSTOM_ID")))
    ;; When we encounter an abbreviation, add that to the list.  We'll
    ;; later use that list to build a localized abbreviation element.
    (let ((abbr-links
            (plist-get info :abbr-links)))
      (unless (alist-get keyword-value abbr-links nil nil #'string=)
        (progn
          ;; TODO Also add description.
          (add-to-list 'abbr-links
            `(:abbr ,keyword-value
               :title ,title
               :description ,description))
          (plist-put info :abbr-links abbr-links))))
    (cond
      ((or (eq format 'html) (eq format 'md))
        (if (and (boundp 'jf/exporting-org-to-tor)
              jf/exporting-org-to-tor)
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
(require 'denote)

(defvar jf/denote-base-dir
  (file-truename
    (if (file-exists-p (expand-file-name "~/.my-computer"))
      "~/git/org/denote/" "~/Documents/denote/"))
  "Where I put my notes; I need to provision differently for personal and
work computers.")

(use-package files
  :straight (:type built-in)
  :custom (make-backup-files nil)
  :config
  (add-hook 'after-save-hook
    #'executable-make-buffer-file-executable-if-script-p))

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
  (defun jf/recentf-keeper (file)
    (and
      (not
        (s-starts-with? (file-truename elfeed-db-directory)
          (file-truename file)))
      (not
        (s-starts-with? (file-truename "~/Maildir/")
          (file-truename file)))))
  (setopt recentf-keep '(jf/recentf-keeper))
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
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  (text-mode . display-fill-column-indicator-mode))

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
  ;; (consult-after-jump . pulsar-recenter-top)
  (consult-after-jump . pulsar-reveal-entry)
  ;; integration with the built-in `imenu':
  ;; (imenu-after-jump . pulsar-recenter-top)
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

(use-package timeclock
  :straight (:host github :repo "bunnylushington/timeclock")
  :init
  (setq timeclock/db-file
    (expand-file-name "timeclock.db" user-emacs-directory)))

(defun jf/timeclock/punch-in (fn)
  (apply (list fn nil 0 "")))
(advice-add #'timeclock/punch-in :around #'jf/timeclock/punch-in)

(use-package emacs
  :straight (:type built-in)
  :after (projectile)
  :preface
  (defvar-local jf/mode-line-format/playing
    '(:eval
       (when (and (boundp 'playing-a-game)
               playing-a-game
               (mode-line-window-selected-p))
         (concat
           (propertize " ⚅ " 'face 'mode-line-highlight) " "))))
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
            (if (fboundp 'nerd-icons-icon-for-mode)
              (nerd-icons-icon-for-mode major-mode)
              (cond
                ((derived-mode-p 'text-mode) "¶")
                ((derived-mode-p 'prog-mode) "λ")
                ((derived-mode-p 'comint-mode) ">_")
                (t "◦")))))
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
        #'jf/toggle-typo-mode)
      map)
    "Keymap to display `typo-mode'.")

  (defun jf/mode-line-format/major-mode-name ()
    "Render the major mode as text.

When `typo-mode' is active, provide an actionable indicator of its
active nature."
    (let ((fmt
            (if (and (boundp 'typo-mode) typo-mode) "“%s”" "%s")))
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
        (concat "mouse-1: #'jf/toggle-typo-mode"))))

  (defvar-local jf/mode-line-format/major-mode
    '(:eval
       (concat
         (jf/mode-line-format/major-mode-indicator)
         " "
         (jf/mode-line-format/major-mode-name))))

  (defun jf/mode-line-indicator (nerd-fn nerd-icon fallback face &optional text)
    (concat
      (propertize
        (format " %s%s "
          (if (fboundp nerd-fn)
            (funcall nerd-fn nerd-icon)
            fallback)
          (or text ""))
        'face face) " "))

  (defvar-local jf/mode-line-format/narrow
    '(:eval
       (when (and (mode-line-window-selected-p)
               (buffer-narrowed-p)
               (not (derived-mode-p
                      'Info-mode
                      'help-mode
                      'special-mode
                      'message-mode)))
         (jf/mode-line-indicator
           'nerd-icons-mdicon "nf-md-filter" "⊆" 'mode-line-highlight))))

  (defvar-local jf/mode-line-format/timeclock
    '(:eval
       (when
         (and
           (fboundp #'timeclock/active-task-name)
           (mode-line-window-selected-p)
           (not (derived-mode-p
                  'Info-mode
                  'help-mode
                  'special-mode
                  'message-mode)))
         (when-let* ((task
                       (timeclock/active-task-name)))
           (jf/mode-line-indicator
             'nerd-icons-mdicon "nf-md-book_clock"
             "↻" 'mode-line-highlight (format "[%s]" task))))))

  (defvar-local jf/mode-line-format/org-clock
    '(:eval
       (when
         (and
           (mode-line-window-selected-p)
           (boundp 'org-clock-current-task)
           org-clock-current-task
           (not (derived-mode-p
                  'Info-mode
                  'help-mode
                  'special-mode
                  'message-mode)))
         ;; The symbol that most looks like an analogue stop watch.
         (jf/mode-line-indicator
           'nerd-icons-mdicon "nf-md-clock_outline"
           "⨶" 'mode-line-highlight))))

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
         (when-let* ((func (which-function)))
           (propertize
             (concat "f(): " func)
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
    (when-let* ((rev (vc-working-revision file backend))
                 (branch (or (vc-git--symbolic-ref file)
                           (substring rev 0 7))))
      branch))

  (defvar-local jf/mode-line-format/flymake
    '(:eval
      (when (and (boundp 'flymake--state)
                 flymake--state
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
                        jf/mode-line-format/playing
                        jf/mode-line-format/major-mode
                        jf/mode-line-format/misc-info
                        jf/mode-line-format/narrow
                        jf/mode-line-format/org-clock
                        jf/mode-line-format/timeclock
                        jf/mode-line-format/project
                        jf/mode-line-format/vc-branch
                        jf/mode-line-format/vterm
                        jf/mode-line-format/which-function
                        ))
    (put construct 'risky-local-variable t))

  (setq-default mode-line-format
    '("%e" " "
       jf/mode-line-format/timeclock
       jf/mode-line-format/org-clock
       jf/mode-line-format/vterm
       jf/mode-line-format/kbd-macro
       jf/mode-line-format/narrow
       jf/mode-line-format/playing
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
  :bind (("M-o" . ace-window)))

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
  (setopt avy-timeout-seconds 1.0)
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

;; Drawing inspiration from
;; https://github.com/bunnylushington/ace-kill/blob/main/ace-kill.el
(link-hint-define-type 'coding-identifiers
  :next #'link-hint--next-coding-identifiers
  :at-point-p #'link-hint--coding-identifiers-at-point-p
  :copy #'kill-new)

(push 'link-hint-coding-identifiers link-hint-types)

(defun link-hint--next-coding-identifiers (bound)
  "Find a coding identifier within the given BOUND."
  (link-hint--next-regexp thing-at-point-uuid-regexp bound))

(defun link-hint--coding-identifiers-at-point-p ()
  "Return the coding identifier at point.

Returns nil when point is not on a coding identifier."
  (save-excursion
    (when-let* ((begin
                  (point))
                 (end
                   (re-search-forward
                     thing-at-point-uuid-regexp
                     (point-max) t)))
      (buffer-substring-no-properties begin end))))

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

(use-package nerd-icons-dired
  :straight t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package typo
  :straight t
  :commands (typo-mode)
  :hook ((org-mode markdown-mode) . typo-mode)
  :config
  (define-typo-cycle typo-cycle-dashes
    "Cycle through various dashes."
    ("-" ; HYPHEN-MINUS
      "–" ; EN DASH
      "—" ; EM DASH
      "―" ; QUOTE-Dash
      "−" ; MINUS SIGN
      "‐" ; HYPHEN
      "‑" ; NON-BREAKING HYPHEN
      ))
  (define-typo-cycle typo-cycle-multiplication-signs
    "Cycle through the asterisk and various multiplication signs"
    ("*" "×" "·"))
  ;; I definitely don’t want my ‘ key to serve double duty
  (bind-key "*" #'typo-cycle-multiplication-signs typo-mode-map)
  (bind-key "?" #'typo-cycle-question-mark typo-mode-map)
  (bind-key "!" #'typo-cycle-exclamation-mark typo-mode-map)
  (unbind-key "`" typo-mode-map))

(defun jf/toggle-typo-mode ()
  "Toggle `typo-mode'."
  (interactive)
  (if (and (boundp 'typo-mode) typo-mode)
    (typo-mode -1)
    (typo-mode 1))
  (message "Toggling typo-mode.")
  (force-mode-line-update))

(use-package window
  :preface (require 'prot-window)
  ;; Wrangle up how windows and buffers display.
  :straight (:type built-in)
  :bind (("H-w" . #'jf/bury-or-unbury-buffer)
          ("C-x 2" . #'jf/window/split-and-follow-below)
          ("C-x 3" . #'jf/window/split-and-follow-right)
          ("H-\\" . #'jf/nav-toggle-split-direction))
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

  (defun switch-to-buffer-side-window (buffer-or-name &optional norecord)
    "Open BUFFER-OR-NAME in side window.

When pass NORECORD along to pop-to-buffer."
    (interactive
      (list (read-buffer-to-switch "Switch to buffer in other window: ")))
    (pop-to-buffer buffer-or-name
      '((display-buffer-in-direction)
         (display-buffer-in-side-window)
         (side . right)
         (window-width 72)
         (window-parameters
           (tab-line-format . none)
           (no-delete-other-windows . t)))
      norecord)
    (recenter 0 t))

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
        (if buffer-read-only (kill-current-buffer) (bury-use))))))

(use-package tab-bar
  :straight (:type built-in)
  :custom (tab-bar-tab-name-function 'tab-bar-tab-name-all-with-projectile)
  (tab-bar-auto-width-max '((330) 30))
  :config
  (defun tab-bar-tab-name-all-with-projectile ()
    (mapconcat
      (lambda (buffer)
        (format "%s%s"
        (alist-get (projectile-project-name)
          tab-bar-project-alist "" nil #'string=)
          (buffer-name buffer)))
      (delete-dups (mapcar #'window-buffer
                     (window-list-1 (frame-first-window)
                       'nomini)))
      ", "))
  (defvar tab-bar-project-alist nil
    "An alist of project names and their corresponding emoji prefix"))
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
  (setopt modus-themes-common-palette-overrides
    '((fg-heading-1 cyan-faint)))
  (setq modus-themes-headings ; read the manual's entry or the doc string
    '((0 . (bold 1.4))
       (1 . (variable-pitch bold 1.7))
       (2 . (overline semibold 1.5))
       (3 . (monochrome overline 1.4 background))
       (4 . (overline 1.3))
       (5 . (rainbow 1.2))
       (6 . (rainbow 1.15))
       (t . (rainbow 1.1))))
  ;; When these are non-nil, the mode line uses the proportional font
  (setopt modus-themes-mixed-fonts t
    modus-themes-variable-pitch-ui t)

  (defun jf/theme-custom-faces ()
    "Set the various custom faces for both `treesit' and `tree-sitter'."
    (modus-themes-with-colors
      (setopt hl-todo-keyword-faces
        `(
           ;; The first five are from Github:
           ;; https://github.com/orgs/community/discussions/16925
           ("NOTE" . ,blue-intense)
           ("TIP" . ,green-intense)
           ("IMPORTANT" . ,magenta-intense)
           ("SEE" . ,magenta-intense)
           ("WARNING" . ,yellow-intense)
           ("CAUTION" . ,red-intense)
           ;; Other keywords that I'm using
           ("HACK" . ,blue-intense)
           ("TODO" . ,red-intense)
           ("DRAFT" . ,red-intense)
           ("FIXME" . ,red-intense)
           ("DONE" . ,green-intense)
           ("PUBLISHED" . ,green-intense)
           ("ASSUMPTION" . ,yellow-intense)
           ("QUESTION" . ,yellow-intense)
           ("BLOCKED" . ,yellow-intense)
           ("WAITING" . ,yellow-intense)))
      (custom-set-faces
        `(bm-fringe-persistent-face
           ((,c :background ,bg-added :foreground ,fg-added)))
        `(bm-persistent-face
           ((,c :background ,bg-added-faint)))
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
           ((,c :foreground ,fg-prominent-note :background ,bg-prominent-note)))
        `(go-test--ok-face
           ((,c :foreground ,fg-prominent-note)))
        `(go-test--error-face
           ((,c :foreground ,err)))
        `(jf/mode-line-format/face-shadow
           ((,c :foreground ,fg-mode-line-active)))
        `(jf/mode-line-format/face-shadow-highlight
           ((,c :foreground ,fg-mode-line-active :background ,bg-hover)))
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
        `(org-verse
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
           ((,c :foreground ,red)))
        `(symbol-overlay-default-face
           ((,c :background ,bg-search-lazy))))
      ))
  ;; I had '(:light ef-cyprus) but the differentiation between function
  ;; and comment was not adequate
  ;; (setq jf/themes-plist '(:dark ef-bio :light ef-elea-light))
  (setq jf/themes-plist '(:dark
                           ef-fig
                           ;; modus-vivendi-deuteranopia
                           ;; ef-owl
                           :light
                           ef-elea-light))
  :init
  (modus-themes-include-derivatives-mode 1))

(use-package custom
  :straight (:type built-in)
  :config
  ;; In organizing the packages, I discovred that themes is part of the
  ;; `custom' package.
  (defun jf/color-scheme:emacs (&optional given-scheme)
    "Function to load named theme."
    (let ((scheme
            (or given-scheme
              (funcall
                (intern
                  (format "jf/color-scheme-func:%s" system-type))))))
      (modus-themes-select (plist-get jf/themes-plist scheme))))

  ;; Theming hooks to further customize colors
  (defvar after-enable-theme-hook nil
    "Normal hook run after enabling a theme.")

  ;; (defvar jf/window-manager
  ;;   (pcase system-type
  ;;     ('gnu/linux
  ;;       (s-downcase
  ;;         (shell-command-to-string
  ;;           "wmctrl -m | rg \"^Name:\\s*(.*)\" -r '$1' | sed 's/\\s\\+/-/g'")))
  ;;     ('darwin
  ;;       "not-applicable"))
  ;;   "Assign the active window manager.")

  (defun run-after-enable-theme-hook (&rest _args)
    "Run `after-enable-theme-hook'."
    (run-hooks 'after-enable-theme-hook))

  (advice-add 'enable-theme :after #'run-after-enable-theme-hook)

  (add-hook 'after-enable-theme-hook #'jf/theme-custom-faces)
  (add-hook 'modus-themes-after-load-theme-hook #'jf/theme-custom-faces)

  (defun jf/color-scheme-func:gnu/linux ()
    "Determine Gnome preferred/current theme."
    (if (equal
          "'prefer-dark'"
          (s-trim
            (shell-command-to-string
              "gsettings get org.gnome.desktop.interface color-scheme")))
      :dark :light))

  (defun jf/color-scheme-func:darwin ()
    "Determine MacOS preferred/current theme."
    (if (equal "Dark"
          (substring
            (shell-command-to-string
              "defaults read -g AppleInterfaceStyle") 0 4))
      :dark :light))

  (defvar jf/color-scheme-commands:gnu/linux
    '((:template "gsettings set org.gnome.settings-daemon.plugins.color night-light-enabled %s"
        :light "false" :dark "true")
       (:template "gsettings set org.gnome.desktop.interface color-scheme %s"
         :light "default" :dark "prefer-dark")
       (:template "gsettings set org.gnome.desktop.interface gtk-theme %s"
         :light "default" :dark "prefer-dark"))
    "A list of plists with three parts:

- :template :: command to run.
- :dark :: what the setting should be to be in \"dark\" mode.
- :light :: what the setting should be to be in \"light\" mode.")

  (defun jf/color-scheme-system-toggle:gnu/linux ()
    "Toggle the gnu/linux system scheme."
    (let* ((target_scheme
              (plist-get '(:dark :light :light :dark)
                (jf/color-scheme-func:gnu/linux))))
      ;; Instead of all of the shelling out, we could assemble the shell
      ;; commands into a singular command and issue that.
      (dolist (setting jf/color-scheme-commands:gnu/linux)
        ;; In essence pipe the output to /dev/null
        (shell-command-to-string
          (format (plist-get setting :template)
            (plist-get setting target_scheme))))
      (jf/color-scheme:emacs target_scheme)))

  (defun jf/color-scheme-system-toggle:darwin ()
    "Toggle the darwin system scheme."
    (shell-command
      (concat "osascript -e 'tell application \"System Events\" "
        "to tell appearance preferences "
        "to set dark mode to not dark mode'"))
    (jf/color-scheme:emacs))

  (defun jf/color-scheme-system-toggle ()
    "Toggle system-wide Dark or Light setting."
    (interactive)
    (funcall
      (intern
        (format "jf/color-scheme-system-toggle:%s" system-type))))

  (defalias 'jf/dark 'jf/color-scheme-system-toggle)

  ;; Set the color scheme of emacs based on existing system function.
  (jf/color-scheme:emacs))

(use-package xref
  ;; Cross-referencing commands.  I suspect there's a lot more that I
  ;; could use to improve my usage.
  :straight (:type built-in)
  :custom
  (xref-file-name-display 'project-relative)
  (xref-auto-jump-to-first-xref 'show)
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

(use-package cond-let
  :straight (cond-let :host github :repo "tarsius/cond-let"))

(use-package transient
  ;; A package for creating turbo-charged menus.  It is the backbone for
  ;; the menu-like dispatch of `magit' functionality.
  :straight (:host github :repo "magit/transient")
  ;; This exposes the --sign switch for git commit
  :config (setopt transient-default-level 7))

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
          ("e" . wgrep-change-to-wgrep-mode)))

(use-package deadgrep
  :straight t
  :bind ("C-c f" . deadgrep)
  (:map deadgrep-mode-map
    ("e" . deadgrep-edit-mode)))

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
          ("<H-backspace>" . crux-kill-line-backwards)
          ("C-c d" . jf/duplicate-current-line-or-lines-of-region))
  :config
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
  (add-hook 'emacs-everywhere-mode-hook #'toggle-frame-maximized))

(use-package ws-butler
  ;; Keep white space tidy.
  :straight t
  :hook (prog-mode . ws-butler-mode))

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
          ("H-5" . jf/org-insert-immediate-active-timestamp)
          ("H-i" . org-insert-link)
          )
  :bind (:map org-mode-map (("C-c j" . org-goto)
                             ("C-c C-j" . jf/project/jump-to-task)
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
    org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar"
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
    org-todo-keywords '((sequence "TODO(t)"
                          "STARTED(s!)"
                          "BLOCKED(b@/!)"
                          "WAITING(w@/!)"
                          "|"
                          "DONE(d!)")
                         (sequence "DRAFT(D!)" "|" "PUBLISHED(p!)")
                         (sequence "|" "CANCELED(c@/!)")))
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

      (if-let* ((task (cdr name-and-task)))
        ;; I like having the most recent writing close to the headline;
        ;; showing a reverse order.  This also allows me to have
        ;; sub-headings within a task and not insert content and clocks
        ;; there.  (if-let* ((drawer (car (org-element-map task 'drawer
        ;; #'identity)))) (goto-char (org-element-property :contents-end
        ;; drawer)) (goto-char (org-element-property :contents-begin
        ;; task)))
        (let* ((name-and-subtask
                 (jf/alist-prompt
                   (format "Sub-Task for %s: " task-name)
                   (jf/org-mode/existing-sub-tasks :task task)))
                (subtask-name
                  (car name-and-subtask)))
          (if-let* ((subtask (cdr name-and-subtask)))
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
    ;; (add-hook 'before-save-hook
    ;;   #'jf/org-mode/recalculate-buffer-tables nil :local)
    ;; (add-hook 'before-save-hook
    ;;   #'jf/org-add-ids-to-headlines-in-file nil 'local)
    (add-hook 'focus-out-hook
      #'org-save-all-org-buffers nil :local)
    (jf/org-capf)
    (org-lint-remove-checker 'invalid-fuzzy-link)
    (turn-on-visual-line-mode)
    (electric-pair-mode -1)
    ;; https://baty.net/posts/2025/05/set-point-at-first-heading-when-opening-org-mode-file/
    (goto-char (point-min))
    (when (re-search-forward "^\\*+ " nil t)
      (goto-char (match-beginning 0))))

  ;; https://stackoverflow.com/questions/13340616/assign-ids-to-every-entry-in-org-mode
  (defun jf/org-add-ids-to-headlines-in-file ()
    "Add ID properties to all file's headlines without an ID."
    (interactive)
    (org-map-entries 'org-id-get-create))

  (add-to-list 'org-structure-template-alist '("m" . "marginnote"))
  (add-to-list 'org-structure-template-alist '("D" . "details"))
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
  :bind (("C-c l s" . org-store-link)
          ("C-c a" . org-agenda)
          ("C-c c" . org-capture))

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

(defvar jf/personal/filename-for-journal
  ""
  "Where I put my journal.")

(with-eval-after-load 'org
  (add-to-list 'org-capture-templates
  '("j" "Journal Entry"
     entry (file+olp+datetree
             jf/personal/filename-for-journal)
     "At %(format-time-string \"%R\")\n:PROPERTIES:\n:CUSTOM_ID: journal-%(format-time-string \"%Y%m%d-%H%M\")\n:END:\n%?"
     :empty-lines-before 1
     :empty-lines-after 1
     :clock-in t
     :clock-resume t))

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
    '("nt" . "@@latex:\\textsc{@@@@html:<i class=\"new-thought\">@@$1@@html:</i>@@@@latex:}@@"))
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
  (progn
    ;; (setq org-latex-classes nil)
    (add-to-list 'org-latex-classes
      '("jf/memoir"
         "\\documentclass[10pt,letter,landscape,twoside]{book}
\\usepackage[letter]{anysize}
\\usepackage{minted}
\\usepackage{array, booktabs, caption}
\\usemintedstyle{emacs}
\\usepackage[linktocpage=true]{hyperref}
\\usepackage[french,english]{babel}
\\usepackage[letter]{geometry}
\\usepackage{minimalist}
\\usepackage{fontspec}
\\usepackage{caption} \\captionsetup{labelfont=bf,font={sf,small}}
\\setmainfont{TeX Gyre Pagella}
\\usepackage{enumitem} \\setlist{nosep}
\\usepackage{longtable}
\\usepackage{microtype}
\\AtBeginEnvironment{longtable}{\\footnotesize}
\\usepackage[marginal,hang]{footmisc}
\\usepackage{relsize,etoolbox}
\\AtBeginEnvironment{quote}{\\smaller}
\\AtBeginEnvironment{tabular}{\\smaller}
\\usepackage{mathabx}
\\usepackage{multicol}
\\setlength\\columnsep{20pt}
\\hypersetup{colorlinks=true, linkcolor=blue, filecolor=magenta, urlcolor=cyan}
\\tolerance=1000
\\usepackage{float}
\\usepackage{rotating}
\\usepackage{sectsty}
\\usepackage{titlesec}
\\titleformat{\\section}{\\normalfont\\fontsize{12}{18}\\bfseries}{\\thesection}{1em}{}
\\setcounter{secnumdepth}{1}"
         ("\\section{%s}" . "\\section{%s}")
         ("\\subsection{%s}" . "\\subsection{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection{%s}")
         ("\\paragraph{%s}" . "\\paragraph{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph{%s}"))))
  (add-to-list 'org-latex-classes
    '("jf/two-column-landscape"
       "\\documentclass[10pt,letter,landscape,twoside]{article}
\\usepackage[letter]{anysize}
\\usepackage{minted}
\\usepackage{array, booktabs, caption}
\\usemintedstyle{emacs}
\\usepackage[linktocpage=true]{hyperref}
\\usepackage[french,english]{babel}
\\usepackage[letter,top=1in,bottom=1in,left=1in,right=1in]{geometry}
\\usepackage{minimalist}
\\usepackage{fontspec}
\\usepackage{caption} \\captionsetup{labelfont=bf,font={sf,small}}
\\setmainfont{TeX Gyre Pagella}
\\usepackage{enumitem} \\setlist{nosep}
\\usepackage{longtable}
\\usepackage{microtype}
\\AtBeginEnvironment{longtable}{\\footnotesize}
\\usepackage[marginal,hang]{footmisc}
\\usepackage{relsize,etoolbox}
\\AtBeginEnvironment{quote}{\\smaller}
\\AtBeginEnvironment{tabular}{\\smaller}
\\usepackage[marginal,hang]{footmisc}
\\usepackage{mathabx}
\\usepackage{multicol}
\\usepackage[skip=10pt plus1pt, indent=40pt]{parskip}
\\setlength\\columnsep{15pt}
\\hypersetup{colorlinks=true, linkcolor=blue, filecolor=magenta, urlcolor=cyan}
\\tolerance=1000
\\usepackage{float}
\\usepackage{rotating}
\\usepackage{sectsty}
\\usepackage{titlesec}
\\titleformat{\\section}{\\normalfont\\fontsize{12}{18}\\bfseries}{\\thesection}{1em}{}
\\setcounter{secnumdepth}{1}"
       ("\\section{%s}" . "\\section{%s}")
       ("\\subsection{%s}" . "\\subsection{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection{%s}")
       ("\\paragraph{%s}" . "\\paragraph{%s}")
       ("\\subparagraph{%s}" . "\\subparagraph{%s}")))
  (add-to-list 'org-latex-classes
    '("jf/two-column-portrait"
       "\\documentclass[10pt,letter,portrait]{article}
\\usepackage[letter]{anysize}
\\usepackage{minted}
\\usepackage{array, booktabs, caption}
\\usemintedstyle{emacs}
\\usepackage[linktocpage=true]{hyperref}
\\usepackage[french,english]{babel}
\\usepackage[letter,top=1in,bottom=1in]{geometry}
\\usepackage{minimalist}
\\usepackage{fontspec}
\\usepackage{caption} \\captionsetup{labelfont=bf,font={sf,small}}
\\setmainfont{TeX Gyre Pagella}
\\usepackage{enumitem} \\setlist{nosep}
\\usepackage{longtable}
\\usepackage{microtype}
\\AtBeginEnvironment{longtable}{\\footnotesize}
\\usepackage[marginal,hang]{footmisc}
\\usepackage{relsize,etoolbox}
\\AtBeginEnvironment{quote}{\\smaller}
\\AtBeginEnvironment{tabular}{\\smaller}
\\usepackage[skip=10pt plus1pt, indent=40pt]{parskip}

\\usepackage[marginal,hang]{footmisc}
\\usepackage{mathabx}
\\usepackage{multicol}
\\setlength\\columnsep{20pt}
\\hypersetup{colorlinks=true, linkcolor=blue, filecolor=magenta, urlcolor=cyan}
\\tolerance=1000
\\usepackage{float}
\\usepackage{rotating}
\\usepackage{sectsty}
\\usepackage{titlesec}
\\titleformat{\\section}{\\normalfont\\fontsize{12}{18}\\bfseries}{\\thesection}{1em}{}
\\setcounter{secnumdepth}{1}"
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
        (if-let* ((filter-body
                    (plist-get plist :filter-body)))
          (progn
            (add-to-list 'filter-body jf/ox/filter-body/latex)
            (plist-put plist :filter-body filter-body))
          (plist-put plist :filter-body '(jf/ox/filter-body/latex)))
        (if-let* ((filter-final-output
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
    (if-let* ((abbr-links (plist-get info :abbr-links)))
      (replace-regexp-in-string
        "^\\\\documentclass\\(.*\\)"
        (lambda (md)
          "Acronym package to matching line."
          (concat "\\\\documentclass" (match-string 1 md)
            "\n\\\\usepackage[printonlyused,footnote]{acronym}"))
        body)
      body))

  (defun jf/ox/filter-body/latex (body backend info)
    "Conditionally add a list of acronyms to the exported LaTeX document.

To have a meaningful render, this requires using the acronym LaTeX
package.  The `jf/ox/filter-final-output/latex' handles injecting that
LaTeX package."
    (if-let* ((abbr-links (plist-get info :abbr-links)))
      ;; We encountered some links, let's add a section.
      (progn
        (concat
          body
          "\n\\section{List of Acronyms}\n"
          "\\begin{acronym}\n"
          (mapconcat
            (lambda (data)
              "Create an acro for link."
              ;; TODO add \\acroextra for additional description
              (format "\\acro{%s}{%s}"
                (plist-get data :abbr)
                (format "{%s%s}"
                  (plist-get data :title)
                  (if-let* ((desc
                             (plist-get data :description)))
                    (format "\\acroextra{: %s}" desc)
                    ""))))
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

;; This line actually replaces Emacs' entire narrowing
;; keymap, that's how much I like this command. Only
;; copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)
(defun narrow-or-widen-dwim (prefix)
  "Widen if buffer is narrowed, narrow-dwim otherwise.

Dwim means: region, org-src-block, org-subtree, logos outline, or defun,
whichever applies first.  Narrowing to org-src-block actually calls
`org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  ;; https://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
  (interactive "P")
  (declare (interactive-only))
  (cond
    ((and (buffer-narrowed-p)
       (not prefix)) (widen))
    ((region-active-p)
      (narrow-to-region (region-beginning)
        (region-end)))
    ((derived-mode-p 'org-mode)
      ;; `org-edit-src-code' is not a real narrowing
      ;; command. Remove this first conditional if
      ;; you don't want it.
      (cond ((ignore-errors (org-edit-src-code) t)
              (delete-other-windows))
        ((ignore-errors (org-narrow-to-block) t))
        (t (org-narrow-to-subtree))))
    ((derived-mode-p 'latex-mode)
      (LaTeX-narrow-to-environment))
    ((and (fboundp 'logos--page-p) (logos--page-p))
      (logos--narrow-to-page 0))
    (t (narrow-to-defun))))

(defun jf/org-link-to-headline (&optional arg)
  "Insert link to an `org-mode' headline.

When ARG is nil, use the current buffer's file as the source for
headlines.  In the case where we are withing an `org-capture' we will
prompt to choose a file from our `org-agenda-files'.

When ARG is non-nil, prompt for a source filename from the
`org-agenda-files' candidate."
  (interactive "P")
  (let* ((filename
           ;; When we are in a capture mode, we do not have a
           ;; `buffer-file-name' so we need to prompt.
           (if (or arg (not (buffer-file-name)))
             (completing-read "Pick Agenda File: " org-agenda-files)
             (buffer-file-name)))
          (headline
            (save-excursion
              (with-current-buffer (find-file-noselect filename)
                (consult-imenu)
                (org-element-at-point))))
          (custom_id
            (or (org-entry-get headline "CUSTOM_ID")
              (let ((id
                      (org-id-new)))
                (org-entry-put headline "CUSTOM_ID" id)
                id)))
          (title
            (read-string "Description: "
              (org-element-property :title headline))))
    (org-insert-link
      filename
      (format "denote:%s::#%s"
        (denote-retrieve-filename-identifier filename)
        custom_id)
      title)))

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
                           (org-entry-get headline "AUTHOR"))
                         (translator
                           (org-entry-get headline "TRANSLATOR"))
                         )
                    (cons
                      (jf/book-make-label
                        :title title :subtitle subtitle
                        :author author :translator translator)
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
       plain (file jf/filename/dictionary)
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
  "Convert region from markdown to org or vice versa."
  ;; From http://yummymelon.com/devnull/converting-a-markdown-region-to-org-revisited.html
  (interactive "r")
  (let* ((from-to (if (derived-mode-p 'org-mode)
                   (cons "org" "markdown")
                   (cons "markdown" "org"))))
    (shell-command-on-region start end
      (format "pandoc -f %s -t %s" (car from-to) (cdr from-to))
      t t)))

(use-package abbrev
  ;; The =abbrev= package is simple and powerful, providing an
  ;; auto-correct that I configure.  No more “teh” in my text.
  :straight (:type built-in)
  :custom (abbrev-file-name
            (cond
              ((f-file? "~/SyncThings/source/emacs.d/abbrev_defs")
                (file-truename "~/SyncThings/source/emacs.d/abbrev_defs"))
              ((f-file? "~/.emacs.d/abbrev_defs")
                (file-truename  "~/.emacs.d/abbrev_defs"))
              (t (user-error "Unable to find suitable abbrev_defs"))))
  :hook (prog-mode . abbrev-mode)
  (text-mode . abbrev-mode))

(defun C-w-dwim ()
  "Copy region or delete backward.

When `active-region-p' is non-nil, copy the region as kill.  When
`active-region-p' is nil, delete backwards.

This all is because long ago I mentally trained 'C-w' as backward delete
word.  Probably form my days on the AS400 terminal."
  (interactive)
  (if (region-active-p)
    (call-interactively #'copy-region-as-kill)
    (jf/delete-region-or-backward-word)))

(use-package emacs
  :bind (("C-M-i" . completion-at-point)
          ("TAB" . indent-for-tab-command)
          ("C-w" . C-w-dwim)
          ("M-DEL" . jf/delete-region-or-backward-word)
          ("C-M-<backspace>" . backward-kill-paragraph))
  :custom
  (menu-bar-mode)
  (column-number-mode t)
  (global-display-fill-column-indicator-mode t)
  (delete-selection-mode t)
  (auto-save-no-message t)
  (auto-save-file-name-transforms
    '((".*" "~/.emacs.d/autosaves/\\1" t)))
  (sentence-end-double-space t)
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
  :after (consult projectile)
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
       :narrow ?d
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
       ;; consult-projectile--source-projectile-dir
       ;; key m
       consult--source-bookmark
       ;; key r
       consult-projectile--source-projectile-recentf
       ;; key R
       ;; jf/consult--source-recent-file
       ;; key d
       jf/consult--source-draft-blog-posts
       ;; key *
       ;; consult--source-modified-buffer
       ))
  :bind
  ("H-p" . consult-projectile))


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
  :bind ("C-H-h" . jf/helpful-menu))

(use-package tempel
  ;; For awhile, I'd used yasnippets; themselves inspired by my beloved
  ;; TextMate.  However, I've found `tempel' to be both more than
  ;; adequate and has a narrower implementation foot-print, cleaving
  ;; closer to emacs-lisp; thus likely easing it's maintenance burden.
  :straight (tempel :host github :repo "minad/tempel")
  :custom (tempel-path "~/git/dotemacs/tempels/*.eld")
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


;; I'm considering this
;; (use-package slash-commands
;;   :straight (:host github :repo "bluzky/slash-commands")
;;   :config (global-slash-commands-mode 1)
;;   (defun insert-date ()
;;     "Insert current date at point."
;;     (interactive)
;;     (insert (format-time-string "%Y-%m-%d")))
;;   ;; Register it
;;   (slash-commands-register-commands
;;     '(("date" . insert-date))))

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

(defvar jf/filename/dictionary
  (denote-get-path-by-id "20230108T083359")
  "Dude, you can put your new words here.")

(defvar jf/filename/glossary
  (denote-get-path-by-id "20250101T000000")
  "Dude, you can put your concepts here.")

(setopt denote-known-keywords
  (if jf/filename/glossary
    (with-current-buffer (find-file-noselect jf/filename/glossary)
      (save-restriction
        (widen)
        (org-map-entries
          (lambda ()
            (or
              (org-entry-get (org-element-at-point) "TAG")
              (user-error
                "Glossary entry %S missing TAG property"
                (org-element-property :title (org-element-at-point)))))
          "+tags+LEVEL=2"
          'file)))
    denote-known-keywords))

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
  ("H-d f" . #'consult-denote-find)
  ("H-d s" . #'consult-denote-grep)
  :custom
  (consult-denote-grep-command #'consult-ripgrep)
  :config
  ;; Consult favors fdfind over fd; so I'm changing that behavior to
  ;; simplify.
  (setopt consult-fd-args '("fd" "--full-path --color=never"))
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
        (font-spec :family "Noto Color Emoji")
        frame
        'prepend)))
  ;; For when Emacs is started in GUI mode:
  (--set-emoji-font nil)
  ;; Hook for when a frame is created with emacsclient see
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
  (add-hook 'after-make-frame-functions '--set-emoji-font))

(use-package sdcv
  ;; http://mbork.pl/2017-01-14_I'm_now_using_the_right_dictionary
  ;; Except, I'm using the Melpa package.
  :commands (sdcv-search)
  :bind ("C-c C-'" . sdcv-search-input)
  :straight t)

(use-package unicode-fonts
  ;; Before the emojii...
  :straight t
  :config (unicode-fonts-setup))

(if (fboundp 'fill-paragraph-semlf)
  (require 'fill-paragraph)
  (use-package unfill
    ;; Provides the reverse of ~fill-paragraph~, and a toggle fill and
    ;; unfill.  In fact, the unfill/fill function of Emacs was the first
    ;; editor function I saw (shown to me by a friend in 2005) that had me
    ;; strongly consider Emacs. Alas I was not prepared for Emacs.
    :bind ("M-q" . unfill-toggle)
    :straight t))

(use-package hungry-delete
  ;; Delete multiple spaces in one delete stroke.
  :straight t
  :config (global-hungry-delete-mode))

(use-package string-inflection
  ;; A quick way to change case and separators for words.
  :straight t)

(use-package move-text
  ;; A simple package ability to move lines up and down.
  :straight t
  :bind (([C-s-down] . move-text-down)
          ([C-s-up] . move-text-up)))

(use-package titlecase
  ;; The rules of “titlecase” are confounding.  The ~titlecase.el~
  ;; package provides numerous ways to cast a string to “titlecase.”  I
  ;; chose wikipedia style as a quasi-opinionated compromise.
  :straight t
  :custom (titlecase-style 'wikipedia))

(use-package multiple-cursors
  ;; Allow Emacs to work with multiple cursors.  See
  ;; https://melpa.org/#/multiple-cursors
  :bind (("C-M-SPC" . set-rectangular-region-anchor)
          ("C->" . mc/mark-next-like-this)
          ("C-<" . mc/mark-previous-like-this)
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

(use-package csv-mode
  :straight t
  ;; By default I want to show the separator character.
  :custom (csv-invisibility-default nil)
  ;; Always enter CSV mode in align mode; makes it easier to read.
  :hook (csv-mode . csv-align-mode))

(use-package emacs
  :hook (emacs-lisp-mode . jf/emacs-lisp-mode-hook)
  :preface
  (defun jf/emacs-lisp-mode-hook ()
    ;; 72 is what I've found works best on exporting to my blog.
    (setq-local fill-column 72)))

(use-package repeat
  :straight (:type built-in)
  :config
  (repeat-mode))

(use-package outline-indent
  ;; Simple and basic collapsable outline modes.
  :straight t
  :hook (prog-mode . outline-indent-minor-mode))

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

(require 'setup-diagraming)

(use-package dotenv-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)))

(use-package vterm
  ;; A terminal in Emacs.
  :straight t
  :config
  (setopt vterm-always-compile-module t))

(use-package multi-vterm
  :straight t)

(use-package dash-docs
  ;; An alternate to devdocs.  Facilitates downloading HTML files and
  ;; index.
  :straight t)

(use-package flymake
  :straight t
  ;; Don't be so hasty in syntax checking.
  :custom (flymake-no-changes-timeout 2))

(use-package flyspell
  :straight (:type built-in)
  :custom
  ;; The default binding is C-; which clobbers iedit; something I find
  ;; unacceptable.
  (flyspell-auto-correct-binding [(hyper ?\;)])
  :config
  (setq flyspell-mode-map nil))

(use-package symbol-overlay
  ;; Very useful when refactoring elisp
  :straight t)

(use-package text-mode
  :straight (:type built-in)
  :config
  (add-hook 'text-mode-hook #'jf/text-mode-configurator)
  (defun jf/text-mode-configurator ()
    (setq show-trailing-whitespace t)))


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

(use-package projectile
  ;; Convenient organization and commands for projects.
  :straight t
  :custom
  (projectile-enable-caching t)
  (projectile-project-search-path
    '("~/git/" "~/git/converge-cloud"))
  :bind ("H-e ." . projectile-toggle-between-implementation-and-test)
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


;; (use-package casual-suite
;;   :straight t
;;   :config
;;   (use-package casual-avy
;;     :straight t)
;;   (keymap-set calc-mode-map "H-c" #'casual-calc-tmenu)
;;   (keymap-set dired-mode-map "H-c" #'casual-dired-tmenu)
;;   (keymap-set isearch-mode-map "H-c" #'casual-isearch-tmenu)
;;   (keymap-set ibuffer-mode-map "H-c" #'casual-ibuffer-tmenu)
;;   (keymap-set ibuffer-mode-map "H-f" #'casual-ibuffer-filter-tmenu)
;;   (keymap-set ibuffer-mode-map "H-s" #'casual-ibuffer-sortby-tmenu)
;;   (keymap-set Info-mode-map "H-c" #'casual-info-tmenu)
;;   (keymap-set reb-mode-map "H-c" #'casual-re-builder-tmenu)
;;   (keymap-set reb-lisp-mode-map "H-c" #'casual-re-builder-tmenu)
;;   (keymap-set bookmark-bmenu-mode-map "H-c" #'casual-bookmarks-tmenu)
;;   (keymap-set org-agenda-mode-map "H-c" #'casual-agenda-tmenu)
;;   (keymap-set symbol-overlay-map "H-c" #'casual-symbol-overlay-tmenu)
;;   (keymap-global-set "H-c a" #'casual-avy-tmenu)
;;   (keymap-global-set "H-c e" #'casual-editkit-main-tmenu))

(use-package bookmark
  :straight (:type built-in)
  :config
  (defconst fallback-bookmark-file "~/emacs-bookmarks.el")
  ;; On each machine I use, I have different bookmarks, yet they all
  ;; point to the same location.
  (setopt bookmark-default-file fallback-bookmark-file)

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

;;;; BEGIN: Random Page in PDF Functionality
(defun pdf-view-bookmark-jump-handler:random (bmk)
  "A handler-function implementing interface for bookmark PDF BMK.

When the handler has a 'pages property, which is assumed to be a list,
pick one from that.  Otherwise fallack to the 'page property.

See also `pdf-view-bookmark-jump-handler' and
`pdf-view-bookmark-make-record'."
  (let ((pages
          (bookmark-prop-get bmk 'pages)))
    (bookmark-prop-set bmk 'page
      (or (seq-random-elt pages) (bookmark-prop-get bmk 'page)))
    (pdf-view-bookmark-jump-handler bmk)))

(defvar pdf-view-bookmark-make-record:prompt-for-random
  nil
  "When non-nil, prompt as to whether or not to create a bookmark
that is randomization.")

(defun pdf-view-bookmark-make-record:with-randomizer (&rest app)
  "Conditionally randomize which page we'll open in a PDF.

See `pdf-view-bookmark-make-record:prompt-for-random'."
  (let ((bmk
          (apply app)))
    (if (and
          pdf-view-bookmark-make-record:prompt-for-random
          (yes-or-no-p "Specify Random Pages?"))
      (let* ((attributes
               (cdr bmk))
              (integers-as-string
                (split-string
                  (read-string "Enter pages (comma-separated): "
                    (format "%s," (alist-get 'page attributes)))
                  "[,; ]+" t "[[:space:]]+")))
        ;; We clobber the existing handler replacing it with one of
        ;; our own devising.
        (setcdr (assoc 'handler attributes)
          'pdf-view-bookmark-jump-handler:random)
        (add-to-list 'attributes
          (cons 'pages
            (mapcar #'string-to-number integers-as-string)))
        ;; We need to return an object of the same form (e.g. a `cons'
        ;; cell).
        (cons (car bmk) attributes))
      bmk)))

(advice-add #'pdf-view-bookmark-make-record
  :around #'pdf-view-bookmark-make-record:with-randomizer)

(defvar default-bookmark-display-function
  nil
  "When non-nil, favor opening bookmarks with this function.")

(defun bookmark-jump-with-display (args)
  (list
    (car args)
    (or (cdr args)
      default-bookmark-display-function
      (when current-prefix-arg 'switch-to-buffer-side-window))))
(advice-add #'bookmark-jump :filter-args #'bookmark-jump-with-display)

;; Show that I'll be opening this PDF to a random page.
(put 'pdf-view-bookmark-jump-handler:random 'bookmark-handler-type "⚅PDF")
  ;;;; END: Random Page in PDF Functionality

(use-package logos
  ;; A `narrow-region' extension that moves towards providing a
  ;; presentation-type experience.
  :straight t
  :bind (:map logos-focus-mode-map
          ("M-]" . #'logos-forward-page-dwim)
          ("M-[" . #'logos-backward-page-dwim))
  :config
  (let ((map
          global-map))
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
    logos-olivetti t)
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
    "Remove some visual chatter."
    (text-scale-adjust 2)
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
    (text-scale-adjust 0)
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
      (define-key map (kbd "M-]") 'logos-forward-page-dwim)
      (define-key map (kbd "M-[") 'logos-backward-page-dwim)
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
            (("f" . elfeed-show-fetch-full-text)
              ("+" . jf/elfeed-show-tag)
              ("-" . elfeed-show-untag))))
  :config
  (defun elfeed-show-fetch-full-text ()
    "Fetch the full text of the current Elfeed entry using eww-readable."
    ;; https://plrj.org/2025/06/14/my-emacs-elfeed-configuration/
    (interactive)
    (let* ((entry elfeed-show-entry)
            (url (elfeed-entry-link entry)))
      (eww url)  ;; Open the URL in eww
      (run-at-time "0.5 sec" nil
        (lambda ()
          (with-current-buffer "*eww*"
            (eww-readable))))))
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
  (defun jf/elfeed-load-db-and-open (&rest args)
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

;; https://punchagan.muse-amuse.in/blog/elfeed-db-back-up-hooks/
(defvar jf/elfeed-db-save-timer nil
  "Timer for debounced elfeed database saves.")

(defun jf/elfeed-db-save-and-backup ()
  "Save the elfeed database and commit to git."
  (when (and (boundp 'elfeed-db) elfeed-db)
    (elfeed-db-save)
    (let ((default-directory elfeed-db-directory))
      (when (file-exists-p ".git")
        (call-process "git" nil "*elfeed-db-backup*" nil "add" "-A")
        (call-process "git" nil "*elfeed-db-backup*" nil "commit" "-m" "auto-backup")
        (call-process "git" nil "*elfeed-db-backup*" nil "push" "origin" "main")))))

(defun jf/elfeed-db-save-soon ()
  "Schedule a database save after 10 seconds of idle."
  (interactive)
  (when jf/elfeed-db-save-timer
    (cancel-timer jf/elfeed-db-save-timer))
  (setq jf/elfeed-db-save-timer
        (run-with-idle-timer 10 nil #'jf/elfeed-db-save-and-backup)))

;; Save and backup when tags change (elfeed-web usage)
(add-hook 'elfeed-tag-hooks   (lambda (&rest _) (jf/elfeed-db-save-soon)))
(add-hook 'elfeed-untag-hooks (lambda (&rest _) (jf/elfeed-db-save-soon)))

;; Save and backup when new entries are added
(add-hook 'elfeed-db-update-hook #'jf/elfeed-db-save-soon)

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
  :hook ((eww-mode . jf/reader-visual)))

;; Given that I have tools to grab results from the browser; it makes
;; sense to have tools to launch a search in my browser.
(use-package emacs-websearch
  :straight '(emacs-websearch :host github :repo "zhenhua-wang/emacs-websearch"))

;; From
;; https://www.emacs.dyerdwelling.family/emacs/20260116182841-emacs--speed-reading-in-emacs-building-an-rsvp-reader/
(defun speedread-in-minibuffer ()
  "Display words from point (or mark to point) in minibuffer using RSVP.
Use f/s for speed, [/] for size, b/n to skip, SPC to pause, q to quit."
  (interactive)
  (let* ((start (if (region-active-p) (region-beginning) (point)))
          (end (if (region-active-p) (region-end) (point-max)))
          (text (buffer-substring-no-properties start end))
          (wpm 350) (font-size 500) (orp-column 20)
          (word-positions '()) (pos 0) (i 0)
          (message-log-max nil)) ; Disable message logging
    ;; Build word positions list
    (dolist (word (split-string text))
      (unless (string-blank-p word)
        (when-let ((word-start
                     (string-match (regexp-quote word) text pos)))
          (push (cons word (+ start word-start)) word-positions)
          (setq pos (+ word-start (length word))))))
    (setq word-positions (nreverse word-positions))
    ;; Display loop
    (while (< i (length word-positions))
      (let* ((word (car (nth i word-positions)))
              (word-pos (cdr (nth i word-positions)))
              (word-len (length word))
              (delay (* (/ 60.0 wpm)
                       (cond
                         ((< word-len 3) 0.8)
                         ((> word-len 8) 1.3)
                         (t 1.0))
                       (if (string-match-p "[.!?;:]$" word) 1.5 1.0)))
              (orp-pos (/ word-len 3))
              (face-mono
                `(:height ,font-size :family "monospace"))
              (face-orp
                `(:foreground "red" :weight normal ,@face-mono))
              (padded-word
                (concat
                  (propertize
                    (make-string (max 0 (- orp-column orp-pos)) ?\s)
                    'face face-mono)
                  (propertize (substring word 0 orp-pos)
                    'face face-mono)
                  (propertize (substring word orp-pos (1+ orp-pos))
                    'face face-orp)
                  (propertize (substring word (1+ orp-pos))
                    'face face-mono))))
        (goto-char (+ word-pos word-len))
        (message "%s" padded-word)
        (pcase (read-event nil nil delay)
          (?f (setq wpm (min 1000 (+ wpm 50))))
          (?s (setq wpm (max 50 (- wpm 50))))
          (?\[ (setq font-size (max 100 (- font-size 20))))
          (?\] (setq font-size (min 500 (+ font-size 20))))
          (?b (setq i (max 0 (- i 10))))
          (?n (setq i (min (1- (length word-positions)) (+ i 10))))
          (?\s (read-event
                 (format "%s [PAUSED - WPM: %d]" padded-word wpm)))
          (?q (setq i (length word-positions)))
          (_ (setq i (1+ i))))))))

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
  :after cond-let
  :init (use-package with-editor
          :straight t
          :custom (with-editor-emacsclient-executable
                    (file-truename "~/bin/git_editor")))
  :config
  ;; git-commit is a package shipped with transient.
  (require 'git-commit)
  (setq git-commit-major-mode 'git-commit-ts-mode)
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

(defun jf/magit-purge-index-locks ()
  "Delete associated index locks for project."
  (interactive)
  (if-let* ((dir
              (magit-gitdir))
             (lock-file
               (f-join dir "index.lock")))
    (when (f-exists? lock-file)
      (progn
        (delete-file lock-file)
        (message "Purged those pesky git locks.")))
    (user-error "Not in Git project")))

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
  :straight (:host github :repo "alphapapa/magit-todos")
  :after (magit cond-let))

(use-package hl-todo
  ;; A simple package to highlight todos identify by the
  ;; `hl-todo-keyword-faces'.
  :straight t
  :bind (("H-t H-n" . hl-todo-next)
          ("H-t H-p" . hl-todo-previous))
  :config (global-hl-todo-mode))

(use-package auth-source
  :straight (:type built-in)
  :config
  (setq auth-sources (list "~/.authinfo.pgp" "~/.authinfo")))

(use-package forge
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
    '("projectblacklight/spotlight"
       "samvera-labs/geomash"
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

(use-package git-commit-ts-mode
  :straight t
  :after (magit git-commit)
  :mode "\\COMMIT_MESSAGE\\'"
  :hook ((git-commit-ts-mode . jf/git-commit-mode-configurator))
  :bind (:map git-commit-ts-mode-map
          (("TAB" .  #'completion-at-point)))
  :config
  (defun jf/git-commit-mode-configurator ()
    "Prepare all of the commit buffer structure"
    (setq-local fill-column git-commit-fill-column)
    (goto-char (point-min))
    (beginning-of-line-text)
    (when (looking-at-p "^$")
      (structured-commit/write-message))))

(use-package structured-commit
  ;; git log --pretty=format:"%s" | rg "\w*\((\w+)\)" -r '$1' --only-matching | sort | uniq | awk '{ print "INSERT INTO scopes (project, scope) VALUES (\"morpho-swagger\", \"" $1 "\");"}' | sqlite3 ~/.emacs.d/structured-commit.db
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

(use-package git-link
  ;; Type ~M-x git-link~ and the function pushes the Git forge URL to
  ;; the kill ring; I’ve configured the URL to use the SHA of the commit
  ;; of the line on which I called `git-link'.  This is helpful for
  ;; sharing links with other folks.  I use this /all of the time./ See
  ;; https://github.com/sshaw/git-link.
  :bind (("C-c l g" . git-link))
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
  :bind (("H-6" . jf/git-messenger-popup)
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
          ("C-M-z" . undo-tree-redo))
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
    (let ((query (concat "level=2+books+" query)))
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
    ;; - inline_comments :: a concession that I need different comments
    ;;                      based on context; and that marginalia may be
    ;;                      too much in some cases.
    ;; - update :: I write updates for my blog posts; corrections or
    ;;             additions based on new information.
    (org-hugo-paired-shortcodes
      "blockquote marginnote inlinecomment update")
    (hugo-use-code-for-kbd t)
    (org-hugo-base-dir (file-truename "~/git/takeonrules.source/"))
    (org-hugo-front-matter-format "yaml")
    :config
    (setq org-hugo-section (format-time-string "posts/%Y"))
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
      (when-let* ((kw-plist (jf/org-keywords-as-plist
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
      (insert (format "#+%s: %s\n" (upcase key) value)))

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

    ;; TODO: Will need to account for publishing blog posts from
    ;; Journal.
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

    ;; TODO: Will need to account for publishing blog posts from
    ;; Journal.
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
  :bind (:map pdf-view-mode-map ("g" . #'pdf-view-goto-page))
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

  ;; (defadvice server-visit-files
  ;;   (around server-visit-files-custom-find
  ;;     activate compile)
  ;;   "Maintain a counter of visited files from a single client call."
  ;;   (let ((server-visit-files-custom-find:buffer-count
  ;;           0))
  ;;     ad-do-it))
  (defun jf/server-visit-files (&rest app)
    (let ((server-visit-files-custom-find:buffer-count
            0))
      (apply app)))
  (advice-add #'server-visit-files :around #'jf/server-visit-files)

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
      ;;   (if-let* ((filename (f-join denote-journal-extras-directory "20240131T000000--time-reporting.org")))
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

  (bind-key "H-2" 'jf/project/jump-to/project-work-space)
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
                        ;; There's a registered handler for the channel
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
            " --ignore-case --follow --only-matching --no-ignore-vcs --with-filename "
            "-r '$1' | tr '\n' '#'"))
        "#")))

  (cl-defun jf/project/get-project/project-source-code (&key (directory jf/denote-base-dir))
    "Return the current \"noted\" project name.

Return nil if the current buffer is not part of a noted project.

Noted projects would be found within the given DIRECTORY."
    (when-let* ((project_path_to_code_truename (cdr (project-current))))
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
    (when-let* ((m (and
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

(use-package org-count-words
  :straight (org-count-words :host github :repo "Elilif/org-count-words"))

(use-package uniline
  :straight t)

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
        ]]
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
        ("g c" "Chrome" jf/menu--org-capture-chrome :if jf/browser-chrome?)
        ("g f" "Firefox" jf/menu--org-capture-firefox :if jf/browser-firefox?)
        ("g l" "Librewolf" jf/menu--org-capture-librewolf :if jf/browser-librewolf?)
        ("g m" "Mullvad" jf/menu--org-capture-mullvad :if jf/browser-mullvad?)
        ("g s" "Safari" jf/menu--org-capture-safari :if jf/browser-safari?)
        ("g v" "Vivaldi" jf/menu--org-capture-vivaldi :if jf/browser-vivaldi?)
        ("g w" "Eww" jf/capture/denote/from/eww-data
          :if-derived eww-mode)
        ]
      ["Bookmark"
        ("b b" "Bookmarks" bookmark-bmenu-list)
        ("b c" "Chrome" jf/menu--bookmark-chrome :if jf/browser-chrome?)
        ("b f" "Firefox" jf/menu--bookmark-firefox :if jf/browser-firefox?)
        ("b l" "Librewolf" jf/menu--bookmark-librewolf :if jf/browser-librewolf?)
        ("b m" "Mullvad" jf/menu--bookmark-mullvad :if jf/browser-mullvad?)
        ("b s" "Safari" jf/menu--bookmark-safari :if jf/browser-safari?)
        ("b v" "Vivaldi" jf/menu--bookmark-vivaldi :if jf/browser-vivaldi?)
        ]])
  :bind ("H-1" . #'jf/menu))

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

(org-babel-do-load-languages 'org-babel-load-languages
  (append org-babel-load-languages
    '((emacs-lisp . t)
       (shell . t)
       (mermaid . t)
       (verb . t)
       (plantuml . t)
       (ruby . t))))

(if (file-exists-p (expand-file-name "~/.my-computer"))
  (progn
    (use-package writeroom-mode
      ;; Where olivetti is great for reading...I find writeroom great for
      ;; writing.
      :straight t)

    (advice-add 'text-scale-adjust :after
      #'visual-fill-column-adjust)

    (load "personal.el"))
  (load "work.el"))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(safe-local-variable-values
     '((projectile-git-fd-args .
         "-H -0 -tf --strip-cwd-prefix -c never -E vendor/ -E pkg/ -E docs/ -E .git")
        (jf/org-latex-src-block-skip . t)
        (auto-save-default . nil)
        (elixir-test-command . "mix testall")
        (projectile-git-command .
          "git ls-files -zco --exclude-from=.projectile.gitignore")
        (org-latex-toc-command .
          "\\begin{multicols}{2}\n\\tableofcontents\n\\end{multicols}\\newpage\\begin{multicols}{2}\\\let\\oldhref\\href\n\\renewcommand{\\href}[2]{\\oldhref{#1}{#2}\\footnote{\\url{#1}}}\n")
        (org-latex-toc-command .
          "\\begin{multicols}{1}\n\\tableofcontents\n\\end{multicols}\\newpage\\begin{multicols}{2}\\\let\\oldhref\\href\n\\renewcommand{\\href}[2]{\\oldhref{#1}{#2}\\footnote{\\url{#1}}}\n")
        (org-latex-toc-command .
          "\\begin{multicols}{2}\n\\tableofcontents\n\\end{multicols}\\newpage\\begin{multicols}{3}\\\let\\oldhref\\href\n\\renewcommand{\\href}[2]{\\oldhref{#1}{#2}\\footnote{\\url{#1}}}\n")
        )))

(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
