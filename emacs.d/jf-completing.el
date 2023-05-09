;;; jf-completing.el --- Packages of the "completing" variety -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;; These packages help configure the "completing" activity in coding.  The
;; general idea of find me something.

;;; Code:

;;;; Other packages and their configurations

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
  :straight (:host github :repo "ChanderG/lam"))

(use-package emacs
  :bind ("C-M-i" . completion-at-point)
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
          ("C-s-b" . consult-buffer-other-window)
          ("C-x 5 b" . consult-buffer-other-frame)
          ;; Custom M-# bindings for fast register access
          ("M-#" . consult-register-load)
          ("M-'" . consult-register-store)
          ("M-`" . consult-register)
          ;; Other custom bindings
          ("C-y" . yank)
          ("C-c C-/" . #'consult-clock-in)
          ("M-y" . consult-yank-from-kill-ring)
          ("<help> a" . consult-apropos)
          ("M-s k" . consult-keep-lines)
          ("M-s u" . consult-focus-lines)
          ;; M-g bindings (goto-map)
          ("M-g e" . consult-compile-error)
          ("M-g g" . consult-goto-line)
          ("H-o" . consult-org-agenda)
          ("C-c o" . consult-org-agenda)
          ("M-g M-o" . consult-org-agenda)
          ("M-g M-g" . consult-goto-line)
          ("s-l" . consult-goto-line)
          ("C-l" . consult-goto-line)
          ("M-g o" . consult-outline)
          ("M-g m" . consult-mark)
          ("M-g M" . consult-global-mark)
          ("C-x C-SPC" . consult-global-mark)
          ("M-i" . consult-imenu)
          ("M-g i" . consult-imenu)
          ("M-g I" . consult-imenu-multi)
          ;; M-s bindings (search-map)
          ("M-s f" . consult-find)
          ("M-s L" . consult-locate)
          ;; ("M-s g" . consult-grep)
          ("M-s G" . consult-git-grep)
          ("M-s r" . consult-ripgrep)
          ("C-c f" . consult-ripgrep)
          ("M-s l" . consult-line)
          ("M-s L" . consult-line-multi)
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
      "--glob !vendor/ --glob !coverage/ --glob !**/tmp/ --glob !**/log/ "
      "--glob !public/ --glob !node_modules/ --glob !.git/ --glob !doc/ "
      "--glob !.yardoc/ "
      " . -e ARG OPTS"))
  (consult-ripgrep-args
    (concat "rg --null --hidden --line-buffered --color=never --max-columns=1000 "
      "--path-separator / --no-ignore-vcs --smart-case --no-heading "
      "--glob !vendor/ --glob !coverage/ --glob !**/tmp/ --glob !**/log/ "
      "--glob !public/ --glob !node_modules/ --glob !.git/ --glob !doc/ "
      "--glob !.yardoc/ "
      "--line-number "))
  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :preface
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
               (buffer-substring-no-properties (region-beginning) (region-end)))
    ;; https://github.com/minad/consult/wiki#org-clock
    consult-clock-in
    :prompt "Clock in: "
    :preview-key "M-."
    :group
    (lambda (cand transform)
      (let* ((marker (get-text-property 0 'consult--candidate cand))
              (name (if (member marker org-clock-history)
                      "*Recent*"
                      (buffer-name (marker-buffer marker)))))
        (if transform (substring cand (1+ (length name))) name))))
  (setq consult-project-root-function #'project-root))


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

(use-package corfu
  ;; Completion overlay; a narrower intreface than the more verbose company.
  :straight t
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
          ("M-m" . corfu-move-to-minibuffer)
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
  :config
  (defun corfu-move-to-minibuffer ()
    "Move \"popup\" completion candidates to minibuffer.
Useful if you want a more robust view into the recommend candidates."
    (interactive)
    (let (completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  :init
  ;; (corfu-indexed-mode)
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  (global-corfu-mode)
  (load "~/.emacs.d/straight/build/corfu/extensions/corfu-indexed.el"
    nil
    jf/silence-loading-log)
  (corfu-indexed-mode)
  (load "~/.emacs.d/straight/build/corfu/extensions/corfu-info.el"
    nil
    jf/silence-loading-log)
  (load "~/.emacs.d/straight/build/corfu/extensions/corfu-popupinfo.el"
    nil
    jf/silence-loading-log)
  (corfu-popupinfo-mode))

(use-package cape
  ;; Completion at point functions, with the amazing `cape-super-capf' for
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
      ("d" "Docs" devdocs-lookup)
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
    completion-category-overrides '((file (styles partial-completion))
                                     ;; enable initialism by default for symbols
                                     (command (styles +orderless-with-initialism))
                                     (variable (styles +orderless-with-initialism))
                                     (symbol (styles +orderless-with-initialism)))
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
    (s-split
      "\n"
      (s-trim
        (shell-command-to-string
          (concat
            "rg \"\\{\\{\\{"
            macro-name
            "\\((.+?)\\)\\}\\}\\}"
            "\" --only-matching --no-filename -r '$1' "
            dir
            " | sort | uniq")))))
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
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (tempel-global-abbrev-mode)
  :init
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  ;; Hyper Macro!
  (tempel-key "H-r b" blockquote_block org-mode-map)
  (tempel-key "H-r u" update_block org-mode-map)
  (tempel-key "H-m c" cite org-mode-map)
  (tempel-key "H-m i" idiomatic org-mode-map)
  (tempel-key "H-m k" keyboard org-mode-map))

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
    "Restrict set of candidates to visible candidates"
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
  (vertico-mode)
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
    (lambda (&rest args)
      (apply (if vertico-mode
               #'consult-completion-in-region
               #'completion--in-region)
        args)))
  (setq read-file-name-completion-ignore-case t
    read-buffer-completion-ignore-case t
    completion-ignore-case t)
  (setq vertico-cycle t)
  :init
  ;; Type "C-3 return" and select the 3rd candidate in the list.
  (load "~/.emacs.d/straight/build/vertico/extensions/vertico-indexed.elc"
    nil
    jf/silence-loading-log)
  (vertico-indexed-mode)
  (load "~/.emacs.d/straight/build/vertico/extensions/vertico-directory.elc"
    nil
    jf/silence-loading-log)
  (load "~/.emacs.d/straight/build/vertico/extensions/vertico-repeat.elc"
    nil
    jf/silence-loading-log)
  (global-set-key (kbd "M-r") #'vertico-repeat)
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
  (which-key-max-description-length 90)
  (which-key-show-docstrings t)
  (which-key-add-column-padding 2)
  :config
  (which-key-mode)
  (which-key-setup-side-window-right)
  (which-key-show-major-mode))

(provide 'jf-completing)
;;; jf-completing.el ends here
