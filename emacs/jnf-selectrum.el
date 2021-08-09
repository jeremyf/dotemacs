;;; jnf-selectrum.el --- Summary -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package selectrum
  :straight t
  :config
  (selectrum-mode +1)
  :bind ("C-x C-z" . selectrum-repeat))

(use-package ripgrep
  :init (setq ripgrep-arguments "--ignore-case")
  :straight t)

(use-package selectrum-prescient
  :straight t
  :after prescient
  :config ;; to make sorting and
           (selectrum-prescient-mode +1)
           ;; filtering more intelligent to save your command history
           ;; on disk, so the sorting gets more intelligent over time
           (prescient-persist-mode +1))

(use-package savehist
  :straight t)

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :after (selectrum)
  :straight t
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle' to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  )

;; Example configuration for Consult
;; https://github.com/minad/consult
(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complet-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("s-b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-s-b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-from-kill-ring)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("C-x C-SPC" . consult-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-c f" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Customizations that map to ivy
         ("s-r" . consult-recent-file)
         ("C-c o" . consult-file-externally)
         ("<f4>" . consult-bookmark)
         ("s-4" . consult-bookmark)
         ("C-y" . yank)
         ("C-s" . consult-line) ;; I've long favored Swiper mapped to c-s
         ("s-l" . consult-goto-line)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         ;; ("s-t" . jnf/consult-find-using-fd)
         ("<f3>" . consult-imenu)
         ("s-3" . consult-imenu)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line))                 ;; required by consult-line to detect isearch

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)


  ;; From https://github.com/minad/consult/wiki#find-files-using-fd
  ;; Note: this requires lexical binding
  (defun jnf/consult-find-using-fd (&optional dir initial)
    "Find project files.

A replacement for `projectile-find-file'."
    (interactive "P")
    (let ((consult-find-command "fd --color=never --hidden --exclude .git/ --full-path ARG OPTS"))
      (consult-find dir initial)))

  (defun jnf/consult-line (consult-line-function &rest rest)
  "Advising function around `CONSULT-LINE-FUNCTION'.

When there's an active region, use that as the first parameter
for `CONSULT-LINE-FUNCTION'.  Otherwise, use the current word as
the first parameter.  This function handles the `REST' of the
parameters."
  (interactive)
  (apply consult-line-function
         (if (use-region-p)
             (buffer-substring (region-beginning) (region-end))
           (thing-at-point 'symbol))
           rest))

  (defun jnf/consult-ripgrep (consult-ripgrep-function &optional dir &rest rest)
    "Use region or thing at point to populate initial parameter for `CONSULT-RIPGREP-FUNCTION'.

When there's an active region, use that as the initial parameter
for the `CONSULT-RIPGREP-FUNCTION'.  Otherwise, use the thing at
point.

`DIR' use the universal argument (e.g. C-u prefix) to first set
the directory.  `REST' is passed to the `CONSULT-RIPGREP-FUNCTION'."
    (interactive "P")
    (apply consult-ripgrep-function
           dir
           (if (use-region-p) (buffer-substring (region-beginning) (region-end)) (thing-at-point 'symbol))
           rest))

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'consult-line :around #'jnf/consult-line '((name . "wrapper")))
  (advice-add #'consult-ripgrep :around #'jnf/consult-ripgrep '((name . "wrapper")))

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Updating the default to include "--ignore-case"
  (setq consult-ripgrep-command "rg --null --line-buffered --color=ansi --max-columns=1000 --ignore-case --no-heading --line-number . -e ARG OPTS")

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. Note that the preview-key can also be
  ;; configured on a per-command basis via `consult-config'. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-p"))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; Probably not needed if you are using which-key.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from:
  ;; * projectile-project-root
  ;; * vc-root-dir
  ;; * project-roots
  ;; * locate-dominating-file
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  ;; (setq consult-project-root-function
  ;;       (lambda ()
  ;;         (when-let (project (project-current))
  ;;           (car (project-roots project)))))
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;; (setq consult-project-root-function
  ;;       (lambda () (locate-dominating-file "." ".git")))
  )



;; Optionally add the `consult-flycheck' command.
(use-package consult-flycheck
  :straight t
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))

;; https://github.com/oantolin/embark
(use-package embark
  :straight t
  :bind
  (("C-s-a" . embark-act)       ;; pick some comfortable binding
   ("C-s-e" . embark-export)
   ("C-h b" . embark-bindings))
  :hook (embark-pre-action . refresh-selectrum)
  :init
  (defun refresh-selectrum ()
    (setq selectrum--previous-input-string nil))
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (setq embark-action-indicator
      (lambda (map &optional _target)
        (which-key--show-keymap "Embark" map nil nil 'no-paging)
        #'which-key--hide-popup-ignore-command)
      embark-become-indicator embark-action-indicator)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

;; Useful for editing grep results:
;;
;; 1) "C-c f" invoke `consult-ripgrep'
;; 2) "C-s-e" invoke `embark-export' (On OS X map that's Ctrl+Cmd+e)
;; 3) "e" or "C-c C-p" invoke `wgrep-change-to-wgrep-mode'
;; 4) Save or cancel
;;    a) Save: "C-x C-s" invoke `save-buffer' (or "C-c C-c")
;;    b) Cancel: "C-c C-k"
(use-package wgrep
  :after (embark-consult ripgrep)
  :straight t
  :bind (:map wgrep-mode-map
              ;; Added keybinding to echo Magic behavior
              ("C-c C-c" . save-buffer)
         :map grep-mode-map
         ("e" . wgrep-change-to-wgrep-mode)
         :map ripgrep-search-mode-map
         ("e" . wgrep-change-to-wgrep-mode)))

;; https://github.com/gagbo/consult-lsp
(use-package consult-lsp
  :after (consult lsp-mode)
  :straight (consult-lsp :host github :type git :repo "gagbo/consult-lsp")
  :commands consult-lsp-symbols)

;; https://github.com/minad/vertico
;;
;; See System Crafters - https://www.reddit.com/r/emacs/comments/neh3d7/streamline_your_emacs_completions_with_vertico/
;; Vertico and Selectrum provide similar functions.  Favor selectrum.
;; (use-package vertico
;;   :straight t
;;   :custom (vertico-cycle t)
;;   :init (vertico-mode))

;; https://github.com/minad/orderless
;;
;; Useful for not requiring strict word order
(use-package orderless
  :straight t
  :demand t
  :config
  ;; See
  ;; https://github.com/minad/consult/wiki#orderless-style-dispatchers-ensure-that-the--regexp-works-with-consult-buffer
  ;; for background of this function.
  ;;
  ;; Recognizes the following patterns:
  ;; * ~flex flex~
  ;; * =literal literal=
  ;; * `initialism initialism`
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun dm/orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern) `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; File extensions
     ((string-match-p "\\`\\.." pattern) `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ((string-suffix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))
        orderless-style-dispatchers '(dm/orderless-dispatch)))

(provide 'jnf-selectrum.el)
;;; jnf-selectrum.el ends here
