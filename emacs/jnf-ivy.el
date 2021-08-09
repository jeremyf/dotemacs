;;; jnf-ivy.el --- Summary -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://oremacs.com/swiper/
;; Note: I've set all searches to use fuzzy regex
(use-package ivy
  :straight t
  :after avy projectile
  :diminish (ivy-mode . "")
  :bind (("C-c C-r" . ivy-resume))
  :config (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 12)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-ignore-order))))

(setq projectile-completion-system 'ivy)

;; Part of the ivy/counsel/swiper trio
(use-package counsel
  :straight t
  :after helpful
  :init (setq ivy-use-selectable-prompt t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x 8 RET" . counsel-unicode-char)
         ("<f4>" . counsel-bookmark)
         ("s-4" . counsel-bookmark)
         ("s-r" . counsel-recentf))
  :config (counsel-mode 1)
  (defalias 'recent 'counsel-recentf))
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(use-package all-the-icons-ivy-rich
  :straight t
  :after (ivy counsel counsel-projectile)
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :straight t
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer nil)
  (ivy-rich-path-style 'full)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode 1))

(use-package ivy-prescient
  :straight t
  :after prescient
  :init (setq prescient-filter-method '(literal fuzzy regexp initialism))
  :config (ivy-prescient-mode t))

(use-package company-prescient
  :straight t
  :after prescient
  :config (company-prescient-mode t))

;; M-o e to open counsel edit mode
;;
;; https://github.com/tyler-dodge/counsel-edit-mode
;; From README:
;;
;; > Once installed, run any of the counsel search commands. Once they
;; > start returning results, type `M-o e` to open up the
;; > `counsel-edit-mode` buffer to edit the results.
;;
;; C-c C-c commits the changes (note analogue to Magit)
;; C-c C-k discards the changes (note analogue to Magit)
;;
;; I suspect that this deprecates wgrep-ag
(use-package counsel-edit-mode
  :straight (counsel-edit-mode :host github :type git :repo "tyler-dodge/counsel-edit-mode")
  :after counsel
  :init (counsel-edit-mode-setup-ivy))


(use-package counsel-projectile
  :straight t
  :after projectile
  :bind ("s-t" . counsel-projectile-find-file)) ; CMD+t, which I carry over from Textmate

;; The silver searcher; I found ripgrep a bit nicer, but wait until
;; you try wgrep-ag
(use-package ag
  :straight t
  :after counsel
  :init
  ;; There are two paths into ag, I most often (like 99.9% of the
  ;; time) use the counsel-ag.  I want both ways into ag to be
  ;; similar.
  ;;
  ;; I've added "--hidden --ignore-dir .git" to both of the default
  ;; cases.
  (setq counsel-ag-base-command "ag --hidden --ignore-dir .git --vimgrep %s"
        ag-arguments (list "--smart-case" "--stats" "--hidden" "--ignore" ".git"))
  :bind (("C-c f" . counsel-ag)))

;; This package is amazing!!!  Render search results to a buffer, edit
;; the buffer and write back to the file hits.  There is not a ripgrep
;; option.
;;
;; Search via ag, see candidates and use ivy to show ALL candidates,
;; then wgrep to edit those candidates and save
;;
;; 1) M-s-f 'counsel-ag
;; 2) C-c C-o 'ivy-occur
;; 3) C-c C-p 'wgrep-toggle-readonly-area
;; 4) C-x C-s to save OR C-x C-q to exit without save
;;
;; Consider counsel-edit-mode as a possible deprecation for this
(use-package wgrep-ag
  :straight t
  :hook (ag-mode . wgrep-ag-setup)
  :after ag)

;; I use this all of the freaking time to peak at other parts of my
;; code.  It's also a navigation tool.
(use-package swiper
  :straight t
  :bind (("C-s" . swiper)))

(global-set-key (kbd "<f3>") 'counsel-imenu)
(global-set-key (kbd "s-3") 'counsel-imenu)

(use-package lsp-ivy
  :after (ivy lsp-mode)
  :straight t
  :commands lsp-ivy-workspace-symbol)

;; I believe this means I should first ensure that I've loaded ivy.
(with-eval-after-load "magit"
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package flyspell-correct-ivy
  :after (flyspel ivy)
  :straight t
  :config (global-set-key (kbd "C-,") 'flyspell-buffer))

(global-set-key (kbd "s-b") 'switch-to-buffer) ;; CMD+b
(global-set-key (kbd "C-s-b") 'switch-to-buffer-other-window) ;; CTRL+CMD+b

(provide 'jnf-ivy.el)
;;; jnf-ivy.el ends here
