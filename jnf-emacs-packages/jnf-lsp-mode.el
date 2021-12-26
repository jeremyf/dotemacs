;;; jnf-lsp-mode.el --- Summary
;;
;;; Commentary:
;;
;;  This package provides the lsp-mode behavior.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-mode
  :straight t
  :hook (
         ;; (ruby-mode . lsp)
         (enh-ruby-mode . lsp)
         ;; (html-mode . lsp)
         ;; (bash-mode . lsp)
         )
  :config (setq read-process-output-max (* 1024 1024)
                lsp-completion-provider :capf
                lsp-idle-delay 1.00)
  :custom (lsp-keymap-prefix "C-c C-l")
  :commands (lsp))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; This package provides some nice UI behavior for documentation and linting
;;
;; In particular, I like 'lsp-ui-peek-find-reference
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  ;; :hook (
         ;; (ruby-mode . lsp-ui-mode)
         ;; (ruby-mode . lsp-ui-peek-mode)
         ;; (ruby-mode . lsp-ui-sideline-mode)
         ;; (enh-ruby-mode . lsp-ui-mode)
         ;; (enh-ruby-mode . lsp-ui-peek-mode)
         ;; (enh-ruby-mode . lsp-ui-sideline-mode)
         ;; (sql-mode . lsp-ui-mode)
         ;; (sql-mode . lsp-ui-peek-mode)
         ;; (sql-mode . lsp-ui-sideline-mode)
         ;; (json-mode . lsp-ui-mode)
         ;; (json-mode . lsp-ui-peek-mode)
         ;; (json-mode . lsp-ui-sideline-mode)
  ;;        (typescript-mode . lsp-ui-mode)
  ;;        (typescript-mode . lsp-ui-peek-mode)
  ;;        (typescript-mode . lsp-ui-sideline-mode)
  ;;        (js-mode . lsp-ui-mode)
  ;;        (js-mode . lsp-ui-peek-mode)
  ;;        (js-mode . lsp-ui-sideline-mode)
  ;;        (json-mode . lsp-ui-mode)
  ;;        (json-mode . lsp-ui-peek-mode)
  ;;        (json-mode . lsp-ui-sideline-mode)
         ;; (html-mode . lsp-ui-mode)
         ;; (html-mode . lsp-ui-peek-mode)
         ;; (html-mode . lsp-ui-sideline-mode)
  ;;        (css-mode . lsp-ui-mode)
  ;;        (css-mode . lsp-ui-peek-mode)
  ;;        (css-mode . lsp-ui-sideline-mode)
         ;; (bash-mode . lsp-ui-mode)
         ;; (bash-mode . lsp-ui-peek-mode)
         ;; (bash-mode . lsp-ui-sideline-mode))
  :straight t)

;; By default indent levels are often 4; That is against what I've seen.
(setq ruby-indent-level 2
      typescript-indent-level 2
      js-indent-level 2)

(add-hook 'emacs-lisp-mode 'eldoc-mode)

;; Solargraph is the language tool for lsp-mode and Ruby
(use-package solargraph
  :straight (solargraph :host github :repo "guskovd/emacs-solargraph")
  :custom
  (lsp-solargraph-use-bundler t)
  :bind
  (:map enh-ruby-mode-map ("M-i" . solargraph:complete))
  (:map ruby-mode-map ("M-i" . solargraph:complete)))

;; (use-package tree-sitter
;;   :straight t)

;; (use-package tree-sitter-langs
;;   :straight)

;; (add-hook 'enh-ruby-mode-hook #'tree-sitter-mode)

(provide 'jnf-lsp-mode.el)
;;; jnf-lsp-mode.el ends here
