;;; jnf-eglot.el --- Summary
;;
;;; Commentary:
;;
;;  This package provides the lsp-mode behavior.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :commands eglot
  :straight t
  :config
  (setq eldoc-idle-delay 0
        eglot-connect-timeout 10
        eglot-send-changes-idle-time 0.5
        eglot-sync-connect 1
        eglot-autoshutdown t)
  (add-to-list 'eglot-server-programs '(enh-ruby-mode . ("solargraph" "socket" "--port" :autoport))))

(use-package solargraph
  :straight (solargraph :host github :repo "guskovd/emacs-solargraph")
  :config (setq lsp-solargraph-use-bundler 1)
  :bind
  (:map enh-ruby-mode-map ("M-i" . solargraph:complete))
  (:map ruby-mode-map ("M-i" . solargraph:complete)))

(provide 'jnf-eglot.el)
;;; jnf-eglot.el ends here
