;;; jnf-typescript.el --- Summary
;;
;;; Commentary:
;;
;;  Modes for typescript and lua.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package typescript-mode
  :straight t
  :mode (("\\.ts\\'" . typescript-mode)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package tide
  :straight t
  :bind (:map typescript-mode-map ("C-l ." . 'tide-references))
  :hook ((typescript-mode . setup-tide-mode)))

(provide 'jnf-typescript.el)
;;; jnf-typescript.el ends here
