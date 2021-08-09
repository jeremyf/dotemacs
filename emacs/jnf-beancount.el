;;; jnf-beancount.el --- Summary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  This package provides most of the Ruby configuration
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package beancount-mode
  :straight (beancount-mode :type git :host github :repo "beancount/beancount-mode")
  :bind (:map beancount-mode-map (
                                  ("C-c C-n" . 'outline-next-visible-heading)
                                  ("C-c C-p" . 'outline-previous-visible-heading)
                                  ))
  :mode (("\\.beancount\\'" . beancount-mode)))

(add-hook 'beancount-mode-hook
          (lambda () (setq-local electric-indent-chars nil)))
(add-hook 'beancount-mode-hook #'outline-minor-mode)
(provide 'jnf-beancount.el)
;;; jnf-beancount.el ends here
