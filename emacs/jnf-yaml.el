;;; jnf-yaml.el --- Summary
;;
;;; Commentary:
;;
;;  Modes for yaml
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yaml-mode
  :bind (:map yaml-mode-map ("C-c t" . jnf/tor-subject-menu-yaml/body))
  :straight t)

;; A pure Lisp implementation
;; (use-package yaml
;;   :straight t)

;; A rust implementation
;; (use-package yamlmod-wrapper
;;   :straight (:host github :type git :repo "perfectayush/emacs-yamlmod"))

(provide 'jnf-yaml.el)
;;; jnf-yaml.el ends here
