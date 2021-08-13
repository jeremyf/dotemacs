;;; jnf-projectile.el --- Summary -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://docs.projectile.mx/en/latest/
;;
;; Helpful for understanding the likely bounds of directory structure
(use-package projectile
  :straight t
  :diminish 'projectile-mode
  :config
  ;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  :custom
  (projectile-project-search-path '("~/git/"))
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map)
         ("s-t" . projectile-find-file)
         ("s-." . projectile-toggle-between-implementation-and-test)))


(provide 'jnf-projectile.el)
;;; jnf-projectile.el ends here
