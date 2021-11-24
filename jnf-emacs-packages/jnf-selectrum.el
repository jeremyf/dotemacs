;;; jnf-selectrum.el --- Summary -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package prescient
  :straight t)

(use-package selectrum
  :straight t
  :config
  (selectrum-mode +1)
  :bind ("C-x C-z" . selectrum-repeat))

(use-package selectrum-prescient
  :straight t
  :after (prescient selectrum)
  :config ;; to make sorting and
           (selectrum-prescient-mode +1)
           ;; filtering more intelligent to save your command history
           ;; on disk, so the sorting gets more intelligent over time
           (prescient-persist-mode +1))

(provide 'jnf-selectrum.el)
;;; jnf-selectrum.el ends here
