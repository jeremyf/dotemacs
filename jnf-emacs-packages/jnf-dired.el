;;; jnf-dired.el --- Summary
;;
;;; Commentary:
;;
;;  Functions for the dired mode
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dired-subtree
  :bind (:map dired-mode-map ("C-, t" . 'dired-subtree-toggle))
  :straight t)


(provide 'jnf-dired.el)
;;; jnf-dired.el ends here
