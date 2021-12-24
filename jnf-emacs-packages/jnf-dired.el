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

(use-package dired-sidebar
  :straight t
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (setq dired-sidebar-use-term-integration t
        dired-sidebar-theme 'vscode
        dired-sidebar-use-custom-font t)
  :commands (dired-sidebar-toggle-sidebar))

(provide 'jnf-dired.el)
;;; jnf-dired.el ends here
