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
  :after pretty-hydra
  :diminish 'projectile-mode
  :config
  (projectile-mode +1)
  (defvar jnf/projectile--title (with-octicon "file-directory" "Projectile" 1 -0.05))
  (pretty-hydra-define jnf/projectile--menu (:foreign-keys warn :title jnf/projectile--title :quit-key "q" :exit t)
    ("Projectile" (
                   ("b" projectile-switch-to-buffer "Buffer Select…")
                   ("d" projectile-dired            "Dired to project root…")
                   ("f" projectile-find-file-dwim   "Find File DWIM…")
                   ("k" projectile-kill-buffers     "Kill Buffers…")
                   ("o" projectile-multi-occur      "Occurs in Project…")
                   ("r" projectile-recentf          "Recent File Select…")
                   ("t" projectile-find-file        "Find File (aka CMD+t)…")
                   )))
  :custom
  (projectile-project-search-path '("~/git/"))
  :bind
  (:map projectile-mode-map (("C-c p" . projectile-command-map)))
  :bind
  ("s-t" . projectile-find-file)
  ("C-c p" . projectile-command-map)
  ("C-s-p". jnf/projectile--menu/body)
  ("s-." . projectile-toggle-between-implementation-and-test))

(provide 'jnf-projectile.el)
;;; jnf-projectile.el ends here
