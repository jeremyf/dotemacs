;;; jnf-tabs.el --- Summary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  This package provides the tabs behavior.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-tab-line-mode t)
(global-set-key (kbd "s-{") 'previous-buffer)
(global-set-key (kbd "s-}") 'next-buffer)

;; https://github.com/lukhas/buffer-move
(use-package buffer-move
  :straight t
  :bind
  ;; Cmd+F12 to then choose a buffer to move
  ("<s-f12>" . buf-move))

;; "The long-awaited Emacs 27 support for native tabs is shaky, both
;; visually and in terms of functionality. As such, centaur-tabs is
;; the best way to simulate a conventional tabs setup, in which tab
;; sets are grouped by the toplevel project working directory."
;; https://blog.sumtypeofway.com/posts/emacs-config.html
;; (use-package centaur-tabs
;;   :straight t
;;   :demand
;;   :config
;;   (centaur-tabs-mode t)
;;   (setq centaur-tabs-show-navigation-buttons t
;;         centaur-tabs-set-icons t
;;         centaur-tabs-cycle-scope 'tabs
;;         centaur-tabs-set-bar 'under)
;;   (centaur-tabs-headline-match)
;;   (defun jnf-centaur-tabs-buffer-groups ()
;;     "A function to define the centaur tabs's buffer group rules.

;; There are four groups:

;; - RSS :: My RSS feed
;; - System :: The *message* and *scratch* type buffers
;; - Help :: When I request help
;; - Editor :: Everything else"
;;     (list
;;      (cond
;;       ((string-equal "*el" (substring (buffer-name) 0 3)) "RSS")
;;       ((string-equal "*" (substring (buffer-name) 0 1)) "System")
;;       ((memq major-mode '(helpful-mode help-mode)) "Help")
;;       (t "Editor"))))
;;   :custom
;;   (centaur-tabs-change-fonts "JetBrains Mono" 90)
;;   (centaur-tabs-height 30)
;;   (centaur-tabs-gray-out-icons 'buffer)
;;   (centaur-tabs-style "rounded")
;;   (centaur-tabs-set-modified-marker t)
;;   (centaur-tabs-modified-marker "●")
;;   (centaur-tabs-buffer-groups-function #'jnf-centaur-tabs-buffer-groups)
;;   :bind (("s-{" . #'centaur-tabs-backward)
;;          ("s-}" . #'centaur-tabs-forward)
;;          ("C-c C-5". #'centaur-tabs-extract-window-to-new-frame)
;;          ([s-up] . #'centaur-tabs-backward-group)
;;          ([s-down] . #'centaur-tabs-forward-group)
;;          ("C-s-t" . #'centaur-tabs-counsel-switch-group)))


(provide 'jnf-tabs.el)
;;; jnf-tabs.el ends here
