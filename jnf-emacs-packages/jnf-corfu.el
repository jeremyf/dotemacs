;;; jnf-corfu.el --- Summary
;;
;;; Commentary:
;;
;;
;;  Extracting company usage
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configure corfu
(use-package corfu
  :straight t
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :init

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  (corfu-global-mode)

  :config
  ;; Optionally enable cycling for `corfu-next' and `corfu-previous'.
  (setq corfu-cycle t)
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let (completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer))

;; https://github.com/minad/orderless
;;
;; Useful for not requiring strict word order
;; (use-package orderless
;;   :straight t
;;   :init
;;    (setq completion-styles '(orderless)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles . (partial-completion))))))


;; Dabbrev works with Corfu
(use-package dabbrev
  :straight t
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))


(provide 'jnf-corfu.el)
;;; jnf-corfu.el ends here
