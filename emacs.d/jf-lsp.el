;;; jf-lsp --- LSP considerations -*- lexical-binding: t -*-

;; Copyright (C) 2024 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;;; Code:

(use-package eglot
  :straight t
  ;; :straight (:type built-in)
  ;; The Language Server Protocol (LSP) is a game changer; having access to that
  ;; tooling is very much a nice to have.
  :hook ((css-mode css-ts-mode
           ruby-mode ruby-ts-mode
           ;; yaml-mode yaml-ts-mode
           html-mode html-ts-mode
           js-mode js-ts-mode
           json-mode json-ts-mode
           python-mode python-ts-mode
           scss-mode scss-ts-mode)
          . eglot-ensure)
  :hook ((eglot-managed-mode . jf/eglot-capf)))

;; (defun jf/eglot-ensure ()
;;   (eglot-ensure)
;;   (jf/eglot-capf))

(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

(defun jf/eglot-capf ()
  "Ensure `eglot-completion-at-point' preceeds everything."
  ;; I don't want `eglot-completion-at-point' to trample my other completion
  ;; options.
  ;;
  ;; https://stackoverflow.com/questions/72601990/how-to-show-suggestions-for-yasnippets-when-using-eglot
  (setq-local completion-at-point-functions
    (list (cape-capf-super
            #'eglot-completion-at-point
            #'tempel-expand
            #'cape-file
            #'cape-keyword))))

(use-package eldoc
  ;; Helps with rendering documentation
  ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
  :config
  (setq eldoc-documentation-strategy
    ;; 'eldoc-documentation-enthusiast))
    'eldoc-documentation-compose-eagerly)
  (add-to-list 'display-buffer-alist
    '("^\\*eldoc"
       (display-buffer-reuse-mode-window display-buffer-below-selected)
       (dedicated . t)
       (body-function . prot-window-select-fit-size)))
  :straight t)



(provide 'jf-lsp)
;;; jf-lsp.el ends here
