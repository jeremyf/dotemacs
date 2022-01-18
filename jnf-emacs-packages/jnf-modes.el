;; -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Intended to be a place for programminig language modes
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emmet-mode
  :straight t
  :bind (("C-c C-e" . emmet-expand-yas ))
  :hook ((sgml-mode . emmet-mode)
         (html-mode . emmet-mode)
         (css-mode . emmet-mode)))

;; https://github.com/AdamNiederer/vue-mode
;; (use-package vue-mode
;;   :straight t
;;   :mode (("\\.vue\\'" . vue-mode)))

(use-package web-mode
  :straight t
  :config (setq web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

;; built-in, consider commenting
;; (use-package sgml-mode
;;   :straight nil
;;   :hook
;;   (html-mode . sgml-electric-tag-pair-mode)
;;   (html-mode . sgml-name-8bit-mode)
;;   :custom
;;   (sgml-basic-offset 2))

(use-package plantuml-mode
  :config (setq plantuml-executable-path (concat (getenv "HB_PATH") "/bin/plantuml")
                plantuml-default-exec-mode 'executable
                org-plantuml-executable-path (concat (getenv "HB_PATH") "/bin/plantuml")
                org-plantuml-exec-mode 'executable)
  :mode (("\\.plantuml\\'" . plantuml-mode))
  :straight t)

(use-package json-mode
  :straight t)

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'jnf/eval-region-dwim)
(define-key lisp-interaction-mode-map (kbd "C-c C-c") 'jnf/eval-region-dwim)
(defun jnf/eval-region-dwim ()
  "When region is active, evaluate it and kill the mark. Else,
evaluate the whole buffer."
  (interactive)
  (if (not (region-active-p))
      (progn
	(eval-buffer)
	(message "Evaluated buffer"))
    (progn
      (eval-region (region-beginning) (region-end))
      (message "Evaluated region"))
    (setq-local deactivate-mark t)))

;; Compressed JSON sure is ugly and illegible; This solves that
;; problem.
(use-package json-reformat
  :straight t
  :after json-mode
  :init (setq json-reformat:indent-width 2))

(use-package go-mode
  :straight t)

;; Open svg files in xml-mode (instead of image rendering mode)
(add-to-list `auto-mode-alist '("\\.svg\\'" . xml-mode))

(use-package markdown-mode
  :straight t
  :hook ((markdown-mode . turn-on-visual-line-mode))
  ;; I use markdown for my blogging platform and very little else.
  ;; Hence, I have this keybind.
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/pandoc"))

(use-package yaml-mode
  :straight t)

(use-package lua-mode
  :straight t)

(use-package git-modes
  :straight t)