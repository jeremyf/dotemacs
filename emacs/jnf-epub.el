;;; -*- lexical-binding: t; -*-
;;; jnf-epub.el --- Summary
;;;
;;; Commentary:
;;
;;  This package includes the various configurations for epub reading.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://depp.brause.cc/nov.el/
(use-package nov
  :straight t
  :mode (("\\.epub\\'" . nov-mode)))

;; Justify paragraphs algorithm
(use-package justify-kp
  :straight (justify-kp :host github :type git :repo "Fuco1/justify-kp"))

(use-package visual-fill-column
  :straight t)

(setq-default split-window-preferred-function 'visual-fill-column-split-window-sensibly)

(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "ETBembo"
                           :height 1.3))
(add-hook 'nov-mode-hook 'my-nov-font-setup)

(setq nov-text-width 80)
(setq nov-text-width t)
(setq visual-fill-column-center-text t)
(add-hook 'nov-mode-hook 'visual-line-mode)
(add-hook 'nov-mode-hook 'visual-fill-column-mode)

(defun my-nov-window-configuration-change-hook ()
  (my-nov-post-html-render-hook)
  (remove-hook 'window-configuration-change-hook
               'my-nov-window-configuration-change-hook
               t))

(defun my-nov-post-html-render-hook ()
  (if (get-buffer-window)
      (let ((max-width (pj-line-width))
            buffer-read-only)
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when (not (looking-at "^[[:space:]]*$"))
              (goto-char (line-end-position))
              (when (> (shr-pixel-column) max-width)
                (goto-char (line-beginning-position))
                (pj-justify)))
            (forward-line 1))))
    (add-hook 'window-configuration-change-hook
              'my-nov-window-configuration-change-hook
              nil t)))

(add-hook 'nov-post-html-render-hook 'my-nov-post-html-render-hook)

(use-package bibliothek
  :straight (bibliothek
             :type git
             :host github
             :repo "cadadr/elisp"
             :files ("bibliothek.el")))


(provide 'jnf-epub.el)
;;; jnf-epub.el ends here