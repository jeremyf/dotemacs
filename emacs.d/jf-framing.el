;;; jf-framing.el --- How Emacs presents the frame -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;;
;; This personal package provides logic for both a focus mode and presentation
;; mode.  There are several assumptions about packages, but I guard those
;; assumptions with `fboundp'.
;;
;; To enter focus mode: "M-x logos-focus-mode"
;; To enter presentation mode: "M-x jf/presenter-minor-mode"

;;; Code
(use-package edit-indirect :straight t)

(use-package logos
  :straight t
  :config
  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim))
  ;; (let ((map logos-focus-mode-map))
  ;;   (define-key map [remap next-line] #'logos-forward-page-dwim)
  ;;   (define-key map [remap previous-line] #'logos-backward-page-dwim))
  (setq logos-outlines-are-pages t)
  (setq-default logos-hide-cursor t
		logos-hide-mode-line t
		logos-hide-buffer-boundaries t
		logos-hide-fringe t
		logos-variable-pitch t
		logos-buffer-read-only t
		logos-scroll-lock nil
		logos-olivetti t
		logos-outline-regexp-alist
		`((emacs-lisp-mode . "^;;;+ ")
		  (org-mode . "^\\*+ +")
		  (markdown-mode . "^\\#+ +")))

  (defun logos--reveal-entry ()
    "Reveal Org or Outline entry."
    (cond
     ((and (eq major-mode 'org-mode)
	   (org-at-heading-p))
      (org-show-subtree))
     ((or (eq major-mode 'outline-mode)
	  (bound-and-true-p outline-minor-mode))
      (outline-show-subtree))))
  :init
  (add-hook 'logos-page-motion-hook #'logos--reveal-entry))

(use-package "nov.el" :straight t
	     :init (use-package esxml :straight t)
	     :config
	     (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
	     :custom (nov-text-width 80))

(use-package so-long
  :defer t
  :straight t
  :bind
  (:map so-long-mode-map
        ("C-s" . isearch-forward)
        ("C-r" . isearch-backward))
  :config (global-so-long-mode 1))

(use-package olivetti
  :straight t
  :custom
  (olivetti-body-width 0.6)
  (olivetti-minimum-body-width 80)
  (olivetti-recall-visual-line-mode-entry-state t))

;;; Presentation mode leveraging logos
(require 'jf-minor-mode-maker)

(defvar jf/presenter-minor-mode-map (let ((map (make-sparse-keymap)))
					(define-key map (kbd "C-n") #'next-line)
					(define-key map (kbd "C-p") #'previous-line)
					(dolist (key `("M-]" "s-]"))
					  (define-key map (kbd key) #'logos-forward-page-dwim))
					(dolist (key `("M-[" "s-["))
					  (define-key map (kbd key) #'logos-backward-page-dwim))
					map))

(jf/minor-mode-maker :title "Logos Presenter"
		     :abbr "presenter"
		     :keymap jf/presenter-minor-mode-map)

(defcustom jf/presenter-minor-mode-on-hook
  (lambda ()
    (let ((logos-hide-cursor nil)
	  (logos-buffer-read-only nil)
	  (org-hide-emphasis-markers t))
      (call-interactively 'logos-narrow-dwim)
      (olivetti-mode t)
      (keycast-mode-line-mode t)
      (display-line-numbers-mode -1)
      (when (fboundp 'fontaine-set-preset) (fontaine-set-preset 'presenting))
      (when (fboundp 'vi-tilde-fringe-mode) (vi-tilde-fringe-mode -1))
      (when (fboundp 'git-gutter-mode) (git-gutter-mode -1))
      (when (fboundp 'centaur-tabs-local-mode) (centaur-tabs-local-mode -1))))
  "Hook when `jf/presenter-minor-mode' activated."
  :type 'hook)

(defcustom jf/presenter-minor-mode-off-hook
  (lambda ()
    (call-interactively 'widen)
    (olivetti-mode -1)
    (keycast-mode-line-mode t)
    ;; (setq-local  org-hide-emphasis-markers nil)
    (display-line-numbers-mode t)
    (when (fboundp 'fontaine-set-preset) (fontaine-set-preset 'default))
    (when (fboundp 'vi-tilde-fringe-mode) (vi-tilde-fringe-mode t))
    (when (fboundp 'git-gutter-mode) (git-gutter-mode t))
    (when (fboundp 'centaur-tabs-local-mode) (centaur-tabs-local-mode t)))
  "Hook when `jf/presenter-minor-mode' deactivated."
  :type 'hook)
(provide 'jf-framing)
;;; jf-framing.el ends here
