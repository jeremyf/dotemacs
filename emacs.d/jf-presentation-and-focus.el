;;; jf-presentation-and-focus.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; This personal package provides logic for both a focus mode and presentation
;; mode.  There are several assumptions about packages, but I guard those
;; assumptions with `fboundp'.
;;
;; To enter focus mode: "M-x logos-focus-mode"
;; To enter presentation mode: "M-x jf/lp-minor-mode"

;;; Code:

(use-package esxml :straight t)

(use-package "nov.el" :straight t
  :config (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :custom (nov-text-width 80))

(use-package olivetti
  :straight t
  :custom
  (olivetti-body-width 0.6)
  (olivetti-minimum-body-width 80)
  (olivetti-recall-visual-line-mode-entry-state t))

(use-package logos
  :straight t
  :config
  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim))
  ;; This is not the correct map; but it’s close to what I want.
  (let ((map logos-focus-mode-map))
    (define-key map [remap next-line] #'logos-forward-page-dwim)
    (define-key map [remap previous-line] #'logos-backward-page-dwim))
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
		  (markdown-mode . "^\\#+ +"))))

(defun logos--reveal-entry ()
  "Reveal Org or Outline entry."
  (cond
   ((and (eq major-mode 'org-mode)
	 (org-at-heading-p))
    (org-show-subtree))
   ((or (eq major-mode 'outline-mode)
	(bound-and-true-p outline-minor-mode))
    (outline-show-subtree))))
(add-hook 'logos-page-motion-hook #'logos--reveal-entry)

;;; Presentation mode leveraging logos
(defvar jf/lp-minor-mode-map (let ((map (make-sparse-keymap)))
			       (dolist (key `("]" "RET" "SPC" "<right>" "<down>" "n" "C-n"))
				 (define-key map (kbd key) #'logos-forward-page-dwim))
			       (dolist (key `("[" "DEL" "<left>" "<up>" "C-p" "p"))
				 (define-key map (kbd key) #'logos-backward-page-dwim))
			       map))

(jf/minor-mode-maker :title "Logos Presenter"
		     :abbr "LP"
		     :keymap jf/lp-minor-mode-map)

(defcustom jf/lp-minor-mode-on-hook
  (lambda ()
    (call-interactively 'logos-narrow-dwim)
    (setq-local  org-hide-emphasis-markers t)
    (read-only-mode 1)
    (display-line-numbers-mode -1)
    (org-indent-mode -1)
    (when (fboundp 'fontaine-set-preset) (fontaine-set-preset 'presenting))
    (when (fboundp 'logos-focus-mode) (logos-focus-mode 1))
    (when (fboundp 'vi-tilde-fringe-mode) (vi-tilde-fringe-mode -1))
    (when (fboundp 'git-gutter-mode) (git-gutter-mode -1))
    (when (fboundp 'centaur-tabs-local-mode) (centaur-tabs-local-mode -1)))
  "Hook when `jf/lp-minor-mode' activated."
  :type 'hook)

(defcustom jf/lp-minor-mode-off-hook
  (lambda ()
    (call-interactively 'widen)
    (setq-local  org-hide-emphasis-markers nil)
    (read-only-mode -1)
    (logos-focus-mode -1)
    (display-line-numbers-mode t)
    (org-indent-mode t)
    (when (fboundp 'fontaine-set-preset) (fontaine-set-preset 'default))
    (when (fboundp 'vi-tilde-fringe-mode) (vi-tilde-fringe-mode t))
    (when (fboundp 'git-gutter-mode) (git-gutter-mode t))
    (when (fboundp 'centaur-tabs-local-mode) (centaur-tabs-local-mode t)))
  "Hook when `jf/lp-minor-mode' deactivated."
  :type 'hook)


(provide 'jf-presentation-and-focus)
;;; jf-presentation-and-focus.el ends here