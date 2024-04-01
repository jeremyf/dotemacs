;;; jf-framing.el --- How Emacs presents the frame -*- lexical-binding: t -*-

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
;; To enter presentation mode: "M-x jf/minor-mode/presenter"

;;; Code:
(use-package edit-indirect
  ;; A nice package for editing regions in separate buffers.  It doesn't appear
  ;; to get the mode guess right.  I haven't used this as much as
  ;; `narrow-region'.  Perhaps it can go?
  :straight t)

(use-package logos
  ;; A `narrow-region' extension that moves towards providing a
  ;; presentation-type experience.
  :straight t
  :bind (:map logos-focus-mode-map
          ("M-]" . #'logos-forward-page-dwim)
          ("s-]" . #'logos-forward-page-dwim)
          ("M-[" . #'logos-backward-page-dwim)
          ("s-[" . #'logos-backward-page-dwim))
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

(use-package "nov.el"
  ;; A package to help in reading epubs.
  :straight t
  :init (use-package esxml :straight t)
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :custom (nov-text-width 80))

(use-package so-long
  ;; Switch to `so-long' when the file gets too long for normal processing.
  :straight t
  :bind
  (:map so-long-mode-map
        ("C-s" . isearch-forward)
        ("C-r" . isearch-backward))
  :config (global-so-long-mode 1))

;;;
;; A package to "narrow" focus; providing a visually appealing interface
(use-package olivetti
  :straight t
  :hook (olivetti-mode-on . jf/olivetti-mode-on-hook)
  (olivetti-mode-off . jf/olivetti-mode-off-hook)
  :config
  ;; I'm typically aiming for 80 character fill-column.
  (setq olivetti-body-width 80)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-style 'fancy)
  (setq olivetti-recall-visual-line-mode-entry-state t)
  :preface
  (defun jf/olivetti-mode-on-hook ()
    "Remove some visual chatter."
    (setq-local original-flymake-fringe-indicator-position
      flymake-fringe-indicator-position)
    (setq-local original-vi-tilde-fringe-mode
      vi-tilde-fringe-mode)
    (setq-local original-display-fill-column-indicator-mode
      display-fill-column-indicator-mode)
    (setq-local original-git-gutter-mode
      git-gutter-mode)
    (setq-local original-display-line-numbers-mode
      display-line-numbers-mode)
    (setq-local original-org-modern-block-fringe
      org-modern-block-fringe)
    ;; The of org-modern blocks is not quite right with olivetti.
    (setq-local org-modern-block-fringe nil)
    (setq-local flymake-fringe-indicator-position nil)
    ;; By restarting org-modern-mode, I hide the expansive fringe; thus
    ;; preserving the "beauty" of Olivetti
    (org-modern-mode 1)
    (vi-tilde-fringe-mode -1)
    (display-line-numbers-mode -1)
    (display-fill-column-indicator-mode -1)
    (git-gutter-mode -1))
  (defun jf/olivetti-mode-off-hook ()
    "Restore some visual chatter."
    (setq-local flymake-fringe-indicator-position
      original-flymake-fringe-indicator-position)
    (org-modern-mode 1)
    (vi-tilde-fringe-mode
      original-vi-tilde-fringe-mode)
    (display-fill-column-indicator-mode
      original-display-fill-column-indicator-mode)
    (display-line-numbers-mode
      original-display-line-numbers-mode)
    (git-gutter-mode
      original-git-gutter-mode)
    (setq-local org-modern-block-fringe
      original-org-modern-block-fringe))
  (defun jf/olivetti-mode (&rest args)
    ;; I want to turn off org-modern-mode as it's drawing of the
    ;; overlays conflicts with Olivetti.  We'll turn it on later.
    (org-modern-mode -1)))

(advice-add 'olivetti-mode :before #'jf/olivetti-mode)

;;; Presentation mode leveraging logos

(defvar jf/minor-mode/presenter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") #'next-line)
    (define-key map (kbd "C-p") #'previous-line)
    (dolist (key `("M-]" "s-]"))
      (define-key map (kbd key) 'logos-forward-page-dwim))
    (dolist (key `("M-[" "s-["))
      (define-key map (kbd key) 'logos-backward-page-dwim))
    map))

(define-minor-mode jf/minor-mode/presenter
  "Enter a `logos' and `olivetti' mode for showing things."
  :init-value nil
  :global nil
  :keymap jf/minor-mode/presenter-map
  :lighter " presenter")

(defcustom jf/minor-mode/presenter-on-hook
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
  "Hook when `jf/minor-mode/presenter' activated."
  :type 'hook)

(defcustom jf/minor-mode/presenter-off-hook
  (lambda ()
    (call-interactively 'widen)
    (olivetti-mode -1)
    (keycast-mode-line-mode -1)
    ;; (setq-local  org-hide-emphasis-markers nil)
    (display-line-numbers-mode t)
    (when (fboundp 'fontaine-set-preset) (fontaine-set-preset 'default))
    (when (fboundp 'vi-tilde-fringe-mode) (vi-tilde-fringe-mode t))
    (when (fboundp 'git-gutter-mode) (git-gutter-mode t))
    (when (fboundp 'centaur-tabs-local-mode) (centaur-tabs-local-mode t)))
  "Hook when `jf/minor-mode/presenter' deactivated."
  :type 'hook)
(provide 'jf-framing)
;;; jf-framing.el ends here
