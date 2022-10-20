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
(use-package logos
  :straight t
  :config (use-package olivetti
	    :straight t
	    :custom
	    (olivetti-body-width 0.7)
	    (olivetti-minimum-body-width 80)
	    (olivetti-recall-visual-line-mode-entry-state t))
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
			       (define-key map (kbd "]") #'logos-forward-page-dwim)
			       (define-key map (kbd "RET") #'logos-forward-page-dwim)
			       (define-key map (kbd "SPC") #'logos-forward-page-dwim)
			       (define-key map (kbd "<right>") #'logos-forward-page-dwim)
			       (define-key map (kbd "<down>") #'logos-forward-page-dwim)
			       (define-key map (kbd "C-n") #'logos-forward-page-dwim)
			       (define-key map (kbd "[") #'logos-backward-page-dwim)
			       (define-key map (kbd "DEL") #'logos-backward-page-dwim)
			       (define-key map (kbd "<left>") #'logos-backward-page-dwim)
			       (define-key map (kbd "<up>") #'logos-backward-page-dwim)
			       (define-key map (kbd "C-p") #'logos-backward-page-dwim)
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
    (when (fboundp 'fontaine-set-preset) (fontaine-set-preset 'presentation))
    (when (fboundp 'logos-focus-mode) (logos-focus-mode 1))
    (when (fboundp 'vi-tilde-fringe-mode) (vi-tilde-fringe-mode -1))
    (when (fboundp 'git-gutter-mode) (git-gutter-mode -1))
    (when (fboundp 'centaur-tabs-mode) (centaur-tabs-mode -1)))
  "Hook when `jf/lp-minor-mode' activated."
  :type 'hook)

(defcustom jf/lp-minor-mode-off-hook
  (lambda ()
    (call-interactively 'widen)
    (setq-local  org-hide-emphasis-markers nil)
    (read-only-mode -1)
    (logos-focus-mode -1)
    (display-line-numbers-mode t)
    (when (fboundp 'fontaine-set-preset) (fontaine-set-preset 'regular))
    (when (fboundp 'vi-tilde-fringe-mode) (vi-tilde-fringe-mode t))
    (when (fboundp 'git-gutter-mode) (git-gutter-mode t))
    (when (fboundp 'centaur-tabs-mode) (centaur-tabs-mode t)))
  "Hook when `jf/lp-minor-mode' deactivated."
  :type 'hook)

;;; jf-quick-help.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;; Package-Requires: ((transient "0.3.7") (emacs "25.1"))

;;; Commentary:
;;
;; This package provides a simple way to register quick help function.

;;; Code
(cl-defun jf/quick-help (&key header body)
  "Create a help window with HEADER and BODY."
  (let ((qh-buff (concat "*Quick Help: " header "*")))
    (progn (or (get-buffer qh-buff)
	       (progn (get-buffer-create qh-buff)
		      (with-current-buffer qh-buff
			(insert (concat "**" header "**\n" body))
			(goto-char (point-min))
			(not-modified)
			(read-only-mode)
			(special-mode)
			(local-set-key (kbd "q") 'kill-buffer-and-window))))
	   (pop-to-buffer qh-buff '((display-buffer-below-selected)
				    ;; When sizing the buffer, if tab-line-format is
				    ;; active then that "line" is not counted in
				    ;; calculating the fit-to-window-buffer value.
				    ;; Which means that the buffer flows into the
				    ;; mode-line.  So turn it off for this buffer.
				    (window-parameters . ((tab-line-format . none)
							  (no-other-window . nil)))
				    (window-height . fit-window-to-buffer)))
	   (message "q - Remove Window"))))

(cl-defmacro jf/transient-quick-help (name &key header label body)
  "Macro for creating callable functions that display help.

      NAME is name of function,
      LABEL is label for the menu
      HEADER is name of buffer, and TEXT is displayed."
  (declare (indent defun))
  `(progn
     (transient-define-suffix ,name nil
       ,header
       :description ,label
       (interactive)
       (jf/quick-help :header ,header :body ,body))))

(provide 'jf-presentation-and-focus)
;;; jf-presentation-and-focus.el ends here