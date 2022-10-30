;;; jf-writing.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;; Packages specifically here for helping with my writing activities.

;;; Code

(require 'jf-org-mode)
(require 'jf-denote)

(use-package emojify
  :straight t
  :config
  (defun --set-emoji-font (frame)
    "Adjust the font settings of FRAME so Emacs can display emoji properly."
    (if (eq system-type 'darwin)
	;; For NS/Cocoa
	(set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
      ;; For Linux
      (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

  ;; For when Emacs is started in GUI mode:
  (--set-emoji-font nil)
  ;; Hook for when a frame is created with emacsclient
  ;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
  (add-hook 'after-make-frame-functions '--set-emoji-font))

;; This follows from http://mbork.pl/2017-01-14_I'm_now_using_the_right_dictionary
(use-package sdcv-mode
  :straight (sdcv-mode :type git :host github :repo "gucong/emacs-sdcv")
  :bind ("C-c C-'" . sdcv-search))

(use-package flymake-proselint :straight t)

(use-package unicode-fonts
  :straight t
  :config (unicode-fonts-setup))


;; Provides the reverse of ~fill-paragraph~, and a toggle fill and unfill.
(use-package unfill
  :bind ("M-q" . unfill-toggle)
  :straight t)


;; Delete multiple spaces in one delete stroke.
(use-package hungry-delete
  :straight t
  :diminish 'hungry-delete-mode
  :config (global-hungry-delete-mode))

;; A simple package ability to move lines up and down.
(use-package move-text
  :straight t
  :bind (([C-s-down] . move-text-down)
         ([C-s-up] . move-text-up)))

;; The rules of “titlecase” are confounding.  The ~titlecase.el~ package
;; provides numerous ways to cast a string to “titlecase.”  I chose wikipedia
;; style as a quasi-opinionated compromise.
(use-package titlecase
  :straight (titlecase :host github :repo "duckwork/titlecase.el")
  :custom (titlecase-style 'wikipedia))

;; Allow Emacs to work with multiple cursors.  See
;; https://melpa.org/#/multiple-cursors
(use-package multiple-cursors
  :bind (("C-M-SPC" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-s-<mouse-1>" . mc/add-cursor-on-click)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/edit-lines)) ;; CTRL+CMD+c
  :straight t)

;; Type \"C-;\" to select current symbol and all matches; Then edit at multiple
;; points.
(use-package iedit :straight t)

;;;; Typopunct

(use-package typopunct
  :straight t
  :config
  (add-hook 'org-mode-hook 'jf/typopunct-init)
  (defun jf/typopunct-init ()
    (require 'typopunct)
    (typopunct-change-language 'english)
    (typopunct-mode 1))
  (setq typopunct-buffer-language 'english)

  ;; To insert a typographical ellipsis sign (…) on three consecutive
  ;; dots, or a middle dot (·) on ‘^.’
  (defconst typopunct-ellipsis (decode-char 'ucs #x2026))
  (defconst typopunct-middot   (decode-char 'ucs #xB7)) ; or 2219
  (defun typopunct-insert-ellipsis-or-middot (arg)
    "Change three consecutive dots to a typographical ellipsis mark."
    (interactive "p")
    (cond
     ((and (= 1 arg)
	   (eq (char-before) ?^))
      (delete-char -1)
      (insert typopunct-middot))
     ((and (= 1 arg)
	   (eq this-command last-command)
	   (looking-back "\\.\\." 1))
      (replace-match "")
      (insert typopunct-ellipsis))
     (t
      (self-insert-command arg))))
  (define-key typopunct-map "." 'typopunct-insert-ellipsis-or-middot)


  (defconst typopunct-prime  (decode-char 'ucs #x2032)) ; feet, arcminutes, derivatives
  (defconst typopunct-dprime (decode-char 'ucs #x2033)) ; inches, arcseconds, double derivatives
  (defconst typopunct-tprime (decode-char 'ucs #x2034))

  ;; The minus sign (−) is separate from the hyphen (-), en dash (–) and
  ;; em dash (—). To build upon the clever behavior of the ‘-’ key
  (defconst typopunct-minus (decode-char 'ucs #x2212))
  (defconst typopunct-pm    (decode-char 'ucs #xB1))
  (defconst typopunct-mp    (decode-char 'ucs #x2213))
  (defadvice typopunct-insert-typographical-dashes
      (around minus-or-pm activate)
    (cond
     ((or (eq (char-before) typopunct-em-dash)
	  (looking-back "\\([[:blank:]]\\|^\\)\\^" 2))
      (delete-char -1)
      (insert typopunct-minus))
     ((looking-back "[^[:blank:]]\\^" 1)
      (insert typopunct-minus))
     ((looking-back "+/" 1)
      (progn (replace-match "")
	     (insert typopunct-pm)))
     (t ad-do-it)))
  (defun typopunct-insert-mp (arg)
    (interactive "p")
    (if (and (= 1 arg) (looking-back "-/" 2))
	(progn (replace-match "")
	       (insert typopunct-mp))
      (self-insert-command arg)))
  (define-key typopunct-map "+" 'typopunct-insert-mp)
  (defconst typopunct-times (decode-char 'ucs #xD7))
  (defun typopunct-insert-times (arg)
    "Insert multiplication sign at ARG."
    (interactive "p")
    (if (and (= 1 arg) (looking-back "\\([[:blank:]]\\|^\\)\\^"))
	(progn (delete-char -1)
	       (insert typopunct-times))
      (self-insert-command arg)))
  (define-key typopunct-map "x" 'typopunct-insert-times)

  (defadvice typopunct-insert-quotation-mark (around wrap-region activate)
    (let* ((lang (or (get-text-property (point) 'typopunct-language)
		     typopunct-buffer-language))
	   (omark (if single
		      (typopunct-opening-single-quotation-mark lang)
		    (typopunct-opening-quotation-mark lang)))
	   (qmark (if single
		      (typopunct-closing-single-quotation-mark lang)
		    (typopunct-closing-quotation-mark lang))))
      (cond
       (mark-active
	(let ((skeleton-end-newline nil)
	      (singleo (typopunct-opening-single-quotation-mark lang))
	      (singleq (typopunct-closing-single-quotation-mark lang)))
	  (if (> (point) (mark))
	      (exchange-point-and-mark))
	  (save-excursion
	    (while (re-search-forward (regexp-quote (string omark)) (mark) t)
	      (replace-match (regexp-quote (string singleo)) nil nil)))
	  (save-excursion
	    (while (re-search-forward (regexp-quote (string qmark)) (mark) t)
	      (replace-match (regexp-quote (string singleq)) nil nil)))
	  (skeleton-insert (list nil omark '_ qmark) -1)))
       ((looking-at (regexp-opt (list (string omark) (string qmark))))
	(forward-char 1))
       (t ad-do-it)))))


(provide 'jf-writing)
;;; jf-writing.el ends here
