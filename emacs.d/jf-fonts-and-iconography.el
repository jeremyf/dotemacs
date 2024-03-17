;;; jf-fonts-and-iconography.el --- How words and symbols are displayed -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package provides font, theme, and window support.

;;; Code:

;;;; Fonts
(use-package fontaine
  ;; A narrow focus package for naming font configurations and then selecting
  ;; them.
  :straight t
  :config
  (setq fontaine-presets
    ;; I'm naming the presets as "actions"; the mindset that I'm using when
    ;; wanting that font.
    '((smallest
        :default-height 100)
       (smaller
        :default-height 120)
       (default
         :default-height 130)
       (bigger
         :default-height 160)
       (iosevka
         :default-height 140
         :default-weight regular
         :bold-weight bold
         :default-family "Iosevka Comfy Motion Fixed")
       (coding
         :default-family "Intel One Mono"
         :default-weight light
         :bold-weight medium
         :default-height 130)
       (biggest
         :default-weight light
         :default-height 220
         :bold-weight bold)
       (reading
         :default-weight semilight
         :default-family "ETBembo"
         :default-height 220
         :bold-weight bold)
       (t
         ;; Following Prot’s example, keeping these for for didactic purposes.
         ;; :default-family "Iosevka Comfy Motion Fixed"
         :default-family "Intel One Mono"
         :default-weight light
         :default-height 130
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :fixed-pitch-serif-family nil ; falls back to :default-family
         :fixed-pitch-serif-weight nil ; falls back to :default-weight
         :fixed-pitch-serif-height 1.0
         :variable-pitch-family "ETBembo"
         :variable-pitch-weight nil
         :variable-pitch-height 1.0
         :bold-family nil ; use whatever the underlying face has
         :bold-weight medium
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)))
  (fontaine-set-preset 'default))

;;;; Icons

(use-package all-the-icons
  ;; It's nice to see icons as a quick visual helper.
  :straight t
  :config
  (cl-defmacro jf/all-the-icons--with(&key name)
    "A macro to provide functions for icon names."
    (let ((defun-fn (intern (concat "jf/all-the-icons--with-" name)))
           (icon-fn (intern (concat "all-the-icons-" name)))
           (docstring (concat
                        "Displays an ICON from `all-the-icons-" name "'.")))
      `(defun ,defun-fn (icon str &optional height v-adjust)
         ,docstring
         (s-concat (,icon-fn
                     icon
                     :v-adjust (or v-adjust 0)
                     :height (or height 1))
           " " str))))
  (jf/all-the-icons--with :name "faicon")
  (jf/all-the-icons--with :name "material")
  (jf/all-the-icons--with :name "octicon")
  (jf/all-the-icons--with :name "alltheicon"))

(use-package all-the-icons-dired
  ;; Incorporates file icons with file listings of dired.  /Note/: On 2021-04-11
  ;; I was getting the following error with this package: "*ERROR*: Symbol's
  ;; value as variable is void: file"
  :straight t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;;;; Typography
(use-package typopunct
  ;; A package that provides some automatic replacement of strings of keys.  For
  ;; example in text-mode, when I type three periods (e.g. “.”) typopunct
  ;; replaces that with an ellipsis (e.g. “…”)
  :straight t
  :custom (typopunct-buffer-language 'english)
  :config
  (add-hook 'org-mode-hook 'jf/typopunct-init)
  (defun jf/typopunct-init ()
    (require 'typopunct)
    (typopunct-change-language 'english)
    (typopunct-mode 1))
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
  ;; feet, arcminutes, derivatives
  (defconst typopunct-prime  (decode-char 'ucs #x2032))
  ;; inches, arcseconds, double derivatives
  (defconst typopunct-dprime (decode-char 'ucs #x2033))
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
(provide 'jf-fonts-and-iconography)
;;; jf-fonts-and-iconography.el ends here
