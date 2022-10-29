;;; jf-fonts-and-iconography.el --- Working to manage my windows -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary
;;
;; This package provides font, theme, and window support.

;;; Code

;;;; Fonts
(use-package fontaine
  :straight t
  :config
  (setq fontaine-presets
        '((small
           :default-height 110)
          (default
            :default-height 155)
          (coding
           :default-height 155)
          (presenting
           :default-weight semilight
           :default-height 220
           :bold-weight extrabold)
          (reading
           :default-weight semilight
           :default-height 220
           :bold-weight extrabold)
          (t
           ;; Following Prot’s example, keeping these for for didactic purposes.
           :default-family "Iosevka Comfy Motion Fixed"
           :default-weight regular
           :default-height 155
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
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil))))
(fontaine-set-preset 'default)

;; Useful for referential icons.
(use-package all-the-icons
  :straight t
  :config
  (cl-defmacro jf/all-the-icons--with(&key name)
    "A macro to provide functions for icon names."
    (let ((defun-fn (intern (concat "jf/all-the-icons--with-" name)))
          (icon-fn (intern (concat "all-the-icons-" name)))
          (docstring (concat "Displays an ICON from `all-the-icons-" name "'.")))
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

;; Incorporates file icons with file listings of dired.  /Note/: On 2021-04-11
;; I was getting the following error with this package: "*ERROR*: Symbol's
;; value as variable is void: file"
(use-package all-the-icons-dired
  :straight t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'jf-fonts-and-iconography)
;;; jf-fonts-and-iconography.el ends here