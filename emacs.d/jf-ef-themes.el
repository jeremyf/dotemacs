(use-package ef-themes
  :straight t)

(setq ef-themes-headings ; read the manual's entry or the doc string
  '((0 . (bold 1.4))
      (1 . (variable-pitch bold 1.7))
       (2 . (overline semibold 1.5))
       (3 . (monochrome overline 1.4 background))
       (4 . (overline 1.3))
       (5 . (rainbow 1.2))
       (6 . (rainbow 1.15))
       (t . (rainbow 1.1))))

;; They are nil by default...
(setq ef-themes-mixed-fonts t
  ef-themes-variable-pitch-ui t)

;; Load the theme of choice:
(add-hook 'ef-themes-post-load-hook #'jf/theme-custom-faces)

;; And now the theme.  I’ve chosen the modus themes (e.g. ~modus-vivendi~ and
;; ~modus-operandi~).  They provide a light and dark theme with a focus on visual
;; accessibility.
(defun jf/theme-custom-faces ()
  "Set the various custom faces for both `treesit' and `tree-sitter'."
  (ef-themes-with-colors
    (custom-set-faces
      `(denote-faces-link
         ((,c (:inherit link
                :box (:line-width (1 . 1)
                       :color ,border
                       :style released-button)))))
      `(jf/bom-face
         ((,c (:width ultra-expanded :box (:line-width (2 . 2)
                                            :color ,underline-err
                                            :style released-button)))))
      `(jf/tabs-face
         ((,c :underline (:style wave :color ,bg-blue-intense))))
      `(jf/org-faces-date
         ((,c :underline nil :foreground ,cyan-faint)))
      `(jf/org-faces-epigraph
         ((,c :underline nil :slant oblique :foreground ,fg-alt)))
      `(jf/org-faces-abbr
         ((,c :underline t :slant oblique :foreground ,fg-dim)))
      `(org-list-dt
         ((,c :bold t :slant italic :foreground ,fg-alt)))
      `(font-lock-misc-punctuation-face
         ((,c :foreground ,green-warmer)))
      `(mode-line
         ((,c :foreground ,cyan :background ,bg-cyan-subtle)))
      `(org-block
         ;; ((,c :background ,bg-yellow-subtle)))
         ((,c :background ,bg-changed-faint)))
      `(org-block-begin-line
         ((,c :background ,bg-removed-faint)))
      `(org-block-end-line
         ((,c :background ,bg-removed-faint)))
      `(hl-todo
         ((,c :foreground ,red-faint)))
      `(color-rg-font-lock-header-line-text
         ((,c :foreground ,green)))
      `(color-rg-font-lock-header-line-keyword
         ((,c :foreground ,keybind)))
      `(color-rg-font-lock-function-location
         ((,c :foreground ,keybind)))
      `(color-rg-font-lock-header-line-edit-mode
         ((,c :foreground ,keybind)))
      `(color-rg-font-lock-header-line-directory
         ((,c :foreground ,cyan :background ,bg-cyan-subtle)))
      `(color-rg-font-lock-command
         ((,c :foreground ,fg-alt :background ,bg-inactive)))
      `(color-rg-font-lock-file
         ((,c :foreground ,cyan :background ,bg-cyan-subtle)))
      `(color-rg-font-lock-line-number
         ((,c :background ,bg-dim :foreground ,fg-dim)))
      `(color-rg-font-lock-column-number
         ((,c :background ,bg-dim :foreground ,fg-dim)))
      `(color-rg-font-lock-position-splitter
         ((,c :background ,bg-dim :foreground ,fg-dim)))
      `(color-rg-font-lock-match
         ((,c :background ,cyan-cooler :foreground ,bg-cyan-subtle)))
      `(color-rg-font-lock-mark-changed
         ((,c :backgr
            ound ,bg-changed :foreground ,fg-changed)))
      `(color-rg-font-lock-mark-deleted
         ((,c :background ,bg-removed :foreground ,fg-removed)))
      `(fill-column-indicator
         ((,c :width ultra-condensed :background ,bg-dim :foreground ,bg-dim)))
      `(font-lock-regexp-face
         ((,c :foreground ,red))))))
