;; And now the theme.  I’ve chosen the modus themes (e.g. ~modus-vivendi~ and
;; ~modus-operandi~).  They provide a light and dark theme with a focus on visual
;; accessibility.
(defun jf/theme-custom-faces ()
  "Set the various custom faces for both `treesit' and `tree-sitter'."
  (ef-themes-with-colors
    (setq hl-todo-keyword-faces
          `(("HOLD" . ,yellow)
            ("TODO" . ,red)
            ("NEXT" . ,blue)
            ("THEM" . ,magenta)
            ("PROG" . ,cyan-warmer)
            ("OKAY" . ,green-warmer)
            ("DONT" . ,yellow-warmer)
            ("FAIL" . ,red-warmer)
            ("BUG" . ,red-warmer)
            ("DONE" . ,green)
            ("NOTE" . ,blue-warmer)
            ("KLUDGE" . ,cyan)
            ("HACK" . ,cyan)
            ("TEMP" . ,red)
            ("FIXME" . ,red-warmer)
            ("XXX+" . ,red-warmer)
            ("REVIEW" . ,red)
            ("DEPRECATED" . ,yellow)))
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
         ((,c :background ,bg-added-faint)))
      `(org-block-begin-line
         ((,c :background ,bg-added-refine)))
      `(org-block-end-line
         ((,c :background ,bg-added-refine)))
      `(fill-column-indicator
         ((,c :width ultra-condensed :background ,bg-dim :foreground ,bg-dim)))
      `(font-lock-regexp-face
         ((,c :foreground ,red))))))

(use-package ef-themes
  :straight t
  :config
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
    ef-themes-variable-pitch-ui t))

(setq jf/themes-plist '(:dark ef-bio :light ef-frost))
