(use-package modus-themes
  ;; I love [[http://protesilaos.com][Prot]]’s attention to detail with the modus
  ;; themes.  Here’s my configuration for these two sibling themes.  There’s a
  ;; bit of chatter, but all told it sets things up how I like.
  :straight (:type git :host gitlab :repo "protesilaos/modus-themes" :branch "main")
  :init
  (setq modus-themes-italic-constructs t
    modus-themes-bold-constructs t
    modus-themes-mixed-fonts t
    modus-themes-variable-pitch-ui nil
    modus-themes-custom-auto-reload t
    modus-themes-disable-other-themes t
    modus-themes-org-blocks 'tinted-background
    modus-themes-common-palette-overrides
    '(
       (comment yellow-faint)
       (string green)
       ;; Favoring more of the defaults; below is some settings I've used for
       ;; quite a while (paired with the above)
       ;;
       ;;    (builtin magenta)
       ;;    (constant magenta-cooler)
       ;;    (docstring green-warmer)
       ;;    (docmarkup magenta-faint)
       ;;    (fnname magenta-warmer)
       ;;    (keyword cyan)
       ;;    (preprocessor cyan-cooler)
       ;;    (type magenta-cooler)
       ;;    (variable blue-warmer)
       ;;    (rx-construct red-faint)
       ;;    (rx-backslash blue-cooler)
       )
    modus-themes-completions '((matches . (extrabold))
                                (selection . (semibold accented))
                                (popup . (accented intense)))
    modus-themes-headings
    '((1 . (variable-pitch bold 1.7))
       (2 . (overline semibold 1.5))
       (3 . (monochrome overline 1.4 background))
       (4 . (overline 1.3))
       (5 . (rainbow 1.2))
       (6 . (rainbow 1.15))
       (t . (rainbow 1.1)))))

;; And now the theme.  I’ve chosen the modus themes (e.g. ~modus-vivendi~ and
;; ~modus-operandi~).  They provide a light and dark theme with a focus on visual
;; accessibility.
(defun jf/theme-custom-faces ()
  "Set the various custom faces for both `treesit' and `tree-sitter'."
  (modus-themes-with-colors
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
         ((,c :background ,bg-changed :foreground ,fg-changed)))
      `(color-rg-font-lock-mark-deleted
         ((,c :background ,bg-removed :foreground ,fg-removed)))
      `(fill-column-indicator
         ((,c :width ultra-condensed :background ,bg-dim :foreground ,bg-dim)))
      `(font-lock-regexp-face
         ((,c :foreground ,red))))))

(setq jf/themes-plist
  '(:dark modus-vivendi-tinted
     :light modus-operandi-tinted))
