(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
    ["#fdf0ed" "#e95678" "#29d398" "#fadad1" "#26bbd9" "#ee64ac" "#26bbd9" "#403c3d"])
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(column-number-mode t)
 '(custom-safe-themes
    '("7afad8f4707c84129e4cb1e5bad4feb0a9f0db02e1cfadb029921a0bde693d1e" "fd9e60866accaa68c8bede88623478e74b7c6e7c91765b0cdf11ddf04a5ff803" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "8607fdf417935af22922d10b4664a4ead5a64c01b55ac9e4eb9f4da9d177f612" "250268d5c0b4877cc2b7c439687f8145a2c85a48981f7070a72c7f47a2d2dc13" "23ba4b4ba4d1c989784475fed58919225db8d9a9751b32aa8df835134fe7ba6f" default))
 '(delete-selection-mode t)
 '(dired-listing-switches "-laGhpX")
 '(dired-use-ls-dired t)
 '(eww-auto-rename-buffer 'title)
 '(global-display-line-numbers-mode t)
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-agenda-files
    '("~/git/org/agenda.org" "~/git/takeonrules.source/agenda.org" "~/git/org/denote/scientist/20221021T221357--scientist-agenda__scientist.org"))
 '(org-export-backends '(ascii html latex md odt))
 '(random-table/reporter 'random-table/reporter/as-insert)
 '(safe-local-variable-values
    '((jf/tor-minor-mode . 1)
       (projectile-require-project-root)
       (projectile-git-command . "git ls-files -zco --exclude-from=.projectile.gitignore")
       (org-insert-tilde-language . ruby)
       (org-insert-tilde-language . emacs-lisp)
       (encoding . utf-8)))
 '(show-paren-mode t)
 '(stem-reading-overlay t)
 '(tmr-timer-finished-functions
    (list #'tmr-print-message-for-completed-timer #'tmr-sound-play #'jf/tmr-notification-notify) nil nil "Customized with use-package tmr")
 '(typopunct-buffer-language 'english)
 '(use-package-always-ensure t)
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(denote-faces-link ((((class color) (min-colors 256)) (:inherit link :box (:line-width (1 . 1) :color "#a59a94" :style released-button)))))
 '(font-lock-misc-punctuation-face ((((class color) (min-colors 256)) :foreground "#316500")))
 '(font-lock-regexp-face ((((class color) (min-colors 256)) :foreground "#8f0075")))
 '(hl-todo ((((class color) (min-colors 256)) :foreground "#7f0000")))
 '(jf/bom-face ((((class color) (min-colors 256)) (:width ultra-expanded :box (:line-width (2 . 2) :color "#d00000" :style released-button)))))
 '(jf/org-faces-abbr ((((class color) (min-colors 256)) :underline t :slant oblique :foreground "#595959")))
 '(jf/org-faces-date ((((class color) (min-colors 256)) :underline nil :foreground "#005077")))
 '(jf/org-faces-epigraph ((((class color) (min-colors 256)) :underline nil :slant oblique :foreground "#193668")))
 '(jf/tabs-face ((((class color) (min-colors 256)) :underline (:style wave :color "#1640b0"))))
 '(org-block ((((class color) (min-colors 256)) :background "#f0e0cc")))
 '(org-block-begin-line ((((class color) (min-colors 256)) :background "#dfdbfa")))
 '(org-block-end-line ((((class color) (min-colors 256)) :background "#dfdbfa")))
 '(org-list-dt ((((class color) (min-colors 256)) :bold t :slant italic :foreground "#193668")))
 '(tree-sitter-hl-face:constant ((((class color) (min-colors 256)) :slant italic :foreground "#531ab6")))
 '(tree-sitter-hl-face:method.call ((((class color) (min-colors 256)) :slant italic :foreground "#193668")))
 '(tree-sitter-hl-face:operator ((((class color) (min-colors 256)) :foreground "#7f0000")))
 '(tree-sitter-hl-face:property ((((class color) (min-colors 256)) :inherit font-lock-variable-name-face))))
