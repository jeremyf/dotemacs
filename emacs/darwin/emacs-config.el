;;; package --- Summary
;;
;;; Commentary:
;;
;;  This package loads darwin specific packages; It assumes that both
;;  "use-package" and "straight-use-package" are loaded.
;;
;;; Code:

;; I have set this in OS X, however without a fallback, on Linux the
;; interpretter halts here
(set-frame-font "JetBrains Mono 13" nil t)

;; Adds the ability to grab a link from various OS X applications
;; Note, the sibling org-mac-link.  That package works within ORG mode
;; with an extended menu option, and assumes ORG styling.  They both
;; have the same keybinding as org-mode favors org-mac-link.
(use-package grab-mac-link
  :straight t
  :config
  ;; A replacement function for existing grab-mac-link-make-html-link
  (defun jnf/grab-mac-link-make-html-link (url name)
    "Using HTML syntax, link to and cite the URL with the NAME."
    (format "<cite><a href=\"%s\" class=\"u-url p-name\" rel=\"cite\">%s</a></cite>" url name))
  ;; The function advice to override the default behavior
  (advice-add
   'grab-mac-link-make-html-link
   :override
   'jnf/grab-mac-link-make-html-link
   '((name . "jnf")))
  :bind (("C-c g" . grab-mac-link)))

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

(use-package org-mac-link
  :ensure t
  :straight (org-mac-link :type git :host github :repo "jeremyf/org-mac-link")
  :defer t)
(add-hook 'org-mode-hook (lambda ()
                           (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

(if (version< "27.0" emacs-version)
           (set-fontset-font
            "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
         (set-fontset-font
          t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

;; Emacs comes with DocView built in.  pdf-tools is a replacement for
;; DocView.  I've found the rendered images a bit more crisp and the
;; interactions a bit more responsive.  However, I have not been able
;; to get `org-noter' working with `pdf-tools'.  `org-noter' provides
;; annotation services for PDFs.
(use-package pdf-tools
  :pin manual ;; manually update
  :straight t
  :defer t
  :ensure t
  :config (pdf-tools-install) ;; initialise
  (setq-default pdf-view-display-size 'fit-page) ;; open pdfs scaled to fit page
  (setq pdf-annot-activate-created-annotations t) ;; automatically annotate highlights
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward);; use normal isearch
  )

;; On  I use ⌘ as meta and prefer ⌥ to do nothing so I can still insert special characters easily.
;;
;; (setq mac-command-modifier 'meta
;;       mac-option-modifier 'none)

;; (setq mac-right-option-modifier nil) ;; Disable Emacs capturing meta-key and fall back to OS X (useful for diacritics)
;;
;; (setq mac-right-option-modifier 'meta) ;; Enable Emacs capturing meta-key, so right M-x will execute extended command


(defun jnf/toggle-osx-alternate-modifier ()
  "Toggle native OS-X Option modifier
setting (e.g. ns-alternate-modifier)."
  (interactive)
  (if ns-alternate-modifier
      (progn (setq ns-alternate-modifier nil)
             (message "Enabling OS X native Option modifier"))
    (progn (setq ns-alternate-modifier 'meta)
           (message "Disabling OX X native Option modifier (e.g. Option as Meta)"))))
(global-set-key (kbd "C-x /") 'jnf/toggle-osx-alternate-modifier)

;; When we get to a REALLY long file or long line, emacs develops problems.
;; This mode helps overcome that.
;;
(use-package so-long
  :ensure t
  :defer t
  :straight t
  :bind
  (:map so-long-mode-map
        ("C-s" . isearch-forward)
        ("C-r" . isearch-backward))
  :config
  (global-so-long-mode 1))

(provide 'emacs-config.el)
;;; emacs-config.el ends here
