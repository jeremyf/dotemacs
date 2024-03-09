;;; jf-illuminating.el --- Packages and functions that "illuminate" the current state -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Within this package are tools for highlighting and illuminating the current
;; moment.

;;; Code:
(use-package expand-region
  ;; A simple package that does two related things really well; expands and
  ;; contracts the current region.  I use this all the time.
  ;;
  ;; In writing, with the cursor at point, when I expand it selects the word.
  ;; The next expand the sentence, then paragraph, then page.  In programming it
  ;; leverages sexp.
  :straight (:host github :repo "jeremyf/expand-region.el")
  :bind (("C-=" . er/expand-region)
          ("C-+" . er/contract-region)))

;; I thought I might use this but I never practiced.  Holding it as a reminder.
;; Learning about this, may be curious about https://tony-zorman.com/posts/change-inner.html
;; (use-package change-inner
;;   :straight t
;;   :bind (;; Note the symmetry between 'change-inner binding and er/expand-region
;;           ("C-c C-=" . 'change-inner)
;;           ;; Below is an alternate consideration; namely if I want inner/outer
;;           ;; behavior
;;           ;; ("C-c TAB" . 'change-inner)
;;           ;; ("C-c C-o" . 'change-outer)
;;           ))


(use-package display-fill-column-indicator
  ;; It's nice to have a gentle reminder showing me the recommended column width
  ;; for the current buffer.
  :straight (:type built-in)
  :hook (prog-mode . display-fill-column-indicator-mode))

(use-package kind-icon
  ;; This packages helps provide additional icons for functions and variables in
  ;; the completion candidates.
  :straight t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as
                                        ; `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and
                                        ; background colors ("blended")?
  (kind-icon-blend-frac 0.08)
  ;; directory that defaults to the `user-emacs-directory'. Here, I change that
  ;; directory to a location appropriate to `no-littering' conventions, a
  ;; package which moves directories of other packages to sane locations.
  ;; (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ;
  ;; Change cache dir
  :config
                                        ; Enable `kind-icon'
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  ;; Add hook to reset cache so the icon colors match my theme
  ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
  ;; the theme using my custom defined command for switching themes. If I don't
  ;; do this, then the backgound color will remain the same, meaning it will not
  ;; match the background color corresponding to the current theme. Important
  ;; since I have a light theme and dark theme I switch between. This has no
  ;; function unless you use something similar
  (add-hook 'kb/themes-hooks
    #'(lambda () (interactive) (kind-icon-reset-cache))))

(use-package lin
  ;;  “LIN locally remaps the hl-line face to a style that is optimal for major
  ;;  modes where line selection is the primary mode of interaction.”  In
  ;;  otherwords, ~lin.el~ improves the highlighted line behavior for the
  ;;  competing contexts.
  :straight (lin :host gitlab :repo "protesilaos/lin")
  :init (global-hl-line-mode)
  :config (lin-global-mode 1)
  (setq lin-face 'lin-blue))

(use-package pulsar
  ;; A little bit of visual feedback.  See
  ;; https://protesilaos.com/codelog/2022-03-14-emacs-pulsar-demo/
  :straight (pulsar :host gitlab :repo "protesilaos/pulsar")
  :hook
  (consult-after-jump . pulsar-recenter-top)
  (consult-after-jump . pulsar-reveal-entry)
  ;; integration with the built-in `imenu':
  (imenu-after-jump . pulsar-recenter-top)
  (imenu-after-jump . pulsar-reveal-entry)
  :config
  (pulsar-global-mode 1)
  (setq pulsar-face 'pulsar-magenta
    pulsar-delay 0.05)
  (setq ring-bell-function 'jf/pulse)
  :preface
  (defun jf/pulse (&optional parg)
    "Pulse the current line.

  If PARG (given as universal prefix), pulse between `point' and `mark'."
    (interactive "P")
    (if (car parg)
      (pulsar--pulse nil nil (point) (mark))
      (pulsar-pulse-line)))
  :bind (("C-c C-l" . jf/pulse)))

(use-package rainbow-mode
  ;; When I toggle on Rainbow mode, it colorizes the text that is color names
  ;; and hex declarations (e.g. "#0000ff" ).  Most useful when working with CSS,
  ;; but sometimes non-CSS files have those declarations as well.
  :straight t)

(use-package rainbow-delimiters
  ;; A quick and useful visual queue for paranthesis.
  :straight t
  :hook ((prog-mode) . rainbow-delimiters-mode))

(use-package recursion-indicator
  ;; I vascilate between yes and no; but invariably find myself stuck in a
  ;; recursed buffer.
  :straight t
  :config
  (setq enable-recursive-minibuffers t)
  (recursion-indicator-mode))

(use-package vi-tilde-fringe
  ;; Show tilde (e.g. ~\~~) on empty trailing lines.  This is a feature ported
  ;; from https://en.wikipedia.org/wiki/Vi
  :straight t
  :config (global-vi-tilde-fringe-mode))

(use-package whole-line-or-region
  ;; From the package commentary, “This minor mode allows functions to operate
  ;; on the current line if they would normally operate on a region and region
  ;; is currently undefined.”  I’ve used this for awhile and believe it’s not
  ;; baked into my assumptions regarding how I navigate Emacs.
  :straight t
  :config (whole-line-or-region-global-mode))

;; Since writing the comment about folding, I haven't used it.
;; (use-package yafolding
;;   ;; It can be helpful to fold regions; I don't do it much but it can be
;;   ;; helpful.
;;   :straight t)

(use-package keycast
  ;; When I turn on `keycast-mode-line' each key press will echo in the
  ;; mode-line.  There are other options for logging which could be conceptually
  ;; useful for feeding a macro.
  :straight t)

(require 'jf-modeline)

(provide 'jf-illuminating)
;;; jf-illuminating.el ends here
