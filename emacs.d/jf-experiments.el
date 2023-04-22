;;; jf-experiments --- Where I put things that I'm exploring -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;;; Code

;; https://github.com/abo-abo/org-download
(use-package org-download
  :straight t
  :hook (dired-mode . org-download-enable))

(use-package parsebib
  :straight t)

(use-package ebib
  :straight t)

(use-package citar
  :custom (citar-bibliography '("~/git/org/bibliography.bib"))
  :straight t)

(use-package citar-denote
  :straight t)

;; https://github.com/ruediger/qrencode-el/
;;
;; Generate an plain text QRCode (or PNG but really why not use those UTF
;; characters)
(use-package qrencode
  :straight t)

;; I'm a huge fan of the Tufte style; my blog is based on that stylesheet and
;; then further simplified
(use-package org-tufte
  :straight (org-tufte :host github :repo "Zilong-Li/org-tufte")
  :config
  (require 'org-tufte)
  (setq org-tufte-htmlize-code t))

(use-package isl
  :straight (:host github :repo "thierryvolpiatto/isearch-light")
  :bind (("C-c C-s" . isl-search)))


;; A package to browse/read reddit in emacs and `org-mode' format.
(use-package reddigg
  :straight (:host github :repo "thanhvg/emacs-reddigg")
  :custom (reddigg-subs '(emacs planetemacs orgmode wwn swn ruby rubyonrails)))

;; Some customizations to the behavior of a reddit buffer.
(advice-add 'reddigg--ensure-modes
  :after (lambda ()
           ;; This seems like a good idea to limit behavior to only reddit.
           (setq-local org-confirm-elisp-link-function nil)
           ;; It's rather odd to consider adding GET request
           (read-only-mode)
           ;; Given that we're in read mode, I like the q key to bury the
           ;; buffer.
           (local-set-key "q" #'jf/bury-or-unbury-buffer)))

;; May as well make a menu for this experiment.
(bind-key "s-3" #'jf/browsing-menu)
(transient-define-prefix jf/browsing-menu ()
  "For browsing things."
  [["Reddit"
     ("r e" "/r/emacs" (lambda () (interactive) (reddigg-view-sub "emacs")))
     ("r o" "/r/orgmode" (lambda () (interactive) (reddigg-view-sub "orgmode")))
     ("r p" "/r/planetemacs" (lambda () (interactive) (reddigg-view-sub "planetemacs")))
     ("r r" "/r/ruby" (lambda () (interactive) (reddigg-view-sub "ruby")))
     ("r R" "/r/rubyonrails" (lambda () (interactive) (reddigg-view-sub "rubyonrails")))
     ("r s" "/r/swn" (lambda () (interactive) (reddigg-view-sub "swn")))
     ("r w" "/r/wwn" (lambda () (interactive) (reddigg-view-sub "wwn")))]])



(provide 'jf-experiments)
;;; jf-experiments.el ends here
