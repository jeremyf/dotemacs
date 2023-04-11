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

;; Let's give the more fluid scrolling a chance
(pixel-scroll-precision-mode)

(provide 'jf-experiments)
;;; jf-experiments.el ends here
