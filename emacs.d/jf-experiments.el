;;; jf-experiments --- Where I put things that I'm exploring -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;;; Code

;; From https://www.reddit.com/r/emacs/comments/10qo7vb/comment/j73idup/
;; (defvar my/consult-buffer-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "\C-k" #'my/consult-buffer-kill)
;;     map))
;; (consult-customize consult-buffer :keymap my/consult-buffer-map)

;; (defun my/consult-buffer-kill ()
;;   "In consult-buffer kill the current candidate"
;;   (interactive)
;;   (let ((marker (string #x200002)) ;; probably some internal detail :(
;;         (candidate (vertico--candidate)))
;;     (when (s-ends-with? marker candidate)
;;       (kill-buffer (s-replace marker "" candidate))
;;       (vertico-next))))

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

(use-package org-tufte
  :straight (org-tufte :host github :repo "Zilong-Li/org-tufte")
  :config
  (require 'org-tufte)
  (setq org-tufte-htmlize-code t))

(provide 'jf-experiments)
;;; jf-experiments.el ends here
