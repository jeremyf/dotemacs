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


(provide 'jf-experiments)
;;; jf-experiments.el ends here
