;;; -*- lexical-binding: t; -*-
;;; jnf-dice.el --- Summary
;;
;;; Commentary:
;;
;;  This package includes some dice functions
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-d20
  :after org
  :straight t)

(defun jnf/roll (sided)
  "Roll an n SIDED die."
  (interactive "sDice Sides: ")
  (let ((result (+ 1 (random (cl-parse-integer sided)))))
    (message "d%s => %s" sided result)))


(defun jnf/roll-expression-dwim (expression &optional)
  "Roll the EXPRESSION, check `thing-at-point' then prompt."
  (interactive (list (if (string-match
                          "[dD][0-9]"
                          (format "%s" (thing-at-point 'sexp t)))
                         (thing-at-point 'sexp t)
                       (read-string "Dice Expression: "))))
  (-let* (((rolls . result) (org-d20--roll expression)))
    (message "%s => %s" expression result)))
(global-set-key (kbd "C-s-r") 'jnf/roll-expression-dwim)

(provide 'jnf-dice.el)
;;; jnf-dice.el ends here
