;;; jnf-macros.el --- Summary
;;
;;; Commentary:
;;
;;  This "package" provides keyboard macro support.
;;
;;  See https://www.emacswiki.org/emacs/KeyboardMacros
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jnf/query-within-macro-definition (arg)
  "Prompt for input using minibuffer during kbd macro execution.

With prefix ARG, allows you to select what prompt string to use.
If the input is non-empty, it is inserted at point."
  (interactive "P")
  (let* ((query (lambda () (kbd-macro-query t)))
         (prompt (if arg (read-from-minibuffer "PROMPT: ") "Input: "))
         (input (unwind-protect
                    (progn
                      (add-hook 'minibuffer-setup-hook query)
                      (read-from-minibuffer prompt))
                  (remove-hook 'minibuffer-setup-hook query))))
    (unless (string= "" input) (insert input))))

(global-set-key (kbd "C-x Q") 'jnf/query-within-macro-definition)

(provide 'jnf-macros.el)
;;; jnf-macros.el ends here
