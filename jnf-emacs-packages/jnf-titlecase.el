;;; jnf-titlecase.el --- Summary
;;
;;; Commentary:
;;
;;  Package to provide titlecase functionality.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package titlecase
  :straight (titlecase :host github :repo "duckwork/titlecase.el")
  :custom (titlecase-style 'wikipedia))

(provide 'jnf-titlecase.el)
;;; jnf-titlecase.el ends here
