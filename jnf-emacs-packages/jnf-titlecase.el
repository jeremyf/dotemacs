;;; Commentary:
;;
;;  Package to provide titlecase functionality.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package titlecase
  :straight (titlecase :host github :repo "duckwork/titlecase.el")
  :custom (titlecase-style 'wikipedia))
