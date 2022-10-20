;;; jf-quick-help.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;; Package-Requires: ((transient "0.3.7") (emacs "25.1"))

;;; Commentary:
;;
;; This package provides a simple way to register quick help function.

;;; Code
(cl-defun jf/quick-help (&key header body)
  "Create a help window with HEADER and BODY."
  (let ((qh-buff (concat "*Quick Help: " header "*")))
    (progn (or (get-buffer qh-buff)
	       (progn (get-buffer-create qh-buff)
		      (with-current-buffer qh-buff
			(insert (concat "**" header "**\n" body))
			(goto-char (point-min))
			(not-modified)
			(read-only-mode)
			(special-mode)
			(local-set-key (kbd "q") 'kill-buffer-and-window))))
	   (pop-to-buffer qh-buff '((display-buffer-below-selected)
				    ;; When sizing the buffer, if tab-line-format is
				    ;; active then that "line" is not counted in
				    ;; calculating the fit-to-window-buffer value.
				    ;; Which means that the buffer flows into the
				    ;; mode-line.  So turn it off for this buffer.
				    (window-parameters . ((tab-line-format . none)
							  (no-other-window . nil)))
				    (window-height . fit-window-to-buffer)))
	   (message "q - Remove Window"))))

(cl-defmacro jf/transient-quick-help (name &key header label body)
  "Macro for creating callable functions that display help.

      NAME is name of function,
      LABEL is label for the menu
      HEADER is name of buffer, and TEXT is displayed."
  (declare (indent defun))
  `(progn
     (transient-define-suffix ,name nil
       ,header
       :description ,label
       (interactive)
       (jf/quick-help :header ,header :body ,body))))

(provide 'jf-quick-help)
;;; jf-quick-help.el ends here
