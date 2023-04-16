;;; jf-minor-mode-maker.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Provides a macro for personal minor mode declaration.  Why the macro?  As a
;; matter of practice and documentation.  I also want to get better at writing.

;;; Code:
(require 'cl-macs)
(cl-defmacro jf/minor-mode-maker (&key title abbr hooks keymap)
    "A macro to declare a minor mode.

  Use TITLE to derive the docstring.
  Use ABBR to derive the mode-name lighter.
  Add hook to each HOOKS provided.
And assign a KEYMAP."
    (let ((mode-name (intern (s-downcase (concat "jf/" abbr "-minor-mode"))))
          (lighter (concat " " abbr))
          (docstring (concat "Minor mode for " title ".")))
      `(progn
         (define-minor-mode ,mode-name
           ,docstring
           :init-value nil
           :global nil
           :keymap ,keymap
           :lighter ,lighter)
         (when ,hooks
           (-each ,hooks (lambda(hook) (add-hook hook (lambda () (,mode-name)))))))))

(provide 'jf-minor-mode-maker)
;;; jf-minor-mode-maker.el ends here
