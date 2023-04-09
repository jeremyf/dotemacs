;;; gherkin-mode --- Font locks for gherkin syntax -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary

;; A simple package for providing font-locks for Gherkin syntax.

;;; Code

(define-derived-mode gherkin-mode prog-mode
  "Gherkin"
  "A mode for writing Gherkin syntax documentation."
  :group 'gherkin-mode
  (make-local-variable 'comment-start)
  (setq comment-start "# "))

(font-lock-add-keywords 'gherkin-mode
  '(("^[[:space:]]*\\(Given\\|When\\|Then\\|But\\|And\\)" . 'font-lock-keyword-face)
     ("^[[:space:]]*\\(Feature\\|Background\\|Scenario\\|Scenario Outline\\|Examples\\|Scenarios\\):.*" . 'font-lock-doc-face)
     ("<[^>]*>" . 'font-lock-variable-name-face)
     ("^[[:space:]]*@.*"  . 'font-lock-preprocessor-face)
     ("^[[:space:]]*#.*"  . 'font-lock-comment-face)))

(provide 'gherkin-mode)
;;; gherkin-mode.el ends here
