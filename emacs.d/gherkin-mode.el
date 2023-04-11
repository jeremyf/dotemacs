;;; gherkin-mode --- Font locks for gherkin syntax -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; License

;; Copyright 2023 Jeremy Friesen <jeremy@jeremyfriesen.com>
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;    http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary

;; A simple package for providing font-locks for Gherkin syntax.  I find myself
;; writing Gherkin Mode inside Org-Mode code blocks.
;;
;; https://cucumber.io/docs/gherkin/

;;; Code

(define-derived-mode gherkin-mode prog-mode
  "Gherkin"
  "A mode for writing Gherkin syntax documentation."
  :group 'gherkin-mode
  (make-local-variable 'comment-start)
  (setq comment-start "# "))

(font-lock-add-keywords 'gherkin-mode
  '(("^[[:space:]]*\\(Given\\|When\\|Then\\|But\\|And\\)" . 'font-lock-keyword-face)
     ("^[[:space:]]*\\(Feature\\|Background\\|Scenario\\|Scenario Outline\\|Examples?\\|Scenarios\\):.*" . 'font-lock-doc-face)
     ("<[^>]*>" . 'font-lock-variable-name-face)
     ("^[[:space:]]*@.*"  . 'font-lock-preprocessor-face)
     ("^[[:space:]]*#.*"  . 'font-lock-comment-face)))

(provide 'gherkin-mode)
;;; gherkin-mode.el ends here
