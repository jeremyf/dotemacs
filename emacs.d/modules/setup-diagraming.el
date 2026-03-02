;;; setup-diagraming --- So you want to diagram -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;;; Code:
(use-package plantuml-mode
  ;; A mode for working with PlantUML.  See https://plantuml.com
  :mode (("\\.plantuml\\'" . plantuml-mode))
  :mode (("\\.puml\\'" . plantuml-mode))
  :straight t)

(use-package mermaid-mode
  :straight t)

(use-package ob-mermaid
  :after (org)
  :straight t
  :custom (ob-mermaid-cli-path (executable-find "mmdc")))

(use-package eplot :straight
  (:host github :repo "larsmagne/eplot"))

(if (eq system-type 'darwin)
  (setq plantuml-executable-path (s-trim (shell-command-to-string "which plantuml"))
    plantuml-default-exec-mode 'executable
    org-plantuml-executable-path plantuml-executable-path))

(provide 'setup-diagraming)
;;; setup-diagraming.el ends here
