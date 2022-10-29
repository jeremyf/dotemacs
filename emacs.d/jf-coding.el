;;; jf-coding.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;; Packages specifically here for helping with my coding activities.

;;; Code
(use-package bundler
  :straight (bundler :type git :host github :repo "endofunky/bundler.el"))

(use-package csv-mode :straight t
  ;; Always enter CSV mode in align mode; makes it easier to read.
  :hook (csv-mode . csv-align-mode))

(use-package dockerfile-mode :straight t)

(use-package editorconfig
    :straight t
    :diminish editorconfig-mode
    :config
    (editorconfig-mode 1))

(use-package eglot
  :hook ((css-mode
          enh-ruby-mode
          yaml-mode
          html-mode
          js-mode
          scss-mode) . eglot-ensure)
  :config
  (setq eglot-ignored-server-capabilites (quote (:documentHighlightProvider))
        completion-category-overrides '((eglot (styles orderless))))
  (add-to-list 'eglot-server-programs
               `(enh-ruby-mode . ("solargraph" "socket" "--port" :autoport)))
  :straight t)

(use-package eldoc :straight t)

(use-package emmet-mode
  :straight t
  :bind (("C-c C-e" . emmet-expand-yas ))
  :hook ((sgml-mode . emmet-mode)
         (html-mode . emmet-mode)
         (css-mode . emmet-mode)))

(use-package enh-ruby-mode
  :straight t
  :hook (enh-ruby-mode . (lambda () (setq fill-column 100)))
  :hook (enh-ruby-mode . eldoc-mode)
  :hook (enh-ruby-mode . enh-ruby-imenu-create-index)
  :bind (:map enh-ruby-mode-map ("C-j" . jf/jump-to-agenda-or-mark)
              ("M-h" . enh-ruby-mark-defun))
  :mode (("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
  :init (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode)))

(use-package go-mode :straight t)

(use-package json-mode :straight t)

;; Because JSON can be quite ugly, I want something to help tidy it up.
(use-package json-reformat
  :straight t
  :after json-mode
  :init (setq json-reformat:indent-width 2))

;; For working with https://www.hammerspoon.org; which provides me the
;; wonderful https://github.com/dmgerman/editWithEmacs.spoon/
(use-package lua-mode :straight t)

(use-package markdown-mode
  :straight t
  :hook ((markdown-mode . turn-on-visual-line-mode))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/opt/homebrew/bin/pandoc"))

;; A mode for working with PlantUML.  See https://plantuml.com
(use-package plantuml-mode
  :config (setq plantuml-executable-path (concat (getenv "HB_PATH") "/bin/plantuml")
                plantuml-default-exec-mode 'executable
                org-plantuml-executable-path (concat (getenv "HB_PATH") "/bin/plantuml")
                org-plantuml-exec-mode 'executable)
  :mode (("\\.plantuml\\'" . plantuml-mode))
  :straight t)

(use-package rspec-mode
  :straight t
  ;; Ensure that we’re loading enh-ruby-mode before we do any rspec loading.
  :after enh-ruby-mode
  :config
  (setq rspec-container-name "web")
  :custom
  (rspec-use-spring-when-possible nil)
  (rspec-use-docker-when-possible t)
  (rspec-docker-cwd "./")
  (rspec-docker-command "docker compose exec")
  :bind (:map rspec-mode-map (("s-." . 'rspec-toggle-spec-and-target)))
  :bind (:map enh-ruby-mode-map (("s-." . 'rspec-toggle-spec-and-target)))
  :diminish 'rspec-mode
  :init (eval-after-load 'rspec-mode '(rspec-install-snippets)))

;; Nice and simple package for string interpolation.
(use-package ruby-interpolation
  :straight t
  :diminish 'ruby-interpolation-mode
  :hook (enh-ruby-mode . ruby-interpolation-mode))

(use-package sql-indent
  :straight t
  :hook (sql-mode . sqlind-minor-mode))

(use-package string-inflection :straight t)

(use-package typescript-mode :straight t)

;; See https://github.com/emacs-tree-sitter/elisp-tree-sitter
;; Waiting on https://github.com/emacs-tree-sitter/elisp-tree-sitter/issues/197 to resolve.
(use-package tree-sitter
  :straight (tree-sitter :host github :repo "emacs-tree-sitter/elisp-tree-sitter")
  :diminish 'tree-sitter-mode
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist '(enh-ruby-mode . ruby))
  :init (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs :straight t)

(use-package vterm :straight t)

(use-package web-mode
  :straight t
  :config (setq web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2)
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list `auto-mode-alist '("\\.svg\\'" . xml-mode)))

(use-package yaml-mode :straight t)

(use-package yard-mode
  :straight t
  :diminish 'yard-mode
  :hook (enh-ruby-mode . yard-mode))


(provide 'jf-coding)
;;; jf-coding.el ends here
