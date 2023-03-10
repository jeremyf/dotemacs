;;; jf-coding.el --- Packages related to "coding" -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary

;; Coding is writing, but not all writing is coding.  This configures and
;; extends packages specifically here for helping with my coding activities.

;;; Code
;;;; Pre-requisites
(require 'jf-writing)

;; On <2023-03-08 Thu> I was noticing that `treesit' as implemented appeared to
;; have notable latency.  I did some profiling and found that for a Ruby file
;; Emacs spent quite a bit of time in the `treesit' functions.  I toggled back to
;; `tree-sitter' and did not notice any slowness.
(use-package tree-sitter
  ;; See https://github.com/emacs-tree-sitter/elisp-tree-sitter
  :straight (tree-sitter :host github
			 :repo "emacs-tree-sitter/elisp-tree-sitter")
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist '(ruby-mode . ruby))
  :init (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs :straight t)

;; (use-package treesit
;;   :custom (treesit-font-lock-level 4)
;;   :straight (:type  built-in))

;; (use-package treesit-auto
;;   :straight (:host github :repo "renzmann/treesit-auto")
;;   :config (setq treesit-auto-install 'prompt)
;;   :config
;;   (global-treesit-auto-mode))

;;;; Other packages and their configurations
(use-package bundler
  :straight (bundler :type git :host github :repo "endofunky/bundler.el"))

(use-package csv-mode :straight t
  ;; By default I want to show the separator character.
  :custom (csv-invisibility-default nil)
  ;; Always enter CSV mode in align mode; makes it easier to read.
  :hook (csv-mode . csv-align-mode))

;; https://github.com/Silex/docker.el
(use-package docker
  :straight t)

(use-package dockerfile-mode :straight t)

(use-package editorconfig
    :straight t
    :config
    (editorconfig-mode 1))

(use-package eglot
  :hook ((css-mode css-ts-mode
          ruby-mode ruby-ts-mode
          yaml-mode yaml-ts-mode
          html-mode html-ts-mode
          js-mode js-ts-mode
          scss-mode scss-ts-mode)
	 . eglot-ensure)
  :hook (eglot-managed-mode . jf/eglot-capf)
  :config
  (setq eglot-ignored-server-capabilites (quote (:documentHighlightProvider))
        completion-category-overrides '((eglot (styles orderless))))
  (add-to-list 'eglot-server-programs
               `(ruby-mode . ("solargraph" "socket" "--port" :autoport)))
  (add-to-list 'eglot-server-programs
               `(ruby-ts-mode . ("solargraph" "socket" "--port" :autoport)))

  (defun jf/eglot-capf ()
    ;; https://stackoverflow.com/questions/72601990/how-to-show-suggestions-for-yasnippets-when-using-eglot
    (setq-local completion-at-point-functions
		(list (cape-super-capf
                       #'eglot-completion-at-point
                       #'tempel-expand
		       #'cape-file))))
  :straight t)

(use-package eglot-tempel
  :after (eglot)
  :straight (eglot-tempel :host github :repo "fejfighter/eglot-tempel"))

(use-package eldoc :straight t)

;; I don't use this package
;; (use-package emacs-refactor
;;   :straight (emacs-refactor :host github :repo "Wilfred/emacs-refactor")
;;   :bind ((:map ruby-mode-map ("M-RET" . emr-show-refactor-menu))
;; 	 (:map emacs-lisp-mode-map ("M-RET" . emr-show-refactor-menu))))

;; I don't use this package (I think...):
;; (use-package emmet-mode
;;   :straight t
;;   :bind (("C-c C-e" . emmet-expand-yas ))
;;   :hook ((sgml-mode . emmet-mode)
;;          (html-mode . emmet-mode)
;;          (css-mode . emmet-mode)))

(use-package ruby-mode
  :straight (:type built-in)
  :hook ((ruby-mode ruby-ts-mode) . (lambda () (setq fill-column 100))))

;; I don't use this package
;; (use-package go-mode :straight t)

(require 'hide-comnt)

(use-package json-mode :straight t)

(use-package json-reformat
  ;; Because JSON can be quite ugly, I want something to help tidy it up.
  :straight t
  :after json-mode
  :init (setq json-reformat:indent-width 2))

(use-package lua-mode
  ;; For working with https://www.hammerspoon.org; which provides me the
  ;; wonderful https://github.com/dmgerman/editWithEmacs.spoon/
  :straight t)

(use-package markdown-mode
  :straight t
  :hook (((markdown-mode markdown-ts-mode) . turn-on-visual-line-mode))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/opt/homebrew/bin/pandoc"))

(use-package plantuml-mode
  ;; A mode for working with PlantUML.  See https://plantuml.com
  :config (setq plantuml-executable-path (concat
					  (getenv "HB_PATH")
					  "/bin/plantuml")
                plantuml-default-exec-mode 'executable
                org-plantuml-executable-path (concat
					      (getenv "HB_PATH")
					      "/bin/plantuml")
                org-plantuml-exec-mode 'executable)
  :mode (("\\.plantuml\\'" . plantuml-mode))
  :mode (("\\.puml\\'" . plantuml-mode))
  :straight t)

(use-package rspec-mode
  :straight t
  ;; Ensure that we’re loading ruby-mode before we do any rspec loading.
  :after ruby-mode
  :custom
  (rspec-docker-container "web")
  (rspec-use-spring-when-possible t)
  (rspec-use-docker-when-possible t)
  (rspec-docker-cwd "./")
  (rspec-docker-command "docker compose exec")
  :hook ((dired-mode . rspec-dired-mode)
	 (ruby-mode . rspec-mode)
	 (ruby-ts-mode . rspec-mode))
  :bind (:map rspec-mode-map (("s-." . 'rspec-toggle-spec-and-target)))
  :bind (:map ruby-mode-map (("s-." . 'rspec-toggle-spec-and-target))))

(defun jf/rspec-spring-p ()
  "Check the project for spring as part of the Gemfile.lock."
  (let ((gemfile-lock (f-join (projectile-project-root) "Gemfile.lock")))
    (and (f-exists? gemfile-lock)
	 (s-present?
	  (shell-command-to-string
	   (concat "rg \"^ +spring \" " gemfile-lock))))))

;; Out of the box, for my typical docker ecosystem, the `rspec-spring-p'
;; function does not work.  So I'm overriding the default behavior to match my
;; ecosystem.
(advice-add #'rspec-spring-p :override #'jf/rspec-spring-p)

(use-package ruby-interpolation
  ;; Nice and simple package for string interpolation.
  :straight t
  :hook (ruby-mode . ruby-interpolation-mode))

(use-package sql-indent
  :straight t
  :hook (sql-mode . sqlind-minor-mode))

(use-package string-inflection :straight t)

(use-package typescript-mode :straight t)

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

(use-package xml-format
  :straight t
  :after nxml-mode)

(use-package yaml-mode :straight t)

(use-package yard-mode
  :straight t
  :hook ((ruby-mode . yard-mode)
	 (ruby-ts-mode . yard-mode)))

;; Download and install documents from https://devdocs.io/
(use-package devdocs
  :straight t
  :commands (devdocs-install))


(provide 'jf-coding)
;;; jf-coding.el ends here
