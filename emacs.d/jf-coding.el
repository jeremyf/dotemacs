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
;;
;; On <2023-03-25 Sat> I read the `tree-sitter' README and they encouraged Emacs
;; 29.x to use the built-in `treesit' package.
;; (use-package tree-sitter
;;   ;; See https://github.com/emacs-tree-sitter/elisp-tree-sitter
;;   :straight (tree-sitter :host github
;; 			 :repo "emacs-tree-sitter/elisp-tree-sitter")
;;   :config
;;   (add-to-list 'tree-sitter-major-mode-language-alist '(ruby-mode . ruby))
;;   :init (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs
;;   ;; Provides the languages "dictionaries" for tree-sitter highlighting.
;;   :straight t)

(use-package treesit
  :custom (treesit-font-lock-level 4)
  :init
  ;; This function, tested against Ruby, will return the module space qualified
  ;; method name (e.g. Hello::World#method_name).
  (cl-defun jf/treesit/qualified_method_name (&key (type "method"))
    "Get the fully qualified name of method at point."
    (interactive)
    (if-let ((func (treesit-defun-at-point)))
      ;; Instance method or class method?
      (let* ((method_type (if (string= type
                                (treesit-node-type func))
                            "#" "."))
	            (method_name (treesit-node-text
                             (car (treesit-filter-child
			                              func
			                              (lambda (node)
			                                (string= "identifier"
                                        (treesit-node-type node)))))))
              (qualified_name (format "%s%s%s"
                                (s-join "::"
                                  (-flatten
                                    (jf/treesit/module_space func)))
                                method_type method_name)))
        (message qualified_name)
        (kill-new (substring-no-properties qualified_name)))
      (user-error "No %s at point." type)))
  ;; An ugly bit of code to recurse upwards from the node to the "oldest"
  ;; parent.  And collect all module/class nodes along the way.
  (defun jf/treesit/module_space (node)
    (when-let* ((parent (treesit-parent-until node
                        (lambda (n) (member (treesit-node-type n)
                                      '("class" "module")))))
               (parent_name (treesit-node-text
                              (car (treesit-filter-child
					               parent (lambda (n)
					                        (string= "constant"
                                    (treesit-node-type n))))))))
      (list (jf/treesit/module_space parent) parent_name)))
  :straight (:type  built-in))

(use-package treesit-auto
  :straight (:host github :repo "renzmann/treesit-auto")
  :config (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;;;; Other packages and their configurations
(use-package bundler
  ;; For Ruby package management
  :straight (bundler :type git :host github :repo "endofunky/bundler.el"))

(use-package csv-mode
  :straight t
  ;; By default I want to show the separator character.
  :custom (csv-invisibility-default nil)
  ;; Always enter CSV mode in align mode; makes it easier to read.
  :hook (csv-mode . csv-align-mode))

;; https://github.com/Silex/docker.el
(use-package docker
  ;; A reality of modern development is that things happen in Docker.
  :straight t)

(use-package dockerfile-mode
  ;; Given that I interact with docker files, I should have some syntax
  ;; awareness.
  :straight t)

(use-package editorconfig
  ;; “EditorConfig helps maintain consistent coding styles for multiple
  ;; developers working on the same project across various editors and IDEs.”
  ;; See https://editorconfig.org/#overview for more details.
  :straight t
  :config
  (editorconfig-mode 1))

(use-package eglot
  ;; The Language Server Protocol (LSP) is a game changer; having access to that
  ;; tooling is very much a nice to have.
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
    ;; I don't want `eglot-completion-at-point' to trample my other completion
    ;; options.
    ;;
    ;; https://stackoverflow.com/questions/72601990/how-to-show-suggestions-for-yasnippets-when-using-eglot
    (setq-local completion-at-point-functions
		(list (cape-super-capf
                       #'eglot-completion-at-point
                       #'tempel-expand
		       #'cape-file))))
  :straight t)

(use-package eglot-tempel
  ;; I use `tempel' and I use `eglot'; having some glue between those helps.
  :after (eglot)
  :straight (eglot-tempel :host github :repo "fejfighter/eglot-tempel"))

(use-package eldoc
  ;; Helps with rendering documentation
  :straight t)

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

(define-derived-mode gherkin-mode prog-mode
  "GK"
  :group 'gherkin-mode
  (make-local-variable 'comment-start)
  (setq comment-start "# "))

(font-lock-add-keywords 'gherkin-mode
  '(("^[[:space:]]*\\(Given\\|When\\|Then\\|But\\|And\\)" . 'font-lock-keyword-face)
     ("^[[:space:]]*\\(Feature\\|Background\\|Scenario\\|Scenario Outline\\|Examples\\|Scenarios\\):.*" . 'font-lock-doc-face)
     ("<[^>]*>" . 'font-lock-variable-name-face)
     ("^[[:space:]]*@.*"  . 'font-lock-preprocessor-face)
     ("^[[:space:]]*#.*"  . 'font-lock-comment-face)))

(use-package ruby-mode
  ;; My language of choice for professional work.
  :straight (:type built-in)
  :bind (:map ruby-mode-map ("C-c C-f" . jf/treesit/qualified_method_name))
  (:map ruby-ts-mode-map ("C-c C-f" . jf/treesit/qualified_method_name))
  :hook ((ruby-mode ruby-ts-mode) . (lambda () (setq fill-column 100))))

;; I don't use this package
;; (use-package go-mode :straight t)

;; An odd little creature, hide all comment lines.  Sometimes this can be a
;; useful tool for viewing implementation details.
(require 'hide-comnt)

(use-package json-mode
  ;; The web's data structure of choice is JSON.
  :straight t)

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
  ;;
  ;;
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
  ;; I write most of my Ruby tests using rspec.  This tool helps manage that
  ;; process.
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
  ;; Dear reader, make sure that you can jump from spec and definition.  And in
  ;; Ruby land when you have lib/my_file.rb, the corresponding spec should be in
  ;; spec/my_file_spec.rb; and when you have app/models/my_file.rb, the spec
  ;; should be in spec/models/my_file_spec.rb
  :bind (:map rspec-mode-map (("s-." . 'rspec-toggle-spec-and-target)))
  :bind (:map ruby-mode-map (("s-." . 'rspec-toggle-spec-and-target)))
  :init
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
  (advice-add #'rspec-spring-p :override #'jf/rspec-spring-p))

(use-package ruby-interpolation
  ;; Nice and simple package for string interpolation.
  :straight t
  :hook (ruby-mode . ruby-interpolation-mode))

(use-package sql-indent
  ;; SQL, oh how I love thee and wish I worked more with thee.
  :straight t
  :hook (sql-mode . sqlind-minor-mode))

(use-package string-inflection
  ;; A quick way to change case and separators for words.
  :straight t)

(use-package typescript-mode
  ;; I have this for the work I once did a few years ago.  I am happiest when
  ;; I'm not working in Javascript.
  :straight t)

(use-package vterm
  ;; A terminal in Emacs.
  :straight t)

(use-package web-mode
  ;; Help consistently edit web documents of SGML markup dialetcs.
  :straight t
  :config (setq web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2)
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list `auto-mode-alist '("\\.svg\\'" . xml-mode)))

(use-package xml-format
  ;; Encountering unformatted XML is jarring; this package helps formatt it for
  ;; human legibility.
  :straight t
  :after nxml-mode)

(use-package yaml-mode
  ;; Oh yaml, I once thought you better than XML.  Now, you are ubiquitous and a
  ;; bit imprecise.  Still better than JSON; which doesn't allow for comments.
  :straight t)

(use-package yard-mode
  ;; My prefered Ruby documentation syntax
  :straight t
  :init
  (defun jf/ruby-mode/yardoc-ify ()
  "Add parameter yarddoc stubs for the current method."
  (interactive)
  ;; Remember where we started.
  (save-excursion
    ;; Goto the beginning of the function
    (ruby-beginning-of-defun)
    ;; Move to just after the first (
    (search-forward "(")
    ;; Move back to just before the (
    (backward-char)
    ;; Select parameters declaration
    (mark-sexp)
    ;; Copy that
    (copy-region-as-kill (point) (mark))
    ;; Split apart the parameters into their identifiers
    (let ((identifiers (mapcar (lambda (token)
			    (replace-regexp-in-string
			     "[^a-z|_]" ""
			     (car (s-split " "
					   (s-trim token)))))
			  (s-split "," (substring-no-properties
					(car kill-ring))))))
      ;; Go to the beginning of the function again
      (ruby-beginning-of-defun)
      ;; Now insert the identifiers as yardoc
      (insert "##\n"
	      (s-join "\n" (mapcar
			    (lambda (param)
			      (concat "# @param "
				      param
				      " [Object]"))
			    identifiers))
	      "\n"))))
  :bind (:map ruby-mode-map (("C-c C-y" . jf/ruby-mode/yardoc-ify)))
  (:map ruby-ts-mode-map (("C-c C-y" . jf/ruby-mode/yardoc-ify)))
  :hook ((ruby-mode ruby-ts-mode) . yard-mode))

(use-package devdocs
  ;; Download and install documents from https://devdocs.io/
  ;; Useful for having local inline docs
  :straight t
  :commands (devdocs-install))


(provide 'jf-coding)
;;; jf-coding.el ends here
