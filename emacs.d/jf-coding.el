;;; jf-coding.el --- Packages related to "coding" -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Coding is writing, but not all writing is coding.  This configures and
;; extends packages specifically here for helping with my coding activities.

;;; Code:

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
;;       :repo "emacs-tree-sitter/elisp-tree-sitter")
;;   :config
;;   (add-to-list 'tree-sitter-major-mode-language-alist '(ruby-mode . ruby))
;;   :init (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs
;;   ;; Provides the languages "dictionaries" for tree-sitter highlighting.
;;   :straight t)

(use-package treesit
  :init
  (setq treesit-font-lock-level 4)
  :preface
  (defun jf/treesit/function-select ()
    "Select the current function at point."
    (interactive)
    (if-let ((func (treesit-defun-at-point)))
      (progn
        (goto-char (treesit-node-start func))
        (call-interactively #'set-mark-command)
        (goto-char (treesit-node-end func)))
      (user-error "No function to select")))
  ;; I love M-q and also have some opinions about how my yard docs should look.
  ;; This allows both of those to peacefully coexist.
  (defun jf/ruby-mode/unfill-toggle ()
    "Either `unfill-toggle' or `tidy-ruby-docs'."
    (interactive)
    (if (string= (treesit-node-type (treesit-node-at (point))) "comment")
      (save-excursion
        ;; Grab the first non-comment line
        (jf/treesit/tidy-ruby-docs (point)))
      (unfill-toggle)))

  ;; A function to tidy up the yardocs in the comment section the method.  This
  ;; is a mostly idempotent script that will format a yardoc comment section to
  ;; my preferred structure/indentation.
  ;;
  ;; TODO: At present this does not handle comment lines of the form:
  ;; "^ *# \w"
  (defun jf/treesit/tidy-ruby-docs (cursor)
    "Tidy the ruby yardoc at CURSOR."
    (interactive "d")
    ;; Ensure that we position at the beginning of the keyword declaration.
    (end-of-line)
    (search-backward-regexp "^ *# +@")
    (if (s-match "^ *# +@example" (jf/get-line-text))
      ;; Move past the example line.
      (progn (next-line) (search-forward-regexp "^ *# +@\\w+"))
      ;; Otherwise
      (progn
        ;; Because the `unfill-toggle' treats comments as the same area, we need
        ;; to add blank lines around the keyword section.
        (newline)
        (set-mark (point))
        (let* ((oldmark (mark))
                ;; Determine the size of the keyword.
                (line (jf/get-line-text nil))
                (keyword_length (length (car (s-match "@\\w+" line)))))
          (search-forward-regexp "^ *# +@\\w+")
          (while (s-match "^ *# [^@]" (jf/get-line-text 1))
            (next-line))
          (end-of-line)
          (newline)
          (backward-char)
          (end-of-line)
          (fill-region oldmark (point))
          (search-backward-regexp "^ *# +@")
          (when (s-match "^ *# [^@]" (jf/get-line-text 1))
            (progn
              (next-line)
              (beginning-of-line)
              (search-forward-regexp "^ *#")
              ;; Now pad that next line
              (let ((padding (s-repeat (+ 1 keyword_length) " ")))
                (unless (s-match (concat "^ *#" padding) (jf/get-line-text))
                  (insert padding)))
              (deactivate-mark)
              (unfill-toggle))))
        (forward-paragraph)
        (delete-char -1)
        (set-mark (point))
        (backward-paragraph)
        (delete-char 1)
        (goto-char (mark))))
    ;; While we're still in the same comment section; move to the next
    ;; interesting element (e.g. non-empty lines)
    (when (string= "comment" (treesit-node-type (treesit-node-at (point))))
      (progn (search-forward-regexp "^ *# ?[^ ]+")
        (jf/treesit/tidy-ruby-docs (point)))))

  ;; This function, tested against Ruby, will return the module space qualified
  ;; method name (e.g. Hello::World#method_name).
  (cl-defun jf/treesit/qualified_method_name ()
    "Return the fully qualified name of method at point.  If not on a
method, get the containing class."
    (if-let ((func (treesit-defun-at-point)))
      ;; Instance method or class method?
      (let* ((method_type (if (string= "method"
                                (treesit-node-type func))
                            "#" "."))
              (method_name (treesit-node-text
                             (car (treesit-filter-child
                                    func
                                    (lambda (node)
                                      (string= "identifier"
                                        (treesit-node-type node)))))))
              (module_space (s-join "::" (jf/treesit/module_space func))))
        (if current-prefix-arg
          module_space
          (concat module_space method_type method_name)))
      (let ((current-node (treesit-node-at (point))))
        (s-join "::" (jf/treesit/module_space current-node)))))
  ;; Handles the following Ruby code:
  ;;
  ;;   module A::B
  ;;     module C
  ;;     end
  ;;     C::D = Struct.new do
  ;;       def call
  ;;       end
  ;;     end
  ;;   end
  ;; Special thanks to https://eshelyaron.com/posts/2023-04-01-take-on-recursion.html
  (defun jf/treesit/module_space (node &optional acc)
    (if-let ((parent (treesit-parent-until
                       node
                       (lambda (n) (member (treesit-node-type n)
                                     '("class" "module" "assignment")))))
              (parent_name (treesit-node-text
                             (car
                               (treesit-filter-child
                                 parent
                                 (lambda (n)
                                   (member (treesit-node-type n)
                                     '("constant" "scope_resolution"))))))))
      (jf/treesit/module_space parent (cons parent_name acc))
      acc)))


(use-package treesit-auto
  :straight (:host github :repo "renzmann/treesit-auto")
  :config (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; Show the scope info of methods, blocks, if/case statements
(use-package scopeline
  :straight (:host github :repo "jeremyf/scopeline.el")
  :hook ((ruby-mode ruby-ts-mode) . scopeline-mode))

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
           json-mode json-ts-mode
           scss-mode scss-ts-mode)
          . eglot-ensure)
  :hook ((eglot-managed-mode . jf/eglot-eldoc)
          (eglot-managed-mode . jf/eglot-capf))
  :preface
  (defun jf/eglot-eldoc ()
    ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
    (setq eldoc-documentation-strategy
      'eldoc-documentation-compose-eagerly))
  :config
  (add-to-list 'eglot-stay-out-of 'flymake)
  (setq completion-category-overrides '((eglot (styles orderless))))
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
              #'cape-keyword))))
  :straight t)

(use-package eglot-tempel
  ;; I use `tempel' and I use `eglot'; having some glue between those helps.
  :after (eglot)
  :straight (eglot-tempel :host github :repo "fejfighter/eglot-tempel"))

(use-package eldoc
  ;; Helps with rendering documentation
  ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
  :preface
  (add-to-list 'display-buffer-alist
    '("^\\*eldoc for" display-buffer-at-bottom
       (window-height . 4)))
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  :straight t)

;; I don't use this package (I think...):
;; (use-package emmet-mode
;;   :straight t
;;   :bind (("C-c C-e" . emmet-expand-yas ))
;;   :hook ((sgml-mode . emmet-mode)
;;          (html-mode . emmet-mode)
;;          (css-mode . emmet-mode)))

(require 'gherkin-mode)

(use-package ruby-mode
  ;; My language of choice for professional work.
  :straight (:type built-in)
  :custom (ruby-flymake-use-rubocop-if-available nil)
  :bind
  (:map ruby-mode-map (("C-M-h" . jf/treesit/function-select)
                        ("C-c C-f" . jf/treesit/qualified_method_name)))
  :hook ((ruby-mode ruby-ts-mode) .
          (lambda ()
            (eldoc-mode)
            (setq fill-column 100))))

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
  :init
  (setq markdown-command "/opt/homebrew/bin/pandoc")
  (font-lock-add-keywords 'markdown-mode
    '(("{{[^}]+}}" . 'font-lock-function-name-face))))

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
  :preface
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
  :preface
  ;; This is not working as I had tested; it's very dependent on the little
  ;; details.  I think I may want to revisit to just work on the current line.
  (defun jf/ruby-mode/yardoc-ify ()
    "Add parameter yarddoc stubs for the current method."
    (interactive)
    (save-excursion
      (when-let* ((func (treesit-defun-at-point))
                   (method_parameters_text
                     (treesit-node-text (car
                                          (treesit-filter-child
                                            func
                                            (lambda (node)
                                              (string= "method_parameters"
                                                (treesit-node-type node))))))))
        (goto-char (treesit-node-start func))
        ;; Grab the parameter names.
        (let* ((identifiers (mapcar (lambda (token)
                                      (replace-regexp-in-string
                                        "[^a-z|_]" ""
                                        (car (s-split " "
                                               (s-trim token)))))
                              (s-split "," method_parameters_text)))
                (indentation (s-repeat (current-column) " ")))
          (previous-line)
          (end-of-line)
          (insert
            (concat "\n" indentation "##\n")
            (s-join "\n" (mapcar
                           (lambda (param)
                             (concat indentation "# @param "
                               param
                               " [Object]"))
                           identifiers))))))))
  :bind* (:map ruby-mode-map (("C-c C-f" . jf/current-scoped-function-name)
                               ("C-c C-y" . jf/ruby-mode/yardoc-ify)))
  :hook ((ruby-mode ruby-ts-mode) . yard-mode))

(defun jf/ruby-ts-mode-configurator ()
  "Configure the `treesit' provided `ruby-ts-mode'."
  ;; I encountered some loading issues where ruby-ts-mode was not available
  ;; during my understanding of the use-package life-cycle.
  (setq-local add-log-current-defun-function #'jf/treesit/qualified_method_name)
  (define-key ruby-ts-mode-map (kbd "C-M-h") #'jf/treesit/function-select)
  (define-key ruby-ts-mode-map (kbd "C-c C-f") #'jf/current-scoped-function-name)
  (define-key ruby-ts-mode-map (kbd "C-c C-y") #'jf/ruby-mode/yardoc-ify))
(add-hook 'ruby-ts-mode-hook #'jf/ruby-ts-mode-configurator)

;; I didn't know about `add-log-current-defun-function' until a blog reader
;; reached out.  Now, I'm making a general function for different modes.
(defun jf/current-scoped-function-name ()
  "Echo and kill the current scoped function name.

See `add-log-current-defun-function'."
  (interactive)
  (if-let ((text (funcall add-log-current-defun-function)))
    (progn
      (message "%s" text)
      (kill-new (substring-no-properties text)))
    (user-error "Warning: Point not on function")))
(bind-key "C-c C-f" #'jf/current-scoped-function-name prog-mode-map)
(bind-key "C-c C-f" #'jf/current-scoped-function-name emacs-lisp-mode-map)

(use-package devdocs
  ;; Download and install documents from https://devdocs.io/
  ;; Useful for having local inline docs.  Perhaps not always in the format that
  ;; I want, but can't have everything.
  :straight t
  :commands (devdocs-install))

(use-package flymake
  :straight t
  ;; Don't be so hasty in syntax checking.
  :custom (flymake-no-changes-timeout 3))

(use-package prog-mode
  :straight (:type built-in)
  :hook (prog-mode . jf/prog-mode-configurator)
  :preface
  (defun jf/prog-mode-configurator ()
    "Do the configuration of all the things."
    ;; (electric-pair-mode)
    (flymake-mode 1)
    (which-function-mode)))

;; From https://emacs.dyerdwelling.family/emacs/20230414111409-emacs--indexing-emacs-init/
;;
;; Creating some outline modes.  Which has me thinking about an outline mode for
;; my agenda file.
(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq imenu-sort-function 'imenu--sort-by-name)
    (setq imenu-generic-expression
      '((nil "^;;[[:space:]]+-> \\(.*\\)$" 1)
         ("defun" "^.*([[:space:]]*\\(cl-\\)?defun[[:space:]]+\\([^(]+\\)" 2)
         ("macro" "^.*([[:space:]]*\\(cl-\\)?defmacro[[:space:]]+\\([^(]+\\)" 2)
         ("use-package" "^.*([[:space:]]*use-package[[:space:]]+\\([[:word:]-]+\\)" 1)))
    (imenu-add-menubar-index)))

(provide 'jf-coding)
;;; jf-coding.el ends here
