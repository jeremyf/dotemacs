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

(use-package treesit
  :straight (:type built-in)
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

  (defun jf/treesit/wrap-rubocop (&optional given-cops)
    "Wrap the current ruby active region or function by disabling/enabling the GIVEN-COPS."
    (interactive)
    (if (derived-mode-p 'ruby-ts-mode 'ruby-mode)
      (if-let ((beg (if (use-region-p)
                               (region-beginning)
                               (treesit-node-start (treesit-defun-at-point))))
                (end (if (use-region-p)
                              (region-end)
                               (treesit-node-end (treesit-defun-at-point)))))
        (let ((cops (or given-cops
                      (completing-read-multiple "Cops to Disable: "
                        jf/rubocop/list-all-cops nil t))))
          (save-excursion
            (goto-char beg)
            (let ((indentation (s-repeat (current-column) " ")))
              (kill-region beg end)
              (beginning-of-line)
              (insert
                (s-join "\n"
                  (mapcar (lambda (cop)
                            (concat indentation "# rubocop:disable " cop))
                    cops))
                "\n" indentation)
              (yank)
              (insert "\n"
                (s-join "\n"
                  (mapcar (lambda (cop)
                            (concat indentation "# rubocop:enable " cop))
                    cops))))))
        (user-error "Not a region nor a function"))
      (user-error "%s is not derived from a ruby mode" major-mode)))

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

(use-package scopeline
  ;; Show the scope info of methods, blocks, if/case statements.  This is done
  ;; via an overlay for "blocks" that are more than 5 (default) lines
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
  :straight (:host github :repo "joaotavora/eglot")
  ;; The Language Server Protocol (LSP) is a game changer; having access to that
  ;; tooling is very much a nice to have.
  :hook ((css-mode css-ts-mode
           ruby-mode ruby-ts-mode
           ;; yaml-mode yaml-ts-mode
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
  (setq eglot-events-buffer-size 0)
  ;; (use-package eglot-tempel
  ;; ;; I use `tempel' and I use `eglot'; having some glue between those helps.
  ;; :straight (eglot-tempel :host github :repo "fejfighter/eglot-tempel"))
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
              #'jf/version-control/project-capf
              #'jf/version-control/issue-capf
              #'eglot-completion-at-point
              #'tempel-expand
              #'cape-keyword)))))

(use-package eldoc
  ;; Helps with rendering documentation
  ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
  :config
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
                        ("C-c C-f" . jf/treesit/qualified_method_name)
                        ("C-c C-r" . jf/treesit/wrap-rubocop)))
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

(use-package hl-todo
  :straight t
  :config (global-hl-todo-mode)
  (setq hl-todo-keyword-faces
    '(("TODO"   . "#7f0000") ;; red-intense
       ("FIXME"  . "#7f0000")))) ;; red-intense

;;
;; https://github.com/alphapapa/magit-todos.git
(use-package magit-todos
  :config (magit-todos-mode)
  :commands (magit-todos-list)
  :custom (magit-todos-exclude-globs '(".git/" "public/"))
  :straight (:host github :repo "alphapapa/magit-todos"))

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
  :preface
  (defun jf/markdown-toc (&optional depth)
    "Extract DEPTH (default 3) of headings from the current Markdown buffer.
   The generated and indented TOC will be inserted at point."
    (interactive "P")
    (let ((max-depth (or depth 3)) toc-list)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\(##+\\)\\s-+\\(.*\\)" nil t)
          (let* ((level (length (match-string 1)))
                  (heading-text (match-string 2))
                  (heading-id (downcase (replace-regexp-in-string "[[:space:]]+" "-" heading-text))))
            (when (<= level max-depth)
              (push (cons level (cons heading-text heading-id)) toc-list)))))
      (setq toc-list (reverse toc-list))
      (dolist (item toc-list)
        (let* ((level (car item))
                (heading-text (cadr item))
                (heading-id (cddr item))
                (indentation (make-string (- (* 2 (1- level)) 2) ?\ ))
                (line (format "- [%s](#%s)\n" heading-text heading-id)))
          (setq markdown-toc (concat markdown-toc (concat indentation line)))))
      (insert markdown-toc)))
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

(defun jf/require-debugger ()
  "Determine the correct debugger based on the Gemfile."
  (let ((gemfile-lock (f-join (projectile-project-root) "Gemfile.lock")))
    (if-let* ((f-exists? gemfile-lock)
               (debuggers
                 (s-split "\n"
                   (shell-command-to-string
                     (concat
                       "rg \"^ +(byebug|debugger|pry-byebug|debug) \" "
                       gemfile-lock
                       " -r '$1' --only-matching | uniq")))))
      (cond
        ((member "byebug" debuggers) "require 'byebug'; byebug")
        ((member "debug" debuggers) "require 'debug'; binding.break")
        ((member "debugger" debuggers) "require 'debugger'; debugger")
        ((member "pry-byebug" debuggers) "require 'pry-byebug'; binding.pry")
        (t "require 'debug'; binding.break"))
      "require 'debug'; binding.break")))

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
            (concat "rg \"^ +spring-commands-rspec \" " gemfile-lock))))))
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
                           identifiers)))))))
  :bind* (:map ruby-mode-map (("C-c C-f" . jf/current-scoped-function-name)
                               ("C-c C-y" . jf/ruby-mode/yardoc-ify)))
  :hook ((ruby-mode ruby-ts-mode) . yard-mode))

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
  :custom (flymake-no-changes-timeout 2))

(use-package prog-mode
  :straight (:type built-in)
  :hook (prog-mode . jf/prog-mode-configurator)
  :preface
  (defun jf/prog-mode-configurator ()
    "Do the configuration of all the things."
    ;; (electric-pair-mode)
    (flymake-mode 1)
    (which-function-mode)))

(defvar jf/comment-header-regexp/major-modes-alist
  '((emacs-lisp-mode . "^;;;;*")
     (ruby-mode . "^[[:space:]]*###*$")
     (ruby-ts-mode . "^[[:space:]]*###*$"))
  "AList of major modes and their comment headers.")

(defun jf/commend-header-forward ()
  "Move to next line matching `jf/comment-header-regexp/ruby-mode'."
  (interactive)
  (let ((regexp (alist-get major-mode jf/comment-header-regexp/major-modes-alist)))
    (when (string-match-p
            regexp
            (buffer-substring-no-properties
              (line-beginning-position)
              (line-end-position)))
      (forward-line))
    (condition-case err
      (progn
        (search-forward-regexp regexp)
        (beginning-of-line)
        (recenter scroll-margin t)
        (pulsar-pulse-line))
      (error (goto-char (point-max))))))

(defun jf/comment-header-backward ()
  (interactive)
    (let ((regexp (alist-get major-mode jf/comment-header-regexp/major-modes-alist)))
  (when (string-match-p
          regexp
          (buffer-substring-no-properties
            (line-beginning-position)
            (line-end-position)))
    (previous-line)
    (recenter scroll-margin t)
    (pulsar-pulse-line))
  (condition-case err
    (progn
      (search-backward-regexp regexp)
      (beginning-of-line)
      (recenter scroll-margin t)
      (pulsar-pulse-line))
    (error (goto-char (point-min))))))

(define-key ruby-mode-map (kbd "s-ESC") #'jf/comment-header-backward)
(define-key emacs-lisp-mode-map (kbd "s-ESC") #'jf/comment-header-backward)
(define-key ruby-mode-map (kbd "C-s-]") #'jf/commend-header-forward)
(define-key emacs-lisp-mode-map (kbd "C-s-]") #'jf/commend-header-forward)

(defun jf/ruby-ts-mode-configurator ()
  "Configure the `treesit' provided `ruby-ts-mode'."
  ;; I encountered some loading issues where ruby-ts-mode was not available
  ;; during my understanding of the use-package life-cycle.
  (setq-local add-log-current-defun-function #'jf/treesit/qualified_method_name)
  (define-key ruby-ts-mode-map (kbd "C-M-h") #'jf/treesit/function-select)
  (define-key ruby-ts-mode-map (kbd "C-c C-f") #'jf/current-scoped-function-name)
  (define-key ruby-ts-mode-map (kbd "C-c C-y") #'jf/ruby-mode/yardoc-ify)
  (define-key ruby-ts-mode-map (kbd "s-ESC") #'jf/comment-header-backward)
  (define-key ruby-ts-mode-map (kbd "C-s-]") #'jf/commend-header-forward)
  (define-key ruby-ts-mode-map (kbd "C-c C-r") #'jf/treesit/wrap-rubocop))
(add-hook 'ruby-ts-mode-hook #'jf/ruby-ts-mode-configurator)

;; From https://emacs.dyerdwelling.family/emacs/20230414111409-emacs--indexing-emacs-init/
;;
;; Creating some outline modes.  Which has me thinking about an outline mode for
;; my agenda file.
(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq imenu-sort-function 'imenu--sort-by-name)
    (setq imenu-generic-expression
      '((nil "^;;[[:space:]]+-> \\(.*\\)$" 1)
         ("defvar" "^([[:space:]]*\\(cl-\\)?defvar[[:space:]]+\\([^ ]*\\)$" 2)
         ("defconst" "^([[:space:]]*\\(cl-\\)?defconst[[:space:]]+\\([^ ]*\\)$" 2)
         ("defcustom" "^([[:space:]]*\\(cl-\\)?defcustom[[:space:]]+\\([^ ]*\\)$" 2)
         ("defun" "^([[:space:]]*\\(cl-\\)?defun[[:space:]]+\\([^(]+\\)" 2)
         ("macro" "^([[:space:]]*\\(cl-\\)?defmacro[[:space:]]+\\([^(]+\\)" 2)
         ("use-package" "^.*([[:space:]]*use-package[[:space:]]+\\([[:word:]-]+\\)" 1)))
    (imenu-add-menubar-index)))

(defvar jf/rubocop/list-all-cops
  "List of all cops."
  ;; rubocop --show-cops | rg "^([A-Z][\w/]+):" -r '$1' | pbcopy
  '("Bundler/DuplicatedGem"
     "Bundler/GemComment"
     "Bundler/GemFilename"
     "Bundler/GemVersion"
     "Bundler/InsecureProtocolSource"
     "Bundler/OrderedGems"
     "Gemspec/DependencyVersion"
     "Gemspec/DeprecatedAttributeAssignment"
     "Gemspec/DevelopmentDependencies"
     "Gemspec/DuplicatedAssignment"
     "Gemspec/OrderedDependencies"
     "Gemspec/RequireMFA"
     "Gemspec/RequiredRubyVersion"
     "Gemspec/RubyVersionGlobalsUsage"
     "Layout/AccessModifierIndentation"
     "Layout/ArgumentAlignment"
     "Layout/ArrayAlignment"
     "Layout/AssignmentIndentation"
     "Layout/BeginEndAlignment"
     "Layout/BlockAlignment"
     "Layout/BlockEndNewline"
     "Layout/CaseIndentation"
     "Layout/ClassStructure"
     "Layout/ClosingHeredocIndentation"
     "Layout/ClosingParenthesisIndentation"
     "Layout/CommentIndentation"
     "Layout/ConditionPosition"
     "Layout/DefEndAlignment"
     "Layout/DotPosition"
     "Layout/ElseAlignment"
     "Layout/EmptyComment"
     "Layout/EmptyLineAfterGuardClause"
     "Layout/EmptyLineAfterMagicComment"
     "Layout/EmptyLineAfterMultilineCondition"
     "Layout/EmptyLineBetweenDefs"
     "Layout/EmptyLines"
     "Layout/EmptyLinesAroundAccessModifier"
     "Layout/EmptyLinesAroundArguments"
     "Layout/EmptyLinesAroundAttributeAccessor"
     "Layout/EmptyLinesAroundBeginBody"
     "Layout/EmptyLinesAroundBlockBody"
     "Layout/EmptyLinesAroundClassBody"
     "Layout/EmptyLinesAroundExceptionHandlingKeywords"
     "Layout/EmptyLinesAroundMethodBody"
     "Layout/EmptyLinesAroundModuleBody"
     "Layout/EndAlignment"
     "Layout/EndOfLine"
     "Layout/ExtraSpacing"
     "Layout/FirstArgumentIndentation"
     "Layout/FirstArrayElementIndentation"
     "Layout/FirstArrayElementLineBreak"
     "Layout/FirstHashElementIndentation"
     "Layout/FirstHashElementLineBreak"
     "Layout/FirstMethodArgumentLineBreak"
     "Layout/FirstMethodParameterLineBreak"
     "Layout/FirstParameterIndentation"
     "Layout/HashAlignment"
     "Layout/HeredocArgumentClosingParenthesis"
     "Layout/HeredocIndentation"
     "Layout/IndentationConsistency"
     "Layout/IndentationStyle"
     "Layout/IndentationWidth"
     "Layout/InitialIndentation"
     "Layout/LeadingCommentSpace"
     "Layout/LeadingEmptyLines"
     "Layout/LineContinuationLeadingSpace"
     "Layout/LineContinuationSpacing"
     "Layout/LineEndStringConcatenationIndentation"
     "Layout/MultilineArrayBraceLayout"
     "Layout/MultilineArrayLineBreaks"
     "Layout/MultilineAssignmentLayout"
     "Layout/MultilineBlockLayout"
     "Layout/MultilineHashBraceLayout"
     "Layout/MultilineHashKeyLineBreaks"
     "Layout/MultilineMethodArgumentLineBreaks"
     "Layout/MultilineMethodCallBraceLayout"
     "Layout/MultilineMethodCallIndentation"
     "Layout/MultilineMethodDefinitionBraceLayout"
     "Layout/MultilineMethodParameterLineBreaks"
     "Layout/MultilineOperationIndentation"
     "Layout/ParameterAlignment"
     "Layout/RedundantLineBreak"
     "Layout/RescueEnsureAlignment"
     "Layout/SingleLineBlockChain"
     "Layout/SpaceAfterColon"
     "Layout/SpaceAfterComma"
     "Layout/SpaceAfterMethodName"
     "Layout/SpaceAfterNot"
     "Layout/SpaceAfterSemicolon"
     "Layout/SpaceAroundBlockParameters"
     "Layout/SpaceAroundEqualsInParameterDefault"
     "Layout/SpaceAroundKeyword"
     "Layout/SpaceAroundMethodCallOperator"
     "Layout/SpaceAroundOperators"
     "Layout/SpaceBeforeBlockBraces"
     "Layout/SpaceBeforeBrackets"
     "Layout/SpaceBeforeComma"
     "Layout/SpaceBeforeComment"
     "Layout/SpaceBeforeFirstArg"
     "Layout/SpaceBeforeSemicolon"
     "Layout/SpaceInLambdaLiteral"
     "Layout/SpaceInsideArrayLiteralBrackets"
     "Layout/SpaceInsideArrayPercentLiteral"
     "Layout/SpaceInsideBlockBraces"
     "Layout/SpaceInsideHashLiteralBraces"
     "Layout/SpaceInsideParens"
     "Layout/SpaceInsidePercentLiteralDelimiters"
     "Layout/SpaceInsideRangeLiteral"
     "Layout/SpaceInsideReferenceBrackets"
     "Layout/SpaceInsideStringInterpolation"
     "Layout/TrailingEmptyLines"
     "Layout/TrailingWhitespace"
     "Lint/AmbiguousAssignment"
     "Lint/AmbiguousBlockAssociation"
     "Lint/AmbiguousOperator"
     "Lint/AmbiguousOperatorPrecedence"
     "Lint/AmbiguousRange"
     "Lint/AmbiguousRegexpLiteral"
     "Lint/AssignmentInCondition"
     "Lint/BigDecimalNew"
     "Lint/BinaryOperatorWithIdenticalOperands"
     "Lint/BooleanSymbol"
     "Lint/CircularArgumentReference"
     "Lint/ConstantDefinitionInBlock"
     "Lint/ConstantOverwrittenInRescue"
     "Lint/ConstantResolution"
     "Lint/Debugger"
     "Lint/DeprecatedClassMethods"
     "Lint/DeprecatedConstants"
     "Lint/DeprecatedOpenSSLConstant"
     "Lint/DisjunctiveAssignmentInConstructor"
     "Lint/DuplicateBranch"
     "Lint/DuplicateCaseCondition"
     "Lint/DuplicateElsifCondition"
     "Lint/DuplicateHashKey"
     "Lint/DuplicateMagicComment"
     "Lint/DuplicateMatchPattern"
     "Lint/DuplicateMethods"
     "Lint/DuplicateRegexpCharacterClassElement"
     "Lint/DuplicateRequire"
     "Lint/DuplicateRescueException"
     "Lint/EachWithObjectArgument"
     "Lint/ElseLayout"
     "Lint/EmptyBlock"
     "Lint/EmptyClass"
     "Lint/EmptyConditionalBody"
     "Lint/EmptyEnsure"
     "Lint/EmptyExpression"
     "Lint/EmptyFile"
     "Lint/EmptyInPattern"
     "Lint/EmptyInterpolation"
     "Lint/EmptyWhen"
     "Lint/EnsureReturn"
     "Lint/ErbNewArguments"
     "Lint/FlipFlop"
     "Lint/FloatComparison"
     "Lint/FloatOutOfRange"
     "Lint/FormatParameterMismatch"
     "Lint/HashCompareByIdentity"
     "Lint/HeredocMethodCallPosition"
     "Lint/IdentityComparison"
     "Lint/ImplicitStringConcatenation"
     "Lint/IncompatibleIoSelectWithFiberScheduler"
     "Lint/IneffectiveAccessModifier"
     "Lint/InheritException"
     "Lint/InterpolationCheck"
     "Lint/LambdaWithoutLiteralBlock"
     "Lint/LiteralAsCondition"
     "Lint/LiteralInInterpolation"
     "Lint/Loop"
     "Lint/MissingCopEnableDirective"
     "Lint/MissingSuper"
     "Lint/MixedRegexpCaptureTypes"
     "Lint/MultipleComparison"
     "Lint/NestedMethodDefinition"
     "Lint/NestedPercentLiteral"
     "Lint/NextWithoutAccumulator"
     "Lint/NoReturnInBeginEndBlocks"
     "Lint/NonAtomicFileOperation"
     "Lint/NonDeterministicRequireOrder"
     "Lint/NonLocalExitFromIterator"
     "Lint/NumberConversion"
     "Lint/NumberedParameterAssignment"
     "Lint/OrAssignmentToConstant"
     "Lint/OrderedMagicComments"
     "Lint/OutOfRangeRegexpRef"
     "Lint/ParenthesesAsGroupedExpression"
     "Lint/PercentStringArray"
     "Lint/PercentSymbolArray"
     "Lint/RaiseException"
     "Lint/RandOne"
     "Lint/RedundantCopDisableDirective"
     "Lint/RedundantCopEnableDirective"
     "Lint/RedundantDirGlobSort"
     "Lint/RedundantRequireStatement"
     "Lint/RedundantSafeNavigation"
     "Lint/RedundantSplatExpansion"
     "Lint/RedundantStringCoercion"
     "Lint/RedundantWithIndex"
     "Lint/RedundantWithObject"
     "Lint/RefinementImportMethods"
     "Lint/RegexpAsCondition"
     "Lint/RequireParentheses"
     "Lint/RequireRangeParentheses"
     "Lint/RequireRelativeSelfPath"
     "Lint/RescueException"
     "Lint/RescueType"
     "Lint/ReturnInVoidContext"
     "Lint/SafeNavigationChain"
     "Lint/SafeNavigationConsistency"
     "Lint/SafeNavigationWithEmpty"
     "Lint/ScriptPermission"
     "Lint/SelfAssignment"
     "Lint/SendWithMixinArgument"
     "Lint/ShadowedArgument"
     "Lint/ShadowedException"
     "Lint/ShadowingOuterLocalVariable"
     "Lint/StructNewOverride"
     "Lint/SuppressedException"
     "Lint/SymbolConversion"
     "Lint/Syntax"
     "Lint/ToEnumArguments"
     "Lint/ToJSON"
     "Lint/TopLevelReturnWithArgument"
     "Lint/TrailingCommaInAttributeDeclaration"
     "Lint/TripleQuotes"
     "Lint/UnderscorePrefixedVariableName"
     "Lint/UnexpectedBlockArity"
     "Lint/UnifiedInteger"
     "Lint/UnmodifiedReduceAccumulator"
     "Lint/UnreachableCode"
     "Lint/UnreachableLoop"
     "Lint/UnusedBlockArgument"
     "Lint/UnusedMethodArgument"
     "Lint/UriEscapeUnescape"
     "Lint/UriRegexp"
     "Lint/UselessAccessModifier"
     "Lint/UselessAssignment"
     "Lint/UselessElseWithoutRescue"
     "Lint/UselessMethodDefinition"
     "Lint/UselessRescue"
     "Lint/UselessRuby2Keywords"
     "Lint/UselessSetterCall"
     "Lint/UselessTimes"
     "Lint/Void"
     "Metrics/AbcSize"
     "Metrics/BlockLength"
     "Metrics/BlockNesting"
     "Metrics/ClassLength"
     "Metrics/CollectionLiteralLength"
     "Metrics/CyclomaticComplexity"
     "Metrics/LineLength"
     "Metrics/MethodLength"
     "Metrics/ModuleLength"
     "Metrics/ParameterLists"
     "Metrics/PerceivedComplexity"
     "Migration/DepartmentName"
     "Naming/AccessorMethodName"
     "Naming/AsciiIdentifiers"
     "Naming/BinaryOperatorParameterName"
     "Naming/BlockForwarding"
     "Naming/BlockParameterName"
     "Naming/ClassAndModuleCamelCase"
     "Naming/ConstantName"
     "Naming/FileName"
     "Naming/HeredocDelimiterCase"
     "Naming/HeredocDelimiterNaming"
     "Naming/InclusiveLanguage"
     "Naming/MemoizedInstanceVariableName"
     "Naming/MethodName"
     "Naming/MethodParameterName"
     "Naming/PredicateName"
     "Naming/RescuedExceptionsVariableName"
     "Naming/VariableName"
     "Naming/VariableNumber"
     "Performance/AncestorsInclude"
     "Performance/ArraySemiInfiniteRangeSlice"
     "Performance/BigDecimalWithNumericArgument"
     "Performance/BindCall"
     "Performance/BlockGivenWithExplicitBlock"
     "Performance/Caller"
     "Performance/CaseWhenSplat"
     "Performance/Casecmp"
     "Performance/ChainArrayAllocation"
     "Performance/CollectionLiteralInLoop"
     "Performance/CompareWithBlock"
     "Performance/ConcurrentMonotonicTime"
     "Performance/ConstantRegexp"
     "Performance/Count"
     "Performance/DeletePrefix"
     "Performance/DeleteSuffix"
     "Performance/Detect"
     "Performance/DoubleStartEndWith"
     "Performance/EndWith"
     "Performance/FixedSize"
     "Performance/FlatMap"
     "Performance/InefficientHashSearch"
     "Performance/IoReadlines"
     "Performance/MapCompact"
     "Performance/MethodObjectAsBlock"
     "Performance/OpenStruct"
     "Performance/RangeInclude"
     "Performance/RedundantBlockCall"
     "Performance/RedundantEqualityComparisonBlock"
     "Performance/RedundantMatch"
     "Performance/RedundantMerge"
     "Performance/RedundantSortBlock"
     "Performance/RedundantSplitRegexpArgument"
     "Performance/RedundantStringChars"
     "Performance/RegexpMatch"
     "Performance/ReverseEach"
     "Performance/ReverseFirst"
     "Performance/SelectMap"
     "Performance/Size"
     "Performance/SortReverse"
     "Performance/Squeeze"
     "Performance/StartWith"
     "Performance/StringIdentifierArgument"
     "Performance/StringInclude"
     "Performance/StringReplacement"
     "Performance/Sum"
     "Performance/TimesMap"
     "Performance/UnfreezeString"
     "Performance/UriDefaultParser"
     "RSpec/AlignLeftLetBrace"
     "RSpec/AlignRightLetBrace"
     "RSpec/AnyInstance"
     "RSpec/AroundBlock"
     "RSpec/Be"
     "RSpec/BeEq"
     "RSpec/BeEql"
     "RSpec/BeNil"
     "RSpec/BeforeAfterAll"
     "RSpec/ChangeByZero"
     "RSpec/ContextMethod"
     "RSpec/ContextWording"
     "RSpec/DescribeClass"
     "RSpec/DescribeMethod"
     "RSpec/DescribeSymbol"
     "RSpec/DescribedClass"
     "RSpec/DescribedClassModuleWrapping"
     "RSpec/Dialect"
     "RSpec/EmptyExampleGroup"
     "RSpec/EmptyHook"
     "RSpec/EmptyLineAfterExample"
     "RSpec/EmptyLineAfterExampleGroup"
     "RSpec/EmptyLineAfterFinalLet"
     "RSpec/EmptyLineAfterHook"
     "RSpec/EmptyLineAfterSubject"
     "RSpec/ExampleLength"
     "RSpec/ExampleWithoutDescription"
     "RSpec/ExampleWording"
     "RSpec/ExcessiveDocstringSpacing"
     "RSpec/ExpectActual"
     "RSpec/ExpectChange"
     "RSpec/ExpectInHook"
     "RSpec/ExpectOutput"
     "RSpec/FilePath"
     "RSpec/Focus"
     "RSpec/HookArgument"
     "RSpec/HooksBeforeExamples"
     "RSpec/IdenticalEqualityAssertion"
     "RSpec/ImplicitBlockExpectation"
     "RSpec/ImplicitExpect"
     "RSpec/ImplicitSubject"
     "RSpec/InstanceSpy"
     "RSpec/InstanceVariable"
     "RSpec/ItBehavesLike"
     "RSpec/IteratedExpectation"
     "RSpec/LeadingSubject"
     "RSpec/LeakyConstantDeclaration"
     "RSpec/LetBeforeExamples"
     "RSpec/LetSetup"
     "RSpec/MessageChain"
     "RSpec/MessageExpectation"
     "RSpec/MessageSpies"
     "RSpec/MissingExampleGroupArgument"
     "RSpec/MultipleDescribes"
     "RSpec/MultipleExpectations"
     "RSpec/MultipleMemoizedHelpers"
     "RSpec/MultipleSubjects"
     "RSpec/NamedSubject"
     "RSpec/NestedGroups"
     "RSpec/NotToNot"
     "RSpec/OverwritingSetup"
     "RSpec/Pending"
     "RSpec/PredicateMatcher"
     "RSpec/ReceiveCounts"
     "RSpec/ReceiveNever"
     "RSpec/RepeatedDescription"
     "RSpec/RepeatedExample"
     "RSpec/RepeatedExampleGroupBody"
     "RSpec/RepeatedExampleGroupDescription"
     "RSpec/RepeatedIncludeExample"
     "RSpec/ReturnFromStub"
     "RSpec/ScatteredLet"
     "RSpec/ScatteredSetup"
     "RSpec/SharedContext"
     "RSpec/SharedExamples"
     "RSpec/SingleArgumentMessageChain"
     "RSpec/StubbedMock"
     "RSpec/SubjectDeclaration"
     "RSpec/SubjectStub"
     "RSpec/UnspecifiedException"
     "RSpec/VariableDefinition"
     "RSpec/VariableName"
     "RSpec/VerifiedDoubleReference"
     "RSpec/VerifiedDoubles"
     "RSpec/VoidExpect"
     "RSpec/Yield"
     "RSpec/Capybara/CurrentPathExpectation"
     "RSpec/Capybara/FeatureMethods"
     "RSpec/Capybara/VisibilityMatcher"
     "RSpec/FactoryBot/AttributeDefinedStatically"
     "RSpec/FactoryBot/CreateList"
     "RSpec/FactoryBot/FactoryClassName"
     "RSpec/FactoryBot/SyntaxMethods"
     "RSpec/Rails/AvoidSetupHook"
     "RSpec/Rails/HttpStatus"
     "Rails/ActionControllerTestCase"
     "Rails/ActionFilter"
     "Rails/ActiveRecordAliases"
     "Rails/ActiveRecordCallbacksOrder"
     "Rails/ActiveRecordOverride"
     "Rails/ActiveSupportAliases"
     "Rails/AddColumnIndex"
     "Rails/AfterCommitOverride"
     "Rails/ApplicationController"
     "Rails/ApplicationJob"
     "Rails/ApplicationMailer"
     "Rails/ApplicationRecord"
     "Rails/ArelStar"
     "Rails/AssertNot"
     "Rails/AttributeDefaultBlockValue"
     "Rails/BelongsTo"
     "Rails/Blank"
     "Rails/BulkChangeTable"
     "Rails/CompactBlank"
     "Rails/ContentTag"
     "Rails/CreateTableWithTimestamps"
     "Rails/Date"
     "Rails/DefaultScope"
     "Rails/Delegate"
     "Rails/DelegateAllowBlank"
     "Rails/DeprecatedActiveModelErrorsMethods"
     "Rails/DotSeparatedKeys"
     "Rails/DuplicateAssociation"
     "Rails/DuplicateScope"
     "Rails/DurationArithmetic"
     "Rails/DynamicFindBy"
     "Rails/EagerEvaluationLogMessage"
     "Rails/EnumHash"
     "Rails/EnumUniqueness"
     "Rails/EnvironmentComparison"
     "Rails/EnvironmentVariableAccess"
     "Rails/Exit"
     "Rails/ExpandedDateRange"
     "Rails/FilePath"
     "Rails/FindBy"
     "Rails/FindById"
     "Rails/FindEach"
     "Rails/HasAndBelongsToMany"
     "Rails/HasManyOrHasOneDependent"
     "Rails/HelperInstanceVariable"
     "Rails/HttpPositionalArguments"
     "Rails/HttpStatus"
     "Rails/I18nLazyLookup"
     "Rails/I18nLocaleAssignment"
     "Rails/I18nLocaleTexts"
     "Rails/IgnoredSkipActionFilterOption"
     "Rails/IndexBy"
     "Rails/IndexWith"
     "Rails/Inquiry"
     "Rails/InverseOf"
     "Rails/LexicallyScopedActionFilter"
     "Rails/LinkToBlank"
     "Rails/MailerName"
     "Rails/MatchRoute"
     "Rails/MigrationClassName"
     "Rails/NegateInclude"
     "Rails/NotNullColumn"
     "Rails/OrderById"
     "Rails/Output"
     "Rails/OutputSafety"
     "Rails/Pick"
     "Rails/Pluck"
     "Rails/PluckId"
     "Rails/PluckInWhere"
     "Rails/PluralizationGrammar"
     "Rails/Presence"
     "Rails/Present"
     "Rails/RakeEnvironment"
     "Rails/ReadWriteAttribute"
     "Rails/RedundantAllowNil"
     "Rails/RedundantForeignKey"
     "Rails/RedundantPresenceValidationOnBelongsTo"
     "Rails/RedundantReceiverInWithOptions"
     "Rails/RedundantTravelBack"
     "Rails/ReflectionClassName"
     "Rails/RefuteMethods"
     "Rails/RelativeDateConstant"
     "Rails/RenderInline"
     "Rails/RenderPlainText"
     "Rails/RequestReferer"
     "Rails/RequireDependency"
     "Rails/ReversibleMigration"
     "Rails/ReversibleMigrationMethodDefinition"
     "Rails/RootJoinChain"
     "Rails/RootPublicPath"
     "Rails/SafeNavigation"
     "Rails/SafeNavigationWithBlank"
     "Rails/SaveBang"
     "Rails/SchemaComment"
     "Rails/ScopeArgs"
     "Rails/ShortI18n"
     "Rails/SkipsModelValidations"
     "Rails/SquishedSQLHeredocs"
     "Rails/StripHeredoc"
     "Rails/TableNameAssignment"
     "Rails/TimeZone"
     "Rails/TimeZoneAssignment"
     "Rails/ToFormattedS"
     "Rails/TransactionExitStatement"
     "Rails/UniqBeforePluck"
     "Rails/UniqueValidationWithoutIndex"
     "Rails/UnknownEnv"
     "Rails/UnusedIgnoredColumns"
     "Rails/Validation"
     "Rails/WhereEquals"
     "Rails/WhereExists"
     "Rails/WhereNot"
     "Security/CompoundHash"
     "Security/Eval"
     "Security/IoMethods"
     "Security/JSONLoad"
     "Security/MarshalLoad"
     "Security/Open"
     "Security/YAMLLoad"
     "Style/AccessModifierDeclarations"
     "Style/AccessorGrouping"
     "Style/Alias"
     "Style/AndOr"
     "Style/ArgumentsForwarding"
     "Style/ArrayCoercion"
     "Style/ArrayIntersect"
     "Style/ArrayJoin"
     "Style/AsciiComments"
     "Style/Attr"
     "Style/AutoResourceCleanup"
     "Style/BarePercentLiterals"
     "Style/BeginBlock"
     "Style/BisectedAttrAccessor"
     "Style/BlockComments"
     "Style/BlockDelimiters"
     "Style/CaseEquality"
     "Style/CaseLikeIf"
     "Style/CharacterLiteral"
     "Style/ClassAndModuleChildren"
     "Style/ClassCheck"
     "Style/ClassEqualityComparison"
     "Style/ClassMethods"
     "Style/ClassMethodsDefinitions"
     "Style/ClassVars"
     "Style/CollectionCompact"
     "Style/CollectionMethods"
     "Style/ColonMethodCall"
     "Style/ColonMethodDefinition"
     "Style/CombinableLoops"
     "Style/CommandLiteral"
     "Style/CommentAnnotation"
     "Style/CommentedKeyword"
     "Style/ComparableClamp"
     "Style/ConcatArrayLiterals"
     "Style/ConditionalAssignment"
     "Style/ConstantVisibility"
     "Style/Copyright"
     "Style/DataInheritance"
     "Style/DateTime"
     "Style/DefWithParentheses"
     "Style/Dir"
     "Style/DirEmpty"
     "Style/DisableCopsWithinSourceCodeDirective"
     "Style/DocumentDynamicEvalDefinition"
     "Style/Documentation"
     "Style/DocumentationMethod"
     "Style/DoubleCopDisableDirective"
     "Style/DoubleNegation"
     "Style/EachForSimpleLoop"
     "Style/EachWithObject"
     "Style/EmptyBlockParameter"
     "Style/EmptyCaseCondition"
     "Style/EmptyElse"
     "Style/EmptyHeredoc"
     "Style/EmptyLambdaParameter"
     "Style/EmptyLiteral"
     "Style/EmptyMethod"
     "Style/Encoding"
     "Style/EndBlock"
     "Style/EndlessMethod"
     "Style/EnvHome"
     "Style/EvalWithLocation"
     "Style/EvenOdd"
     "Style/ExpandPathArguments"
     "Style/ExplicitBlockArgument"
     "Style/ExponentialNotation"
     "Style/FetchEnvVar"
     "Style/FileEmpty"
     "Style/FileRead"
     "Style/FileWrite"
     "Style/FloatDivision"
     "Style/For"
     "Style/FormatString"
     "Style/FormatStringToken"
     "Style/FrozenStringLiteralComment"
     "Style/GlobalStdStream"
     "Style/GlobalVars"
     "Style/GuardClause"
     "Style/HashAsLastArrayItem"
     "Style/HashConversion"
     "Style/HashEachMethods"
     "Style/HashExcept"
     "Style/HashLikeCase"
     "Style/HashSyntax"
     "Style/HashTransformKeys"
     "Style/HashTransformValues"
     "Style/IdenticalConditionalBranches"
     "Style/IfInsideElse"
     "Style/IfUnlessModifier"
     "Style/IfUnlessModifierOfIfUnless"
     "Style/IfWithBooleanLiteralBranches"
     "Style/IfWithSemicolon"
     "Style/ImplicitRuntimeError"
     "Style/InPatternThen"
     "Style/InfiniteLoop"
     "Style/InlineComment"
     "Style/InverseMethods"
     "Style/InvertibleUnlessCondition"
     "Style/IpAddresses"
     "Style/KeywordParametersOrder"
     "Style/Lambda"
     "Style/LambdaCall"
     "Style/LineEndConcatenation"
     "Style/MagicCommentFormat"
     "Style/MapCompactWithConditionalBlock"
     "Style/MapToHash"
     "Style/MapToSet"
     "Style/MethodCallWithArgsParentheses"
     "Style/MethodCallWithoutArgsParentheses"
     "Style/MethodCalledOnDoEndBlock"
     "Style/MethodDefParentheses"
     "Style/MinMax"
     "Style/MinMaxComparison"
     "Style/MissingElse"
     "Style/MissingRespondToMissing"
     "Style/MixinGrouping"
     "Style/MixinUsage"
     "Style/ModuleFunction"
     "Style/MultilineBlockChain"
     "Style/MultilineIfModifier"
     "Style/MultilineIfThen"
     "Style/MultilineInPatternThen"
     "Style/MultilineMemoization"
     "Style/MultilineMethodSignature"
     "Style/MultilineTernaryOperator"
     "Style/MultilineWhenThen"
     "Style/MultipleComparison"
     "Style/MutableConstant"
     "Style/NegatedIf"
     "Style/NegatedIfElseCondition"
     "Style/NegatedUnless"
     "Style/NegatedWhile"
     "Style/NestedFileDirname"
     "Style/NestedModifier"
     "Style/NestedParenthesizedCalls"
     "Style/NestedTernaryOperator"
     "Style/Next"
     "Style/NilComparison"
     "Style/NilLambda"
     "Style/NonNilCheck"
     "Style/Not"
     "Style/NumberedParameters"
     "Style/NumberedParametersLimit"
     "Style/NumericLiteralPrefix"
     "Style/NumericLiterals"
     "Style/NumericPredicate"
     "Style/ObjectThen"
     "Style/OneLineConditional"
     "Style/OpenStructUse"
     "Style/OperatorMethodCall"
     "Style/OptionHash"
     "Style/OptionalArguments"
     "Style/OptionalBooleanParameter"
     "Style/OrAssignment"
     "Style/ParallelAssignment"
     "Style/ParenthesesAroundCondition"
     "Style/PercentLiteralDelimiters"
     "Style/PercentQLiterals"
     "Style/PerlBackrefs"
     "Style/PreferredHashMethods"
     "Style/Proc"
     "Style/QuotedSymbols"
     "Style/RaiseArgs"
     "Style/RandomWithOffset"
     "Style/RedundantArgument"
     "Style/RedundantAssignment"
     "Style/RedundantBegin"
     "Style/RedundantCapitalW"
     "Style/RedundantCondition"
     "Style/RedundantConditional"
     "Style/RedundantConstantBase"
     "Style/RedundantDoubleSplatHashBraces"
     "Style/RedundantEach"
     "Style/RedundantException"
     "Style/RedundantFetchBlock"
     "Style/RedundantFileExtensionInRequire"
     "Style/RedundantFreeze"
     "Style/RedundantHeredocDelimiterQuotes"
     "Style/RedundantInitialize"
     "Style/RedundantInterpolation"
     "Style/RedundantLineContinuation"
     "Style/RedundantParentheses"
     "Style/RedundantPercentQ"
     "Style/RedundantRegexpCharacterClass"
     "Style/RedundantRegexpEscape"
     "Style/RedundantReturn"
     "Style/RedundantSelf"
     "Style/RedundantSelfAssignment"
     "Style/RedundantSelfAssignmentBranch"
     "Style/RedundantSort"
     "Style/RedundantSortBy"
     "Style/RedundantStringEscape"
     "Style/RegexpLiteral"
     "Style/RequireOrder"
     "Style/RescueModifier"
     "Style/RescueStandardError"
     "Style/ReturnNil"
     "Style/SafeNavigation"
     "Style/Sample"
     "Style/SelectByRegexp"
     "Style/SelfAssignment"
     "Style/Semicolon"
     "Style/Send"
     "Style/SignalException"
     "Style/SingleArgumentDig"
     "Style/SingleLineBlockParams"
     "Style/SingleLineMethods"
     "Style/SlicingWithRange"
     "Style/SoleNestedConditional"
     "Style/SpecialGlobalVars"
     "Style/StabbyLambdaParentheses"
     "Style/StaticClass"
     "Style/StderrPuts"
     "Style/StringChars"
     "Style/StringConcatenation"
     "Style/StringHashKeys"
     "Style/StringLiterals"
     "Style/StringLiteralsInInterpolation"
     "Style/StringMethods"
     "Style/Strip"
     "Style/StructInheritance"
     "Style/SwapValues"
     "Style/SymbolArray"
     "Style/SymbolLiteral"
     "Style/SymbolProc"
     "Style/TernaryParentheses"
     "Style/TopLevelMethodDefinition"
     "Style/TrailingBodyOnClass"
     "Style/TrailingBodyOnMethodDefinition"
     "Style/TrailingBodyOnModule"
     "Style/TrailingCommaInArguments"
     "Style/TrailingCommaInArrayLiteral"
     "Style/TrailingCommaInBlockArgs"
     "Style/TrailingCommaInHashLiteral"
     "Style/TrailingMethodEndStatement"
     "Style/TrailingUnderscoreVariable"
     "Style/TrivialAccessors"
     "Style/UnlessElse"
     "Style/UnlessLogicalOperators"
     "Style/UnpackFirst"
     "Style/VariableInterpolation"
     "Style/WhenThen"
     "Style/WhileUntilDo"
     "Style/WhileUntilModifier"
     "Style/WordArray"
     "Style/YodaCondition"
     "Style/YodaExpression"
     "Style/ZeroLengthPredicate"))

;; (require 'jf-copilot)

(provide 'jf-coding)
;;; jf-coding.el ends here
