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

(use-package adaptive-wrap
  :custom (adaptive-wrap-extra-indent 4)
  :straight t)

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
    "Wrap the current ruby region by disabling/enabling the GIVEN-COPS."
    (interactive)
    (if (derived-mode-p 'ruby-ts-mode 'ruby-mode)
      (if-let ((region (jf/treesit/derive-region-for-rubocop)))
        (let ((cops
                (or given-cops
                  (completing-read-multiple "Cops to Disable: "
                    jf/rubocop/list-all-cops nil t))))
          (save-excursion
            (goto-char (cdr region))
            (call-interactively #'crux-move-beginning-of-line)
            (let ((indentation (s-repeat (current-column) " ")))
              (goto-char (cdr region))
              (insert "\n"
                (s-join "\n"
                  (mapcar
                    (lambda (cop)
                      (concat indentation "# rubocop:enable " cop))
                    cops)))
              (goto-char (car region))
              (beginning-of-line)
              (insert
                (s-join "\n"
                  (mapcar
                    (lambda (cop)
                      (concat indentation "# rubocop:disable " cop))
                    cops))
                "\n"))))
        (user-error "Not a region nor a function"))
      (user-error "%s is not derived from a ruby mode" major-mode)))

  (defun jf/treesit/derive-region-for-rubocop ()
    "Return `cons' of begin and end positions of region."
    (cond
      ;; When given, first honor the explicit region
      ((use-region-p)
        (cons (region-beginning) (region-end)))
      ;; Then honor the current function
      ((treesit-defun-at-point)
        (cons (treesit-node-start (treesit-defun-at-point))
          (treesit-node-end (treesit-defun-at-point))))
      ;; Then fallback to attempting to find the containing
      ;; class/module.
      (t
        (when-let ((node
                     (treesit-parent-until
                       (treesit-node-at (point))
                       (lambda (n) (member (treesit-node-type n)
                                     '("class" "module"))))))
          (cons (treesit-node-start node) (treesit-node-end node))))))

  ;; This function, tested against Ruby, will return the module space
  ;; qualified method name (e.g. Hello::World#method_name).
  (cl-defun jf/treesit/yank-qualified-method-fname ()
    "Return the fully qualified name of method at point.  If not on a
method, get the containing class."
    (if-let ((func (treesit-defun-at-point)))
      ;; Instance method or class method?
      (let* ((method_type
               (if (string= "method"
                     (treesit-node-type func))
                 "#" "."))
              (method_name
                (treesit-node-text
                  (car (treesit-filter-child
                         func
                         (lambda (node)
                           (string= "identifier"
                             (treesit-node-type node)))))))
              (module_space
                (s-join "::" (jf/treesit/module_space func))))
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
    (if-let ((parent
               (treesit-parent-until
                 node
                 (lambda (n) (member (treesit-node-type n)
                               '("class" "module" "assignment")))))
              (parent_name
                (treesit-node-text
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
  ;; Show the scope info of methods, blocks, if/case statements.  This
  ;; is done via an overlay for "blocks" that are more than 5 (default)
  ;; lines
  :straight (:host github :repo "jeremyf/scopeline.el")
  ;; The original scopeline prefix was creating line height issues for
  ;; my font of choice.  Namely adding just a bit more spacing for the
  ;; scopeline overlay, thus making line heights inconsistent.
  :config (setq scopeline-overlay-prefix "  ~ ")
  :hook ((ruby-mode ruby-ts-mode) . scopeline-mode))

;;;; Other packages and their configurations
(use-package bundler
  ;; For Ruby package management
  :straight (bundler
              :type git
              :host github
              :repo "endofunky/bundler.el"))

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
  ;; developers working on the same project across various editors and
  ;; IDEs.”  See https://editorconfig.org/#overview for more details.
  :straight t
  :config
  (editorconfig-mode 1))

(require 'jf-lsp)

(use-package emacs
  :hook (emacs-lisp-mode . jf/emacs-lisp-mode-hook)
  :preface
  (defun jf/emacs-lisp-mode-hook ()
    ;; 72 is what I've found works best on exporting to my blog.
    (setq-local fill-column 72)))

;; I don't use this package (I think...):
;; (use-package emmet-mode
;;   :straight t
;;   :bind (("C-c C-e" . emmet-expand-yas ))
;;   :hook ((sgml-mode . emmet-mode)
;;          (html-mode . emmet-mode)
;;          (css-mode . emmet-mode)))

(use-package go-mode
  :straight t
  :hook ((go-mode go-ts-mode) . jf/go-mode)
  :config
  (defun jf/go-mode ()
    (setq-local tab-width 2)))

(setq go-ts-mode-indent-offset 2)

(use-package go-imenu
  :straight t
  :hook (go-mode . go-imenu-setup))

;; (use-package flymake-go
;;   :straight t)

(require 'gherkin-mode)

(use-package ruby-mode
  ;; My language of choice for professional work.
  :straight (:type built-in)
  :custom (ruby-flymake-use-rubocop-if-available nil)
  :bind
  (:map ruby-mode-map
    (("C-M-h" . jf/treesit/function-select)
      ("C-c y f" . jf/treesit/yank-qualified-method-fname)
      ("C-c w r" . jf/treesit/wrap-rubocop)
      ("M-{" . ruby-beginning-of-block)
      ("M-}" . ruby-end-of-block)))
  :hook ((ruby-mode ruby-ts-mode) . #'jf/ruby-mode-configurator)
  :config
  (defun jf/ruby-mode-configurator ()
    (eldoc-mode t)
    (setq-local fill-column 80)))

(use-package python
  :straight (:type built-in)
  :hook (python-mode . jf/python-mode-configurator)
  :bind (:map python-mode-map ("M-." . xref-find-definitions))
  :config
  (defun jf/python-mode-configurator ()
    (eldoc-mode t)
    (python-docstring-mode t)
    (setq-default python-indent-offset 4)
    (setq-local fill-column 80)))

(defun jf/python-ts-mode-configurator ()
  (define-key python-ts-mode-map
    (kbd "M-.") #'xref-find-definitions)
  (jf/python-mode-configurator))

(add-hook 'python-ts-mode-hook #'jf/python-ts-mode-configurator)

(use-package flymake-ruff
  :straight t)
;;   :hook (eglot-managed-mode . flymake-ruff-load))

(use-package python-docstring
  :straight t)

(use-package pydoc-info
  :straight t
  :config
  (dolist (python '(python-mode python-ts-mode))
    (info-lookup-add-help
      :mode python
      :parse-rule 'pydoc-info-python-symbol-at-point
      :doc-spec
      '(("(python)Index" pydoc-info-lookup-transform-entry)
         ("(sphinx)Index" pydoc-info-lookup-transform-entry)))))

(use-package virtualenvwrapper
  :straight t
  :config
  ;; if you want interactive shell support
  (venv-initialize-interactive-shells)
  ;; if you want eshell support note that setting `venv-location` is not
  ;; necessary if you use the default location (`~/.virtualenvs`), or if
  ;; the the environment variable `WORKON_HOME` points to the right
  ;; place
  (venv-initialize-eshell)
  (setq projectile-switch-project-action
      '(lambda ()
         (venv-projectile-auto-workon)
         (projectile-find-file))))

;; An odd little creature, hide all comment lines.  Sometimes this can
;; be a useful tool for viewing implementation details.
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
  :config (global-hl-todo-mode))

;;
;; https://github.com/alphapapa/magit-todos.git
(use-package magit-todos
  :config (magit-todos-mode)
  :commands (magit-todos-list)
  :custom (magit-todos-exclude-globs '(".git/" "public/"))
  (magit-todos-insert-after
    '(bottom) nil nil
    "Changed by setter of obsolete option `magit-todos-insert-at'")
  :straight (:host github :repo "alphapapa/magit-todos"))

(use-package lua-mode
  ;; For working with https://www.hammerspoon.org; which provides me the
  ;; wonderful https://github.com/dmgerman/editWithEmacs.spoon/
  :straight t)

(use-package markdown-mode
  :straight t
  :bind (:map markdown-mode-map ("C-c C-j" . jf/project/jump-to-task))
  :hook (((markdown-mode markdown-ts-mode) . turn-on-visual-line-mode))
  :mode (("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . markdown-mode)
          ("\\.markdown\\'" . markdown-mode))
  :preface
  (defun jf/markdown-toc (&optional depth)
    "Extract DEPTH of headings from the current Markdown buffer.
   The generated and indented TOC will be inserted at point."
    (interactive "P")
    (let ((max-depth (or depth 3)) toc-list)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\(##+\\)\\s-+\\(.*\\)" nil t)
          (let* ((level
                   (length (match-string 1)))
                  (heading-text
                    (match-string 2))
                  (heading-id
                    (downcase (replace-regexp-in-string
                                "[[:space:]]+" "-" heading-text))))
            (when (<= level max-depth)
              (push (cons level
                      (cons heading-text heading-id))
                toc-list)))))
      (setq toc-list (reverse toc-list))
      (dolist (item toc-list)
        (let* ((level
                 (car item))
                (heading-text
                  (cadr item))
                (heading-id
                  (cddr item))
                (indentation
                  (make-string (- (* 2 (1- level)) 2) ?\ ))
                (line
                  (format "- [%s](#%s)\n" heading-text heading-id)))
          (setq markdown-toc
            (concat markdown-toc (concat indentation line)))))
      (insert markdown-toc)))
  :init
  (setq markdown-command "/usr/local/bin/pandoc")
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
  (let ((gemfile-lock
          (f-join (projectile-project-root) "Gemfile.lock")))
    (if-let* ((f-exists? gemfile-lock)
               (debuggers
                 (s-split "\n"
                   (shell-command-to-string
                     (concat
                       "rg \"^ +(byebug|debugger|pry-byebug|debug) \" "
                       gemfile-lock
                       " -r '$1' --only-matching | uniq")))))
      (cond
        ((member "byebug" debuggers)
          "require 'byebug'; byebug")
        ((member "debug" debuggers)
          "require 'debug'; binding.break")
        ((member "debugger" debuggers)
          "require 'debugger'; debugger")
        ((member "pry-byebug" debuggers)
          "require 'pry-byebug'; binding.pry")
        (t "require 'debug'; binding.break"))
      "require 'debug'; binding.break")))

(use-package rspec-mode
  ;; I write most of my Ruby tests using rspec.  This tool helps manage
  ;; that process.
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
          (rspec-mode . jf/rspec-mode-hook))
  ;; Dear reader, make sure that you can jump from spec and definition.
  ;; And in Ruby land when you have lib/my_file.rb, the corresponding
  ;; spec should be in spec/my_file_spec.rb; and when you have
  ;; app/models/my_file.rb, the spec should be in
  ;; spec/models/my_file_spec.rb
  :bind (:map rspec-mode-map
          (("s-." .
             'rspec-toggle-spec-and-target)
            ("C-c y r" .
              'jf/yank-bundle-exec-rspec-to-clipboard)))
  :bind (:map ruby-mode-map (("s-." . 'rspec-toggle-spec-and-target)))
  :preface
  (defun jf/rspec-mode-hook ()
    (setq imenu-generic-expression
      '(("Method" "^\\s *def\\s +\\([^\(\n; ]+\\)" 1)
         ("Describe" "^\\( *\\(its?\\|specify\\|example\\|describe\\|context\\|feature\\|scenario\\) +.+\\)" 1))))
  (defun jf/yank-bundle-exec-rspec-to-clipboard ()
    "Grab a ready to run rspec command."
    (interactive)
    (let* ((filename
             (file-relative-name (buffer-file-name)
               (projectile-project-root)))
            (text
              (format "bundle exec rspec %s:%s"
                filename (line-number-at-pos))))
      (kill-new text)
      (message "Killed: %s" text)
      text))
  (defun jf/rspec-spring-p ()
    "Check the project for spring as part of the Gemfile.lock."
    (let ((gemfile-lock
            (f-join (projectile-project-root) "Gemfile.lock")))
      (and (f-exists? gemfile-lock)
        (s-present?
          (shell-command-to-string
            (concat "rg \"^ +spring-commands-rspec \" "
              gemfile-lock))))))
  ;; Out of the box, for my typical docker ecosystem, the
  ;; `rspec-spring-p' function does not work.  So I'm overriding the
  ;; default behavior to match my ecosystem.
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
  ;; I have this for the work I once did a few years ago.  I am happiest
  ;; when I'm not working in Javascript.
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
  ;; Encountering unformatted XML is jarring; this package helps format
  ;; it for human legibility.
  :straight t
  :after nxml-mode)

(use-package yaml-mode
  ;; Oh yaml, I once thought you better than XML.  Now, you are
  ;; ubiquitous and a bit imprecise.  Still better than JSON; which
  ;; doesn't allow for comments.
  :straight t)

(use-package combobulate
  :straight (:host github :repo "mickeynp/combobulate")
  :hook ((json-ts-mode . combobulate-mode)
          (html-ts-mode . combobulate-mode)
          (yaml-ts-mode . combobulate-mode)))

(use-package yard-mode
  ;; My prefered Ruby documentation syntax
  :straight t
  :preface
  ;; This is not working as I had tested; it's very dependent on the
  ;; little details.  I think I may want to revisit to just work on the
  ;; current line.
  (defun jf/ruby-mode/yank-yardoc ()
    "Add parameter yarddoc stubs for the current method."
    (interactive)
    (save-excursion
      (when-let* ((func (treesit-defun-at-point))
                   (method_parameters_text
                     (treesit-node-text
                       (car
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
  :bind* (:map ruby-mode-map
           (("C-c y f" . jf/yank-current-scoped-function-name)
             ("C-c y y" . jf/ruby-mode/yank-yardoc)))
  :hook ((ruby-mode ruby-ts-mode) . yard-mode))

;; I didn't know about `add-log-current-defun-function' until a blog
;; reader reached out.  Now, I'm making a general function for different
;; modes.
(defun jf/yank-current-scoped-function-name ()
  "Echo and kill the current scoped function name.

See `add-log-current-defun-function'."
  (interactive)
  (if-let ((text (funcall add-log-current-defun-function)))
    (progn
      (message "%s" text)
      (kill-new (substring-no-properties text)))
    (user-error "Warning: Point not on function")))
(bind-key "C-c y f"
  #'jf/yank-current-scoped-function-name prog-mode-map)
(bind-key "C-c y f"
  #'jf/yank-current-scoped-function-name emacs-lisp-mode-map)

(use-package devdocs
  ;; Download and install documents from https://devdocs.io/ Useful for
  ;; having local inline docs.  Perhaps not always in the format that I
  ;; want, but can't have everything.
  :straight t
  :commands (devdocs-install))

;; An alternate to devdocs.  Facilitates downloading HTML files and
;; index.
(use-package dash-docs
  :straight t)

(use-package consult-dash
  :straight t)

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
    ;; I'll type my own parenthesis thank you very much.
    ;; (electric-pair-mode)
    (flymake-mode 1)
    (setq truncate-lines t)
    (which-function-mode)))

(use-package copilot
  ;; I want to explore this a bit, but by default want it "off" and to
  ;; be as unobtrusive.
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :bind (:map copilot-mode-map
          (("C-c 0 <return>" . copilot-accept-completion)
            ("C-c 0 <down>" .  copilot-next-completion)
            ("C-c 0 <up>" . copilot-previous-completion)
            ("C-c 0 DEL" . copilot-clear-overlay)
            ("C-c 0 TAB" . copilot-panel-complete)
            ("C-c 0 ESC" . copilot-mode)))
  :bind ("C-c 0 ESC" . copilot-mode)
  :custom
  ;; Copilot...never give me code comment recommendations.
  (copilot-disable-predicates '(er--point-is-in-comment-p))
  (copilot-idle-delay 1.5)
  :ensure t)

(defvar jf/comment-header-regexp/major-modes-alist
  '((emacs-lisp-mode . "^;;;+$")
     (ruby-mode . "^[[:space:]]*##+$")
     (ruby-ts-mode . "^[[:space:]]*##+$"))
  "AList of major modes and their comment headers.")

(defun jf/commend-header-forward ()
  "Move to previous line that starts a comment block.

See `jf/comment-header-regexp/major-modes-alis'."
  (interactive)
  (let ((regexp
          (alist-get major-mode
            jf/comment-header-regexp/major-modes-alist)))
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
  "Move to previous line that starts a comment block.
 See `jf/comment-header-regexp/major-modes-alis'."
  (interactive)
  (let ((regexp
          (alist-get major-mode
            jf/comment-header-regexp/major-modes-alist)))
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

(dolist (el jf/comment-header-regexp/major-modes-alist)
  (let ((jf-map (intern (format "%s-map" (car el)))))
    ;; The treesitter mode maps don't seem to exist at this point
    (unless (s-contains? "-ts-" (format "%s" (car el)))
      (progn
        (define-key (symbol-value jf-map)
          (kbd "s-ESC") #'jf/comment-header-backward)
        (define-key (symbol-value jf-map)
          (kbd "C-s-]") #'jf/commend-header-forward)))))

(defun jf/ruby-ts-mode-configurator ()
  "Configure the `treesit' provided `ruby-ts-mode'."
  ;; I encountered some loading issues where ruby-ts-mode was not
  ;; available during my understanding of the use-package life-cycle.
  (cond ((string-match "_spec.rb$" buffer-file-name)
          (rspec-mode 1)))
  (setq-local add-log-current-defun-function
    #'jf/treesit/yank-qualified-method-fname)
  (define-key ruby-ts-mode-map (kbd "C-M-h")
    #'jf/treesit/function-select)
  (define-key ruby-ts-mode-map (kbd "M-.")
    #'xref-find-definitions)
  (define-key ruby-ts-mode-map (kbd "s-.")
    #'rspec-toggle-spec-and-target)
  (define-key ruby-ts-mode-map
    (kbd "C-c y f") #'jf/yank-current-scoped-function-name)
  (define-key ruby-ts-mode-map
    (kbd "C-c y y") #'jf/ruby-mode/yank-yardoc)
  (define-key ruby-ts-mode-map
    (kbd "s-ESC") #'jf/comment-header-backward)
  (define-key ruby-ts-mode-map
    (kbd "C-s-]") #'jf/commend-header-forward)
  (define-key ruby-ts-mode-map
    (kbd "C-c w r") #'jf/treesit/wrap-rubocop)
  (define-key ruby-ts-mode-map
    (kbd "M-{") #'ruby-beginning-of-block)
  (define-key ruby-ts-mode-map
    (kbd "M-}") #'ruby-end-of-block))
(add-hook 'ruby-ts-mode-hook #'jf/ruby-ts-mode-configurator)

;; From
;; https://emacs.dyerdwelling.family/emacs/20230414111409-emacs--indexing-emacs-init/
;;
;; Creating some outline modes.  Which has me thinking about an outline
;; mode for my agenda file.
(defun jf/emacs-lisp-mode-configurator ()
  (setq imenu-sort-function 'imenu--sort-by-name)
  (setq imenu-generic-expression
    '((nil "^;;[[:space:]]+-> \\(.*\\)$" 1)
       ("Variables"
         "^([[:space:]]*\\(cl-\\)?defvar[[:space:]]+\\([^ ]*\\)$" 2)
       ("Variables"
         "^([[:space:]]*\\(cl-\\)?defconst[[:space:]]+\\([^ ]*\\)$" 2)
       ("Variables"
         "^([[:space:]]*\\(cl-\\)?defcustom[[:space:]]+\\([^ ]*\\)$" 2)
       ("Functions"
         "^([[:space:]]*\\(cl-\\)?defun[[:space:]]+\\([^(]+\\)" 2)
       ("Macros"
         "^([[:space:]]*\\(cl-\\)?defmacro[[:space:]]+\\([^(]+\\)" 2)
       ("Types"
         "^([[:space:]]*\\(cl-\\)?defstruct[[:space:]]+\\([^(]+\\)" 2)
       ("Packages"
         "^.*([[:space:]]*use-package[[:space:]]+\\([[:word:]-]+\\)" 1)))
  (imenu-add-menubar-index))
(add-hook 'emacs-lisp-mode-hook #'jf/emacs-lisp-mode-configurator)

(use-package annotate
  :straight t
  :config (setq annotate-annotation-position-policy :margin)
  :hook (prog-mode . annotate-mode))

(setq annotate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-s-a a") #'annotate-annotate)
    (define-key map (kbd "C-s-a k") #'annotate-delete-annotation)
    (define-key map (kbd "C-s-a c") #'annotate-change-annotation-colors)
    (define-key map (kbd "C-s-a s") #'annotate-show-annotation-summary)
    (define-key map (kbd "C-s-a ]") #'annotate-goto-next-annotation)
    (define-key map (kbd "C-s-a [") #'annotate-goto-previous-annotation)
    map))

(load "jf-rubocop-cops.el")

;; (require 'jf-copilot)

(provide 'jf-coding)
;;; jf-coding.el ends here
