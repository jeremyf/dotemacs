;;; setup-coding --- Setup my programming language suppor -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;;; Code:

;;;; Language Server Integration (LSP)

(use-package dape
  :straight t)

;; I've been exploring either `lsp-mode' or `eglot' and thusfar prefer
;; the lightweight nature of `eglot'.
(use-package eglot
  :straight (:type built-in)
  :bind
  ("H-e h" . flymake-show-buffer-diagnostics)
  ("H-e n" . flymake-goto-next-error)
  ("H-e p" . flymake-goto-prev-error)
  ("H-e r" . eglot)
  (:map eglot-mode-map
    ("H-e s" . consult-eglot-symbols)
    ("H-e r" . eglot-reconnect)
    ;; Type M-/ then M-? to jump to definition and then find its usage
    ("M-/" . jf/treesit/jump-to-declaration-identifier)
    ;; ("H-e s" . jf/treesit/func-signature/dwim)
    ("H-e m" . eglot-rename)
    ("H-e a" . eglot-code-actions)
    ("H-e i" . eglot-find-implementation))
  ;; :straight (:type built-in) The Language Server Protocol (LSP)
  ;; is a game changer; having access to that tooling is very much a
  ;; nice to have.
  :hook ((
           yaml-mode yaml-ts-mode
           angular-mode angular-ts-mode ;; npm install -g @angular/language-service@next typescript @angular/language-server
           css-mode css-ts-mode
           elixir-ts-mode
           go-mode go-ts-mode ;; https://github.com/golang/tools/tree/master/gopls
           html-mode html-ts-mode
           js-mode js-ts-mode
           json-mode json-ts-mode ;; npm i -g vscode-langservers-extracted
           python-mode ;; python-ts-mode ;; brew install python-lsp-server
           ruby-mode ruby-ts-mode
           scss-mode scss-ts-mode
           typescript-ts-mode typescript-mode ;; https://github.com/typescript-language-server/typescript-language-server
           )
          . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
    `((python-ts-mode python-mode) . ("pyrefly" "lsp")))
  (defun jf/treesit/jump-to-declaration-identifier ()
    "Within a defun context, jump to the identifier.

Useful for Eglot."
    (interactive)
    (if-let* ((dfn
                (treesit-defun-at-point))
               (dfn-identifier
                 (car (treesit-filter-child
                        dfn
                        (lambda (node)
                          (string= "identifier"
                            (treesit-node-type node)))))))
      (goto-char (treesit-node-start dfn-identifier))))
  ;; Cribbed from:
  ;; https://codeberg.org/mekeor/init/src/commit/11e3d86aa18090a5e3a6f0d29373c24373f29aaf/init.el#L666-L811
  ;;
  ;; References:
  ;; https://github.com/golang/tools/blob/master/gopls/doc/settings.md
  (setf (plist-get eglot-workspace-configuration :gopls)
    '(;; :allExperiments ;; bool false

       ;; BUILD

       ;; :buildFlags                 ;; []string                 []
       ;; :env                        ;; map[string]string        {}
       ;; :directoryFilters           ;; []string                 ["-**/node_modules"]
       ;; :templateExtensions         ;; []string                 []
       ;; :memoryMode                 ;; "DegradeClosed"|"Normal" "Normal"
       ;; :expandWorkspaceToModule    ;; bool                     true
       ;; :allowModfileModifications  ;; bool                     false
       ;; :allowImplicitNetworkAccess ;; bool                     false
       ;; :standaloneTags             ;; []string                 ["ignore"]

       ;; FORMATTING

       ;; :local   ;; string ""
       :gofumpt t ;; bool   false

       ;; UI

       ;; :codelenses (
       ;; :gc_details         ;; bool false
       ;; :generate           ;; bool true
       ;; :regenerate_cgo     ;; bool true
       ;; :run_govulncheck    ;; bool undocumented
       ;; :test               ;; bool undocumented
       ;; :tidy               ;; bool true
       ;; :upgrade_dependency ;; bool true
       ;; :vendor             ;; bool true
       ;; )

       ;; :semanticTokens   ;; bool false
       ;; :noSemanticString ;; bool false
       ;; :noSemanticNumber ;; bool false

       ;; COMPLETION

       ;; :usePlaceholders                ;; bool                                      false
       ;; :completionBudget               ;; time.Duration                             "100ms"
       ;; :matcher                        ;; "CaseInsensitive"|"CaseSensitive"|"Fuzzy" "Fuzzy"
       ;; :experimentalPostfixCompletions ;; bool                                      true
       ;; :completeFunctionCalls          ;; bool                                      true

       ;; DIAGNOSTIC

       :analyses
       (
         ;; :appends              ;; bool true
         ;; :asmdecl              ;; bool true
         ;; :assign               ;; bool true
         ;; :atomic               ;; bool true
         ;; :atomicalign          ;; bool true
         ;; :bools                ;; bool true
         ;; :buildtag             ;; bool true
         ;; :cgocall              ;; bool true
         ;; :composites           ;; bool true
         ;; :copylocks            ;; bool true
         ;; :deepequalerrors      ;; bool true
         ;; :defers               ;; bool true
         ;; :deprecated           ;; bool true
         ;; :directive            ;; bool true
         ;; :embed                ;; bool true
         ;; :errorsas             ;; bool true
         ;; :fieldalignment       ;; bool false
         ;; :fillreturns          ;; bool true
         ;; :fillstruct           ;; bool true
         ;; :httpresponse         ;; bool true
         ;; :ifaceassert          ;; bool true
         ;; :infertypeargs        ;; bool true
         ;; :loopclosure          ;; bool true
         ;; :lostcancel           ;; bool true
         ;; :nilfunc              ;; bool true
         ;; :nilness              ;; bool true
         ;; :nonewvars            ;; bool true
         ;; :noresultvalues       ;; bool true
         ;; :printf               ;; bool true
         :shadow t                ;; bool false
         ;; :shift                ;; bool true
         ;; :simplifycompositelit ;; bool true
         ;; :simplifyrange        ;; bool true
         ;; :simplifyslice        ;; bool true
         ;; :slog                 ;; bool true
         ;; :sortslice            ;; bool true
         ;; :stdmethods           ;; bool true
         ;; :stringintconv        ;; bool true
         ;; :structtag            ;; bool true
         ;; :stubmethods          ;; bool true
         ;; :testinggoroutine     ;; bool true
         ;; :tests                ;; bool true
         ;; :timeformat           ;; bool true
         ;; :undeclaredname       ;; bool true
         ;; :unmarshal            ;; bool true
         ;; :unreachable          ;; bool true
         ;; :unsafeptr            ;; bool true
         :unusedparams t          ;; bool false
         ;; :unusedresult         ;; bool true
         :unusedvariable t        ;; bool false
         :unusedwrite t           ;; bool false
         :useany t                ;; bool false
         )

       :staticcheck t ;; bool            false

       ;; :annotations (
       ;; :bounds ;; bool true
       ;; :escape ;; bool true
       ;; :inline ;; bool true
       ;; :nil    ;; bool true
       ;; )

       ;; :vulncheck                 ;; "Imports"|"Off" "Off"
       :diagnosticsDelay "250ms"     ;; time.Duration   "1s"
       ;; :diagnosticsTrigger        ;; "Edit"|"Save"   "Edit"
       ;; :analysisProgressReporting ;; bool            true

       ;; DOCUMENTATION

       ;; :hoverKind    ;; "FullDocumentation"|"NoDocumentation"|"SingleLine"|"Structured"|"SynopsisDocumentation" "FullDocumentation"
       ;; :linkTarget   ;; "godoc.org"|"pkg.go.dev"|string                                                         "pkg.go.dev"
       :linksInHover :json-false ;; bool                                                                           true

       ;; INLAY HINTS

       :hints
       (
         :assignVariableTypes    t           ;; bool false
         :compositeLiteralFields t           ;; bool false
         :compositeLiteralTypes  t           ;; bool false
         :constantValues         t           ;; bool false
         :functionTypeParameters t           ;; bool false
         :parameterNames         :json-false ;; bool false
         :rangeVariableTypes     t           ;; bool false
         )

       ;; NAVIGATION

       ;; :importShortcut ;; "Both"|"Definition"|"Link"                            "Both"
       ;; :symbolMatcher  ;; "CaseInsensitive"|"CaseSensitive"|"FastFuzzy"|"Fuzzy" "FastFuzzy"
       ;; :symbolStyle    ;; "Dynamic"|"Full"|"Package"                            "Dynamic"
       ;; :symbolScope    ;; "all"|"workspace"                                     "all"
       ;; :verboseOutput  ;; bool                                                  false
       ;; :newDiff        ;; "both"|"old"|"new"                                    "both"
       ))
  (defun jf/eglot-managed-mode ()
    "Ensure `eglot-completion-at-point' preceeds everything."
    ;; I don't want `eglot-completion-at-point' to trample my other
    ;; completion options.
    ;;
    ;; https://stackoverflow.com/questions/72601990/how-to-show-suggestions-for-yasnippets-when-using-eglot
    (setq-local completion-at-point-functions
      (list (cape-capf-super
              #'eglot-completion-at-point
              #'tempel-expand)))
    ;; I could configure `eglot-stay-out-of' to skip fly-make; or I
    ;; could simply check here if I'm in `go-ts-mode' and if so
    ;; enable the linting `flymake-go-staticcheck-enable'
    (when (and (derived-mode-p 'go-ts-mode)
            (fboundp 'flymake-go-staticcheck-enable))
      (progn
        (flymake-go-staticcheck-enable)
        (flymake-start))))
  ;; https://github.com/elixir-lsp/elixir-ls?tab=readme-ov-file

  ;; https://github.com/emacs-lsp/lsp-mode/wiki/Install-Angular-Language-server
  ;; with modifications for homebrew
  (add-to-list 'eglot-server-programs
    '(angular-mode
       "node /opt/homebrew/lib/node_modules/@angular/language-server --ngProbeLocations /opt/homebrew/lib/node_modules --tsProbeLocations /opt/homebrew/lib/node_modules --stdio"))
  (add-to-list 'eglot-server-programs
    '(angular-ts-mode
       "node /opt/homebrew/lib/node_modules/@angular/language-server --ngProbeLocations /opt/homebrew/lib/node_modules --tsProbeLocations /opt/homebrew/lib/node_modules --stdio"))
  :hook ((eglot-managed-mode . jf/eglot-managed-mode)))

(add-hook 'go-ts-mode-hook 'flymake-mode 8)
;; (add-hook 'go-ts-mode-hook 'flymake-show-buffer-diagnostics 9)
;; (add-hook 'go-ts-mode-hook 'eglot-ensure 10)
(add-hook 'elixir-ts-mode-hook 'eglot-ensure 10)

(use-package editorconfig
  ;; “EditorConfig helps maintain consistent coding styles for multiple
  ;; developers working on the same project across various editors and
  ;; IDEs.”  See https://editorconfig.org/#overview for more details.
  :straight t
  :config
  (editorconfig-mode 1))

(use-package heex-ts-mode
  :straight t)

(use-package mix
  :straight t
  :config
  (keymap-set mix-minor-mode-map "C-c e"
    #'mix-minor-mode-command-map)
  (keymap-set mix-minor-mode-map "C-c d"
    #'jf/duplicate-current-line-or-lines-of-region)
  (add-hook 'elixir-ts-mode-hook 'mix-minor-mode))

(use-package elixir-ts-mode
  :after heex-ts-mode
  :straight t)

(use-package flymake-elixir
  :straight t
  :config
  (add-hook 'elixir-ts-mode-hook 'flymake-elixir-load))

(use-package flymake-go-staticcheck
  ;; Adding a linter to go files, leveraging the staticcheck binary.
  ;; See: https://github.com/dominikh/go-tools/tree/master/cmd/staticcheck
  :straight t)

(use-package go-mode
  :straight t)

(use-package go-ts-mode
  :straight (:type built-in)
  :hook (go-ts-mode . jf/go-ts-mode-configurator)
  :bind (:map go-ts-mode-map
          (("H-e ." . 'jf/go/toggle-test-impl)
            ("H-e c" . 'jf/go/show-coverage)))
  :config
  (defun jf/go/show-coverage (&optional coverage-file)
    (interactive)
    (let* ((file (or coverage-file "test.coverage")))
      (if (f-file-p file)
        (let ((pos (point)))
          (go-coverage file)
          (when (< pos (max-char))
            (goto-char pos)))
        (go-test-current-project))))

  (defun jf/go/toggle-test-impl ()
    "As `projectile-toggle-between-implementation-and-test'."
    (interactive)
    (if (s-ends-with? "_test.go" (buffer-file-name))
      (projectile-toggle-between-implementation-and-test)
      (find-file (format "%s_test.go" (file-name-base (buffer-file-name))))))
  (defun go-format ()
    "Formats the current buffer according to the goimports tool."
    (interactive)
    (let ((tmpfile (make-temp-file "gofmt" nil ".go"))
           (patchbuf (get-buffer-create "*Gofmt patch*"))
           (errbuf (get-buffer-create "*Gofmt Errors*"))
           (coding-system-for-read 'utf-8)
           (coding-system-for-write 'utf-8))

      (with-current-buffer errbuf
        (setq buffer-read-only nil)
        (erase-buffer))
      (with-current-buffer patchbuf
        (erase-buffer))

      (write-region nil nil tmpfile)

      ;; We're using errbuf for the mixed stdout and stderr output. This
      ;; is not an issue because gofmt -w does not produce any stdout
      ;; output in case of success.
      (if (zerop (call-process "goimports" nil errbuf nil "-w" tmpfile))
        (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
          (progn
            (kill-buffer errbuf)
            (message "Buffer is already gofmted"))
          (go--apply-rcs-patch patchbuf)
          (kill-buffer errbuf)
          (message "Applied gofmt"))
        (message "Could not apply gofmt. Check errors for details")
        (gofmt--process-errors (buffer-file-name) tmpfile errbuf))

      (kill-buffer patchbuf)
      (delete-file tmpfile)))
  (setq go-ts-mode-indent-offset 2)
  ;; Copied from
  ;; https://github.com/Homebrew/brew/blob/c2ed3327c605c3e738359c9807b8f4cd6fec09eb/Cellar/emacs-plus%4029/29.3/share/emacs/29.3/lisp/progmodes/go-ts-mode.el#L115-L206
  ;;
  ;; Modifications made to remove parse error.  Someday I'll get rid of
  ;; this.
  (setq go-ts-mode--font-lock-settings
    (treesit-font-lock-rules
      :language 'go
      :feature 'bracket
      '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

      :language 'go
      :feature 'comment
      '((comment) @font-lock-comment-face)

      :language 'go
      :feature 'constant
      `([(false) (nil) (true)] @font-lock-constant-face
         ,@(when (go-ts-mode--iota-query-supported-p)
             '((iota) @font-lock-constant-face))
         (const_declaration
           (const_spec name: (identifier) @font-lock-constant-face)))

      :language 'go
      :feature 'delimiter
      '((["," "." ";" ":"]) @font-lock-delimiter-face)

      :language 'go
      :feature 'definition
      '((function_declaration
          name: (identifier) @font-lock-function-name-face)
         (method_declaration
           name: (field_identifier) @font-lock-function-name-face)
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;; method_spec is, as of 2024-05-01 and Emacs v29.3, something
         ;;; that breaks with the installed tree-sitter language.
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;; (method_spec
         ;;  name: (field_identifier) @font-lock-function-name-face)
         (field_declaration
           name: (field_identifier) @font-lock-property-name-face)
         (parameter_declaration
           name: (identifier) @font-lock-variable-name-face)
         (short_var_declaration
           left: (expression_list
                   (identifier) @font-lock-variable-name-face
                   ("," (identifier) @font-lock-variable-name-face)*))
         (var_spec name: (identifier) @font-lock-variable-name-face
           ("," name: (identifier) @font-lock-variable-name-face)*))

      :language 'go
      :feature 'function
      '((call_expression
          function: (identifier) @font-lock-function-call-face)
         (call_expression
           function: (selector_expression
                       field: (field_identifier)
                       @font-lock-function-call-face)))

      :language 'go
      :feature 'keyword
      `([,@go-ts-mode--keywords] @font-lock-keyword-face)

      :language 'go
      :feature 'label
      '((label_name) @font-lock-constant-face)

      :language 'go
      :feature 'number
      '([(float_literal)
          (imaginary_literal)
          (int_literal)] @font-lock-number-face)

      :language 'go
      :feature 'string
      '([(interpreted_string_literal)
          (raw_string_literal)
          (rune_literal)] @font-lock-string-face)

      :language 'go
      :feature 'type
      '([(package_identifier) (type_identifier)] @font-lock-type-face)

      :language 'go
      :feature 'property
      '((selector_expression field: (field_identifier)
          @font-lock-property-use-face)
         (keyed_element (_ (identifier) @font-lock-property-use-face)))

      :language 'go
      :feature 'variable
      '((identifier) @font-lock-variable-use-face)

      :language 'go
      :feature 'escape-sequence
      :override t
      '((escape_sequence) @font-lock-escape-face)

      :language 'go
      :feature 'error
      :override t
      '((ERROR) @font-lock-warning-face))))

(use-package ob-go
  :straight t)

(use-package go-playground
  :straight t)

(use-package go-tag
  ;; go install github.com/fatih/gomodifytags@latest
  :straight t
  :after go-ts-mode
  :bind (:map go-ts-mode-map
          (("H-g d" . #'go-browse-doc)
            ("H-g t" . #'go-tag-add)
            ("H-g H-g f" . #'go-goto-function)
            ("H-g H-g d" . #'go-goto-docstring)
            ("H-g H-g a" . #'go-goto-arguments)
            ("H-g T" . #'go-tag-remove))))

(use-package go-fill-struct
  ;; go install github.com/davidrjenni/reftools/cmd/fillstruct@latest
  :straight t
  :after go-ts-mode
  :bind (:map go-ts-mode-map
          (("H-g f" . #'go-fill-struct))))

(use-package gotest
  :straight t
  :after go-ts-mode
  :config
  (setq-default go-run-go-command "LOGGING_LEVEL=22 go")
  (setq-default go-test-go-command "LOGGING_LEVEL=22 go")
  (setq-default go-test-args (concat "-count 1 -v  --failfast -coverprofile=test.coverage")))

(defun flymake-go-vet (orig-fun)
  "A flymake backend to run `go vet` on a Go source file."
  (when (eq major-mode 'go-mode)
    (let* ((filename (buffer-file-name))
           (dir (file-name-directory filename))
           (cmd (executable-find "go")))
      (when (and cmd filename dir)
        ;; The command to run go vet
        (list cmd (list "vet" "-json=true" filename))))))

;; Add the custom backend function to the hook
(add-hook 'go-ts-mode-hook
          (lambda ()
            (add-to-list 'flymake-diagnostic-functions
                         #'flymake-go-vet)))

;; Optional: install eglot-format-buffer as a save hook.
;; The depth of -10 places this before eglot's willSave notification,
;; so that that notification reports the actual contents that will be saved.
;; (defun eglot-format-buffer-on-save ())
;; (add-hook 'go-ts-mode-hook #'eglot-format-buffer-on-save)

;; See https://elixir-lsp.github.io/elixir-ls/getting-started/emacs/

(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :hook ((eglot-managed-mode . eglot-booster-mode))
  :config
  (advice-add 'eglot-completion-at-point
    :around #'cape-wrap-buster))

(use-package eglot-hierarchy
  :straight (:host github :repo "dolmens/eglot-hierarchy")
  :bind (:map eglot-mode-map ("H-e ." . eglot-hierarchy-call-hierarchy))
  :after eglot)

(use-package consult-eglot
  :after (consult eglot)
  :straight t)

(use-package consult-eglot-embark
  :straight t
  :config
  (with-eval-after-load 'embark
    (with-eval-after-load 'consult-eglot
      (require 'consult-eglot-embark)
      (consult-eglot-embark-mode))))

;;;; Programming Modes

(use-package prog-mode
  :straight (:type built-in)
  :config
  (add-hook 'prog-mode-hook #'jf/prog-mode-configurator)
  ;; I didn't know about `add-log-current-defun-function' until a blog
  ;; reader reached out.  Now, I'm making a general function for
  ;; different modes.
  (defun jf/yank-current-scoped-function-name ()
    "Echo and kill the current scoped function name.

See `add-log-current-defun-function'."
    (interactive)
    (if-let* ((text
                (funcall add-log-current-defun-function)))
      (progn
        (message "%s" text)
        (kill-new (substring-no-properties text)))
      (user-error "Warning: Point not on function")))
  (defun jf/yank-current-scoped-function-as-org-mode-link ()
    "Yank the current function and region as an `org-mode' link."
    (interactive)
    (if-let* ((text
                (funcall add-log-current-defun-function)))
      (let ((link
              (format "[[%s][%s]]"
                (call-interactively #'git-link)
                text)))
        (message link)
        (kill-new (substring-no-properties link)))
      (user-error "Warning: Point not on function")))
  (bind-key "M-<down>"
    #'end-of-defun prog-mode-map)
  (bind-key "M-<up>"
    #'beginning-of-defun prog-mode-map)
  (bind-key "C-c y f"
    #'jf/yank-current-scoped-function-name prog-mode-map)
  (bind-key "C-c y o"
    #'jf/yank-current-scoped-function-as-org-mode-link prog-mode-map)
  (bind-key "C-c y f"
    #'jf/yank-current-scoped-function-name emacs-lisp-mode-map)
  (bind-key "C-c y o"
    #'jf/yank-current-scoped-function-as-org-mode-link emacs-lisp-mode-map)

  (defvar jf/comment-header-regexp/major-modes-alist
    '((emacs-lisp-mode . "^;;;+$")
       (ruby-mode . "^[[:space:]]*##+$")
       (ruby-ts-mode . "^[[:space:]]*##+$"))
    "AList of major modes and their comment headers.")

  (defun jf/comment-header-forward ()
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
    (let ((jf-map
            (intern (format "%s-map" (car el)))))
      ;; The treesitter mode maps don't seem to exist at this point
      (unless (s-contains? "-ts-" (format "%s" (car el)))
        (progn
          (define-key (symbol-value jf-map)
            (kbd "H-[") #'jf/comment-header-backward)
          (define-key (symbol-value jf-map)
            (kbd "H-]") #'jf/comment-header-forward)))))

  (defun jf/prog-mode-configurator ()
    "Do the configuration of all the things."
    ;; I'll type my own parenthesis thank you very much.
    ;; (electric-pair-mode)

    ;; CVE-2024-53920
    ;; https://eshelyaron.com/posts/2024-11-27-emacs-aritrary-code-execution-and-how-to-avoid-it.html
    (unless (derived-mode-p 'emacs-lisp-mode)
      (flymake-mode 1))
    (hl-todo-mode t)
    (setq show-trailing-whitespace t)
    (setq truncate-lines t)
    (which-function-mode)
    ))

(use-package devdocs
  ;; Download and install documents from https://devdocs.io/ Useful for
  ;; having local inline docs.  Perhaps not always in the format that I
  ;; want, but can't have everything.
  :straight t
  :commands (devdocs-install))

(use-package json-mode
  ;; The web's data structure of choice is JSON.
  :straight t)

(use-package json-reformat
  ;; Because JSON can be quite ugly, I want something to help tidy it
  ;; up.
  :straight t
  :after json-mode
  :init (setq json-reformat:indent-width 2))

(use-package lua-mode
  ;; For working with https://www.hammerspoon.org/
  :straight t)

(use-package ruby-mode
  ;; My language of choice for professional work.
  :straight (:type built-in)
  :custom (ruby-flymake-use-rubocop-if-available nil)
  :bind
  (:map ruby-mode-map
    (("C-M-h" . jf/treesit/function-select)
      ("C-c w r" . jf/treesit/wrap-rubocop)
      ("M-{" . ruby-beginning-of-block)
      ("M-}" . ruby-end-of-block)))
  :hook ((ruby-mode ruby-ts-mode) . #'jf/ruby-mode-configurator)
  :config
  (defun jf/ruby-mode-configurator ()
    (eldoc-mode t)
    (setq-local fill-column 80))
  (defun jf/require-debugger ()
    "Determine the correct debugger based on the Gemfile."
    (let ((gemfile-lock
            (f-join (projectile-project-root) "Gemfile.lock")))
      (if-let* ((f-exists? gemfile-lock)
                 (debuggers
                   (s-split "\n"
                     (shell-command-to-string
                       (concat
                         "rg \"^ +(byebug|debugger|pry-byebug|debug) \""
                         " " gemfile-lock
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
        "require 'debug'; binding.break"))))

(use-package python
  :straight (:type built-in)
  :hook (python-mode . jf/python-mode-configurator)
  :bind (:map python-mode-map ("M-." . xref-find-definitions))
  :config
  (defun jf/python-mode-configurator ()
    (eldoc-mode t)
    (python-docstring-mode t)
    (setq-default python-indent-offset 4)
    (setq-local fill-column 80))
  (defun jf/python-ts-mode-configurator ()
    (define-key python-ts-mode-map
      (kbd "M-.") #'xref-find-definitions)
    (jf/python-mode-configurator))
  (add-hook 'python-ts-mode-hook #'jf/python-ts-mode-configurator)

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
    (setq venv-location '("/Users/jfriesen/git/pvc-2.3.0/pvc_venv"))
    (setopt projectile-switch-project-action
      '(lambda ()
         (venv-projectile-auto-workon)
         (projectile-find-file)))))

(use-package rspec-mode
  ;; I write most of my Ruby tests using rspec.  This tool helps manage
  ;; that process.
  :straight t
  ;; ensure that we’re loading ruby-mode before we do any rspec loading.
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
          (("H-e ." .
             'rspec-toggle-spec-and-target)
            ("C-c y r" .
              'jf/yank-bundle-exec-rspec-to-clipboard)))
  :bind (:map ruby-mode-map (("H-e ." . 'rspec-toggle-spec-and-target)))
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

(use-package typescript-mode
  ;; I have this for the work I once did a few years ago.  I am happiest
  ;; when I'm not working in Javascript.
  :straight t)

(use-package ruby-ts-mode
  :straight (:type built-in)
  :config
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
    (define-key ruby-ts-mode-map (kbd "H-.")
      #'rspec-toggle-spec-and-target)
    (define-key ruby-ts-mode-map
      (kbd "C-c y f") #'jf/yank-current-scoped-function-name)
    (define-key ruby-ts-mode-map
      (kbd "C-c y y") #'jf/ruby-mode/yank-yardoc)
    ;; (define-key ruby-ts-mode-map
    ;;   (kbd "s-ESC") #'jf/comment-header-backward)
    ;; (define-key ruby-ts-mode-map
    ;;   (kbd "C-s-]") #'jf/comment-header-forward)
    (define-key ruby-ts-mode-map
      (kbd "C-c w r") #'jf/treesit/wrap-rubocop)
    (define-key ruby-ts-mode-map
      (kbd "M-{") #'ruby-beginning-of-block)
    (define-key ruby-ts-mode-map
      (kbd "M-}") #'ruby-end-of-block))
  :hook (ruby-ts-mode . jf/ruby-ts-mode-configurator))

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

(use-package yaml-pro
  :hook (yaml-ts-mode . jf/yaml-mode-configurator)
  :bind (:map yaml-ts-mode-map
          ("C-c y f" . yaml-pro-copy-node-path-at-point))
  :config
  (defun jf/yaml-mode-configurator ()
    (which-function-mode -1)
    (setq-local add-log-current-defun-function
      #'jf/yaml-node-path-at-point))
  (defun jf/yaml-node-path-at-point ()
    (yaml-pro-ts--imenu-node-label (treesit-node-at (point) 'yaml)))
  :straight t)

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
        (let* ((identifiers
                 (mapcar (lambda (token)
                           (replace-regexp-in-string
                             "[^a-z|_]" ""
                             (car (s-split " "
                                    (s-trim token)))))
                   (s-split "," method_parameters_text)))
                (indentation
                  (s-repeat (current-column) " ")))
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

(use-package gotest-ts
  :straight (:host github :repo "chmouel/gotest-ts.el")
  :custom (dape-cwd-fn #'jf/dape-cwd-fn)
  :config
  (defun jf/dape-cwd-fn ()
    default-directory)
  :init
  (defun jf/dape/go-test-at-point ()
    "Run `dape' for go test at point.

See https://github.com/chmouel/gotest-ts.el"
    (interactive)
    (dape (dape--config-eval-1
            `(modes (go-ts-mode)
               ensure dape-ensure-command
               fn dape-config-autoport
               command "dlv"
               command-args ("dap" "--listen" "127.0.0.1::autoport")
               command-cwd dape-cwd-fn
               port :autoport
               :type "debug"
               :request "launch"
               :mode "test"
               :cwd dape-cwd-fn
               :program (lambda () (concat "./" (file-relative-name default-directory (funcall dape-cwd-fn))))
               :args (lambda ()
                       (when-let* ((test-name (gotest-ts-get-subtest-ts)))
                         (if test-name `["-test.run" ,test-name]
                           (error "No test selected")))))))))

(use-package scopeline
  ;; Show the scope info of methods, blocks, if/case statements.  This
  ;; is done via an overlay for "blocks" that are more than 5 (default)
  ;; lines
  :straight (:host github :repo "meain/scopeline.el")
  ;; The original `scopeline' prefix was creating line height issues for
  ;; my font of choice.  Namely adding just a bit more spacing for the
  ;; `scopeline' overlay, thus making line heights inconsistent.
  :config (setq scopeline-overlay-prefix "  ~ ")

  (add-to-list 'scopeline-targets
    '(go-mode "function_declaration"
       "func_literal"
       "method_declaration"
       "if_statement"
       "for_statement"
       "type_declaration"
       "call_expression"))
  (add-to-list 'scopeline-targets
    '(go-ts-mode "function_declaration"
       "func_literal"
       "method_declaration"
       "if_statement"
       "for_statement"
       "type_declaration"
       "call_expression"))
  :hook ((go-ts-mode go-mode) . scopeline-mode))

(use-package bundler
  ;; For Ruby package management
  :straight (bundler
              :type git
              :host github
              :repo "endofunky/bundler.el"))

(use-package docker
  ;; https://github.com/Silex/docker.el
  ;; A reality of modern development is that things happen in Docker.
  :straight t)

(use-package dockerfile-mode
  ;; Given that I interact with docker files, I should have some syntax
  ;; awareness.
  :straight t)

;;;; Supporting Functions

(defun jf/go-ts-mode-configurator ()
  ;; From go-mode
  (setq-local paragraph-start
    (concat "[[:space:]]*\\(?:"
      comment-start-skip
      "\\|\\*/?[[:space:]]*\\|\\)$"))
  (setq-local paragraph-separate paragraph-start)
  (setq-local fill-paragraph-function #'go-fill-paragraph)
  (setq-local fill-forward-paragraph-function #'go--fill-forward-paragraph)
  (setq-local adaptive-fill-function #'go--find-fill-prefix)
  (setq-local adaptive-fill-first-line-regexp "")
  (setq-local comment-line-break-function #'go--comment-indent-new-line)

  ;; (add-to-list
  ;;   'treesit-simple-imenu-settings
  ;;   `("t.Run" "\\`call_expression\\'" go-ts-mode--testing-run-node-p go-ts-mode--testing-run-name))
  (setq-local tab-width 2)
  (setq-local eglot-stay-out-of '(imenu))
  (if (s-ends-with? "_test.go" (buffer-file-name))
    (jf/minor-mode/go-ts-test-mode)
    (jf/minor-mode/go-ts-implementation-mode))
  ;; (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
  (add-hook 'before-save-hook #'go-format -15 t))


(defvar jf/minor-mode/go-ts-test-mode-map
  (let ((map
          (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'jf/go-test-current)
    map))

(defun jf/go-test-current (prefix)
  "Run current test.  When PREFIX given run using `dape`."
  (interactive "p")
  (cond
    ((>= prefix 16)
      (let ((current-prefix-arg nil))
        (go-test-current-file)))
    ((>= prefix 4)
      (let ((current-prefix-arg nil))
        (jf/dape/go-test-at-point)))
    (t
      (gotest-ts-run-dwim))))

(defvar jf/minor-mode/go-ts-implementation-mode-map
  (let ((map
          (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'go-test-current-project)
    map))

(define-minor-mode jf/minor-mode/go-ts-test-mode
  "A minor mode to augment test files in `go-ts-mode'."
  :init-value nil
  :global nil
  :keymap jf/minor-mode/go-ts-test-mode-map)

(define-minor-mode jf/minor-mode/go-ts-implementation-mode
  "A minor mode to augment implementation files in `go-ts-mode'."
  :init-value nil
  :global nil
  :keymap jf/minor-mode/go-ts-implementation-mode-map)

(provide 'setup-coding)
;;; setup-coding.el ends here
