;;; init.el --- Summary:
;;; -*- lexical-binding: t; -*-
;;
;;  Emacs configuration for Jeremy Friesen
;;
;;; Commentary:
;;
;;  This is my journey into Emacs.  Let's see where we go!
;;
;;; CODE:

;; I have additional files that I require in the emacs directory
(add-to-list 'load-path (expand-file-name "~/git/jnf-emacs-config/emacs"))

;; https://www.reddit.com/r/emacs/comments/mtb05k/emacs_init_time_decreased_65_after_i_realized_the/
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; This preamble is part of straight-use-package My understanding, in
;; reading straight documentation is that it has better load
;; times. However, the configuration options I often see leverage
;; "use-package" which is why most of my package declarations look as
;; they do.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; I saw that straight loaded use-package to take advantage of the
;; use-package syntax which is often how things are documented.
(straight-use-package 'use-package)

;; GCMH does GC when the user is idle.
(use-package gcmh
  :straight t
  :diminish 'gcmh-mode
  :init
  (setq gcmh-idle-delay 5
	gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
	:config (gcmh-mode))

;;Slow down the UI being updated to improve performance
(setq idle-update-delay 1.1)
(use-package diminish
  :straight t)

(require 'jnf-config.el)
(require 'jnf-display.el)
(require 'jnf-hydra.el)

(use-package prescient
  :straight t)

;; https://docs.projectile.mx/en/latest/
;;
;; Helpful for understanding the likely bounds of directory structure
(use-package projectile
  :straight t
  :diminish 'projectile-mode
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/git/"))
  :bind (("s-t" . projectile-find-file)
         ("s-." . projectile-toggle-between-implementation-and-test)))

(require 'jnf-selectrum.el)


;; I have found this package quite "helpful"; When I want to know the
;; name of a function or key or variable, I can use the helpful
;; package.
(use-package helpful
  :straight t
  :after all-the-icons
  :pretty-hydra
  ((:title (with-material "help_outline" "Helpful Menus") :quit-key "q" :exit t)
   ("Helpful"
    (("f" helpful-callable "callable")
     ("c" helpful-command "command")
     ("u" helpful-function "function")
     ("k" helpful-key "key")
     ("d" helpful-at-point "thing at point")
     ("v" helpful-variable "variable")
     ("b" embark-bindings "bindings"))))
  :bind ("C-s-h" . helpful-hydra/body))

;; A window manager for emacs, allowing fast toggles between windows
;; as well as opening or moving those windows.
;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :straight t
  :bind (("M-o" . ace-window)))

(global-auto-revert-mode)
;; https://www.emacswiki.org/emacs/RecentFiles#h5o-1
;; Save recentf list every five minutes
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(run-at-time nil (* 5 60) 'recentf-save-list)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#fdf0ed" "#e95678" "#29d398" "#fadad1" "#26bbd9" "#ee64ac" "#26bbd9" "#403c3d"])
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(column-number-mode t)
 '(custom-safe-themes
   '("7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "8607fdf417935af22922d10b4664a4ead5a64c01b55ac9e4eb9f4da9d177f612" "250268d5c0b4877cc2b7c439687f8145a2c85a48981f7070a72c7f47a2d2dc13" "23ba4b4ba4d1c989784475fed58919225db8d9a9751b32aa8df835134fe7ba6f" default))
 '(delete-selection-mode t)
 '(dired-listing-switches "-laGhpX")
 '(dired-use-ls-dired t)
 '(flycheck-checkers
   '(ada-gnat asciidoctor asciidoc awk-gawk bazel-buildifier c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint css-stylelint cuda-nvcc cwl d-dmd dockerfile-hadolint elixir-credo emacs-lisp emacs-lisp-checkdoc ember-template erlang-rebar3 erlang eruby-erubis eruby-ruumba fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert go-staticcheck groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jshint javascript-standard json-jsonlint json-python-json json-jq jsonnet less less-stylelint llvm-llc lua-luacheck lua markdown-markdownlint-cli markdown-mdl nix nix-linter opam perl perl-perlcritic php php-phpmd php-phpcs processing proselint protobuf-protoc protobuf-prototool pug puppet-parser puppet-lint python-flake8 python-pylint python-pycompile python-mypy r-lintr racket rpm-rpmlint rst-sphinx rst ruby-standard ruby-rubylint ruby ruby-jruby rust-cargo rust rust-clippy scala scala-scalastyle scheme-chicken scss-lint scss-stylelint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tcl-nagelfar terraform terraform-tflint tex-chktex tex-lacheck texinfo textlint typescript-tslint verilog-verilator vhdl-ghdl xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby yaml-yamllint))
 '(global-display-line-numbers-mode t)
 '(org-agenda-files
   '("/Users/jfriesen/git/org/agenda.org" "/Users/jfriesen/git/org/elfeed.org" "/Users/jfriesen/git/org/index.org" "/Users/jfriesen/git/org/readings.org" "/Users/jfriesen/git/org/sessions.org" "/Users/jfriesen/git/org/troubleshooting.org"))
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(safe-local-variable-values '((encoding . utf-8)))
 '(show-paren-mode t)
 '(typopunct-buffer-language 'english)
 '(use-package-always-ensure t))

(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(require 'jnf-epub.el)
(require 'jnf-modes.el)

;; That letter is the beginning of a word. Narrow results from there.
(use-package avy
  :straight t)

;; Add a gopher and gemini client
(use-package elpher
  :straight t)
(use-package keychain-environment
  :straight t
  :config
  (keychain-refresh-environment))


;; I never want these.
(unbind-key "C-x C-d") ;; list-directory
(global-set-key (kbd "C-x C-d") 'dired)
;; This addition echoes a previous mapping of neotree to the F8 key.
(global-set-key (kbd "<f8>") 'dired)
(unbind-key "C-z") ;; suspend-frame


(global-set-key (kbd "C-s-w") 'browse-url-at-point) ;; CTRL+CMD+w
(setq browse-url-browser-function 'eww-browse-url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN frame and window quick setup
(defun gk-layouts-3col ()
  "Three column layout.

Tries to preserve the order of window buffers and active window."
  (interactive)
  ;; Record active window buffer.
  (let ((cbuf (current-buffer)))
    ;; Switch to leftmost window.
    (ignore-errors (cl-loop do (windmove-left)))
    (let ((buffers
           (mapcar #'window-buffer (-take 3 (window-list))))
          (width (/ (frame-width) 3)))
      (delete-other-windows)
      (split-window-horizontally width)
      (other-window 1)
      (split-window-horizontally)
      (other-window -1)
      (dolist (b buffers)
        (switch-to-buffer b)
        (other-window 1)))
    ;; Switch to previously visible buffer’s window.
    (select-window (get-buffer-window cbuf))))


(defun gk-layouts-main-and-sidekicks ()
  "One horizontal split, the right window split in two.

Tries to preserve the order of window buffers and active window."
  (interactive)
  ;; Record active window buffer.
  (let ((cbuf (current-buffer)))
    ;; Switch to leftmost window.
    (ignore-errors (cl-loop do (windmove-left)))
    (let ((buffers
           (mapcar #'window-buffer (-take 3 (window-list)))))
      (delete-other-windows)
      (split-window-horizontally)
      (other-window 1)
      (split-window-vertically)
      (other-window -1)
      (dolist (b buffers)
        (switch-to-buffer b)
        (other-window 1)))
    ;; Switch to previously visible buffer’s window.
    (select-window (get-buffer-window cbuf))))

(bind-key "C-x \\" #'gk-layouts-main-and-sidekicks)


;; END frame and window quick setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (eq system-type 'darwin)
    (require 'darwin-emacs-config.el))

(if (eq system-type 'gnu/linux)
    (require 'gnu-linux-emacs-config.el))

;; This is a common function that I've used in other text editors.
;; It's a simple stitch together of sort-lines and
;; delete-duplicate-lines.
(defun sort-unique-lines (reverse beg end &optional adjacent keep-blanks interactive)
  "Sort lines and delete duplicates.
By default the sort is lexigraphically ascending.  To sort as
descending set REVERSE to non-nil.  Specify BEG and END for the
bounds of sorting.  By default, this is the selected region.

I've included ADJACENT, KEEP-BLANKS, and INTERACTIVE so I can
echo the method signature of `'delete-duplicate-lines`"
  (interactive "P\nr")
  (sort-lines reverse beg end)
  (delete-duplicate-lines beg end reverse adjacent keep-blanks interactive))

(require 'jnf-company.el)
;; (require 'jnf-corfu.el)
(require 'jnf-in-buffer.el)
(require 'jnf-org.el)
(require 'jnf-basic-config.el)
(require 'jnf-git.el)
(require 'jnf-spelling.el)
(require 'jnf-typopunct.el)
(require 'jnf-ruby.el)
(require 'jnf-lsp-mode.el)
(require 'jnf-beancount.el)
(require 'jnf-blogging.el)
(require 'jnf-tabs.el)
(require 'jnf-stars-without-number.el)
(require 'jnf-elfeed.el)
;; (require 'jnf-fennel.el)
(require 'jnf-yaml.el)
(require 'jnf-dired.el)

(diminish 'eldoc-mode)

;; Consider for publishing: https://github.com/rnkn/binder
;; Consider as replacement for org-roam: https://github.com/EFLS/zetteldeft
;; And https://github.com/zzamboni/ox-leanpub

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Begin burly/bufly
;;

;; (use-package burly :straight (burly :type git :host github :repo "alphapapa/burly.el"))
;; (use-package bufly :straight (bufly :type git :host github :repo "alphapapa/burly.el"))

;; (use-package bufler
;;   :straight (bufler :type git :host github :repo "alphapapa/bufler.el"
;;                     :files (:defaults (:exclude "helm-bufler.el"))))

;; End burly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
