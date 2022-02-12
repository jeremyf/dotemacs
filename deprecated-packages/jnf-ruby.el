;; -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  This package provides most of the Ruby configuration
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun jnf/ruby-mode-hook-hook ()
	     (setq fill-column 100))

;; I most often write tests using rspec.
(use-package rspec-mode
  :straight t
  :custom (rspec-use-spring-when-possible nil)
  :diminish 'rspec-mode
  :bind (:map rspec-mode-map (("s-." . 'rspec-toggle-spec-and-target)))
  :bind (:map ruby-mode-map (("s-." . 'rspec-toggle-spec-and-target)))
  :hook (ruby-mode . rspec-mode) ;; should this be `ruby-mode'
  (ruby-mode . eldoc-mode)) ;; should this be `ruby-mode'

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(use-package projectile-rails
  :after (projectile)
  :diminish 'projectile-rails-mode
  :straight t
  :config
  (defun projectile-rails-find-liquid ()
    "Find a liquid tag."
    (interactive)
    (projectile-rails-find-resource
     "liquid: "
     '(("app/liquid_tags/" "\\(.+?\\)\\.rb\\'"))
     "app/liquid_tags/${filename}.rb"))
  :bind (:map
         projectile-rails-mode-map (("C-s-." . 'projectile-rails-goto-file-at-point)
                                    ("C-c r" .  'jnf/projectile-rails--menu/body)))
  :config (projectile-rails-global-mode))

;; (use-package rails-i18n
;;   :straight t)

;; Nice and simple pakcage for string interpolation.
(use-package ruby-interpolation
  :straight t
  :diminish 'ruby-interpolation-mode
  :hook (ruby-mode . ruby-interpolation-mode)
  )

(use-package ruby-electric
  :straight t
  :diminish 'ruby-electric-mode
  ;; Somtimes I want this enabled, other times not; but it's a bit
  ;;  obnoxious in spec.rb files.
  ;;
  ;; :hook (ruby-mode . ruby-electric-mode)
  )

(use-package rails-routes
  :after projectile-rails
  :straight t)

(defvar jnf/projectile-rails--title (with-alltheicon "ruby-alt" "Rails" 1 -0.05))
(pretty-hydra-define jnf/projectile-rails-find-resource--menu
  (:foreign-keys warn :title jnf/projectile-rails--title :quit-key "q" :exit t)
  ("Rails > Find Resources"
   (("m" projectile-rails-find-model       "model")
    ("v" projectile-rails-find-view        "view")
    ("c" projectile-rails-find-controller  "controller")
    ("h" projectile-rails-find-helper      "helper")
    ("l" projectile-rails-find-lib         "lib")
    ("j" projectile-rails-find-javascript  "javascript")
    ("w" projectile-rails-find-component   "component")
    ("s" projectile-rails-find-stylesheet  "stylesheet")
    ("p" projectile-rails-find-spec        "spec")
    ("u" projectile-rails-find-fixture     "fixture")
    ("t" projectile-rails-find-test        "test")
    ("f" projectile-rails-find-feature     "feature")
    ("i" projectile-rails-find-initializer "initializer")
    ("o" projectile-rails-find-log         "log")
    ("t" projectile-rails-find-liquid      "liquid tag")
    ("@" projectile-rails-find-mailer      "mailer")
    ("!" projectile-rails-find-validator   "validator")
    ("y" projectile-rails-find-layout      "layout")
    ("n" projectile-rails-find-migration   "migration")
    ("k" projectile-rails-find-rake-task   "rake task")
    ("b" projectile-rails-find-job         "job")
    ("z" projectile-rails-find-serializer  "serializer"))))

(pretty-hydra-define jnf/projectile-rails-find-current-resource--menu
  (:foreign-keys warn :title jnf/projectile-rails--title :quit-key "q" :exit t)
  ("Rails > Find Current Resources"
   (("M" projectile-rails-find-current-model      "current model")
    ("V" projectile-rails-find-current-view       "current view")
    ("C" projectile-rails-find-current-controller "current controller")
    ("H" projectile-rails-find-current-helper     "current helper")
    ("J" projectile-rails-find-current-javascript "current javascript")
    ("S" projectile-rails-find-current-stylesheet "current stylesheet")
    ("P" projectile-rails-find-current-spec       "current spec")
    ("U" projectile-rails-find-current-fixture    "current fixture")
    ("T" projectile-rails-find-current-test       "current test")
    ("N" projectile-rails-find-current-migration  "current migration")
    ("Z" projectile-rails-find-current-serializer "current serializer"))))

(pretty-hydra-define jnf/projectile-rails-goto--menu
  (:foreign-keys warn :title jnf/projectile-rails--title :quit-key "q" :exit t)
  ("Rails > Goto"
   (("f" projectile-rails-goto-file-at-point "file at point")
    ("g" projectile-rails-goto-gemfile       "Gemfile")
    ("p" projectile-rails-goto-package       "package")
    ("r" projectile-rails-goto-routes        "routes")
    ("d" projectile-rails-goto-schema        "schema")
    ("s" projectile-rails-goto-seeds         "seeds")
    ("h" projectile-rails-goto-spec-helper   "spec helper"))))

(pretty-hydra-define jnf/projectile-rails-run--menu
  (:foreign-keys warn :title jnf/projectile-rails--title :quit-key "q" :exit t)
  ("Rails > Run"
   (("r" projectile-rails-rake       "rake")
    ("c" projectile-rails-console    "console")
    ("b" projectile-rails-dbconsole  "dbconsole")
    ("s" projectile-rails-server     "server")
    ("g" projectile-rails-generate   "generate")
    ("d" projectile-rails-destroy    "destroy")
    ("x" projectile-rails-extract-region "extract region"))))

(pretty-hydra-define jnf/projectile-rails--menu
  (:foreign-keys warn :title jnf/projectile-rails--title :quit-key "q" :exit t)
  ("Rails"
   (("." projectile-rails-goto-file-at-point "Goto file at point")
    ("c" jnf/projectile-rails-find-current-resource--menu/body "Find current resource…")
    ("f" jnf/projectile-rails-find-resource--menu/body "Find a resource…")
    ("g" jnf/projectile-rails-goto--menu/body "Goto…")
    ("i" rails-routes-insert-no-cache "Insert route…")
    ("I" rails-routes-insert "Insert route (from cache)…")
    ("r" jnf/projectile-rails-run--menu/body "Run & interact…"))))

;; (defun rspec-hyrax ()
;;   "Setup rspec mode docker configuration for Hyrax."
;;   (interactive)
;;   (setq rspec-docker-command "docker-compose exec -w /app/samvera/hyrax-engine")
;;   (setq rspec-docker-cwd "/app/samvera/hyrax-engine/")
;;   (setq rspec-docker-container "app"))

;; I most often write documentation using yard.  See
;; https://yardoc.org.
(use-package yard-mode
  :straight t
  :hook (ruby-mode . yard-mode)
  )

;; Adds the helpful `bundle-open'
(use-package bundler
  :straight (bundler :type git :host github :repo "endofunky/bundler.el"))
