;;; jnf-ruby.el --- Summary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  This package provides most of the Ruby configuration
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; I'm provisionally addinig enh-ruby-mode and robe.  I've found that
;; LSP can work, but has it's own problems; namely you need solargraph
;; installed for all versions.
(use-package enh-ruby-mode
  :straight t
  :mode (("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode)))
;; (add-to-list 'auto-mode-alist
             ;; '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(use-package robe
  :straight t
  :hook (enh-ruby-mode . robe-mode))

;; I most often write tests using rspec.
(use-package rspec-mode
  :straight t
  :custom (rspec-use-spring-when-possible nil)
  :bind (:map rspec-mode-map (("s-." . 'rspec-toggle-spec-and-target)))
  :bind (:map enh-ruby-mode-map (("s-." . 'rspec-toggle-spec-and-target)))
  :hook (ruby-mode . rspec-mode) ;; should this be `ruby-mode'
  (ruby-mode . eldoc-mode)) ;; should this be `ruby-mode'

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(use-package rails-routes
  :straight t)
  ;; :bind (:map ruby-mode-map (("C-c C-o" . 'rails-routes-insert-no-cache)))
  ;; :bind (:map web-mode-map (("C-c C-o" . 'rails-routes-insert-no-cache))))


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
  :hook (enh-ruby-mode . yard-mode))

;; Adds the helpful `bundle-open'
(use-package bundler
  :straight (bundler :type git :host github :repo "endofunky/bundler.el"))

(provide 'jnf-ruby.el)
;;; jnf-ruby.el ends here
