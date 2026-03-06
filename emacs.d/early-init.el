;; -*- lexical-binding: t; -*-

;; Optimization: Increase GC threshold and disable file-name-handler during startup
(setq gc-cons-threshold (* 100 1024 1024))
(defvar ii/file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1024 1024))
            (setq file-name-handler-alist ii/file-name-handler-alist-old)))

;; (add-to-list 'default-frame-alist '(undecorated-round . t))
(setq package-enable-at-startup nil)

;; Bootstrap straight.el
;; https://github.com/radian-software/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
