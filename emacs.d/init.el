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

(add-to-list 'load-path "~/git/dotemacs/emacs.d")
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file :noerror)

(require 'jf-launching)
(require 'jf-minor-mode-maker)
(require 'jf-illuminating)
(require 'jf-navigating)
(require 'jf-fonts-and-iconography)
(require 'jf-windows)
(require 'jf-utility)
(require 'jf-completing)
(require 'jf-coding)
(require 'jf-organizing)
(require 'jf-framing)
(require 'jf-utility)
(require 'jf-writing)
(require 'jf-communicating)
(require 'jf-org-mode)
(require 'jf-denote)
(require 'jf-reading)
(require 'jf-versioning)
(require 'jf-quick-help)
(require 'jf-gaming)
(require 'jf-blogging)
(require 'jf-project)
(require 'jf-menus)
(require 'jf-experiments)


(setq jf/artist-mode-spraycan "🞄⁛◌🞆⊖⊗⁛●◯⦿⬤")
(load (concat user-emacs-directory "hide-comnt.el") :noerror)

(server-start)

(setq gc-cons-threshold (expt 2 24) ;; 16777216
      gc-cons-percentage 0.1)

(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
