;;; jf-framing.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;; Packages specifically here for helping with my framing activities.

;;; Code

(use-package so-long
  :defer t
  :straight t
  :bind
  (:map so-long-mode-map
        ("C-s" . isearch-forward)
        ("C-r" . isearch-backward))
  :config (global-so-long-mode 1))

(provide 'jf-framing)
;;; jf-framing.el ends here
