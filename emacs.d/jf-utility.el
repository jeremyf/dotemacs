;;; jf-utility.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;; Packages specifically here for helping with my coding activities.

;;; Code

(use-package xref
  :straight t
  :custom
  (xref-file-name-display 'project-relative)
  (xref-search-program 'ripgrep))

(provide 'jf-utility)
;;; jf-utility.el ends here
