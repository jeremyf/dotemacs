;;; jf-writing.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;; Packages specifically here for helping with my writing activities.

;;; Code

;; This follows from http://mbork.pl/2017-01-14_I'm_now_using_the_right_dictionary
(use-package sdcv-mode
  :straight (sdcv-mode :type git :host github :repo "gucong/emacs-sdcv")
  :bind ("C-c C-'" . sdcv-search))

(provide 'jf-writing)
;;; jf-writing.el ends here
