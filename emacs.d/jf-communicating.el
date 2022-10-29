;;; jf-communicating.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;; Packages specifically here for helping with my communicating activities.

;;; Code

(use-package mastodon
    :straight t
    :config (setq mastodon-instance-url "https://tabletop.social"
		  mastodon-active-user "takeonrules"))

(provide 'jf-communicating)
;;; jf-communicating.el ends here
