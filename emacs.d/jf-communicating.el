;;; jf-communicating.el --- Communication tooling -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;; Packages specifically here for helping with my communicating activities.

;;; Code

(use-package mastodon
  :straight t
  :config (setq mastodon-instance-url "https://dice.camp"
                          mastodon-active-user "takeonrules"))

(bind-key "s-3" #'jf/browsing-menu)
(transient-define-prefix jf/browsing-menu ()
  "For browsing things."
  [["Mastodon"
     ("m /" "Buffers" mastodon-switch-to-buffer)
     ("m f" "Federated Timeline" mastodon-tl--get-federated-timeline)
     ("m h" "Home Timeline" mastodon-tl--get-home-timeline)
     ("m l" "Local Timeline" mastodon-tl--get-local-timeline)
     ("m n" "Notifications" mastodon-notifications-get)
     ("m #" "Tags Timeline" mastodon-tl--followed-tags-timeline)
     ("m t" "Toot…" mastodon-toot)
     ]])

(provide 'jf-communicating)
;;; jf-communicating.el ends here
