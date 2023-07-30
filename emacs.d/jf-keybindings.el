;;; jf-keybindings --- These are the keys that bind -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;; I'm experimenting with `general'; and as such feel that I want to start from
;; a clean "package" and build up from there.  That means going into my existing
;; bindings and migrating them...if I like this pathway.
;;
;;

;;; Code:

;; emacs.d/jf-versioning.el:102:  :bind (("C-c m" . magit-status)
;; emacs.d/jf-versioning.el:107:  :bind (:map magit-log-mode-map ("C-x g b" . 'jf/magit-browse-pull-request))
;; emacs.d/jf-versioning.el:133:  :bind ("C-x g =" . git-gutter:popup-hunk)
;; emacs.d/jf-versioning.el:172:  :bind (:map git-messenger-map (("p" . 'jf/open-pull-request-for-current-line)
;; emacs.d/jf-versioning.el:174:  :bind (("s-6" . jf/git-messenger-popup)
;; emacs.d/jf-versioning.el:206:  :bind (("C-z" . undo)
;; emacs.d/jf-writing.el:47:  :bind ("C-c C-'" . sdcv-search))
;; emacs.d/jf-writing.el:71:  :bind ("M-q" . unfill-toggle)
;; emacs.d/jf-writing.el:82:  :bind (([C-s-down] . move-text-down)
;; emacs.d/jf-writing.el:95:  :bind (("C-M-SPC" . set-rectangular-region-anchor)
;; emacs.d/jf-completing.el:27:  :bind ("C-M-i" . completion-at-point)
;; emacs.d/jf-completing.el:80:  :bind (;; C-c bindings (mode-specific-map)
;; emacs.d/jf-completing.el:249:  :bind (("C-x C-d" . consult-dir)
;; emacs.d/jf-completing.el:281:  :bind (:map corfu-map
;; emacs.d/jf-completing.el:339:  :bind (("C-c p d" . cape-dabbrev)
;; emacs.d/jf-completing.el:363:  :bind (("C-c g" . grab-mac-link)))
;; emacs.d/jf-completing.el:392:  :bind ("H-h" . jf/helpful-menu)
;; emacs.d/jf-completing.el:415:  :bind (("M-SPC" . hippie-expand))
;; emacs.d/jf-completing.el:514:  :bind (:map org-mode-map (("C-c g" . org-mac-grab-link))))
;; emacs.d/jf-completing.el:524:  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
;; emacs.d/jf-completing.el:526:  :bind (:map tempel-map (([backtab] . tempel-previous)
;; emacs.d/jf-denote.el:77:  :bind ("H-l" . 'jf/denote/link-or-create)
;; emacs.d/jf-reading.el:13:  :bind (:map doc-view-mode-map
;; emacs.d/jf-reading.el:29:  :bind ((:map elfeed-search-mode-map
;; emacs.d/jf-reading.el:139:  :bind (:map eww-mode-map ("U" . eww-up-url))
;; emacs.d/jf-reading.el:140:  :bind (("C-s-w" . browse-url-at-point))
;; emacs.d/jf-illuminating.el:22:  :bind (("C-=" . er/expand-region)
;; emacs.d/jf-illuminating.el:103:  :bind (("C-c C-l" . jf/pulse)))
;; emacs.d/jf-illuminating.el:153:;;   :bind (:map symbol-overlay-mode-map
;; emacs.d/jf-utility.el:85:  :bind (:map deadgrep-mode-map
;; emacs.d/jf-utility.el:111:  :bind (:map wgrep-mode-map
;; emacs.d/jf-utility.el:129:  :bind (("C-a" . crux-move-beginning-of-line)
;; emacs.d/jf-utility.el:139:  :bind ("C-c =" . math-at-point))
;; emacs.d/jf-coding.el:263:;;   :bind (("C-c C-e" . emmet-expand-yas ))
;; emacs.d/jf-coding.el:347:  :bind (:map rspec-mode-map (("s-." . 'rspec-toggle-spec-and-target)))
;; emacs.d/jf-coding.el:348:  :bind (:map ruby-mode-map (("s-." . 'rspec-toggle-spec-and-target)))
;; emacs.d/jf-menus.el:71:  :bind ("C-x f" . file-info-show)
;; emacs.d/jf-organizing.el:23:  :bind ("s-." . projectile-toggle-between-implementation-and-test))
;; emacs.d/jf-org-mode.el:60:  :bind ("C-c C-j" . jf/org-mode/jump-to-agenda-or-mark)
;; emacs.d/jf-org-mode.el:61:  :bind (:map org-mode-map (("C-c C-j" . jf/org-mode/jump-to-agenda-or-mark)
;; emacs.d/jf-org-mode.el:234:  :bind (:map org-mode-map
;; emacs.d/jf-org-mode.el:237:  :bind (("C-c l s" . org-store-link)
;; emacs.d/jf-org-mode.el:720:  :bind ("C-M-s-c" . jf/formatted-copy-org-to-html)
;; emacs.d/jf-navigating.el:16:  :bind (("C-x o" . ace-window)
;; emacs.d/jf-navigating.el:22:  :bind (("C-j" . avy-goto-char-timer))
;; emacs.d/jf-navigating.el:42:  :bind ("s-4" . 'imenu-list-smart-toggle)
;; emacs.d/jf-navigating.el:43:  :bind (:map imenu-list-major-mode-map ("o" . 'imenu-list-goto-entry))
;; emacs.d/jf-windows.el:218:;;   :bind (:map bufler-list-mode-map
;; emacs.d/jf-windows.el:221:;;   :bind (("s-3" . bufler-switch-buffer)


(use-package general
  :straight t
  :config
  (general-define-key
    :prefix "C-c C-o"
    "a" 'org-agenda
    "c" 'org-capture))



(provide 'jf-keybindings)
;;; jf-keybindings.el ends here
