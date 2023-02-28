;;; jf-project-theme-colors --- Consolidated colors for themes -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;;; Code

(require 'modus-themes)

(defvar jf/project/theme-colors/table
  '(("~/git/dotemacs/" . bg-green-nuanced)
    ("~/git/dotzshrc/" . bg-green-nuanced)
    ("~/git/takeonrules.source/" . bg-magenta-nuanced))
  "The `car' of each list item should be of begin with \"~/\" and
 end with \"/\" (so as to conform to multiple machines and
 projectile's interface.")

(defun jf/project/theme-colors/current ()
  (let* ((project-dir (abbreviate-file-name (projectile-project-root)))
	 (name (alist-get project-dir
			  jf/project/theme-colors/table
			  'bg-blue-subtle nil #'string=)))
	 (modus-themes-get-color-value name)))

(defun jf/mode-line/set-active-mode-line-colors ()
  (unless (active-minibuffer-window)
    (progn
      (face-remap-add-relative
       'mode-line-active
       `( :background ,(jf/project/theme-colors/current)
	  :foreground ,(face-attribute 'default :foreground))))))

;; I need to ensure that I'm not doing this while Emacs is initializing.  If I
;; don't have the 'after-init-hook I experience significant drag/failure to
;; initialize.
(add-hook 'after-init-hook
	  (lambda ()
	    (add-hook 'buffer-list-update-hook
		      #'jf/mode-line/set-active-mode-line-colors)
	    (add-hook 'projectile-after-switch-project-hook
		      #'jf/mode-line/set-active-mode-line-colors)))


(provide 'jf-project-theme-colors)
;;; jf-project-theme-colors.el ends here
