;;; jf-project-theme-colors --- Consolidated colors for themes -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;; The options that I'm considering are from the modus color palette:
;;
;; bg-red-intense
;; bg-green-intense
;; bg-yellow-intense
;; bg-blue-intense
;; bg-magenta-intense
;; bg-cyan-intense
;; bg-red-subtle
;; bg-green-subtle
;; bg-yellow-subtle
;; bg-blue-subtle
;; bg-magenta-subtle
;; bg-cyan-subtle
;; bg-red-nuanced
;; bg-green-nuanced
;; bg-yellow-nuanced
;; bg-blue-nuanced
;; bg-magenta-nuanced
;; bg-cyan-nuanced
;; bg-ochre
;; bg-lavender
;; bg-sage

;;; Code
(require 'modus-themes)
(require 'projectile)
(defvar jf/project/theme-colors/table
  '(("~/git/dotemacs/" . bg-green-subtle)
    ("~/git/dotzshrc/" . bg-green-subtle)
    ("~/git/takeonrules.source/" . bg-green-nuanced)
    ("~/git/org/" . bg-green-nuanced)
    ("~/git/britishlibrary/" . bg-blue-intense)
    ("~/git/adventist-dl/" . bg-yellow-intense)
    ("~/git/utk-hyku/" . bg-red-intense)
    ("~/git/bulkrax/" . bg-sage)
    ("~/git/space_stone/" . bg-magenta-nuanced)
    ("~/git/derivative-rodeo/" . bg-red-intense)
    ("~/git/hyrax/" . bg-sage))
  "The `car' of each list item should be of begin with \"~/\" and
 end with \"/\" (so as to conform to multiple machines and
 projectile's interface.")

(cl-defun jf/project/theme-colors/current (&key (default 'bg-blue-subtle))
  "Returns a HEX color (e.g. \"#CCDDEE\") for the given project.

The DEFAULT is a named color in the `modus-themes' palette."

  (let* ((project-dir (abbreviate-file-name (or (projectile-project-root) "~/")))
	 (name (alist-get project-dir
			  jf/project/theme-colors/table
			  default nil #'string=)))
	 (modus-themes-get-color-value name)))

(defvar jf/project/theme-colors/faces
  (list 'line-number-current-line 'mode-line-active)
  "The faces to update with the theme-colors.")

(defvar jf/project/theme-colors/hooks
  (list 'buffer-list-update-hook
	'projectile-after-switch-project-hook)
  "The hooks to call to set the theme colors.")

(defun jf/project/theme-colors/apply-to-buffer ()
  "Apply the the project's colors to the buffer (e.g. 'mode-line-active)."
  (unless (active-minibuffer-window)
    (dolist (element jf/project/theme-colors/faces)
      (face-remap-add-relative
       element
       `( :background ,(jf/project/theme-colors/current)
	  :foreground ,(face-attribute 'default :foreground))))))

;; I need to ensure that I'm not doing this while Emacs is initializing.  If I
;; don't have the 'after-init-hook I experience significant drag/failure to
;; initialize.
(add-hook 'after-init-hook
	  (lambda ()
	    (dolist (hook jf/project/theme-colors/hooks)
	      (add-hook hook #'jf/project/theme-colors/apply-to-buffer))))

(provide 'jf-project-theme-colors)
;;; jf-project-theme-colors.el ends here
