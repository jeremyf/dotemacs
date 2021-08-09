;;; jnf-display.el --- Summary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  Some basic display options:
;;
;;  * Themes
;;  * Colors
;;  * Cursor type
;;  * Icons
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'default-frame-alist '(font . "JetBrains Mono 14" ))
(set-face-attribute 'default t :font "JetBrains Mono 14" )
;; I'm just going to trust themes
(setq custom-safe-themes t)

(require 'jnf-modus-main.el)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN BLOCK
;;
;; With a quick bit of testing, it appears that the following
;; set-face-attribute declarations should be made after the theme
;; declarations.  When the following statements were declared before
;; the themes, and I toggled my theme, the font changed to something
;; unexpected.  With them declared after, I keep the fonts between
;; toggles.
;;
;; Main typeface, I'm toggling between "JetBrains Mono" and "Hack"
(set-face-attribute 'default nil :family "JetBrains Mono" :height 140)
;; Proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "ETBembo" :height 1.0)
;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height 1.0)
;;
;; END BLOCK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(blink-cursor-mode t)
;; Doing a bit of configuration of my cursors
(setq-default cursor-type 'bar)

;; Nice for neotree
(use-package all-the-icons
  :straight t)

;; Disabled because on 2021-04-11 I got the following error:
;; *ERROR*: Symbol’s value as variable is void: file
;;
;; Incorporates file icons with file listings of dired
;; (use-package all-the-icons-dired
;;   :straight t
;;   :after all-the-icons
;;   :hook (dired-mode . all-the-icons-dired-mode))

(defun jnf/powerline-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face0 (if active 'powerline-active0 'powerline-inactive0))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" face0 'l)
                                     (powerline-raw "%4l" face0 'l)
                                     (powerline-raw ":" face0 'l)
                                     (powerline-raw "%3c" face0 'r)
                                     (powerline-raw " " face0)
                                     (powerline-raw "%6p" face0 'r)
                                     ;; Commenting out the heads up display of relative position.
                                     ;; (when powerline-display-hud
                                     ;;   (powerline-hud face2 face0))
                                     ;; I rarely care about buffer size, so let's not worry about this.
                                     ;; (when powerline-display-buffer-size
                                     ;;   (powerline-buffer-size face0 'l))
                                     ;; (when powerline-display-mule-info
                                     ;;   (powerline-raw mode-line-mule-info face0 'l))
                                     (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format face0 'l))
                                     (powerline-raw " " face0)
                                     (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                       (powerline-raw erc-modified-channels-object face0 'l))
                                     (powerline-major-mode face0 'l)
                                     (powerline-process face0)
                                     (powerline-vc face0 'r)
                                     (powerline-minor-modes face0 'l)
                                     (powerline-narrow face0 'l)
                                     (powerline-raw " " face0)
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) face0 'l))))
                          (rhs (list (powerline-raw global-mode-string face0 'r)
                                     (unless window-system
                                       (powerline-raw (char-to-string #xe0a1) face0 'l))
                                     (when powerline-display-hud
                                       (powerline-hud face0 face0))
                                     (powerline-fill face0 0)
                                     )))
                     (concat (powerline-render lhs)
                             (powerline-fill face0 (powerline-width rhs))
                             (powerline-render rhs)))))))

(use-package powerline
  :straight (powerline :type git :host github :repo "milkypostman/powerline")
  :config (jnf/powerline-theme))

;; A nice looking modeline enhancement
;; (use-package spaceline
  ;; :straight t)

;; ;; Add some visual flair to the modeline enhancements
;; (use-package spaceline-all-the-icons
;;   :straight t
;;   :after spaceline
;;   :config (spaceline-all-the-icons-theme))


;; A convenience function to create a nice string
(defun with-faicon (icon str &optional height v-adjust)
    "Displays an icon from Font Awesome icon."
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun with-material (icon str &optional height v-adjust)
    "Displays an icon from Font Material icon."
    (s-concat (all-the-icons-material icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(provide 'jnf-display.el)
;;; jnf-display.el ends here
