;; -*- lexical-binding: t; -*-
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
;; I'm just going to trust themes
(setq custom-safe-themes t)

(load "jnf-modus-main.el" nil jnf-silence-loading-log)

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
(set-face-attribute 'default nil :family jnf/fixed-width-font-name :height 140)
;; Proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "ETBembo" :height 1.1)
;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family jnf/fixed-width-font-name :height 1.0)
;;
;; END BLOCK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(blink-cursor-mode t)
;; Doing a bit of configuration of my cursors
(setq-default cursor-type 'bar)

;; Useful for referential icons.
(use-package all-the-icons
  :config
  ;; A convenience function to create a nice string
  (defun with-faicon (icon str &optional height v-adjust)
    "Displays an ICON  from Font Awesome icon.

The STR identifies the icon and the HEIGHT and V-ADJUST provide
the configuration."
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-material (icon str &optional height v-adjust)
    "Displays an ICON  from Font Material icon.

The STR identifies the icon and the HEIGHT and V-ADJUST provide
the configuration."
    (s-concat (all-the-icons-material icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-octicon (icon str &optional height v-adjust)
    "Displays an ICON  from Octicon icon.

The STR identifies the icon and the HEIGHT and V-ADJUST provide
the configuration."
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-alltheicon (icon str &optional height v-adjust)
    "Displays an ICON  from All the Icons icon.

The STR identifies the icon and the HEIGHT and V-ADJUST provide
the configuration."
    (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
  :straight t)

;; Disabled because on 2021-04-11 I got the following error:
;; *ERROR*: Symbol’s value as variable is void: file
;;
;; Incorporates file icons with file listings of dired
(use-package all-the-icons-dired
  :straight t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; (use-package powerline
;;   :straight (powerline :type git :host github :repo "milkypostman/powerline")
;;   :config (powerline-default-theme))

;; A nice looking modeline enhancement
(use-package spaceline
  :straight t)

;; Add some visual flair to the modeline enhancements
;; (use-package spaceline-all-the-icons
;;   :straight t
;;   :after spaceline
;;   :config (spaceline-all-the-icons-theme))

;; Ensuring that some windows are treated as popups (e.g., something
;; easier to dismiss, a bit more like the mini-buffer).
(use-package popper
  :straight t
  :bind (("C-`" . jnf/popper))
  :config
  (defun jnf/popper (prefix_arg)
  "Call `popper-cycle', but with PREFIX_ARG invoke a less common popper method.

With one PREFIX_ARG, `popper-toggle-latest'.
With two (or more) PREFIX_ARG `popper-toggle-type'."
  (interactive "P")
  (let ((prefix (car prefix_arg)))
    (cond
     ((not prefix)  (popper-cycle))
     ((= prefix 4)  (popper-toggle-latest))
     (t (popper-toggle-type)))))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode
          "^\\*helpful.*\\*$"))
  (popper-mode +1)
  (popper-echo-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BEGIN frame and window quick setup
(defun gk-layouts-3col ()
  "Three column layout.

Tries to preserve the order of window buffers and active window."
  (interactive)
  ;; Record active window buffer.
  (let ((cbuf (current-buffer)))
    ;; Switch to leftmost window.
    (ignore-errors (cl-loop do (windmove-left)))
    (let ((buffers
           (mapcar #'window-buffer (-take 3 (window-list))))
          (width (/ (frame-width) 3)))
      (delete-other-windows)
      (split-window-horizontally width)
      (other-window 1)
      (split-window-horizontally)
      (other-window -1)
      (dolist (b buffers)
        (switch-to-buffer b)
        (other-window 1)))
    ;; Switch to previously visible buffer’s window.
    (select-window (get-buffer-window cbuf))))


(defun gk-layouts-main-and-sidekicks ()
  "One horizontal split, the right window split in two.

Tries to preserve the order of window buffers and active window."
  (interactive)
  ;; Record active window buffer.
  (let ((cbuf (current-buffer)))
    ;; Switch to leftmost window.
    (ignore-errors (cl-loop do (windmove-left)))
    (let ((buffers
           (mapcar #'window-buffer (-take 3 (window-list)))))
      (delete-other-windows)
      (split-window-horizontally)
      (other-window 1)
      (split-window-vertically)
      (other-window -1)
      (dolist (b buffers)
        (switch-to-buffer b)
        (other-window 1)))
    ;; Switch to previously visible buffer’s window.
    (select-window (get-buffer-window cbuf))))

(bind-key "C-x \\" #'gk-layouts-main-and-sidekicks)
;; END frame and window quick setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A window manager for emacs, allowing fast toggles between windows
;; as well as opening or moving those windows.
;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :straight t
  :bind (("M-o" . ace-window)))


;; See https://www.reddit.com/r/emacs/comments/r7l3ar/how_do_you_scroll_half_a_page/
(global-set-key (kbd "M-n") 'jnf/scroll-down-half-page)
(defun jnf/scroll-down-half-page ()
  "Scroll down half a page while keeping the cursor centered"
  (interactive)
  (let ((ln (line-number-at-pos (point)))
    (lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) (move-to-window-line nil))
      ((= ln lmax) (recenter (window-end)))
      (t (progn
           (move-to-window-line -1)
           (recenter))))))

(global-set-key (kbd "M-p") 'jnf/scroll-up-half-page)
(defun jnf/scroll-up-half-page ()
  "Scroll up half a page while keeping the cursor centered"
  (interactive)
  (let ((ln (line-number-at-pos (point)))
    (lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) nil)
      ((= ln lmax) (move-to-window-line nil))
      (t (progn
           (move-to-window-line 0)
           (recenter))))))
