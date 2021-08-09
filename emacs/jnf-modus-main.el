;;; jnf-modus-main.el --- Summary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This package provides the "master" branch method for loading modus themes.
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I'm just going to trust themes

(use-package modus-themes
  :straight (:type git :host gitlab :repo "protesilaos/modus-themes" :branch "main")
  :init
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs t
        modus-themes-fringes 'intense ; {nil,'subtle,'intense}
        modus-themes-mode-line '(accented 3d) ; {nil,'3d,'moody}
        modus-themes-syntax '(alt-syntax yellow-comments green-strings)
        modus-themes-hl-line '(accented intense underline)
        modus-themes-intense-paren-match '(bold intense)
        modus-themes-links '(faint)
        modus-themes-subtle-line-numbers t
        modus-themes-no-mixed-fonts nil
        modus-themes-region '(bg-only accented)
        modus-themes-prompts '(intense accented)
        modus-themes-tabs-accented t
        modus-themes-completions 'opinionated ; {nil,'moderate,'opinionated}
        modus-themes-diffs nil ; {nil,'desaturated,'fg-only}
        modus-themes-org-blocks 'rainbow
        modus-themes-variable-pitch-headings nil
        modus-themes-scale-headings t
        modus-themes-scale-1 1.1
        modus-themes-scale-2 1.15
        modus-themes-scale-3 1.21
        modus-themes-scale-4 1.27
        modus-themes-scale-5 1.33))


;; Based on system type, either load the OSX apperance (e.g. dark or
;; light) and load accordingly.
(if (eq system-type 'darwin)
    (progn
      (defun jnf/dark ()
        "Toggle system-wide Dark or Light setting."
        (interactive)
        (shell-command "osascript -e 'tell application \"System Events\" to tell appearance preferences to set dark mode to not dark mode'")
        (jnf/emacs-theme-by-osx-appearance))

      (defalias 'modus-themes-toggle 'jnf/dark)
      (defun jnf/emacs-theme-by-osx-appearance ()
        "Set theme based on OSX apperance state."
        (if (equal "Dark" (substring (shell-command-to-string "defaults read -g AppleInterfaceStyle") 0 4))
            (modus-themes-load-vivendi)
            (modus-themes-load-operandi)))
      (jnf/emacs-theme-by-osx-appearance))
  (progn
    (defun modus-themes-toggle ()
      "Toggle between `modus-operandi' and `modus-vivendi' themes."
      (interactive)
      (if (eq (car custom-enabled-themes) 'modus-operandi)
            (modus-themes-load-vivendi)
            (modus-themes-load-operandi)))
      (modus-themes-load-operandi)))

  (global-set-key (kbd "<s-clear>") 'modus-themes-toggle)
  (provide 'jnf-modus-main.el)
;;; jnf-modus-main.el ends here
