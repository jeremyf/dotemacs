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

;; Set some timers.
(use-package tmr
  ;; My dbus install is not behaving so I'm cheating with a bit of AppleScript
  :config (defun jf/tmr-notification-notify (timer)
	    "Dispatch a notification for TIMER."
	    (let ((title "TMR May Ring (Emacs tmr package)")
		  (description (tmr--timer-description timer)))
	      (ns-do-applescript (concat "display notification \"" description "\" sound name \"Glass\""))))
  :custom (tmr-notify-function #'jf/notifications-notify)
  (tmr-timer-completed-functions
   (list #'tmr-print-message-for-completed-timer
	 #'tmr-sound-play
	 #'jf/tmr-notification-notify))
  :straight (tmr :host nil :type git :repo "https://git.sr.ht/~protesilaos/tmr"))

(use-package transient :straight t)

(use-package ts :straight t)

;;; Support packages

;; Load keychain environment
(use-package keychain-environment
  :straight t
  :config (keychain-refresh-environment))

;; A modern list API for Emacs. No 'cl required.  (See https://github.com/magnars/dash.el/)
(use-package dash :straight t)

;; A modern API for working with files and directories in Emacs. (See https://github.com/rejeep/f.el/)
(use-package f :straight t)

;; The long lost Emacs string manipulation library.  (See https://github.com/magnars/s.el/)
(use-package s :straight t)

;; “EditorConfig helps maintain consistent coding styles for multiple
;; developers working on the same project across various editors and IDEs.”
;; See https://editorconfig.org/#overview for more details.
(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

(use-package rg
  :config (rg-enable-menu)
  ;; :init (setq ripgrep-arguments "--ignore-case")
  :straight t)

;; “Edit a grep buffer and apply those changes to the file buffer.”  In other
;; words, after “searching” for something, sending the results to a buffer
;; (via `embark-export' or such thing), you can edit that search results
;; buffer and propogate the changes to the locations of the elements that
;; matched the search.
;;
;;   1.  Call `consult-ripgrep' (via ~C-c f~) to search for something.
;;   2.  Call `embark-export' (via ~C-s-e~) to export to a grep buffer.
;;   3.  Call `wgrep-change-to-wgrep-mode' (via ~e~ or ~C-c C-p~)
;;   4.  Edit the grep buffer as you would anywhere else.
;;   5.  Save (via ~C-x C-s~) or Cancel (via ~C-c C-k~).
(use-package wgrep
  :after (embark-consult ripgrep)
  :straight t
  :bind (:map wgrep-mode-map
	      ;; Added keybinding to echo Magit behavior
	      ("C-c C-c" . wgrep-finish-edit)
	      :map grep-mode-map
	      ("e" . wgrep-change-to-wgrep-mode)
	      :map ripgrep-search-mode-map
	      ("e" . wgrep-change-to-wgrep-mode)))

;; A mix of a few odd and useful functions.
(use-package crux
  :straight t
  :bind (("C-a" . crux-move-beginning-of-line)
	 ("<C-s-return>" . crux-smart-open-line-above)
	 ("C-s-k" . crux-kill-line-backwards)
	 ("<s-backspace>" . crux-kill-line-backwards)
	 ("<f9>" . crux-kill-other-buffers)))

(use-package math-at-point
  :straight (math-at-point :type git :host github :repo "shankar2k/math-at-point")
  :bind ("C-c =" . math-at-point))

;;;; Hammerspoon --------------------------------------------------------------



;; Hammerspoon is Lua application that provides a consistent API for
;; interacting with MacOS.  The editWithEmacs.spoon allows me to copy text from
;; one region, edit it in Emacs, and paste it back into the Application.
(when (file-directory-p
       "~/git/dotzshrc/symlinks/.hammerspoon/Spoons/editWithEmacs.spoon")
  (load
   "~/git/dotzshrc/symlinks/.hammerspoon/Spoons/editWithEmacs.spoon/hammerspoon.el"
   nil
   jf/silence-loading-log))

(require 'transient)
;; this suffix provides a dynamic description of the current major mode for a
;; `hammerspoon-edit-minor-mode' buffer.  And the prefix’s function toggles
;; that mode.
(transient-define-suffix jf/hammerspoon-toggle-mode ()
  "Set the hammerspoon mode"
  :description '(lambda ()
		  (concat
		   "Hammerspoon Mode: "
		   (propertize
		    (format "%s" major-mode)
		    'face 'transient-argument)))
  (interactive)
  (hammerspoon-toggle-mode))




;; The following function facilitates a best of both worlds.  By default, I
;; want Option to be Meta (e.g. \"M-\") in Emacs.  However, I can toggle that
;; setting.  That way if I need an umlaut (e.g., \"¨\"), I can use MacOS’s
;; native functions to type \"⌥\" + \"u\".
;;
;; I like having MacOS’s native Option (e.g. =⌥=) modifier available.  But
;; using that default in Emacs would be a significant hinderance.
(defun jf/toggle-osx-alternate-modifier ()
  "Toggle native OS-X Option modifier setting (e.g. `ns-alternate-modifier')."
  (interactive)
  (if ns-alternate-modifier
      (progn (setq ns-alternate-modifier nil)
	     (message "Enabling OS X native Option modifier"))
    (progn (setq ns-alternate-modifier 'meta)
	   (message "Disabling OX X native Option modifier (e.g. Option as Meta)"))))

;; I try to get quick feedback when writing emacs-lisp; the
;; `jf/eval-region-dwim' binds a mnemonic key sequence to an extend
;; `eval-region'.
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'jf/eval-region-dwim)
(defun jf/eval-region-dwim ()
  "When region is active, evaluate it and kill the mark. Else,
      evaluate the whole buffer."
  (interactive)
  (if (not (region-active-p))
      (progn
	(message "Evaluating buffer...")
	(eval-buffer))
    (progn
      (message "Evaluating region...")
      (eval-region (region-beginning) (region-end)))
    (setq-local deactivate-mark t)))


(provide 'jf-utility)
;;; jf-utility.el ends here
