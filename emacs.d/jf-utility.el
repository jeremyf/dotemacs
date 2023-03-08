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
	      (ns-do-applescript (concat "display notification \""
					 description
					 "\" sound name \"Glass\""))))
  :custom (tmr-notify-function #'jf/notifications-notify)
  (tmr-timer-completed-functions
   (list #'tmr-print-message-for-completed-timer
	 #'tmr-sound-play
	 #'jf/tmr-notification-notify))
  :straight (tmr :host nil :type git
		 :repo "https://git.sr.ht/~protesilaos/tmr"))

(use-package transient :straight t)

(use-package ts :straight t)

;;; Support packages

;; Load keychain environment
(use-package keychain-environment
  :straight t
  :config (keychain-refresh-environment))

(use-package dash
  ;; A modern list API for Emacs. No 'cl required.
  ;; (See https://github.com/magnars/dash.el/)
  :straight t)

(use-package f
  ;; A modern API for working with files and directories in Emacs.
  ;; (See https://github.com/rejeep/f.el/)
  :straight t)

(use-package s
  ;; The long lost Emacs string manipulation library.
  ;; (See https://github.com/magnars/s.el/)
  :straight t)

(use-package editorconfig
  ;; “EditorConfig helps maintain consistent coding styles for multiple
  ;; developers working on the same project across various editors and IDEs.”
  ;; See https://editorconfig.org/#overview for more details.
  :straight t
  :config
  (editorconfig-mode 1))

;; Where consult-rg provides a live search feature, deadgrep provides a
;; resulting search buffer.  You submit your search term and get the metadata
;; and the matches.
(use-package deadgrep
  :custom (deadgrep-display-buffer-function
	   (lambda (buffer) (display-buffer-same-window buffer '())))
  :straight t
  :config
  (defun jf/deadgrep/exit-with-save ()
    "Exit deadgrep edit mode and prompt to save buffers."
    (interactive)
    (when (eq major-mode #'deadgrep-edit-mode)
      (progn
	(deadgrep-mode)
	(call-interactively 'save-some-buffers))))
  :bind (:map deadgrep-mode-map
	      (("C-c C-p" . deadgrep-edit-mode)
	       ("e" . deadgrep-edit-mode))
	      :map deadgrep-edit-mode-map
	      ("C-c C-c" . jf/deadgrep/exit-with-save)))

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
  :straight (:type git :host github :repo "jeremyf/Emacs-wgrep" :branch "main")
  :bind (:map wgrep-mode-map
	      ;; Added keybinding to echo Magit behavior
	      ("C-c C-c" . wgrep-finish-edit)
	      :map grep-mode-map
	      ("e" . wgrep-change-to-wgrep-mode)
	      :map ripgrep-search-mode-map
	      ("e" . wgrep-change-to-wgrep-mode)))

(use-package rg
  :after (wgrep)
  :config (rg-enable-menu)
  ;; :init (setq ripgrep-arguments "--ignore-case")
  :straight t)

;; A mix of a few odd and useful functions.
(use-package crux
  :straight t
  :bind (("C-a" . crux-move-beginning-of-line)
	 ("<C-s-return>" . crux-smart-open-line-above)
	 ("C-s-k" . crux-kill-line-backwards)
	 ("<s-backspace>" . crux-kill-line-backwards)
	 ("<f9>" . crux-kill-other-buffers)))

(use-package math-at-point
  :straight (math-at-point :type git :host github
			   :repo "shankar2k/math-at-point")
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


(global-set-key (kbd "C-k") 'jf/kill-line-or-region)
(defun jf/kill-line-or-region (&optional arg)
  "Kill the selected region otherwise kill the ARG number of lines."
  (interactive "P")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-line arg)))

(global-set-key (kbd "C-c n") 'jf/nab-file-name-to-clipboard)
(defun jf/nab-file-name-to-clipboard (arg)
  "Nab, I mean copy, the current buffer file name to the clipboard.

  When you pass the universal ARG prompt for different aspects of a file."
  ;; https://blog.sumtypeofway.com/posts/emacs-config.html
  (interactive "P")
  (let* ((prefix (car arg))
         (raw-filename
          (if (equal major-mode 'dired-mode)
	      default-directory
	    (buffer-file-name)))
	 (options '(("Filename, Basename" . (lambda (f) (file-name-nondirectory f)))
		    ("Filename, Project Relative" . (lambda (f) (concat "./" (file-relative-name f (projectile-project-root)))))
		    ("Filename, Full" . (lambda (f) (f)))
		    ("Dirname" . (lambda (f) (file-name-directory f)))
		    ("Dirname, Project Relative" . (lambda (f) (concat "./" (file-relative-name (file-name-directory f) (projectile-project-root)))))))
         (filename
	  (if prefix
	      (funcall (alist-get (completing-read "Option: " options nil t)
				  options nil nil #'string=)
		       raw-filename)
	    raw-filename)))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun jf/sort-unique-lines (reverse beg end
				     &optional adjacent keep-blanks interactive)
  "Sort lines and delete duplicates.

  By default the sort is lexigraphically ascending.  To sort as
  descending set REVERSE to non-nil.  Specify BEG and END for the
  bounds of sorting.  By default, this is the selected region.

  I've included ADJACENT, KEEP-BLANKS, and INTERACTIVE so I can
  echo the method signature of `sort-lines' and
  `delete-duplicate-lines'"
  (interactive "P\nr")
  (sort-lines reverse beg end)
  (delete-duplicate-lines beg end reverse adjacent keep-blanks interactive))

;; Sometimes I just want to duplicate an area without copy and paste.  This
  ;; helps that process.  It’s not as smart as TextMate’s equivalent function,
  ;; but it’s close enough.

(global-set-key (kbd "C-M-d") 'jf/duplicate-current-line-or-lines-of-region)
(global-set-key (kbd "C-c d") 'jf/duplicate-current-line-or-lines-of-region)
(defun jf/duplicate-current-line-or-lines-of-region (arg)
  "Duplicate ARG times current line or the lines of the current region."
  (interactive "p")
  (if (use-region-p)
      (progn
        (when (> (point) (mark))
          (exchange-point-and-mark))
        (beginning-of-line)
        (exchange-point-and-mark)
        (end-of-line)
        (goto-char (+ (point) 1))
        (exchange-point-and-mark)
        (let* ((end (mark))
               (beg (point))
               (region
                (buffer-substring-no-properties beg end)))
          (dotimes (_i arg)
            (goto-char end)
            (insert region)
            (setq end (point)))))
    (crux-duplicate-current-line-or-region arg)))

;; A simple wrapper around scratch, that helps name it and sets the major mode
;; to `org-mode'.
(global-set-key (kbd "<f12>") 'jf/create-scratch-buffer)
(cl-defun jf/create-scratch-buffer (&key (mode 'org-mode))
  "Quickly open a scratch buffer and enable the given MODE."
  (interactive)
  (crux-create-scratch-buffer)
  (rename-buffer (concat "*scratch* at " (format-time-string "%Y-%m-%d %H:%M")))
  (funcall mode))

;; Sometimes I want to move, without renaming, a file.  This function helps
;; make that easy.
(global-set-key (kbd "C-x m") 'jf/move-file)
(defun jf/move-file (target-directory)
  "Write this file to TARGET-DIRECTORY, and delete old one."
  (interactive "DTarget Directory: ")
  (let* ((source (expand-file-name (file-name-nondirectory
				    (buffer-file-name))
				   default-directory))
         (target (f-join target-directory (file-name-nondirectory
					   (buffer-file-name)))))
    (save-buffer)
    (rename-file source target)
    (kill-current-buffer)))

(global-set-key (kbd "s-5") 'jf/org-insert-immediate-active-timestamp)
(defun jf/org-insert-immediate-active-timestamp (arg)
  "Insert an active date for today.

  One universal arg (e.g., prefix call with C-u) inserts timestamp.
  Two universal arsg (e.g., prefix call with C-u C-u) prompts for date
  then insertes active date."
  ;; Insert an active timestamp, with a few options.
  (interactive "P")
  (let ((prefix (car arg)))
    (cond
     ((not prefix)  (org-insert-time-stamp nil nil nil))
     ((= prefix 4)  (org-insert-time-stamp nil t nil))
     ((= prefix 16) (org-insert-time-stamp (org-read-date nil t nil "Date")
					   nil nil)))))

(global-set-key (kbd "C-w") 'jf/delete-region-or-backward-word)
(global-set-key (kbd "M-DEL") 'jf/delete-region-or-backward-word)
(global-set-key (kbd "<C-M-backspace>") 'backward-kill-paragraph)
(defun jf/delete-region-or-backward-word (&optional arg)
  "Delete selected region otherwise delete backwards the ARG number of words."
  (interactive "p")
  (if (region-active-p)
      (delete-region (region-beginning) (region-end))
    (jf/delete-word (- arg))))

(defun jf/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

(defun jf/auto-create-missing-dirs ()
  "Ensure that we create directories along the new path."
  ;; Ensure that we create the directories along the path of a new file I’m
  ;; creating.  See
  ;; https://emacsredux.com/blog/2022/06/12/auto-create-missing-directories/
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))

(add-to-list 'find-file-not-found-functions #'jf/auto-create-missing-dirs)

(use-package run-command
  :straight t
  :config
  (defun jf/run-command-recipes ()
    "Run command recipes"
    (list
     (let ((dir (projectile-project-root)))
       (when (f-exists? (f-join (projectile-project-root) "Gemfile.lock"))
	 (list :command-name "run-command-samvera-versions"
	       :command-line (format "cd %s; rg \"^ +((bulk|hy)rax|\\(*.\\)iiif\\(*.\\)|rails|qa|blacklight(-.*)?) \\(\\d+\\.\\d+\\.\\d+\" Gemfile.lock" dir)
	       :display (format "Samvera gem versions for %s" dir))))
     (list :command-name "run-command-takeonrules-server"
	   :command-line "cd ~/git/takeonrules.source/; bin/rake knowledge_manager:pull"
	   :display "Serve takeonrules.com locally")))
  (add-to-list 'run-command-recipes 'jf/run-command-recipes))

(provide 'jf-utility)
;;; jf-utility.el ends here
