;;; jf-utility.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;; Packages specifically here for helping with my coding activities.

;;; Code:

(use-package xref
  :straight t
  :custom
  (xref-file-name-display 'project-relative)
  (xref-search-program 'ripgrep))

(use-package tmr
  ;; A timer package.
  ;;
  ;; My dbus install is not behaving so I'm cheating with a bit of AppleScript
  :preface
  (defun jf/tmr-notification-notify (timer)
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
  (tmr-timer-finished-functions
    (list #'tmr-print-message-for-completed-timer #'tmr-sound-play #'jf/tmr-notification-notify) nil nil "Customized with use-package tmr")
  :straight (:host github :type git
              :repo "protesilaos/tmr"))

(use-package transient
  ;; A package for creating turbo-charged menus.  It is the backbone for the
  ;; menu-like dispatch of `magit' functionality.
  :straight t)

(use-package ts
  ;; Timestamp library (not typescript)
  :straight t)

;;; Support packages

(use-package keychain-environment
  ;; Help me manage my secrets via the OS
  ;; Load keychain environment
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

(use-package wgrep
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
  :after (embark-consult ripgrep)
  :config (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  :straight t
  :bind (:map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          :map ripgrep-search-mode-map
          ("e" . wgrep-change-to-wgrep-mode)))

(use-package rg
  ;; A highly performant successor to the venerable grep.
  :after (wgrep)
  :custom (rg-keymap-prefix (kbd "C-c f"))
  :config (rg-enable-menu)
  ;; https://github.com/dajva/rg.el/issues/142 Give focus to *rg* buffer.
  (add-to-list 'rg-finish-functions (lambda (buffer _) (pop-to-buffer buffer)))

  ;; Override the baseline rg-project to include files
  (rg-define-search rg-project
    :dir project
    :files "*.*")

  ;; Prompt for file types
  (rg-define-search rg-project-prompt-for-files
    :dir project
    :menu ("Search" "P" "Project prompt file type"))

  ;; Remember to keep these alphabetized
  (when (f-dir-p "~/git/blacklight/")
    (rg-define-search rg-projects-blacklight
      "Search Blacklight"
      :dir "~/git/blacklight/"
      :files "*.*"
      :menu ("Projects" "j b" "Blacklight")))

  (when (f-dir-p "~/git/bulkrax/")
    (rg-define-search rg-projects-bulkrax
      "Search Bulkrax"
      :dir "~/git/bulkrax/"
      :files "*.*"
      :menu ("Projects" "j B" "Bulkrax")))

  (when (f-dir-p "~/git/dotemacs/")
    (rg-define-search rg-projects-dotemacs
      "Search Dotemacs"
      :dir "~/git/dotemacs/"
      :files "*.*"
      :menu ("Projects" "j d" "Dotemacs")))

  (when (f-dir-p "~/git/hyku/")
    (rg-define-search rg-projects-hyku
      "Search Hyku"
      :dir "~/git/hyku/"
      :files "*.*"
      :menu ("Projects" "j h" "Hyku")))

  (when (f-dir-p "~/git/hyrax/")
    (rg-define-search rg-projects-hyrax
      "Search Hyrax"
      :dir "~/git/hyrax/"
      :files "*.*"
      :menu ("Projects" "j H" "Hyrax")))

  (when (f-dir-p "~/git/iiif_print/")
    (rg-define-search rg-projects-iiif_print
      "Search IIIF Print"
      :dir "~/git/iiif_print/"
      :files "*.*"
      :menu ("Projects" "j i" "IIIF Print")))

  (when (f-dir-p "~/git/valkyrie/")
    (rg-define-search rg-projects-valkyrie
      "Search Valkyrie"
      :dir "~/git/valkyrie/"
      :files "*.*"
      :menu ("Projects" "j v" "Valkyrie")))
  :init (setq ripgrep-arguments "--ignore-case")
  :straight t)

(use-package visual-regexp
  ;; I haven't used much search and replace but the visual queues are useful.
  ;; And I learned about ,\ in this play.  ,\(upcase \1) will upcase the first
  ;; capture region.
  :straight t
  :bind ("C-c C-r r" . vr/replace)
  ("C-c C-r m" . vr/mc-mark)
  ("C-c C-r q" . vr/query-replace)
  ("C-c C-r p" . project-query-replace-regexp))

;; https://github.com/hokomo/query-replace-parallel
;; Presented at https://pad.emacsconf.org/2023-parallel
;; (use-package query-replace-parallel
;;   :straight (:host github :repo "hokomo/query-replace-parallel")
;;   :commands (query-replace-parallel query-replace-parallel-regexp))

(use-package crux
  ;; A mix of a few odd and useful functions.
  :straight t
  :bind (("C-a" . crux-move-beginning-of-line)
          ("<C-s-return>" . crux-smart-open-line-above)
          ("C-s-k" . crux-kill-line-backwards)
          ("<s-backspace>" . crux-kill-line-backwards)
          ("<f9>" . crux-kill-other-buffers)))

(use-package math-at-point
  ;; Sometimes you just want to do math
  :straight (math-at-point :type git :host github
              :repo "shankar2k/math-at-point")
  :bind ("C-c =" . math-at-point))

;;;; Hammerspoon --------------------------------------------------------------

;; Hammerspoon is a Lua application that provides a consistent API for
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
  "Set the hammerspoon mode."
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
  "When region is active, evaluate it and kill the mark.

Else, evaluate the whole buffer."
  (interactive)
  (if (not (region-active-p))
    (progn
      (message "Evaluating buffer...")
      (eval-buffer))
    (progn
      (message "Evaluating region...")
      (eval-region (region-beginning) (region-end)))
    (setq-local deactivate-mark t)))

(keymap-global-set "C-k" 'jf/kill-line-or-region)
(defun jf/kill-line-or-region (&optional arg)
  "Kill the selected region otherwise kill the ARG number of lines."
  (interactive "P")
  (if (use-region-p)
    (kill-region (region-beginning) (region-end))
    (kill-line arg)))

(keymap-global-set "C-c n" 'jf/yank-file-name-to-clipboard) ;; Deprecated
(keymap-global-set "C-c y n" 'jf/yank-file-name-to-clipboard)
(defun jf/yank-file-name-to-clipboard (arg)
  "Nab, I mean copy, the current buffer file name to the clipboard.

  When you pass one universal prefix ARG, nab the project relative filename.
  When you pass two or more prompt for different aspects of a file."
  ;; https://blog.sumtypeofway.com/posts/emacs-config.html
  (interactive "P")
  (let* ((prefix (car arg))
          (raw-filename
            (if (equal major-mode 'dired-mode)
              default-directory
              (buffer-file-name)))
          (filename
            (cond
              ((not prefix) raw-filename)
              ((= prefix 4) (concat "./" (file-relative-name raw-filename (projectile-project-root))))
              ((>= prefix 16)
                (let ((options '(("Filename, Basename" .
                       (lambda (f) (file-name-nondirectory f)))
                      ("Filename, Project Relative" .
                        (lambda (f) (concat "./" (file-relative-name f (projectile-project-root)))))
                      ("Filename, Full" .
                        (lambda (f) (f)))
                      ("Dirname" .
                        (lambda (f) (file-name-directory f)))
                      ("Dirname, Project Relative" .
                        (lambda (f) (concat "./" (file-relative-name (file-name-directory f) (projectile-project-root))))))))
                  (funcall (alist-get (completing-read "Option: " options nil t)
                                       options nil nil #'string=)
                    raw-filename))))))
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
(keymap-global-set "C-M-d" 'jf/duplicate-current-line-or-lines-of-region)
(keymap-global-set "C-c d" 'jf/duplicate-current-line-or-lines-of-region)
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
(keymap-global-set "<f12>" 'jf/create-scratch-buffer)
(cl-defun jf/create-scratch-buffer (&optional arg)
  "Create a `scratch' buffer; if ARG given create a `denote' scratch note."
  (interactive "P")
  (if (car arg)
    (jf/denote/create-scratch (format-time-string "%Y-%m-%d Scratch"))
    (progn
      (crux-create-scratch-buffer)
      (rename-buffer (concat "*scratch* at " (format-time-string "%Y-%m-%d %H:%M")))
      (org-mode))))

;; Sometimes I want to move, without renaming, a file.  This function helps
;; make that easy.
(keymap-global-set "C-x m" 'jf/move-file)
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

(keymap-global-set "s-5" 'jf/org-insert-immediate-active-timestamp)
(defun jf/org-insert-immediate-active-timestamp (arg)
  "Insert an active date for today.

  One universal ARG prompts for date
  Two universal ARG inserts timestamp.
  then insertes active date."
  ;; Insert an active timestamp, with a few options.
  (interactive "P")
  (let ((prefix (car arg)))
    (cond
      ((not prefix)  (org-insert-time-stamp nil nil nil))
      ((= prefix 4) (org-insert-time-stamp (org-read-date nil t nil "Date")
                      nil nil))
      ((>= prefix 16)  (org-insert-time-stamp nil t nil)))))

(keymap-global-set "C-w" 'jf/delete-region-or-backward-word)
(keymap-global-set "M-DEL" 'jf/delete-region-or-backward-word)
(keymap-global-set "C-M-<backspace>" 'backward-kill-paragraph)
(defun jf/delete-region-or-backward-word (&optional arg)
  "Delete selected region otherwise delete backwards the ARG number of words."
  (interactive "p")
  (if (region-active-p)
    (delete-region (region-beginning) (region-end))
    (jf/delete-word (- arg))))

(defun jf/delete-word (arg)
  "Delete characters forward until encountering the end of a word.

With ARG, do this that many times."
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

(defun jf/filename/tilde-based (filename)
  "Return ~/ relative FILENAME."
  (string-replace (getenv "HOME") "~"
    (if (consp filename) (cadr filename) filename)))

(defun jf/utility/maybe-url-domain-from-string (string)
  "Return domain from STRING when URL scheme present.

Else fallback to provided STRING"
  (require 's)
  (if (s-contains? "://" string)
      (s-join "."
              (cl-subseq
               (s-split "\\." (nth 2 (s-split "/" string))) -2))
    string))

(provide 'jf-utility)
;;; jf-utility.el ends here
