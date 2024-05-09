;;; jf-utility.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;; Packages specifically here for helping with my coding activities.

;;; Code:

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

(provide 'jf-utility)
;;; jf-utility.el ends here
