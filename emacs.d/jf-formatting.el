;;; jf-formatting.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary

;; Whitespace hygene package.  The author's documentation and commentary echoes
;; my sentiments.
(use-package ethan-wspace
  :straight t
  :hook (before-save . delete-trailing-whitespace)
  :init (setq-default mode-require-final-newline nil)
  :config (global-ethan-wspace-mode 1))

(use-package tomelr
  :straight (tomelr :host github :repo "kaushalmodi/tomelr"))

(use-package htmlize
  :straight t
  :bind ("C-M-s-c" . jf/formatted-copy-org-to-html)
  :config
  ;; The following functions build on both org and the htmlize package.  I
  ;; define them as part of the config because without the package these won't
  ;; work.
  ;;
  ;; For this to work, I needed to permit my \"~/bin/emacsclient\" in the Security
  ;; & Privacy > Accessibility system preference.
  ;;
  ;; http://mbork.pl/2021-05-02_Org-mode_to_Markdown_via_the_clipboard
  (defun jf/org-copy-region-as-markdown ()
    "Copy the region (in Org) to the system clipboard as Markdown."
    (interactive)
    (require 'ox)
    (if (use-region-p)
	(let* ((region
		(buffer-substring-no-properties
		 (region-beginning)
		 (region-end)))
	       (markdown
		(org-export-string-as region 'md t '(:with-toc nil))))
	  (gui-set-selection 'CLIPBOARD markdown))))

  ;; I have found that Slack resists posting rich content, so I often need to open up TextEdit, paste into an empty file, copy the contents, and then paste into Slack.
  (defun jf/formatted-copy-org-to-html (prefix)
    "Export region to HTML, and copy it to the clipboard.

When given the PREFIX arg, paste the content into TextEdit (for future copy)."
    (interactive "P")
    (save-window-excursion
      (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
	     (html (with-current-buffer buf (buffer-string))))
	(with-current-buffer buf
	  (shell-command-on-region
	   (point-min)
	   (point-max)
	   "textutil -inputencoding UTF-8 -stdout -stdin -format html -convert rtf | pbcopy"))
	(kill-buffer buf)
	;; Paste into TextEdit
	(when (car prefix)
	  (ns-do-applescript
	   (concat
	    "tell application \"TextEdit\"\n"
	    "\tactivate\n"
	    "\tset myrtf to the clipboard as «class RTF »\n"
	    "\tset mydoc to make new document\n"
	    "\tset text of mydoc to myrtf\n"
	    "end tell")))
	))))

;; `fill-sentences-correctly-mode' ensures that `fill-paragraph' (e.g. M-q)
;; preserves two spaces.
(use-package fill-sentences-correctly
  :straight (fill-sentences-correctly :host github :repo "duckwork/fill-sentences-correctly.el")
  :hook (fundamental-mode . fill-sentences-correctly-mode))


(provide 'jf-formatting)
;;; jf-formatting.el ends here
