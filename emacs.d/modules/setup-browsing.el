;;; setup-browsing --- We shall browse the internet -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;;; Code:

(use-package eww
  ;; A plain text browser.  Use this to see just how bad much of the web
  ;; has become.
  :straight t
  :custom (eww-auto-rename-buffer 'title)
  :config
  (setopt shr-cookie-policy nil)
  (defun shr-tag-dfn (dom)
    (shr-generic dom))

  (defun shr-tag-cite (dom)
    (shr-generic dom))

  (defun shr-tag-q (dom)
    (shr-insert (car shr-around-q-tag))
    (shr-generic dom)
    (shr-insert (cdr shr-around-q-tag)))

  (defcustom shr-around-q-tag '("“" . "”")
    "The before and after quotes.

`car' is inserted before the Q-tag and `cdr' is inserted after
the Q-tag.

Alternative suggestions are: - '(\"\\\"“\" . \"\\\"\")"
    :type (cons 'string 'string))

  (defface shr-small
    '((t :height 0.8))
    "Face for <small> elements.")

  ;; Drawing inspiration from shr-tag-h1
  (defun shr-tag-small (dom)
    (shr-fontize-dom dom (when shr-use-fonts 'shr-small)))

  (defface shr-time
    '((t :inherit underline :underline (:style wave)))
    "Face for <time> elements.")

  ;; Drawing inspiration from shr-tag-abbr
  (defun shr-tag-time (dom)
    (when-let* ((datetime
                  (or
                    (dom-attr dom 'title)
                    (dom-attr dom 'datetime)))
                 (start
                   (point)))
      (shr-generic dom)
      (shr-add-font start (point) 'shr-time)
      (add-text-properties
        start (point)
        (list
          'help-echo datetime
          'mouse-face 'highlight))))

  (defmacro shr-display-block (tag &optional face)
    "Register TAG a paragraph (in CSS parlance \"display:block;\").

See https://developer.mozilla.org/en-US/docs/Glossary/Block-level_content"
    (let ((fname
            (intern (format "shr-tag-%s" tag)))
           (docstring
             (format "Render \"%s\" tag as paragraph." tag)))
      `(defun ,fname (dom)
         ,docstring
         (shr-ensure-paragraph)
         (shr-generic dom)
         (shr-ensure-paragraph))))

  (shr-display-block "article")
  (shr-display-block "aside")
  (shr-display-block "footer")
  (shr-display-block "header")
  (shr-display-block "nav")
  (shr-display-block "section")

  (defmacro jf/shr-facify (tag)
    "Create and apply the face to the given TAG."
    (let ((face
            (intern (concat "shr-" tag)))
           (docstring-face
             (format "Face for <%s> elements." tag)))
      `(progn
         (defface ,face
           '((default :inherit shr-text))
           ,docstring-face)
         (advice-add (intern (concat "shr-tag-" ,tag))
           :around
           (lambda (advised-function func &rest args)
             (let ((start (point)))
               (apply advised-function func args)
               (shr-add-font start (point) (intern (concat "shr-" ,tag)))))))))

  (jf/shr-facify "blockquote")
  (jf/shr-facify "aside")
  (jf/shr-facify "cite")
  (jf/shr-facify "dfn")
  (jf/shr-facify "dt")

  (defun eww-first-url ()
    "Go to the page marked `first'.
A page is marked `first' if rel=\"first\" appears in a <link> or <a> tag."
    (interactive nil eww-mode)
    (let ((best-url
            (plist-get eww-data :first)))
      (if best-url
        (eww-browse-url (shr-expand-url best-url (plist-get eww-data :url)))
        (user-error "No `first' for this page"))))

  (defun eww-last-url ()
    "Go to the page marked `last'.
A page is marked `last' if rel=\"last\" appears in a <link> or <a> tag."
    (interactive nil eww-mode)
    (let ((best-url
            (plist-get eww-data :last)))
      (if best-url
        (eww-browse-url (shr-expand-url best-url (plist-get eww-data :url)))
        (user-error "No `last' for this page"))))

  ;; Favor librewolf as default firefox browser, failing that mullvad.
  (if (executable-find "librewolf")
    (setopt browse-url-firefox-program "librewolf")
    (when (executable-find "mullvad-browser")
      (setopt browse-url-firefox-program "mullvad-browser")))

  (defun jf/reader-visual ()
    ;; A little bit of RSS beautification.
    "A method to turn on visual line mode and adjust text scale."
    (require 'olivetti)
    (olivetti-mode 1)
    (text-scale-set 2))

  (unbind-key "u" shr-map)
  :bind (:map eww-mode-map
          ("u" . eww-up-url)
          ("M-<left>" . eww-first-url)
          ("M-<right>" . eww-last-url)
          ("h" . eww-home-url))
  :hook ((eww-mode . jf/reader-visual)))

(when (f-dir-p "~/git/kagi-search.el")
  ;; https://git.andros.dev/andros/kagi-search.el
  (use-package kagi-search
    :load-path "~/git/kagi-search.el"
    :config
    ;; Explicitly use authsource
    (setq kagi-search-token nil)
    (kagi-search-eww-mode 1)))
(provide 'setup-browsing)
;;; setup-browsing.el ends here
