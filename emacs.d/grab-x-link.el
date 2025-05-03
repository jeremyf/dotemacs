;;; grab-x-link.el --- Grab links from X11 apps and insert into Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2018  Xu Chunyang
;; Copyright (C) 2025 Jeremy Friesen

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Contributor: Jeremy Friesen <git@jeremyfriesen.com>
;; URL: https://github.com/jeremyf/grab-x-link
;; Package-Requires: ((emacs "28"))
;; Keywords: hyperlink
;; Version: 0.5

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Grab link and title from Firefox and Chromium, insert into Emacs buffer as
;; plain, markdown or org link.
;;
;; To use, invoke `M-x grab-x-link' and other commands provided by this package.
;;
;; Prerequisite:
;; - xdotool(1)
;; - xsel(1) or xclip(1) if you are running Emacs inside a terminal emulator
;;
;; Changes:
;; - 2025-04-28 v0.7 Refactor for Firefox-based browsers
;; - 2024-12-24 v0.6 Support Vivaldi
;; - 2018-02-05 v0.5 Support Google Chrome
;; - 2016-12-01 v0.4 Handle case that app is not running
;; - 2016-12-01 v0.3 Add the command `grab-x-link'
;; - 2016-11-19 v0.2 Rename grab-x11-link to grab-x-link
;; - 2016-11-19 v0.1 Support Emacs running inside terminal emulator

;;; Code:

(declare-function org-make-link-string "org" (link &optional description))

(defun grab-x-link--shell-command-to-string (command)
  (with-temp-buffer
    (if (and (zerop (call-process-shell-command command nil t))
             (> (buffer-size) 0))
        (substring (buffer-string) 0 -1)
      nil)))

(defun grab-x-link--build (url-title &optional type)
  "Build plain or markdown or org link."
  (let ((url (car url-title))
        (title (cdr url-title)))
    (pcase type
      ('org  (progn (require 'org)
                    (org-make-link-string url title)))
      ('markdown (format "[%s](%s)" title url))
      (_ (format "[%s](%s)" title url)))))

(defun grab-x-link--title-strip (string suffix)
  "Remove SUFFIX from STRING."
  (cond
    ((< (length string) (length suffix))
      string)
    ((string= (substring string (- (length string) (length suffix))) suffix)
      (substring string 0 (- (length suffix))))
    (t string)))

(defun grab-x-link--get-clipboard ()
  "Get the contents of the clipboard.

Favor native `gui-get-selection' but fallback to xsel and xclip."
  (cond
    ((and (display-graphic-p) (fboundp 'gui-get-selection))
      (gui-get-selection))
    ((executable-find "xsel")
      (grab-x-link--shell-command-to-string
        "xsel --clipboard"))
    ((executable-find "xclip")
      (grab-x-link--shell-command-to-string
        "xclip -selection clipboard -o"))
    (_
      (error "Can't get clipboard, see `grab-x-link--get-clipboard' for details"))))

(defvar grab-x-link-apps
  nil
  "A plist of registered apps for `grab-x-link'.")

(defun grab-x-link (app &optional link-type apps)
  "Prompt for an application to grab a link from.
When done, go gtab the link, and insert it at point.

If called from Lisp, grab link APP and return it (as a string) in
LINK-TYPE.  APP is a symbol and must be one of '(chromium
firefox), LINK-TYPE is also a symbol and must be one of '(plain
markdown org), if LINK-TYPE is omitted or nil, plain link will be used."
  (interactive
    (let ((apps
            (mapcar (lambda (app)
                      (cons
                        (plist-get app :menu-key)
                        (plist-get app :name)))
              grab-x-link-apps))
           (link-types
             '((?p . plain)
                (?m . markdown)
                (?o . org)))
           (propertize-menu
             (lambda (string)
               "Propertize substring between [] in STRING."
               (with-temp-buffer
                 (insert string)
                 (goto-char 1)
                 (while (re-search-forward "\\[\\(.+?\\)\\]" nil 'no-error)
                   (replace-match (format "[%s]" (propertize (match-string 1) 'face 'bold))))
                 (buffer-string))))
           input app link-type)

      (message (funcall propertize-menu
                 (concat "Grab link from "
                   (mapconcat (lambda (app)
                                (plist-get app :menu-label))
                     grab-x-link-apps " ")
                   ":")))
      (setq input (read-char-exclusive))
      (setq app (cdr (assq input apps)))

      (message (funcall propertize-menu
                 (format "Grab link from %s as a [p]lain [m]arkdown [o]rg link:" app)))
      (setq input (read-char-exclusive))
      (setq link-type (cdr (assq input link-types)))
      (list app link-type apps)))

  (unless link-type
    (setq link-type 'plain))

  (unless (and (memq app (mapcar (lambda (cell) (cdr cell)) apps))
               (memq link-type '(plain org markdown)))
    (error "Unknown app %s or link-type %s" app link-type))

  (let ((link (grab-x-link--build
               (funcall (intern (format "grab-x-link-%s" app)))
               link-type)))
    (and (called-interactively-p 'any) (insert link))
    link))

(cl-defmacro grab-x-link:register-app (&key menu-key menu-label classname name key suffix)
  "Leverage xdotool to copy the URL and title from the found window.

We intersect the name and classname in xdotool.

MENU-KEY: when choosing what to grab, this is the key to select the app.
MENU-LABEL: label for the grab menu app name.
NAME: the xdotool --name parameter
CLASSNAME: the xdotool --classname parameter
KEY: The key sequence to send to the application.
SUFFIX: The suffix to strip from the returned title.
"
  (let ((fn
          (intern (format "grab-x-link-%s" (downcase name)))))
    (add-to-list 'grab-x-link-apps
      (list :menu-key menu-key :menu-label menu-label :name (intern (downcase name))))
    `(defun ,fn ()
       (let ((emacs-window
               (grab-x-link--shell-command-to-string
                 "xdotool getactivewindow"))
              (app-window
                (or
                  (grab-x-link--shell-command-to-string
                    (format "comm -12 <(xdotool search --name \"%s\" | sort) <(xdotool search --classname \"%s\" | sort)" ,name ,classname))
                  (error "Can't find %s Window -- is it running?" ,name))))
         (shell-command
           (format "xdotool windowactivate --sync %s key %s" app-window ,key))
         (shell-command
           (format "xdotool windowactivate %s" emacs-window))
         (sit-for 0.2)
         (let ((url (substring-no-properties (grab-x-link--get-clipboard)))
                (title (grab-x-link--title-strip
                         (grab-x-link--shell-command-to-string
                           (concat "xdotool getwindowname " app-window))
                         ,suffix)))
           (cons url title))))))

(when (executable-find "firefox")
  (grab-x-link:register-app
    :menu-key ?f
    :menu-label "[f]irefox"
    :name "Firefox"
    :classname "Navigator"
    :key "ctrl+l Escape ctrl+l ctrl+c Escape"
    :suffix " - Mozilla Firefox"))

(when (executable-find "librewolf")
  (grab-x-link:register-app
    :menu-key ?l
    :menu-label "[l]ibrewolf"
    :name "Librewolf"
    :classname "Navigator"
    :key "ctrl+l Escape ctrl+l ctrl+c Escape"
    :suffix " — LibreWolf"))

(when (executable-find "mullvad-browser")
  (grab-x-link:register-app
    :menu-key ?m
    :menu-label "[m]ullvad"
    :name "Mullvad"
    :classname "Navigator"
    :key "ctrl+l Escape ctrl+l ctrl+c Escape"
    :suffix " - Mullvad Browser"))

(bind-key "C-c g" 'grab-x-link)

(provide 'grab-x-link)
;;; grab-x-link.el ends here
