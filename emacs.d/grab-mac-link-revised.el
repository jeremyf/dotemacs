(use-package grab-mac-link
  ;; Grab a link from a variety of MacOS applications.
  :straight (:host github :repo "stevenbagley/grab-mac-link.el")
  ;; Ensuring we load these, as I'll need them later.
  :commands (grab-mac-link-safari-1 grab-mac-link-firefox-1 grab-mac-link-split)
  :config
  ;; A replacement function for existing grab-mac-link-make-html-link
  (defun jf/grab-mac-link-make-html-link (url name)
    "Using HTML syntax, link to and cite the URL with the NAME."
    (format (concat "<cite>"
              "<a href=\"%s\" class=\"u-url p-name\" rel=\"cite\">"
              "%s"
              "</a>"
              "</cite>")
      url name))
  ;; The function advice to override the default behavior
  (advice-add 'grab-mac-link-make-html-link
    :override 'jf/grab-mac-link-make-html-link
    '((name . "jnf")))
  :bind (("C-c g" . grab-mac-link-revised)))

(defmacro grab-mac-link-register-firefox-fork-func (code macos_app_name)
  "Create a `grab-mac-link' function for MACOS_APP_NAME.

By convention the CODE is used to generate the function we'll dispatch
to fetch the link from the MACOS_APP_NAME.

This macro should work for any Firefox-based application."
  (let ((fn
          (intern (format "grab-mac-link-%s-1" code))))
    `(defun ,fn ()
       (let ((result
               (do-applescript
                 (concat
                   "set oldClipboard to the clipboard\n"
                   "set frontmostApplication to path to frontmost application\n"
                   "tell application \"" ,macos_app_name "\"\n"
                   " 	activate\n"
                   " 	delay 0.15\n"
                   " 	tell application \"System Events\"\n"
                   " 	 	keystroke \"l\" using {command down}\n"
                   " 	 	keystroke \"a\" using {command down}\n"
                   " 	 	keystroke \"c\" using {command down}\n"
                   " 	end tell\n"
                   " 	delay 0.15\n"
                   " 	set theUrl to the clipboard\n"
                   " 	set the clipboard to oldClipboard\n"
                   " 	set theResult to (get theUrl) & \"::split::\" & (get name of window 1)\n"
                   "end tell\n"
                   "activate application (frontmostApplication as text)\n"
                   "set links to {}\n"
                   "copy theResult to the end of links\n"
                   "return links as string\n"))))
         (grab-mac-link-split
           (car (split-string result "[\r\n]+" t)))))))

;; TODO: Create data structure for registering an app.
(defvar grab-mac-link-revised-apps
  '((?c . chrome)
     (?f . firefox)
     (?l . librewolf)
     (?m . mullvad)
     (?s . safari))
  "A map of keys and application codes for `grab-mac-link-revised'.

The `car' is the key, the `cdr' is the code.")

(grab-mac-link-register-firefox-fork-func mullvad "Mullvad Browser")
(grab-mac-link-register-firefox-fork-func librewolf "LibreWolf")

;;;###autoload
(defun grab-mac-link-revised (app &optional link-type)
  "Prompt for an application to grab a link from.
  When done, go grab the link, and insert it at point.

  With a prefix argument, instead of \"insert\", save it to
  kill-ring. For org link, save it to `org-stored-links', then
  later you can insert it via `org-insert-link'.

  If called from lisp, grab link from APP and return it (as a
 	 	 	 	 	 	 	    string) with LINK-TYPE.  APP is a symbol and must be one of
  '(chrome safari finder mail terminal), LINK-TYPE is also a symbol
  and must be one of '(plain markdown org), if LINK-TYPE is omitted
  or nil, plain link will be used."
  (interactive
    (let ((apps
            grab-mac-link-revised-apps)
           (link-types
             '((?p . plain)
                (?m . markdown)
                (?o . org)
                (?h . html)))
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
      (let ((message-log-max nil))
        (message (funcall propertize-menu
                   (format "Grab link from: %s"
                     (mapconcat (lambda (data)
                                  (message "%S" (cdr data))
                                  (replace-regexp-in-string
                                    (format "\\(%s\\).*$" (byte-to-string (car data)))
                                    (format "[%s]" (byte-to-string (car data)))
                                    (format "%s" (cdr data))
                                    nil nil 1))
                       grab-mac-link-revised-apps
                       " ")))))
      (setq input (read-char-exclusive))
      (setq app (cdr (assq input grab-mac-link-revised-apps)))
      (let ((message-log-max nil))
        (message (funcall propertize-menu
                   (format "Grab link from %s as a [p]lain [m]arkdown [o]rg [h]tml link:" app))))
      (setq input (read-char-exclusive))
      (setq link-type (cdr (assq input link-types)))
      (list app link-type)))

  (setq link-type (or link-type 'plain))
  (unless (and (memq app (mapcar (lambda (data) (cdr data)) grab-mac-link-revised-apps))
            (memq link-type '(plain org markdown html)))
    (error "Unknown app %s or link-type %s" app link-type))
  (let* ((grab-link-func (intern (format "grab-mac-link-%s-1" app)))
          (make-link-func (intern (format "grab-mac-link-make-%s-link" link-type)))
          (link (apply make-link-func (funcall grab-link-func))))
    (when (called-interactively-p 'any)
      (if current-prefix-arg
        (if (eq link-type 'org)
          (let* ((res (funcall grab-link-func))
                  (link (car res))
                  (desc (cadr res)))
            (push (list link desc) org-stored-links)
            (message "Stored: %s" desc))
          (kill-new link)
          (message "Copied: %s" link))
        (insert link)))
    link))
