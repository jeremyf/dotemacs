;;; -*- lexical-binding: t; -*-
;;; jnf-elfeed.el --- Summary
;;
;;; Commentary:
;;
;;  This package includes the configuration for elfeed, an Emacs RSS
;;  reader.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package elfeed
  :straight t
  :after org
  :config
  (setq-default elfeed-search-filter "@2-days-ago +unread ")
  (defun jnf/amplify-elfeed ()
    "Amplify the current `elfeed-show-entry'"
    (interactive)
    (let* ((citeURL (elfeed-entry-link elfeed-show-entry))
           (citeTitle (elfeed-entry-title elfeed-show-entry))
           ;; Grab the author from the elfeed metadata.  Making the
           ;; assumption that there's only one.
           (citeAuthor
            (plist-get
             (car (plist-get
                   (elfeed-entry-meta elfeed-show-entry)
                   :authors))
             :name)))
      (tor-post-amplifying-the-blogosphere citeTitle
                                           :citeTitle citeTitle
                                           :citeURL citeURL
                                           :citeAuthor citeAuthor)))
  :bind (
         (:map elfeed-search-mode-map
               ("q" . jnf/elfeed-save-db-and-bury))
         (:map elfeed-show-mode-map
               ("<f7>" . jnf/amplify-elfeed)
               ("s-7" . jnf/amplify-elfeed))))

(use-package eww
  :straight t
  :config
  (defun jnf/amplify-eww ()
    "Amplify the current `eww-data'"
    (interactive)
    (let* ((citeURL (plist-get eww-data :url))
           (citeTitle (plist-get eww-data :title)))
      (tor-post-amplifying-the-blogosphere citeTitle
                                           :citeTitle citeTitle
                                           :citeURL citeURL)))
  :bind (:map eww-mode-map
              ("U" . eww-up-url)
              ("<f7>" . jnf/amplify-eww)
              ("s-7" . jnf/amplify-eww))
  :hook ((eww-mode . jnf/reader-visual)))


;; A little bit of RSS beautification
(add-hook 'elfeed-show-mode-hook 'jnf/reader-visual)
(defun jnf/reader-visual ()
  "A method to turn on visual line mode and adjust text scale."
  (text-scale-set 2)
  (turn-on-visual-line-mode))

;;write to disk when quiting
(defun jnf/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(defun jnf/elfeed-load-db-and-open ()
  "Load the elfeed db from disk before opening"
  (interactive)
  (elfeed)
  (elfeed-update)
  (elfeed-db-load)
  (elfeed-search-update--force))
(defalias 'rss 'jnf/elfeed-load-db-and-open)

(use-package elfeed-org
  :straight t
  :after elfeed
  :config (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/git/org/personal/elfeed.org")))


;; https://github.com/alphapapa/unpackaged.el#feed-for-url
;;;###autoload
(cl-defun unpackaged/feed-for-url (url &key (prefer 'atom) (all nil))
  "Return feed URL for web page at URL.
Interactively, insert the URL at point.  PREFER may be
`atom' (the default) or `rss'.  When ALL is non-nil, return all
feed URLs of all types; otherwise, return only one feed URL,
preferring the preferred type."
  (interactive (list (org-web-tools--get-first-url)))
  (require 'esxml-query)
  (require 'org-web-tools)
  (cl-flet ((feed-p (type)
                    ;; Return t if TYPE appears to be an RSS/ATOM feed
                    (string-match-p (rx "application/" (or "rss" "atom") "+xml")
                                    type)))
    (let* ((preferred-type (format "application/%s+xml" (symbol-name prefer)))
           (html (org-web-tools--get-url url))
           (dom (with-temp-buffer
                  (insert html)
                  (libxml-parse-html-region (point-min) (point-max))))
           (potential-feeds (esxml-query-all "link[rel=alternate]" dom))
           (return (if all
                       ;; Return all URLs
                       (cl-loop for (_tag attrs) in potential-feeds
                                when (feed-p (alist-get 'type attrs))
                                collect (url-expand-file-name (alist-get 'href attrs) url))
                     (or
                      ;; Return the first URL of preferred type
                      (cl-loop for (_tag attrs) in potential-feeds
                               when (equal preferred-type (alist-get 'type attrs))
                               return (url-expand-file-name (alist-get 'href attrs) url))
                      ;; Return the first URL of non-preferred type
                      (cl-loop for (_tag attrs) in potential-feeds
                               when (feed-p (alist-get 'type attrs))
                               return (url-expand-file-name (alist-get 'href attrs) url))))))
      (if (called-interactively-p 'interactive)
          (insert (if (listp return)
                      (s-join " " return)
                    return))
        return))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From https://karthinks.com/blog/lazy-elfeed/
(defun elfeed-search-show-entry-pre (&optional lines)
  "Returns a function to scroll forward or back in the Elfeed
  search results, displaying entries without switching to them."
  (lambda (times)
    (interactive "p")
    (forward-line (* times (or lines 0)))
    (recenter)
    (call-interactively #'elfeed-search-show-entry)
    (select-window (previous-window))
    (unless elfeed-search-remain-on-entry (forward-line -1))))
(eval-after-load 'elfeed-search
  '(define-key elfeed-search-mode-map (kbd "n") (elfeed-search-show-entry-pre +1)))
(eval-after-load 'elfeed-search
  '(define-key elfeed-search-mode-map (kbd "p") (elfeed-search-show-entry-pre -1)))
(eval-after-load 'elfeed-search
  '(define-key elfeed-search-mode-map (kbd "M-RET") (elfeed-search-show-entry-pre)))
;; End https://karthinks.com/blog/lazy-elfeed/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'jnf-elfeed.el)
;;; jnf-elfeed.el ends here
