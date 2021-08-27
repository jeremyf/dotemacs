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
  (defun shr-tag-dfn (dom)
    (shr-fontize-dom dom 'italic))

  (defun shr-tag-cite (dom)
    (shr-fontize-dom dom 'italic))

  (defun shr-tag-q (dom)
    (shr-insert (car shr-around-q-tag))
    (shr-generic dom)
    (shr-insert (cdr shr-around-q-tag)))

  (defcustom shr-around-q-tag '("“" . "”")
    "The before and after quotes.  `car' is inserted before the Q-tag and `cdr' is inserted after the Q-tag.

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
    (when-let* ((datetime (or
                           (dom-attr dom 'title)
                           (dom-attr dom 'datetime)))
	        (start (point)))
      (shr-generic dom)
      (shr-add-font start (point) 'shr-time)
      (add-text-properties
       start (point)
       (list
        'help-echo datetime
        'mouse-face 'highlight))))


  ;; EWW lacks a style for article
  (defun shr-tag-article (dom)
    (shr-ensure-paragraph)
    (shr-generic dom)
    (shr-ensure-paragraph))

  ;; EWW lacks a style for section; This is quite provisional
  (defun shr-tag-section (dom)
    (shr-ensure-paragraph)
    (shr-generic dom)
    (shr-ensure-paragraph))

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

(use-package shrface
  :straight t
  :after eww
  :hook (eww-mode . shrface-mode)
  :custom (shrface-toggle-bullets nil)
  :config
  (shrface-basic)
  ;; (shrface-trial)
  (setq shrface-href-versatile t)

  ;; I proposed `jnf/shrface-headlines-consult' and
  ;; `jnf/shrface-links-consult' over at
  ;; https://github.com/chenyanming/shrface/issues/14.  I added the
  ;; jnf prefix to highlight that they are my own creation.  If the
  ;; maintainer adds these functions, then I'll remove them.
  ;;
  ;; Working on `consult-headlines' from shrface package.
  (defun jnf/shrface-headlines-consult ()
    "Use consult to show all headlines in order founded in the buffer.
Current headline will be the one of the candidates to initially select."
    (interactive)
    (let ((current (point-min)) (start (1+ (point))) point number)
      ;; Scan from point-min to (1+ (point)) to find the current headline.
      ;; (1+ (point)) to include under current point headline into the scan range.
      (unless (> start (point-max))
        (while (setq point (text-property-not-all
                            current start shrface-headline-number-property nil))
          (setq current (1+ point))))

      (cond ((equal (point) 1) (setq number 0))
            ((equal (point) 2) (setq number 0))
            ((equal (point) (point-max)) (setq number 0))
            (t
             (ignore-errors (setq number (1- (get-text-property (1- current) shrface-headline-number-property))))))

      ;; Start the consult--read
      (setq start (point)) ; save the starting point
      (if (fboundp 'consult--read)
          (consult--read (shrface-headline-selectable-list)
                         :prompt "shrface headline:"
                         :category 'shrface-headlines-consult
                         :sort nil)
        (message "Please install 'consult' before using 'shrface-headlines-consult'"))))


  (defun jnf/shrface-links-consult ()
    "Use consult to present all urls in order founded in the buffer."
    (interactive)
    (let ((start (point)) next url)
      ;; get the next nearest url
      (setq next (text-property-not-all
                  (point) (point-max) shrface-href-follow-link-property nil))
      ;; only if the next url exists
      (if next
          (setq url (get-text-property next shrface-href-property)))
      (if (fboundp 'consult--read)
          (consult--read (shrface-links-selectable-list)
                         :prompt "shrface link:"
                         :category 'shrface-links-consult
                         :sort nil)
        (message "Please install 'consult' before using 'shrface-links-consult'"))))

  :bind (:map
         eww-mode-map (("<tab>" . shr-next-link)
                       ("<backtab>" . shr-previous-link)))
  :bind (:map
         shrface-mode-map (("<C-M-tab>" . shrface-outline-cycle-buffer)
                           ("C-t" . shrface-toggle-bullets)
                           ("C-j" . shrface-next-headline)
                           ("C-k" . shrface-previous-headline)
                           ("M-l" . jnf/shrface-links-consult)
                           ("M-h" . jnf/shrface-headlines-consult))))

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
