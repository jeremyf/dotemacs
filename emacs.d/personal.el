(defun jf/syncthing-aling ()
  "Synchronize files into SyncThing bucket."
  (interactive)
  ;; Ensure we have our queue and our ready location
  (mkdir (file-truename "~/SyncThings/queue") t)
  (mkdir (file-truename "~/SyncThings/source") t)
  (message "Synchronzing denote files to cloud...")
  ;; There's a 1 in 10 chance that we'll perform the sync.  Toss that d10.
  (if (= 0 (random 10))
    (progn
      (message "Syncing elfeed database...")
      ;; We tar zip into one directory (our queue) and then move that
      ;; file in its complete state into the ready directory.  As move
      ;; is instantaneous and we don't need to worry about syncthing
      ;; picking up a partially completed file.
      (shell-command
        (concat "tar -cvzf "
          (file-truename "~/SyncThings/queue/elfeed.tar.gz")
          " " elfeed-db-directory " && mv -f ~/SyncThings/queue/elfeed.tar.gz ~/SyncThings/source&")
        "*syncthing-aling*"
        "*syncthing-aling*"))
    (message "I'll get you next time Gadget")))

;; Based on the idea of habit stacking, whenever I pull down my RSS
;; feed, I'll go ahead and sync my notes.
(advice-add #'jf/elfeed-load-db-and-open :before #'jf/syncthing-aling)
(add-hook 'after-init-hook #'jf/syncthing-aling)

(use-package mastodon
  ;; :straight (:host codeberg :repo "martianh/mastodon.el")
  :when (file-exists-p (expand-file-name "~/.my-computer"))
  :init
  (use-package tp
  :straight (:host codeberg :repo "martianh/tp.el")
    :when (file-exists-p (expand-file-name "~/.my-computer")))
  :custom
  (mastodon-tl--timeline-posts-count "50")
  :config
  (setopt
    mastodon-instance-url "https://dice.camp"
    mastodon-active-user "takeonrules")
  (defun jf/mastodon-tl--reading (&optional prefix)
    "Read my reading timeline.

Forward PREFIX to `mastodon-tl--show-tag-timeline'."
    (interactive "p")
    (mastodon-tl--show-tag-timeline
      prefix
      '("reading" "books" "booktok"
         "literature" "libraries" "bookstodon"
         "writing" "library")))

  (defun jf/mastodon-tl--insert-status (&rest args)
    "A little hack to help narrow region."
    (insert "🐘  ·  ·  ·  ·  ·  ·  ·"))
  (advice-add 'mastodon-tl--insert-status
    :before #'jf/mastodon-tl--insert-status))

;; A cautionary tale in regards to using this function.  If I have an
;; org-mode file and I export it to markdown, that markdown file will be
;; what we pick-up.
(defvar jf/personal/filename-for-journal
  (denote-get-path-by-id "20241114T075414")
  "Where I put my journal.")

(add-to-list 'org-capture-templates
  '("b" "Blog Post"
     plain (file denote-last-path)
     (function
       (lambda ()
         (let* ((denote-use-title
                  ;; If I don't provide the title, then it'll use the
                  ;; active region as the title, and failing that list
                  ;; previous titles I've used from which to select.
                  ;; With this let clause, I'm asking for a clean title.
                  (read-string "Blog Post Title: "))
                 (denote-use-keywords
                  `(,jf/denote/keywords/blogPosts))
                 (denote-org-capture-specifiers
                   ;; When I am capturing with an active region, use the
                   ;; capture wrap; that is bring the highlighted region
                   ;; along.  Else, when I'm not capturing with an
                   ;; active region, just prompt to start writing.
                   (if (use-region-p)
                     "%(jf/denote/capture-wrap :link \"%L\" :content \"%i\")"
                     "%?")))
           (denote-org-capture))))
     :no-save t
     :immediate-finish nil
     :kill-buffer t
     :jump-to-captured t))

(add-to-list 'org-capture-templates
  '("g" "Glossary Entry"
     plain (file denote-last-path)
     (function
       (lambda ()
         ;; (setq-local keywords (list))
         (let* ((keywords
                  `(,jf/denote/keywords/glossary))
                 (title
                  (read-string "Glossary Entry Title: "))
                 (is-a-game
                   (yes-or-no-p "Is this a game?"))
                 (is-a-tag
                   (yes-or-no-p "Is a tag for Take on Rules?"))
                 (abbr
                   (read-from-minibuffer "Abbreviation (empty to skip): "))
                 (key
                   (downcase (denote-sluggify-title title))))
           (when is-a-game
             (add-to-list 'keywords jf/denote/keywords/games))
           (when (s-present? abbr)
             (add-to-list 'keywords jf/denote/keywords/abbr))
           (let* ((denote-use-title
                    title)
                   (denote-use-template
                     (concat
                       "#+GLOSSARY_KEY: " key "\n"
                       (when (s-present? abbr)
                         (concat "#+ABBR: " abbr "\n"))
                       (when is-a-game
                         (concat "#+GAME: " key "\n"))
                       (when (s-present? abbr)
                         (concat
                           "#+PLURAL_ABBR: %^{Plural Abbr|" abbr "s}\n"
                           "#+PLURAL_TITLE: %^{Plural Title|" title "s}\n"))
                       (when is-a-tag
                         (concat "#+TAG: " key "\n"))))
                   (denote-use-signature
                     (when (s-present? abbr)
                       (denote-sluggify-signature abbr)))
                   (denote-use-keywords
                     keywords)
                   (denote-org-capture-specifiers
                     "%?"))
             (denote-org-capture)))))
     :no-save t
     :immediate-finish nil
     :kill-buffer t
     :jump-to-captured t))

(add-to-list 'org-capture-templates
  '("j" "Journal Entry"
     plain (file+olp+datetree
             jf/personal/filename-for-journal)
     "%T :: %?"
     :empty-lines-before 1
     :empty-lines-after 1
     :clock-in t
     :clock-resume t))

(add-to-list 'org-capture-templates
  `("v" "Add to Today's Private Thoughts"
     plain (file+function
             jf/personal/filename-for-journal
             jf/personal/position-in-private-thoughts)
     "%T :: From %a.  %?"
     :empty-lines-before 1
     :empty-lines-after 1
     :clock-in t
     :clock-resume t))

(add-to-list 'org-capture-templates
  `("r" "Add to RSS Feed"
     entry (file+headline
             ,(denote-get-path-by-id "20110202T000001")
             "RSS Feed")
     "%^{URL} %^g\n%?"
     :empty-lines-before 1
     :empty-lines-after 1
     :immediate-finish t))

(add-to-list 'org-capture-templates
  `("t" "Task"
     entry (file+headline
             jf/personal/filename-for-journal
             "Tasks")
     "TODO %^{Task}\n\n%T :: %(jf/denote/capture-wrap :link \"%L\" :content \"%i\")"
     :empty-lines-before 1
     :empty-lines-after 1
     :clock-in t
     :clock-resume t))

(defun jf/personal/position-in-private-thoughts ()
  "Position to the end of today's private thoughts."
  (let ((entry-date (format-time-string "%Y-%m-%d %A")))
    (if-let ((position
               (car (org-element-map
                      (org-element-parse-buffer 'headline)
                      'headline
                      (lambda (hl)
                        (and
                          (=
                            (org-element-property :level hl) 4)
                          (member "private"
                            (org-element-property :tags hl))
                          (string=
                            (format-time-string "%Y-%m-%d %A")
                            (format "%s"
                              (org-element-property
	                                   :title
	                                   (car (org-element-lineage hl)))))
                          (org-element-property :contents-end hl)))))))
      (goto-char (1- position))
      (user-error "Missing :private: entry for date %S" entry-date))))

(defvar jf/epigraphs/cache
  nil
  "When non-nil, use this cache for picking a random epigraphs.")

(defun jf/epigraph/random ()
  "Open a random epigraph for reading and review."
  (interactive)
  (let ((epigraphs
          (or jf/epigraphs/cache (jf/epigraphs/all-randomized))))
    (setq jf/epigraphs/cache epigraphs)
    (let* ((use-hard-newlines
             t)
            (epigraph
              (seq-random-elt epigraphs))
            (id
              (plist-get epigraph :id))
            (text
              (plist-get epigraph :text))
            (author
              (plist-get epigraph :author))
            (work
              (plist-get epigraph :work))
            (content
              (format "%s%s"
                text
                (cond
                  ((and (s-present? author) (s-present? work))
                    (format "\n―%s, [[epigraph:%s][%s]]" author id work))
                  ((s-present? author)
                    (format "\n―[[epigraph:%s][%s]]" id author))
                  ((s-present? work)
                    (format "\n―[[epigraph:%s][%s]]" id work))
                  (t ""))))
            (buffer
              (get-buffer-create "*epigraph*")))
      ;; (message "%s" content)
      (with-current-buffer buffer
        (erase-buffer)
        (insert content)
        (org-mode)
        (jf/epigraph-mode t)
        (pop-to-buffer buffer
          `((display-buffer-in-side-window)
             (side . bottom)
             (window-width 72)
             (window-parameters
               (tab-line-format . none)
               (no-delete-other-windows . t))))))))

(defvar jf/epigraph-mode-map
  (let ((map
          (make-sparse-keymap)))
    (define-key map (kbd "q") #'jf/epigraph/bury-buffer)
    (define-key map (kbd "g") #'jf/epigraph/random)
    map)
  "Map for `jf/epigraph-mode'.")

(defun jf/epigraph/bury-buffer ()
  (interactive)
  (setq jf/epigraphs/cache nil)
  (bury-buffer))

(define-minor-mode jf/epigraph-mode
  "Defined for rendering random epigraph."
  :init-value nil
  :global nil
  :keymap jf/epigraph-mode-map)

(defun jf/epigraphs/all-randomized ()
  (save-excursion
    (with-current-buffer
      (find-file-noselect jf/filename/bibliography)
      (elfeed--shuffle
        (org-element-map
          (org-element-parse-buffer)
          '(quote-block verse-block)
          (lambda (el)
            ;; Skip un-named blocks as we can’t link to them.
            (when-let* ((id
                          (org-element-property :name el)))
              (let* ((lineage
                       (org-element-lineage el))
                      (h-node
                        (car
                          (seq-filter
                            (lambda (el)
                              (and
                                (eq (org-element-type el) 'headline)
                                (= (org-element-property :level el) 2)))
                            lineage)))
                      (people?
                        (member "people"
                          (org-element-property :tags h-node))))
                (list
                  :id id
                  :type (org-element-type el)
                  :work (if people?
                          ""
                          (car
                            (org-element-property
                              :title h-node)))
                  :author
                  (if people?
                    (car
                      (org-element-property :title h-node))
                    (org-entry-get h-node "AUTHOR"))
                  :text
                  (buffer-substring-no-properties
                    (org-element-property
                      :contents-begin el)
                    (org-element-property
                      :contents-end el)))))))))))

;; (use-package notmuch
;;   ;; Consider the NotMuch Mail Emacs tips and tricks: https://notmuchmail.org/emacstips/
;;   ;;
;;   ;; https://blog.tomecek.net/post/removing-messages-with-notmuch/
;;   :load-path  "/opt/homebrew/share/emacs/site-lisp/notmuch"
;;   :custom
;;   ((notmuch-saved-searches
;;      '((:name "inbox" :query "tag:inbox" :key "i" :sort-order newest-first)
;;         (:name "unread" :query "tag:unread" :key "u" :sort-order newest-first)
;;         (:name "correspondence" :query "tag:correspondence" :key "c" :sort-order newest-first)
;;         (:name "flagged" :query "tag:flagged" :key "f" :sort-order newest-first)
;;         (:name "sent" :query "tag:sent" :key "t" :sort-order newest-first)
;;         (:name "drafts" :query "tag:draft" :key "d" :sort-order newest-first)
;;         (:name "all mail" :query "*" :key "a" :sort-order newest-first)))
;;     (notmuch-init-file "~/.notmuch-config"))
;;   :config
;;   /(setq notmuch-fcc-dirs "sent")
;;   (setq mail-specify-envelope-from t)
;;   (define-key notmuch-search-mode-map (kbd "C-k")
;;     (lambda ()
;;       "Delete message."
;;       (interactive)
;;       (notmuch-search-tag (list "+deleted" "-inbox"))
;;       (notmuch-search-next-thread)))
;;   (define-key notmuch-tree-mode-map (kbd "C-k")
;;     (lambda ()
;;       "Delete message."
;;       (interactive)
;;       (notmuch-tree-tag (list "+deleted" "-inbox"))
;;       (notmuch-tree-next-matching-message)))

  (defun jf/notmuch/expunge-deleted ()
    "Expunge deleted emails."
    (interactive)
    (shell-command
      "for x in $(notmuch search --output=files tag:deleted | rg \":2(,.*[^T])?$\" | sed -E \"s/:2(,.+)?$/:2/g\" ) ; do mv $x ${x}T ; done"))

;;   (defun jf/notmuch-unthreaded-show-recipient-if-sent (format-string result)
;;     (let* ((headers (plist-get result :headers))
;;             (to (plist-get headers :To))
;;             (author (plist-get headers :From))
;;             (face (if (plist-get result :match)
;;                     'notmuch-tree-match-author-face
;;                     'notmuch-tree-no-match-author-face)))
;;       (propertize
;;         (format format-string
;;           (if (string-match "jeremy@jeremyfriesen.com" author)
;;             (concat "↦ " (notmuch-tree-clean-address to))
;;             (notmuch-tree-clean-address to)
;;             author))
;;         'face face)))

;;   (setq notmuch-unthreaded-result-format
;;     '(("date" . "%12s  ")
;;        (jf/notmuch-unthreaded-show-recipient-if-sent . "%-20.20s")
;;        ((("subject" . "%s"))
;;          . " %-54s ")
;;        ("tags" . "(%s)"))))

;; (use-package ol-notmuch
;;   :straight t
;;   :after notmuch)
;; (use-package notmuch-transient
;;   :straight t
;;   :after notmuch)

;; (use-package notmuch-addr
;;   ;; Relies on Emacs 27.1 updated completion framework.
;;   :straight t
;;   :config
;;   (with-eval-after-load 'notmuch-address
;;     (notmuch-addr-setup)))

(use-package mu4e
  ;; This follows the build from https://vhbelvadi.com/emacs-mu4e-on-macos.
  ;;
  ;; One variation: the tls_trust_file in ~/.msmtprc did not need to
  ;; come from the Apple Keychain.  But was from "Export TLS
  ;; Certificates from the Proton Bridge > Settings > Advanced Settings
  :load-path  "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"
  :init (require 'smtpmail)
  :config
  ;; BINARIES
  (setq mu4e-mu-binary (executable-find "mu")
    mu4e-get-mail-command (concat (executable-find "mbsync") " -a")
    sendmail-program (executable-find "msmtp"))
  (setq mu4e-user-mail-address-list '("jeremy@jeremyfriesen.com"))
  (setq mu4e-change-filenames-when-moving t ; avoid sync conflicts
    mu4e-update-interval (* 10 60) ; check mail 10 minutes
    mu4e-compose-format-flowed t ; re-flow mail so it's not hard wrapped
    mu4e-maildir "~/Maildir/proton")
  (setq mu4e-drafts-folder "/Drafts"
    mu4e-sent-folder   "/Sent"
    mu4e-refile-folder "/All Mail"
    mu4e-trash-folder  "/Trash")
  (setq mu4e-completing-read-function 'completing-read)
  (setq mu4e-maildir-shortcuts
    '(("/inbox"     . ?i)
       ("/Sent"      . ?s)
       ("/Archive" . ?A)
       ("/Trash"     . ?t)
       ("/Drafts"    . ?d)
       ("/All Mail"  . ?a)))
  (setq mu4e-use-fancy-chars t)

  ;; SENDING
  (setq send-mail-function 'message-send-mail-with-sendmail
    message-send-mail-function 'message-send-mail-with-sendmail)
  (setq message-sendmail-envelope-from 'header)

  ;; POLICIES
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'ask)
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-hide-index-messages t)
  (setq org-mu4e-link-query-in-headers-mode nil)
  (setq mu4e-headers-visible-lines 25)
  (setq mu4e-view-show-addresses t)

  (setq mu4e-headers-include-related t)
  (setq mu4e-headers-show-threads t)
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  (setq message-citation-line-format "%N @ %Y-%m-%d %H:%M :\n")


  ;; (setq message-send-mail-function 'smtpmail-send-it
  ;;     auth-sources '("~/.authinfo") ;need to use gpg version but only local smtp stored for now
  ;;     smtpmail-smtp-server "127.0.0.1"
  ;;     smtpmail-smtp-service 1025
  ;;     smtpmail-stream-type  'ssl)
  )

(defvar jf/filename/bibliography-takeonrules
  (file-truename "~/git/takeonrules.source/content/site-map/bibliography/_index.md"))

(defun jf/bibliography/export-to-takeonrules (&optional file)
  "Export completed bibliography reading to FILE."
  (interactive)
  (let* ((works
           (save-excursion
             (with-current-buffer
               (find-file-noselect jf/filename/bibliography)
               (org-map-entries
                 (lambda ()
                   (list
                     :title
                     (org-element-property
                       :title (org-element-at-point))
                     :subtitle
                     (org-entry-get
                       (org-element-at-point) "SUBTITLE")
                     :author
                     (org-entry-get
                       (org-element-at-point) "AUTHOR")
                     :editor
                     (org-entry-get
                       (org-element-at-point) "EDITOR")
                     ))
                 "+LEVEL=2+books-skipBibliography+TODO=\"DONE\"" 'file))))
          (buffer
            (find-file-noselect (or file jf/filename/bibliography-takeonrules))))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max))
      (insert
        (concat
          "---\n"
          "Sitemap:\n"
          "  ChangeFreq: monthly\n"
          "  Priority: 0.45\n"
          "date: 2023-07-01 16:56:12.000000000 -04:00\n"
          "images: []\n"
          "lastmod: " (format-time-string "%Y-%m-%d %H:%M:%S.%N %z") "\n"
          "layout: page\n"
          "licenses:\n"
          "- all-rights-reserved\n"
          "permalink: \"/site-map/bibliography/\"\n"
          "schema_dot_org_type: AboutPage\n"
          "skipCreativeContentLicense: true\n"
          "title: Bibliography\n"
          "type: page\n"
          "---\n"
          "\n"
          "In {{< linkToTable \"249\" >}}, I list most of the books that I've read.  I have chosen to exclude most {{< glossary key=\"RPG\" >}} books; in part because there are so many.  I plan to create a Ludography; a list of games that I've played, both {{< glossary key=\"RPG\" >}} and otherwise.\n"
          "\n"
          "{{< table table_number=\"249\" caption=\"Most of the Books that I've Read\" >}}\n"
          "<thead>\n"
          "<tr><th scope=\"col\">Title</th><th scope=\"col\">Author</th></tr>\n"
          "</thead>\n"
          "<tbody>\n"
          (s-join "\n"
            (mapcar (lambda (work)
	                    (format "<tr><td><cite>%s%s</cite></td><td>%s%s</td></tr>"
                        (lax-plist-get work :title)
                        (if-let ((subtitle
                                   (lax-plist-get work :subtitle)))
                          (concat ": " subtitle)
                          "")
                        (lax-plist-get work :author)
                        (if-let ((editor
                                   (lax-plist-get work :editor)))
                          (concat "Editor: " editor)
                          "")))
              works))
          "\n<tbody>\n"
          "{{< /table >}}\n"))
      (save-buffer))))
