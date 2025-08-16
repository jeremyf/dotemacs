(defvar jf/subscribe-me/default-tags
  '("1st" "provisional")
  "Default tags to apply when subscribing to an RSS feed.")

(defun jf/subscribe-me (&optional url tags)
  "Subscribe to the given URL.

URL is assumed to be either of an RSS feed or Atom feed."
  (interactive
    (list
      (if (derived-mode-p 'eww-mode)
        (progn
          (call-interactively #'eww-copy-alternate-url)
          (car kill-ring))
        (user-error "current buffer is not 'eww-mode'"))))
  (let ((tags
          (or tags jf/subscribe-me/default-tags)))
    (save-excursion
      (save-restriction
        (require 'org)
        (widen)
        ;; Where I add RSS entries.
        (org-capture-goto-target "r")
        (org-insert-subheading nil)
        (insert url " :" (s-join ":" tags) ":\n"
          ":PROPERTIES:\n"
          ":ID: " (org-id-new) "\n"
          ":DATE_ADDED: " (format-time-string "%Y-%m-%d") "\n"
          ":END:\n")
        (save-buffer)
        (bury-buffer)))))

(defun jf/syncthing-aling ()
  "Synchronize files into SyncThing bucket."
  (interactive)
  ;; Ensure we have our queue and our ready location
  (mkdir (file-truename "~/SyncThings/queue") t)
  (mkdir (file-truename "~/SyncThings/source") t)
  ;; There's a 1 in 10 chance that we'll perform the sync.  Toss that d10.
  (if (= 0 (random 10))
    (progn
      (message "Clearing mail log...")
      (shell-command
        "cat /dev/null > ~/.msmtp.log")
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

(require 'ox-takeonrules)

(use-package mastodon
  ;; :straight (:host codeberg :repo "martianh/mastodon.el")
  :when (file-exists-p (expand-file-name "~/.my-computer"))
  :after (logos)
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

  (defvar jf/toot-prefix "🐘"
    "The value that we insert before each mastodon toot.

Useful for narrowing regions.")

  (setq-default
    logos-outline-regexp-alist
    (append `((mastodon-mode . ,(concat "^" jf/toot-prefix)))
      logos-outline-regexp-alist))

  (defun jf/mastodon-tl--insert-status (&rest args)
    "A little hack to help narrow region."
    (insert jf/toot-prefix " ·  ·  ·  ·  ·  ·  ·"))
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
     entry (file+olp+datetree
             jf/personal/filename-for-journal)
     "DRAFT At %(format-time-string \"%R\") :blogPosts:\n:PROPERTIES:\n:CUSTOM_ID: journal-%(format-time-string \"%Y%m%d-%H%M\")\n:END:\n%?"
     :empty-lines-before 1
     :empty-lines-after 1
     :jump-to-captured t
     :clock-in t
     :clock-resume t))

(add-to-list 'org-capture-templates
  '("g" "Glossary Entry"
     entry (file+headline
             jf/filename/glossary
             "Terms")
     "%^{Entry} %^g\n:PROPERTIES:\n:END:\n\n%?"
     :prepare-finalize jf/org/capture/glossary/finalize
     :no-save t
     :immediate-finish nil
     :kill-buffer t
     :jump-to-captured t))


(defun jf/org/capture/glossary/finalize ()
  "Name the glossary entry."
  (save-excursion
    (goto-char (point-min))
    (let* ((entry
             (org-element-at-point))
            (tags
              (org-get-tags nil t))
            (title
              (org-element-property :title entry)))
      ;; TODO: Assign
      (message "%S" tags)
      (org-set-tags
        (seq-sort #'string< (seq-union tags '("glossary"))))
      (org-entry-put entry "CUSTOM_ID"
        (format "GLOSSARY-%s" (s-upcase
                                (denote-sluggify 'title title)))))))

(add-to-list 'org-capture-templates
  '("j" "Journal Entry"
     entry (file+olp+datetree
             jf/personal/filename-for-journal)
     "At %(format-time-string \"%R\")\n:PROPERTIES:\n:CUSTOM_ID: journal-%(format-time-string \"%Y%m%d-%H%M\")\n:END:\n%?"
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
     :empty-lines-after 1))

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


(defvar jf/filename/glossary-takeonrules
  (file-truename "~/git/takeonrules.source/content/site-map/glossary/index.md"))
(defvar jf/filename/glossary-data-takeonrules
  (file-truename "~/git/takeonrules.source/data/glossary.yml"))
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

(require 'request)
(require 's)

(cl-defstruct jf/book
  "A basic representation of a book as it relates to my personal
bibliography.

Slots:
- label:     Used for comparing to labels from my bibliography; expected
             to conform to `jf/book-make-label' function.
- title:     The ubiquitous human readable identifier of a work.
- author:    The author(s) of the book; separated by \" and \".
- subtitle:  Usually the words after the colon of a title.
- tags:      A list of tags for the book; these are internal values.
- isbn:      The ISBN for the given book.  One of the unique
             identifiers.
- custom_id: The `org-mode' headline CUSTOM_ID property, used for
             helping find the headline."
  label title author subtitle tags isbn custom_id)

(defvar my-cache-of-books
  (make-hash-table :test 'equal)
  "We use this as a cache of my bibliography entries that are books,
reprsented as an alist, and for each pair the `car' is a
`jf/book-label' (as formated by `jf/book-make-label') and the `cdr'
being an instance of `jf/book'.

See `my-cache-of-books/populate' for details on populating
this structure.

Normally I'd prefix this kind thing with \"jf/\", however I like how
this variable reads.")

(defun my-cache-of-books/contains-isbn-p(isbn)
  "Return non-nil when `my-cache-of-books' has ISBN."
  (seq-find (lambda (book)
              (equal isbn (jf/book-isbn book)))
            (hash-table-values my-cache-of-books)))

(defconst jf/bibliography/tag-owns
  "owns"
  "The tag used to indicate ownership of a work.")

(defconst jf/bibliography/tag-books
  "books"
  "The tag used to indicate that a work is book.")

(defun my-cache-of-books/populate (&optional clear-cache)
  "Populates `my-cache-of-books' with my current books.

When CLEAR-CACHE is non-nil, clobber the cache and rebuild."
  (when clear-cache (clrhash my-cache-of-books))
  (when (hash-table-empty-p my-cache-of-books)
    (save-excursion
      (with-current-buffer
        (find-file-noselect jf/filename/bibliography)
        (save-restriction
          (widen)
          (message "Rebuilding `my-cache-of-books'...")
          (org-map-entries
            (lambda ()
              ;; For some reason the org-map-entries is not filtering
              ;; on only items tagged as books.  Hence the
              ;; conditional.
              (when-let* ((tags
                            (org-element-property
                              :tags (org-element-at-point)))
                           (_
                             (member jf/bibliography/tag-books tags)))
                (let* ((title
                         (org-element-property
                           :title (org-element-at-point)))
                        (author
                          (org-entry-get
                            (org-element-at-point) "AUTHOR"))
                        (subtitle
                          (org-entry-get
                            (org-element-at-point) "SUBTITLE"))
                        (label (jf/book-make-label
                                 title subtitle author)))
                  (puthash label
                    (make-jf/book
                      :label label
                      :tags tags
                      :title title
                      :subtitle subtitle
                      :author author
                      :custom_id (org-entry-get
                                   (org-element-at-point)
                                   "CUSTOM_ID")
                      :isbn (org-entry-get
                              (org-element-at-point)
                              "ISBN"))
                    my-cache-of-books))))
            (concat "+level=2+" jf/bibliography/tag-books) 'file)))))
  my-cache-of-books)

(defun jf/book-from-isbn (isbn)
  "Fetch the associated ISBN from Google API and return a `jf/book'.

TODO: Instead of returning the book, consider taking a function that
operates on the book.  There would need to be an inversion of behavior."
  (let ((book nil))
    (request "https://www.googleapis.com/books/v1/volumes"
      :params (list (cons "q" (concat "isbn:" isbn)))
      :parser (lambda ()
                (let ((json-object-type 'plist))
                  (json-read)))
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let* ((item
                          (aref (plist-get data :items) 0))
                         (volumeInfo
                          (plist-get item :volumeInfo)))
                    (setq book
                          (let ((title
                                 (plist-get volumeInfo :title))
                                (subtitle
                                 (plist-get volumeInfo :subtitle))
                                (author
                                 (s-join " and "
                                         (plist-get
                                          volumeInfo :authors))))
                            (make-jf/book
                             :isbn isbn
                             :label (jf/book-make-label
                                     title subtitle author)
                             :subtitle subtitle
                             :author author
                             :title title)))))))
    book))

;;; Entry points
(defun jf/add-to-bibliography-from-isbn (isbn &optional prefix)
  "Append or amend to my bibliography the book associated with the ISBN.

See `jf/bibliography/update-with-book' for further details.

When PREFIX is non-nil clobber and rebuild `my-cache-of-books' and
ignore any guards against performing work on an already existing ISBN."
  (interactive "nEnter an ISBN (as number): \nP")
  (my-cache-of-books/populate prefix)
  (jf/bibliography/update-with-book
    isbn
    (lambda ()
      "Fetch and assemble the book from the ISBN."
      (jf/book-from-isbn isbn))
    prefix))

(defun jf/add-to-bibliography-from-bookaccio-file (filename &optional prefix)
  "Process books from FILENAME exported by Bookaccio app.

See `jf/bibliography/update-with-book' for details.

When PREFIX is non-nil clobber and rebuild `my-cache-of-books' and
ignore any guards against performing work on an already existing ISBN."
  (interactive "FFilename of a Bookaccio Export: \nP")
  (my-cache-of-books/populate prefix)
  (with-current-buffer (find-file-noselect filename)
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((nodes
              (json-parse-buffer :object-type 'plist)))
        (seq-each
          (lambda (node)
            (when-let ((isbn
                         (plist-get node :isbn)))
              (jf/bibliography/update-with-book
                isbn
                (lambda ()
                  "Assemble the book from the JSON node."
                  (let* ((title
                           (plist-get node :title))
                          (subtitle
                            (plist-get node :subtitle))
                          (author
                            (s-join " and "
                              (plist-get node :authors)))
                          (label
                            (jf/book-make-label title subtitle author)))
                    (make-jf/book
                      :label label
                      :title title
                      :subtitle subtitle
                      :author author
                      :isbn isbn)))
                prefix)))
          nodes)))))

;;; Bibliography Interaction
(defun jf/bibliography/update-with-book (isbn book-builder &optional force)
  "Append or amend to my bibliography the book associated with the ISBN.

The BOOK-BUILDER is a function with arity 0; responsible for assembling
a `jf/book'.

When FORCE is non-nil, we ignore looking up whether or not the
`my-cache-of-books' contains the given ISBN.

When FORCE is nil, we check if the given ISBN is in our
`my-cache-of-books', and bail on the process when it is.

Put another way, when we are FORCE-ing the update or the ISBN is not in
`my-cache-of-books', we build the book and either append or amend our
entry."
  (when (or force
          (not (my-cache-of-books/contains-isbn-p isbn)))
    (let* ((tags-for-bibliography
             `(,jf/bibliography/tag-owns ,jf/bibliography/tag-books))
            (book-from-builder
              (funcall book-builder))
            (completed-value
              (completing-read
                (format "Match %s: " (jf/book-label book-from-builder))
                my-cache-of-books nil nil
                ;; Maybe we'll get a direct hit?
                (jf/book-label book-from-builder))))
      (if-let ((from-bibliography
                 (gethash completed-value my-cache-of-books)))
        ;; When book **is** found in bibliography, update a book entry
        ;; with one already in my bibliography.
        (progn
          (setf
            (jf/book-isbn from-bibliography)
            (jf/book-isbn book-from-builder)
            (jf/book-tags from-bibliography)
            (sort (seq-union
                    (jf/book-tags from-bibliography)
                    tags-for-bibliography
                    #'string=)))
          (save-restriction
            (widen)
            (save-excursion
              (with-current-buffer
                (find-file-noselect jf/filename/bibliography)
                (org-map-entries
                  (lambda ()
                    (let ((hl (org-element-at-point)))
                      (when (string=
                              (org-entry-get hl "CUSTOM_ID")
                              (jf/book-custom_id from-bibliography))
                        (progn
                          (org-set-property "ISBN"
                            (jf/book-isbn from-bibliography))
                          (org-set-tags
                            (jf/book-tags from-bibliography))
                          (save-buffer)))))
                  (concat "+" jf/bibliography/tag-books) 'file)
                (message "Updated %s with ISBN %s"
                  (jf/book-label from-bibliography)
                  (jf/book-isbn from-bibliography))))))
        ;; When book **is not** found in bibliography, insert a new book
        ;; entry into my bibliography.
        ;;
        ;; NOTE: My first incarnation was to use `org-capture-string'
        ;; which meant adding to `org-capture-templates' a conceptually
        ;; \"private\" capture template.  That incarnation used \"?\" as
        ;; the template body.  However, when I'd test the behavior, I
        ;; was getting an empty entry.
        ;;
        ;; Instead I deconstructed the concise `org-capture-string' and
        ;; shifted towards binding `kill-ring' and using the \"%c\"
        ;; capture variable.  I like this approach as it eschews adding
        ;; a useless capture template while leveraging the power of the
        ;; `org-capture' ecosystem.
        (let* ((kill-ring
                 (list
                   (concat (jf/book-title book-from-builder)
                     " :" (s-join ":" tags-for-bibliography) ":\n"
                     ":PROPERTIES:\n"
                     ":CUSTOM_ID: "
                     (jf/denote-sluggify-title
                       (jf/book-label book-from-builder)) "\n"
                     (when (s-present?
                             (jf/book-subtitle book-from-builder))
                       (concat ":SUBTITLE: "
                         (jf/book-subtitle book-from-builder) "\n"))
                     (when (s-present?
                             (jf/book-author book-from-builder))
                       (concat ":AUTHOR: "
                         (jf/book-author book-from-builder) "\n"))
                     ":ISBN: " (jf/book-isbn book-from-builder) "\n"
                     ":END:\n")))
                (org-capture-entry
                  '("B" "Book from ISBN Lookup"
                     entry
                     (file+headline jf/filename/bibliography "Works")
                     "%c"
                     :immediate-finish t)))
          (org-capture)
          (puthash (jf/book-label book-from-builder)
            book-from-builder my-cache-of-books)
          (message "Appended %s to bibliography"
            (jf/book-label book-from-builder)))))))

(defvar jf/site-lisp:mu4e
  (if (f-exists? "/usr/local/share/emacs/site-lisp/mu4e")
    "/usr/local/share/emacs/site-lisp/mu4e"
    "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"))
;; (add-to-list 'load-path jf/site-lisp:mu4e)
;; (require 'mu4e)
(use-package mu4e
  :load-path jf/site-lisp:mu4e
  ;; This follows the build from https://vhbelvadi.com/emacs-mu4e-on-macos.
  ;;
  ;; One variation: the tls_trust_file in ~/.msmtprc did not need to
  ;; come from the Apple Keychain.  But was from "Export TLS
  ;; Certificates from the Proton Bridge > Settings > Advanced Settings
  ;; :load-path "/usr/share/emacs/site-lisp/mu4e" ;; jf/site-lisp
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
       ("/Archive" . ?a)
       ("/Trash"     . ?t)
       ("/Drafts"    . ?d)))
  (setq mu4e-use-fancy-chars t)

  ;; SENDING
  (setq send-mail-function 'message-send-mail-with-sendmail
    message-send-mail-function 'message-send-mail-with-sendmail)
  (setq message-sendmail-envelope-from 'header)
  (add-hook 'mu4e-compose-mode-hook (lambda () (setq-local fill-column 80)))
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

(use-package libmpdel
  :straight t)

(use-package mpdel
  :after libmpdel
  :straight t
  :custom (mpdel-prefix-key (kbd "H-m"))
  :config (mpdel-mode))

(use-package bongo
  :straight t)

(use-package vlc
  :straight t)

(use-package p-search
  :straight (:host github :repo "zkry/p-search"))

(use-package doric-themes
  :straight (:host github :repo "protesilaos/doric-themes"))

;;; One Off Scripts
;; Goal: to migrate all [[denote:identifier]] references to each
;; glossary entry to [[denote:<GLOSSARY-FILE-IDENTIFIER>::#GLOSSARY-<OLD-IDENTIFIER>>]]
(defun jf/convert-glossary-entries ()
  (message "Δ Begin converting entries")
  (dolist (filename (s-split "\n"
                      (s-trim
                        (shell-command-to-string "fd _glossary ~/git/org/denote"))))
    (message "Δ converting %s" filename)
    (jf/convert-glossary filename))
  (message "Δ Done converting entries"))

(defun jf/convert-glossary (&optional from into)
  "Convert FROM file INTO Glossary Entry."
  (let ((into
          (or into  jf/filename/glossary)))
    (with-current-buffer (if from (find-file-noselect from) (current-buffer))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char 0)
          (let* ((properties
                   (jf/org-keywords-as-plist :keywords-regexp ""))
                  (title
                    (org-get-title)))
            ;; Move past the initial front-matter
            (while (and
                     (> (point-max) (point))
                     (or
                       (string-match-p "^\#\+" (thing-at-point 'line))
                       (string-match-p "\\`\\s-*$" (thing-at-point 'line))))
              (next-line)
              (end-of-line))
            (let ((content
                    (s-trim
                      (replace-regexp-in-string
                        "^\*" "***"
                        (buffer-substring (point) (point-max))))))
              (with-temp-buffer (concat "*migrating " title "*")
                (insert "\n** " title " " (lax-plist-get properties "FILETAGS") "\n")
                (insert ":PROPERTIES:\n")
                (dotimes (i (length properties))
                  (when (= 0 (mod i 2))
                    (let ((key (nth i properties)))
                      (when (string= key "GLOSSARY_KEY")
                        (progn
                          (insert ":ID: GLOSSARY-" (upcase (nth (+ i 1) properties)) "\n")
                          (insert ":CUSTOM_ID: GLOSSARY-" (upcate (nth (+ i 1) properties)) "\n")))
                      (when (not (member key '("TITLE" "FILETAGS" "DATE" "ORIGINAL_ORG_ID")))
                        (insert ":" (s-upcase (nth i properties)) ": " (nth (+ i 1) properties) "\n")))))
                (insert ":END:\n")
                (insert content)
                (append-to-file (point-min) (point-max) into)))))))))

(defun jf/migrate-denote-links-for-glossary ()
  (with-current-buffer (find-file-noselect jf/filename/glossary)
    (org-map-entries
      (lambda ()
        (let* ((entry
                 (org-element-at-point))
                (identifier
                  (org-entry-get entry "IDENTIFIER"))
                (custom_id
                  (org-entry-get entry "CUSTOM_ID"))
                (glossary_key
                  (org-entry-get entry "GLOSSARY_KEY")))
          ;; Update org docs
          (if custom_id
            (shell-command
              (format "cd ~/git/org/denote; lil-regy -f \":%s\\]\" -r \":20250101T000000::#%s]\" -d -- -i"
                identifier custom_id))
            (user-error "⧳ no custom id for %s" identifier))
          ;; Update blog references
          (when glossary_key
            (shell-command
              (format "cd ~/git/takeonrules.source; lil-regy -f \"key=\\\"%s\\\"\" -r \"key=\\\"%s\\\"\" -d -- -i"
                glossary_key custom_id)))))
      "LEVEL=2+glossary"
      'file)))
