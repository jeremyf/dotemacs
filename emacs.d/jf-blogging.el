;;; jf-blogging.el --- Blogging related functions -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;; Packages specifically here for helping with my writing activities.

;;; Code:
(require 'jf-formatting)
(with-eval-after-load 'org

  (require 'ox)
(use-package ox-hugo
  :straight t
  :custom
  ;; - blockquote :: for chunks of text that I attribute to other folks.
  ;; - marginnote :: a "dangling" note that is only partially part of the
  ;;                 conversation.
  ;; - poem :: because poetic spacing is critical.
  ;; - inline_comments :: a concession that I need different comments based on
  ;;                      context; and that marginalia may be too much in some
  ;;                      cases.
  ;; - update :: I write updates for my blog posts; corrections or additions
  ;;             based on new information.
  (org-hugo-paired-shortcodes "blockquote marginnote poem inlinecomment update")
  (hugo-use-code-for-kbd t)
  :config
  ;; I want to have backticks instead of indentations;  The backticks also
  (advice-add #'org-md-example-block :override #'org-blackfriday-src-block)
  :after ox)

;; These functions work too aggressively.  The types of lists (ordered,
;; definition, and unordered) are co-mingled.  This co-mingling means that I'm
;; not getting the behavior I want.  So I'll proceed with the default ox-hugo
;; behavior.
;;
;; (advice-add #'org-blackfriday-plain-list :override #'org-html-plain-list '((name . "wrapper")))
;; (advice-add #'org-blackfriday-item :override #'org-html-item '((name . "wrapper")))

;; Convert footnote to sidenote for HTML export
(defun jf/org-hugo-sidenote (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to Hugo sidenote shortcode.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((element (car (org-export-get-footnote-definition footnote-reference info)))
          (beg (org-element-property :contents-begin element))
          (end (org-element-property :contents-end element))
          (content (s-trim
                     (org-export-string-as
                       (buffer-substring-no-properties beg end)
                       'md t '(:with-toc nil)))))
    (format "{{< sidenote >}}%s{{< /sidenote >}}" content)))


(advice-add #'org-blackfriday-footnote-reference
  :override #'jf/org-hugo-sidenote
  '((name . "wrapper")))
(advice-add #'org-blackfriday-footnote-section
  :override (lambda (&rest rest) ())
  '((name . "wrapper")))

(advice-add #'org-hugo−-get-front-matter :filter-return #'jf/org-hugo-rewrite-tags)
(defun jf/org-hugo-rewrite-tags (info)
  "Turn OneWordTags into one-word-tags."
  (require 's)
  (dolist (field '(:categories :tags))
    (when (plist-get info field)
      (plist-put info field
		    (mapcar #'s-dashed-words (plist-get info field)))))
  info)

(defun jf/org-md-quote-block (quote-block contents info)
  "Render a QUOTE-BLOCK with CONTENTS and INFO.

Either render via the standard markdown way or when exporting to
Take on Rules using the \"blockquote\" special block."
  (if jf/exporting-org-to-tor
    (progn
      (org-element-put-property quote-block :type "blockquote")
      (org-hugo-special-block quote-block contents info))
    ;; The original md quote block method; probably a better way to do this.
    (replace-regexp-in-string
      "^" "> "
      (replace-regexp-in-string "\n\\'" "" contents))))
(advice-add #'org-md-quote-block :override #'jf/org-md-quote-block)

(setq org-hugo-base-dir "~/git/takeonrules.source")

(defvar jf/org-macros-setup-filename
  "~/git/dotemacs/lib/org-macros.setup"
  "The path to the file that has inline org macros.")

(defvar jf/exporting-org-to-tor nil
  "Not nil while performing the export of org file to Take on Rules.")

(cl-defun jf/export-org-to-tor (&key (buffer (current-buffer)))
  "Export current org BUFFER for TakeOnRules post."
  (interactive)
  ;; Ensure that we have an ID property.
  (setq jf/exporting-org-to-tor t)
  (with-current-buffer buffer
    (save-excursion
      (let* ((export-global-plist (jf/org-keywords-as-plist))
              (section (jf/export-org-to-tor--global-buffer-prop-ensure
                         :key "HUGO_SECTION"
                         :plist export-global-plist
                         :default (format-time-string "posts/%Y")))
              (base_dir (jf/export-org-to-tor--global-buffer-prop-ensure
                          :key "HUGO_BASE_DIR"
                          :plist export-global-plist
                          :default "~/git/takeonrules.source"))
              (format (jf/export-org-to-tor--global-buffer-prop-ensure
                        :key "HUGO_FRONT_MATTER_FORMAT"
                        :plist export-global-plist
                        :default "yaml"))
              (title (lax-plist-get export-global-plist "TITLE"))
              (identifier (lax-plist-get export-global-plist "IDENTIFIER")))
        (save-buffer)
        (jf/export-org-to-tor--inject-additional-front-matter
          :title title
          :identifier identifier)
        ;; Write metadata
        (save-buffer)
        (unless org-transclusion-mode (org-transclusion-mode))
        (org-open-file (org-hugo-export-wim-to-md nil nil t)))))
  (setq jf/exporting-org-to-tor nil))

(fset 'jf/tidy-ox-hugo-header-export
  (kmacro-lambda-form [?\C-c ?\C-n ?\s-f ?\{ return ?\C-b ?\C-b ?\C-k] 0 "%d"))

(cl-defun jf/export-org-to-tor--inject-additional-front-matter (&key identifier title)
  "Export additional front matter.

    We want to ensure that we export the IDENTIFIER and TITLE.
    And add relevant metadata."
  (goto-char (point-min))
  (search-forward-regexp "#\\+HUGO_FRONT_MATTER_FORMAT: yaml")
  (insert (concat
            "\n#+HUGO_SLUG: " (denote-sluggify-title title)
            ;; 2022-02-26 07:46:15.000000000 -04:00
            "\n#+HUGO_PUBLISHDATE: " (format-time-string "%Y-%m-%d %H:%M:%S %z")
            "\n#+HUGO_TYPE: post"
            "\n#+HUGO_LAYOUT: post"
            "\n#+HUGO_DRAFT: true"
            "\n#+HUGO_CUSTOM_FRONT_MATTER: :licenses '(by-nc-nd-4_0)"
            "\n#+HUGO_CUSTOM_FRONT_MATTER: :org_id " identifier))
  (when-let ((kw-plist (jf/org-keywords-as-plist
                         :keywords-regexp "\\(SESSION_REPORT_DATE\\|SESSION_REPORT_LOCATION\\|SESSION_REPORT_GAME\\)")))
    (insert
      (format
        "\n#+HUGO_CUSTOM_FRONT_MATTER: :sessionReport '((date . \"%s\") (game . \"%s\") (location . \"%s\"))"
        (plist-get kw-plist "SESSION_REPORT_DATE")
        (plist-get kw-plist "SESSION_REPORT_GAME")
        (plist-get kw-plist "SESSION_REPORT_LOCATION")))))

(cl-defun jf/export-org-to-tor--global-buffer-prop-ensure (&key key plist (default nil))
  "Ensure the current buffer has the given KEY in the global PLIST, if not set the DEFAULT or prompt for it."
  (let ((value (plist-get plist key #'string=)))
    (if value value
      (jf/export-org-to-tor--global-buffer-prop-set
        :key key
        :value (or default (read-from-minibuffer (format "%s: " key)))))))

(cl-defun jf/export-org-to-tor--global-buffer-prop-set (&key key value)
  "Set the global property named KEY to the VALUE for the current buffer."
  (goto-char (point-min))
  (forward-line 4)
  (insert (format "\n#+%s: %s" (upcase key) value)))

(defvar jf/tor-session-report-location
  '("around the table" "via Zoom" "via Discord and Roll20" "via Discord" "in my living room")
  "TakeOnRules session report locations.")

(cl-defun jf/org-keywords-as-plist (&key (keywords-regexp "\\(IDENTIFIER\\|FILETAGS\\|HUGO_FRONT_MATTER_FORMAT\\|HUGO_SECTION\\|HUGO_BASE_DIR\\|TITLE\\|SUBTITLE\\)"))
  (flatten-list (mapcar (lambda (prop)
                      (list (org-element-property :key prop)
                        (org-element-property :value prop)))
              (jf/org-global-props keywords-regexp))))

(defun jf/org-global-props (&optional property)
  "Get the plist of global org PROPERTY of current buffer."
  (unless property (setq property "PROPERTY"))
  (org-element-map
    (org-element-parse-buffer)
    'keyword
    (lambda (el)
      (when (string-match property (org-element-property :key el)) el))))

(cl-defun jf/blog-post/tootify ()
  "Create a toot from the current buffer."
  (interactive)
  (if (jf/org-mode/blog-entry?)
    (let* ((metadata (jf/org-keywords-as-plist :keywords-regexp "\\(ROAM_REFS\\|DESCRIPTION\\)"))
            (url (lax-plist-get metadata "ROAM_REFS"))
            (description (lax-plist-get metadata "DESCRIPTION")))
      (call-interactively #'mastodon-toot)
      (end-of-buffer)
      (insert (s-join "\n\n" (flatten-list (list description url)))))
    (user-error "Current buffer is not a blog post.")))

(cl-defun jf/jump_to_corresponding_hugo_file (&key (buffer (current-buffer)))
  "Find the TakeOnRules.com url in the BUFFER and jump to corresponding Hugo file."
  (interactive)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (if (re-search-forward "^\\#\\+ROAM_REFS:.+\\(https?://takeonrules\.com[^ \n]*\\)" nil t)
          (jf/tor-find-hugo-file-by-url (match-string 1))
          (message "Unable to find Take on Rules URL in buffer."))))))

(cl-defun jf/jump_to_corresponding_denote_file (&key (buffer (current-buffer)))
  "Find the org_id in the BUFFER and jump to corresponding `denote' file."
  (interactive)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (if (re-search-forward "^org_id: \\([[:digit:]]+T[[:digit:]]+\\)$" nil t)
          (find-file (denote-get-path-by-id (match-string 1)))
          (message "Unable to find Denote ID in buffer."))))))

(defun jf/org-mode-get-keyword-key-value (kwd)
  "Map KWD to list."
  (let ((data (cadr kwd)))
    (list (plist-get data :key)
      (plist-get data :value))))

(cl-defun jf/org-mode-extract-body-and-properties (node-id)
  "Extract quotable body and properties from NODE-ID."
  (with-current-buffer (find-file-noselect (org-id-find-id-file node-id))
    (list :properties (org-element-map (org-element-parse-buffer 'object)
                        '(keyword node-property)
                        #'jf/org-mode-get-keyword-key-value)
      :body (jf/org-mode-extract-body-from-current-buffer))))


(defun jf/org-mode-extract-body-from-current-buffer ()
  "Extract the body from the current `org-mode' body."
  (buffer-substring (save-excursion
                      (jf/org-mode-find-point-that-starts-body t)
                      (point))
    (org-entry-end-position)))

(defun jf/org-mode-find-point-that-starts-body (&optional unsafe)
  "Skip headline, planning line, and all drawers in current entry.

If UNSAFE is non-nil, assume point is on headline."
  (unless unsafe
    ;; To improve performance in loops (e.g. with `org-map-entries')
    (org-back-to-heading))
  (cl-loop for element = (org-element-at-point)
    for pos = (pcase element
                (`(headline . ,_) (org-element-property :contents-begin element))
                (`(,(or 'planning 'property-drawer 'node-property 'keyword 'drawer) . ,_) (org-element-property :end element)))
    while pos
    do (goto-char pos)))
;;******************************************************************************
;;
;;; BEGIN Non-Interactive Utility Functions
;;
;;******************************************************************************
(defun jf/tor-convert-text-to-post-title (title)
  "Convert TITLE to correct format."
  (message "Titleizing...")
  (replace-regexp-in-string
    ;; Replace "Hello World" with “Hello World”
    "\"\\([^\"]+\\)\""
    "“\\1”"
    (s-replace "'" "’" title)))

(defun jf/tor-convert-text-to-slug (&optional string)
  "Convert STRING to appropriate slug."
  (s-replace "'" "" (s-dashed-words (s-downcase string))))
;;******************************************************************************
;;
  ;;; END Non-Interactive Utility Functions
;;
;;******************************************************************************

;;******************************************************************************
;;
  ;;; BEGIN Interactive Non-Wrapping Functions
;;
;;******************************************************************************
(cl-defun jf/tor-toggle-hugo-server (&key
                                      (directory jf/tor-home-directory)
                                      (buffer-name "*Hugo Server*"))
  "This will start or stop a Hugo server in the given DIRECTORY.

  The BUFFER-NAME is where we'll run the Hugo process."
  (interactive)
  (if (get-buffer buffer-name)
    (progn
      (kill-buffer buffer-name)
      (message (concat "Stopping Hugo in \"" buffer-name "\" buffer…")))
    (let* ((default-directory directory))
      (start-process "hugo-server" buffer-name "hugo" "server" "-D")
      (message (concat "Starting Hugo in \"" buffer-name "\" buffer…")))))

(defvar jf/tor-hostname-regexp
  "^https?://takeonrules\.com"
  "A regular expression for checking if it's TakeOnRules.com.")

(defvar jf/tor-hugo-regexp-for-post-path
  (concat jf/tor-hostname-regexp
    "/[0-9]\\{4\\}/[0-9]\\{2\\}/[0-9]\\{2\\}/\\([^/]+\\)/?$")
  "A regular expression for matching blog posts.")

(defvar jf/tor-hugo-regexp-for-pages-path
  (concat jf/tor-hostname-regexp "/\\([^z-a]*[^/]\\)/?$")
  "A regular expression for matching pages.")

(defun jf/tor-find-hugo-file-by-url (url)
  "Find the associated TakeOnRules.com file for the given URL."
  (interactive (list
                 (jf/prompt-for-url-dwim
                   :url-regexp jf/tor-hostname-regexp)))
  (cond
    ;; Blog post
    ((string-match jf/tor-hugo-regexp-for-post-path url)
      (let* ((slug (match-string-no-properties 1 url))
              (filename (car
                          (jf/list-filenames-with-file-text
                            :matching (concat "^slug: " slug "$")
                            :in "content"))))
        (find-file (f-join jf/tor-home-directory "content" filename))))
    ;; Pages
    ((string-match jf/tor-hugo-regexp-for-pages-path url)
      (let* ((permalink (match-string-no-properties 1 url))
              (filename (car
                          (jf/list-filenames-with-file-text
                            :matching (concat "^permalink: ['\\\"]?/?"
                                        permalink "/?['\\\"]?$")
                            :in "content"))))
        (find-file (f-join jf/tor-home-directory "content" filename))))
    ;; No match found
    (t (message "Unable to find post for \"%s\"" url))))

(defun jf/tor-find-changelog-and-insert-entry ()
  "Find TakeOnRules glossary and begin entering a changelog entry."
  (interactive)
  (find-file (f-join jf/tor-home-directory "data" "changelog.yml"))
  ;; The changelog is structured in date descending order.  The first
  ;; line is the YAML preamble indicating a data object (e.g. "---")
  (goto-char (point-min))
  (end-of-line)
  (insert (concat "\n- date: "
            (format-time-string "%Y-%m-%d")
            "\n  entries:\n    - ")))

(defun jf/tor-find-series-and-insert-entry (title)
  "Find TakeOnRules series and add an entry with TITLE."
  (interactive "sSeries Entry's Title: ")
  (find-file (f-join jf/tor-home-directory "data" "series.yml"))
  (let ((key (downcase (s-dashed-words title))))
    (goto-char (point-max))
    (insert (concat
              (if (looking-at-p "^$") "" "\n")
              "- title: " title
              "\n  key: " key))))

;; Note: I needed to use `fboundp' because if I invoked this functions
;; before other consult functions I got a method void error.
(cl-defun jf/find-file-via-matching (&key prompt matching in (switch "--files-with-matches"))
  "PROMPT for files IN the directory with MATCHING content with given SWITCH.

If `consult--read' is defined, use that.  Otherwise fallback to
`completing-read'."
  (if (fboundp 'consult--read)
    (consult--read
      (consult--with-increased-gc
        (jf/list-full-filenames-with-file-text
          :matching matching
          :in in
          :switch switch))
      :prompt prompt
      :sort nil
      :require-match t
      :category 'file
      :history 'file-name-history
      :state (consult--file-preview))
    (list (completing-read
            prompt
            (jf/list-filenames-with-file-text
              :matching matching
              :in in)))))


;;******************************************************************************
;;
    ;;; END Interactive Non-Wrapping Functions
;;
;;******************************************************************************

;;******************************************************************************
;;
    ;;; BEGIN Listing functions for TakeOnRules.com data
;;
;;******************************************************************************
(defun jf/tor-tags-list ()
  "Return a list of tags from TakeOnRules.com."
  (jf/tor-list-by-key-from-filename :key "tag" :filename "data/glossary.yml"))

(defun jf/tor-epigraph-list ()
  "Return a list of epigraph keys from TakeOnRules.com."
  (jf/tor-list-by-key-from-filename :key "key" :filename "data/epigraphs.yml"))

(defun jf/tor-game-list ()
  "Return a list of games from TakeOnRules.com."
  (jf/tor-list-by-key-from-filename :key "game" :filename "data/glossary.yml"))

(defun jf/tor-glossary-title-list ()
  "Return a list of titles from TakeOnRules.com."
  (jf/tor-list-by-key-from-filename :key "title" :filename "data/glossary.yml"))

(defun jf/tor-glossary-key-list ()
  "Return a list of keys from TakeOnRules.com glossary."
  (jf/tor-list-by-key-from-filename :key "key" :filename "data/glossary.yml"))

(defun jf/tor-series-list ()
  "Return a list of series from TakeOnRules.com."
  (jf/tor-list-by-key-from-filename :key "key" :filename "data/series.yml"))

(defun jf/tor-licenses-list ()
  "Return a list of available licenses for TakeOnRules.com."
  (jf/tor-list-by-key-from-filename :key "Key" :filename "data/licenses.yml"))
;;******************************************************************************
;;
    ;;; END Listing functions for TakeOnRules.com data
;;
;;******************************************************************************

;;******************************************************************************
;;
    ;;; BEGIN querying and list generation functions
;;
;;******************************************************************************
(cl-defun jf/tor-list-by-key-from-filename (&key
                                             key
                                             filename
                                             (directory jf/tor-home-directory))
  "Build a list of entries of the KEY from the FILENAME in DIRECTORY."
  (split-string-and-unquote
    (shell-command-to-string
      (concat
        "rg \"^[- ] " key ": .*$\" "
        (f-join directory filename)
        " --only-matching --no-filename | sed 's/^[ -] " key ": //' | sort | tr '\n' '@'"))
    "@"))

(cl-defun jf/list-filenames-with-file-text (&key matching in)
  "Build a list of filenames MATCHING the pattern IN the given directory."
  (let ((default-directory (f-join jf/tor-home-directory in)))
    (split-string-and-unquote
      (shell-command-to-string
        (concat
          "rg \""
          matching "\" --only-matching --files-with-matches --sortr modified"
          "| tr '\n' '@'"))
      "@")))

(cl-defun jf/list-full-filenames-with-file-text (&key matching in (switch "--files-with-matches"))
  "List of filenames MATCHING with SWITCH the pattern IN the given directory."
  (split-string-and-unquote
    (shell-command-to-string
      (concat
        "rg \""
        matching "\" " in " --only-matching " switch " --sortr modified"
        "| tr '\n' '@'"))
    "@"))

(defun jf/tor-page-relative-pathname-list ()
  "Return a list of pages for TakeOnRules.com."
  (jf/list-filenames-with-file-text :matching "^title: " :in "content"))

(defun jf/tor-asset-relative-pathname-list ()
  "Return a list of image filenames for TakeOnRules.com."
  (let ((default-directory (f-join jf/tor-home-directory "assets" "images")))
    (split-string-and-unquote
      (shell-command-to-string "ls"))))

(defun jf/matches-in-buffer (regexp &optional buffer)
  "Return a list of matches of REGEXP in BUFFER or the current buffer if not given."
  (let ((matches))
    (save-match-data
      (save-excursion
        (with-current-buffer (or buffer (current-buffer))
          (save-restriction
            (widen)
            (goto-char 1)
            (while (search-forward-regexp regexp nil t 1)
              (push (match-string 0) matches)))))
      matches)))

(defun jf/kill-new-markdown-heading-as-slug (heading)
  "Push onto the `kill-ring' a slugified version of HEADING."
  (interactive
    (list (completing-read
            "Heading: "
            (jf/matches-in-buffer "^#+ +.*$"))))
  (kill-new (jf/tor-convert-text-to-slug
              (replace-regexp-in-string "^#+ +" "" heading))))

;;******************************************************************************
;;
    ;;; END querying and list generation functions
;;
;;******************************************************************************

(cl-defun jf/create-lore-24-blog-entry (&key (series "in-the-shadows-of-mont-brun")
                                         (keywords '("lore24" "rpgs")))
  "Create #Lore24 entry from current node.

Add the blog post to the given SERIES with the given KEYWORDS."
  (interactive)
  ;; Guard against running this on non- `jf/lore24-filename'.
  (unless (string=
            (jf/filename/tilde-based (buffer-file-name (current-buffer)))
            jf/lore24-filename)
    (user-error "You must be in %S" jf/lore24-filename))
  ;; Now that we know we're on the right buffer...
  (let* (
          ;; Get the node of the current entry I'm working from
          (node-id (org-id-get-create))

          ;; Prompt for a name.
          (name (read-string "#Lore24 Blog Post Name: "))

          ;; Determine the last blog post created.
          (previous-post-basename
            (s-trim (shell-command-to-string
                      (concat
                        "cd ~/git/org/denote/blog-posts; "
                        "find *--lore24-entry-* | sort | tail -1"))))

          ;; From the last blog post, derive the next index value.
          (next-index (format "%03d"
                        (+ 1
                          (string-to-number
                            (progn
                              (string-match
                                "--lore24-entry-\\([[:digit:]]+\\)-"
                                previous-post-basename)
                              (match-string-no-properties 1
                                previous-post-basename))))))

          ;; We must name the post.  "Lore 24 - Entry NNN: Name"
          (title (format "Lore24 - Entry %s: %s" next-index name ))

          ;; The body of the blog post; by default I leverage
          ;; `org-transclusion'.
          (template (format
                      (concat "#+HUGO_CUSTOM_FRONT_MATTER: :series %s"
                        "\n\n#+TRANSCLUDE: [[id:%s]] :only-contents "
                        ":exclude-elements \"drawer keyword headline\"")
                      series
                      node-id))
          ;; This will be a blog post.
          (directory (f-join (denote-directory) "blog-posts"))

          ;; Series are added as signature.
          (signature (denote-sluggify-signature series)))

    ;; Create the blog post
    (denote title keywords 'org directory nil template signature)))

(fset 'jf/tidy-ox-hugo-header-export
  (kmacro-lambda-form [?\C-c ?\C-n ?\s-f ?\{ return ?\C-b ?\C-b ?\C-k] 0 "%d"))

;;; Org-Mode link to table numbers.
(defun jf/path-to-table-number (table-number)
  (let* ((table-filename
	  (f-join jf/tor-home-directory "data/list_of_all_tables.yml"))
	 (path (s-trim (shell-command-to-string
			(concat "rg \"^- table_number:\\s" table-number "$\" -A4 "
				table-filename
				" | rg \"^ +path: +\\\"([^\\\"]+)\\\"\" -r '$1' "
				"--only-matching")))))
    (s-trim (shell-command-to-string
	     (concat "rg \"^#\\+ROAM_REFS: .*(https://takeonrules.com"
		     path ")\" -r '$1' --files-with-matches " org-directory)))))

(defun jf/list-blog-tables (&optional table-number)
  "Return a list of blog tables of the form \"TABLE-NUMBER: CAPTION\"."
  (let* ((table-filename
	  (f-join jf/tor-home-directory "data/list_of_all_tables.yml"))
	 (match (or table-number "\\d+")))
    (s-split
     "\n"
     (s-trim
      (shell-command-to-string
       (concat "rg \"^- table_number: (" match ")\\n\\s+caption: (.*)\""
	       " --multiline --replace '$1: $2' --only-matching "
	       table-filename))))))

(defun jf/org/link-table-complete ()
  "Complete a file from table."
  (completing-read "Table: " (jf/list-blog-tables) nil t))

(defun jf/org/link-table-follow (link _)
  "Open a file from table for LINK."
  (find-file (jf/path-to-table-number link)))

(defun jf/org/link-table-export (table-number desc format _)
  "Export TABLE-NUMBER with DESC for FORMAT."
  (let ((desc (concat "Table " table-number ": "
		            (string-trim
		              (cadr
			              (s-split ":"
				              (car (jf/list-blog-tables table-number)))))))
	       (link (format "https://takeonrules.com/tables/%s" table-number)))
    (pcase format
	    ((or 'html '11ty)
	      (if jf/exporting-org-to-tor
	        (format "{{< linkToTable %s >}}" table-number)
	        (format "<a href=\"%s\">%s</a>" link desc)))
      ('md (if jf/exporting-org-to-tor
		         (format "{{< linkToTable %s >}}" table-number)
		         (format "[%s](%s)" desc link)))
      ('latex (format "\\href{%s}{%s}" link desc))
      ('texinfo (format "@uref{%s,%s}" link desc))
      ('ascii (format "%s (%s)" desc link))
      (_ (format "%s (%s)" desc link)))))


  (org-link-set-parameters
   "table"
   :complete #'jf/org/link-table-complete
   :export #'jf/org/link-table-export
   :follow #'jf/org/link-table-follow))

(provide 'jf-blogging)
;;; jf-blogging.el ends here
