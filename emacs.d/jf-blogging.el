;;; jf-blogging.el --- Blogging related functions -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;; Packages specifically here for helping with my writing activities.

;;; Code
(require 'ox)

(require 'jf-formatting)

(use-package ox-hugo
  :straight t
  :custom (org-hugo-paired-shortcodes "marginnote sidenote poem")
  :after ox)

(setq org-hugo-base-dir "~/git/takeonrules.source")

(defvar jf/org-macros-setup-filename
  "~/git/dotemacs/lib/org-macros.setup"
  "The path to the file that has inline org macros.")

(with-eval-after-load 'ox-hugo
  (add-to-list 'org-hugo-special-block-type-properties '("sidenote" . (:trim-pre t :trim-post t))))

(defun jf/org-html-verse-block (_verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to HTML.
  CONTENTS is verse block contents.  INFO is a plist holding
  contextual information."
  (format "<section class=\"verse\">\n%s</section>"
	  ;; Replace leading white spaces with non-breaking spaces.
	  (replace-regexp-in-string
	   "^[ \t]+" (lambda (m) (org-html--make-string (length m) "&#xa0;"))
	   ;; Replace each newline character with line break.  Also
	   ;; remove any trailing "br" close-tag so as to avoid
	   ;; duplicates.
	   (let* ((br (org-html-close-tag "br" nil info))
		  (re (format "\\(?:%s\\)?[ \t]*\n" (regexp-quote br))))
	     (replace-regexp-in-string re (concat br "\n") contents)))))


(cl-defun jf/org-markdown-export-format-link-for (&key node desc)
  "Return a \"link\" text based on the given NODE and DESC.

  This relates to my glossary.html Hugo short-code."
  (when-let (url (jf/org-roam-external-url-for :node node))
    (let ((key (jf/org-roam-node-get-org-mode-property :node node :property "GLOSSARY_KEY")))
      (cond
       ((jf/org-roam-node-get-org-mode-property :node node :property "OFFER")
	(format "{{< glossary key=\"%s\" >}}" key))
       ((jf/org-roam-node-get-org-mode-property :node node :property "SAME_AS")
	(format "{{< glossary key=\"%s\" link=\"sameAs\" >}}" key))
       (t (format "[%s](%s)" desc url))))))

  ;;; For testing:
;;
;; (message "%s" (jf/org-markdown-export-format-link-for :node (org-roam-node-from-id "FC017488-D8EC-43DE-A35D-4D10A87B6A0D") :desc "Burning Wheel Gold"))
;; (message "%s" (jf/org-markdown-export-format-link-for :node (org-roam-node-from-id "86F3E44F-AA0E-4B08-B0D8-30A764B4CD13") :desc "Org Roam"))


(defcustom jf/exporting-org-to-tor nil
  "True while performing the export of org file to Take on Rules")

(cl-defun jf/export-org-to-tor (&key (buffer (current-buffer)))
  "Export current org buffer for TakeOnRules post."
  (interactive)
  ;; Ensure that we have an ID property.
  (setq jf/exporting-org-to-tor t)
  (with-current-buffer buffer
    (save-excursion
      (let* ((export-global-plist (jf/org-global-props-as-plist))
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
	     (subtitle (jf/export-org-to-tor--global-buffer-prop-ensure
			:key "SUBTITLE"
			:plist export-global-plist))
	     (title (lax-plist-get export-global-plist "TITLE"))
	     (identifier (lax-plist-get export-global-plist "IDENTIFIER")))
	(save-buffer)
	(jf/export-org-to-tor--inject-additional-front-matter
	 :subtitle subtitle
	 :title title
	 :identifier identifier)
	;; Write metadata
	(save-buffer)
	(unless org-transclusion-mode (org-transclusion-mode))
	(org-open-file (org-hugo-export-wim-to-md nil nil t)))))
  (setq jf/exporting-org-to-tor nil))

(fset 'jf/tidy-ox-hugo-header-export
      (kmacro-lambda-form [?\C-c ?\C-n ?\s-f ?\{ return ?\C-b ?\C-b ?\C-k] 0 "%d"))

(cl-defun jf/export-org-to-tor--inject-additional-front-matter (&key identifier subtitle title)
  "Export additional front matter.

    We want to ensure that we export the IDENTIFIER, SUBTITLE, and TITLE.
    And add relevant metadata."
  (beginning-of-buffer)
  (search-forward-regexp "#\\+HUGO_FRONT_MATTER_FORMAT: yaml")
  (insert (concat
	   "\n#+HUGO_CUSTOM_FRONT_MATTER: :slug " (jf/tor-convert-text-to-slug title)
	   "\n#+HUGO_CUSTOM_FRONT_MATTER: :headline " subtitle
	   ;; 2022-02-26 07:46:15.000000000 -04:00
	   "\n#+HUGO_CUSTOM_FRONT_MATTER: :date " (format-time-string "%Y-%m-%d %H:%M:%S %z")
	   "\n#+HUGO_CUSTOM_FRONT_MATTER: :type post"
	   "\n#+HUGO_CUSTOM_FRONT_MATTER: :layout post"
	   "\n#+HUGO_CUSTOM_FRONT_MATTER: :licenses '(all-rights-reserved)"
	   "\n#+HUGO_CUSTOM_FRONT_MATTER: :draft true"
	   "\n#+HUGO_CUSTOM_FRONT_MATTER: :org_id " identifier
	   "\n#+INCLUDE: " jf/org-macros-setup-filename)
	  )
  (let ((date (car (org-property-values "SESSION_REPORT_DATE"))))
    (when date
      (let ((game (car (org-property-values "SESSION_REPORT_GAME")))
	    (location (car (org-property-values "SESSION_REPORT_LOCATION"))))
	(insert
	 (format
	  "\n#+HUGO_CUSTOM_FRONT_MATTER: :sessionReport '((date . \"%s\") (game . \"%s\") (location . \"%s\"))"
	  date game location))))))

(cl-defun jf/export-org-to-tor--global-buffer-prop-ensure (&key key plist (default nil))
  "Ensure the current buffer has the given KEY in the global PLIST, if not set the DEFAULT or prompt for it."
  (let ((value (lax-plist-get plist key)))
    (if value value
      (jf/export-org-to-tor--global-buffer-prop-set
       :key key
       :value (or default (read-from-minibuffer (format "%s: " key)))))))

(cl-defun jf/export-org-to-tor--global-buffer-prop-set (&key key value)
  "Set the global property named KEY to the VALUE for the current buffer"
  (goto-line 5)
  (insert (format "\n#+%s: %s" (upcase key) value)))

(defvar jf/tor-session-report-location
  '("around the table" "via Zoom" "via Discord and Roll20" "via Discord")
  "TakeOnRules session report locations")

(cl-defun jf/org-tag-as-session-report (&key (buffer (current-buffer)))
  "Set the current BUFFER as a \"session-report\".

    This involves adding a FILETAG and metadata around the details of the session report."
  (interactive)
  (message "TODO: Adjust for Denote methodology")
  ;; (with-current-buffer buffer
  ;;   (save-excursion
  ;;     (beginning-of-buffer)
  ;;     (org-roam-tag-add '("sessions"))
  ;;     (let* ((date (org-read-date nil nil nil "Session Date"))
  ;; 	     (game (completing-read "Game: " (jf/tor-game-list)))
  ;; 	     (location (completing-read "Location: " jf/tor-session-report-location)))
  ;; 	(org-set-property "SESSION_REPORT_DATE" date)
  ;; 	(org-set-property "SESSION_REPORT_GAME" game)
  ;; 	(org-set-property "SESSION_REPORT_LOCATION" location))))
  )

(cl-defun jf/org-global-props-as-plist (&key (props-regexp "\\(IDENTIFIER\\|FILETAGS\\|HUGO_FRONT_MATTER_FORMAT\\|HUGO_SECTION\\|HUGO_BASE_DIR\\|TITLE\\|SUBTITLE\\)"))
  (-flatten (mapcar (lambda (prop)
		      (list (org-element-property :key prop)
			    (org-element-property :value prop)))
		    (jf/org-global-props props-regexp))))

(defun jf/org-global-props (&optional property)
  "Get the plists of global org properties of current buffer."
  (unless property (setq property "PROPERTY"))
  (org-element-map
      (org-element-parse-buffer)
      'keyword
    (lambda (el)
      (when (string-match property (org-element-property :key el)) el))))

(cl-defun jf/jump_to_corresponding_hugo_file (&key (buffer (current-buffer)))
  "Find the TakeOnRules.com url in the BUFFER and jump to corresponding Hugo file."
  (interactive)
  (with-current-buffer buffer
    (save-excursion
      (beginning-of-buffer)
      (save-match-data
	(if (re-search-forward "\n:ROAM_REFS:.+\\(https?://takeonrules\.com[^ \n]*\\)" nil t)
	    (jf/tor-find-hugo-file-by-url (match-string 1))
	  (message "Unable to find Take on Rules URL in buffer."))))))

(cl-defun jf/org-tag-session-scene-with-date (date &key (tags '("scene")) (buffer (current-buffer)))
  "Tag the BUFFER with the TAGS and prompt for the DATE in which the scene occurred."
  (interactive (list (completing-read "Scene Date: " (jf/org-macro-value-list "scene-date"))))
  (save-excursion
    (org-roam-tag-add tags)
    (beginning-of-buffer)
    (search-forward "#+FILETAGS:")
    (next-line)
    (insert (concat "\n{{{scene-date(" date ")}}}\n"))))

(defun jf/blockquote-hugo (node-id)
  "Export the blockquote for the given NODE-ID"
  (let ((data (jf/org-mode-extract-body-and-properties node-id)))
    (concat
     "\n{{{< blockquote " (jf/hugo-blockquote-attributes-for (plist-get data :properties)) ">}}}\n"
     (format "%s" (plist-get data :body))
     "\n{{{< /blockquote >}}}\n")))

(defun jf/hugo-blockquote-attributes-for (properties)
  "Map the PROPERTIES to attributes."
  (seq-mapcat (lambda (element)
		(let ((key (car element))
		      (text (cadr element)))
		  (pcase key
		    ("ID" (format "orgId=\"%s\" " text))
		    ("TITLE" (format "cite=\"%s\" " text))
		    ("CITE_URL" (format "citeUrl=\"%s\" " text))
		    ("AUTHOR" (format "pre=\"%s\" " text))
		    ("CITE_POST" (format "post=\"%s\" " text))
		    (_ ""))))
	      properties))

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
  "Extract the body from the current org-mode body"
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
(cl-defun jf/convert-text-to-key (text &key (length 5))
  "Convert the given TEXT to an epigraph key.

  The LENGTH is how many words to use for the key."
  (let ((list-of-words (s-split-words text)))
    (if (> (length list-of-words) length)
	(upcase (s-join "-" (subseq list-of-words 0 length)))
      "")))

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

(defun jf/slugify ()
  "Convert the active region or line to a URL friendly slug."
  (interactive)
  (let* ((range (if (region-active-p)
		    (list (region-beginning) (region-end))
		  (list (point-at-bol) (point-at-eol))))
	 (text (buffer-substring-no-properties (car range) (cadr range))))
    (save-excursion
      (delete-region (car range) (cadr range))
      (goto-char (car range))
      (insert (jf/tor-convert-text-to-slug text)))))

(cl-defun jf/tor-post---create-or-append (&key
					  title subheading
					  (tags '("null")) series toc
					  citeTitle citeURL citeAuthor)
  "Create or append a post with TITLE.

    The following keys are optional:

    :SUBHEADING if you have an active region, use this header.
    :TAGS one or more tags, as a list or string, to add to the
	    frontmatter.
    :SERIES the series to set in the frontmatter.
    :TOC whether to include a table of contents in the post.
    :CITETITLE the title of the URL cited (if any)
    :CITEURL the URL cited (if any)
    :CITEAUTHOR the author cited (if any)

    If there's an active region, select that text and place it."
  (let* ((default-directory (f-join jf/tor-home-directory
				    "content" "posts"
				    (format-time-string "%Y/")))

	 (slug (jf/tor-convert-text-to-slug title))
	 (fpath (expand-file-name
		 (concat default-directory slug ".md"))))
    ;; If the file does not exist, create the file with the proper
    ;; frontmatter.
    (if (not (file-exists-p fpath))
	(write-region
	 (concat "---"
		 "\ndate: " (format-time-string "%Y-%m-%d %H:%M:%S %z")
		 "\ndraft: true"
		 "\nlayout: post"
		 "\nlicenses:\n- all-rights-reserved"
		 "\nslug: " (format "%s" slug)
		 "\ntitle: '" (jf/tor-convert-text-to-post-title title) "'"
		 "\ntype: post"
		 (when series (concat "\nseries: " series))
		 (when toc (concat "\ntoc: true"))
		 "\ntags:"
		 (if tags
		     (concat (mapconcat
			      (lambda (tag) (concat "\n- " tag))
			      (flatten-tree tags) ""))
		   "\n- null")
		 "\n---\n")
	 nil fpath))
    ;; If we have an active region, append that region's content to
    ;; the given file.
    (if (use-region-p)
	(write-region
	 (concat
	  (if subheading
	      (concat "\n## " subheading "\n")
	    (when citeTitle (concat "\n## " citeTitle "\n")))
	  (when citeURL (concat
			 "\n{{< blockquote"
			 (when citeAuthor
			   (concat " pre=\"" citeAuthor "\""))
			 " cite=\"" citeTitle
			 "\" cite_url=\"" citeURL "\" >}}\n"))
	  (buffer-substring (region-beginning) (region-end))
	  (when citeURL "\n{{< /blockquote >}}"))
	 nil fpath t)
      ;; Without an active region, if we have a citeURL insert a link
      ;; to it.
      (when citeURL
	(write-region
	 (concat
	  "\n<cite><a href=\"" citeURL
	  "\" class=\"u-url p-name\" rel=\"cite\">"
	  (or (citeTitle) (citeURL)) "</a></cite>\n")
	 nil fpath t)))
    ;; Finally open that file for editing.
    (find-file fpath)
    (end-of-buffer)))
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
  "This will start or stop a Hugo server in the given DIRECTORY for TakeOnRules.com.

  The BUFFER-NAME is where we'll run the Hugo process."
  (interactive)
  (if (get-buffer buffer-name)
      (progn
	(kill-buffer buffer-name)
	(message (concat "Stopping Hugo in \"" buffer-name "\" buffer…")))
    (let* ((default-directory directory))
      (start-process "hugo-server" buffer-name "hugo" "server" "-D")
      (message (concat "Starting Hugo in \"" buffer-name "\" buffer…")))))

(defun jf/tor-retitle-post (title)
  "Replace the given buffer's title with the new TITLE.

    This function will: replace the content's title, update the slug,
    and rename the buffer."
  (interactive "sNew Post's Title: ")
  (let* ((metadataTitle (concat "title: '"
				(jf/tor-convert-text-to-post-title title) "'"))
	 (slug (jf/tor-convert-text-to-slug title))
	 (metadataSlug (concat "slug: " slug))
	 (filename (buffer-file-name))
	 (new-filename (concat (file-name-directory filename)
			       slug ".md")))

    ;; Replace the title metadata entry
    (goto-char (point-min))
    (while (search-forward-regexp "^title:.*$" nil t)
      (replace-match metadataTitle))

    ;; Replace the slug metadata entry
    (goto-char (point-min))
    (while (search-forward-regexp "^slug:.*$" nil t)
      (replace-match metadataSlug))

    ;; Need to save before we rename the buffer
    (save-buffer)

    ;; Rename the buffer, accounting for version control
    (cond
     ((vc-backend filename)
      (vc-rename-file filename new-filename))
     (t
      (rename-file filename new-filename t)
      (set-visited-file-name new-filename t t)))

    ;; Report filename change
    (message "Renamed %s -> %s" filename new-filename)))

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

(cl-defun jf/tor-view-blog-post (&key
				 (hostname jf/tor-hostname-current))
  "Browse the url for the HOSTNAME

    The front matter of blog posts contains YAML, with two
    attributes: slug and date.  Based on the site configuration, the
    URLs for one of those posts is: hostname/year/month/day/slug"
  (interactive)
  (let ((slugs))
    (save-excursion
      ;; Remember we are making a list and pushing to the beginning of
      ;; the list.  Hence we start with the last slug in mind.
      (goto-char 1)
      (re-search-forward "^slug: \\(.*\\)$" nil t)
      (push (match-string 1) slugs)
      (goto-char 1)
      (re-search-forward
       "^date: \\([[:digit:]]+\\)-\\([[:digit:]]+\\)-\\([[:digit:]]+\\) "
       nil t)
      ;; Then move to day, month, then year.
      (push (match-string 3) slugs)
      (push (match-string 2) slugs)
      (push (match-string 1) slugs)
      ;; And finally the host name.
      (push hostname slugs))
    (browse-url (format "%s" (s-join "/" slugs)))))

(defun jf/tor-create-post (title)
  "Create and visit a new draft post.  Prompt for a TITLE.

    The file for the blog post conforms to the path schema of posts
    for TakeOnRules.com."
  (interactive "sBlog Post Title: ")
  (jf/tor-post---create-or-append :title title))

(defun jf/tor-tag-post (tags)
  "Apply the TAGS to the current TakeOnRules.com post.

    No effort is made to check if this is a post."
  (interactive (list (completing-read-multiple "Tags: " (jf/tor-tags-list))))
  (let ((saved-point (point))
	(to-insert (concat "\n- " (s-join "\n- " tags))))
    (replace-regexp "^tags:$" (concat "tags:" to-insert) nil 0 (point-max))
    (goto-char (+ saved-point (length to-insert)))))

(defun jf/tor-insert-glossary-key (key)
  "Insert the KEY at point."
  (interactive (list (completing-read "Key: " (jf/tor-glossary-key-list))))
  (insert key))

(defun jf/tor-find-changelog-and-insert-entry ()
  "Find TakeOnRules glossary and begin entering a changelog entry."
  (interactive)
  (find-file (f-join jf/tor-home-directory "data" "changelog.yml"))
  ;; The changelog is structured in date descending order.  The first
  ;; line is the YAML preamble indicating a data object (e.g. "---")
  (beginning-of-buffer)
  (end-of-line)
  (insert (concat "\n- date: "
		  (format-time-string "%Y-%m-%d")
		  "\n  entries:\n    - ")))

(defun jf/tor-find-series-and-insert-entry (title)
  "Find TakeOnRules series and add an entry with TITLE."
  (interactive "sSeries Entry's Title: ")
  (find-file (f-join jf/tor-home-directory "data" "series.yml"))
  (let ((key (downcase (s-dashed-words title))))
    (end-of-buffer)
    (insert (concat
	     (if (looking-at-p "^$") "" "\n")
	     "- title: " title
	     "\n  key: " key))))

(defun jf/tor-find-glossary-and-insert-entry (title)
  "Find TakeOnRules glossary and add an entry with TITLE."
  (interactive "sGlossary Entry's Title: ")
  (find-file (f-join jf/tor-home-directory "data" "glossary.yml"))
  (let ((key (upcase (s-dashed-words title))))
    (end-of-buffer)
    (insert (concat
	     (if (looking-at-p "^$") "" "\n")
	     "- title: " title
	     "\n  key: " key))))

(defun jf/tor-insert-epigraph-entry ()
  "Prompt for a new a new data/epigraphs.yml entry."
  (interactive)
  (find-file (f-join jf/tor-home-directory "data" "epigraphs.yml"))
  (end-of-buffer)
  (insert (concat
	   (if (looking-at-p "^$") "" "\n")
	   "epi"))
  (end-of-buffer)
  "Assumes that the 'epi' is the correct expansion for the snippet."
  (yas-expand)
  (message "Ready to insert a new epigraph"))

(cl-defun jf/tor-post-amplifying-the-blogosphere (subheading
						  &key
						  citeTitle
						  citeURL
						  citeAuthor)
  "Create and visit draft post for amplifying the blogosphere.

    If there's an active region, prompt for the :SUBHEADING.  The file
    for the blog post conforms to the path schema of posts for
    TakeOnRules.com.

    We'll pass the :CITETITLE, :CITEAUTHOR, and :CITEURL to
    `jf/tor-post---create-or-append'"
  (interactive (list (if (use-region-p)
			 (read-string "Sub-Heading: ")
		       nil)))
  (jf/tor-post---create-or-append
   :title (format-time-string "Amplifying the Blogosphere (v%Y-%m-%d)")
   :toc "true"
   :subheading subheading
   :series "amplifying-the-blogosphere"
   :tags "response to other blogs"
   :citeTitle citeTitle
   :citeURL citeURL
   :citeAuthor citeAuthor))

;; Note: I needed to use `fboundp' because if I invoked this functions
;; before other consult functions I got a method void error.
(cl-defun jf/find-file-via-matching (&key prompt matching in)
  "PROMPT for files IN the directory with MATCHING content.

    If `consult--read' is defined, use that.  Otherwise fallback to `completing-read'."
  (if (fboundp 'consult--read)
      (consult--read
       (consult--with-increased-gc
	(jf/list-full-filenames-with-file-text :matching matching :in in))
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

(defun jf/tor-find-file-draft (filename)
  "Find a draft FILENAME in the TakeOnRules content directory."
  (interactive
   (list (jf/find-file-via-matching
	  :prompt "Draft filename: "
	  :matching "^draft: true"
	  :in (f-join jf/tor-home-directory "content"))))
  (find-file filename))

(defun jf/tor-find-file (filename)
  "Find a FILENAME in the TakeOnRules content directory."
  (interactive
   (list (jf/find-file-via-matching
	  :prompt "Filename: "
	  :matching "^title:"
	  :in (f-join jf/tor-home-directory "content"))))
  (find-file filename))
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

(cl-defun jf/list-full-filenames-with-file-text (&key matching in)
  "Build a list of filenames MATCHING the pattern IN the given directory."
  (split-string-and-unquote
   (shell-command-to-string
    (concat
     "rg \""
     matching "\" " in " --only-matching --files-with-matches --sortr modified"
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

;;******************************************************************************
;;
    ;;; BEGIN Wrapping Functions
;;
;;******************************************************************************
(cl-defun jf/tor-wrap-with-text (&key before after strategy)
  "Wrap the STRATEGY determined region with the BEFORE and AFTER text.

    Valid STRATEGY options are:

    - :lineOrRegion
    - :pointOrRegion
    - :sentenceOrRegion
    - :wordOrRegion

    TODO: I would love create a lookup table for the case statement,
    as the behavior's well defined."
  (pcase strategy
    (:lineOrRegion (pcase-let* ((origin (point))
				(`(,begin . ,end)
				 (crux-get-positions-of-line-or-region)))
		     (goto-char end)
		     (insert after)
		     (goto-char begin)
		     (insert before)))
    (:sentenceOrRegion (let* ((begin (if (use-region-p)
					 (region-beginning)
				       (car (bounds-of-thing-at-point 'sentence))))
			      (end (if (use-region-p)
				       (region-end)
				     (cdr (bounds-of-thing-at-point 'sentence)))))
			 (goto-char end)
			 (insert after)
			 (goto-char begin)
			 (insert before)))
    (:pointOrRegion (let* ((begin (if (use-region-p) (region-beginning) (point)))
			   (end (if (use-region-p) (region-end) (point))))
		      (goto-char end)
		      (insert after)
		      (goto-char begin)
		      (insert before)))
    (:wordOrRegion (let* ((begin (if (use-region-p)
				     (region-beginning)
				   (car (bounds-of-thing-at-point 'word))))
			  (end (if (use-region-p)
				   (region-end)
				 (cdr (bounds-of-thing-at-point 'word)))))
		     (goto-char end)
		     (insert after)
		     (goto-char begin)
		     (insert before)))
    ))

(defun jf/tor-wrap-in-html-tag (tag &optional attributes)
  "Wrap the word or region with the given TAG with optional ATTRIBUTES."
  (interactive "sHTML Tag: \nsAttributes (optional): ")
  (jf/tor-wrap-with-text
   :before (concat "<" tag (if (s-blank? attributes)
			       ""
			     (concat " " attributes)) ">")
   :after (concat "</" tag ">")
   :strategy :wordOrRegion))

(defun jf/tor-wrap-in-poem ()
  "Wrap the point or region as a poem."
  (interactive)
  (jf/tor-wrap-with-text
   :before "<pre class=\"poem\">\n"
   :after "\n</pre>"
   :strategy :pointOrRegion))

(defun jf/tor-wrap-date (date)
  "Wrap the point or region with the given DATE."
  (interactive (list (org-read-date nil nil nil "Date")))
  (jf/tor-wrap-in-html-tag
   "time"
   (concat "datetime=\"" date "\" title=\"" date "\"")))

(defun jf/tor-wrap-as-marginnote-dwim ()
  "Wrap the line or current region as a marginnote Hugo shortcode."
  (interactive)
  (jf/tor-wrap-with-text
   :before "{{< marginnote >}}\n"
   :after "\n{{< /marginnote >}}"
   :strategy :lineOrRegion))

(defun jf/tor-wrap-as-sidenote-dwim ()
  "Wrap the line or current region as a sidenote Hugo shortcode."
  (interactive)
  (jf/tor-wrap-with-text
   :before "{{< sidenote >}}"
   :after "{{< /sidenote >}}"
   :strategy :sentenceOrRegion))

(defun jf/tor-wrap-link-active-region-dwim (url)
  "Wrap current region (or point) in an A-tag with the given URL.

    For the URL:

    - If `car' of `kill-ring' starts with \"http\", then use that as the URL.
    - Otherwise prompt for a URL.

    If the URL is an empty string, then send a message.  Else, if we
    have a non-0 length URL, use the URL and wrap the region in an A
    tag."
  (interactive (list (jf/prompt-for-url-dwim)))
  (if (eq (length url) 0)
      (message "No URL to use for A-tag creation")
    (jf/tor-wrap-with-text
     :before (concat "<a href=\"" url "\">")
     :after "</a>"
     :strategy :pointOrRegion)))

(defun jf/tor-wrap-as-pseudo-dfn ()
  "Wrap current region (or word) in an I-tag with a DFN dom class."
  (interactive)
  (jf/tor-wrap-with-text
   :before "<i class=\"dfn\">"
   :after "</i>"
   :strategy :wordOrRegion))

(defun jf/tor-wrap-cite-active-region-dwim (url)
  "Wrap current region (or point) in a CITE-tag and optional A-tag with URL.

    For the URL:

    - If `car' of `kill-ring' starts with \"http\", then use that as the URL.
    - Otherwise prompt for a URL.

    If the URL an empty string, then wrap the current region or point
    in a CITE tag.  Else, if we have a non-0 length URL, wrap it in
    CITE and A tag."
  (interactive (list (jf/prompt-for-url-dwim)))

  ;; Were we to start writing at the START position, we'd invariably
  ;; change the contents such that the END position was no longer
  ;; accurate.  So instead, we append at the END position, hop back to
  ;; the START position and append to the START position.
  (if (eq (length url) 0)
      (jf/tor-wrap-with-text
       :before "<cite>"
       :after "</cite >"
       :strategy :pointOrRegion)
    (jf/tor-wrap-with-text
     :before (concat "<cite><a href=\"" url
		     "\" class=\"u-url p-name\" rel=\"cite\">")
     :after "</a></cite>"
     :strategy :pointOrRegion)))
;;******************************************************************************
;;
    ;;; END Wrapping Functions
;;
;;******************************************************************************


(fset 'jf/tidy-ox-hugo-header-export
      (kmacro-lambda-form [?\C-c ?\C-n ?\s-f ?\{ return ?\C-b ?\C-b ?\C-k] 0 "%d"))

(provide 'jf-blogging)
;;; jf-blogging.el ends here
