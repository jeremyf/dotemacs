;;; -*- lexical-binding: t; -*- jnf-blogging.el --- Summary
;;
;;; Commentary:
;;
;;  This package includes numerous tools for helping me with my
;;  blogging efforts.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/jrblevin/markdown-mode/
(use-package markdown-mode
  :straight t
  ;; I use markdown for my blogging platform and very little else.
  ;; Hence, I have this keybind.
  :bind (:map markdown-mode-map (("C-c t" . jnf/tor-subject-menu-markdown/body)
                                 ("C-M-s-t" . jnf/tor-subject-menu-markdown/body))))

(use-package decide
  :straight (decide :host github :type git :repo "jeremyf/decide-mode"))

;;******************************************************************************
;;
;;; BEGIN Menu Declarations
;;
;;******************************************************************************

;; These menu commands, plus some yasnippets, are some useful
;; functions for helping my blogging effort.
(defvar jnf/tor-menu--title
  (with-faicon "pencil-square" "Take on Rules" 1 -0.05)
  "The TakeOnRules.com Subject Menu Title.")
(pretty-hydra-define jnf/tor-subject-menu-markdown (:foreign-keys warn :title jnf/tor-menu--title :quit-key "q" :exit t)
  ("Wrapping"
   (("w a" jnf/tor-wrap-link-active-region-dwim "[A] link at point or region…")
    ("w c" jnf/tor-wrap-cite-active-region-dwim "[C]ite point or region…")
    ("w d" jnf/tor-wrap-date "[D]ate point or region…")
    ("w f" jnf/tor-wrap-as-pseudo-dfn "Wrap word or region in pseudo-d[f]n…")
    ("w m" jnf/tor-wrap-as-marginnote-dwim "[M]argin-note line or region…")
    ("w p" jnf/tor-wrap-in-poem "Wrap point or region as [P]oem…")
    ("w s" jnf/tor-wrap-as-sidenote-dwim "[S]ide-note sentence or region…")
    ("w w" jnf/tor-wrap-in-html-tag "[W]rap point or region in html…"))
   "Posts"
   (("p r" jnf/tor-retitle-post "[R]e-title post…")
    ("p t" jnf/tor-tag-post "[T]ag post…" :exit nil)
    ("p v" jnf/tor-view-blog-post "[V]iew post…"))
   "Utilities"
   (("c a" jnf/tor-post-amplifying-the-blogosphere "Create [a]mplify the blogosphere…")
    ("c e" jnf/tor-insert-epigraph-entry "Create [e]pigraph entry…")
    ("c g" jnf/tor-find-glossary-and-insert-entry "Create [g]lossary entry…")
    ("c c" jnf/tor-find-changelog-and-insert-entry "Create [c]hange log entry…")
    ("c p" jnf/tor-create-post "Create [p]ost…")
    ("c s" jnf/tor-find-series-and-insert-entry "Create [s]eries…")
    ("k h" jnf/kill-new-markdown-heading-as-slug "Kill slug version of given [h]eading…")
    ("? d" jnf/tor-find-file-draft "Find blog in [d]raft status…")
    ("? u" jnf/tor-find-hugo-file-by-url "Find blog by [u]rl…")
    ("? f" jnf/tor-find-file "Find blog by [f]ilename…")
    ("t d" decide-mode "[D]ecide Mode" :toggle t)
    ("t h" (jnf/tor-toggle-hugo-server :buffer-name "*Hugo Server*") "Toggle [H]ugo server" :toggle (get-buffer "*Hugo Server*"))
    ("t v" variable-pitch-mode "Toggle [v]ariable pitch mode" :toggle t))))

;; The `C-c t' key combo is engrained for my TakeOnRules incantations;
;; there's a markdown menu but if I'm not in markdown, it likely means
;; I'm not in Take on Rules pages.
(global-set-key (kbd "C-M-s-t") 'jnf/tor-subject-menu-default/body)
(global-set-key (kbd "C-c t") 'jnf/tor-subject-menu-default/body)

(pretty-hydra-define jnf/tor-subject-menu-default (:foreign-keys warn :title jnf/tor-menu--title :quit-key "q" :exit t)
  ("Utilities"
   (("c a" jnf/tor-post-amplifying-the-blogosphere "Create [a]mplify the blogosphere…")
    ("c e" jnf/tor-insert-epigraph-entry "Create [e]pigraph entry…")
    ("c g" jnf/tor-find-glossary-and-insert-entry "Create [g]lossary entry…")
    ("c c" jnf/tor-find-changelog-and-insert-entry "Create [c]hange log entry…")
    ("c p" jnf/tor-create-post "Create [p]ost…")
    ("c s" jnf/tor-find-series-and-insert-entry "Create [s]eries…")
    ("? d" jnf/tor-find-file-draft "Find blog in [d]raft status…")
    ("? u" jnf/tor-find-hugo-file-by-url "Find blog by [u]rl…")
    ("? f" jnf/tor-find-file "Find blog by [f]ilename…")
    ("t d" decide-mode "[D]ecide Mode" :toggle t)
    ("t h" (jnf/tor-toggle-hugo-server :buffer-name "*Hugo Server*") "Toggle [H]ugo server" :toggle (get-buffer "*Hugo Server*"))
    ("t v" variable-pitch-mode "Toggle [v]ariable pitch mode"))))

;;******************************************************************************
;;
;;; END Menu Declarations
;;
;;******************************************************************************

;;******************************************************************************
;;
;;; BEGIN Non-Interactive Utility Functions
;;
;;******************************************************************************
(cl-defun jnf/convert-text-to-key (text &key (length 5))
  "Convert the given TEXT to an epigraph key.

The LENGTH is how many words to use for the key."
  (let ((list-of-words (s-split-words text)))
    (if (> (length list-of-words) length)
        (upcase (s-join "-" (subseq list-of-words 0 length)))
      "")))

(defun jnf/tor-convert-text-to-post-title (title)
  "Convert TITLE to correct format."
  (message "Titleizing...")
  (replace-regexp-in-string
   ;; Replace "Hello World" with “Hello World”
   "\"\\([^\"]+\\)\""
   "“\\1”"
   (s-replace "'" "’" title)))

(defun jnf/tor-convert-text-to-slug (string)
  "Convert STRING to appropriate slug."
  (s-replace "'" "" (s-dashed-words (s-downcase string))))

(cl-defun jnf/tor-prompt-or-kill-ring-for-url (&key (url-regexp "^https?://"))
  "Prompt and return a url.

If the `car' of `kill-ring' matches the URL-REGEXP, default the
prompt value to the `car' of `kill-ring'."
  (let ((car-of-kill-ring (substring-no-properties (car kill-ring))))
    (read-string "URL (optional): "
                 (when (string-match url-regexp car-of-kill-ring)
                   car-of-kill-ring))))

(cl-defun jnf/tor-post---create-or-append (&key
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
  (let* ((default-directory (f-join jnf/tor-home-directory
                                    "content" "posts"
                                    (format-time-string "%Y/")))

         (slug (jnf/tor-convert-text-to-slug title))
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
                 "\ntitle: '" (jnf/tor-convert-text-to-post-title title) "'"
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
(cl-defun jnf/tor-toggle-hugo-server (&key
                                      (directory jnf/tor-home-directory)
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

(defun jnf/tor-retitle-post (title)
  "Replace the given buffer's title with the new TITLE.

This function will: replace the content's title, update the slug,
and rename the buffer."
  (interactive "sNew Post's Title: ")
  (let* ((metadataTitle (concat "title: '"
                                (jnf/tor-convert-text-to-post-title title) "'"))
         (slug (jnf/tor-convert-text-to-slug title))
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

(defvar jnf/tor-hostname-regexp
  "^https?://takeonrules\.com"
  "A regular expression for checking if it's TakeOnRules.com.")

(defvar jnf/tor-hugo-regexp-for-post-path
  (concat jnf/tor-hostname-regexp
          "/[0-9]\\{4\\}/[0-9]\\{2\\}/[0-9]\\{2\\}/\\([^/]+\\)/?$")
  "A regular expression for matching blog posts.")

(defvar jnf/tor-hugo-regexp-for-pages-path
  (concat jnf/tor-hostname-regexp "/\\([^z-a]*[^/]\\)/?$")
  "A regular expression for matching pages.")

(defun jnf/tor-find-hugo-file-by-url (url)
  "Find the associated TakeOnRules.com file for the given URL."
  (interactive (list
                (jnf/tor-prompt-or-kill-ring-for-url
                 :url-regexp jnf/tor-hostname-regexp)))
  (cond
   ;; Blog post
   ((string-match jnf/tor-hugo-regexp-for-post-path url)
    (let* ((slug (match-string-no-properties 1 url))
              (filename (car
                         (jnf/list-filenames-with-file-text
                          :matching (concat "^slug: " slug "$")
                          :in "content"))))
      (find-file (f-join jnf/tor-home-directory "content" filename))))
   ;; Pages
   ((string-match jnf/tor-hugo-regexp-for-pages-path url)
    (let* ((permalink (match-string-no-properties 1 url))
           (filename (car
                         (jnf/list-filenames-with-file-text
                          :matching (concat "^permalink: ['\\\"]?/?"
                                            permalink "/?['\\\"]?$")
                          :in "content"))))
      (find-file (f-join jnf/tor-home-directory "content" filename))))
   ;; No match found
   (t (message "Unable to find post for \"%s\"" url))))

(cl-defun jnf/tor-view-blog-post (&key
                                  (hostname jnf/tor-default-local-hostname))
  "Open `eww' in a new window to preview the current buffer at the HOSTNAME.

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
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (eww (format "%s" (s-join "/" slugs)))))

(defun jnf/tor-create-post (title)
  "Create and visit a new draft post.  Prompt for a TITLE.

The file for the blog post conforms to the path schema of posts
for TakeOnRules.com."
  (interactive "sBlog Post Title: ")
  (jnf/tor-post---create-or-append :title title))

(defun jnf/tor-tag-post (tag)
  "Apply the TAG to the current TakeOnRules.com post.

No effort is made to check if this is a post."
  (interactive (list (completing-read "Tag: " (jnf/tor-tags-list))))
  (let ((saved-point (point))
        (to-insert (concat "\n- " tag)))
    (replace-regexp "^tags:$" (concat "tags:" to-insert) nil 0 (point-max))
    (goto-char (+ saved-point (length to-insert)))))

(defun jnf/tor-insert-glossary-key (key)
  "Insert the KEY at point."
  (interactive (list (completing-read "Key: " (jnf/tor-glossary-key-list))))
  (insert key))

(defun jnf/tor-find-changelog-and-insert-entry ()
  "Find TakeOnRules glossary and begin entering a changelog entry."
  (interactive)
  (find-file (f-join jnf/tor-home-directory "data" "changelog.yml"))
  ;; The changelog is structured in date descending order.  The first
  ;; line is the YAML preamble indicating a data object (e.g. "---")
  (beginning-of-buffer)
  (end-of-line)
  (insert (concat "\n- date: "
                  (format-time-string "%Y-%m-%d")
                  "\n  entries:\n    - ")))

(defun jnf/tor-find-series-and-insert-entry (title)
  "Find TakeOnRules series and add an entry with TITLE."
  (interactive "sSeries Entry's Title: ")
  (find-file (f-join jnf/tor-home-directory "data" "series.yml"))
  (let ((key (downcase (s-dashed-words title))))
    (end-of-buffer)
    (insert (concat
             (if (looking-at-p "^$") "" "\n")
             "- title: " title
             "\n  key: " key))))

(defun jnf/tor-find-glossary-and-insert-entry (title)
  "Find TakeOnRules glossary and add an entry with TITLE."
  (interactive "sGlossary Entry's Title: ")
  (find-file (f-join jnf/tor-home-directory "data" "glossary.yml"))
  (let ((key (upcase (s-dashed-words title))))
    (end-of-buffer)
    (insert (concat
             (if (looking-at-p "^$") "" "\n")
             "- title: " title
             "\n  key: " key))))

(defun jnf/tor-insert-epigraph-entry ()
  "Prompt for a new a new data/epigraphs.yml entry."
  (interactive)
  (find-file (f-join jnf/tor-home-directory "data" "epigraphs.yml"))
  (end-of-buffer)
  (insert (concat
           (if (looking-at-p "^$") "" "\n")
           "epi"))
  (end-of-buffer)
  "Assumes that the 'epi' is the correct expansion for the snippet."
  (yas-expand)
  (message "Ready to insert a new epigraph"))

(cl-defun jnf/tor-post-amplifying-the-blogosphere (subheading
                                                   &key
                                                   citeTitle
                                                   citeURL
                                                   citeAuthor)
  "Create and visit draft post for amplifying the blogosphere.

If there's an active region, prompt for the :SUBHEADING.  The file
for the blog post conforms to the path schema of posts for
TakeOnRules.com.

We'll pass the :CITETITLE, :CITEAUTHOR, and :CITEURL to
`jnf/tor-post---create-or-append'"
  (interactive (list (if (use-region-p)
                         (read-string "Sub-Heading: ")
                       nil)))
  (jnf/tor-post---create-or-append
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
(cl-defun jnf/find-file-via-matching (&key prompt matching in)
  "PROMPT for files IN the directory with MATCHING content.

If `consult--read' is defined, use that.  Otherwise fallback to `completing-read'."
  (if (fboundp 'consult--read)
      (consult--read
       (consult--with-increased-gc
        (jnf/list-full-filenames-with-file-text :matching matching :in in))
       :prompt prompt
       :sort nil
       :require-match t
       :category 'file
       :history 'file-name-history
       :state (consult--file-preview))
    (list (completing-read
           prompt
           (jnf/list-filenames-with-file-text
            :matching matching
            :in in)))))

(defun jnf/tor-find-file-draft (filename)
  "Find a draft FILENAME in the TakeOnRules content directory."
  (interactive
   (list (jnf/find-file-via-matching
    :prompt "Draft filename: "
    :matching "^draft: true"
    :in (f-join jnf/tor-home-directory "content"))))
  (find-file filename))

(defun jnf/tor-find-file (filename)
  "Find a FILENAME in the TakeOnRules content directory."
  (interactive
   (list (jnf/find-file-via-matching
    :prompt "Filename: "
    :matching "^title:"
    :in (f-join jnf/tor-home-directory "content"))))
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
(defun jnf/tor-tags-list ()
  "Return a list of tags from TakeOnRules.com."
  (jnf/tor-list-by-key-from-filename :key "tag" :filename "data/glossary.yml"))

(defun jnf/tor-epigraph-list ()
  "Return a list of epigraph keys from TakeOnRules.com."
  (jnf/tor-list-by-key-from-filename :key "key" :filename "data/epigraphs.yml"))

(defun jnf/tor-game-list ()
  "Return a list of games from TakeOnRules.com."
  (jnf/tor-list-by-key-from-filename :key "game" :filename "data/glossary.yml"))

(defun jnf/tor-glossary-title-list ()
  "Return a list of titles from TakeOnRules.com."
  (jnf/tor-list-by-key-from-filename :key "title" :filename "data/glossary.yml"))

(defun jnf/tor-glossary-key-list ()
  "Return a list of keys from TakeOnRules.com glossary."
  (jnf/tor-list-by-key-from-filename :key "key" :filename "data/glossary.yml"))

(defun jnf/tor-series-list ()
  "Return a list of series from TakeOnRules.com."
  (jnf/tor-list-by-key-from-filename :key "key" :filename "data/series.yml"))

(defun jnf/tor-licenses-list ()
  "Return a list of available licenses for TakeOnRules.com."
  (jnf/tor-list-by-key-from-filename :key "Key" :filename "data/licenses.yml"))
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
(cl-defun jnf/tor-list-by-key-from-filename (&key
                                             key
                                             filename
                                             (directory jnf/tor-home-directory))
  "Build a list of entries of the KEY from the FILENAME in DIRECTORY."
  (split-string-and-unquote
   (shell-command-to-string
    (concat
     "rg \"^[- ] " key ": .*$\" "
     (f-join directory filename)
     " --only-matching --no-filename | sed 's/^[ -] " key ": //' | sort | tr '\n' '@'"))
   "@"))

(cl-defun jnf/list-filenames-with-file-text (&key matching in)
  "Build a list of filenames MATCHING the pattern IN the given directory."
  (let ((default-directory (f-join jnf/tor-home-directory in)))
    (split-string-and-unquote
     (shell-command-to-string
      (concat
       "rg \""
       matching "\" --only-matching --files-with-matches --sortr modified"
       "| tr '\n' '@'"))
     "@")))

(cl-defun jnf/list-full-filenames-with-file-text (&key matching in)
  "Build a list of filenames MATCHING the pattern IN the given directory."
  (split-string-and-unquote
   (shell-command-to-string
    (concat
     "rg \""
     matching "\" " in " --only-matching --files-with-matches --sortr modified"
     "| tr '\n' '@'"))
   "@"))

(defun jnf/tor-page-relative-pathname-list ()
  "Return a list of pages for TakeOnRules.com."
  (jnf/list-filenames-with-file-text :matching "^title: " :in "content"))

(defun jnf/tor-asset-relative-pathname-list ()
  "Return a list of image filenames for TakeOnRules.com."
  (let ((default-directory (f-join jnf/tor-home-directory "assets" "images")))
    (split-string-and-unquote
     (shell-command-to-string "ls"))))

(defun jnf/matches-in-buffer (regexp &optional buffer)
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

(defun jnf/kill-new-markdown-heading-as-slug (heading)
  "Push onto the `kill-ring' a slugified version of HEADING."
  (interactive
   (list (completing-read
	  "Heading: "
	  (jnf/matches-in-buffer "^#+ +.*$"))))
  (kill-new (jnf/tor-convert-text-to-slug
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
(cl-defun jnf/tor-wrap-with-text (&key before after strategy)
  "Wrap the STRATEGY determined region with the BEFORE and AFTER text.

Valid STRATEGY options are:

* :lineOrRegion
* :pointOrRegion
* :sentenceOrRegion
* :wordOrRegion

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

(defun jnf/tor-wrap-in-html-tag (tag &optional attributes)
  "Wrap the word or region with the given TAG with optional ATTRIBUTES."
  (interactive "sHTML Tag: \nsAttributes (optional): ")
  (jnf/tor-wrap-with-text
   :before (concat "<" tag (if (s-blank? attributes)
                               ""
                             (concat " " attributes)) ">")
   :after (concat "</" tag ">")
   :strategy :wordOrRegion))

(defun jnf/tor-wrap-in-poem ()
  "Wrap the point or region as a poem."
  (interactive)
  (jnf/tor-wrap-with-text
   :before "<pre class=\"poem\">\n"
   :after "\n</pre>"
   :strategy :pointOrRegion))

(defun jnf/tor-wrap-date (date)
  "Wrap the point or region with the given DATE."
  (interactive (list (org-read-date nil nil nil "Date")))
  (jnf/tor-wrap-in-html-tag
   "time"
   (concat "datetime=\"" date "\" title=\"" date "\"")))

(defun jnf/tor-wrap-as-marginnote-dwim ()
  "Wrap the line or current region as a marginnote Hugo shortcode."
  (interactive)
  (jnf/tor-wrap-with-text
   :before "{{< marginnote >}}\n"
   :after "\n{{< /marginnote >}}"
   :strategy :lineOrRegion))

(defun jnf/tor-wrap-as-sidenote-dwim ()
  "Wrap the line or current region as a sidenote Hugo shortcode."
  (interactive)
  (jnf/tor-wrap-with-text
   :before "{{< sidenote >}}"
   :after "{{< /sidenote >}}"
   :strategy :sentenceOrRegion))

(defun jnf/tor-wrap-link-active-region-dwim (url)
  "Wrap current region (or point) in an A-tag with the given URL.

For the URL:

- If `car' of `kill-ring' starts with \"http\", then use that as the URL.
- Otherwise prompt for a URL.

If the URL is an empty string, then send a message.  Else, if we
have a non-0 length URL, use the URL and wrap the region in an A
tag."
  (interactive (list (jnf/tor-prompt-or-kill-ring-for-url)))
  (if (eq (length url) 0)
      (message "No URL to use for A-tag creation")
    (jnf/tor-wrap-with-text
     :before (concat "<a href=\"" url "\">")
     :after "</a>"
     :strategy :pointOrRegion)))

(defun jnf/tor-wrap-as-pseudo-dfn ()
  "Wrap current region (or word) in an I-tag with a DFN dom class."
  (interactive)
  (jnf/tor-wrap-with-text
   :before "<i class=\"dfn\">"
   :after "</i>"
   :strategy :wordOrRegion))

(defun jnf/tor-wrap-cite-active-region-dwim (url)
  "Wrap current region (or point) in a CITE-tag and optional A-tag with URL.

For the URL:

- If `car' of `kill-ring' starts with \"http\", then use that as the URL.
- Otherwise prompt for a URL.

If the URL an empty string, then wrap the current region or point
in a CITE tag. Else, if we have a non-0 length URL, wrap it in
CITE and A tag."
  (interactive (list (jnf/tor-prompt-or-kill-ring-for-url)))

  ;; Were we to start writing at the START position, we'd invariably
  ;; change the contents such that the END position was no longer
  ;; accurate.  So instead, we append at the END position, hop back to
  ;; the START position and append to the START position.
  (if (eq (length url) 0)
      (jnf/tor-wrap-with-text
       :before "<cite>"
       :after "</cite >"
       :strategy :pointOrRegion)
    (jnf/tor-wrap-with-text
     :before (concat "<cite><a href=\"" url
                     "\" class=\"u-url p-name\" rel=\"cite\">")
     :after "</a></cite>"
     :strategy :pointOrRegion)))
;;******************************************************************************
;;
;;; END Wrapping Functions
;;
;;******************************************************************************

(provide 'jnf-blogging.el)
;;; jnf-blogging.el ends here
