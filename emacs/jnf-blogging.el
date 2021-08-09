;;; -*- lexical-binding: t; -*-
;;; jnf-blogging.el --- Summary
;;
;;; Commentary:
;;
;;  This package provides some blogging tooling.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/jrblevin/markdown-mode/
(use-package markdown-mode
  :straight t
  :hook ((markdown-mode . turn-on-visual-line-mode))
  :bind (:map markdown-mode-map ("C-c t" . jnf/tor-subject-menu-markdown/body))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/pandoc"))

;; These menu commands, plus some yasnippets, are some useful
;; functions for helping my blogging effort.
(defvar jnf/tor-menu--title (with-faicon "pencil-square" "Take on Rules" 1 -0.05))
(pretty-hydra-define jnf/tor-subject-menu-markdown (:foreign-keys warn :title jnf/tor-menu--title :quit-key "q" :exit t)
  ("Posts"
   (("a" jnf/tor-wrap-link-active-region-dwim "A link at point or region…")
    ("c" jnf/tor-wrap-cite-active-region-dwim "Cite point or region…")
    ("d" jnf/tor-wrap-date "Date point or region…")
    ("e" jnf/tor-insert-epigraph-entry "Create epigraph entry…")
    ("f" jnf/tor-wrap-as-pseudo-dfn "Wrap word or region in pseudo-dfn…")
    ("g" jnf/tor-find-glossary-and-insert-entry "Create glossary entry…")
    ("k" jnf/tor-insert-glossary-key "Insert glossary key at point…")
    ("m" jnf/tor-wrap-as-marginnote-dwim "Margin-note line or region…")
    ("n" jnf/tor-create-post "Create new post…")
    ("r" jnf/tor-retitle-post "Re-title post…")
    ("s" jnf/tor-wrap-as-sidenote-dwim "Side-note sentence or region…")
    ("t" jnf/tor-tag-post "Tag post…")
    ("w" jnf/tor-wrap-in-html-tag "Wrap point or region…"))))

(pretty-hydra-define jnf/tor-subject-menu-yaml (:foreign-keys warn :title jnf/tor-menu--title :quit-key "q" :exit t)
  ("Posts"
   (("e" jnf/tor-insert-epigraph-entry "Create epigraph entry…")
    ("g" jnf/tor-find-glossary-and-insert-entry "Create glossary entry…")
    ("k" jnf/tor-insert-glossary-key "Insert key at point…")
    ("n" jnf/tor-create-post "Create new post…"))))

(pretty-hydra-define jnf/tor-subject-menu-default (:foreign-keys warn :title jnf/tor-menu--title :quit-key "q" :exit t)
  ("Posts"
   (("e" jnf/tor-insert-epigraph-entry "Create epigraph entry…")
    ("g" jnf/tor-find-glossary-and-insert-entry "Create glossary entry…")
    ("n" jnf/tor-create-post "Create new post…"))))

(defun jnf/epigraph-keyify (text)
  "Convert the given TEXT to an epigraph key."
  (let ((list-of-words (s-split-words text)))
    (if (> (length list-of-words) 5)
        (upcase (s-join "-" (subseq list-of-words 0 5)))
      "")))

(defun jnf/tor-create-post (title)
  "Create and visit a new draft post.  Prompt for a `TITLE'.

The file for the blog post conforms to the path schema of posts
for TakeOnRules.com."
  (interactive "sBlog Post Title: ")
  (jnf/tor-post---create-or-append :title title))

(defun jnf/tor-wrap-in-html-tag (tag &optional attributes)
  "Wrap the point or region with the given TAG with optional ATTRIBUTES."
  (interactive "sHTML Tag: \nsAttributes (optional): ")
  (jnf/tor-wrap-with-text
   :before (concat "<" tag (if (s-blank? attributes) "" (concat " " attributes)) ">")
   :after (concat "</" tag ">")
   :strategy :pointOrRegion))

(defun jnf/tor-wrap-date (date)
  "Wrap the point or region with the given DATE."
  (interactive (list
                (read-string
                 (concat "Date (default \"" (format-time-string "%Y-%m-%d") "\"): ")
                 nil nil (format-time-string "%Y-%m-%d"))))
  (jnf/tor-wrap-in-html-tag
   "time"
   (concat "datetime=\"" date "\" title=\"" date "\"")))

(defun jnf/tor-tag-post (tag)
  "Apply the TAG to the current TakeOnRules.com post.

No effort is made to check if this is a post."
  (interactive (list (completing-read "Tag: " (jnf/tor-tags-list))))
  (let ((saved-point (point))
        (to-insert (concat "\n- " tag)))
    (replace-regexp "^tags:$" (concat "tags:" to-insert) nil 0 (point-max))
    (goto-char (+ saved-point (length to-insert)))))

(cl-defun jnf/tor-wrap-with-text (&key before after strategy)
  "Wrap the STRATEGY determined region with the BEFORE and AFTER text.

Valid STRATEGY options are:

* `:lineOrRegion'
* `:pointOrRegion'
* `:sentenceOrRegion'

TODO: I would love create a lookup table for the case statement,
as the behavior's well defined."
  (pcase strategy
    (:lineOrRegion (pcase-let* ((origin (point))
               (`(,begin . ,end) (crux-get-positions-of-line-or-region)))
                     (goto-char end)
                     (insert after)
                     (goto-char begin)
                     (insert before)))
    (:sentenceOrRegion (let* ((begin (if (use-region-p) (region-beginning) (car (bounds-of-thing-at-point 'sentence))))
                              (end (if (use-region-p) (region-end) (cdr (bounds-of-thing-at-point 'sentence)))))
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
    (:wordOrRegion (let* ((begin (if (use-region-p) (region-beginning) (cdr (bounds-of-thing-at-point 'word))))
                           (end (if (use-region-p) (region-end) (cdr (bounds-of-thing-at-point 'word)))))
                      (goto-char end)
                      (insert after)
                      (goto-char begin)
                      (insert before)))
    ))

(defun jnf/tor-wrap-as-marginnote-dwim ()
  "Wrap the line or current region as a marginnote."
  (interactive)
  (jnf/tor-wrap-with-text
   :before "{{< marginnote >}}\n"
   :after "\n{{< /marginnote >}}"
   :strategy :lineOrRegion))

(defun jnf/tor-wrap-as-sidenote-dwim ()
  "Wrap the line or current region as a sidenote."
  (interactive)
  (jnf/tor-wrap-with-text
   :before "{{< sidenote >}}"
   :after "{{< /sidenote >}}"
   :strategy :sentenceOrRegion))

(defun jnf/tor-insert-glossary-key (key)
  "Insert the KEY at point."
  (interactive (list (completing-read "Key: " (jnf/tor-glossary-key-list))))
  (insert key))

(defun jnf/tor-wrap-link-active-region-dwim (url)
  "Wrap current region (or point) in an `A' tag with URL.

For the URL:

- If `car' in `kill-ring' starts with \"http\", then use that as the URL.
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
  "Wrap current region (or word) in an `span' tag with a `dfn' class."
  (interactive)
  (jnf/tor-wrap-with-text
     :before (concat "<i class=\"dfn\">")
     :after "</i>"
     :strategy :wordOrRegion))

(defun jnf/tor-prompt-or-kill-ring-for-url ()
  "Return a URL, either from the kill ring or prompted."
  (let ((car-of-kill-ring (substring-no-properties (car kill-ring))))
    ;; Need to ensure we're not dealing with something out of bounds
    (if (and (> (length car-of-kill-ring) 3)
             (string= (substring car-of-kill-ring 0 4) "http"))
        (substring-no-properties (car kill-ring))
      (read-string "URL (optional): "))))

(defun jnf/tor-find-glossary-and-insert-entry (title)
  "Find TakeOnRules glossary and add an entry with TITLE."
  (interactive "sGlossary Entry's Title: ")
  (find-file "~/git/takeonrules.github.io/data/glossary.yml")
  (jnf/tor-insert-glossary-entry title))

(defun jnf/tor-insert-glossary-entry (title)
  "Create an glossary entry with the given TITLE."
  (interactive "sGlossary Entry's Title: ")
  (let ((key (upcase (s-dashed-words title))))
    (end-of-buffer)
    (insert (concat
             (if (looking-at-p "^$") "" "\n")
             "- title: " title
             "\n  key: " key))))

(defun jnf/tor-insert-epigraph-entry ()
  "Prompt for a new a new data/epigraphs.yml entry."
  (interactive)
  (find-file "~/git/takeonrules.github.io/data/epigraphs.yml")
  (end-of-buffer)
  (insert (concat
           (if (looking-at-p "^$") "" "\n")
           "epi"))
  (end-of-buffer)
  "Assumes that the `epi' is the correct expansion"
  (yas-expand)
  (message "Ready to insert a new epigraph"))

(defun jnf/tor-wrap-cite-active-region-dwim (url)
  "Wrap current region (or point) in a `CITE' and optional `A' tag with URL.

For the URL:

- If `car' in `kill-ring' starts with \"http\", then use that as the URL.
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

(global-set-key (kbd "s-7") 'jnf/tor-post-amplifying-the-blogosphere)
(global-set-key (kbd "<f7>") 'jnf/tor-post-amplifying-the-blogosphere)

;; The `C-c t' key combo is engrained for my TakeOnRules incantations;
;; there's a markdown menu but if I'm not in markdown, it likely means
;; I'm not in Take on Rules pages.
(global-set-key (kbd "C-c t") 'jnf/tor-subject-menu-default/body)

(cl-defun jnf/tor-post-amplifying-the-blogosphere (subheading &key citeTitle citeURL citeAuthor)
  "Create and visit draft post for amplifying the blogosphere.

If there's an active region, prompt for the `SUBHEADING'.  The file
for the blog post conforms to the path schema of posts for
TakeOnRules.com.

We'll pass the `CITETITLE', `CITEAUTHOR', and `CITEURL' to
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

(defun jnf/tor-post-titleize (title)
  "Convert TITLE to correct format."
  (message "Titleizing...")
  (replace-regexp-in-string
   "\"\\([^\"]+\\)\""
   "“\\1”"
   (s-replace "'" "’" title)))

(defun jnf/tor-slugify (string)
  "Convert STRING to appropriate slug."
  (s-replace "'" "" (s-dashed-words string)))

(cl-defun jnf/tor-post---create-or-append (&key title tags series toc citeTitle citeURL citeAuthor subheading)
  "Create or append a post with `TITLE'.

The following keys are optional:

`TAGS' one or more tags, as a list or string, to add to the
        frontmatter.
`SERIES' the series to set in the frontmatter.
`TOC' whether to include a table of contents in the post.
`CITETITLE' the title of the URL cited (if any)
`CITEURL' the URL cited (if any)
`CITEAUTHOR' the author cited (if any)
`SUBHEADING' if you have an active region, use this header.

If there's an active region, select that text and place it."
  (let* ((default-directory (concat "~/git/takeonrules.github.io/"
                                    "/content/posts/"
                                    (format-time-string "%Y/")))

         (slug (jnf/tor-slugify title))
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
                 "\ntitle: '" (jnf/tor-post-titleize title) "'"
                 "\ntype: post"
                 (if series (concat "\nseries: " series))
                 (if toc (concat "\ntoc: true"))
                 "\ntags:"
                 (if tags (concat (mapconcat
                                   (lambda (tag)
                                     (concat "\n- " tag))
                                   (flatten-tree tags) "")))
                 "\n---\n")
         nil fpath))
    ;; If we have an active region, append that region's content to
    ;; the given file.
    (if (use-region-p)
        (write-region
         (concat
          (if subheading
              (concat "\n## " subheading "\n")
            (if citeTitle (concat "\n## " citeTitle "\n")))
          (if citeURL (concat
                       "\n{{< blockquote"
                       (if citeAuthor
                           (concat " pre=\"" citeAuthor "\""))
                       " cite=\""
                       citeTitle "\" cite_url=\""
                       citeURL "\" >}}\n"))
          (buffer-substring (region-beginning) (region-end))
          (if citeURL "\n{{< /blockquote >}}"))
         nil fpath t)
      ;; Without an active region, if we have a citeURL insert a link
      ;; to it.
      (if citeURL
          (write-region
           (concat
            "\n<cite><a href=\"" citeURL
            "\" class=\"u-url p-name\" rel=\"cite\">"
            (or (citeTitle) (citeURL)) "</a></cite>\n")
           nil fpath t)))
    ;; Finally open that file for editing.
    (find-file fpath)))

(cl-defun jnf/tor-list-by-key-from-filename (&key key filename)
  "Build a list of entries of the `KEY' from the `FILENAME'."
  (split-string-and-unquote
   (shell-command-to-string
    (concat
     "rg \"" key ": .*$\" "
     (f-join "~/git/takeonrules.github.io/" filename)
     " --only-matching --no-filename | cut -d \" \" -f 2- | sort | tr '\n' '~'"))
   "~"))

;; Used in ./emacs/snippets/text-mode/tag
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

(defun jnf/tor-page-relative-pathname-list ()
  "Return a list of pages for TakeOnRules.com."
  (split-string-and-unquote
   (let ((default-directory "~/git/takeonrules.github.io/content"))
     (shell-command-to-string "rg \"^title: \" --files-with-matches | sort"))))

(defun jnf/tor-asset-relative-pathname-list ()
  "Return a list of image filenames for TakeOnRules.com."
  (split-string-and-unquote
   (let ((default-directory "~/git/takeonrules.github.io/assets/images"))
     (shell-command-to-string "ls"))))

(defun org-files-names-in-project-list ()
  "Return a list of filenames in the current files directory."
  (split-string-and-unquote
   (shell-command-to-string
    (concat
     "ls " (file-name-directory buffer-file-name)))))

(defun jnf/roll (sided)
  "Roll an n `SIDED' die."
  (interactive "sDice Sides: ")
  (let ((result (+ 1 (random (cl-parse-integer sided)))))
    (message "d%s => %s" sided result)))


(defun jnf/roll-expression-dwim (expression &optional)
  "Roll the `EXPRESSION', check `thing-at-point' then prompt."
  (interactive (list (if (string-match "[dD][0-9]" (format "%s" (thing-at-point 'sexp t)))
                         (thing-at-point 'sexp t)
                       (read-string "Dice Expression: "))))
  (-let* (((rolls . result) (org-d20--roll expression)))
    (message "%s => %s" expression result)))
(global-set-key (kbd "C-s-r") 'jnf/roll-expression-dwim)

(defun jnf/tor-retitle-post (title)
  "Replace the given buffer's title with the new `TITLE'.

This function will: replace the content's title, update the slug,
and rename the buffer."
    (interactive "sNew Post's Title: ")
    (let* ((metadataTitle (concat "title: '" (jnf/tor-post-titleize title) "'"))
           (slug (jnf/tor-slugify title))
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

(provide 'jnf-blogging.el)
;;; jnf-blogging.el ends here
