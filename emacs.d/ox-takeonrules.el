;;; ox-takeonrules --- A Simplified Hugo Export -*- lexical-binding: t -*-

;; Copyright (C) 2024 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;; A minimal viable "Export HTML to Markdown suitable for Hugo, in
;; particular Take on Rules" package.
;;; Code:


;; TODO: Handle margin notes and other sort of blocks
(org-export-define-derived-backend 'takeonrules 'md
  :translate-alist
  '((footnote-reference . org-takeonrules-footnote-reference)
     (inner-template . org-takeonrules-inner-template)
     (timestamp . org-takeonrules-timestamp))
  :filters-alist
  '((:filter-body org-takeonrules-body-filter))
  :options-alist
  '((:with-toc nil "toc" nil)))

(defvar ox-takeonrules/license
  "by-nc-nd-4_0"
  "The applicable license for this blog post.")
(defvar jf/exporting-org-to-tor nil
  "When non-nil, I'm in the middle of exporting the document to takeonrules.")

(defun export-blog-post-to-takeonrules ()
  "Conditionally export to TakeOnRules the blog post at point.

When point is a descendant of a headline tagged as a blog post (see
`jf/denote/keywords/blogPosts'), export that headline's subtree as a
blog post to Take on Rules.

This leverages `org-export-to-file' along with the 'takeonrules
org-export backend derived from the 'md backend."
  (interactive)
  ;; Bail if we aren't in `org-mode'
  (unless (derived-mode-p 'org-mode)
    (user-error "Current buffer not 'org-mode"))
  (org-set-regexps-and-options)
  (jf/bibliography/export-to-takeonrules)
  (export-glossary-to-takeonrules)
  (let ((blogPost
          (jf/org-mode/get-blog-post)))
    (unless blogPost
      (user-error
        "Current node is not child of headline tagged with :%s:"
        jf/denote/keywords/blogPosts))
    (goto-char (org-element-property :begin blogPost))
    ;; Ensure we set metadata that we will use in the export for initial
    ;; publication as well as future updates (if any)
    (let ((jf/exporting-org-to-tor
            t)
           (slug
             (or (org-element-property :SLUG blogPost)
               (let ((s (jf/denote-sluggify-title
                          (org-element-property :title blogPost))))
                 (org-entry-put blogPost "SLUG" s)
                 s)))
           (custom_id
             (or (org-element-property :CUSTOM_ID blogPost)
               (let ((id (format "blogPost-%s"
                           (jf/denote-sluggify-title
                             (org-element-property :title blogPost)))))
                 (org-entry-put blogPost "CUSTOM_ID" id)
                 id))))
      (or (org-element-property :ID blogPost)
        (org-entry-put blogPost "ID" custom_id))
      (or (org-element-property :PUBLISHED_AT blogPost)
        (org-entry-put blogPost "PUBLISHED_AT"
          (format-time-string "%Y-%m-%d %H:%M:%S %z")))
      (when (org-entry-is-done-p)
        (org-entry-put blogPost "LAST_MODIFIED_AT"
          (format-time-string "%Y-%m-%d %H:%M:%S %z")))
      (or (org-element-property :DESCRIPTION blogPost)
        (org-entry-put blogPost "DESCRIPTION"
          (read-string "Description: ")))
      ;; We opt only for the filename, relying on the exporter to place
      ;; the file in the correct location.
      (or (org-element-property :EXPORT_FILE_NAME blogPost)
        (org-entry-put blogPost "EXPORT_FILE_NAME"
          (format "%s--%s"
            (format-time-string "%Y%m%dT%H%M%S")
            slug)))

      ;; With all of that done, we save the buffer to ensure the
      ;; properties are set.
      (save-buffer)

      ;; Now we export the file to the correct location and open it upon
      ;; completion.  It's always a good idea to review things.
      (let* ((file
               (org-export-output-file-name ".md" t
                 (f-join jf/tor-home-directory "content" "posts"
                   (format-time-string "%Y")))))
        (and (org-export-to-file 'takeonrules file nil t t t)
          (find-file-other-window file))))))

(defun org-takeonrules-inner-template (contents info)
  "Transcode CONTENTS to markdown body.

INFO is a plist holding contextual information."
  contents)

(defun export-glossary-to-takeonrules (&optional glossary blog data)
  "Export GLOSSARY to DATA file and BLOG file."
  (interactive)
  (let ((entries
          ;; With the given tag, find all associated headlines
          ;; that match that tag.
          (org-map-entries
            (lambda ()
              (let* ((epom
                       (org-element-at-point))
                      (entry
                        (list
                          (cons "key" (upcase (org-entry-get epom "CUSTOM_ID"))))))
                (when-let* ((value (org-entry-get epom "DESCRIPTION")))
                  (add-to-list 'entry (cons "description" value)))
                (when-let* ((value (org-entry-get epom "ABBR")))
                  (add-to-list 'entry (cons "abbr" value)))
                (when-let* ((value (org-entry-get epom "PLURAL_ABBR")))
                  (add-to-list 'entry (cons "plural_abbr" value)))
                (when-let* ((value (org-entry-get epom "PLURAL_TITLE")))
                  (add-to-list 'entry (cons "plural_title" value)))
                (when-let* ((value (org-entry-get epom "SAME_AS")))
                  (add-to-list 'entry (cons "same_as" value)))
                (when-let* ((value (org-entry-get epom "OFFER")))
                  (add-to-list 'entry (cons "offer" value)))
                (when-let* ((value (org-entry-get epom "ITEMID")))
                  (add-to-list 'entry (cons "itemid" value)))
                ;; (when-let* ((value (org-entry-get epom "GAME")))
                ;;   (add-to-list 'entry (cons "game" value)))
                (when-let* ((value (org-entry-get epom "ROAM_REFS")))
                  (add-to-list 'entry (cons "url" value)))
                (when-let* ((value (org-element-property :title epom)))
                  (add-to-list 'entry (cons "title" value)))
                (when-let* ((value (org-entry-get epom "TAG")))
                  (add-to-list 'entry (cons "tag" value)))
                entry))
            "+LEVEL=2+glossary-noexport"
            (list (or glossary jf/filename/glossary)))))
    (with-current-buffer (find-file-noselect
                           (or data jf/filename/glossary-data-takeonrules))
      (widen)
      (delete-region (point-min) (point-max))
      (insert (yaml-encode entries))
      (save-buffer))
    (with-current-buffer (find-file-noselect
                           (or blog jf/filename/glossary-takeonrules))
      (widen)
      (delete-region (point-min) (point-max))
      (insert
        "---\n"
        "aliases:\n"
        "- \"/metadata/glossary/\"\n"
        "- \"/more/glossary/\"\n"
        "date: 2020-04-24 17:57:35.000000000 -04:00\n"
        "full_width: true\n"
        "images: []\n"
        "lastmod: " (format-time-string "%Y-%m-%d %H:%M:%S.%N %z") "\n"
        "layout: page\n"
        "permalink: \"/site-map/glossary/\"\n"
        "title: Glossary of Terms for Take on Rules\n"
        "type: page\n"
        "---\n\n"
        "Below are the details the glossary entries used throughout <cite>Take on Rules</cite>. In some cases, I've linked to external pages that further define the concept.\n"
        "\n"
        "For some terms there are corresponding tags.  Each term may have one and only one tag.  A tag is a keyword or indicator of what the post is about.  When a post is tagged with a term, it means that it's the subject of the page; and not just a mention.\n")
      (dolist (entry entries)
        (let ((key (alist-get "key" entry nil nil #'string=))
               (title (alist-get "title" entry nil nil #'string=))
               (description (alist-get "description" entry nil nil #'string=))
               (verbose_title (alist-get "verbose_title" entry nil nil #'string=))
               (abbr (alist-get "abbr" entry nil nil #'string=))
               (url (alist-get "url" entry nil nil #'string=))
               (tag (alist-get "tag" entry nil nil #'string=)))
          (insert
            "\n<details id=\"abbr-dfn-" key "\" aria-label=\"Term: &ldquo;" title "&rdquo;\">\n"
            "<summary>\n"
            "<dfn>" title "</dfn>"
            (if abbr
              (concat "(<abbr title=\"" title "\">" abbr "</abbr>)") "")
            ":\n"
            "<dl aria-label=\"Definition details for &ldquo;" title "&rdquoa;\" class=\"grid\">\n"
            (if url
              (concat "<dt>URL</dt>\n<dd>" url "</dd>\n")
              "")
            (if verbose_title
              (concat "<dt>Verbose Name</dt>\n<dd>" verbose_title "</dd>\n")
              "")
            (if description
              (concat "<dt>Description</dt>\n<dd>" description "</dd>\n")
              "")
            (if tag
              (concat "<dt>Tag</dt>\n<dd>" tag "</dd>\n")
              "")
            "</details>\n")))
      (save-buffer))
    (message "Done exporting glossary to blog")))

(defun jf/bibliography/export-epigraphs (&optional file)
  "Export epigraphs to my blog."
  (interactive)
  (let* ((epigraphs
           (save-restriction
             (widen)
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
                                 ;; Loop
                                 (citable-work
                                   (car
                                     (seq-filter
                                       (lambda (el)
                                         (and
                                           (eq (org-element-type el) 'headline)
                                           (or (member "citables"
                                                 (org-element-property :tags el))
                                             (= (org-element-property :level el) 2))))
                                       lineage)))
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
                                         :title citable-work)))
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
                                 :contents-end el)))))))))))))
    (let* ((buffer
             (find-file-noselect
               (or file jf/filename/epigraph-takeonrules))))
      (with-current-buffer buffer
        (delete-region (point-min) (point-max))
        (insert
          "---\n"
          "date: 2021-07-22 19:23:43.883686000 -04:00 \n"
          "full_width: true\n"
          "images: []\n"
          "lastmod: " (format-time-string "%Y-%m-%d %H:%M:%S.%N %z") "\n"
          "layout: page\n"
          "permalink: \"/site-map/epigraphs/\"\n"
          "title: Epigraphs\n"
          "type: page\n"
          "---\n"
          "\n"
          "Ever since reading {{< glossary key=\"GLOSSARY-DUNE-NOVEL\" >}} by {{< glossary key=\"GLOSSARY-FRANK-HERBERT\" >}} I've loved epigraphs.  "
          "In that novel, the epigraphs are quotes from fictional works written within the Dune universe.  "
          "Below are quotes that I've gathered, and in some cases, I've used as epigraphs throughout <cite>Take on Rules</cite>.\n")
        (dolist (epigraph epigraphs)
          (let ((work
                  (plist-get epigraph :work))
                 (author
                   (plist-get epigraph :author))
                 (text
                   (plist-get epigraph :text)))
            (insert
              (format "<section class=\"epigraphs\"><blockquote data-id=\"%s\">%s%s\n</blockquote></section>\n"
                (plist-get epigraph :id)
                (if (eq (plist-get epigraph :type) 'verse-block)
                  (concat "<pre class=\"verse\">"  text "</pre>")
                  (org-export-string-as (s-trim text) 'html t))
                (cond
                  ((and (s-present? work) (s-present? author))
                    (format "\n<footer>&#8213;%s, <cite>%s</cite></footer>"
                      author work))
                  ((s-present? work)
                    (format "\n<footer>&#8213; <cite>%s</cite></footer>"
                      work))
                  ((s-present? author)
                    (format "\n<footer>&#8213; %s</footer>"
                      author))
                  (t ""))))))
        (save-buffer))
      (message "Done exporting epigraphs to blog"))))

(org-link-set-parameters "epigraph"
  :complete #'jf/org-link-ol-complete/epigraph
  :export #'jf/org-link-ol-export/epigraph
  :face #'jf/org-faces-epigraph
  :follow #'jf/org-link-ol-follow/epigraph)

(defun jf/org-link-ol-export/epigraph (link description format channel)
  "Export the text of the LINK epigraph in the corresponding FORMAT.

We ignore the DESCRIPTION and probably the CHANNEL."
  (let ((buffer
          (find-file-noselect jf/filename/bibliography)))
    (save-restriction
      (widen)
      (save-excursion
        (with-current-buffer buffer
          (let* ((epigraph
                   (car
                     (org-element-map
                       (org-element-parse-buffer)
                       '(quote-block verse-block)
                       (lambda (el)
                         ;; Skip un-named blocks as we can’t link to
                         ;; them.
                         (when (string=
                                 (org-element-property :name el)
                                 link)
                           el)))))
                  (_
                    (unless epigraph
                      (user-error "Unable to find %s epigraph in file %s" link jf/filename/bibliography)))
                  (id
                    (org-element-property :name epigraph))
                  (class
                    (if (eq 'verse-block (org-element-type epigraph))
                      "verse"
                      "quote"))
                  (lineage
                    (org-element-lineage epigraph))
                  (context
                    (car
                      (seq-filter
                        (lambda (el)
                          (and
                            (eq (org-element-type el) 'headline)
                            (= (org-element-property :level el) 2)))
                        lineage)))
                  (people?
                    (member "people"
                      (org-element-property :tags context)))
                  (work
                    (if people?
                      ""
                      (car (org-element-property :title context))))
                  (author
                    (if people?
                      (car (org-element-property :title context))
                      (org-entry-get context "AUTHOR")))
                  (text
                    (buffer-substring-no-properties
                      (org-element-property
                        :contents-begin epigraph)
                      (org-element-property
                        :contents-end epigraph))))
            (cond
              ((or (eq format 'html) (eq format 'md))
                (format "<blockquote class=\"%s epigraph\" data-id=\"%s\">\n%s%s</blockquote>\n"
                  class
                  id
                  (if (string= class "verse")
                    (s-replace "\n" "<br />\n" text)
                    (org-export-string-as text 'html t))
                  (cond
                    ((and (s-present? work) (s-present? author))
                      (format "\n<footer>&#8213;%s, <cite>%s</cite></footer>"
                        author work))
                    ((s-present? work)
                      (format "\n<footer>&#8213; <cite>%s</cite></footer>"
                        work))
                    ((s-present? author)
                      (format "\n<footer>&#8213; %s</footer>"
                        author))
                    (t ""))))
              ((eq format 'latex)
                (format "\\begin{%s}\n%s%s\n\\end{%s}\n"
                  class
                  (if (string= "verse" class)
                    (s-replace "\n" "\\\\\n" text)
                    text)
                  (cond
                    ((and (s-present? work) (s-present? author))
                      (format "---%s, \\textit{%s}" author work))
                    ((s-present? work)
                      (format "---\\textit{%s}" work))
                    ((s-present? author)
                      (format "---%s" author))
                    (t ""))
                  class))
              (t
                (let* ((use-hard-newlines t))
                  (s-replace
                    "\n" hard-newline
                    (format "%s%s"
                      text
                      (cond
                        ((and (s-present? work) (s-present? author))
                          (format "\n\n—%s, “%s”" author work))
                        ((s-present? work)
                          (format "\n\n—“%s”" work))
                        ((s-present? author)
                          (format "\n\n—%s" author))
                        (t "")))))))))))))

(defun jf/org-link-ol-complete/epigraph ()
  "Find and insert an epigraph for export.

Wires into `org-insert-link'."
  (let* ((buffer
           (find-file-noselect jf/filename/bibliography))
          (candidates
            (save-restriction
              (widen)
              (save-excursion
                (with-current-buffer buffer
                  (org-element-map
                    (org-element-parse-buffer)
                    '(quote-block verse-block)
                    (lambda (el)
                      ;; Skip un-named blocks as we can’t link to them.
                      (when-let* ((id
                                    (org-element-property :name el))
                                   (left
                                     (org-element-property
                                       :contents-begin el))
                                   (right
                                     (org-element-property
                                       :contents-end el))
                                   (text
                                     ;; Compress the result into a
                                     ;; single line.
                                     (s-replace "\n" "⮒"
                                       (s-trim
                                         (buffer-substring-no-properties
                                           left
                                           right)))))
                        (cons text id))))))))
          (candidate
            (completing-read "Epigraph: " candidates nil t))
          (id
            (alist-get candidate candidates nil nil #'string=)))
    (when id
      (progn
        (message "Added %S to the kill ring" candidate)
        ;; Expand the result into a single line.
        (kill-new (s-replace "⮒" "\n" candidate))
        (format "epigraph:%s" id)))))

(defun jf/org-link-ol-follow/epigraph (name)
  "Follow the NAME to the epigraph."
  (let* ((file
           jf/filename/bibliography)
          (case-fold-search
            t))
    (find-file file)
    (goto-char (point-min))
    (let ((case-fold-search t))
      (search-forward-regexp (format "^#\\+name: +%s$" name)))
    (pulsar--pulse)))

(org-link-set-parameters "work"
  ;; TODO: Allow link to specify to include author.
  :follow #'jf/org-link-ol-follow/work
  :complete #'jf/org-link-ol-complete/work
  :export #'jf/org-link-ol-export/work
  :face #'jf/org-faces-work)

(defun jf/org-link-ol-follow/work (name)
  "Follow the NAME to the work."
  (let* ((file
           jf/filename/bibliography)
          (case-fold-search
            t))
    (find-file file)
    (widen)
    (goto-char (point-min))
    (let ((case-fold-search
            t)
           (name
             (car (s-split "::" name))))
      (search-forward-regexp
        (format "^:custom_id:[[:space:]]+%s$" name))
      (call-interactively #'org-previous-visible-heading))
    (pulsar--pulse)))

(defun jf/book-make-label (title subtitle author)
  "From the given TITLE, SUBTITLE and AUTHOR return it's formatted label."
  (format "«%s»%s"
    (if (s-present? subtitle)
      (concat title ": " subtitle)
      title)
    (if (s-present? author)
      (concat " by " author) "")))

(defun jf/org-link-ol-complete/work ()
  "Prompt for a work from my bibliography"
  (interactive)
  (let* ((buffer
           (find-file-noselect jf/filename/bibliography))
          (works
            (save-restriction
              (widen)
              (save-excursion
                (with-current-buffer buffer
                  ;; With the given tag, find all associated headlines
                  ;; that match that tag.
                  (org-map-entries
                    (lambda ()
                      (let* ((headline
                               (org-element-at-point))
                              (title
                                (org-element-property :title headline))
                              (subtitle
                                (org-entry-get headline "SUBTITLE"))
                              (author
                                (org-entry-get headline "AUTHOR")))
                        (cons
                          (jf/book-make-label title subtitle author)
                          (list
                            :id (org-entry-get headline "CUSTOM_ID")
                            :title title
                            :subtitle subtitle
                            :author author))))
                    "+LEVEL=2+!people" 'file)))))
          (work
            (completing-read "Citable: " works nil t)))
    (when-let* ((work-data
                 (alist-get work works nil nil #'string=)))
      (let* ((include-author
               (and (plist-get work-data :author)
                 (yes-or-no-p "Include Author: ")))
              (include-subtitle
                (and (plist-get work-data :subtitle)
                  (yes-or-no-p "Include Subtitle: ")))
              (desc
                (jf/book-make-label
                  (plist-get work-data :title)
                  (when include-subtitle (plist-get work-data :subtitle))
                  (when include-author (plist-get work-data :author)))))
        (message "Added %S to the kill ring" desc)
        (kill-new desc)
        (format "work:%s%s%s"
          (plist-get work-data :id)
          (if include-author "::author" "")
          (if include-subtitle "::subtitle" ""))))))

(defvar jf/works/cache
  (make-hash-table :test 'equal)
  "A cache of all works ready for exporting.

See `jf/works/populate'.")

(defun jf/works/populate (&optional clear-cache)
  "Populates and returns the `jf/works/cache'.

When CLEAR-CACHE is non-nil, clobber the cache and rebuild."
  (when clear-cache (clrhash jf/works/cache))
  (when (hash-table-empty-p jf/works/cache)
    (message "Rebuilding `jf/works/cache'...")
    (org-map-entries
      (lambda ()
        (let ((el (org-element-at-point)))
          ;; Skip un-named blocks as we can’t link to
          ;; them.
          (when-let* ((id
                        (org-entry-get el "CUSTOM_ID")))
            (message "🕮 %s" id)
            (puthash id
              (list
                :title
                (org-element-property :title el)
                :subtitle
                (org-entry-get el "SUBTITLE")
                :author
                (org-entry-get el "AUTHOR")
                :url
                (org-entry-get el "ROAM_REFS"))
              jf/works/cache))))
      (concat "+level=2+works")
      `(,jf/filename/bibliography)))
  jf/works/cache)

(defun jf/org-link-ol-export/work (link description format channel)
  "Export the text of the LINK work in the corresponding FORMAT.

We ignore the DESCRIPTION and probably the CHANNEL."
  (let* ((link-with-properties
           (s-split "::" link))
          (link
            (car link-with-properties))
          (with-author
            (member "author" link-with-properties))
          (with-subtitle
            (member "subtitle" link-with-properties))
          (works
            (jf/works/populate)))
    (when-let* ((work
                 (gethash link works)))
      (let ((author-suffix
              (if (and
                    with-author
                    (s-present? (plist-get work :author)))
                (format " by %s" (plist-get work :author))
                "")))
        ;; Then we create the corresponding format.
        (cond
          ((or (eq format 'html) (eq format 'md))
            (format "<cite data-id=\"%s\">%s</cite>%s"
              link
              (if-let* ((url
                         (plist-get work :url)))
                (format "<a href=\"%s\">%s</a>"
                  url (plist-get work :title))
                (plist-get work :title))
              author-suffix))
          ((eq format 'latex)
            (format "\\textit{%s}%s"
              (if-let* ((url
                         (plist-get work :url)))
                (format "\\href{%s}{%s}"
                  url (plist-get work :title))
                (plist-get work :title))
              author-suffix))
          ((eq format 'odt)
            (format
              "<text:span text:style-name=\"%s\">%s</text:span>%s"
              "Emphasis"
              (if-let* ((url
                         (plist-get work :url)))
                (format "<text:a xlink:type=\"simple\" xlink:href=\"%s\">%s</text:a>"
                  url (plist-get work :title))
                (plist-get work :title))
              author-suffix))
          (t
            (format "“%s”%s"
              (plist-get work :title)
              author-suffix)))))))

(org-link-set-parameters "elfeed"
  :follow #'elfeed-link-open
  :store #'elfeed-link-store-link
  :export #'elfeed-link-export-link)

(defun elfeed-link-export-link (link desc format _channel)
  "Export `org-mode' `elfeed' LINK with DESC for FORMAT."
  (if (string-match "\\([^#]+\\)#\\(.+\\)" link)
    (if-let* ((entry
                (elfeed-db-get-entry
                  (cons (match-string 1 link)
                    (match-string 2 link))))
               (url
                 (xml-escape-string (elfeed-entry-link entry)))
               (title
                 (elfeed-entry-title entry)))
      (pcase format
        ('html (format "<a href=\"%s\">%s</a>" url desc))
        ('md (format "[%s](%s)" desc url))
        ('latex (format "\\href{%s}{%s}" url desc))
        ('odt
          (format "<text:a xlink:type=\"simple\" xlink:href=\"%s\">%s</text:a>"
            url desc))
        ('texinfo (format "@uref{%s,%s}" url desc))
        (_ (format "%s (%s)" desc url)))
      (format "%s (%s)" desc url))
    (format "%s (%s)" desc link)))

(defface jf/org-faces-date '((default :inherit link))
  "Face used to style `org-mode' date links in the buffer."
  :group 'denote-faces
  :package-version '(denote . "0.5.0"))

(defface jf/org-faces-epigraph '((default :inherit link))
  "Face used to style `org-mode' epigraph links in the buffer."
  :group 'denote-faces
  :package-version '(denote . "0.5.0"))

(defface jf/org-faces-work '((default :inherit link))
  "Face used to style `org-mode' work links in the buffer."
  :group 'denote-faces
  :package-version '(denote . "0.5.0"))

(defface jf/org-faces-abbr '((default :inherit link))
  "Face used to style `org-mode' abbr links in the buffer."
  :group 'denote-faces
  :package-version '(denote . "0.5.0"))

(defun jf/denote/plist-for-export-of-id (identifier)
  "Given an IDENTIFIER export a `plist' with the following properties:

    - :title
    - :key
    - :url

    Return nil when:

    - is not a denote file
    - IDENTIFIER leads to a non `org-mode' file"
  (when-let* ((filename (denote-get-path-by-id identifier)))
    (when (string= (file-name-extension filename) "org")
      (with-current-buffer (find-file-noselect filename)
        (let ((kw-plist
                (jf/org-keywords-as-plist
                  :keywords-regexp
                  (concat "\\(TITLE\\|CUSTOM_ID\\|OFFER"
                    "\\|ROAM_REFS\\|SAME_AS\\)"))))
          (list
            :title (lax-plist-get kw-plist "TITLE")
            :key (lax-plist-get kw-plist "CUSTOM_ID")
            :url (or
                   (lax-plist-get kw-plist "OFFER")
                   (when-let* ((refs
                                (lax-plist-get kw-plist "ROAM_REFS")))
                     (if (listp refs)
                       (first (s-split " " refs t))
                       refs))
                   (lax-plist-get kw-plist "SAME_AS"))))))))

(defun jf/denote/link-ol-export-concept (path identifier &optional slug)
  "Converge PATH, IDENTIFIER, and SLUG to a concept (as plist).

Concept types are:

- :glossary :: a personal glossary entry
- :linkToSeries :: a series in my blog
- :url :: a URL to some external source, maybe my blog
- :internal :: a reference to an internal document; this may not be
  possible.

The plist will have the following keys:

- :type
- :target that will be a plist with subkeys :path, :identifier, :slug

Each type will have the following keys:

- :glossary :: :key
- :linkToSeries :: :series
- :url :: :title, :url, :cite
- :internal :: :cite"
  (if (string= (file-name-extension path) "org")
    (cond
      ((eq nil slug)
        '(:title "doc-title"))
      ((or (s-starts-with? "#" slug) (s-starts-with? "*" slug))
        (let* ((normalized_slug
                 (substring slug 1)))
          (if-let* ((headline
                     (with-current-buffer (find-file-noselect path)
                       (seq-first
                         (org-map-entries
                           #'org-element-at-point
                           (format
                             "CUSTOM_ID=\"%s\"|TITLE=\"%s\""
                             normalized_slug
                             normalized_slug)
                           'file)))))
            (let ((data
                    (list :title (org-element-property :title headline))))
              (if-let* ((urls
                          (or
                            (org-entry-get headline "OFFER")
                            (org-entry-get headline "ROAM_REFS")
                            (org-entry-get headline "SAME_AS")))
                         (url
                           (xml-escape-string
                             (car (s-split " " urls)))))
                (progn
                  (plist-put data :cite t)
                  (plist-put data :type 'url)
                  (plist-put data :url url))
                (progn
                  (plist-put data :type 'internal)
                  (plist-put data :path 'normalized-slug)))
              (when (member "glossary"
                      (org-get-tags headline))
                (when-let* ((key
                             (org-entry-get headline "CUSTOM_ID")))
                  (plist-put data :type 'glossary)
                  (plist-put data :key key)))
              (when-let* ((key
                           (org-entry-get headline "SERIES_KEY")))
                (plist-put data :type 'series)
                (plist-put data :key key))
              data)
            (user-error "unable to find headline for identifier %s slug %s" identifier slug))))
      ;; '(:title "headline-title" :link "headline-link" :key "headline-key" :url "headline-url"))
      (t
        (user-error "unable to handle, at this time, context denote links.  Try 'id instead.")))))

(defun jf/denote/link-ol-export (link description format)
  "Export a `denote:' link from Org files.

The LINK, DESCRIPTION, FORMAT, and CHANNEL are handled by the
export backend.

When USE_HUGO_SHORTCODE is given use glossary based exporting."
  (let* ((export-plist
           (apply #'jf/denote/link-ol-export-concept (denote-link--ol-resolve-link-to-target link t)))
          (url
            (plist-get export-plist :url))
          (desc
            (or description (plist-get export-plist :title))))
    (if url
      (cond
        ;; TODO Consider links within document; especially when not
        ;; exporting.  Use the TakeOnRules shortcode that leverages
        ;; Hugo built-in
        ((and jf/exporting-org-to-tor (eq (plist-get export-plist :type) 'series))
          (format "{{< linkToSeries \"%s\" >}}" (plist-get export-plist :key)))
        ((and jf/exporting-org-to-tor (eq (plist-get export-plist :type) 'glossary))
          (format "{{< glossary key=\"%s\" >}}" (plist-get export-plist :key)))
        ((eq format 'html)
          (format "%s<a href=\"%s\">%s</a>%s"
            (if (plist-get export-plist :cite) "<cite>" "")
            url desc
            (if (plist-get export-plist :cite) "</cite>" "")))
        ((eq format 'md) (format "[%s](%s)" desc url))
        ((or (eq format 'latex) (eq format 'beamer))
          (format "\\href{%s}{%s}"
            (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" url)
            desc))
        ((eq format 'texinfo) (format "@uref{%s,%s}" url desc))
        ((eq format 'odt)
          (format "<text:a xlink:type=\"simple\" xlink:href=\"%s\">%s</text:a>"
            url
            desc))
        (t (format "[%s](%s)" desc url))
        ;; ((eq format 'odt) (org-odt-link url (format "%s" desc) (list)))
        ;; (t path)
        )
      desc)))

(advice-add #'denote-link-ol-export
  :override #'jf/denote/link-ol-export
  '((name . "wrapper")))

(defun jf/associate-blog-post-url-with-identifier (url identifier)
  "Associate given URL with correct note.

This might be either the `denote' IDENTIFIER or the ID of an `org-mode'
node in one of my agenda files."
  (message "Associating URL: %s with IDENTIFIER: %s." identifier url)
  (if-let* ((filename
             (denote-get-path-by-id identifier)))
    (let ((buffer
            (find-file-noselect filename)))
      (with-current-buffer buffer
        (jf/export-org-to-tor--global-buffer-prop-ensure
          :key "ROAM_REFS"
          :plist (jf/org-keywords-as-plist :keywords-regexp "ROAM_REFS")
          :default url)
        (save-buffer)))
    (if-let* ((filename-and-pos
               (org-id-find identifier)))
      (let ((buffer
              (find-file-noselect (car filename-and-pos))))
        (with-current-buffer buffer
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (cdr filename-and-pos))
              (org-todo "PUBLISHED")
              (org-clock-in)
              (org-clock-out)
              (org-entry-put (org-element-at-point) "ROAM_REFS" url)))))
      (user-error "Unable to find org_id %s for url %s" identifier url))))

(defun jf/capture/denote/from/eww-data ()
  "Create an `denote' entry from `eww' data."
  (interactive)
  (jf/denote/capture-reference :url (plist-get eww-data :url)))

(defun jf/capture/denote/from/elfeed-show-entry ()
  "Create `denote' entry from `elfeed-show-entry'."
  (interactive)
  (jf/denote/capture-reference
    :url (elfeed-entry-link elfeed-show-entry)))


;; I'd love to avoid re-fetching the content.
(cl-defun jf/sanitized-dom (&key html)
  "Convert HTML to sanitized dom."
  (with-temp-buffer
    (insert html)
    (org-web-tools--sanitized-dom)
    (buffer-string)))

(cl-defun jf/denote/capture-reference (&key url
                                        (keywords (denote-keywords-prompt))
                                        (domain "references"))
  "Create a `denote' entry in DOMAIN for URL with KEYWORDS.

The DOM could be as sanitized by `org-web-tools--sanitized-dom'."
  (let* ((url
           (or url (org-web-tools--get-first-url)))
          (dom
            (plz 'get url :as #'org-web-tools--sanitized-dom))
          (title-readable
            (org-web-tools--eww-readable dom))
          (title
            (org-web-tools--cleanup-title
              (or (car title-readable) "")))
          (article
            (org-web-tools--html-to-org-with-pandoc
              (cdr title-readable))))
    (denote title
      keywords
      'org
      (f-join (denote-directory) domain)
      nil
      (concat "#+ROAM_REFS: " url "\n\n" article))))

(cl-defun jf/org-mode/add-series-to-file (&key
                                           file series drop-tags all)
  "Add SERIES to FILE.

Optionally DROP-TAGS, as there may have been a TAG associated
with the series."
  (interactive)
  (with-current-buffer (if file
                         (find-file-noselect file)
                         (current-buffer))
    (when (or current-prefix-arg all (jf/blog-entry?))
      (let ((series
              (or series
                (completing-read "Series: "
                  (jf/tor-series-list) nil t))))
        (unless (and (jf/blog-entry?)
                  (s-contains? "#+HUGO_CUSTOM_FRONT_MATTER: :series "
                    (buffer-substring-no-properties
                      (point-min) (point-max))))
          (save-excursion
            (goto-char (point-min))
            (re-search-forward "^$")
            (insert "\n#+HUGO_CUSTOM_FRONT_MATTER: :series " series)
            (save-buffer)))
        (let* ((file
                 (buffer-file-name))
                (id
                  (denote-retrieve-filename-identifier file))
                (file-type
                  'org)
                (title
                  (denote-retrieve-title-value file file-type))
                (keywords
                  (seq-difference
                    (denote-retrieve-keywords-value file file-type)
                    (flatten-list drop-tags)))
                (extension
                  (denote-get-file-extension file))
                (dir
                  (file-name-directory file))
                (new-name
                  (denote-format-file-name
                    dir id keywords title extension series)))
          (denote-rename-file-and-buffer file new-name)
          (denote-update-dired-buffers))))))

(defun org-takeonrules-footnote-reference (footnote-reference _contents info)
  "Transcode FOOTNOTE-REFERENCE element to Hugo sidenote shortcode.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((element
           (car (org-export-get-footnote-definition
                  footnote-reference info)))
          (beg
            (org-element-property :contents-begin element))
          (end
            (org-element-property :contents-end element))
          (content
            ;; Because of the narrowing/widening happening during
            ;; export, we need to ensure that we can get the contents of
            ;; the footnote; hence we widen the restriction.
            (save-excursion
              (save-restriction
                (widen)
                (s-trim
                  ;; Send the text through the org markdown exporter,
                  ;; ensuring that we translate any org markup into
                  ;; markdown.
                  (org-export-string-as
                    (buffer-substring-no-properties beg end)
                    'md t '(:with-toc nil)))))))
    (format "{{< sidenote >}}%s{{< /sidenote >}}" content)))

(defun org-takeonrules-body-filter (body _backend info)
  "Add front-matter to the BODY of the document.

BODY is the result of the export.
INFO is a plist holding export options."
  (let ((front-matter
          (save-excursion
            (save-restriction
              ;; The point is at the beginning of the heading body
              ;; in this function! So move the point back by 1 char
              ;; to bring it into the Org heading before calling
              ;; `org-hugo--get-front-matter', because in there we
              ;; use `org-entry-get' at (point) to retrieve certain
              ;; property values.
              (widen)
              (ignore-errors ;If the point is at beginning of buffer even after widening
                (backward-char))
              (org-takeonrules--front-matter info)))))
    (format "---\n%s\n---\n%s" front-matter body)))

(defun org-takeonrules--front-matter (info)
  "Generate the corresponding YAML Front-Matter from INFO.

We also rely on the org-element at point."
  ;; Instead of relying on the EXPORT_* per Org-Mode documentation, I'm
  ;; directly fetching the property.
  (let* ((description
           (org-entry-get (point) "DESCRIPTION"))
          (published_at
            (org-entry-get (point) "PUBLISHED_AT"))
          (tags
            (mapcar
              ;; Convert the camel case to kebab (all lower case)
              #'string-inflection-kebab-case-function
              ;; We intersect my allowed tags and those assigned to the
              ;; headline.
              (seq-intersection
                (org-get-tags (point) t)
                denote-known-keywords #'string=)))
          (title
            (car (plist-get info :title)))
          (slug
            (org-entry-get (point) "SLUG"))
          (licenses
            (-list
              (or
                (org-entry-get (point) "LICENSE")
                ox-takeonrules/license)))
          (org_id
            (org-entry-get (point) "ID"))
          (metadata `(("title" . ,title)
                       ("description" . ,description)
                       ("org_id" . ,org_id)
                       ("date" . ,published_at)
                       ("tags" . ,tags)
                       ("slug" . ,slug)
                       ;; These could become a variable, but for now, we'll
                       ;; leave things hard-coded.
                       ("author" . ("Jeremy Friesen"))
                       ("layout" . "post")
                       ("type" . "post")
                       ("licenses" . ,licenses))))
    (if-let* ((lastmod
               (org-entry-get (point) "LAST_MODIFIED_AT")))
      (add-to-list 'metadata `("lastmod" . ,lastmod))
      ;; Assume that the first time we publish, this is in a draft
      ;; state.  That is what picks up what I am to publish.
      (add-to-list 'metadata '("draft" . "true")))
    (yaml-encode metadata)))

(defun org-takeonrules-timestamp (timestamp _contents info)
  "Transcode a TIMESTAMP object from Org to HTML time element.
CONTENTS is ignored.  INFO is a plist holding contextual
information."
  (let* ((as-plain-text
           (org-timestamp-translate timestamp))
          (time
            (org-timestamp-to-time timestamp))
          (datetime-attr
            ;; Sniff out if we have a date or time
            (if (string-match-p ":[[:digit:]][[:digit:]]\>$" as-plain-text)
              ;; It's me, time, with my hour hand and minute hand.
              (format-time-string "%F %R" time)
              ;; No specific time for the day, so we'll use
              (format-time-string "%F" time))))
    ;; This will include the html escaped '&lt;' and '&gt;'.  I'm
    ;; thinking about getting rid of that.
    (format "<time datetime=\"%s\">%s</time>"
      datetime-attr
      (string-trim
        (replace-regexp-in-string "--" "&#x2013;"
          (org-html-plain-text as-plain-text info))))))
(defun jf/org-mode/get-blog-post (&optional element)
  "Get blogPost associated with current ELEMENT.

When none found return `nil'."
  (let ((epom
          (or element (org-element-at-point))))
    (car
      (seq-filter
        (lambda (hl)
          (and
            (eq (org-element-type hl) 'headline)
            (member jf/denote/keywords/blogPosts
              (org-element-property :tags hl))
            hl))
        (org-element-lineage epom nil t)))))

(defun jf/blog-post/tootify (&optional at-point)
  "Create a toot from the current blog post AT-POINT."
  (interactive)
  (if-let* ((blogPost
             (jf/org-mode/get-blog-post
               (or at-point (org-element-at-point)))))
    (progn
      (call-interactively #'mastodon-toot)
      (delete-rectangle (point-min) (point-max))
      (insert
        (s-join "\n\n"
          (flatten-list
            (list
              (format "«%s»"
                (org-element-property :title blogPost))
              (org-entry-get blogPost "DESCRIPTION")
              (when-let* ((roam_refs
                           (org-entry-get blogPost "ROAM_REFS")))
                (car (s-split " " roam_refs)))
              (s-join " "
                (mapcar
                (lambda (e) (format "#%s" e))
                (org-get-tags blogPost t))))))))
    (progn
      (user-error "Unable to find blogPost at point.")
      (pulsar-pulse-line-red))))

(advice-add 'org-html-special-block
  :around #'jf/org-html-special-block)

(defun jf/org-html-special-block (func special-block contents info)
  "Handle SPECIAL-HANDLE when details, quote, or marginnote.

Otherwise, use pre-existing handling."
  (let* ((block-type
           (org-element-property :type special-block))
          (params
            (flatten-list
              (mapcar (lambda (cell)
                        (list (car cell) (cdr cell)))
                (car (mapcar (lambda (h)
                               (org-babel-parse-header-arguments h t))
                       (cons (org-element-property :parameters special-block)
                         (org-element-property :header special-block))))))))
    (pcase block-type
      ("details"
        (format "<details%s>%s%s</details>"
          (if (plist-get params :open) " open" "")
          (if-let* ((summary
                     (plist-get params :summary)))
            (concat "<summary>" summary "</summary>\n")
            "")
          contents))
      ("update"
        (format
          "{{< update %s >}}\n%s\n{{< /update >}}"
          (org-html--make-attribute-string params)
          contents))
      ("blockquote"
        ;; TODO: handle pre, post, etc
        (format "{{< blockquote %s >}}\n%s{{< /blockquote >}}"
          (org-html--make-attribute-string params)
          contents))
      ("marginnote"
        (concat "{{< marginnote >}}" contents "{{< /marginnote >}}"))
      (_ (apply func (list special-block contents info))))))

(defun jf/org-md-quote-block (func quote-block contents info)
  "Render a QUOTE-BLOCK with CONTENTS and INFO.

Either render via the standard markdown way or when exporting to
Take on Rules using the \"blockquote\" special block."
  (if jf/exporting-org-to-tor
    (progn
      (org-element-put-property quote-block :type "blockquote")
      (jf/org-html-special-block func quote-block contents info))
    (apply func (list quote-block contents info))))

(defun jf/org-html-quote-block (func quote-block contents info)
  "Render a QUOTE-BLOCK with CONTENTS and INFO.

Either render via the standard markdown way or when exporting to
Take on Rules using the \"blockquote\" special block."
  (if jf/exporting-org-to-tor
    (progn
      (org-element-put-property quote-block :type "blockquote")
      (jf/org-html-special-block func quote-block contents info))
    (apply func (list quote-block contents info))))

(advice-add #'org-html-quote-block :around #'jf/org-html-quote-block)
(advice-add #'org-md-quote-block :around #'jf/org-md-quote-block)

(defvar-local jf/org-latex-src-block-skip nil
  "When non-nil skip exporting src and example blocks for LaTeX.")

(defun jf/org-latex-src-block (&rest args)
  (if jf/org-latex-src-block-skip
    "\\vspace{5mm}\\fbox{Code omitted for brevity}\\vspace{5mm}\n"
    (apply args)))

(advice-add #'org-latex-example-block :around #'jf/org-latex-src-block)
(advice-add #'org-latex-src-block :around #'jf/org-latex-src-block)

(provide 'ox-takeonrules)
;;; ox-takeonrules.el ends here
