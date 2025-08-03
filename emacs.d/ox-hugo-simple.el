;;; ox-hugo-simple --- A Simplified Hugo Export -*- lexical-binding: t -*-

;; Copyright (C) 2024 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;; A less configurable, more opinionated `org-mode' to Hugo exporter.
;;; Code:

;;; Variables

;; TODO: Handle margin notes and other sort of blocks
;; TODO: Handle date time.
(org-export-define-derived-backend 'takeonrules 'md
  :translate-alist
  '(
     (footnote-reference . org-hugo-simple-footnote-reference)
     (inner-template . org-hugo-simple-inner-template)
     (timestamp . org-hugo-simple-timestamp)
     )
  :filters-alist
  '((:filter-body org-hugo-simple-body-filter))
  :options-alist
  '((:with-toc nil "toc" nil)))

(defun ox-tor-export ()
  "Export current org element at point to TakeOnRules post."
  (interactive)
  (org-set-regexps-and-options)
  ;; (jf/bibliography/export-to-takeonrules)
  ;; Get element at point, then walk-up ancestry tree, finding node
  ;; that has :blogPosts: tag.
  (unless (derived-mode-p 'org-mode)
    (user-error "Current buffer not 'org-mode"))
  (let* ((blogPost
           (car
             (seq-filter
               (lambda (hl)
                 (and
                   (eq (org-element-type hl) 'headline)
                   (member jf/denote/keywords/blogPosts
                     (org-element-property :tags hl))
                   hl))
               (org-element-lineage (org-element-at-point) nil t)))))
    (unless blogPost
      (user-error "Current node is not child of headline tagged with :%s:" jf/denote/keywords/blogPosts))

    ;; Ensure we set metadata that we will use in the export for initial
    ;; publication as well as future updates (if any)
    (let ((slug
            (or (org-element-property :SLUG blogPost)
              (let ((s (jf/denote-sluggify-title
                         (org-element-property :title blogPost))))
                (org-entry-put blogPost "SLUG" s)
                s))))
      (or (org-element-property :CUSTOM_ID blogPost)
        (org-entry-put blogPost "CUSTOM_ID"
          (format "blogPost-%s"
            (jf/denote-sluggify-title
              (org-element-property :title blogPost)))))
      (or (org-element-property :PUBLISHED_AT blogPost)
        (org-entry-put blogPost "PUBLISHED_AT"
          (format-time-string "%Y-%m-%d %H:%M:%S %z")))
      (or (org-element-property :DESCRIPTION blogPost)
        (org-entry-put blogPost "DESCRIPTION" (read-string "Description: ")))
      (or (org-element-property :EXPORT_FILE_NAME blogPost)
        (org-entry-put blogPost "EXPORT_FILE_NAME"
          (format
              "%s--%s"
              (format-time-string "%Y%m%dT%H%M%S")
              slug))))
    (save-buffer)
    (let* ((file
             (org-export-output-file-name ".md" t
               (f-join jf/tor-home-directory "content" "posts"
                 (format-time-string "%Y")))))
      (and (org-export-to-file 'takeonrules file nil t t t)
        (org-open-file file)))))

(defun org-hugo-simple-inner-template (contents info)
  "Transcode CONTENTS to markdown body.

INFO is a plist holding contextual information."
  contents)

(defun org-hugo-simple-footnote-reference (footnote-reference _contents info)
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
            (save-excursion
              (save-restriction
                (widen)
            (s-trim
              (org-export-string-as
                (buffer-substring-no-properties beg end)
                'md t '(:with-toc nil)))))))
    (format "{{< sidenote >}}%s{{< /sidenote >}}" content)))

(defun org-hugo-simple-body-filter (body _backend info)
  "Add front-matter to the BODY of the document.

BODY is the result of the export.
INFO is a plist holding export options."
  (let ((fm
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
              (org-hugo-simple--front-matter info)))))

    (format "---\n%s\n---\n%s" fm body)))

(defun org-hugo-simple--front-matter (info)
  "Generate the corresponding YAML Front-Matter from INFO.

We also rely on the org-element at point."
  (let ((description
          (org-entry-get (point) "DESCRIPTION"))
         (published_at
           (org-entry-get (point) "PUBLISHED_AT"))
         (tags
           ;; TODO: Intersect
           (mapcar
             #'string-inflection-kebab-case-function
             (seq-intersection
               (org-get-tags (point) t)
               denote-known-keywords #'string=)))
         (title
           (car (plist-get info :title)))
         (org_id
           (org-entry-get (point) "CUSTOM_ID")))
    (yaml-encode `(("title" . ,title)
                    ("description" . ,description)
                    ("org_id" . ,org_id)
                    ("date" . ,published_at)
                    ("tags" . ,tags)
                    ;; These could become a variable, but for now, we'll
                    ;; leave things hard-coded.
                    ("author" . ("Jeremy Friesen"))
                    ("layout" . "post")
                    ("type" . "post")
                    ("draft" . "true")
                    ("licenses" . ("by-nc-nd-4_0"))))))

(defun org-hugo-simple-timestamp (timestamp _contents info)
  "Transcode a TIMESTAMP object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let* ((as-plain-text
           (org-timestamp-translate timestamp))
          (time
            (org-timestamp-to-time timestamp))
          (datetime-attr
            (if (string-match-p ":[[:digit:]][[:digit:]]\>$" as-plain-text)
              (format-time-string "%Y-%m-%d %R" time)
              (format-time-string "%Y-%m-%d" time))))
    (format "<time datetime=\"%s\">%s</time>"
      datetime-attr
      (string-trim
        (replace-regexp-in-string "--" "&#x2013;" (org-html-plain-text as-plain-text info))))))

(provide 'ox-hugo-simple)
;;; ox-hugo-simple.el ends here
