;; -*- lexical-binding: t; -*-

;;; Overview

;; Ingest blog posts into my journal.

;;; Commentary

;; I want to take a blog post (written in org-mode) and migrate it into
;; my journal.  For the time being, I want to focus on blog posts from
;; 2025.
;;
;; I will need to account for backlinks and footnotes.  This will also
;; likely mean that I'll want to starting caching my epigraphs.

(defun ingest-blog-post (identifier)
  "Insert the given blog by IDENTIFIER into my journal."
  (let* ((file
           (denote-get-path-by-id identifier))
          (_
            (unless (f-exists? file)
              (user-error "Missing %s" file)))
          (time
            (date-to-time identifier))
          (parent-headline-title
            (format-time-string "%F %A" time))
          (properties
            (with-current-buffer (find-file-noselect file)
              (save-excursion
                (save-restriction
                  (widen)
                  (let ((p
                          (jf/org-keywords-as-plist
                            :keywords-regexp
                            (concat "\\(TITLE\\|FILETAGS\\|ROAM_REFS\\|"
                              "HUGO_SLUG\\|HUGO_PUBLISHDATE\\|"
                              "DESCRIPTION\\)"))))
                    (plist-put p :content-and-footnotes
                      (progn
                        (goto-char (point-min))
                        (while (org-match-line "^\\(#\+.+\\|\s*\\)$")
                          (next-line))
                        (beginning-of-line)
                        (let ((the-point
                                (point)))
                          (if (search-forward-regexp "^\* Footnotes" nil t)
                            (beginning-of-line)
                            (goto-char (point-max)))
                          (cons
                            (concat "\n" (buffer-substring the-point (point)))
                            (if (org-match-line "^\* Footnotes")
                              (progn
                                (next-line)
                                (beginning-of-line)
                                (buffer-substring (point) (point-max)))
                              nil)))))
                    ;; (kill-buffer)
                    p)))))
          (content
            (s-replace "[fn:" (concat "[fn:" identifier "-")
              (s-replace-regexp
                "^\*" "*****" (car (lax-plist-get properties :content-and-footnotes)))))
          (footnotes
            (if-let* ((fn
                        (cdr (lax-plist-get properties :content-and-footnotes))))
              (s-replace "[fn:" (concat "[fn:" identifier "-") fn)
              nil))
          (published-p
            (s-present?
              (lax-plist-get properties "ROAM_REFS"))))
    ;; DO WE HAVE FOOTNOTES?
    (with-current-buffer (find-file-noselect
                           jf/personal/filename-for-journal)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          ;; Position to the parent headline.  This will error if one
          ;; does not exist
          (search-forward-regexp
            (concat "^\\*\\*\\* +" parent-headline-title))
          (org-insert-subheading '(4))
          (insert (if published-p "PUBLISHED " "DRAFT ")
            (lax-plist-get properties "TITLE")
            " "
            (lax-plist-get properties "FILETAGS")
            "\nCLOSED: "
            (format-time-string "[%F %a %H:%M]" time)
            "\n:PROPERTIES:"
            "\n:CUSTOM_ID: "
            identifier
            "\n:ID: "
            identifier
            (if published-p
              (concat
                "\n:ROAM_REFS: "
                (lax-plist-get properties "ROAM_REFS")
                "\n:SLUG: "
                (lax-plist-get properties "HUGO_SLUG")
                "\n:DESCRIPTION: "
                (lax-plist-get properties "DESCRIPTION")
                "\n:EXPORT_FILE_NAME: "
                (f-base file))
              "")
            "\n:END:"
            "\n"
            content
            )
          (when footnotes
            (progn
              (search-forward-regexp "^\* Footnotes")
              (next-line)
              (insert footnotes "\n")))))
      (save-buffer)
      (f-delete file)
      )))
