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
              (lax-plist-get properties "ROAM_REFS")))
          (closed-at
            (when published-p
              (format-time-string "[%F %a %H:%M]"
                (date-to-time
                  (lax-plist-get properties "HUGO_PUBLISHDATE")))))
          )
    ;; DO WE HAVE FOOTNOTES?
    (with-current-buffer (find-file-noselect
                           jf/personal/filename-for-journal)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          ;; Ensure we have the month's headline
          (unless (search-forward-regexp
                    (concat "^\\*\\* +"
                      (format-time-string "%Y-%m %B" time)) nil t)
            (progn
              (search-forward-regexp
                (concat "^\\* +"
                  (format-time-string "%Y" time)))
              (org-insert-subheading '(4))
              (insert "** " (format-time-string "%Y-%m %B" time))))
          ;; Ensure we have the day's headline
          (unless (search-forward-regexp
                    (concat "^\\*\\*\\* +" parent-headline-title) nil t)
            (progn
              (search-forward-regexp
                (concat "^\\*\\* +"
                  (format-time-string "%Y-%m %B" time)))
              (org-insert-subheading '(4))
              (insert parent-headline-title)))
          (org-insert-subheading '(4))
          (insert (if published-p "PUBLISHED " "DRAFT ")
            (lax-plist-get properties "TITLE")
            " "
            (lax-plist-get properties "FILETAGS")
            (if closed-at
              (concat
                "\nCLOSED: "
                closed-at)
                "")
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
                (f-base file)
                "\n:PUBLISHED_AT: "
                (lax-plist-get properties "HUGO_PUBLISHDATE"))
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

;; A list of denote identifiers that I'm going to be ingesting into a
;; journal.
(setq identifiers '(
                     ;; "20250101T085001"
                     ;; "20250104T093754"
                     ;; "20250105T075810"
                     ;; "20250106T113811"
                     ;; "20250107T082446"
                     ;; "20250107T183243"
                     ;; "20250110T171802"
                     ;; "20250111T063043"
                     ;; "20250112T205723"
                     ;; "20250113T063713"
                     ;; "20250113T183357"
                     ;; "20250114T104916"
                     ;; "20250115T083225"
                     ;; "20250116T082654"
                     ;; "20250117T162430"
                     ;; "20250118T061015"
                     ;; "20250120T053808"
                     ;; "20250120T202237"
                     ;; "20250122T105133"
                     ;; "20250122T194932"
                     ;; "20250123T094551"
                     ;; "20250127T172223"
                     ;; "20250129T071405"
                     ;; "20250129T175338"
                     ;; "20250204T183116"
                     ;; "20250205T201043"
                     ;; "20250207T194529"
                     ;; "20250208T124907"
                     ;; "20250301T143446"
                     ;; "20250309T210928"
                     ;; "20250310T140745"
                     ;; "20250314T142830"
                     ;; "20250319T133918"
                     ;; "20250325T095903"
                     ;; "20250327T074541"
                     ;; "20250327T082356"
                     ;; "20250402T184910"
                     ;; "20250408T093243"
                     ;; "20250409T122434"
                     ;; "20250410T104847"
                     ;; "20250411T112545"
                     ;; "20250414T093730"
                     ;; "20250415T113302"
                     ;; "20250415T140348"
                     ;; "20250417T120140"
                     ;; "20250502T112002"
                     ;; "20250502T164508"
                     ;; "20250503T083457"
                     ;; "20250504T090346"
                     ;; "20250505T081314"
                     ;; "20250506T164359"
                     ;; "20250510T183456"
                     ;; "20250523T133027"
                     ;; "20250524T121747"
                     ;; "20250528T140108"
                     ;; "20250530T085004"
                     ;; "20250605T072051"
                     ;; "20250608T081003"
                     ;; "20250609T084513"
                     ;; "20250612T081424"
                     ;; "20250616T120103"
                     ;; "20250617T075329"
                     ;; "20250623T200252"
                     ;; "20250627T125456"
                     ;; "20250628T080426"
                     ;; "20250709T215652"
                     ;; "20250728T173103"
                     ))

(dolist (identifier identifiers)
  (ingest-blog-post identifier)
  (save-buffer))

(dolist (identifier identifiers)
  (with-current-buffer (find-file-noselect jf/personal/filename-for-journal)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (replace-string
          (format "[denote:%s]" identifier)
          (format "[denote:20241114T075414::#%s]" identifier))
        (save-buffer)))))

;; Ran query-replace-regexp with the following:
;; ::GLOSSARY-\([^]]+\) --> \,(upcase \&)
