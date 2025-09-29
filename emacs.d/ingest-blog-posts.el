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

(with-current-buffer (find-file-noselect jf/personal/filename-for-journal)
  (save-excursion
    (save-restriction
      (widen)
      (dolist (before-after '(("[dentoe:20250101T085001]" . "[denote:20241114T075414::#20250101T085001]")
                               ("[dentoe:20250104T093754]" . "[denote:20241114T075414::#20250104T093754]")
                               ("[denote:20250105T075810]" . "[denote:20241114T075414::#20250105T075810]")
                               ("[denote:20250106T113811]" . "[denote:20241114T075414::#20250106T113811]")
                               ("[denote:20250107T082446]" . "[denote:20241114T075414::#20250107T082446]")
                               ("[denote:20250107T183243]" . "[denote:20241114T075414::#20250107T183243]")
                               ("[denote:20250110T171802]" . "[denote:20241114T075414::#20250110T171802]")
                               ("[denote:20250111T063043]" . "[denote:20241114T075414::#20250111T063043]")
                               ("[denote:20250112T205723]" . "[denote:20241114T075414::#20250112T205723]")
                               ("[denote:20250113T063713]" . "[denote:20241114T075414::#20250113T063713]")
                               ("[denote:20250113T183357]" . "[denote:20241114T075414::#20250113T183357]")
                               ("[denote:20250114T104916]" . "[denote:20241114T075414::#20250114T104916]")
                               ("[denote:20250115T083225]" . "[denote:20241114T075414::#20250115T083225]")
                               ("[denote:20250116T082654]" . "[denote:20241114T075414::#20250116T082654]")
                               ("[denote:20250117T162430]" . "[denote:20241114T075414::#20250117T162430]")
                               ("[denote:20250118T061015]" . "[denote:20241114T075414::#20250118T061015]")
                               ("[denote:20250120T053808]" . "[denote:20241114T075414::#20250120T053808]")
                               ("[denote:20250120T202237]" . "[denote:20241114T075414::#20250120T202237]")
                               ("[denote:20250122T105133]" . "[denote:20241114T075414::#20250122T105133]")
                               ("[denote:20250122T194932]" . "[denote:20241114T075414::#20250122T194932]")
                               ("[denote:20250123T094551]" . "[denote:20241114T075414::#20250123T094551]")
                               ("[denote:20250127T172223]" . "[denote:20241114T075414::#20250127T172223]")
                               ("[denote:20250129T071405]" . "[denote:20241114T075414::#20250129T071405]")
                               ("[denote:20250129T175338]" . "[denote:20241114T075414::#20250129T175338]")
                               ("[denote:20250204T183116]" . "[denote:20241114T075414::#20250204T183116]")
                               ("[denote:20250205T201043]" . "[denote:20241114T075414::#20250205T201043]")
                               ("[denote:20250207T194529]" . "[denote:20241114T075414::#20250207T194529]")
                               ("[denote:20250208T124907]" . "[denote:20241114T075414::#20250208T124907]")
                               ("[denote:20250301T143446]" . "[denote:20241114T075414::#20250301T143446]")
                               ("[denote:20250309T210928]" . "[denote:20241114T075414::#20250309T210928]")
                               ("[denote:20250310T140745]" . "[denote:20241114T075414::#20250310T140745]")
                               ("[denote:20250314T142830]" . "[denote:20241114T075414::#20250314T142830]")
                               ("[denote:20250319T133918]" . "[denote:20241114T075414::#20250319T133918]")
                               ("[denote:20250320T225456]" . "[denote:20241114T075414::#20250320T225456]")
                               ("[denote:20250325T095903]" . "[denote:20241114T075414::#20250325T095903]")
                               ("[denote:20250327T074541]" . "[denote:20241114T075414::#20250327T074541]")
                               ("[denote:20250327T082356]" . "[denote:20241114T075414::#20250327T082356]")
                               ("[denote:20250402T184910]" . "[denote:20241114T075414::#20250402T184910]")
                               ("[denote:20250408T093243]" . "[denote:20241114T075414::#20250408T093243]")
                               ("[denote:20250409T122434]" . "[denote:20241114T075414::#20250409T122434]")
                               ("[denote:20250410T104847]" . "[denote:20241114T075414::#20250410T104847]")
                               ("[denote:20250411T112545]" . "[denote:20241114T075414::#20250411T112545]")
                               ("[denote:20250414T093730]" . "[denote:20241114T075414::#20250414T093730]")
                               ("[denote:20250415T113302]" . "[denote:20241114T075414::#20250415T113302]")
                               ("[denote:20250415T140348]" . "[denote:20241114T075414::#20250415T140348]")
                               ("[denote:20250417T120140]" . "[denote:20241114T075414::#20250417T120140]")
                               ("[denote:20250502T112002]" . "[denote:20241114T075414::#20250502T112002]")
                               ("[denote:20250502T164508]" . "[denote:20241114T075414::#20250502T164508]")
                               ("[denote:20250503T083457]" . "[denote:20241114T075414::#20250503T083457]")
                               ("[denote:20250504T090346]" . "[denote:20241114T075414::#20250504T090346]")
                               ("[denote:20250505T081314]" . "[denote:20241114T075414::#20250505T081314]")
                               ("[denote:20250506T164359]" . "[denote:20241114T075414::#20250506T164359]")
                               ("[denote:20250510T183456]" . "[denote:20241114T075414::#20250510T183456]")
                               ("[denote:20250523T133027]" . "[denote:20241114T075414::#20250523T133027]")
                               ("[denote:20250524T121747]" . "[denote:20241114T075414::#20250524T121747]")
                               ("[denote:20250528T140108]" . "[denote:20241114T075414::#20250528T140108]")
                               ("[denote:20250530T085004]" . "[denote:20241114T075414::#20250530T085004]")
                               ("[denote:20250605T072051]" . "[denote:20241114T075414::#20250605T072051]")
                               ("[denote:20250608T081003]" . "[denote:20241114T075414::#20250608T081003]")
                               ("[denote:20250609T084513]" . "[denote:20241114T075414::#20250609T084513]")
                               ("[denote:20250612T081424]" . "[denote:20241114T075414::#20250612T081424]")
                               ("[denote:20250613T075044]" . "[denote:20241114T075414::#20250613T075044]")
                               ("[denote:20250616T120103]" . "[denote:20241114T075414::#20250616T120103]")
                               ("[denote:20250617T075329]" . "[denote:20241114T075414::#20250617T075329]")
                               ("[denote:20250623T200252]" . "[denote:20241114T075414::#20250623T200252]")
                               ("[denote:20250627T125456]" . "[denote:20241114T075414::#20250627T125456]")
                               ("[denote:20250628T080426]" . "[denote:20241114T075414::#20250628T080426]")
                               ("[denote:20250709T215652]" . "[denote:20241114T075414::#20250709T215652]")
                               ("[denote:20250728T173103]" . "[denote:20241114T075414::#20250728T173103]")))
        (goto-char (point-min))
        (replace-string (car before-after) (cdr before-after)))
      (save-buffer))))
