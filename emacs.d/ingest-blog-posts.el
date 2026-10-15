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

(defvar container-id "20240101T000001")

(defun ingest-blog-post (identifier)
  "Insert the given blog by IDENTIFIER into my journal."
  (let* ((file
          (denote-get-path-by-id identifier))
         (_
          (if file
              (unless (f-exists? file)
                (user-error "Missing %s" file))
            (user-error "File with identifier %S not found" identifier)))
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
                  (plist-put p :slug
                             (or
                              (lax-plist-get p "HUGO_SLUG")
                              (progn
                                (goto-char (point-min))
                                (when
                                    (search-forward-regexp
                                     "^\\#\\+HUGO_CUSTOM_FRONT_MATTER: +:slug +" nil t)
                                  (let ((beg (point)))
                                    (end-of-line)
                                    (buffer-substring-no-properties beg (point)))))
                              (user-error "Unable to find :slug for %S" identifier)))
                  (plist-put p :pubdate
                             (or
                              (lax-plist-get p "HUGO_PUBLISHDATE")
                              (progn
                                (goto-char (point-min))
                                (when
                                    (search-forward-regexp
                                     "^\\#\\+HUGO_CUSTOM_FRONT_MATTER: +:date +" nil t)
                                  (let ((beg (point)))
                                    (end-of-line)
                                    (buffer-substring-no-properties beg (point)))))
                              (user-error "Unable to find :pubdate for %S" identifier)))
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
                                 (lax-plist-get properties :pubdate)))))
         )
    ;; DO WE HAVE FOOTNOTES?
    (with-current-buffer
        (find-file-noselect (denote-get-path-by-id container-id))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
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
              (insert (format-time-string "%Y-%m %B" time))))
          ;; Ensure we have the day's headline
          (goto-char (point-min))
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
                       (lax-plist-get properties :slug)
                       "\n:DESCRIPTION: "
                       (lax-plist-get properties "DESCRIPTION")
                       "\n:EXPORT_FILE_NAME: "
                       (f-base file)
                       "\n:PUBLISHED_AT: "
                       (lax-plist-get properties :pubdate))
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
      (f-delete file))))

;; A list of denote identifiers that I'm going to be ingesting into a
;; journal.
(setq identifiers '(
                    "20240204T110117"
                    "20241201T092315"
                    "20240206T175335"
                    "20241203T102627"
                    "20241122T135402"
                    "20240220T151913"
                    "20240602T205430"
                    "20240224T224550"
                    "20240827T150253"
                    "20240601T093420"
                    "20241022T170411"
                    "20240115T211750"
                    "20240811T081704"
                    "20240108T144512"
                    "20241118T170941"
                    "20240831T084740"
                    "20240608T091214"
                    "20240514T170219"
                    "20241127T211811"
                    "20240503T190542"
                    "20240505T083659"
                    "20241101T183617"
                    "20241208T085307"
                    "20241122T081428"
                    "20240106T094035"
                    "20240930T184835"
                    "20240224T163506"
                    "20241114T075414"
                    "20240722T073733"
                    "20240113T221058"
                    "20240301T190908"
                    "20241004T190010"
                    "20241012T200014"
                    "20240731T142819"
                    "20241218T090111"
                    "20241204T204951"
                    "20240504T224148"
                    "20241124T080648"
                    "20240521T180032"
                    "20240111T214017"
                    "20240127T150830"
                    "20241228T100547"
                    "20241125T170531"
                    "20240117T193353"
                    "20240104T100820"
                    "20240224T122551"
                    "20240518T105400"
                    "20240114T212119"
                    "20241204T181035"
                    "20241217T073916"
                    "20241212T164846"
                    "20241207T134939"
                    "20241119T092628"
                    "20241202T190222"
                    "20240519T152314"
                    "20240621T131413"
                    "20241105T170107"
                    "20240211T085751"
                    "20240316T175356"
                    "20240131T180355"
                    "20240726T211818"
                    "20241005T080335"
                    "20241204T124745"
                    "20241120T173827"
                    "20241118T100331"
                    "20241022T202951"
                    "20241231T145420"
                    "20240110T205452"
                    "20241027T104245"
                    "20241101T170823"
                    "20240228T084742"
                    "20241204T172133"
                    "20240803T081723"
                    "20240102T103545"
                    "20240522T181409"
                    "20241108T202424"
                    "20240211T091410"
                    "20240728T085447"
                    "20241108T160854"
                    "20240108T180705"
                    "20240528T180814"
                    "20240102T095516"
                    "20240109T182730"
                    "20241227T142653"
                    "20240202T083431"
                    "20240804T081820"
                    "20240103T105250"
                    "20240103T133505"
                    "20241031T210918"
                    "20240515T173056"
                    "20240517T171736"
                    "20240107T105502"
                    "20240129T175021"
                    "20240110T215321"
                    "20240718T075718"
                    "20240619T082959"
                    "20241020T131201"
                    "20241116T071226"
                    "20241212T221431"
                    "20240922T195748"
                    "20240123T232919"
                    "20240626T122743"
                    "20240316T154703"
                    "20241117T093718"
                    "20241130T095958"
                    "20241228T083116"
                    "20240107T121614"
                    "20241124T080648"
                    "20241106T175631"
                    "20240117T083905"
                    "20241105T194918"
                    "20240113T224930"
                    "20240723T180920"
                    "20241008T201225"
                    "20240105T093756"
                    "20241218T163133"
                    "20241213T081312"
                    "20241124T110238"
                    "20240302T075548"
                    "20241125T083650"
                    "20240101T173349"
                    "20241219T104327"
                    "20241127T135148"
                    "20240106T115203"
                    "20240318T142600"
                    "20240221T111022"
                    "20240106T105421"
                    "20240317T190004"
                    ))

;; (dolist (identifier identifiers)
;;   (ingest-blog-post identifier)
;;   (save-buffer))

(dolist (journal-id '("20241114T075414" "20240101T000001" "20260101T093750"))
  (dolist (identifier identifiers)
    (with-current-buffer (find-file-noselect
                          (denote-get-path-by-id journal-id))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (replace-string
           (format "[denote:%s]" identifier)
           (format "[denote:%s::#%s]" container-id identifier))
          (save-buffer))))))

;; Ran query-replace-regexp with the following:
;; ::GLOSSARY-\([^]]+\) --> \,(upcase \&)
