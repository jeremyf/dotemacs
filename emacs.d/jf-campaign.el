(defvar jf/campaign/file-name
  "~/git/org/denote/indices/20231127T184806--the-shadows-of-mont-brun-status-document__campaigns_projects_rpgs_StatusDocuments.org"
  "The current campaign file.")

;; Function assumes that we have a heading with a tag and direct subheading.
(cl-defun jf/campaign/named-element (&key tag)
  "Fetch PROPERTY from headlines with parent that has given TAG."
  (with-current-buffer (find-file-noselect jf/campaign/file-name)
    (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
      (lambda (headline)
        (and (member tag
               (org-element-property
                 :tags
                 ;; Get immediate parent
                 (car (org-element-lineage headline))))
          (org-element-property :title headline))))))

(defun jf/campaign/random-npc-as-entry ()
  "Create an NPC entry."
  (let* ((random-table/reporter
           ;; Bind a function that will only output the results of the
           ;; table, excluding the expression that generated the
           ;; results.
           (lambda (expression results) (format "%s" results)))
          (name
            (random-table/roll "In the Shadows of Mont Brun > Names"))
          (quirk
            (random-table/roll "Random NPC Quirks"))
          (alignment
            (random-table/roll "Errant > Alignment"))
          (lore-table
            (random-table/roll "The One Ring > Lore"))
          (locations
            (s-join ", "
              (completing-read-multiple
                "Location(s): "
                (jf/campaign/named-element :tag "locations"))))
          (factions
            (s-join ", "
              (completing-read-multiple
                "Faction(s): "
                (jf/campaign/named-element :tag "factions")))))
    (format (concat
              "%s\n:PROPERTIES:\n::BACKGROUND:\n"
              ":LOCATIONS:  %s\n:DEMEANOR:\n:ALIGNMENT:  %s\n"
              ":QUIRKS:  %s\n:FACTIONS:  %s\n:END:\n\n%s")
      name locations alignment quirk factions lore-table)))

(defun jf/campaign/populate-property-draw-value (&optional headline property table)
  "Populate HEADLINE's direct children's empty PROPERTY with roll on given TABLE.

When given no HEADLINE, prompt for ones from the document.

When given no PROPERTY, prompt for a property.

When given no TABLE, prompt for an expression to evaluate via
`random-table/roll'."
  (interactive)
  (let* ((level 1)
          (headline (or headline
                      (completing-read "Headline: "
                        (jf/org-get-headlines level) nil t)))
          (property (or property
                      (org-read-property-name)))
          (table (or table
                   (completing-read "Table: "
                     random-table/storage/tables)))
          (random-table/reporter
            ;; Bind a function that will only output the results of the
            ;; table, excluding the expression that generated the
            ;; results.
            (lambda (expression results) (format "%s" results))))
    (unless (org--valid-property-p property)
      (user-error "Invalid property name: \"%s\"" property))
    (save-excursion
      ;; Now that we have the information, let’s start rolling on some
      ;; tables.
      (dolist
        ;; Because we will not be recalculating heading positions, we
        ;; need to reverse the list, as updates to the last element will
        ;; not alter the position of the next to last element.
        (subheading (reverse
                      (org-element-map
                        (org-element-parse-buffer)
                        'headline
                        (lambda (el)
                          (and
                            ;; We want the children of the given level
                            (=
                              (+ 1 level)
                              (org-element-property :level el))
                            (string=
                              ;; Match the text
                              (org-element-property
                                :raw-value
                                (car (org-element-lineage el)))
                              headline)
                            ;; Don't clobber already set values
                            (not (org-element-property property el))
                            ;; We found a match now return it
                            el)))))
        ;; We have finally found the element, now roll on the table and
        ;; populate.  Unfortunately this does not re-calculate
        ;; positions.
        (org-entry-put
          (goto-char (org-element-property :begin subheading))
          property (random-table/roll table))))))
