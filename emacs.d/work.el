(dir-locals-set-class-variables
  'go-lang
  '((nil . ((projectile-git-fd-args .
              "-H -0 -tf --strip-cwd-prefix -c never -E vendor/ -E pkg/ -E docs/ -E .git")))))

(dir-locals-set-directory-class
  "~/git/converge-cloud/marketplace-provider" 'go-lang)

(dir-locals-set-directory-class
  "~/git/converge-cloud/concierge" 'go-lang)

(dir-locals-set-directory-class
  "~/git/converge-cloud/morpho-account-service" 'go-lang)

(dir-locals-set-directory-class
  "~/git/converge-cloud/morpho-service-broker" 'go-lang)


;; At my current employer, I have quarterly managed business objectives
;; (MBOs).  These focal activities involve: initiation, tracking work,
;; and completion (with supporting documentation).
;;
;; **Initiation** :: Writing the stated objective with narrative around
;; the goal, measurement, and artifacts.
;;
;; **Tracking Work** :: As I work through these objectives, I'm
;; recording and tracking tasks towards the larger MBO.  Some tasks
;; become blocked and I need to work towards their resolution by
;; providing sustained energy up and out; to get help with my team.
;;
;; **Completion** :: This involves providing documentation demonstrating
;; the completion of the MBO.  I can leverage the tracking information
;; to provide the documentation.
;;
;; The two capture template letters reflect what I'm using for my
;; personal (non-work related) notes.  So I'm hoping to piggy back on
;; that muscle memory as I adopt these tracking approaches.
(defvar jf/work/filename-for-mbos
  (denote-get-path-by-id "20250117T101521")
  "Where I put my MBOs.")

(add-to-list 'org-capture-templates
  '("T" "Add to task for MBO"
     plain (file+function
             jf/work/filename-for-mbos
             jf/work/position-at-end-of-mbo-task-entity)
     "%T :: %?"
     :empty-lines-before 1
     :empty-lines-after 1
     :clock-in t
     :clock-resume t))

(add-to-list 'org-capture-templates
  '("t" "New task for MBO"
     entry (file+function
             jf/work/filename-for-mbos
             jf/work/position-at-start-of-mbo)
     "TODO %^{Task} :tasks:\n:PROPERTIES:\n:CUSTOM_ID: %(org-id-new)\n:END:\n%?"
     :empty-lines-before 1
     :empty-lines-after 1
     :clock-in t
     :clock-resume t))

(add-to-list 'org-capture-templates
  '("j" "Journal entry for MBO"
     plain (file+function
             jf/work/filename-for-mbos
             jf/work/position-in-mbo-journal-entry)
     "%T :: %?"
     :empty-lines-before 1
     :empty-lines-after 1
     :clock-in t
     :clock-resume t))

(defvar jf/work/org-map-entries-mbo-task-filter
  "+LEVEL=4+mbos+tasks-journals+TODO!=\"DONE\""
  "The default filter for collecting MBO tasks.")

(defun jf/work/position-in-mbo-journal-entry ()
  "Position point just before the selected journal.

Included to allow the re-use of logic for different 3rd level tasks."
  (let ((jf/work/org-map-entries-mbo-task-filter
          "+LEVEL=4+mbos+tasks+journals"))
    (jf/work/position-at-end-of-mbo-task-entity)))

(defun jf/work/position-at-end-of-mbo-task-entity ()
  "Position point just before content end of MBO task.

For inserting plain text."
  (let* ((incomplete-mbo-tasks
           (org-map-entries
             (lambda ()
               (let* ((task
                        (org-element-at-point))
                       (mbo
                         (car (org-element-lineage task)))
                       (quarter
                         (caddr (org-element-lineage task))))
                 (cons
                   (concat
                     (propertize
                       (org-element-property :title quarter)
                       'face 'org-level-1)
                     (propertize
                       " > "
                       'face 'consult-separator)
                     (propertize
                       (org-element-property :title mbo)
                       'face 'org-level-3)
                     (propertize
                       " > "
                       'face 'consult-separator)
                     (propertize
                       (org-element-property :title task)
                       'face 'org-level-4)
                     )
                   (org-element-property :contents-end task))))
             jf/work/org-map-entries-mbo-task-filter))
          (task
            (completing-read
              "Task: " incomplete-mbo-tasks nil t)))
    ;; The contents-end of the task is the begining of the line that
    ;; contains the next heading.  Which the capture process for a
    ;; "plain" item then interprets as "add an item to the current
    ;; heading".  Not ideal, so move point backwards one character.
    (goto-char
      (1- (alist-get task incomplete-mbo-tasks nil nil #'string=)))))

(defun jf/work/position-at-start-of-mbo ()
  "Position point at start of MBO.

For inserting entity."
  (let* ((incomplete-mbos
           (org-map-entries
             (lambda ()
               (let* ((mbo
                        (org-element-at-point))
                       (quarter
                         (cadr (org-element-lineage mbo))))
                 (cons
                   (concat
                     (propertize
                       (org-element-property :title quarter)
                       'face 'org-level-1)
                     (propertize
                       " > "
                       'face 'consult-separator)
                     (propertize
                       (org-element-property :title mbo)
                       'face 'org-level-3)
                     )
                   (org-element-property :begin mbo))))
             "+LEVEL=3+mbos-noexport+TODO!=\"DONE\""))
          (mbo
            (completing-read
              "MBO: " incomplete-mbos nil t)))
    (goto-char (alist-get mbo incomplete-mbos nil nil #'string=))))
