(require 'elfeed)
(let ((left
        (expand-file-name "index.darter" elfeed-db-directory)))
  (cl-letf (((default-value 'major-mode) 'fundamental-mode))
    (with-current-buffer (find-file-noselect left :nowarn)
      (goto-char (point-min))
      (let ((db-1 (read (current-buffer)))
             (db-2 (ignore-errors (read (current-buffer)))))
        (setf left-db (or db-2 db-1)))
      (kill-buffer)))
  (ht-map (lambda (k v)
            (cl-pushnew k elfeed-feeds :test #'equal))
    (plist-get left-db :feeds))
  (ht-map (lambda (k v)
            (elfeed-db-add (list v)))
    (plist-get left-db :entries)))
(elfeed-db-save)

;;           )
;; (let (
;;        (left-index
;;          (plist-get left-db :index))
;;        (left-feeds
;;          (plist-get left-db :feeds))
;;        (right-entries
;;          (plist-get right-db :entries))
;;        (right-index
;;          (plist-get right-db :index))
;;        (right-feeds
;;          (plist-get right-db :feeds)))
;;   (message "LEFT %s\tRIGHT %s\tMERGE %s"
;;     (ht-size left-entries)
;;     (ht-size right-entries)
;;     (ht-size (ht-merge left-entries right-entries)))))
