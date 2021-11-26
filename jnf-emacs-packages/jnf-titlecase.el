;;; jnf-titlecase.el --- Summary
;;
;;; Commentary:
;;
;;  Package to provide titlecase functionality.
;;
;;  Copied from https://hungyi.net/posts/programmers-way-to-title-case/
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;******************************************************************************
;;
;;; BEGIN Titlecase definitions
;;
;;******************************************************************************
(defun titlecase-string (str)
  "Convert string STR to title case and return the resulting string."
  (let* ((case-fold-search nil)
         (str-length (length str))
         ;; A list of markers that indicate start of a new phrase within the title, e.g. "The Lonely Reindeer: A Christmas Story"
         (new-phrase-chars '(?: ?. ?? ?\; ?\n ?\r)) ; must be followed by one of  word-boundary-chars
         (immediate-new-phrase-chars '(?\n ?\r))    ; immediately triggers new phrase behavior without waiting for word boundary
         ;; A list of characters that indicate "word boundaries"; used to split the title into processable segments
         (word-boundary-chars (append '(?  ?– ?— ?- ?‑ ?/) immediate-new-phrase-chars))
         ;; A list of small words that should not be capitalized (in the right conditions)
         (small-words (split-string "a an and as at but by en for if in is it nor of on or the to up v v. vs vs. via was yet" " "))
         ;; Fix if str is ALL CAPS
         (str (if (string-match-p "[a-z]" str) str (downcase str)))
         ;; Reduce over a state machine to do title casing
         (final-state (cl-reduce
                       (lambda (state char)
                         (let* ((result               (aref state 0))
                                (last-segment         (aref state 1))
                                (first-word-p         (aref state 2))
                                (was-in-path-p        (aref state 3))
                                (last-char            (car last-segment))
                                (in-path-p            (or (and (eq char ?/)
                                                               (or (not last-segment) (member last-char '(?. ?~))))
                                                          (and was-in-path-p
                                                               (not (or (eq char ? )
                                                                        (member char immediate-new-phrase-chars))))))
                                (end-p                (eq (+ (length result) (length last-segment) 1)
                                                          str-length))                                          ; are we at the end of the input string?
                                (pop-p                (or end-p (and (not in-path-p)
                                                                     (member char word-boundary-chars))))       ; do we need to pop a segment onto the output result?
                                (segment              (cons char last-segment))                                 ; add the current char to the current segment
                                (segment-string       (apply #'string (reverse segment)))                       ; the readable version of the segment
                                (small-word-p         (member (downcase (substring segment-string 0 -1))
                                                              small-words))                                     ; was the last segment a small word?
                                (capitalize-p         (or end-p first-word-p (not small-word-p)))               ; do we need to capitalized this segment or lowercase it?
                                (ignore-segment-p     (or (string-match-p "[a-zA-Z].*[A-Z]" segment-string)     ; ignore explicitly capitalized segments
                                                          (string-match-p "^https?:" segment-string)            ; ignore URLs
                                                          (string-match-p "\\w\\.\\w" segment-string)           ; ignore hostnames and namespaces.like.this
                                                          (string-match-p "^[A-Za-z]:\\\\" segment-string)      ; ignore windows filesystem paths
                                                          was-in-path-p                                         ; ignore unix filesystem paths
                                                          (member ?@ segment)))                                 ; ignore email addresses and user handles with @ symbol
                                (next-result          (if pop-p
                                                          (concat
                                                           result
                                                           (if ignore-segment-p
                                                               segment-string                                   ; pop segment onto the result without processing
                                                             (titlecase--segment segment-string capitalize-p))) ; titlecase the segment before popping onto result
                                                        result))
                                (next-segment         (unless pop-p segment))
                                (will-be-first-word-p (if pop-p
                                                          (or (not last-segment)
                                                              (member last-char new-phrase-chars)
                                                              (member char immediate-new-phrase-chars))
                                                        first-word-p)))
                           (vector next-result next-segment will-be-first-word-p in-path-p)))
                       str
                       :initial-value
                       (vector nil      ; result stack
                               nil      ; current working segment
                               t        ; is it the first word of a phrase?
                               nil))))  ; are we inside of a filesystem path?
    (aref final-state 0)))

(defun titlecase--segment (segment capitalize-p)
  "Convert a title's inner SEGMENT to capitalized or lower case depending on CAPITALIZE-P, then return the result."
  (let* ((case-fold-search nil)
         (ignore-chars '(?' ?\" ?\( ?\[ ?‘ ?“ ?’ ?” ?_))
         (final-state (cl-reduce
                       (lambda (state char)
                         (let ((result (aref state 0))
                               (downcase-p (aref state 1)))
                           (cond
                            (downcase-p                 (vector (cons (downcase char) result) t))  ; already upcased start of segment, so lowercase the rest
                            ((member char ignore-chars) (vector (cons char result) downcase-p))    ; check if start char of segment needs to be ignored
                            (t                          (vector (cons (upcase char) result) t))))) ; haven't upcased yet, and we can, so do it
                       segment
                       :initial-value (vector nil (not capitalize-p)))))
    (thread-last (aref final-state 0)
      (reverse)
      (apply #'string))))

(defun titlecase-region (begin end)
  "Convert text in region from BEGIN to END to title case."
  (interactive "*r")
  (let ((pt (point)))
    (insert (titlecase-string (delete-and-extract-region begin end)))
    (goto-char pt)))

(defun titlecase-dwim ()
  "Convert the region or current line to title case.
If Transient Mark Mode is on and there is an active region, convert
the region to title case.  Otherwise, work on the current line."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (titlecase-region (region-beginning) (region-end))
    (titlecase-region (point-at-bol) (point-at-eol))))
;;******************************************************************************
;;
;;; END Titlecase definitions
;;
;;******************************************************************************


(provide 'jnf-titlecase.el)
;;; jnf-titlecase.el ends here
