;;******************************************************************************
;;
    ;;; BEGIN Wrapping Functions
;;
;;******************************************************************************
(cl-defun jf/tor-wrap-with-text (&key before after strategy)
  "Wrap the STRATEGY determined region with the BEFORE and AFTER text.

    Valid STRATEGY options are:

    - :lineOrRegion
    - :pointOrRegion
    - :sentenceOrRegion
    - :wordOrRegion

    TODO: I would love create a lookup table for the case statement,
    as the behavior's well defined."
  (pcase strategy
    (:lineOrRegion (pcase-let* ((origin (point))
				(`(,begin . ,end)
				 (crux-get-positions-of-line-or-region)))
		     (goto-char end)
		     (insert after)
		     (goto-char begin)
		     (insert before)))
    (:sentenceOrRegion (let* ((begin (if (use-region-p)
					 (region-beginning)
				       (car (bounds-of-thing-at-point 'sentence))))
			      (end (if (use-region-p)
				       (region-end)
				     (cdr (bounds-of-thing-at-point 'sentence)))))
			 (goto-char end)
			 (insert after)
			 (goto-char begin)
			 (insert before)))
    (:pointOrRegion (let* ((begin (if (use-region-p) (region-beginning) (point)))
			   (end (if (use-region-p) (region-end) (point))))
		      (goto-char end)
		      (insert after)
		      (goto-char begin)
		      (insert before)))
    (:wordOrRegion (let* ((begin (if (use-region-p)
				     (region-beginning)
				   (car (bounds-of-thing-at-point 'word))))
			  (end (if (use-region-p)
				   (region-end)
				 (cdr (bounds-of-thing-at-point 'word)))))
		     (goto-char end)
		     (insert after)
		     (goto-char begin)
		     (insert before)))
    ))

(defun jf/tor-wrap-in-html-tag (tag &optional attributes)
  "Wrap the word or region with the given TAG with optional ATTRIBUTES."
  (interactive "sHTML Tag: \nsAttributes (optional): ")
  (jf/tor-wrap-with-text
   :before (concat "<" tag (if (s-blank? attributes)
			       ""
			     (concat " " attributes)) ">")
   :after (concat "</" tag ">")
   :strategy :wordOrRegion))

(defun jf/tor-wrap-in-poem ()
  "Wrap the point or region as a poem."
  (interactive)
  (jf/tor-wrap-with-text
   :before "<pre class=\"poem\">\n"
   :after "\n</pre>"
   :strategy :pointOrRegion))

(defun jf/tor-wrap-date (date)
  "Wrap the point or region with the given DATE."
  (interactive (list (org-read-date nil nil nil "Date")))
  (jf/tor-wrap-in-html-tag
   "time"
   (concat "datetime=\"" date "\" title=\"" date "\"")))

(defun jf/tor-wrap-as-marginnote-dwim ()
  "Wrap the line or current region as a marginnote Hugo shortcode."
  (interactive)
  (jf/tor-wrap-with-text
   :before "{{< marginnote >}}\n"
   :after "\n{{< /marginnote >}}"
   :strategy :lineOrRegion))

(defun jf/tor-wrap-as-sidenote-dwim ()
  "Wrap the line or current region as a sidenote Hugo shortcode."
  (interactive)
  (jf/tor-wrap-with-text
   :before "{{< sidenote >}}"
   :after "{{< /sidenote >}}"
   :strategy :sentenceOrRegion))

(defun jf/tor-wrap-link-active-region-dwim (url)
  "Wrap current region (or point) in an A-tag with the given URL.

    For the URL:

    - If `car' of `kill-ring' starts with \"http\", then use that as the URL.
    - Otherwise prompt for a URL.

    If the URL is an empty string, then send a message.  Else, if we
    have a non-0 length URL, use the URL and wrap the region in an A
    tag."
  (interactive (list (jf/prompt-for-url-dwim)))
  (if (eq (length url) 0)
      (message "No URL to use for A-tag creation")
    (jf/tor-wrap-with-text
     :before (concat "<a href=\"" url "\">")
     :after "</a>"
     :strategy :pointOrRegion)))

(defun jf/tor-wrap-as-pseudo-dfn ()
  "Wrap current region (or word) in an I-tag with a DFN dom class."
  (interactive)
  (jf/tor-wrap-with-text
   :before "<i class=\"dfn\">"
   :after "</i>"
   :strategy :wordOrRegion))

(defun jf/tor-wrap-cite-active-region-dwim (url)
  "Wrap current region (or point) in a CITE-tag and optional A-tag with URL.

    For the URL:

    - If `car' of `kill-ring' starts with \"http\", then use that as the URL.
    - Otherwise prompt for a URL.

    If the URL an empty string, then wrap the current region or point
    in a CITE tag.  Else, if we have a non-0 length URL, wrap it in
    CITE and A tag."
  (interactive (list (jf/prompt-for-url-dwim)))

  ;; Were we to start writing at the START position, we'd invariably
  ;; change the contents such that the END position was no longer
  ;; accurate.  So instead, we append at the END position, hop back to
  ;; the START position and append to the START position.
  (if (eq (length url) 0)
      (jf/tor-wrap-with-text
       :before "<cite>"
       :after "</cite >"
       :strategy :pointOrRegion)
    (jf/tor-wrap-with-text
     :before (concat "<cite><a href=\"" url
		     "\" class=\"u-url p-name\" rel=\"cite\">")
     :after "</a></cite>"
     :strategy :pointOrRegion)))
;;******************************************************************************
;;
    ;;; END Wrapping Functions
;;
;;******************************************************************************
