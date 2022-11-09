;;; jf-denote.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary

;;;; Preamble
;;
;; Prior to `denote' I made extensive use of `org-roam'; I was following
;; `denote' development and appreciate Protesilaos's pedagological approach to
;; documentation.  I also appreciate the design considerations; which I wrote
;; about here: https://takeonrules.com/2022/10/09/denote-emacs-configuration/
;;
;; I installed denote and began exploring.  I am a software developer by trade,
;; and found the code accessible and discernable; that with it's sole dependency
;; being `emacs' I felt warranted further exploration.  Accessible, discernable,
;; and no dependencies are attractive attributes of software that I use as my
;; tools of work and play.  In my experience, the maintenance and enhancement is
;; easier for this kind of software.
;;
;; With further exploration, I migrated fully from `org-roam' to `denote'.
;;
;;
;;;; On Domains
;;
;; This package configures and extends `denote' by adding conceptual
;; domains to my note taking.  The domains are larger demarcations than
;; simple tags.  This is built on top of the `denote-directory' variable
;; and function.
;;
;; Further by leveraging domains, I have three means of searching:
;;
;; - "posts/" are all of my blog posts
;; - "-word" will find title's with "word" in them
;; - "_tag" will find the "tag" amongst the files keywords
;;
;; This allows me to leverage, if I want, Denote's siloing feature.

;;;; On Org Mode integration
;;
;; I make extensive use of `org-mode'; it is the format I use for crafting my
;; blog posts (see https://takeonrules.com).  It is also the tool I use for my
;; day to day task tracking and time tracking.
;;
;; I have structured my workflow so that any of these day to day activities can
;; easily produce blog posts.  I want my internal writing to have lots of
;; connective references; to help me find previous notes and perhaps look for
;; interesting connections.
;;
;; I also want posts that I publish to provide a similar experience; but the
;; links need to only be for publicly available connections.  In other words,
;; when I export a blog post, any internal links that have an external proxy are
;; rendered as links to those external proxies.  Any internal links without an
;; external proxy are rendered without links.
;;
;; This is done via `org-link-set-parameters' and denote's documentation (see
;; https://protesilaos.com/emacs/denote) provides excellent examples a
;; `org-link-set-parameters'.

;;; Code

;;;; Loading dependencies
(use-package denote
  :straight (denote :host nil
		    :type git
		    :repo "https://git.sr.ht/~protesilaos/denote")
  :commands (denote-directory
	     denote-file-prompt
	     denote--title-prompt
	     denote-get-path-by-id)
  :bind ("H-f" . 'jf/denote-find-file)
  ("H-l" . 'denote-link-or-create)
  ("H-i" . 'denote-link-or-create)
  :hook (dired-mode . denote-dired-mode)
  :custom ((denote-directory (expand-file-name "denote" org-directory))
           ;; These are the minimum viable prompts for notes
           (denote-prompts '(title keywords))
           ;; I love ‘org-mode format; reading ahead I'm setting this
           (denote-file-type 'org)
           (denote-known-keywords (jf/calculated-list-of-denote-known-keywords
                                   :from (expand-file-name "denote/glossary"
							   org-directory)))
           ;; Explicitly ensuring that tags can be multi-word (e.g. two or more
           ;; words joined with a dash).  Given that I export these tags, they
           ;; should be accessible to screen-readers.  And without the dashes
           ;; they are a garbled word salad.
           (denote-allow-multi-word-keywords t)
           ;; And `org-read-date' is an amazing bit of tech
           (denote-date-prompt-denote-date-prompt-use-org-read-date t)))

(use-package consult-notes
  ;;Let’s add another way at looking up files.  I appreciate the ability to
  ;;search all files and start with a character (e.g. =b=) followed by <space> to
  ;;filter to the note source keyed as =s= (e.g. Scientist).
  :straight (:type git :host github :repo "mclear-tools/consult-notes")
  :after (consult denote)
  :bind
  ("H-d s" . 'consult-notes-search-in-all-notes)
  ("H-d f RET" . 'consult-notes)
  ;; Ensuring that I search my denote/scientist sub-directory, which is
  ;; excluded from it's containing project's git repository.
  :custom (consult-notes-ripgrep-args
	   (concat
	    "rg --null --line-buffered --color=never --max-columns=1000 "
	    "--path-separator / --ignore-case --no-heading --line-number "
	    "--hidden --glob=!.git/ -L --sortr=accessed")
	   :commands (consult-notes
		      consult-notes-search-in-all-notes)))

;;;; Note taking configurations

;;;;; `denote' and `org-mode' integration
(cl-defun jf/denote-org-property-from-id (&key identifier property)
  ;; This function helps me retrieve Org-Mode properties from the given Denote
  ;; ID.
  "Given an IDENTIFIER and PROPERTY return it's value or nil.

    Return nil when:

    - is not a `denote' file
    - IDENTIFIER leads to a non `org-mode' file
    - PROPERTY does not exist on the file"
  (when-let ((filename (denote-get-path-by-id identifier)))
    (when (string= (file-name-extension filename) "org")
      (with-current-buffer (find-file-noselect filename)
        (cadar (org-collect-keywords (list property)))))))

(cl-defun jf/denote-org-properties-from-id (&key identifier properties)
  "Given an IDENTIFIER and PROPERTIES list return an a-list of values.

    Return nil when:

    - is not a denote file
    - IDENTIFIER leads to a non `org-mode' file
    - PROPERTY does not exist on the file.

This function is the plural version of `jf/denote-org-property-from-id'."
  ;; ;; Testing jf/denote-org-property-from-id
  ;; (message "%s" (jf/denote-org-property-from-id
  ;; 		 :identifier "20220930T215235"
  ;;		 :property "ABBR"))
  ;; ;; Testing jf/denote-org-properties-from-id
  ;; (message "%s" (jf/denote-org-properties-from-id
  ;; 		 :identifier "20220930T215235"
  ;; 		 :properties '("TITLE" "ABBR")))

  (when-let ((filename (denote-get-path-by-id identifier)))
    (when (string= (file-name-extension filename) "org")
      (with-current-buffer (find-file-noselect filename)
        (org-collect-keywords properties)))))


(cl-defun jf/calculated-list-of-denote-known-keywords (&key from)
  "Return a list of known `denote' keywords.

Our controlled vocabulary...if you will."
  (split-string-and-unquote
   (shell-command-to-string
    (concat
     "rg \"#\\+TAG:\\s([\\w-]+)\" "
     from
     " --only-matching"
     " --no-filename "
     " --replace '$1'"))
   "\n"))

;;;;; `denote' file finding functions

;; (defun jf/denote-file-prompt (fn &optional initial-dir)
;;   "An override of the provided denote-file-prompt."
;;   ;; I’m not looking at active silo-ing and want to be able to search
;;   ;; specifically from the top-level and all subdirectories.
;;   (if initial-dir
;;       (funcall fn initial-dir)
;;     (let* ((vc-dirs-ignores (mapcar
;;                              (lambda (dir)
;; 			       (concat dir "/"))
;;                              vc-directory-exclusion-list))
;;            (all-files (mapcan
;; 		       (lambda (sub-dir)
;;                          (project--files-in-directory (f-join
;; 						       (denote-directory)
;; 						       sub-dir)
;; 						      vc-dirs-ignores))
;; 		       jf/denote-subdirectories)))
;;       (funcall project-read-file-name-function
;; 	       "Find file" all-files nil 'file-name-history))))
;; (advice-add #'denote-file-prompt
;; 	    :around #'jf/denote-file-prompt
;; 	    '((name . "wrapper")))

(setq consult-notes-sources (list))
(setq jf/denote-subdirectories (list))

(defun jf/denote-find-file ()
  "Find file in the current `denote-directory'."
  (interactive)
  (require 'consult-projectile)
  (require 'denote)
  ;; For this query, override the `projectile-git-command' so that I can
  ;; include my "denote/scientist" notes.
  (let ((projectile-git-command
	 "git ls-files -zco --exclude-from=.projectile.gitignore"))
    (consult-projectile--file (denote-directory))))

;;;;; Note taking Domains
(cl-defmacro jf/denote-create-functions-for (&key domain key (create-fn nil))
  "A macro to create functions for the given DOMAIN.

          The KEY is the ASCII value of the binding key.

          Creates:

          - Wrapping function of `jf/denote-find-file' that narrows results
          to the given DOMAIN.
          - Create linking function for DOMAIN.
          - Add the domain to the `jf/denote-subdirectories'.
          - Adds DOMAIN to `consult-notes-sources'."
  (let* ((finder-fn (intern (concat "jf/denote-find-file--" domain)))
         (subdirectory (f-join "~/git/org/denote" domain))
         (finder-docstring (concat "Find file in \""
                                   domain
                                   "\" subdirectory of `denote-directory'."))
         (default-create-fn (intern (concat "jf/denote-create--"
					    domain
					    "--default")))
         (default-create-docstring (concat "Create denote in \""
                                           domain
                                           "\" subdirectory of "
					   "`denote-directory'."))
         (link-or-creator-fn (intern (concat "jf/denote-link-or-create--" domain)))
         (link-or-creator-docstring (concat "Link to denote in \""
					    domain
					    "\" subdirectory of "
					    "`denote-directory'.")))
    `(progn
       (add-to-list 'jf/denote-subdirectories ,domain)
       (when (boundp 'consult-notes-sources)
         (add-to-list 'consult-notes-sources '(,domain ,key ,subdirectory)))
       (defun ,default-create-fn ()
	 ,default-create-docstring
	 (interactive)
	 (let ((denote-directory (f-join (denote-directory) ,domain))
	       (denote-prompts '(title keywords)))
	   (call-interactively #'denote)))
       (bind-key (format "H-d c %c" ,key) (or ,create-fn ',default-create-fn))
       (bind-key (format "H-d f %c" ,key) ',finder-fn)
       (defun ,finder-fn ()
         ,finder-docstring
         (interactive)
         (let ((denote-directory (f-join (denote-directory) ,domain)))
           (call-interactively #'jf/denote-find-file)))
       (bind-key (format "H-d l %c" ,key) ',link-or-creator-fn)
       (defun ,link-or-creator-fn ()
         ,link-or-creator-docstring
         (interactive)
         (let ((denote-directory (f-join (denote-directory) ,domain)))
           (call-interactively #'denote-link-or-create)))
       )))

;;;;;; Blog Posts
;; The blog-post domain is for things that I have, will, or might publish to
;; https://takeonrules.com
(jf/denote-create-functions-for :domain "blog-posts"
                                :key ?b)

;;;;;; Scientist
(jf/denote-create-functions-for :domain "scientist"
                                :key ?s)

;;;;;; Epigraphs
(cl-defun jf/denote-create-epigraph (&key
                                     (body (read-from-minibuffer
					    "Epigraph Text: "))
                                     ;; Todo prompt for Author Name
                                     (author_name (read-from-minibuffer
						   "Author Name: "))
                                     ;; Todo prompt for Work Title
                                     (work_title (read-from-minibuffer
						  "Work Title: "))
                                     (nth-words 8))
  "Create an epigraph from the given BODY, AUTHOR_NAME, and WORK TITLE.

      Default the note’s title to the first NTH-WORDS of the BODY."
  (interactive)
  (let* ((body-as-list (s-split-words body))
         (title (s-join " " (if (> (length body-as-list) nth-words)
                                (subseq body-as-list 0 nth-words)
			      body-as-list)))
         (template (concat
                    ;; The name of the author
                    "#+AUTHOR_NAME: " author_name "\n"
                    ;; Where can you “find” this author?
                    "#+AUTHOR_URL:\n"
                    ;; The GLOSSARY_KEY for the given author
                    "#+AUTHOR_KEY:\n"
                    ;; What’s the title of the work?
                    "#+WORK_TITLE: " work_title "\n"
                    ;; Where can you “get” this work?
                    "#+WORK_URL:\n"
                    ;; The GLOSSARY_KEY for the given work
                    "#+WORK_KEY:\n"
                    ;; Indicates if this is a poem (or not)
                    "#+POEM:\n"
                    ;; The page in which this passage appears in the given
                    ;; work.
                    "#+PAGE:\n"
                    ;; The name of the translator
                    "#+TRANSLATOR_NAME:\n")))
    (denote title
            nil
            'org
            (f-join (denote-directory) "epigraphs")
            nil
            template)))

(jf/denote-create-functions-for :domain "epigraphs"
                                :key ?e
                                :create-fn 'jf/denote-create-epigraph)

;;;;;; Glossary Entries
(cl-defun jf/denote-create-glossary-entry
    (&key
     (title (read-from-minibuffer "Name the Entry: "))
     (is-a-game (yes-or-no-p "Is this a game?"))
     (abbr (read-from-minibuffer "Abbreviation (empty to skip): ")))
  "Create a `denote' entry for the given TITLE and ABBR.

    And if this IS-A-GAME then amend accordingly.

    NOTE: At present there is no consideration for uniqueness."
  (interactive)
  (let* ((key (downcase (denote-sluggify (if (s-present? abbr) abbr title))))
         (template (concat "#+GLOSSARY_KEY: " key "\n"
                           "#+ABBR:" (when (s-present? abbr)
				       (concat " " abbr))
			   "\n"
			   ;; TODO: Include a prompt of existing disclaimers
                           "#+CONTENT_DISCLAIMER:\n"
                           "#+DESCRIPTION:\n"
                           (when is-a-game "#+GAME: " key "\n")
                           "#+ITEMID:\n"
                           "#+ITEMTYPE:\n"
                           "#+MENTION_AS:\n"
                           "#+OFFER:\n"
                           "#+PLURAL_ABBR:\n"
                           "#+PLURAL_TITLE:\n"
                           "#+SAME_AS:\n"
                           "#+TAG:\n" ;; TODO: Assert uniqueness
                           "#+VERBOSE_TITLE:\n"))
         (keywords (list)))
    ;; Add both "abbr" and the abbr to the keywords; both help in searching
    ;; results
    (when (s-present? abbr)
      (progn (add-to-list 'keywords "abbr") (add-to-list 'keywords abbr)))
    (when is-a-game (add-to-list 'keywords "game"))
    (denote title
            keywords
            'org
            (f-join (denote-directory) "glossary")
            nil
            template)))

(jf/denote-create-functions-for :domain "glossary"
				:key ?g
				:create-fn 'jf/denote-create-glossary-entry)
;; Testing jf/denote-org-property-from-id
;; (message "%s" (jf/denote-org-property-from-id :id "20220930T215235"
;; 					      :property "ABBR"))

;;;;;; Melange
;; All the other things; perhaps they could become blog posts, but for now they
;; remain part of the mixture and medley.
(jf/denote-create-functions-for :domain "melange"
				:key ?m)

;;;;;; People
;; I do write notes about people I interact with.  Technically I have glossary
;; entries for people.  But those entries are for folks I don’t interact with.
(jf/denote-create-functions-for :domain "people"
				:key ?p)

;;;;;; Indices

;; On my site I write https://takeonrules.com/series/.  I track this data in a
;; YAML file; I’d like to treat this data similar to my glossary.
(cl-defun jf/denote-create-indices-entry (&key
                                          (title (read-from-minibuffer
						  "Name the index: "))
                                          (is-a-series
					   (yes-or-no-p
					    "Take on Rules series?")))
  "Create a `denote' index entry for the given TITLE."
  (interactive)
  (let* ((keywords (list))
         (template (concat (when (s-present? is-a-series)
                             "#+HIGHLIGHT: true\n"))))
    (when (s-present? is-a-series)
      (add-to-list 'keywords "series"))
    (denote title
            nil
            'org
            (f-join (denote-directory) "indices")
            nil
            template)))

(jf/denote-create-functions-for :domain "indices"
				:key ?i
				:create-fn 'jf/denote-create-indices-entry)

;;;;; `org-link-parameters'
(cl-defun jf/org-link-complete-link-for (parg &key scheme filter subdirectory)
  "Prompt for `denote' with filename FILTER in the given SUBDIRECTORY.

    Returns a string of format: \"SCHEME:<id>\" where <id> is
    an `denote' identifier."
  (let* ((denote-directory (if subdirectory
			       (f-join (denote-directory)
				       (concat subdirectory "/"))
                             (denote-directory))))
    ;; This leverages a post v1.0.0 parameter of Denote
    ;; See https://git.sr.ht/~protesilaos/denote/commit/c6c3fc95c66ba093a266c775f411c0c8615c14c7
    (concat scheme
            ":"
            (denote-retrieve-filename-identifier
	     (denote-file-prompt filter)))))

(cl-defun jf/denote-link-ol-link-with-property (link
						description
						format
						protocol
                                                &key
                                                property-name
                                                additional-hugo-parameters
                                                (use_hugo_shortcode
						 jf/exporting-org-to-tor))
  "Export a LINK with DESCRIPTION for the given PROTOCOL and FORMAT.

    FORMAT is an Org export backend. We will discard the given
    DESCRIPTION.  PROTOCOL is ignored."
  (let* ((prop-list (jf/denote-org-properties-from-id
                     :identifier link
                     :properties (list "TITLE" property-name  "GLOSSARY_KEY")))
         (title (car (alist-get "TITLE" prop-list nil nil #'string=)))
         (property (car (alist-get property-name prop-list nil nil #'string=)))
         (key (car (alist-get "GLOSSARY_KEY" prop-list nil nil #'string=))))
    (cond
     ((or (eq format 'html) (eq format 'md))
      (if use_hugo_shortcode
          (format "{{< glossary key=\"%s\" %s >}}"
                  key
                  additional-hugo-parameters)
        (format "<abbr title=\"%s\">%s</abbr>"
                title
                property)))
     (t (format "%s (%s)"
                title
                property)))))

(org-link-set-parameters
 "abbr"
 :complete (lambda (&optional parg)
	     (jf/org-link-complete-link-for
              parg
              :scheme "abbr"
              :filter " _abbr*"
              :subdirectory "glossary"))
 :export (lambda (link description format protocol)
           (jf/denote-link-ol-link-with-property
	    link description format protocol
            :property-name "ABBR"
            :additional-hugo-parameters "abbr=\"t\""))
 :face #'denote-faces-link
 :follow #'denote-link-ol-follow
 )

(org-link-set-parameters
 "abbr-plural"
 :complete (lambda (&optional parg)
	     (jf/org-link-complete-link-for
              parg
              :scheme "abbr-plural"
              :filter " _plural_abbr*"
              :subdirectory "glossary"))
 :export (lambda (link description format protocol)
           (jf/denote-link-ol-link-with-property
	    link description format protocol
            :property-name "PLURAL_ABBR"
            :additional-hugo-parameters "abbr=\"t\" plural=\"t\""))
 :face #'denote-faces-link
 :follow #'denote-link-ol-follow
    ;;;; I'm unclear if/how I want to proceed with this
 ;; :store (lambda (jf/org-link-store-link-for :scheme "abbr-plural"))
 )

(org-link-set-parameters "date"
                         :complete #'jf/denote-link-complete-date
                         :export #'jf/denote-link-export-date
                         :face #'denote-faces-link
                         :follow #'jf/denote-link-follow-date
                         )

(cl-defun jf/denote-link-complete-date (&optional parg)
  "Prompt for the given DATE.

    While we are prompting for a year, month, and day; a reminder
    that this is intended to be conformant with the TIME element.
    But for my typical use I write these as either years; years and
    months; and most often year, month, and days."
  (format "date:%s" (org-read-date)))

(cl-defun jf/denote-link-export-date (link description format protocol)
  "Export a date for the given LINK, DESCRIPTION, FORMAT, and PROTOCOL."
  (cond
   ((or (eq format 'html) (eq format 'md))
    (format "<time datetime=\"%s\">%s</time>" link description))
   (t (format "%s (%s)" description link))))

(cl-defun jf/denote-link-follow-date (date &optional parg)
  "Follow the given DATE; uncertain what that means."
  (message "TODO, implement link for %s" date))

;; I want to be able to link and export my epigraph entries.  For now, I'm
;; going to focus on the HTML and Markdown version; as most often when I
;; include an epigraph it is for my blog posts.
(cl-defun jf/denote-link-ol-epigraph-link (link
					   description format protocol
					   &key
					   property-name
					   additional-hugo-parameters
					   (use_hugo_shortcode
					    jf/exporting-org-to-tor))
  "Export the epigraph for the given LINK, DESCRIPTION, PROTOCOL, and FORMAT.

  NOTE: This only works for blog export.
  TODO: Consider how to expand beyond blog support."
  (let* ((prop-list (jf/denote-org-properties-from-id
                     :identifier link
                     :properties (list "TITLE"
				       property-name
				       "GLOSSARY_KEY"))))
    (cond
     ((and use_hugo_shortcode (or (eq format 'html) (eq format 'md)))
      (format "{{< epigraph key=\"%s\" >}}" link))
     (t nil))))

(org-link-set-parameters "epigraph"
                         :complete (lambda (&optional parg)
				     (jf/org-link-complete-link-for
                                      parg
                                      :scheme "epigraph"
                                      :filter ""
                                      :subdirectory "epigraphs"))
                         :export (lambda (link description format protocol)
                                   (jf/denote-link-ol-epigraph-link
				    link description format protocol
                                    :property-name "ABBR"
                                    :additional-hugo-parameters "abbr=\"t\""))
                         :face #'denote-faces-link
                         :follow #'denote-link-ol-follow
                         )
(cl-defun jf/denote-link-ol-export (link
				    description
				    format
				    protocol
                                    &key
				    (use_hugo_shortcode
				     jf/exporting-org-to-tor))
  "Export a `denote:' link from Org files.
    The LINK, DESCRIPTION, and FORMAT are handled by the export
    backend.

  TODO: I need to handle the case where I'm exporting MD or HTML
  and linking an item that doesn't have a URL.  This is the case
  for leveraging using my shortcode:

  {{< glossary key=\"GLOSSARY_KEY\" >}}."
  (let* ((path-id (denote-link--ol-resolve-link-to-target link :path-id))
         (title (jf/denote-org-property-from-id
		 :identifier link
		 :property "TITLE"))
         (path (file-name-nondirectory (car path-id)))
         (url (jf/denote-export-url-from-id link))
         (desc (or description title)))
    (if url
        (cond
         ((eq format 'html) (format "<a href=\"%s\">%s</a>" url desc))
         ((or (eq format 'latex) (eq format 'beamer))
	  (format "\\href{%s}{%s}"
		  (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" path)
		  desc))
         ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
         ((eq format 'ascii) (format "[%s] <denote:%s>" desc path))
         ((eq format 'md) (format "[%s](%s)" desc url))
         (t path))
      desc)))

(advice-add #'denote-link-ol-export
	    :override #'jf/denote-link-ol-export
	    '((name . "wrapper")))

;; When I link to glossary entries, I want to use their URLs.  I have several
;; different fields that could have the “export url”:
;;
;; - OFFER :: The URL which you can “get” the item (e.g. purchase the game,
;;   find where to check it out at a library)
;;
;; - ROAM_REFS :: In past incarnations, I would add a ROAM_REFS to the Org-Roam
;;   node that was my “local” blog post.
;;
;; - SAME_AS :: This could be the primary URL; however due to past
;;   implementations, I was extracting the SAME_AS URL from the ITEMID; which
;;   was typically the Wikidata URL.
(defun jf/denote-export-url-from-id (identifier)
  "Return the appropriate url for the given `denote' identifier."
  (when-let ((filename (denote-get-path-by-id identifier)))
    (when (string= (file-name-extension filename) "org")
      (with-current-buffer (find-file-noselect filename)
        (let* ((props-plist
		(jf/org-global-props-as-plist
		 :props-regexp "\\(OFFER\\|ROAM_REFS\\|SAME_AS\\)")))
          (cond
           ;; Favor affiliate links
           ((lax-plist-get props-plist "OFFER"))
           ((when-let ((refs (lax-plist-get props-plist "ROAM_REFS")))
	      (first (s-split " " refs t))))
           ((lax-plist-get props-plist "SAME_AS"))))))))

;;  ;; Should be: https://www.worldcat.org link
;; (message "%s" (jf/denote-export-url-from-id "20221009T115949"))
;;  ;; Should be https://samvera.org
;; (message "%s" (jf/denote-export-url-from-id "20221009T120341"))
;;  ;; Should be https://en.wikipedia.org/wiki/Jira_(software)
;; (message "%s" (jf/denote-export-url-from-id "20221009T120152"))
;;  ;; Should be nil
;;(message "%s" (jf/denote-export-url-from-id "20221009T120712"))

(defun jf/associate-blog-post-url-with-identifier (url identifier)
  "Associate given URL with the `denote' IDENTIFIER."
  (message "Associating URL: %s with IDENTIFIER: %s." identifier url)
  (let* ((filename (denote-get-path-by-id identifier))
         (buffer (find-file-noselect filename)))
    (with-current-buffer buffer
      (jf/export-org-to-tor--global-buffer-prop-ensure
       :key "ROAM_REFS"
       :plist (jf/org-global-props-as-plist :props-regexp "ROAM_REFS")
       :default url)
      (save-buffer))))

;;;;; Capturing functions for applications
(defun jf/menu--org-capture-firefox ()
  "Create an `denote' entry from Firefox page."
  (interactive)
  (require 'grab-mac-link)
  (let* ((link-title-pair (grab-mac-link-firefox-1))
         (url (car link-title-pair))
         (title (cadr link-title-pair)))
    (jf/denote-capture-reference :url url :title title)))

(defun jf/menu--org-capture-safari ()
  "Create an `denote' entry from Safari page."
  (interactive)
  (require 'grab-mac-link)
  (let* ((link-title-pair (grab-mac-link-safari-1))
         (url (car link-title-pair))
         (title (cadr link-title-pair)))
    (jf/denote-capture-reference :url url :title title)))

(defun jf/menu--org-capture-eww ()
  "Create an `denote' entry from `eww' data."
  (interactive)
  (let* ((url (plist-get eww-data :url))
         (title (plist-get eww-data :title)))
    (jf/denote-capture-reference :url url :title title)))

(cl-defun jf/menu--org-capture-elfeed-show (&key (entry elfeed-show-entry))
  "Create a `denote' from `elfeed' ENTRY."
  (interactive)
  (let* ((url (elfeed-entry-link entry))
         (title (elfeed-entry-title entry)))
    (jf/denote-capture-reference :url url :title title)))

(cl-defun jf/denote-capture-reference (&key
				       title
				       url
				       (keywords (denote-keywords-prompt))
				       (domain "melange"))
  "Create a `denote' entry for the TITLE and URL.

TODO: Would it make sense to prompt for the domain?
"
  (denote title
          keywords
          'org
          (f-join (denote-directory) domain)
          nil
          (concat "#+ROAM_REFS: " url "\n")))

(provide 'jf-denote)
;;; jf-denote.el ends here
