;;; Commentary
;;
;; This is my third iteration on an `org-roam'.  It's goal is to address
;; use-cases that I've encountered while moving more of my note-taking with
;; `org-roam'.
;;
;; One use-case is when I'm running or playing in an RPG session.  During those
;; sessions, when I create/find/insert nodes, I almost certainly want to use
;; the same tag filter.  This is something I observed while running my 13
;; session "Thel Sector" campaign.
;;
;; A second use-case is when I'm writing notes or thoughts related to work.  In
;; a past life, I might have written notes for either my employer or Samvera (a
;; community in which I participated).  Those notes might overlap but rarely
;; did.
;;
;; Another use case is less refined, namely I'm writing but am not "in" a
;; specific context.
;;
;; However, v2 of my org-roam structure, didn't quite get out of the way.  I
;; never quite got to the speed of note taking that I had for the original Thel
;; Sector campaign.
;;
;; What follows builds on Jethro Kuan's "How I Take Notes with Org-Roam" (see
;; https://jethrokuan.github.io/org-roam-guide/).  Reading Jethro Kuan's post
;; helped me see how I could do this.
;;
;; The `jnf/org-context-plist' defines and names some of the contexts in which
;; I might be writing.  Each named context defines the tags.  These are the
;; tags that all nodes will have when they were written in the defined context.
;;
;; I can use `jnf/org-auto-tags--set' to create a "yet to be named" context
;; (e.g., an ad hoc context).  Or I can use `jnf/org-auto-tags--set-by-context'
;; to establish the current context (or clear it).
;;
;;; BEGIN Supporting Declarations
(defvar jnf/org-roam-capture-templates-plist
  (list
   ;; These are references to "other people's thoughts."
   :refs '("r" "refs" plain "%?"
           :if-new (file+head "refs/%<%Y%m%d>---${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
   ;; These are "my thoughts" with references to "other people's thoughts."
   :main '("m" "main" plain "%?"
           :if-new (file+head "main/%<%Y%m%d>---${slug}.org"
                              "#+title: ${title}\n#+FILETAGS: ${auto-tags}%^G\n")
           :immediate-finish t
           :unnarrowed t)
   ;; These are publications of "my thoughts" referencing "other people's thoughts".
   :pubs '("p" "pubs" plain "%?"
           :if-new (file+head "pubs/%<%Y%m%d>---${title}.org" "#+title: ${title}\n")q
           :immediate-finish t
           :unnarrowed t))
  "Templates to use for `org-roam' capture.")

(defvar jnf/org-context-plist
  (list
   :none (list
	  :name "none"
	  :tags (list))
   :burning-locusts (list
		     :name "burning-locusts"
		     :tags '("burning-locusts" "rpg" "burning-wheel"))
   :forem (list
	   :name "forem"
	   :tags '("forem"))
   :mistimed-scroll (list
		     :name "mistimed-scroll"
		     :tags '("eberron" "mistimed-scroll" "rpg" "burning-wheel"))
   :thel-sector (list
		 :name "thel-sector"
		 :tags '("thel-sector" "rpg" "swn")))
  "A list of contexts that I regularly write about.")

(defvar jnf/org-auto-tags--current-list
  (list)
  "The list of tags to automatically apply to an `org-roam' capture.")

(defun jnf/org-auto-tags--set (tags)
  "Prompt user or more TAGS."
  (interactive
   (list (completing-read-multiple "Tag(s): " (org-roam-tag-completions))))
  (setq jnf/org-auto-tags--current-list tags))

(cl-defun jnf/org-auto-tags--set-by-context (context &key (context-plist jnf/org-context-plist))
  "Set auto-tags by CONTEXT.

Prompt for CONTEXT from CONTEXT-PLIST."
  (interactive (list
                (completing-read
                 "Context: " (jnf/org-context-list-completing-read))))
  (setq jnf/org-auto-tags--current-list
	(plist-get (plist-get context-plist (intern (concat ":" context))) :tags)))

(cl-defun org-roam-node-auto-tags (node &key (tag-list jnf/org-auto-tags--current-list))
  "Inject the TAG-LIST into the {auto-tags} region of captured NODE.

See https://www.orgroam.com/manual.html#Template-Walkthrough"
  (if (and tag-list (> (length tag-list) 0))
      (concat ":" (s-join ":" tag-list) ":")
    ""))

(cl-defun jnf/org-roam-templates-list (template &key (template-plist jnf/org-roam-capture-templates-plist))
  "List of `org-roam' capture templates based on given TEMPLATE.

Searches the TEMPLATE-PLIST for the templates.

Note, the :all template assumes we use the whole list."
  (if (eq template :all)
      (-non-nil (seq-map-indexed (lambda (tmp index)
				   (when (oddp index)
                                     tmp))
				 template-plist))
    (list (plist-get template-plist template))))

(cl-defun jnf/org-roam-templates-context-fn (&key (tag-list jnf/org-auto-tags--current-list))
  "Returns a set of templates based on TAG-LIST.

A key assumption is that if there's a default tag list, use the
:main template."
  (if (and tag-list (> (length tag-list) 0))
      (jnf/org-roam-templates-list :main)
    (jnf/org-roam-templates-list :all)))

(cl-defun jnf/org-context-list-completing-read (&key
                                                (context-plist
						 jnf/org-context-plist))
  "Create a list from the CONTEXT-PLIST for completing read.

The form should be '((\"forem\" 1) (\"burning-loscusts\" 2))."
  ;; Skipping the even entries as those are the "keys" for the plist,
  ;; the odds are the values.
  (-non-nil (seq-map-indexed (lambda (context index)
                               (when (oddp index)
                                 (list (plist-get context :name) index)))
                             context-plist)))

(cl-defun jnf/org-roam-filter-context-fn (node &key (tag-list jnf/org-auto-tags--current-list))
  "Determine TAG-LIST is subset of NODE's tags."
  (cl-subsetp tag-list (org-roam-node-tags node)))

;;; BEGIN org-roam declaration
(use-package org-roam
  :straight t
  :config
  ;; I encountered the following message when attempting to export data:
  ;;
  ;; => "org-export-data: Unable to resolve link: EXISTING-PROPERTY-ID"
  ;;
  ;; See https://takeonrules.com/2022/01/11/resolving-an-unable-to-resolve-link-error-for-org-mode-in-emacs/ for details
  (defun jnf/force-org-rebuild-cache ()
    "Call some functions to rebuild the `org-mode' and `org-roam' cache."
    (interactive)
    (org-id-update-id-locations)
    ;; Note: you may need `org-roam-db-clear-all' followed by `org-roam-db-sync'
    (org-roam-db-sync)
    (org-roam-update-org-id-locations))
  (defun jnf/org-roam-capture (&optional goto keys)
    "Call `org-roam-capture' based on set tags."
    (interactive "P")
    (org-roam-capture goto
                      keys
                      :filter-fn 'jnf/org-roam-filter-context-fn
                      :templates 'jnf/org-roam-templates-context-fn))

  (defun jnf/org-roam-node-insert ()
    "Call `org-roam-node-insert' based on set tags."
    (interactive)
    (org-roam-node-insert 'jnf/org-roam-filter-context-fn
			  :templates 'jnf/org-roam-templates-context-fn))

  (defun jnf/org-roam-find-node (&optional other-window initial-input)
    "Call `org-roam-node-find' based on set tags."
    (interactive current-prefix-arg)
    (org-roam-node-find other-window
			initial-input
			'jnf/org-roam-filter-context-fn
			:templates 'jnf/org-roam-templates-context-fn))
  :custom
  (org-roam-directory (file-truename "~/git/org"))
  ;; Set more spaces for tags; As much as I prefer the old format,
  ;; this is the new path forward.
  (org-roam-node-display-template
   (concat "${type:15} ${title:*} " (propertize "${tags:40}" 'face 'org-tag)))
  (org-roam-capture-templates (jnf/org-roam-templates-list :all))
  :bind (("C-s-f" . jnf/org-roam-find-node)
	 ("C-s-c" . jnf/org-roam-capture))
  :bind (:map org-mode-map (
			    ("C-s-;" . org-roam-buffer-toggle)
			    ("s-i" . jnf/org-roam-node-insert)))
  :init
  ;; Help keep the `org-roam-buffer', toggled via `org-roam-buffer-toggle', sticky.
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\#"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  ;; When t the autocomplete in org documents would query the org roam database
  (setq org-roam-completion-everywhere nil)
  (setq org-roam-v2-ack t)
  (org-roam-db-autosync-mode))

(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))