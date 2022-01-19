;; -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  This package provides configuration for org-roam
;;
;; See
;; http://takeonrules.com/2021/08/22/ever-further-refinements-of-org-roam-usage/
;; for details on this configuration.
;;
;; See https://takeonrules.com/2021/08/23/diving-into-the-implementation-of-subject-menus-for-org-roam/
;; for a walk through of the implementation.
;;
;;; Code
;;******************************************************************************
;;
;;; BEGIN Template and Subject Definitions
;;
;;******************************************************************************
(defvar jnf/org-roam-capture-templates-plist
  (list
   :eberron
   '("e" "Eberron" plain "%?"
     :target
     (file+head
      "personal/%<%Y%m%d>---${slug}.org"
      "#+title: ${title}\n#+FILETAGS: :personal:rpg:eberron:\n\n")
     :unnarrowed t)
   :hesburgh-libraries
   '("h" "Hesburgh Libraries" plain "%?"
     :target
     (file+head
      "hesburgh-libraries/%<%Y%m%d>---${slug}.org"
      "#+title: ${title}\n#+FILETAGS: :hesburgh: %^G\n\n")
     :unnarrowed t)
   :forem
   '("f" "Forem" plain "%?"
     :target
     (file+head
      "forem/%<%Y%m%d>---${slug}.org"
      "#+title: ${title}\n#+FILETAGS: :forem: %^G\n\n")
     :unnarrowed t)
   :jf-consulting
   '("j" "JF Consulting" plain "%?"
     :target
     (file+head
      "jeremy-friesen-consulting/%<%Y%m%d>---${slug}.org"
      "#+title: ${title}\n#+FILETAGS: :personal:jeremy-friesen-consulting: %^G\n\n")
     :unnarrowed t)
   :personal
   '("p" "Personal" plain "%?"
     :target
     (file+head
      "personal/%<%Y%m%d>---${slug}.org"
      "#+title: ${title}\n#+FILETAGS: :personal: %^G\n\n")
     :unnarrowed t)
   :public
   '("u" "Public" plain "%?"
     :target
     (file+head
      "public/%<%Y%m%d>---${slug}.org"
      "#+title: ${title}\n#+FILETAGS: :public: %^G\n\n")
     :unnarrowed t)
   :thel-sector
   '("t" "Thel Sector" plain "%?"
     :target
     (file+head
      "personal/thel-sector/%<%Y%m%d>---${slug}.org"
      "#+title: ${title}\n#+FILETAGS: :thel-sector: %^G\n\n")
     :unnarrowed t)
   )
  "A plist defining my `org-roam' capture templates.")

(defvar jnf/org-roam-capture-subjects-plist
  (list
   :eberron (list
              :templates (list :eberron)
              :name "eberron"
              :title "Eberron"
              :path-to-todo "~/git/org/personal/todo.org")
   :forem (list
                   :templates (list :forem)
                   :name "forem"
                   :title "Forem"
                   :path-to-todo "~/git/org/forem/todo.org")
   :jf-consulting (list
                   :templates (list :jf-consulting)
                   :name "jf-consulting"
                   :title "JF Consulting"
                   :path-to-todo "~/git/org/jeremy-friesen-consulting/todo.org")
   :hesburgh-libraries (list
                        :templates (list :hesburgh-libraries)
                        :name "hesburgh-libraries"
                        :title "Hesburgh Libraries"
                        :path-to-todo "~/git/org/hesburgh-libraries/todo.org")
   :personal (list
              :templates (list :personal)
              :name "personal"
              :title "Personal"
              :path-to-todo "~/git/org/personal/todo.org")
   :public (list
            :templates (list :public)
            :name "public"
            :title "Public"
            :path-to-todo "~/git/org/public/todo.org")
   :thel-sector (list
                 :templates (list :thel-sector)
                 :name "thel-sector"
                 :title "Thel Sector"
                 :path-to-todo "~/git/org/personal/thel-sector/todo.org")
   )
"A plist that contains the various `org-roam' subjects.

Each subject has a plist of :templates, :title, :name, and :path-to-todo.

- :templates defines the named templates available for this
  subject.  See `jnf/org-roam-capture-templates-plist' for list
  of valid templates.
- :name is the string version of the subject, suitable for
  creating function names.
- :title is the human readable \"title-case\" form of the
  subject.
- :path-to-todo is the path to the todo file for this subject."
)

;; Add the special ":all" case to  `jnf/org-roam-capture-subjects-plist'
(plist-put jnf/org-roam-capture-subjects-plist
           :all
           (list
            ;; Iterate only through the templates that have a
            ;; corresponding todo file.
            :templates (-uniq
                        (-flatten
                         (-non-nil
                          (seq-map-indexed
                           (lambda (subject index)
                             (when (oddp index)
                               (if (f-exists? (plist-get subject :path-to-todo))
                                   (plist-get subject :templates)
                                 nil)))
                           jnf/org-roam-capture-subjects-plist))))
            :name "all"
            :title "All"
            :path-to-todo "~/git/org/todo.org"))


(cl-defun jnf/org-roam-subject-exists-on-machine? (subject
                                                   &key
                                                   (subjects-plist jnf/org-roam-capture-subjects-plist))
  "Return tif the todo file exists for given SUBJECT in the SUBJECTS-PLIST."
  (let ((path-to-todo (plist-get (plist-get subjects-plist subject) :path-to-todo)))
    (f-exists? path-to-todo)))

(cl-defun jnf/org-roam-templates-for-subject (subject
                                              &key
                                              (subjects-plist jnf/org-roam-capture-subjects-plist)
                                              (template-definitions-plist jnf/org-roam-capture-templates-plist))
  "Return a list of `org-roam' templates for the given SUBJECT.

Use the given (or default) SUBJECTS-PLIST to fetch from the
given (or default) TEMPLATE-DEFINITIONS-PLIST."
  (let ((templates (plist-get (plist-get subjects-plist subject) :templates)))
    (-map (lambda (template) (plist-get template-definitions-plist template))
          templates)))
;;******************************************************************************
;;
;;; END Template and Subject Definitions
;;
;;******************************************************************************

;;******************************************************************************
;;
;;; BEGIN Register Subjects
;;
;;******************************************************************************
(defvar jnf/org-subject-menu--title
  (with-faicon "book" "Org Subject Menu" 1 -0.05)
  "The menu title for the `org-roam' subject.  See `jnf/toggle-roam-subject-filter'.")

(pretty-hydra-define jnf/org-subject-menu--all (:foreign-keys warn :title jnf/org-subject-menu--title :quit-key "q" :exit t)
  ("All" ()))

(cl-defmacro create-org-roam-subject-fns-for (subject
                                              &key
                                              menu_group
                                              menu_prefix
                                              (subjects-plist jnf/org-roam-capture-subjects-plist))
  "Define the org roam SUBJECT functions and create & update hydra menus.

The MENU_GROUP defines the menu column where we place the functions.
The MENU_PREFIX is the leading key for the menu option.

The functions are wrappers for `org-roam-capture',
`org-roam-node-find', `org-roam-node-insert', and `find-file'.

Create a subject specific `pretty-define-hydra' and append to the
`jnf/org-subject-menu--all' hydra via the `pretty-define-hydra+'
macro.

Fetch the given SUBJECT from the given SUBJECTS-PLIST."
  (let* ((subject-plist (plist-get subjects-plist subject))
         (subject-as-symbol subject)
         (subject-title (plist-get subject-plist :title))
         (subject-name (plist-get subject-plist :name))

         ;; For todo related antics
         (todo-fn-name (intern (concat "jnf/find-file--" subject-name "--todo")))
         (path-to-todo (plist-get subject-plist :path-to-todo))
         (todo-docstring (concat "Find the todo file for " subject-name " subject."))

         ;; For hydra menu related antics
         ;;
         ;; Note: I'm creating a bogus menu when I have the ":all" subject.
         (hydra-fn-name (intern (concat "jnf/org-subject-menu--" (if (eq subject :all) "all--dev--null" subject-name))))
         (hydra-menu-title (concat subject-title " Subject Menu"))
         (hydra-todo-title (concat subject-title " Todo…"))
         (hydra-kbd-prefix-todo    (s-trim (concat menu_prefix " @")))
         (hydra-kbd-prefix-capture (s-trim (concat menu_prefix " +")))
         (hydra-kbd-prefix-insert  (s-trim (concat menu_prefix " !")))
         (hydra-kbd-prefix-find    (s-trim (concat menu_prefix " ?")))

         ;; For `org-roam-capture' related antics
         (capture-fn-name (intern (concat "jnf/org-roam--" subject-name "--capture")))
         (capture-docstring (concat "As `org-roam-capture' but scoped to " subject-name
                            ".\n\nArguments GOTO and KEYS see `org-capture'."))

         ;; For `org-roam-insert-node' related antics
         (insert-fn-name (intern (concat "jnf/org-roam--" subject-name "--node-insert")))
         (insert-docstring (concat "As `org-roam-insert-node' but scoped to " subject-name " subject."))

         ;; For `org-roam-find-node' related antics
         (find-fn-name (intern (concat "jnf/org-roam--" subject-name "--node-find")))
         (find-docstring (concat "As `org-roam-find-node' but scoped to "
                            subject-name " subject."
                            "\n\nArguments INITIAL-INPUT and OTHER-WINDOW are from `org-roam-find-mode'."))
         )
    (when (jnf/org-roam-subject-exists-on-machine? subject)
    `(progn
       (defun ,todo-fn-name ()
         ,todo-docstring
         (interactive)
         (find-file (file-truename ,path-to-todo))
         (end-of-buffer))

       (defun ,capture-fn-name (&optional goto keys)
         ,capture-docstring
         (interactive "P")
         (org-roam-capture goto
                           keys
                           :filter-fn (lambda (node) (-contains-p (org-roam-node-tags node) ,subject-name))
                           :templates (jnf/org-roam-templates-for-subject ,subject-as-symbol)))
       (defun ,insert-fn-name ()
         ,insert-docstring
         (interactive)
         (org-roam-node-insert (lambda (node) (-contains-p (org-roam-node-tags node) ,subject-name))
                               :templates (jnf/org-roam-templates-for-subject ,subject-as-symbol)))

       (defun ,find-fn-name (&optional other-window initial-input)
         ,find-docstring
         (interactive current-prefix-arg)
         (org-roam-node-find other-window
                             initial-input
                             (lambda (node) (-contains-p (org-roam-node-tags node) ,subject-name))
                             :templates (jnf/org-roam-templates-for-subject ,subject-as-symbol)))


       ;; Create a hydra menu for the given subject
       (pretty-hydra-define ,hydra-fn-name (:foreign-keys warn :title jnf/org-subject-menu--title :quit-key "q" :exit t)
         (
          ,hydra-menu-title
          (
           ("@" ,todo-fn-name        ,hydra-todo-title)
           ("+" ,capture-fn-name     " ├─ Capture…")
           ("!" ,insert-fn-name      " ├─ Insert…")
           ("?" ,find-fn-name        " └─ Find…")
           (";" org-roam-buffer-toggle            "Toggle Buffer")
           ("#" jnf/toggle-roam-subject-filter    "Toggle Filter…")
           )))

       ;; Append the following menu items to the `jnf/org-subject-menu--all'
       (pretty-hydra-define+ jnf/org-subject-menu--all()
         (,menu_group
          (
           (,hydra-kbd-prefix-todo    ,todo-fn-name    ,hydra-todo-title)
           (,hydra-kbd-prefix-capture ,capture-fn-name " ├─ Capture…")
           (,hydra-kbd-prefix-insert  ,insert-fn-name  " ├─ Insert…")
           (,hydra-kbd-prefix-find    ,find-fn-name    " └─ Find…")
           )))
       ))))

;; I tried using a dolist to call each of the macros, but that didn't
;; work.  I'd love some additional help refactoring this.  But for
;; now, what I have is quite adequate.  It would be nice to
;; more programatically generate the hydra menus (see below).
(create-org-roam-subject-fns-for :all
                                 :menu_group "All"
                                 :menu_prefix "")

;; Including the aliases to reduce switching necessary for re-mapping
;; keys via `jnf/toggle-roam-subject-filter'.  And while the
;; `create-org-roam-subject-fns-for' macro for :all creates the
;; methods, it has filter functions.
(defalias 'jnf/org-roam--all--node-insert 'org-roam-node-insert)
(defalias 'jnf/org-roam--all--node-find 'org-roam-node-find)
(defalias 'jnf/org-roam--all--capture 'org-roam-capture)

(create-org-roam-subject-fns-for :personal
                                 :menu_group "Life"
                                 :menu_prefix "p")
(create-org-roam-subject-fns-for :public
                                 :menu_group "Life"
                                 :menu_prefix "u")
(create-org-roam-subject-fns-for :forem
                                 :menu_group "Work"
                                 :menu_prefix "f")
(create-org-roam-subject-fns-for :hesburgh-libraries
                                 :menu_group "Work"
                                 :menu_prefix "h")
(create-org-roam-subject-fns-for :jf-consulting
                                 :menu_group "Projects"
                                 :menu_prefix "j")
(create-org-roam-subject-fns-for :eberron
                                 :menu_group "Projects"
                                 :menu_prefix "e")
(create-org-roam-subject-fns-for :thel-sector
                                 :menu_group "Projects"
                                 :menu_prefix "t")


(pretty-hydra-define+ jnf/org-subject-menu--all()
  ("All"
   (
    ("/" org-roam-buffer-toggle         "Toggle Buffer")
    ("#" jnf/toggle-roam-subject-filter "Toggle Default Filter")
    ("~" jnf/magit-list-repositories    "Magit List Repositories")
    ("1" (find-file (f-join jnf/tor-home-directory "todo.org")) "Todo for TakeOnRules.com")
    ("2" org-transclusion-add "Org Transclusion Add…")
    ("3" org-transclusion-mode "Org Transclusion Mode" :toggle t)
    )))
;;******************************************************************************
;;
;;; END Register Subjects
;;
;;******************************************************************************

;;******************************************************************************
;;
;;; BEGIN Toggle Subject Filter
;;
;;******************************************************************************
(cl-defun jnf/subject-list-for-completing-read (&key
                                                (subjects-plist
                                                 jnf/org-roam-capture-subjects-plist))
  "Create a list from the SUBJECTS-PLIST for completing read.

The form should be '((\"all\" 1) (\"hesburgh-libraries\" 2))."
  ;; Skipping the even entries as those are the "keys" for the plist,
  ;; the odds are the values.
  (-non-nil (seq-map-indexed (lambda (subject index)
                               (when (oddp index)
                                 (when (f-exists? (plist-get subject :path-to-todo))
                                   (list (plist-get subject :name) index))))
                             subjects-plist)))

(defun jnf/toggle-roam-subject-filter (subject)
  "Prompt for a SUBJECT, then toggle the 's-i' kbd to filter for that subject."
  (interactive (list
                (completing-read
                 "Subject: " (jnf/subject-list-for-completing-read))))
  (global-set-key
   (kbd "C-s-2")
   (intern (concat "jnf/org-roam--" subject "--todo")))
  (global-set-key
   (kbd "C-s-1")
   (intern (concat "jnf/org-roam--" subject "--node-insert")))
  (global-set-key
   (kbd "C-s-=")
   (intern (concat "jnf/org-roam--" subject "--capture")))
  (global-set-key
   (kbd "C-s-/")
   (intern (concat "jnf/org-roam--" subject "--node-find")))
  (global-set-key
   (kbd "s-i")
   (intern (concat "jnf/org-roam--" subject "--node-insert")))
  (global-set-key
   (kbd "C-c i")
   (intern (concat "jnf/org-subject-menu--" subject "/body"))))

;;******************************************************************************
;;
;;; END Toggle Subject Filter
;;
;;******************************************************************************

;; With the latest update of org-roam, things again behavior
;; correctly.  Now I can just load org-roam as part of my day to day
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
  :custom
  (org-roam-directory (file-truename "~/git/org"))
  ;; Set more spaces for tags; As much as I prefer the old format,
  ;; this is the new path forward.
  (org-roam-node-display-template "${title:*} ${tags:40}")
  (org-roam-capture-templates (jnf/org-roam-templates-for-subject :all))
  :bind
  (("C-s-;" . org-roam-buffer-toggle)
   ("C-s-3" . jnf/toggle-roam-subject-filter))
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
  (org-roam-db-autosync-mode)
  ;; Configure the "all" subject key map
  (jnf/toggle-roam-subject-filter "all"))

(use-package org-transclusion
  :straight t)
