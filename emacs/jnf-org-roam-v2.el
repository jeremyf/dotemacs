;;; jnf-org-roam-v2.el --- Summary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  This package provides configuration for org-roam
;;
;;; Code

;; See
;; http://takeonrules.com/2021/08/22/ever-further-refinements-of-org-roam-usage/
;; for details on this configuration.
;;
;; A Property List of my `org-roam' capture templates.
(setq jnf/org-roam-capture-templates-plist
      (list
       :hesburgh-libraries
       '("h" "Hesburgh Libraries" plain "%?"
	 :if-new
         (file+head
          "hesburgh-libraries/%<%Y%m%d>---${slug}.org"
          "#+title: ${title}\n#+FILETAGS: :hesburgh: %^G\n\n")
	 :unnarrowed t)
       :jf-consulting
       '("j" "JF Consulting" plain "%?"
	 :if-new
         (file+head
          "jeremy-friesen-consulting/%<%Y%m%d>---${slug}.org"
          "#+title: ${title}\n#+FILETAGS: :personal:jeremy-friesen-consulting: %^G\n\n")
	 :unnarrowed t)
       :personal
       '("p" "Personal" plain "%?"
	 :if-new
         (file+head
          "personal/%<%Y%m%d>---${slug}.org"
	  "#+title: ${title}\n#+FILETAGS: :personal: %^G\n\n")
	 :unnarrowed t)
       :personal-encrypted
       '("P" "Personal (Encrypted)" plain "%?"
	 :if-new
         (file+head
          "personal/%<%Y%m%d>---${slug}.org.gpg"
          "#+title: ${title}\n#+FILETAGS: :personal:encrypted: %^G\n\n")
	 :unnarrowed t)
       :public
       '("u" "Public" plain "%?"
	 :if-new
         (file+head
          "public/%<%Y%m%d>---${slug}.org"
	  "#+title: ${title}\n#+FILETAGS: :public: %^G\n\n")
	 :unnarrowed t)
       :thel-sector
       '("t" "Thel Sector" plain "%?"
         :if-new
         (file+head
          "personal/thel-sector/%<%Y%m%d>---${slug}.org"
          "#+title: ${title}\n#+FILETAGS: :thel-sector: %^G\n\n")
         :unnarrowed t)
       ))

;; A plist that contains the various org-roam subject.  Each subject
;; has a plist of :templates, :title, :name, :path-to-todo, :prefix,
;; and :group.
;;
;; The :templates defines the ;; named templates available for this subject.  See
;; `jnf/org-roam-capture-templates-plist' for list of valid templates.
;;
;; The :name is the string version of the subject, suitable for
;; creating function names.
;;
;; The :title is the human readable "title-case" form of the subject.
;;
;; The :group is for `pretty-hydra-define+'
;;
;; The :prefix helps with menu key build for `pretty-hydra-define+'
;;
;; The :path-to-todo is the path to the todo file for this subject.
(setq jnf/org-roam-capture-subjects-plist
      (list
       ;; The :all subject is different from the other items.
       :all (list
             :templates (list :jf-consulting :hesburgh-libraries :personal :personal-encrypted :thel-sector :public)
             :name "all"
             :title "All"
             :group "All"
             :prefix "a"
             :path-to-todo "~/git/org/todo.org")
       :jf-consulting (list
                       :templates (list :jf-consulting)
                       :name "jf-consulting"
                       :title "JF Consulting"
                       :group "Projects"
                       :prefix "j"
                       :path-to-todo "~/git/org/jeremy-friesen-consulting/todo.org")
       :hesburgh-libraries (list
                            :templates (list :hesburgh-libraries)
                            :name "hesburgh-libraries"
                            :title "Hesburgh Libraries"
                            :group "Projects"
                            :prefix "h"
                            :path-to-todo "~/git/org/hesburgh-libraries/todo.org")
       :personal (list
                  :templates (list :personal :personal-encrypted)
                  :name "personal"
                  :title "Personal"
                  :group "Personal / Public"
                  :prefix "p"
                  :path-to-todo "~/git/org/personal/todo.org")
       :public (list
                :templates (list :public)
                :name "public"
                :title "Public"
                :group "Personal / Public"
                :prefix "u"
                :path-to-todo "~/git/org/public/todo.org")
       :thel-sector (list
                     :templates (list :thel-sector)
                     :name "thel-sector"
                     :title "Thel Sector"
                     :group "Projects"
                     :prefix "t"
                     :path-to-todo "~/git/org/personal/thel-sector/todo.org")
       ))

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

;; A menu of common tasks for `org-roam'.  This menu is for all subjects.
;;
;; Note the convention:
;;
;; * t - for todo
;; * c - for capture
;; * i - for insert
;; * f - for find
;;
;; The `create-org-roam-subject-fns-for' presupposes those keys for
;; narrowed subjects.
(defvar jnf/org-subject-menu--title (with-faicon "book" "Org Subject Menu" 1 -0.05))
(pretty-hydra-define jnf/org-subject-menu--all (:foreign-keys warn :title jnf/org-subject-menu--title :quit-key "q" :exit t)
  (
   ;; Note: This matches at least one of the :groups in `jnf/org-roam-capture-subjects-plist'
   "Personal / Public"
   ()
   ;; Note: This matches at least one of the :groups in `jnf/org-roam-capture-subjects-plist'
   "Projects"
   ()
   "Org Mode"
   (("t" (lambda ()
           (interactive)
           (find-file (file-truename (plist-get (plist-get jnf/org-roam-capture-subjects-plist :all) :path-to-todo))))
     "Todo…")
    ("c" jnf/org-roam--all--capture     "Capture…")
    ("i" jnf/org-roam--all--node-insert " ├─ Insert…")
    ("f" jnf/org-roam--all--node-find   " └─ Find…")
    ("/" org-roam-buffer-toggle         "Toggle Buffer")
    ("#" jnf/toggle-roam-project-filter "Toggle Default Filter")
    )))

(cl-defmacro create-org-roam-subject-fns-for (subject
                                              &key
                                              (subjects-plist jnf/org-roam-capture-subjects-plist))
  "Define the org roam SUBJECT functions and create & update hydra menus.

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
         (hydra-fn-name (intern (concat "jnf/org-subject-menu--" subject-name)))
         (hydra-menu-title (concat subject-title " Subject Menu"))
         (hydra-todo-title (concat subject-title " Todo…"))
         (hydra-group (plist-get subject-plist :group))
         (hydra-prefix (plist-get subject-plist :prefix))
         (hydra-kbd-prefix-todo    (concat hydra-prefix " t"))
         (hydra-kbd-prefix-capture (concat hydra-prefix " c"))
         (hydra-kbd-prefix-insert  (concat hydra-prefix " i"))
         (hydra-kbd-prefix-find    (concat hydra-prefix " f"))

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
    `(progn
       (defun ,todo-fn-name ()
         ,todo-docstring
         (interactive)
         (find-file (file-truename ,path-to-todo)))

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
           ("t" ,todo-fn-name        ,hydra-todo-title)
           ("c" ,capture-fn-name     " ├─ Capture…")
           ("i" ,insert-fn-name      " ├─ Insert…")
           ("f" ,find-fn-name        " └─ Find…")
           ("/" org-roam-buffer-toggle            "Toggle Buffer")
           ("#" jnf/toggle-roam-project-filter    "Toggle Filter…")
           )))

       ;; Append the following menu items to the `jnf/org-subject-menu--all'
       (pretty-hydra-define+ jnf/org-subject-menu--all()
         (,hydra-group
          (
           (,hydra-kbd-prefix-todo    ,todo-fn-name    ,hydra-todo-title)
           (,hydra-kbd-prefix-capture ,capture-fn-name " ├─ Capture…")
           (,hydra-kbd-prefix-insert  ,insert-fn-name  " ├─ Insert…")
           (,hydra-kbd-prefix-find    ,find-fn-name    " └─ Find…")
           )))
       )))

;; I tried using a dolist to call each of the macros, but that didn't
;; work.  I'd love some additional help refactoring this.  But for
;; now, what I have is quite adequate.  It would be nice to
;; more programatically generate the hydra menus (see below).
(create-org-roam-subject-fns-for :personal)
(create-org-roam-subject-fns-for :public)
(create-org-roam-subject-fns-for :hesburgh-libraries)
(create-org-roam-subject-fns-for :jf-consulting)
(create-org-roam-subject-fns-for :thel-sector)

;; Including the aliases to reduce switching necessary for re-mapping
;; keys via `jnf/toggle-roam-project-filter'.
(defalias 'jnf/org-roam--all--node-insert 'org-roam-node-insert)
(defalias 'jnf/org-roam--all--node-find 'org-roam-node-find)
(defalias 'jnf/org-roam--all--capture 'org-roam-capture)

(defun jnf/toggle-roam-project-filter (project)
  "Prompt for a PROJECT, then toggle the 's-i' kbd to filter for that project."
  (interactive (list
                (completing-read
                 ;; TODO: Use the
                 ;; `jnf/org-roam-capture-subjects-plist' to create
                 ;; this list.
                 "Project: " '(("all" 1)
                               ("hesburgh-libraries" 2)
                               ("jf-consulting" 3)
                               ("thel-sector" 4)
                               ("personal" 5)
                               ("public" 6)
                               ))))
  (global-set-key
   ;; Command + Control + i
   (kbd "s-TAB")
   (intern (concat "jnf/org-roam--" project "--node-insert")))
  (global-set-key
   (kbd "C-s-c")
   (intern (concat "jnf/org-roam--" project "--capture")))
  (global-set-key
   (kbd "C-s-f")
   (intern (concat "jnf/org-roam--" project "--find")))
  (global-set-key
   (kbd "s-i")
   (intern (concat "jnf/org-roam--" project "--node-insert")))
  (global-set-key
   (kbd "C-c i")
   (intern (concat "jnf/org-subject-menu--" project "/body"))))

;; With the latest update of org-roam, things again behavior
;; correctly.  Now I can just load org-roam as part of my day to day
(use-package org-roam
  :straight t
  :custom
  (org-roam-directory (file-truename "~/git/org"))
  ;; Set more spaces for tags; As much as I prefer the old format,
  ;; this is the new path forward.
  (org-roam-node-display-template "${title:*} ${tags:40}")
  (org-roam-capture-templates (jnf/org-roam-templates-for-subject :all))
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

  (setq org-roam-v2-ack t)
  (org-roam-db-autosync-mode)
  ;; Configure the "all" subject key map
  (jnf/toggle-roam-project-filter "all"))

(provide 'jnf-org-roam-v2.el)
;;; jnf-org-roam-v2.el ends here
