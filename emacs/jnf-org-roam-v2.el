;;; jnf-org-roam-v2.el --- Summary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  This package provides configuration for org-roam
;;
;;; Code

;; A plist that contains the various org-roam contexts.  Each context
;; has a plist of :templates and :name.
(setq jnf/org-roam-capture-contexts-plist
      (list
       :all (list
             :templates (list
                         :hesburgh-libraries
                         :personal
                         :personal-encrypted
                         :thel-sector
                         :public)
             :name "all")
       :hesburgh-libraries (list
                            :templates (list :hesburgh-libraries)
                            :name "hesburgh-libraries")
       :personal (list
                  :templates (list
                              :personal
                              :personal-encrypted)
                  :name "personal")
       :public (list
                :templates (list
                            :public)
                :name "public")
       :thel-sector (list
                     :templates (list
                                 :thel-sector)
                     :name "thel-sector")
       ))

;; A Property List of my `org-roam' capture templates.
(setq jnf/org-roam-capture-templates-plist
      (list
       :hesburgh-libraries '("h" "Hesburgh Libraries" plain "%?"
		             :if-new (file+head "hesburgh-libraries/%<%Y%m%d>---${slug}.org"
				                "#+title: ${title}\n#+FILETAGS: :hesburgh: %^G\n\n")
		             :unnarrowed t)
       :personal '("p" "Personal" plain "%?"
		   :if-new (file+head "personal/%<%Y%m%d>---${slug}.org"
				      "#+title: ${title}\n#+FILETAGS: :personal: %^G\n\n")
		   :unnarrowed t)
       :personal-encrypted '("P" "Personal (Encrypted)" plain "%?"
		             :if-new (file+head "personal/%<%Y%m%d>---${slug}.org.gpg"
				                "#+title: ${title}\n#+FILETAGS: :personal:encrypted: %^G\n\n")
		             :unnarrowed t)
       :public '("u" "Public" plain "%?"
		 :if-new (file+head "public/%<%Y%m%d>---${slug}.org"
				    "#+title: ${title}\n#+FILETAGS: :public: %^G\n\n")
		 :unnarrowed t)
       :thel-sector '("t" "Thel Sector" plain "%?"
                      :if-new (file+head "personal/thel-sector/%<%Y%m%d>---${slug}.org"
                                         "#+title: ${title}\n#+FILETAGS: :thel-sector: %^G\n\n")
                      :unnarrowed t)
       ))

(cl-defun jnf/org-roam-templates-for-context (context
                                              &key
                                              (contexts-plist jnf/org-roam-capture-contexts-plist)
                                              (template-definitions-plist jnf/org-roam-capture-templates-plist))
  "Return a list of `org-roam' templates for the given CONTEXT.

Use the given (or default) CONTEXTS-PLIST to fetch from the
given (or default) TEMPLATE-DEFINITIONS-PLIST."
  (let ((templates (plist-get (plist-get contexts-plist context) :templates)))
    (-map (lambda (template) (plist-get template-definitions-plist template))
          templates)))

(defmacro create-org-roam-capture-fn-for (project)
  "Define a `jnf/org-roam-capture' function for PROJECT."
  (let* ((project-as-symbol (intern (concat ":" project)))
         (fn-name (intern (concat "jnf/org-roam--" project "--capture")))
         (docstring (concat "As `jnf/org-roam-capture' but scoped to " project
                            ".\n\nArguments GOTO and KEYS see `org-capture'.")))
    `(defun ,fn-name (&optional goto keys)
       ,docstring
       (interactive "P")
       (org-roam-capture goto
                         keys
                         :filter-fn (lambda (node) (-contains-p (org-roam-node-tags node) ,project))
                         :templates (jnf/org-roam-templates-for-context ,project-as-symbol)))))

(defmacro create-org-roam-node-insert-fn-for (project)
  "Define a `jnf/org-roam-node-insert' function for PROJECT."
  (let* ((project-as-symbol (intern (concat ":" project)))
         (fn-name (intern (concat "jnf/org-roam--" project "--node-insert")))
         (docstring (concat "As `jnf/org-roam-insert-node' but scoped to " project " project.")))
    `(defun ,fn-name ()
       ,docstring
       (interactive)
       (org-roam-node-insert (lambda (node) (-contains-p (org-roam-node-tags node) ,project))
                             :templates (jnf/org-roam-templates-for-context ,project-as-symbol)))))

(defmacro create-org-roam-node-find-fn-for (project)
  "Define a `jnf/org-roam-node-find' function for PROJECT."
  (let* ((project-as-symbol (intern (concat ":" project)))
         (fn-name (intern (concat "jnf/org-roam--" project "--node-find")))
         (docstring (concat "As `jnf/org-roam-find-node' but scoped to "
                            project " project."
                            "\n\nArguments INITIAL-INPUT and OTHER-WINDOW are from `org-roam-find-mode'.")))
    `(defun ,fn-name (&optional other-window initial-input)
       ,docstring
       (interactive current-prefix-arg)
       (org-roam-node-find other-window
                           initial-input
                           (lambda (node) (-contains-p (org-roam-node-tags node) ,project))
                           :templates (jnf/org-roam-templates-for-context ,project-as-symbol)))))

(create-org-roam-capture-fn-for "thel-sector")
(create-org-roam-node-insert-fn-for "thel-sector")
(create-org-roam-node-find-fn-for "thel-sector")

(create-org-roam-capture-fn-for "public")
(create-org-roam-node-insert-fn-for "public")
(create-org-roam-node-find-fn-for "public")

(create-org-roam-capture-fn-for "personal")
(create-org-roam-node-insert-fn-for "personal")
(create-org-roam-node-find-fn-for "personal")

(create-org-roam-capture-fn-for "hesburgh-libraries")
(create-org-roam-node-insert-fn-for "hesburgh-libraries")
(create-org-roam-node-find-fn-for "hesburgh-libraries")

(defun jnf/find-file--personal-todo ()
  "Find personal todo file."
  (interactive)
  (find-file (file-truename "~/git/org/personal/todo.org")))

(defun jnf/find-file--hesburgh-libraries-todo ()
  "Find hesburgh-libraries todo file."
  (interactive)
  (find-file (file-truename "~/git/org/hesburgh-libraries/todo.org")))


;; A menu of common tasks for `org-roam'.
(defvar jnf/org-subject-menu--title (with-faicon "book" "Org Subject Menu" 1 -0.05))
(pretty-hydra-define jnf/org-subject-menu--all (:foreign-keys warn :title jnf/org-subject-menu--title :quit-key "q" :exit t)
  (
   "Public / Personal"
   (
    ("p t" jnf/find-file--personal-todo        "Personal Todo…")
    ("p +" jnf/org-roam--personal--capture     " ├─ Capture…")
    ("p !" jnf/org-roam--personal--node-insert " ├─ Insert…")
    ("p ?" jnf/org-roam--personal--node-find   " ╰─ Find…")
    ("u +" jnf/org-roam--public--capture       "Public: Capture…")
    ("u !" jnf/org-roam--public--node-insert   " ├─ Insert…")
    ("u ?" jnf/org-roam--public--node-find     " ╰─ Find…")
    )
   "Projects"
   (
    ("h t" jnf/find-file--hesburgh-libraries-todo        "Hesburgh Libraries Todo…")
    ("h +" jnf/org-roam--hesburgh-libraries--capture     " ├─ Capture…")
    ("h !" jnf/org-roam--hesburgh-libraries--node-insert " ├─ Insert…")
    ("h ?" jnf/org-roam--hesburgh-libraries--node-find   " ╰─ Find…")
    ("t +" jnf/org-roam--thel-sector--capture            "Thel Sector: Capture …")
    ("t !" jnf/org-roam--thel-sector--node-insert        " ├─ Insert…")
    ("t ?" jnf/org-roam--thel-sector--node-find          " ╰─ Find…")
    )
   "Org Mode"
   (("+" org-roam-capture               "Capture…")
    ("!" org-roam-node-insert           " ├─ Insert…")
    ("?" org-roam-node-find             " ╰─ Find…")
    ("@" org-roam-dailies-capture-today "Capture Daily…")
    ("#" org-roam-buffer-toggle         "Toggle Buffer")
    ("*" jnf/toggle-roam-project-filter "Toggle Default Filter")
    )))

(pretty-hydra-define jnf/org-subject-menu--thel-sector (:foreign-keys warn :title jnf/org-subject-menu--title :quit-key "q" :exit t)
  (
   "Thel Sector Subject Menu"
   (("+" jnf/org-roam--thel-sector--capture     "Thel Sector: Capture…")
    ("!" jnf/org-roam--thel-sector--node-insert " ├─ Insert…")
    ("?" jnf/org-roam--thel-sector--node-find   " ╰─ Find Node…")
    ("@" org-roam-dailies-capture-today         "Capture Daily…")
    ("#" org-roam-buffer-toggle                 "Toggle Buffer")
    ("*" jnf/toggle-roam-project-filter         "Toggle Filter…")
    )))

(pretty-hydra-define jnf/org-subject-menu--personal (:foreign-keys warn :title jnf/org-subject-menu--title :quit-key "q" :exit t)
  (
   "Personal Subject Menu"
   (
    ("t" jnf/find-file--personal-todo       "Personal Todo…")
    ("+" jnf/org-roam--personal--capture     " ├─ Capture…")
    ("!" jnf/org-roam--personal--node-insert " ├─ Insert…")
    ("?" jnf/org-roam--personal--node-find   " ╰─ Find…")
    ("@" org-roam-dailies-capture-today      "Capture Daily…")
    ("#" org-roam-buffer-toggle              "Toggle Buffer")
    ("*" jnf/toggle-roam-project-filter      "Toggle Filter…")
    )))

(pretty-hydra-define jnf/org-subject-menu--public (:foreign-keys warn :title jnf/org-subject-menu--title :quit-key "q" :exit t)
  (
   "Public Subject Menu"
   (
    ("+" jnf/org-roam--public--capture     "Public: Capture…")
    ("!" jnf/org-roam--public--node-insert " ├─ Insert…")
    ("?" jnf/org-roam--public--node-find   " ╰─ Find…")
    ("@" org-roam-dailies-capture-today    "Capture Daily…")
    ("#" org-roam-buffer-toggle            "Toggle Buffer")
    ("*" jnf/toggle-roam-project-filter    "Toggle Filter…")
    )))

(pretty-hydra-define jnf/org-subject-menu--hesburgh-libraries (:foreign-keys warn :title jnf/org-subject-menu--title :quit-key "q" :exit t)
  (
   "Hesburgh Libraries Subject Menu"
   (
    ("t" jnf/find-file--hesburgh-libraries-todo        "Hesburgh Libraries Todo…")
    ("+" jnf/org-roam--hesburgh-libraries--capture     " ├─ Capture…")
    ("!" jnf/org-roam--hesburgh-libraries--node-insert " ├─ Insert…")
    ("?" jnf/org-roam--hesburgh-libraries--node-find   " ╰─ Find Node…")
    ("@" org-roam-dailies-capture-today                "Capture Daily…")
    ("#" org-roam-buffer-toggle                        "Toggle Buffer")
    ("*" jnf/toggle-roam-project-filter                "Toggle Filter…")
    )))

(defun jnf/toggle-roam-project-filter (project)
  "Prompt for a PROJECT, then toggle the `s-i' kbd to filter for that project."
  (interactive (list
                (completing-read
                 "Project: " '(("all" 1)
                               ("hesburgh-libraries" 2)
                               ("thel-sector" 3)
                               ("personal" 4)
                               ("public" 5)
                               ))))
  (global-set-key (kbd "s-i") (intern (concat "jnf/org-roam--" project "--node-insert")))
  (global-set-key (kbd "C-c i") (intern (concat "jnf/org-subject-menu--" project "/body"))))

;; Including the alias to reduce switching necessary for `jnf/toggle-roam-project-filter'.
(defalias 'jnf/org-roam--all--node-insert 'org-roam-node-insert)

;; With the latest update of org-roam, things again behavior
;; correctly.  Now I can just load org-roam as part of my day to day
(use-package org-roam
  :straight t
  :custom
  (org-roam-directory (file-truename "~/git/org"))
  ;; Set more spaces for tags; As much as I prefer the old format,
  ;; this is the new path forward.
  (org-roam-node-display-template "${title:*} ${tags:40}")
  (org-roam-capture-templates (jnf/org-roam-templates-for-context :all))
  :init
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))

  (setq org-roam-v2-ack t)
  :bind (("s-i" . jnf/org-roam--all--node-insert)
         ("C-c i" . jnf/org-subject-menu--all/body)))

(org-roam-setup)

(provide 'jnf-org-roam-v2.el)
;;; jnf-org-roam-v2.el ends here
