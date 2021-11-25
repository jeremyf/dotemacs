;;; jnf-forem.el --- Summary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  Provides some Forem specific shortcut tooling.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key
  hammerspoon-edit-minor-map
  (kbd "C-c t")
  #'jnf/forem-tidy-pull-request)

(defun jnf/forem-tidy-pull-request ()
  "Perform some quick tidying of the Forem PR template."
  (interactive)
  ;; Start from the beginning.
  (beginning-of-buffer)

  ;; The text before the first HTML/Markdown
  ;; comments is the commit message.  Cut that
  ;; text...
  (search-forward "<!--")
  (kill-region 1 (- (point) 4))

  ;; ...and paste it inside the description
  ;; section.
  (replace-string
   "## Description\n\n"
   (concat "## Description\n\n"
           (format "%s" (car kill-ring))))

  ;; We've moved point (e.g., the cursor) so let's
  ;; jump back to the beginning of the buffer.
  (beginning-of-buffer)

  ;; Remove HTML/Markdown comments
  (replace-regexp
   "\\(\n\\)*<!--\\(.\\|\n\\)*-->\\(\n\\)*"
   "")

  ;; Clean out the comments for QA instructions;
  ;; I'll write them, but the notes are
  ;; unnecessary.
  (replace-regexp
   "QA Instructions, Screenshots, Recordings\\([^#]\\)*"
   "QA Instructions, Screenshots, Recordings\n\n")

  ;; Clean out accessibility concerns; I'll write
  ;; them, but the notes are unnecessary.
  (replace-regexp
   "UI accessibility concerns?\\([^#]\\)*"
   "UI accessibility concerns?\n\n"))

(global-set-key (kbd "C-M-s-f") 'jnf/forem-menu/body)
(defvar jnf/forem-menu--title
  (with-octicon "code" "Forem" 1 -0.05)
  "The Forem Subject Menu Title.")
(pretty-hydra-define jnf/forem-menu (:foreign-keys warn :title jnf/forem-menu--title :quit-key "q" :exit t)
  ("Forem Github Workflow"
   (
    ("i" jnf/form-browse-url-issues "Browse to [i]ssues…")
    ("j" jnf/form-browse-url-projects "Browse to pro[j]ects…")
    ("p" jnf/form-browse-url-pull-requests "Browse to [p]ull requests…"))))

(defun jnf/form-browse-url-pull-requests ()
  "Open forem pull requests."
  (interactive)
  (if (equal current-prefix-arg nil)
      (browse-url "https://github.com/forem/forem/pulls/")
    (browse-url "https://github.com/forem/forem/pulls/jeremyf")))

(defun jnf/form-browse-url-issues ()
  "Open forem issues."
  (interactive)
  (if (equal current-prefix-arg nil)
      (browse-url "https://github.com/forem/forem/issues/")
    (browse-url "https://github.com/forem/forem/issues/assigned/jeremyf")))

(defun jnf/form-browse-url-projects ()
  "Open forem projects."
  (interactive)
  (if (equal current-prefix-arg nil)
      (browse-url "https://github.com/orgs/forem/projects/")
    (browse-url "https://github.com/orgs/forem/projects/39")))

(when (file-directory-p "~/git/org/forem")
  (progn
    (defconst jnf/forem-dashboard-filename
      "~/git/org/forem/dashboard.org"
      "The file to the dashboard documentation and links for Forem.")

    (cl-defun jnf/open-dashboard (&key (filename jnf/forem-dashboard-filename))
      "For the given FILENAME open the links in the default browser.

With the universal prefix (e.g. C-u) open the file instead."
      (interactive)
      (if (equal current-prefix-arg nil) ; no C-u
          (call-process-shell-command
           (concat "rg \"\\[\\[(.*)\\]\\[\" "
                   filename
                   " --only-matching"
                   " | rg \"[^\\[|\\]]+\" --only-matching"
                   " | xargs open"))
        (find-file filename)))

    (defun jnf/open-forem-todo ()
      "Open ~/git/org/forem/todo.org"
      (interactive)
      (find-file "~/git/org/forem/todo.org"))

    (defun jnf/open-forem-brag-book ()
      "Open forem brag book."
      (interactive)
      (find-file "~/git/org/forem/20211005---brag_book_for_jeremy_friesen.org"))

    (pretty-hydra-define+ jnf/forem-menu()
      ("Org-Mode"
       (
        ("@" jnf/open-forem-todo "Open Forem Todo")
        ("B" jnf/open-forem-brag-book "[B]rag book open…")
        ("D" jnf/open-dashboard "[D]ashboard open…"))))
    ))

(provide 'jnf-forem.el)
;;; jnf-forem.el ends here
