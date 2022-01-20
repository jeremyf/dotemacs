;; -*- lexical-binding: t; -*-
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

  (beginning-of-buffer)

  ;; Clear out the text I almost always delete.
  ;;
  ;; This clears out the two or three line "please"!
  (replace-regexp ": _please.*\\(\n +.+\\)+_\n" ":\n")

  (beginning-of-buffer)

  ;; Clear out some comments.
  (replace-regexp "^\n_\\(If\\|Please\\|Will\\).*\\(\n.+\\)+_\n+" "\n")

  ;; Clean out the GIF section.  As much as I try
  ;; I just don't like adding GIFs to PRs
  (replace-regexp
   "\n## \\[optional\\] What gif\\(.\\)*\n+\\!.*"
   "")

  ;; Jump to the beginning of the buffer...again.
  (beginning-of-buffer))

(global-set-key (kbd "C-M-s-f") 'jnf/forem-menu/body)
(defvar jnf/forem-menu--title
  (with-octicon "code" "Forem" 1 -0.05)
  "The Forem Subject Menu Title.")
(pretty-hydra-define jnf/forem-menu (:foreign-keys warn :title jnf/forem-menu--title :quit-key "q" :exit t)
  ("Forem Workflow"
   (
    ("i" (lambda () (interactive) (browse-url "https://github.com/forem/forem/issues/")) "Browse to [i]ssues…")
    ("j" (lambda () (interactive) (browse-url "https://github.com/orgs/forem/projects/")) "Browse to pro[j]ects…")
    ("m" (lambda () (interactive) (browse-url "https://github.com/forem/forem/pulls?q=is%3Apr+is%3Aclosed")) "Browse [m]erged Pull Requests…")
    ("p" (lambda () (interactive) (browse-url "https://github.com/forem/forem/pulls/")) "Browse to pull requests…"))
   "Jeremy's Workflow"
   (
    ("I" (lambda () (interactive) (browse-url "https://github.com/forem/forem/issues/assigned/jeremyf")) "Browse to Jeremy's [I]ssues…")
    ("J" (lambda () (interactive) (browse-url "https://github.com/orgs/forem/projects/39")) "Browse to Content Experience pro[J]ects…")
    ("M" (lambda () (interactive) (browse-url "https://github.com/forem/forem/pulls/jeremyf?q=is%3Apr+is%3Aclosed")) "Browse Jeremy's [M]erged Pull Requests…")
    ("P" (lambda () (interactive) (browse-url "https://github.com/forem/forem/pulls/jeremyf")) "Browse to Jeremy's [P]ull requests…")
    ("N" (lambda () (interactive) (browse-url "https://github.com/notifications")) "Browse Github [N]otifications"))))

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
        ("D" jnf/open-dashboard "[D]ashboard open…")
        ("d" (lambda () (interactive) (find-file jnf/forem-dashboard-filename)) "Visit [d]ashboard file…"))))
    ))
