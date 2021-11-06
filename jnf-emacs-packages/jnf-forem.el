;;; jnf-forem.el --- Summary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  Provides some Forem specific shortcut tooling.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (file-directory-p "~/git/org/forem")
  (progn
    (defconst jnf/forem-dashboard-filename
      "~/git/org/forem/dashboard.org"
      "The file to the dashboard documentation and links for Forem.")

    (global-set-key (kbd "C-M-s-d") 'jnf/open-dashboard)
    (cl-defun jnf/open-dashboard (&key (filename jnf/forem-dashboard-filename))
      "For the given FILENAME open the links in the default browser.

With the universal prefix (e.g. C-u) open the file instead."
      (interactive)
      (if (equal current-prefix-arg nil) ; no C-u
          (call-process-shell-command
           (concat "rg \"\\[\\[(.*)\\]\\[\""
                   filename
                   " --only-matching"
                   " | rg \"[^\\[|\\]]+\" --only-matching"
                   " | xargs open")
        (find-file filename))))

    (global-set-key (kbd "C-M-s-f") 'jnf/open-forem-todo)
    (defun jnf/open-forem-todo ()
      "Open ~/git/org/forem/todo.org"
      (interactive)
      (find-file "~/git/org/forem/todo.org"))

    (global-set-key (kbd "C-M-s-b") 'jnf/open-forem-brag-book)
    (defun jnf/open-forem-brag-book ()
      "Open forem brag book."
      (interactive)
      (find-file "~/git/org/forem/20211005---brag_book_for_jeremy_friesen.org"))
    ))
(provide 'jnf-forem.el)
;;; jnf-forem.el ends here
