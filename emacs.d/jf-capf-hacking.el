;;; jf-capf-hacking ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Allow for completion of projects and then issues.  Likely something I want to
;; include in commit messages.  This behaves in a two step fashion:

;; - Type in the project (/project)
;; - Tab and select the project
;; - It fills in /project#
;; - Then type a number (e.g. /project#123)
;; - Tab and it will unfurl to a github issue URL

;;; Code:

;;;; Dependencies

(require 'project)
(require 'projectile)

;;;; Primary Functions:

(defun jf/version-control/project-capf ()
  "Complete project links."
  (when (looking-back "[^[:word:]]/[[:word:][:digit:]_\-]+" (jf/capf-max-bounds))
    (let ((right (point))
           (left (save-excursion
                     ;; First check for the project
                     (search-backward-regexp "/[[:word:][:digit:]_\-]+" (jf/capf-max-bounds) t) (point))))
      (list left right
        (jf/version-control/known-project-names)
        :exit-function
        (lambda (text _status)
          (delete-char (- (length text)))
          (insert text "#"))
        :exclusive 'no))))

(defun jf/version-control/issue-capf ()
  "Complete links."
  (when (looking-back "[^[:word:]]/[[:word:][:digit:]_\-]+#[[:digit:]]+" (jf/capf-max-bounds))
    (let ((right (point))
           (left (save-excursion
                     ;; First check for the project
                     (search-backward-regexp "/[[:word:][:digit:]_\-]+#[[:digit:]]+" (jf/capf-max-bounds) t) (point))))
      (list left right
        (jf/version-control/text)
        :exit-function
        #'jf/version-control/unfurl-issue-to-url
        :exclusive 'no))))

(add-to-list 'completion-at-point-functions #'jf/version-control/issue-capf)
(add-to-list 'completion-at-point-functions #'jf/version-control/project-capf)

;;;; Service functions

(cl-defun jf/capf-max-bounds (&key (window-size 40))
  "Return the max bounds for `point' based on given WINDOW-SIZE."
  (let ((boundary (- (point) window-size)))
    (if (> 0 boundary) 1 boundary)))

(cl-defun jf/version-control/known-project-names (&key (prefix "/"))
  "Return a list of project, prepending PREFIX to each."
  (mapcar (lambda (proj)
            (concat prefix (f-base proj)))
    projectile-known-projects))

(cl-defun jf/version-control/unfurl-project-as-issue-url-template (project &key (prefix "/"))
  "Return the issue URL template for the given PROJECT.

Use the provided PREFIX to help compare against `projectile-known-projects'."
  (let* ((project-path
           (car (seq-filter (lambda (el)
                              (or
                                (s-ends-with? (concat project prefix) el)
                                (s-ends-with? project el)))
                  projectile-known-projects)))
          (remote
            (s-trim (shell-command-to-string
                      (format "cd %s && git remote get-url origin" project-path)))))
    (s-replace ".git" "/issues/%s" remote)))

(defun jf/version-control/text ()
  "Find all matches for project and issue."
  (s-match-strings-all "/[[:word:][:digit:]_\-]+#[[:digit:]]+" (buffer-string)))

(defun jf/version-control/unfurl-issue-to-url (text _status)
  "Unfurl the given TEXT to a URL.

Ignoring _STATUS."
  (delete-char (- (length text)))
  (let* ((parts (s-split "#" text))
          (issue (cadr parts))
          (project (or (car parts) (cdr (project-current)))))
    (insert (format
              (jf/version-control/unfurl-project-as-issue-url-template project)
              issue))))

(provide 'jf-capf-hacking)
;;; jf-capf-hacking.el ends here
