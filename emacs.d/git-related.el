;;; git-related.el --- Find related files through commit history analysis -*- lexical-binding: t -*-

;; Copyright (C) 2023 Nthcdr

;; Author: Nthcdr <nthcdr@macroexpand.net>
;; Maintainer: Nthcdr <nthcdr@macroexpand.net>
;; Contributions: Jeremy Friesen <jeremy@jeremyfriesen.com>
;; URL: https://macroexpand.net/el/git-related.el
;; Version: 1.0
;; Package-Requires: ((emacs "28.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Find files by recommendation based on git commit history.

;; Usage: Visiting a git versioned file run once (and then only when
;; you feel the need to refresh) `consult-git-related-update' than you will get
;; suggestions based on the current file through invocations to
;; `consult-git-related-find-file'

;;; Todo:

;; Test that the graph exists, if not, run the command for the graph then
;; proceed.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'vc-git)
(require 'consult)

(defvar git-related--graphs nil)

(cl-defstruct git-related--graph files commits)
(cl-defstruct git-related--file (name "" :type string) (commits nil :type list))
(cl-defstruct git-related--commit (sha "" :type string) (files nil :type list))

(defun git-related--new-graph ()
  "Create an empty graph."
  (make-git-related--graph
    :files (make-hash-table :test 'equal :size 2500)
    :commits (make-hash-table :test 'equal :size 2500)))

(defun git-related--record-commit (graph sha filenames)
  "Record in the GRAPH the relation between SHA and FILENAMES."
  (let ((commit (make-git-related--commit :sha sha)))
    (dolist (filename filenames)
      (let* ((seen-file (gethash filename (git-related--graph-files graph)))
              (file-found (not (null seen-file)))
              (file (or seen-file (make-git-related--file :name filename))))

        (cl-pushnew commit (git-related--file-commits file))
        (cl-pushnew file (git-related--commit-files commit))

        (unless file-found
          (setf (gethash filename (git-related--graph-files graph)) file))))

    (setf (gethash sha (git-related--graph-commits graph)) commit)))

(defun git-related--replay (&optional graph)
  "Replay git commit history into optional GRAPH."
  (let ((graph (or graph (git-related--new-graph))))
    (with-temp-buffer
      (process-file vc-git-program nil t nil "log" "--name-only" "--format=%x00%H")
      (let* ((commits (split-string (buffer-string) "\0" t))
              (replay-count 0)
              (progress-reporter (make-progress-reporter "Building commit-file graph..." 0 (length commits))))
        (dolist (commit commits)
          (let* ((sha-and-paths (split-string commit "\n\n" t (rx whitespace)))
                  (sha (car sha-and-paths))
                  (paths (when (cadr sha-and-paths)
                           (split-string (cadr sha-and-paths) "\n" t (rx whitespace)))))
            (git-related--record-commit graph sha paths)
            (progress-reporter-update progress-reporter (cl-incf replay-count))))
        (progress-reporter-done progress-reporter)))
    graph))

(defun git-related--similar-files (graph filename)
  "Return files in GRAPH that are similar to FILENAME."
  (unless (git-related--graph-p graph)
    (user-error "You need to index this project first"))
  (when-let ((file (gethash filename (git-related--graph-files graph))))

    (let ((file-sqrt (sqrt (length (git-related--file-commits file))))
           (neighbor-sqrts (make-hash-table :test 'equal :size 100))
           (hits (make-hash-table :test 'equal :size 100)))

      (dolist (commit (git-related--file-commits file))
        (dolist (neighbor (remove file (git-related--commit-files commit)))
          (let ((count (cl-incf (gethash (git-related--file-name neighbor) hits 0))))
            (when (= count 1)
              (setf (gethash (git-related--file-name neighbor) neighbor-sqrts)
                (sqrt (length (git-related--file-commits neighbor))))))))

      (let (ranked-neighbors)
        (maphash
          (lambda (neighbor-name neighbor-sqrt)
            (let ((axb (* file-sqrt neighbor-sqrt))
                   (n (gethash neighbor-name hits)))
              (push (list (if (cl-plusp axb) (/ n axb) 0.0) neighbor-name) ranked-neighbors)))
          neighbor-sqrts)
        ;; We want to sort in descending score order.  Thus the more "related"
        ;; files are at the beginning of the list.
        (cl-sort
          (cl-remove-if-not #'git-related--file-exists-p ranked-neighbors :key #'cadr)
          #'> :key #'car)))))

(defun git-related--file-exists-p (relative-filename)
  "Determine if RELATIVE-FILENAME currently exists."
  (file-exists-p
    (expand-file-name relative-filename
      (project-root (project-current)))))

(defun consult-git-related--propertize-hit (hit)
  "Given the cons HIT return a rendered representation for completion."
  (propertize
    (cadr hit)
    'score (car hit)
    'path (cadr hit)))

;;;###autoload
(defun consult-git-related-update ()
  "Update graph for the current project."
  (interactive)
  (let* ((default-directory (project-root (project-current)))
          (project-symbol (intern (project-name (project-current))))
          (graph (cl-getf git-related--graphs project-symbol)))
    (setf (cl-getf git-related--graphs project-symbol)
      (git-related--replay graph))))

;;;###autoload
(defun consult-git-related-find-file ()
  "Find files related through commit history."
  (interactive)
  (if (buffer-file-name)
    (let ((default-directory (project-root (project-current))))
      (find-file
        (when-let ((selection (consult-git-related--read)))
          (format "%s"  selection))))
    (user-error "Current buffer has no file")))

(defun consult-git-related--read ()
  "A completing read function leveraging `consult-read'."
  (consult--read
    (consult--slow-operation "Building Git Relationships..."
      (mapcar #'consult-git-related--propertize-hit
        (git-related--similar-files
          (cl-getf git-related--graphs (intern (project-name (project-current))))
          (file-relative-name (buffer-file-name) (project-root (project-current))))))
    :prompt "Related files in Git history: "
    :category 'consult-git-related
    ;; This should be nil so we leverage the sort of the `git-related--similar-files'
    :sort nil
    :annotate #'consult-git-related--annotator
    :require-match t
    :history t))

(defun consult-git-related--annotator (cand)
  "Annotate the given CAND with it's score and modified date."
  (consult--annotate-align cand
    (format "%3.3f · %s"
      (get-text-property 0 'score cand)
      (format-time-string "%Y-%m-%d" (f-change-time cand)))))

(provide 'git-related)

;;; git-related.el ends here
