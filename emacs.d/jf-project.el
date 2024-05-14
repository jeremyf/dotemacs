;;; jf-project --- Connective Tissue for Projects -*- lexical-binding: t -*-

;; Copyright (C) 2022 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary

;; There are three interactive functions:
;;
;; - `jf/project/jump-to/notes'
;; - `jf/project/jump-to/project-work-space'
;; - `jf/project/jump-to/timesheet'
;;
;; Let's talk of the three targets for jumping.
;;
;; Notes: Each project has an index.  The index is a place for high-level notes
;; and links to related concepts:
;;
;; Project Space: Each project has different spaces where I do work, examples
;; include the following:
;;
;; - Agenda :: Where I track time.
;; - Code :: Where I write code.
;; - Discussion :: Where I discuss the project with collaborators.
;; - Notes :: Where I take larger conceptual notes.
;; - Project board :: Where I see what's in flight.
;; - Remote :: Where I read/write issues and pull requests.
;;
;; Timesheet: For many projects, I track time.  This lets me jump to today's
;; headline for the given project.  The headline is where I record tasks to
;; which I then track time.
;;
;; Each project's index is assumed to be an `org-mode' file with two top-level
;; keywords:
;;
;; `#+PROJECT_NAME:'
;; `#+PROJECT_PATHS:'
;;
;; There should be one `#+PROJECT_NAME:' keyword and there can be many
;; `#+PROJECT_PATHS:'.  Each `#+PROJECT_PATHS:' is a `cons' cell.  The `car' is
;; the label and the `cdr' is the path.  The path can be a filename or a URL.
;;
;; The `jf/project/jump-to/project-work-space' will prompt for a project then a
;; workspace.  From there, it will jump to the corresponding location.

;;; Code

;;;; Dependencies
(require 's)
(require 'f)
(require 'pulsar)
;;;; Interactive Commands


(provide 'jf-project)
;;; jf-project.el ends here
