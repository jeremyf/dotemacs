;;; ox-hugo-simple --- A Simplified Hugo Export -*- lexical-binding: t -*-

;; Copyright (C) 2024 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;; A less configurable, more opinionated `org-mode' to Hugo exporter.
;;; Code:

;;; Variables
(defvar ox-hugo-simple-footnotes-as-sidenotes t
  "Default `ox-md' exports footnotes as links at the bottom of
 the page.  The author prefers sidenotes.")

(defcustom ox-hugo-content-block-export-alist
  '((marginnote . ox-hugo-simple-content-block-marginnote)
     ; Baseline block elements.  There are more
     (details . ox-hugo-simple-content-block-as-html-block-element)
     (summary . ox-hugo-simple-content-block-as-html-block-element)
     (aside . ox-hugo-simple-content-block-as-html-block-element)
     (header . ox-hugo-simple-content-block-as-html-block-element)
     (footer . ox-hugo-simple-content-block-as-html-block-element)
     (nav . ox-hugo-simple-content-block-as-html-block-element)
     (div . ox-hugo-simple-content-block-as-html-block-element))
  "An alist of named content blocks (e.g. \"#+begin_details\")
 and their associated rendering function.")

(provide 'ox-hugo-simple)
;;; ox-hugo-simple.el ends here
