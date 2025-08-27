;;; dig-my-grave --- Expand triple backtick/grave character -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; License

;; Copyright 2023 Jeremy Friesen <jeremy@jeremyfriesen.com>
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;    http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; I used to write a lot of Markdown.  And the triple grave character
;; (e.g. “```”) is a wonderful convenience for indicating code blocks.  This
;; package builds on that idea and repurposes the triple backtick to a quick
;; prompt that feels idiomatic to Markdown's triple backtick.

;;; Code:

(require 'org)

(defvar dig-my-grave/templates-alist/org-mode
  '(("Bash" . "#+begin_src bash :results scalar replace :exports both :tangle yes\n#+end_src")
     ("Blockquote" . jf/org-mode/insert-block/quote_block)
     ("Details" . tempel-insert-details_block)
     ("Emacs Lisp" . "#+begin_src emacs-lisp\n#+end_src")
     ("Gherkin" . "#+begin_src gherkin\n#+end_src")
     ("Go Lang" . "#+begin_src go-ts\n#+end_src")
     ("Org Structure" . org-insert-structure-template)
     ("Elixir" . "#+begin_src elixir-ts\n#+end_src")
     ("Plant UML (puml)" . "#+begin_src plantuml\n@startuml\n!theme amiga\n\n@enduml\n#+end_src")
     ("Ruby" . "#+begin_src ruby\n#+end_src")
     ("Sudo" . "#+begin_src shell :dir \"/sudo::/\" :cache no :export source :results raw silent\n#+end_src")
     ("Update" . tempel-insert-update_block)
     ("Verb" . tempel-insert-verb_block))

  "A list of `cons' cells used for `dig-my-grave' `completing-read'.
The `car' as the label and `cdr' as the value that we'll insert.")

(defun jf/org-mode/insert-block/quote_block (author cite cite_url)
  (interactive (list (read-string "Author: ")
                 (read-string "Cite: ")
                 (read-string "Cite URL: ")))
  (insert
    "\n#+begin_quote"
    (if (s-present? author) (concat " :pre " author) "")
    (if (s-present? cite) (concat " :cite " cite) "")
    (if (s-present? cite_url) (concat " :cite_url " cite_url) "")
    "\n\n#+end_quote")
  (re-search-backward "^$"))

(define-key org-mode-map (kbd "`") #'dig-my-grave)
(defun dig-my-grave ()
  "Prompt to `insert' block when 3 consecutive graves (e.g. “`”) start line.

See `dig-my-grave/templates-alist/org-mode'."
  (interactive)
  (if (and (= (current-column) 2) (looking-back "``" (- (point) 2)))
    ;; We have just hit our third backtick at the beginning of the line.
    (progn
      (delete-char -2)
      ;; I use the alist-get pattern a lot...perhaps a function?
      (let ((value (alist-get (completing-read "Block Type: "
                                  dig-my-grave/templates-alist/org-mode nil t)
                     dig-my-grave/templates-alist/org-mode nil nil #'string=)))
        (cond
          ;; Let's assume that we're dealing with registered org blocks.
          ((stringp value)
            (insert value) (forward-line -1) (org-edit-special))
          ;; Trust the function
          ((commandp value) (call-interactively value))
          ((functionp value) (funcall value))
          ;; Time for a pull request
          (t (error "Unprocessable value %s for #'dig-my-grave" value)))))
    (setq last-command-event ?`)
    (call-interactively #'org-self-insert-command)))

(provide 'dig-my-grave)
;;; dig-my-grave.el ends here
