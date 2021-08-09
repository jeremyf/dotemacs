;;; elisp-experiments.el --- Summary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  This "package" contains experiments of practice.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Call the open-pdf-to-page with a known PDF.  The goal is to ensure
;; that the shell script works reasonably well.
(shell-command "open-pdf-to-page ~/Documents/RPGs/Warhammer-Fantasy-RPG-WHFRPG/CB72400-Warhammer-Fantasy-Roleplay-4E-WHFRPG.pdf")

;; In this function, I want to find an org file that has a matching
;; substring; which is how org-noter builds it's annotations.
;;
;; Find file with NOTER_DOCUMENT, this aligns with the org-noter
;; behavior.  Note, ripgrep exits with a non-zero if it fails to find
;; the file.
(shell-command "rg \":NOTER_DOCUMENT: WFRP-Enemy-in-the-Shadows-v-1.02.pdf\" ~/Documents/RPGs/Warhammer-Fantasy-RPG-WHFRPG/The-Enemy-within-Campaign/ --files-with-matches --glob \"*.org\"")

;; Write an org file to annotate the given filename
(let* ((filename "~/Documents/RPGs/Warhammer-Fantasy-RPG-WHFRPG/The-Enemy-within-Campaign/WFRP-Enemy-in-the-Shadows-v-1.02.pdf")
       (title (file-name-base filename))
       (annotation_file_path (concat "~/git/org/annotations/" (format-time-string "%Y%m%d---") (s-dashed-words title) ".org")))
  (write-region (concat
                 "#+title: " title
                 "\n#+roam_tags:"
                 "\n* " title
                 "\n  :PROPERTIES:"
                 "\n  :NOTER_DOCUMENT: " annotation_file_path
                 "\n  :END:"
                 "\n\n** Begin Topic"
                 "\n   :PROPERTIES:"
                 "\n   :NOTER_PAGE:"
                 "\n   :END:"
                 )
                nil (expand-file-name annotation_file_path) nil nil nil t)
  (find-file (expand-file-name annotation_file_path)))

;; I wanted to experiment with grabbing a random element from a list.
(defun randomElementFrom (list)
  "Get a random element from the given LIST."
  (nth (random (length list)) list))
(message "%s" (randomElementFrom (list "Hello" "World")))

;; I want a script that:
(let* ((dirs (list "~/git/org" "~/git/dotzshrc" "~/git/takeonrules.github.io" "~/git/takeonrules.github.io/themes/hugo-tufte")))
  (dolist (dir dirs)
    (magit-status dir)))


;; Begin experimenting with advice to pass templates to `org-roam-capture-'
;;
;; In this experiment, I manually add advice, then with that advice,
;; remove itself.
(defun jnf/org-roam-template-wrapper (org-roam-capture-function &rest rest)
  (plist-put rest :templates (jnf/org-roam-templates-for :thel-sector))
  (apply org-roam-capture-function rest)
  (advice-remove 'org-roam-capture- #'jnf/org-roam-template-wrapper))
(advice-add #'org-roam-capture- :around #'jnf/org-roam-template-wrapper)



(defun xah-filter-list (@predicate @sequence)
   "Return a new list such that @PREDICATE is true on all members of @SEQUENCE.
URL `http://ergoemacs.org/emacs/elisp_filter_list.html'
Version 2016-07-18"
   (delete
    "e3824ad41f2ec1ed"
    (mapcar
     (lambda ($x)
       (if (funcall @predicate $x)
           $x
         "e3824ad41f2ec1ed" ))
     @sequence)))

(provide 'elisp-experiments.el)
;;; elisp-experiments.el ends here
