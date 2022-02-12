;;; -*- lexical-binding: t; -*-
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
(let* ((dirs (list "~/git/org" "~/git/dotzshrc" "~/git/takeonrules.source" "~/git/takeonrules.source/themes/hugo-tufte")))
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


;; If the system-name is that of the machine provided by Hesburgh Libraries.
(if (string-equal system-name "lib-2601")
    (message "%s" system-name))


(use-package qrencode
  :straight (qrencode :host github :type git :repo "ruediger/qrencode-el"))

(use-package edraw
  :straight (edraw :host github :type git :repo "misohena/el-easydraw"))

(with-eval-after-load 'org
  (require 'edraw-org)
  (edraw-org-setup-default))

(use-package tmr
  :straight (tmr :host gitlab :type git :repo "protesilaos/tmr.el"))

;; Edit in Emacs
(use-package emacs-everywhere
  :straight t)


(defun kf-display-command-output (command)
  "Display output of COMMAND."
  (interactive "sCommand: ")
  (let ((cbuf (get-buffer-create "*Output*")))
    (set-buffer cbuf)
    (erase-buffer)
    (insert "\n")
    (goto-char (point-min))
    (shell-command command t)
    (kf-display-buffer cbuf)))

(defun kf-display-buffer (buffer)
  "Display BUFFER in a size-appropriate way."
  (display-buffer buffer)
  (shrink-window-if-larger-than-buffer (get-buffer-window buffer)))

(defun kf-ps ()
  "Show processes.  Hit any key to make the window go away.
 The character typed is treated normally, not lost, by the way."
  (interactive)
  (kf-display-command-output "ps -ax"))
(if (not (fboundp 'ps)) (defalias 'ps 'kf-ps))

(defun kf-browse-kill-ring ()
  "Browse the kill ring."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Browse Kill Ring*"))
  (widen)
  (delete-region (point-min) (point-max))
  (mapcar
   (lambda (str)
     ;; We could put the full string as a text property on the summary
     ;; text displayed, but with yank-match available, there's no need.
     (insert (substring str 0 (min (1- (length str)) 72))
             "\n-*- -*- -*- -*- -*-\n"))
   kill-ring)
  (goto-char (point-min)))


(defun kf-🧵 ()
  "Insert a thread (spool of thread) emoji.  See also
https://twitter.com/jenny8lee/status/1189751069913411589."
  (interactive)
  (insert ?🧵)) ; 129525
(defalias 'kf-thread 'kf-🧵)

(defun kf-checkbox (parg)
  "Insert a checkbox.
With one prefix arg, insert a checked checkbox.
With two prefix args, insert an x'ed checkbox."
  (interactive "P")
  (let ((prefix (car parg)))
    (cond
     ((not prefix)  (insert ?☐)) ; 9744
     ((= prefix 4)  (insert ?☑)) ; 9745
     ((= prefix 16) (insert ?☒)) ; 9746
     (t (error "What do you want me to put in that checkbox?")))))


(defun kf-arrow (type)
  "Insert an arrow of TYPE, where type is a single letter:
    - \"[u]p\"
    - \"[d]own\"
    - \"[l]eft\"
    - \"[r]ight\"
    - \"[h]orizontal double arrow\"
    - \"[v]ertical double arrow\""
  (interactive
   "cArrow type ([u]p, [d]own, [l]eft, [r]ight, [h]oriz, [v]ert): ")
    (insert (cdr (assoc type '((?u . ?↑)
                               (?d . ?↓)
                               (?l . ?←)
                               (?r . ?→)
                               (?h . ?↔)
                               (?v . ?↕)
                               )))))

;; - Unless point is ~[~ `(isearch-backward "[")'
;; - Set mark
;; - `(search-forward "]")'
;; - `(search-forward ")")'
;; - `kill-region'
;; - let string `car-of-kill-ring'
;; - string replace "^[" ""
;; - string replace ")$" ""
;; - let fragments split "]("
;; - concat "<a href=\"" (car(cdr fragments)) "\">" (car fragments) "</>"
;; - write to buffer ~<a href="url">text</a>~.
(defun jnf/convert-markdown-link-to-html-a-tag ()
  (interactive)
  (unless (eq ?[ (char-after))
    (search-backward "[")))
  (activate-mark)
  (search-forward "]")
  (search-forward ")")
  (kill-region))
;; (kill-region)


(defun in-range (n min max)
  (memq n (number-sequence min max)))

(defun d20-ability-modifier (n)
  (cond
   ((in-range n 2 3) -4)
   ((in-range n 4 5) -3)
   ((in-range n 6 7) -2)
   ((in-range n 8 9) -1)))

(message "%s" (jnf/lookup 3 table/swn/ability-modifier))

(use-package org-lookup-dnd
  :straight t)

(use-package org-pdftools
  :straight t)


(defun jf/entry-evaluator (entry)
  (if (stringp entry)
      entry
    (jf/entry-evaluator (cddr (seq-random-elt entry)))))

;; What we have here is a modification of the tabular behavior
;;
;; The seq-random-elt is "rolling" on the table
;; The `cddr' gets the 3rd+ elements in the list
(defvar modifier '((1 1 "less")
		   (2 2 "more")))
(defvar suffix '((1 1 "Wise")
		 (2 2 "Filth" modifier)))
(let* ((table '((1 2 "Sam" modifier)
		(3 4 "Frodo")))
       (result (jf/entry-evaluator table)))
  (message "%s" result))
  (message "%s" (s-join "" (seq-map #'jf/entry-evaluator result))))
  (message "%s" ))
(message "%s" (cddr '(3 4 "Frodo")))



;; Macro to kill a markdown link
;; {M-x search-backward RET [ C-d RET C-SPC M-x search-forward RET ] RET M-x search-forward RET ) RET  S-<delete>}
