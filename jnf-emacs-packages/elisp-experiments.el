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


(defmacro quick-help (name buffer text)
  "Macro for creating callable functions that display help.
Where NAME is name of function, BUFFER is name of buffer, and TEXT is displayed."
  (declare (indent defun))
  `(progn
     (defun ,name nil
       ,buffer
       (interactive)
       (let ((qh-buff (concat "*Quick Help: " ,buffer "*"))
             (qh-text ,text))
         (get-buffer-create qh-buff)
         (with-current-buffer qh-buff
           (insert qh-text)
           (goto-char (point-min))
           (not-modified)
           (read-only-mode)
           (special-mode)
           (local-set-key (kbd "C-g") (lambda () (interactive) (other-window -1)))
           (local-set-key (kbd "q") 'kill-buffer-and-window))
         (pop-to-buffer qh-buff '((display-buffer-below-selected)
                                  (window-parameters . ((no-other-window . nil)))
                                  (window-height . fit-window-to-buffer)))
         (message "C-g - Previous Window, q - Remove Window")))))

(quick-help qh--it-hotline
  "IT Hotline"
  "IT HOTLINE: 855-555-5555")


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




(provide 'elisp-experiments.el)
;;; elisp-experiments.el ends here
