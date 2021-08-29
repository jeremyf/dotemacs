;;; jnf-stars-without-number.el --- Summary
;;; -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  Some useful tools for Stars without Number.
;;
;;  This relies on the `swnt' command line tool (see
;;  https://github.com/nboughton/swnt).
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-roam-project-filename (title &optional project)
  "Convert the given TITLE to a filename.

PROJECT defaults to \"thel-sector\".

The filename is conformant to my org-roam capture templates."
  (f-join org-directory
          "projects"
          "thel-sector"
          (concat
           (format-time-string "%Y%m%d---")
           (s-snake-case title) ".org")))

;; TODO - Can this be shifted to an org-roam capture template?
(defun swn-npc (culture &optional project)
  "Capture a random `swnt' npc of the prompted CULTURE.

This function writes to the as-of-now hard-coded Thel Sector
project in my org-directory."

  ;; Prompt for a culture that will be used as the basis for the
  ;; random name.
  (interactive (list
                (completing-read
                 "Culture: " '(
                               ("Arabic" 1)
                               ("Chinese" 2)
                               ("English" 3)
                               ("Greek" 4)
                               ("Indian" 5)
                               ("Japanese" 6)
                               ("Latin" 7)
                               ("Nigerian" 8)
                               ("Russian" 9)
                               ("Spanish" 10)))))
  (let* (
         ;; Get a list of the lines output from
         ;; swnt's command.
         (npc-string-list (split-string-and-unquote
                           (shell-command-to-string
                            (concat
                             "swnt new npc "
                             "--is-patron "
                             "--culture "
                             culture))
                           "\n"))
         ;; Extract the NPCs name
         (npc-name (string-trim
                    (replace-regexp-in-string
                     ":"
                     ""
                     (car npc-string-list))))
         ;; Build the document's body, conforming
         ;; to org-mode format.
         (body (concat
                "#+title: " npc-name
                "\n#+roam_tags: npc"
                "\n\n* " npc-name
                "\n\n** Summary"
                "\n\n** Details"
                "\n\n** COMMENT For Referee"
                "\n\n" (string-join
                        npc-string-list "\n")))
         ;; Get the path to the file
         (fpath (org-roam-project-filename npc-name)))

    ;; Write the body to the file at the FPATH.
    (write-region body nil fpath nil nil nil t)

    ;; Insert at point an org-mode link to
    ;; the randomly generated NPC.
    (insert (concat
             "[[file:" fpath "]["
             npc-name "]]"))

    ;; Rebuild the org-roam cache, as I've just added an NPC.  This is
    ;; a kludge, as I'm treating org-roam as a black box.  My
    ;; preference would be to avoid rebuilding the cache, but instead
    ;; rely on the inner workings of org-roam to do this work.  If I
    ;; go the path of an org-roam capture template, that would work.
    (org-roam-db-build-cache)))

;; Given that I'm in an org-mode context, then the following kbd
;; combination will prompt to generate a random SWN npc.
(define-key org-mode-map (kbd "C-c s n") 'swn-npc)
(defalias 'org-roam-insert-random-thel-sector-npc 'swn-npc)

(provide 'jnf-stars-without-number.el)
;;; jnf--stars-without-number.el ends here