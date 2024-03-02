;;; jf-experiments --- Where I put things that I'm exploring -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;;; Code

;; https://github.com/abo-abo/org-download
(use-package org-download
  :straight t
  :hook (dired-mode . org-download-enable))

;; (use-package parsebib
;;   :straight t)

;; (use-package ebib
;;   :straight t)

;; (use-package citar
;;   :custom (citar-bibliography '("~/git/org/bibliography.bib"))
;;   :straight t)

;; (use-package citar-denote
;;   :straight t)

;; https://github.com/ruediger/qrencode-el/
;;
;; Generate an plain text QRCode (or PNG but really why not use those UTF
;; characters)
(use-package qrencode
  :straight t)

;; ;; A package to browse/read reddit in emacs and `org-mode' format.
;; (use-package reddigg
;;   :straight (:host github :repo "jeremyf/emacs-reddigg")
;;   :config (setq reddigg--sub-url "https://www.reddit.com/r/%s/new.json?count=25")
;;   :custom (reddigg-subs '(emacs planetemacs orgmode wwn swn ruby rubyonrails)))

;; ;; Some customizations to the behavior of a reddit buffer.
;; (advice-add 'reddigg--ensure-modes
;;   :after (lambda ()
;;            ;; This seems like a good idea to limit behavior to only reddit.
;;            (setq-local org-confirm-elisp-link-function nil)
;;            ;; It's rather odd to consider adding GET request
;;            (read-only-mode)))

;; May as well make a menu for this experiment.
;; (defmacro jf/reddigg/create-view-function-for (sub)
;;   (let* ((fn (intern (concat "jf/reddigg/view-" sub)))
;;           (doc (concat "View /r/" sub))
;;           (desc (concat "/r/" sub)))
;;     `(transient-define-suffix ,fn ()
;;       ,doc
;;       :description ,desc
;;       (interactive)
;;        (reddigg-view-sub ,sub))))

;; (jf/reddigg/create-view-function-for "emacs")
;; (jf/reddigg/create-view-function-for "orgmode")
;; (jf/reddigg/create-view-function-for "planetemacs")
;; (jf/reddigg/create-view-function-for "ruby")
;; (jf/reddigg/create-view-function-for "rubyonrails")
;; (jf/reddigg/create-view-function-for "swn")
;; (jf/reddigg/create-view-function-for "wwn")

;; Removing as of <2023-08-05 Sat> as I don't use it and I was seein gmany
;; errors
;;
;; (use-package breadcrumb
;;   :straight (:host github :repo "joaotavora/breadcrumb")
;;   :config (breadcrumb-mode))

(defun toggle-transparency ()
  "Toggle on and off transparency.

I'm uncertain if this is useful/practical.  However there is
 literature regarding the benefits of transparency of files."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                 ((numberp (cdr alpha)) (cdr alpha))
                 ;; Also handle undocumented (<active> <inactive>) form.
                 ((numberp (cadr alpha)) (cadr alpha)))
            100)
        '(75 . 50) '(100 . 100)))))


;; Going to experiment a moment with this.

(setq gnus-select-method '(nntp "campaignwiki.org"))
;; (add-to-list 'gnus-secondary-select-methods '(nnml ""))


(defun jf/rancher/rm-then-vim-project-file (&optional filename)
  "Kill some text to edit a FILENAME in Rancher."
  (interactive)
  (let* ((f (or filename (buffer-file-name)))
          (relative-name (concat "./" (file-relative-name f (projectile-project-root)))))
    (kill-new (f-read f))
    (kill-new (format "rm %s ; vim %s" relative-name relative-name))))

;; (use-package eyebrowse
;;   :straight t)

;; (use-package org-timeblock
;;   :straight (org-timeblock :type git
;;               :host github
;;               :repo "ichernyshovvv/org-timeblock"))

(defun calculate-distance-to (to fun pred)
  "Calculate distance from BEG to END in units of FUN.
Assume BEG <= END.  FUN is a function moving forward by one unit
of measurement (e.g., a word or sentence)."
  (when to
    (save-excursion
      (let ((count 0))
        (while (funcall pred (point) to)
          (funcall fun)
          (cl-incf count))
        count))))

(defun find-nearest-word-repetitions ()
  "Find and report the nearest repetitions of word at point."
  (interactive)
  (let* ((word (word-at-point))
          (re (format "\\b%s\\b" (regexp-quote word)))
          (case-fold-search t)
          (prev (save-excursion
                  (beginning-of-thing 'word)
                  (when (re-search-backward re nil t)
                    (point))))
          (prev-overlay (when prev
                          (make-overlay prev
                            (save-excursion
                              (goto-char prev)
                              (forward-word)
                              (point)))))
          (next (save-excursion
                  (end-of-thing 'word)
                  (when (re-search-forward re nil t)
                    (point))))
          (next-overlay (when next
                          (make-overlay next
                            (save-excursion
                              (goto-char next)
                              (backward-word)
                              (point)))))
          (prev-words (save-excursion
                        (beginning-of-thing 'word)
                        (calculate-distance-to prev #'backward-word #'>)))
          (next-words (save-excursion
                        (end-of-thing 'word)
                        (calculate-distance-to next #'forward-word #'<)))
          (prev-sentences (save-excursion
                            (beginning-of-thing 'sentence)
                            (calculate-distance-to prev #'backward-sentence #'>)))
          (next-sentences (save-excursion
                            (end-of-thing 'sentence)
                            (calculate-distance-to next #'forward-sentence #'<))))
    (message "%s\n%s\n%s"
      (format "Word on point is `%s'." word)
      (if prev
        (format "The previous occurrence was %s word(s)/%s sentence(s) ago."
          prev-words prev-sentences)
        "This is the first occurrence.")
      (if next
        (format "The next occurrence will be in %s word(s)/%s sentence(s)."
          next-words next-sentences)
        "This is the last occurrence."))
    (when prev
      (overlay-put prev-overlay 'face 'show-paren-match)
      (run-at-time "4 sec" nil (lambda ()
                                 (delete-overlay prev-overlay))))
    (when next
      (overlay-put next-overlay 'face 'show-paren-match)
      (run-at-time "4 sec" nil (lambda ()
                                 (delete-overlay next-overlay))))))

;; https://macowners.club/posts/org-capture-from-everywhere-macos/
(defun jf/func-make-capture-frame ()
  "Create a new frame and run `org-capture'."
  (interactive)
  (make-frame '((name . "capture")
                 (top . 300)
                 (left . 700)
                 (width . 80)
                 (height . 25)))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (cl-flet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
    (org-capture)))

(defadvice org-capture-finalize
  (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
    (delete-frame)))

(defadvice org-capture-destroy
  (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
    (delete-frame)))

(use-package eat
  :straight (:type git
              :host codeberg
              :repo "akib/emacs-eat"
              :files ("*.el" ("term" "term/*.el") "*.texi"
                       "*.ti" ("terminfo/e" "terminfo/e/*")
                       ("terminfo/65" "terminfo/65/*")
                       ("integration" "integration/*")
                       (:exclude ".dir-locals.el" "*-tests.el"))))

(transient-define-prefix jf/isearch-menu ()
  "isearch Menu"
  [["Edit Search String"
     ("e"
       "Edit the search string (recursive)"
       isearch-edit-string
       :transient nil)
     ("w"
       "Pull next word or character word from buffer"
       isearch-yank-word-or-char
       :transient nil)
     ("s"
       "Pull next symbol or character from buffer"
       isearch-yank-symbol-or-char
       :transient nil)
     ("l"
       "Pull rest of line from buffer"
       isearch-yank-line
       :transient nil)
     ("y"
       "Pull string from kill ring"
       isearch-yank-kill
       :transient nil)
     ("t"
       "Pull thing from buffer"
       isearch-forward-thing-at-point
       :transient nil)]
    ["Replace"
      ("q"
        "Start ‘query-replace’"
        isearch-query-replace
        :if-nil buffer-read-only
        :transient nil)
      ("x"
        "Start ‘query-replace-regexp’"
        isearch-query-replace-regexp
        :if-nil buffer-read-only
        :transient nil)]]
  [["Toggle"
     ("X"
       "Toggle regexp searching"
       isearch-toggle-regexp
       :transient nil)
     ("S"
       "Toggle symbol searching"
       isearch-toggle-symbol
       :transient nil)
     ("W"
       "Toggle word searching"
       isearch-toggle-word
       :transient nil)
     ("F"
       "Toggle case fold"
       isearch-toggle-case-fold
       :transient nil)
     ("L"
       "Toggle lax whitespace"
       isearch-toggle-lax-whitespace
       :transient nil)]
    ["Misc"
      ("o"
        "occur"
        isearch-occur
        :transient nil)]])
(define-key isearch-mode-map (kbd "<f2>") #'jf/isearch-menu)

(provide 'jf-experiments)
;;; jf-experiments.el ends here
