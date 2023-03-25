;;; jf-experiments --- Where I put things that I'm exploring -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;;; Code

;; From https://www.reddit.com/r/emacs/comments/10qo7vb/comment/j73idup/
;; (defvar my/consult-buffer-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "\C-k" #'my/consult-buffer-kill)
;;     map))
;; (consult-customize consult-buffer :keymap my/consult-buffer-map)

;; (defun my/consult-buffer-kill ()
;;   "In consult-buffer kill the current candidate"
;;   (interactive)
;;   (let ((marker (string #x200002)) ;; probably some internal detail :(
;;         (candidate (vertico--candidate)))
;;     (when (s-ends-with? marker candidate)
;;       (kill-buffer (s-replace marker "" candidate))
;;       (vertico-next))))

;; https://github.com/abo-abo/org-download
(use-package org-download
  :straight t
  :hook (dired-mode . org-download-enable))

(use-package parsebib
  :straight t)

(use-package ebib
  :straight t)

(use-package citar
  :custom (citar-bibliography '("~/git/org/bibliography.bib"))
  :straight t)

(use-package citar-denote
  :straight t)

;; https://github.com/baron42bba/.emacs.d/blob/master/bba.org
(defvar jf/bracket/brackets nil "string of left/right brackets pairs.")
(setq jf/bracket/brackets "()[]{}<>（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩｟｠⧘⧙⧚⧛⸜⸝⸌⸍⸂⸃⸄⸅⸉⸊᚛᚜༺༻༼༽⏜⏝⎴⎵⏞⏟⏠⏡﹁﹂﹃﹄︹︺︻︼︗︘︿﹀︽︾﹇﹈︷︸")

(defvar jf/bracket/left-brackets '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" )
  "List of left bracket chars.")

(progn
  ;; make jf/bracket/left-brackets based on jf/bracket/brackets
  (setq jf/bracket/left-brackets '())
  (dotimes (-x (- (length jf/bracket/brackets) 1))
    (when (= (% -x 2) 0)
	    (push (char-to-string (elt jf/bracket/brackets -x))
        jf/bracket/left-brackets)))
  (setq jf/bracket/left-brackets (reverse jf/bracket/left-brackets)))

(defvar jf/bracket/right-brackets '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»")
  "list of right bracket chars.")
(progn
  (setq jf/bracket/right-brackets '())
  (dotimes (-x (- (length jf/bracket/brackets) 1))
    (when (= (% -x 2) 1)
	    (push (char-to-string (elt jf/bracket/brackets -x))
        jf/bracket/right-brackets)))
  (setq jf/bracket/right-brackets (reverse jf/bracket/right-brackets)))

(defun jf/bracket/backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
 The list of brackets to jump to is defined by `jf/bracket/left-brackets'.
 URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
 Version 2015-10-01"
  (interactive)
  (search-backward-regexp (regexp-opt jf/bracket/left-brackets) nil t))

(defun jf/bracket/forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
 The list of brackets to jump to is defined by `jf/bracket/right-brackets'.
 URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
 Version 2015-10-01"
  (interactive)
  (re-search-forward (regexp-opt jf/bracket/right-brackets) nil t))

(define-key global-map (kbd "C-c C-<left>") 'jf/bracket/backward-left-bracket)
(define-key global-map (kbd "C-c C-<right>") 'jf/bracket/forward-right-bracket)

(provide 'jf-experiments)
;;; jf-experiments.el ends here
