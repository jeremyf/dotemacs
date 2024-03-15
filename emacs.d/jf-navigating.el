;;; jf-navigating.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;;; Code:

;;;; Packages
(use-package ace-window
  ;; Quick navigation from window to window.
  :straight t
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("C-x o" . ace-window)
          ("M-o" . ace-window)))

(use-package avy
  ;; Pick a letter, avy finds all words with that at the beginning of it.  Narrow
  ;; results from there.
  :bind (("C-j" . avy-goto-char-timer))
  :straight t
  :config
  (setq avy-dispatch-alist
    '((?. . jf/avy-action-embark)
       (?x . avy-action-kill-move)
       (?X . avy-action-kill-stay)
       (?y . avy-action-yank)
       (?Y . jf/avy-action-yank-whole-line)
       (?w . avy-action-copy)
       (?W . jf/avy-action-copy-whole-line)
       (?k . avy-action-kill)
       (?K . jf/avy-action-kill-whole-line)
       (?t . avy-action-teleport)
       (?T . jf/avy-action-teleport-whole-line))))

;; https://karthinks.com/software/avy-can-do-anything/#remembering-to-avy
(defun jf/avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
    (cdr
      (ring-ref avy-ring 0)))
  t)

(defun jf/avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
      (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
    (cdr
      (ring-ref avy-ring 0)))
  t)

(defun jf/avy-action-yank-whole-line (pt)
  (jf/avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

(defun jf/avy-action-teleport-whole-line (pt)
  (jf/avy-action-kill-whole-line pt)
  (save-excursion (yank)) t)

(defun jf/avy-action-embark (pt)
  (unwind-protect
    (save-excursion
      (goto-char pt)
      (embark-act))
    (select-window
      (cdr (ring-ref avy-ring 0))))
  t)

(use-package browse-at-remote
  ;; Because I sometimes want to jump to the source code.  And in looking at
  ;; this I learned about vc-annotate; a better blame than what I've had before.
  ;; `bar-browse' is faster than `browse-at-remote'.
  :straight t
  :bind
  ;; Note this is in the same prefix space as `link-hint'
  ("C-c l r" . browse-at-remote)
  ("C-c l a" . vc-annotate)
  ("C-c l n" . jf/project/jump-to/notes)
  ("C-c l t" . git-timemachine))

(use-package imenu-list
  ;; Show an outline summary of the current buffer.
  :custom (imenu-list-focus-after-activation t)
  (imenu-list-size 0.4)
  (imenu-list-position 'right)
  :bind ("s-4" . 'imenu-list-smart-toggle)
  :bind (:map imenu-list-major-mode-map ("o" . 'imenu-list-goto-entry))
  :straight t)

(use-package link-hint
  ;; I use this more and more and more.  Invoking `link-hint-open-link'
  ;; highlights the visible URLs, providing quick keys to then open those URLs.
  ;; If there's only one candidate, the function opens that URL.
  :straight t
  :bind
  ;; Note this is in the same prefix space as `browse-at-remote'
  ("C-c l o" . jf/link-hint-open-link)
  ("C-c l c" . link-hint-copy-link)
  :preface
  (defun jf/link-hint-open-link (prefix)
    "Hint at then browse a URL.

When given PREFIX use `eww-browse-url'."
    (interactive "P")
    (let ((browse-url-browser-function
            (if prefix #'eww-browse-url browse-url-browser-function)))
      (link-hint-open-link))))

;; https://github.com/baron42bba/.emacs.d/blob/master/bba.org
(defvar jf/bracket/brackets nil "String of left/right brackets pairs.")
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
  "List of right bracket chars.")
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

(provide 'jf-navigating)
;;; jf-navigating.el ends here
