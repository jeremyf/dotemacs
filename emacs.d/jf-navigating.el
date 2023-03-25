;;; jf-navigating.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;;; Code

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
  :bind (("C-j" . avy-goto-char-2))
  :straight t)

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
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))

;;;; Custom Functions
;; (defun jf/scroll-down-half-page ()
;;   "Scroll down half a page while keeping the cursor centered"
;;   ;; See https://www.reddit.com/r/emacs/comments/r7l3ar/how_do_you_scroll_half_a_page/
;;   (interactive)
;;   (let ((ln (line-number-at-pos (point)))
;;         (lmax (line-number-at-pos (point-max))))
;;     (cond ((= ln 1) (move-to-window-line nil))
;;           ((= ln lmax) (recenter (window-end)))
;;           (t (progn
;;                (move-to-window-line -1)
;;                (recenter))))))

;; (defun jf/scroll-up-half-page ()
;;   "Scroll up half a page while keeping the cursor centered"
;;   ;; See https://www.reddit.com/r/emacs/comments/r7l3ar/how_do_you_scroll_half_a_page/
;;   (interactive)
;;   (let ((ln (line-number-at-pos (point)))
;;         (lmax (line-number-at-pos (point-max))))
;;     (cond ((= ln 1) nil)
;;           ((= ln lmax) (move-to-window-line nil))
;;           (t (progn
;;                (move-to-window-line 0)
;;                (recenter))))))


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

(provide 'jf-navigating)
;;; jf-navigating.el ends here
