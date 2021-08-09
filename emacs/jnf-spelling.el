;;; -*- lexical-binding: t; -*-
;;; package --- Summary
;;
;;; Commentary:
;;
;;  This package provides configuration for spell checking.  It
;;  assumes the use of ivy; See the flyspell-correct-ivy package.
;;
;;  By default, I don't render hints for spell checking nor grammar
;;  checking.  Instead I rely on two checks:
;;
;;  1. flyspell-buffer (via kbd "C-,")
;;  2. writegood-mode (via kbd "C-c w")
;;
;;  Once I've called flyspell-buffer, I can then navigate through
;;  identified issues and address them via flyspell-popup-correct (via
;;  kbd "C-").
;;
;;  For writegood-mode, I
;;
;;; Code:
(use-package flycheck
  :straight t
  :hook
  (org-src-mode . disable-flycheck-for-elisp)
  :custom
  (flycheck-display-errors-delay 0.1)
  (flycheck-emacs-lisp-initialize-packages t)
  :config
  (flycheck-set-indication-mode 'left-margin)

  (defun disable-flycheck-for-elisp ()
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

  (add-to-list 'flycheck-checkers 'proselint))
(add-hook 'after-init-hook #'global-flycheck-mode)

(use-package flyspell-correct
  :straight t
  )

(eval-when-compile (require 'cl))

(defun append-aspell-word (new-word)
  "Append the given NEW-WORD to the current dictionary, and reload the dictionary."
 (let ((header "personal_ws-1.1")
       (file-name (substitute-in-file-name "$HOME/.aspell.en.pws"))
       (read-words (lambda (file-name)
                    (let ((all-lines (with-temp-buffer
                                      (insert-file-contents file-name)
                                      (split-string (buffer-string) "\n" t))))
                     (if (null all-lines)
                       ""
                      (split-string (mapconcat 'identity (cdr all-lines) "\n")
                                    nil
                                    t))))))
  (when (file-readable-p file-name)
   (let* ((cur-words (eval (list read-words file-name)))
          (all-words (delq header (cons new-word cur-words)))
          (words (delq nil (remove-duplicates all-words :test 'string=))))
    (with-temp-file file-name
     (insert (concat header
                     " en "
                     (number-to-string (length words))
                     "\n"
                     (mapconcat 'identity (sort words #'string<) "\n"))))))
  (unless (file-readable-p file-name)
   (with-temp-file file-name
    (insert (concat header " en 1\n" new-word "\n")))))
 (ispell-kill-ispell t) ; restart ispell
 (flyspell-mode))

(defun append-current-word-to-aspell-dictionary ()
 "Add current word to aspell dictionary."
 (interactive)
 (append-aspell-word (thing-at-point 'word)))

(setq ispell-program-name "aspell"
      ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
      ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))

;; Run flyspell-buffer
(use-package flyspell-popup
  :straight t
  :hook (flyspell-mode . flyspell-popup-auto-correct-mode))

;; For grammar nerd
(use-package writegood-mode
  :straight t
  :bind ("C-c w" . writegood-mode)
  :config
  (add-to-list 'writegood-weasel-words "actionable"))
(provide 'jnf-spelling.el)
;;; jnf-spelling.el ends here
