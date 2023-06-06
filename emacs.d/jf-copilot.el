;;; jf-copilot --- A wrapper for Copilot -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;; Based on https://github.com/rksm/copilot-emacsd/blob/master/init.el

;;; Code:

(defun jf/copilot-tab ()
  "Tab command that will complete with copilot if a completion is
available. Otherwise will try company, yasnippet or normal
tab-indent."
  (interactive)
  (or (copilot-accept-completion)
      (indent-for-tab-command)))

(defun jf/copilot-complete-or-accept ()
  "Command that either triggers a completion or accepts one if one
is available. Useful if you tend to hammer your keys like I do."
  (interactive)
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion)
        (open-line 1)
        (next-line))
    (copilot-complete)))

(defun jf/copilot-quit ()
  "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
cleared, make sure the overlay doesn't come back too soon."
  (interactive)
  (condition-case err
      (when copilot--overlay
        (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
          (setq copilot-disable-predicates (list (lambda () t)))
          (copilot-clear-overlay)
          (run-with-idle-timer
           1.0
           nil
           (lambda ()
             (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
    (error handler)))

(defun jf/copilot-complete-if-active (next-func n)
  (let ((completed (when copilot-mode (copilot-accept-completion))))
    (unless completed (funcall next-func n))))

(defun jf/no-copilot-mode ()
  "Helper for `jf/no-copilot-modes'."
  (copilot-mode -1))

(defvar jf/no-copilot-modes '(shell-mode
                              inferior-python-mode
                              eshell-mode
                              term-mode
                              vterm-mode
                              comint-mode
                              compilation-mode
                              debugger-mode
                              dired-mode-hook
                              compilation-mode-hook
                              flutter-mode-hook
                              minibuffer-mode-hook)
  "Modes in which copilot is inconvenient.")

(defvar jf/copilot-manual-mode nil
  "When `t' will only show completions when manually triggered, e.g. via M-C-<return>.")

(defvar jf/copilot-enable-for-org nil
  "Should copilot be enabled for org-mode buffers?")

(defun jf/copilot-enable-predicate ()
  ""
  (and
   (eq (get-buffer-window) (selected-window))))

(defun jf/copilot-disable-predicate ()
  "When copilot should not automatically show completions."
  (or jf/copilot-manual-mode
      (member major-mode jf/no-copilot-modes)
      (and (not jf/copilot-enable-for-org) (eq major-mode 'org-mode))))

(defun jf/copilot-change-activation ()
  "Switch between three activation modes:
- automatic: copilot will automatically overlay completions
- manual: you need to press a key (M-C-<return>) to trigger completions
- off: copilot is completely disabled."
  (interactive)
  (if (and (boundp 'copilot-mode) copilot-mode  jf/copilot-manual-mode)
      (progn
        (message "deactivating copilot")
        (global-copilot-mode -1)
        (setq jf/copilot-manual-mode nil))
    (if (and (boundp 'copilot-mode) copilot-mode)
        (progn
          (message "activating copilot manual mode")
          (setq jf/copilot-manual-mode t))
      (message "activating copilot mode")
      (global-copilot-mode))))

;; https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/#tab-key
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :commands (copilot-login copilot-mode)
  :bind (:map copilot-mode-map (("C-M-<down>" .  copilot-next-completion)
                                 ("C-M-<up>" . copilot-previous-completion)
                                 ("C-M-<right>" . copilot-accept-completion-by-word)))
  :bind (("C-M-<return>" . jf/copilot-complete-or-accept)
          ("C-M-<escape>" . jf/copilot-change-activation))
  :config   ;; Do copilot-quit when pressing C-g
  (advice-add 'keyboard-quit :before #'jf/copilot-quit)

  ;; complete by pressing right or tab but only when copilot completions are
  ;; shown. This means we leave the normal functionality intact.
  (advice-add 'right-char :around #'jf/copilot-complete-if-active)
  (advice-add 'indent-for-tab-command :around #'jf/copilot-complete-if-active)

  ;; deactivate copilot for certain modes
  (add-to-list 'copilot-enable-predicates #'jf/copilot-enable-predicate)
  (add-to-list 'copilot-disable-predicates #'jf/copilot-disable-predicate)
  :ensure t)

(provide 'jf-copilot)
;;; jf-copilot.el ends here
