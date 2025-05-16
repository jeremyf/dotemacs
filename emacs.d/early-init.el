;; A startup optimization with regard to startup speed optimisation.
;; Here I am storing the default value with the intent of restoring it
;; via the `emacs-startup-hook'.
(defvar jf-emacs--file-name-handler-alist file-name-handler-alist)
(defvar jf-emacs--vc-handled-backends vc-handled-backends)
(setq file-name-handler-alist nil
      vc-handled-backends nil)

;; From https://jackjamison.xyz/blog/emacs-garbage-collection/
(defun jf/minibuffer-setup-hook ()
  ;; With
  (setq gc-cons-threshold most-positive-fixnum))
(add-hook 'minibuffer-setup-hook 'jf/minibuffer-exit-hook)
(defun jf/minibuffer-exit-hook ()
  (setq gc-cons-threshold 256 * 1024 * 1024))
(add-hook 'minibuffer-exit-hook 'jf/minibuffer-exit-hook)
(setopt gc-cons-threshold most-positive-fixnum)
(run-with-idle-timer 1.2 t 'garbage-collect)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq
      file-name-handler-alist jf-emacs--file-name-handler-alist
      vc-handled-backends jf-emacs--vc-handled-backends)))

;; From straight.el, "Users of Emacs versions >= 27 will want to add
;; the following:"
(setopt package-enable-at-startup nil)
