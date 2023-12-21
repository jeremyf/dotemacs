;; Warning this `most-positive-fixnum' should not be the "resting" value.  After
;; we're done with initialization we want to set it to something more agreeable
;; (e.g. 20 MB or so)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Same idea as above for the `file-name-handler-alist' and the
;; `vc-handled-backends' with regard to startup speed optimisation.
;; Here I am storing the default value with the intent of restoring it
;; via the `emacs-startup-hook'.
(defvar jf-emacs--file-name-handler-alist file-name-handler-alist)
(defvar jf-emacs--vc-handled-backends vc-handled-backends)
(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 20)
                  gc-cons-percentage 0.2
                  file-name-handler-alist prot-emacs--file-name-handler-alist
                  vc-handled-backends prot-emacs--vc-handled-backends)))

;; From straight.el, "Users of Emacs versions >= 27 will want to add
;; the following:"
(setq package-enable-at-startup nil)
