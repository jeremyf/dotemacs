(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#fdf0ed" "#e95678" "#29d398" "#fadad1" "#26bbd9" "#ee64ac" "#26bbd9" "#403c3d"])
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(column-number-mode t)
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(custom-safe-themes
   '("7afad8f4707c84129e4cb1e5bad4feb0a9f0db02e1cfadb029921a0bde693d1e" "fd9e60866accaa68c8bede88623478e74b7c6e7c91765b0cdf11ddf04a5ff803" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "8607fdf417935af22922d10b4664a4ead5a64c01b55ac9e4eb9f4da9d177f612" "250268d5c0b4877cc2b7c439687f8145a2c85a48981f7070a72c7f47a2d2dc13" "23ba4b4ba4d1c989784475fed58919225db8d9a9751b32aa8df835134fe7ba6f" default))
 '(delete-selection-mode t)
 '(dired-listing-switches "-laGhpX")
 '(dired-use-ls-dired t)
 '(global-display-line-numbers-mode t)
 '(org-agenda-files
   '("~/git/org/denote/melange/20230212T112800--inventory-of-worlds-without-number-subsystems__rpgs.org" "/Users/jfriesen/git/org/agenda.org" "/Users/jfriesen/git/takeonrules.source/agenda.org" "/Users/jfriesen/git/org/denote/scientist/20221021T221357--scientist-agenda__scientist.org"))
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(safe-local-variable-values
   '((mode-line-bg-color-name . bg-red-intense)
     (mode-line-bg-color-name . bg-green-intense)
     (mode-line-bg-color-name . bg-yellow-intense)
     (mode-line-bg-color-name . bg-blue-intense)
     (mode-line-bg-color-name . bg-magenta-intense)
     (mode-line-bg-color-name . bg-cyan-intense)
     (mode-line-bg-color-name . bg-red-subtle)
     (mode-line-bg-color-name . bg-green-subtle)
     (mode-line-bg-color-name . bg-yellow-subtle)
     (mode-line-bg-color-name . bg-blue-subtle)
     (mode-line-bg-color-name . bg-magenta-subtle)
     (mode-line-bg-color-name . bg-cyan-subtle)
     (mode-line-bg-color-name . bg-red-nuanced)
     (mode-line-bg-color-name . bg-green-nuanced)
     (mode-line-bg-color-name . bg-yellow-nuanced)
     (mode-line-bg-color-name . bg-blue-nuanced)
     (mode-line-bg-color-name . bg-magenta-nuanced)
     (mode-line-bg-color-name . bg-cyan-nuanced)
     (projectile-require-project-root)
     (projectile-git-command . "git ls-files -zco --exclude-from=.projectile.gitignore")
     (org-insert-tilde-language . ruby)
     (org-insert-tilde-language . emacs-lisp)
     (jf/forem-minor-mode . 1)
     (jf/tor-minor-mode . 1)
     (encoding . utf-8)))
 '(show-paren-mode t)
 '(tmr-timer-finished-functions
   (list #'tmr-print-message-for-completed-timer #'tmr-sound-play #'jf/tmr-notification-notify) nil nil "Customized with use-package tmr")
 '(typopunct-buffer-language 'english)
 '(use-package-always-ensure t)
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(denote-faces-link ((t (:inherit link :box (:line-width (1 . 1) :color "grey75" :style released-button))))))
