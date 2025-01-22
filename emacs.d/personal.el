(defun jf/syncthing-aling ()
  "Synchronize files into SyncThing bucket."
  (interactive)
  ;; Ensure we have our queue and our ready location
  (mkdir (file-truename "~/SyncThings/queue") t)
  (mkdir (file-truename "~/SyncThings/source") t)
  (message "Synchronzing denote files to cloud...")
  ;; There's a 1 in 10 chance that we'll perform the sync.  Toss that d10.
  (if (= 0 (random 10))
    (progn
      (message "Syncing elfeed database...")
      ;; We tar zip into one directory (our queue) and then move that
      ;; file in its complete state into the ready directory.  As move
      ;; is instantaneous and we don't need to worry about syncthing
      ;; picking up a partially completed file.
      (shell-command
        (concat "tar -cvzf "
          (file-truename "~/SyncThings/queue/elfeed.tar.gz")
          " " elfeed-db-directory " && mv -f ~/SyncThings/queue/elfeed.tar.gz ~/SyncThings/source&")
        "*syncthing-aling*"
        "*syncthing-aling*"))
    (message "I'll get you next time Gadget")))

;; Based on the idea of habit stacking, whenever I pull down my RSS
;; feed, I'll go ahead and sync my notes.
(advice-add #'jf/elfeed-load-db-and-open :before #'jf/syncthing-aling)
(add-hook 'after-init-hook #'jf/syncthing-aling)

(use-package tp
  :straight (:host codeberg :repo "martianh/tp.el")
  :when (file-exists-p (expand-file-name "~/.my-computer")))
(use-package mastodon
  ;; :straight (:host codeberg :repo "martianh/mastodon.el")
  :when (file-exists-p (expand-file-name "~/.my-computer"))
  :custom
  (mastodon-tl--timeline-posts-count "50")
  :preface
  (setq
    mastodon-instance-url "https://dice.camp"
    mastodon-active-user "takeonrules")

  :config
  (defun jf/mastodon-tl--reading (&optional prefix)
    "Read my reading timeline.

Forward PREFIX to `mastodon-tl--show-tag-timeline'."
    (interactive "p")
    (mastodon-tl--show-tag-timeline
      prefix
      '("reading" "books" "booktok"
         "literature" "libraries" "bookstodon"
         "writing" "library")))

  (defun jf/mastodon-tl--insert-status (&rest args)
    "A little hack to help narrow region."
    (insert "🐘  ·  ·  ·  ·  ·  ·  ·"))
  (advice-add 'mastodon-tl--insert-status
    :before #'jf/mastodon-tl--insert-status))

(add-to-list 'org-capture-templates
  `("j" "Start Today's Journal Entry"
     plain (file+olp+datetree
             ,(f-join jf/denote-base-dir "private/20241114T075414--personal-journal__personal_private.org"))
     "[[date:%<%Y-%m-%d>][Today:]]\n\n- [ ] Read one book chapter\n- [ ] Read one poem\n- [ ] Read one essay\n- [ ] Tend my daily feed\n- [ ] Write one response to a feed item\n\n%?"
     :empty-lines-before 1
     :empty-lines-after 1
     :clock-in t
     :clock-resume t))

(add-to-list 'org-capture-templates
  `("J" "Add to Today's Journal Entry"
     plain (file+olp+datetree
             ,(f-join jf/denote-base-dir "private/20241114T075414--personal-journal__personal_private.org"))
     "%?"
     :empty-lines-before 1
     :empty-lines-after 1
     :clock-in t
     :clock-resume t))
