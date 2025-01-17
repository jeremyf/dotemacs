(defun jf/rsync-files-to-cloud ()
  "Synchronize files to cloud storage."
  (interactive)
  (message "Synchronzing denote files to cloud...")
  (shell-command
    (concat "rsync -a " jf/denote-base-dir " "
      (file-truename "~/Library/CloudStorage/ProtonDrive-jeremy@jeremyfriesen.com-folder/denote/")
      " --exclude .git/&")
    "*rsync-denote-to-cloud*"
    "*rsync-denote-to-cloud*")

  ;; Only perform sync 20% of the time; as its somewhat expensive.
  (if (= 0 (random 5))
    (progn
      (message "Synchronizing elfeed database to cloud...")
      (shell-command
        (concat "tar -cvzf "
          (file-truename "~/Library/CloudStorage/ProtonDrive-jeremy@jeremyfriesen.com-folder/elfeed.tar.gz")
          " " elfeed-db-directory "&")
        "*rsync-elfeed-to-cloud*"
        "*rsync-elfeed-to-cloud*"))
    (message "Skipping elfeed database sync")))

;; Based on the idea of habit stacking, whenever I pull down my RSS
;; feed, I'll go ahead and sync my notes.
(advice-add #'jf/elfeed-load-db-and-open :before #'jf/rsync-files-to-cloud)
(add-hook 'after-init-hook #'jf/rsync-files-to-cloud)

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
