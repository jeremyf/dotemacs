(defun jf/rsync-files-to-cloud ()
  "Synchronize files to cloud storage."
  (interactive)
  (message "Synchronzing denote files to cloud...")
  (shell-command
    (concat "rsync -a " jf/denote-base-dir " "
      (file-truename "~/Library/CloudStorage/ProtonDrive-jeremy@jeremyfriesen.com-folder/denote/")
      " --exclude .git/&"))
  (message "Synchronizing elfeed database to cloud...")
  (shell-command
    (concat "rsync -a " elfeed-db-directory " "
      (file-truename "~/Library/CloudStorage/ProtonDrive-jeremy@jeremyfriesen.com-folder/.elfeed/")
       " --exclude .git/&")))

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
