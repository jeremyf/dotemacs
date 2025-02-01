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

;; A cautionary tale in regards to using this function.  If I have an
;; org-mode file and I export it to markdown, that markdown file will be
;; what we pick-up.
(defvar jf/personal/filename-for-journal
  (denote-get-path-by-id "20241114T075414")
  "Where I put my journal.")

(add-to-list 'org-capture-templates
  '("j" "Start Today's Journal Entry"
     plain (file+olp+datetree
             jf/personal/filename-for-journal)
     "[[date:%<%Y-%m-%d>][Today:]]\n\n- [ ] Read one book chapter\n- [ ] Read one poem\n- [ ] Read one essay\n- [ ] Tend my daily feed\n- [ ] Write one response to a feed item\n\n%?\n\n**** Private Thoughts :private:\n:PROPERTIES:\n:END:\n"
     :empty-lines-before 1
     :empty-lines-after 1
     :clock-in t
     :clock-resume t))

(add-to-list 'org-capture-templates
  `("J" "Add to Today's Journal Entry"
     plain (file+olp+datetree
             jf/personal/filename-for-journal)
     "%T :: %?"
     :empty-lines-before 1
     :empty-lines-after 1
     :clock-in t
     :clock-resume t))

(add-to-list 'org-capture-templates
  `("v" "Add to Today's Private Thoughts"
     plain (file+function
             jf/personal/filename-for-journal
             jf/personal/position-in-private-thoughts)
     "%T :: %?"
     :empty-lines-before 1
     :empty-lines-after 1
     :clock-in t
     :clock-resume t))

(add-to-list 'org-capture-templates
  `("b" "Blogroll"
     entry (file+headline
             "~/git/org/denote/indices/private-elfeed.org"
             "Private Blog Roll")
     "%^{URL} %^g"
     :empty-lines-before 1
     :empty-lines-after 1
     :immediate-finish t))

(defun jf/personal/position-in-private-thoughts ()
  "Position to the end of today's private thoughts."
  (let ((entry-date (format-time-string "%Y-%m-%d %A")))
    (if-let ((position
               (car (org-element-map
                      (org-element-parse-buffer 'headline)
                      'headline
                      (lambda (hl)
                        (and
                          (=
                            (org-element-property :level hl) 4)
                          (member "private"
                            (org-element-property :tags hl))
                          (string=
                            (format-time-string "%Y-%m-%d %A")
                            (format "%s"
                              (org-element-property
	                                   :title
	                                   (car (org-element-lineage hl)))))
                          (org-element-property :contents-end hl)))))))
      (goto-char (1- position))
      (user-error "Missing :private: entry for date %S" entry-date))))

(defvar jf/epigraphs/cache
  nil
  "When non-nil, use this cache for picking a random epigraphs.")

(defun jf/epigraph/random ()
  "Open a random epigraph for reading and review."
  (interactive)
  (let ((epigraphs
          (or jf/epigraphs/cache (jf/epigraphs/all-randomized))))
    (setq jf/epigraphs/cache epigraphs)
    (let* ((use-hard-newlines
             t)
            (epigraph
              (seq-random-elt epigraphs))
            (id
              (plist-get epigraph :id))
            (text
              (plist-get epigraph :text))
            (author
              (plist-get epigraph :author))
            (work
              (plist-get epigraph :work))
            (content
              (format "%s%s"
                text
                (cond
                  ((and (s-present? author) (s-present? work))
                    (format "\n―%s, [[epigraph:%s][%s]]" author id work))
                  ((s-present? author)
                    (format "\n―[[epigraph:%s][%s]]" id author))
                  ((s-present? work)
                    (format "\n―[[epigraph:%s][%s]]" id work))
                  (t ""))))
            (buffer
              (get-buffer-create "*epigraph*")))
      ;; (message "%s" content)
      (with-current-buffer buffer
        (erase-buffer)
        (insert content)
        (org-mode)
        (jf/epigraph-mode t)
        (pop-to-buffer buffer
          `((display-buffer-in-side-window)
             (side . bottom)
             (window-width 72)
             (window-parameters
               (tab-line-format . none)
               (no-delete-other-windows . t))))))))

(defvar jf/epigraph-mode-map
  (let ((map
          (make-sparse-keymap)))
    (define-key map (kbd "q") #'jf/epigraph/bury-buffer)
    (define-key map (kbd "g") #'jf/epigraph/random)
    map)
  "Map for `jf/epigraph-mode'.")

(defun jf/epigraph/bury-buffer ()
  (interactive)
  (setq jf/epigraphs/cache nil)
  (bury-buffer))

(define-minor-mode jf/epigraph-mode
  "Defined for rendering random epigraph."
  :init-value nil
  :global nil
  :keymap jf/epigraph-mode-map)

(defun jf/epigraphs/all-randomized ()
  (save-excursion
    (with-current-buffer
      (find-file-noselect jf/filename/bibliography)
      (elfeed--shuffle
        (org-element-map
          (org-element-parse-buffer)
          '(quote-block verse-block)
          (lambda (el)
            ;; Skip un-named blocks as we can’t link to them.
            (when-let* ((id
                          (org-element-property :name el)))
              (let* ((lineage
                       (org-element-lineage el))
                      (h-node
                        (car
                          (seq-filter
                            (lambda (el)
                              (and
                                (eq (org-element-type el) 'headline)
                                (= (org-element-property :level el) 2)))
                            lineage)))
                      (people?
                        (member "people"
                          (org-element-property :tags h-node))))
                (list
                  :id id
                  :type (org-element-type el)
                  :work (if people?
                          ""
                          (car
                            (org-element-property
                              :title h-node)))
                  :author
                  (if people?
                    (car
                      (org-element-property :title h-node))
                    (org-entry-get h-node "AUTHOR"))
                  :text
                  (buffer-substring-no-properties
                    (org-element-property
                      :contents-begin el)
                    (org-element-property
                      :contents-end el)))))))))))

;; (use-package notmuch
;;   ;; Consider the NotMuch Mail Emacs tips and tricks: https://notmuchmail.org/emacstips/
;;   ;;
;;   ;; https://blog.tomecek.net/post/removing-messages-with-notmuch/
;;   :load-path  "/opt/homebrew/share/emacs/site-lisp/notmuch"
;;   :custom
;;   ((notmuch-saved-searches
;;      '((:name "inbox" :query "tag:inbox" :key "i" :sort-order newest-first)
;;         (:name "unread" :query "tag:unread" :key "u" :sort-order newest-first)
;;         (:name "correspondence" :query "tag:correspondence" :key "c" :sort-order newest-first)
;;         (:name "flagged" :query "tag:flagged" :key "f" :sort-order newest-first)
;;         (:name "sent" :query "tag:sent" :key "t" :sort-order newest-first)
;;         (:name "drafts" :query "tag:draft" :key "d" :sort-order newest-first)
;;         (:name "all mail" :query "*" :key "a" :sort-order newest-first)))
;;     (notmuch-init-file "~/.notmuch-config"))
;;   :config
;;   /(setq notmuch-fcc-dirs "sent")
;;   (setq mail-specify-envelope-from t)
;;   (define-key notmuch-search-mode-map (kbd "C-k")
;;     (lambda ()
;;       "Delete message."
;;       (interactive)
;;       (notmuch-search-tag (list "+deleted" "-inbox"))
;;       (notmuch-search-next-thread)))
;;   (define-key notmuch-tree-mode-map (kbd "C-k")
;;     (lambda ()
;;       "Delete message."
;;       (interactive)
;;       (notmuch-tree-tag (list "+deleted" "-inbox"))
;;       (notmuch-tree-next-matching-message)))

;;   (defun jf/notmuch/expunge-deleted ()
;;     "Expunge deleted emails."
;;     (interactive)
;;     (shell-command
;;       "for x in $(notmuch search --output=files tag:deleted | rg \":2(,.*[^T])?$\" | sed -E \"s/:2(,.+)?$/:2/g\" ) ; do mv $x ${x}T ; done")
;;     ;; "notmuch search --output=files tag:deleted | tr '\\n' '\\0' | xargs -0 -L 1 rm ; notmuch new 1&> /dev/null")
;;   )

;;   (defun jf/notmuch-unthreaded-show-recipient-if-sent (format-string result)
;;     (let* ((headers (plist-get result :headers))
;;             (to (plist-get headers :To))
;;             (author (plist-get headers :From))
;;             (face (if (plist-get result :match)
;;                     'notmuch-tree-match-author-face
;;                     'notmuch-tree-no-match-author-face)))
;;       (propertize
;;         (format format-string
;;           (if (string-match "jeremy@jeremyfriesen.com" author)
;;             (concat "↦ " (notmuch-tree-clean-address to))
;;             (notmuch-tree-clean-address to)
;;             author))
;;         'face face)))

;;   (setq notmuch-unthreaded-result-format
;;     '(("date" . "%12s  ")
;;        (jf/notmuch-unthreaded-show-recipient-if-sent . "%-20.20s")
;;        ((("subject" . "%s"))
;;          . " %-54s ")
;;        ("tags" . "(%s)"))))

;; (use-package ol-notmuch
;;   :straight t
;;   :after notmuch)
;; (use-package notmuch-transient
;;   :straight t
;;   :after notmuch)

;; (use-package notmuch-addr
;;   ;; Relies on Emacs 27.1 updated completion framework.
;;   :straight t
;;   :config
;;   (with-eval-after-load 'notmuch-address
;;     (notmuch-addr-setup)))

(use-package mu4e
  ;; This follows the build from https://vhbelvadi.com/emacs-mu4e-on-macos.
  ;;
  ;; One variation: the tls_trust_file in ~/.msmtprc did not need to
  ;; come from the Apple Keychain.  But was from "Export TLS
  ;; Certificates from the Proton Bridge > Settings > Advanced Settings
  :load-path  "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"
  :init (require 'smtpmail)
  :config
  ;; BINARIES
  (setq mu4e-mu-binary (executable-find "mu")
    mu4e-get-mail-command (concat (executable-find "mbsync") " -a")
    sendmail-program (executable-find "msmtp"))
  (setq mu4e-user-mail-address-list '("jeremy@jeremyfriesen.com"))
  (setq mu4e-change-filenames-when-moving t ; avoid sync conflicts
    mu4e-update-interval (* 10 60) ; check mail 10 minutes
    mu4e-compose-format-flowed t ; re-flow mail so it's not hard wrapped
    mu4e-maildir "~/Maildir/proton")
  (setq mu4e-drafts-folder "/Drafts"
    mu4e-sent-folder   "/Sent"
    mu4e-refile-folder "/All Mail"
    mu4e-trash-folder  "/Trash")
  (setq mu4e-completing-read-function 'completing-read)
  (setq mu4e-maildir-shortcuts
    '(("/inbox"     . ?i)
       ("/Sent"      . ?s)
       ("/Archive" . ?A)
       ("/Trash"     . ?t)
       ("/Drafts"    . ?d)
       ("/All Mail"  . ?a)))
  (setq mu4e-use-fancy-chars t)

  ;; SENDING
  (setq send-mail-function 'message-send-mail-with-sendmail
    message-send-mail-function 'message-send-mail-with-sendmail)
  (setq message-sendmail-envelope-from 'header)

  ;; POLICIES
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'ask)
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-hide-index-messages t)
  (setq org-mu4e-link-query-in-headers-mode nil)
  (setq mu4e-headers-visible-lines 25)
  (setq mu4e-view-show-addresses t)

  (setq mu4e-headers-include-related t)
  (setq mu4e-headers-show-threads t)
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  (setq message-citation-line-format "%N @ %Y-%m-%d %H:%M :\n")


  ;; (setq message-send-mail-function 'smtpmail-send-it
  ;;     auth-sources '("~/.authinfo") ;need to use gpg version but only local smtp stored for now
  ;;     smtpmail-smtp-server "127.0.0.1"
  ;;     smtpmail-smtp-service 1025
  ;;     smtpmail-stream-type  'ssl)
  )
