;; See https://github.com/tecosaur/screenshot/blob/master/screenshot.el for some examples
(use-package transient
  :straight t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; BEGIN minor mode definitions
;;
;;; Commentary:
;;
;;  I like the idea of a "C-c m" invoking a general menu of utility.  This
;;  file, by it's very nature, has knowledge of other functions.  The other
;;  option is for other "files" to know about the menu structure.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defmacro minor-mode-maker (&key title abbr hooks)
  "A macro to declare a minor mode.

Use TITLE to derive the docstring.
Use ABBR to derive the mode-name lighter.
Add hook to each HOOKS provided."
  (let ((mode-name (intern (s-dashed-words (s-downcase (concat "jnf-" abbr "-minor-mode")))))
	(lighter (concat " " abbr))
	(docstring (concat "Minor mode for " title " .")))
    `(progn
       (define-minor-mode ,mode-name
	 ,docstring
	 :global nil
	 :lighter ,lighter)
       (when ,hooks
	 (-each ,hooks (lambda(hook) (add-hook hook (lambda () (,mode-name)))))))))

(minor-mode-maker :title "Burning Wheel Gold" :abbr "bwg" :hooks (list 'org-mode-hook 'markdown-mode-hook))
;; (minor-mode-maker :title "Stars without Number" :abbr "swn")
;; (minor-mode-maker :title "Worlds without Number" :abbr "wwn")
(minor-mode-maker :title "Take on Rules" :abbr "tor")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; END minor mode definitions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(transient-define-prefix jnf/menu-dwim--bwg-help ()
  "Define the BWG help prefix."
  ["Burning Wheel"
   ("c" jnf/qh--bwg-circles-obstacles)
   ("d" jnf/qh--bwg-absolute-difficulty)
   ("e" jnf/qh--bwg-expertise-exponent)
   ("s" jnf/qh--bwg-steel-test-adjustments)
   ("w" jnf/qh--bwg-wises)
   ])

(transient-define-prefix jnf/menu-dwim--tor-find-files ()
  "Define the Take on Rules find files prefix."
  ["Take on Rules > Find Blog"
   ("d" "in draft status…" jnf/tor-find-file-draft)
   ("u" "by url…" jnf/tor-find-hugo-file-by-url)
   ("f" "by filename…" jnf/tor-find-file)])

(transient-define-prefix jnf/menu-dwim--tor-create ()
  "Define the Take on Rules create prefix."
  ["Take on Rules > Create"
   ("a" "Amplify the Blogosphere…" jnf/tor-post-amplifying-the-blogosphere)
   ("c" "Changelog entry…" jnf/tor-find-changelog-and-insert-entry)
   ("e" "Epigraph entry…" jnf/tor-insert-epigraph-entry)
   ("g" "Glossary entry…" jnf/tor-find-glossary-and-insert-entry)
   ("p" "Post…" jnf/tor-create-post)
   ("s" "Series…" jnf/tor-find-series-and-insert-entry)])

(transient-define-prefix jnf/menu-dwim--hammerspoon ()
  "Define the Take on Rules find files prefix."
  ["Hammerspoon"
   :if-non-nil hammerspoon-edit-minor-mode
   ("m" "Toggle hammerspoon editor mode" hammerspoon-toggle-mode)
   ("p" "Tidy pull request" jnf/forem-tidy-pull-request)])

(transient-define-prefix jnf/menu-dwim ()
  "A \"super\" menu of context specific functions."
  [
   ["Markdown Utilities"
    ("k h" "Kill slug version of given heading…" jnf/kill-new-markdown-heading-as-slug :if-derived (or markdown-mode html-mode))
    ("w a" "A-tag at point or region…" jnf/tor-wrap-link-active-region-dwim  :if-derived (or markdown-mode html-mode))
    ("w c" "CITE-tag point or region…" jnf/tor-wrap-cite-active-region-dwim  :if-derived (or markdown-mode html-mode))
    ("w d" "DATETIME-tag point or region…" jnf/tor-wrap-date  :if-derived (or markdown-mode html-mode))
    ("w f" "Wrap word or region in pseudo-DFN…" jnf/tor-wrap-as-pseudo-dfn  :if-derived (or markdown-mode html-mode))
    ("w m" "Margin-note line or region…" jnf/tor-wrap-as-marginnote-dwim  :if-derived (or markdown-mode html-mode))
    ("w p" "Wrap point or region as Poem…" jnf/tor-wrap-in-poem  :if-derived (or markdown-mode html-mode))
    ("w s" "Side-note sentence or region…" jnf/tor-wrap-as-sidenote-dwim  :if-derived (or markdown-mode html-mode))
    ("w w" "Wrap point or region in html…" jnf/tor-wrap-in-html-tag  :if-derived (or markdown-mode html-mode))
    ]
   ["Take on Rules > Posts"
    :if-non-nil jnf-tor-minor-mode
    ("p r" "Re-title post…" jnf/tor-retitle-post)
    ("p t" "Tag post…" jnf/tor-tag-post :transient t)
    ("p v" "View post…" jnf/tor-view-blog-post)
    ]
   ])

(defun jnf/menu-dwim--global-suffixes ()
  "Return a `transient' compliant list to apply to different transients."
  (list
   ["Contexts"
    ("? b" "Burning Wheel…"  jnf/menu-dwim--bwg-help)
    ("? f" "Forem…" jnf/forem-menu/body)
    ("? t" "TakeOnRules Find…" jnf/menu-dwim--tor-find-files)
    ("c t" "TakeOnRules Create…" jnf/menu-dwim--tor-create)
    ("? h" "Hammerspoon…" jnf/menu-dwim--hammerspoon :if-non-nil hammerspoon-edit-minor-mode)]
   ["Modes"
    ;; I could write functions for these, but this is concise enough
    ("-t" "Typopunct ( )" typopunct-mode :if-nil typopunct-mode)
    ("-t" "Typopunct (*)" typopunct-mode :if-non-nil typopunct-mode)
    ("-o" "MacOS Native Option ( )" jnf/toggle-osx-alternate-modifier :if-non-nil ns-alternate-modifier)
    ("-o" "MacOS Native Option (*)" jnf/toggle-osx-alternate-modifier :if-nil ns-alternate-modifier)
    ]))

(transient-insert-suffix 'jnf/menu-dwim (list 0)
  `["Global"
    ,@(jnf/menu-dwim--global-suffixes)])

(transient-insert-suffix 'org-menu (list 1)
  `["Global"
    ,@(jnf/menu-dwim--global-suffixes)])

(global-set-key (kbd "C-c m") 'jnf/menu-dwim)