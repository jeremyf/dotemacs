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

(transient-define-prefix jnf/menu-dwim--tor ()
  "Define the Take on Rules menu."
  ["Take on Rules"
   ["Posts"
    :if-non-nil jnf-tor-minor-mode
    ("p r" "Re-title post…" jnf/tor-retitle-post)
    ("p t" "Tag post…" jnf/tor-tag-post :transient t)
    ("p v" "View post…" jnf/tor-view-blog-post)
    ]
   ["Find"
    ("f d" "in draft status…" jnf/tor-find-file-draft)
    ("f u" "by url…" jnf/tor-find-hugo-file-by-url)
    ("f f" "by filename…" jnf/tor-find-file)]
   ["Create"
    ("c a" "Amplify the Blogosphere…" jnf/tor-post-amplifying-the-blogosphere)
    ("c c" "Changelog entry…" jnf/tor-find-changelog-and-insert-entry)
    ("c e" "Epigraph entry…" jnf/tor-insert-epigraph-entry)
    ("c g" "Glossary entry…" jnf/tor-find-glossary-and-insert-entry)
    ("c p" "Post…" jnf/tor-create-post)
    ("c s" "Series…" jnf/tor-find-series-and-insert-entry)]
   ])

(transient-define-prefix jnf/menu-dwim--hammerspoon ()
  "Define the Take on Rules find files prefix."
  ["Hammerspoon"
   :if-non-nil hammerspoon-edit-minor-mode
   ("m" "Toggle hammerspoon editor mode" hammerspoon-toggle-mode)
   ("p" "Tidy pull request" jnf/forem-tidy-pull-request)])

(cl-defun jnf/menu-dwim--org-capture-elfeed-show (&key (entry elfeed-show-entry))
  "Create an `org-roam-node' from elfeed ENTRY."
  (interactive)
  (let ((ref (elfeed-entry-link entry))
	(title (elfeed-entry-title entry)))
    (jnf/org-roam-capture-ref :ref ref :title title)))

(defun jnf/menu-dwim--org-capture-safari ()
  "Create an `org-roam-node' from Safari page.

Depends on the `grab-mac-link' package."
  (interactive)
  (let* ((link-title-pair (grab-mac-link-safari-1))
	 (ref (car link-title-pair))
	 (title (cadr link-title-pair)))
    (jnf/org-roam-capture-ref :ref ref :title title)))

(defun jnf/menu-dwim--org-capture-firefox ()
  "Create an `org-roam-node' from Firefox page.

Depends on the `grab-mac-link' package."
  (interactive)
  (let* ((link-title-pair (grab-mac-link-firefox-1))
	 (ref (car link-title-pair))
	 (title (cadr link-title-pair)))
    (jnf/org-roam-capture-ref :ref ref :title title)))

(transient-define-suffix jnf/org-auto-tags--transient (tags)
  "Set the tags from minibuffer read"
  :description '(lambda ()
                  (concat
                   "Org Tags: "
                   (propertize
                    (format "%s" jnf/org-auto-tags--current-list)
                    'face 'transient-argument)))
  (interactive
   (list (completing-read-multiple "Tag(s): " (org-roam-tag-completions))))
  (setq jnf/org-auto-tags--current-list tags))

(transient-define-prefix jnf/menu-dwim ()
  "Return a `transient' compliant list to apply to different transients."
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
   ["Contexts"
    ("c b" "Burning Wheel…"  jnf/menu-dwim--bwg-help)
    ("c f" "Forem…" jnf/forem-menu/body)
    ("c t" "TakeOnRules…" jnf/menu-dwim--tor)
    ("c h" "Hammerspoon…" jnf/menu-dwim--hammerspoon :if-non-nil hammerspoon-edit-minor-mode)
    ]
   ["Grab"
    ("g e" "Elfeed" jnf/menu-dwim--org-capture-elfeed-show :if-derived elfeed-show-mode)
    ("g f" "Firefox" jnf/menu-dwim--org-capture-firefox)
    ("g s" "Safari" jnf/menu-dwim--org-capture-safari)
    ;; ("g u" "URL")
    ]
   ["Jump to"
    ;; ("j a" "Agenda" ibuffer)
    ("j b" "Buffer" ibuffer)
    ("j g" "Global Mark" consult-global-mark)
    ("j m" "Mark" consult-mark)
    ]
   ["Modes"
    ;; I could write functions for these, but this is concise enough
    ("m t" "Typopunct ( )" typopunct-mode :if-nil typopunct-mode)
    ("m t" "Typopunct (*)" typopunct-mode :if-non-nil typopunct-mode)
    ("m o" "MacOS Native Option ( )" jnf/toggle-osx-alternate-modifier :if-non-nil ns-alternate-modifier)
    ("m o" "MacOS Native Option (*)" jnf/toggle-osx-alternate-modifier :if-nil ns-alternate-modifier)
    ]]
  ["Org Add-Ons"
   ("o c" "Context…" jnf/org-auto-tags--set-by-context :transient t)
   ("o t" "Add Org Tag…" org-roam-tag-add :if-derived org-mode)
   ("o r" "Add Org Ref…" org-roam-ref-add :if-derived org-mode)
   ("o s" jnf/org-auto-tags--transient :transient t)
   ])

(global-set-key (kbd "s-1") 'jnf/menu-dwim)
(global-set-key (kbd "C-c m") 'jnf/menu-dwim)