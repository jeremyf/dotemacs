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

(defun rpg-menu-items ()
  "Return a `transient' compliant list for BWG items."
  (list
   ["Game References"
    ("b c" "Circles" jnf/qh--bwg-circles-obstacles :if-non-nil jnf-bwg-minor-mode)
    ("b d" "Difficulty (Absolute)" jnf/qh--bwg-absolute-difficulty :if-non-nil jnf-bwg-minor-mode)
    ("b e" "Exponent" jnf/qh--bwg-expertise-exponent :if-non-nil jnf-bwg-minor-mode)
    ("b s" "Steel" jnf/qh--bwg-steel-test-adjustments :if-non-nil jnf-bwg-minor-mode)
    ("b w" "Wises" jnf/qh--bwg-wises :if-non-nil jnf-bwg-minor-mode)]))

(minor-mode-maker :title "Stars without Number" :abbr "swn")
(minor-mode-maker :title "Worlds without Number" :abbr "wwn")
(minor-mode-maker :title "Take on Rules" :abbr "tor")

(defun minor-modes-menu-items ()
  "Return a `transient' compliant list for minor modes."
  (list
  ["Modes"
    ("-b" "Burning Wheel mode" jnf-bwg-minor-mode)
    ("-h" "Hammerspoon mode" hammerspoon-edit-minor-mode)
    ;; ("-s" "Stars without Number mode" jnf-swn-minor-mode)
    ("-t" "Take on Rules mode" jnf-tor-minor-mode)
    ("-T" "Typopunct mode" typopunct-mode)
    ;; ("-w" "Worlds without Number mode" jnf-wwn-minor-mode)
    ]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; END minor mode definitions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-transient-command jnf/menu-dwim ()
  "A \"super\" menu of context specific functions."
  ["Take on Rules"
   ["Wrapping"
    ("w a" "[A] link at point or region…" jnf/tor-wrap-link-active-region-dwim  :if-derived (or markdown-mode html-mode))
    ("w c" "[C]ite point or region…" jnf/tor-wrap-cite-active-region-dwim  :if-derived (or markdown-mode html-mode))
    ("w d" "[D]ate point or region…" jnf/tor-wrap-date  :if-derived (or markdown-mode html-mode))
    ("w f" "Wrap word or region in pseudo-d[f]n…" jnf/tor-wrap-as-pseudo-dfn  :if-derived (or markdown-mode html-mode))
    ("w m" "[M]argin-note line or region…" jnf/tor-wrap-as-marginnote-dwim  :if-derived (or markdown-mode html-mode))
    ("w p" "Wrap point or region as [P]oem…" jnf/tor-wrap-in-poem  :if-derived (or markdown-mode html-mode))
    ("w s" "[S]ide-note sentence or region…" jnf/tor-wrap-as-sidenote-dwim  :if-derived (or markdown-mode html-mode))
    ("w w" "[W]rap point or region in html…" jnf/tor-wrap-in-html-tag  :if-derived (or markdown-mode html-mode))
    ]
   ["Posts"
    :if-non-nil jnf-tor-minor-mode
    ("p r" "[R]e-title post…" jnf/tor-retitle-post)
    ("p t" "[T]ag post…" jnf/tor-tag-post :transient t)
    ("p v" "[V]iew post…" jnf/tor-view-blog-post)
    ]
   ["Utilities"
    ("? d" "Find blog in [d]raft status…" jnf/tor-find-file-draft)
    ("? u" "Find blog by [u]rl…" jnf/tor-find-hugo-file-by-url)
    ("? f" "Find blog by [f]ilename…" jnf/tor-find-file)
    ("c a" "Create [a]mplify the blogosphere…" jnf/tor-post-amplifying-the-blogosphere)
    ("c e" "Create [e]pigraph entry…" jnf/tor-insert-epigraph-entry)
    ("c g" "Create [g]lossary entry…" jnf/tor-find-glossary-and-insert-entry)
    ("c c" "Create [c]hange log entry…" jnf/tor-find-changelog-and-insert-entry)
    ("c p" "Create [p]ost…" jnf/tor-create-post)
    ("c s" "Create [s]eries…" jnf/tor-find-series-and-insert-entry)
    ("k h" "Kill slug version of given [h]eading…" jnf/kill-new-markdown-heading-as-slug :if-derived (or markdown-mode html-mode))
    ("t h m" "Toggle hammerspoon editor mode" hammerspoon-toggle-mode :if-non-nil hammerspoon-edit-minor-mode)
    ("t p" "Tidy pull request" jnf/forem-tidy-pull-request :if-non-nil hammerspoon-edit-minor-mode)
    ]])

(transient-append-suffix 'jnf/menu-dwim (list 0)
  `[,@(minor-modes-menu-items)
    ,@(rpg-menu-items)])

(transient-insert-suffix 'org-menu (list 0)
  `[,@(minor-modes-menu-items)
    ,@(rpg-menu-items)])

(global-set-key (kbd "C-c m") 'jnf/menu-dwim)