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
(cl-defmacro rpg-minor-mode (&key title abbr)
  "A macro to declare an RPG minor mode.

Use TITLE to derive the mode-name and docstring.
Use ABBR to derive the lighter."
  (let ((mode-name (intern (s-dashed-words (s-downcase (concat "rpg-" title "-minor-mode")))))
	(lighter (concat "_" abbr "_"))
	(docstring (concat "Minor mode for " title " RPG.")))
    `(define-minor-mode ,mode-name
	 ,docstring
	 :global nil
	 :lighter ,lighter)))

(rpg-minor-mode :title "Burning Wheel Gold" :abbr "bwg")
(add-hook 'markdown-mode-hook
          (lambda () (rpg-burning-wheel-gold-minor-mode)))
(add-hook 'org-mode-hook
          (lambda () (rpg-burning-wheel-gold-minor-mode)))
(rpg-minor-mode :title "Stars without Number" :abbr "swn")
(rpg-minor-mode :title "Worlds without Number" :abbr "wwn")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; END minor mode definitions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(transient-define-prefix jnf/menu-dwim ()
  "A \"super\" menu of context specific functions."
  [["Modes"
    ("m b" "Burning Wheel" rpg-burning-wheel-gold-minor-mode)
    ("m s" "Stars without Number" rpg-stars-without-number-minor-mode)
    ("m t" "Typo Punct Mode" typopunct-mode)
    ("m w" "Worlds without Number" rpg-worlds-without-number-minor-mode)
    ]
   ["ToR Wrapping"
    :if-derived markdown-mode
    ("w a" "[A] link at point or region…" jnf/tor-wrap-link-active-region-dwim)
    ("w c" "[C]ite point or region…" jnf/tor-wrap-cite-active-region-dwim)
    ("w d" "[D]ate point or region…" jnf/tor-wrap-date)
    ("w f" "Wrap word or region in pseudo-d[f]n…" jnf/tor-wrap-as-pseudo-dfn)
    ("w m" "[M]argin-note line or region…" jnf/tor-wrap-as-marginnote-dwim)
    ("w p" "Wrap point or region as [P]oem…" jnf/tor-wrap-in-poem)
    ("w s" "[S]ide-note sentence or region…" jnf/tor-wrap-as-sidenote-dwim)
    ("w w" "[W]rap point or region in html…" jnf/tor-wrap-in-html-tag)
    ]
   ["ToR Posts"
    :if-derived markdown-mode
    ("p r" "[R]e-title post…" jnf/tor-retitle-post)
    ("p t" "[T]ag post…" jnf/tor-tag-post :transient t)
    ("p v" "[V]iew post…" jnf/tor-view-blog-post)
    ("k h" "Kill slug version of given [h]eading…" jnf/kill-new-markdown-heading-as-slug)
    ]
   ["ToR Utilities"
    ("c a" "Create [a]mplify the blogosphere…" jnf/tor-post-amplifying-the-blogosphere)
    ("c e" "Create [e]pigraph entry…" jnf/tor-insert-epigraph-entry)
    ("c g" "Create [g]lossary entry…" jnf/tor-find-glossary-and-insert-entry)
    ("c c" "Create [c]hange log entry…" jnf/tor-find-changelog-and-insert-entry)
    ("c p" "Create [p]ost…" jnf/tor-create-post)
    ("c s" "Create [s]eries…" jnf/tor-find-series-and-insert-entry)
    ("? d" "Find blog in [d]raft status…" jnf/tor-find-file-draft)
    ("? u" "Find blog by [u]rl…" jnf/tor-find-hugo-file-by-url)
    ("? f" "Find blog by [f]ilename…" jnf/tor-find-file)
    ]])

(transient-insert-suffix 'jnf/menu-dwim (list 0)
   [["BWG References"
    :if-non-nil rpg-burning-wheel-gold-minor-mode
    ("b c" "Circles" jnf/qh--bwg-circles-obstacles)
    ("b d" "Difficulty (Absolute)" jnf/qh--bwg-absolute-difficulty)
    ("b e" "Exponent" jnf/qh--bwg-expertise-exponent)
    ("b s" "Steel" jnf/qh--bwg-steel-test-adjustments)
    ("b w" "Wises" jnf/qh--bwg-wises)
    ]])

(transient-insert-suffix 'org-menu (list 0)
  [["Modes"
    ("m b" "Burning Wheel" rpg-burning-wheel-gold-minor-mode)
    ("m s" "Stars without Number" rpg-stars-without-number-minor-mode)
    ("m w" "Worlds without Number" rpg-worlds-without-number-minor-mode)
    ]
   ["BWG References"
    :if-non-nil rpg-burning-wheel-gold-minor-mode
    ("b c" "Circles" jnf/qh--bwg-circles-obstacles)
    ("b d" "Difficulties" jnf/qh--bwg-absolute-difficulty)
    ("b e" "Exponents" jnf/qh--bwg-expertise-exponent)
    ("b s" "Steel" jnf/qh--bwg-steel-test-adjustments)
    ("b w" "Wises" jnf/qh--bwg-wises)
    ]])

(global-set-key (kbd "C-c m") 'jnf/menu-dwim)