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

(defun burning-wheel-gold-menu-items ()
  "Return a `transient' compliant list for BWG items."
  (list
   ["BWG References"
    :if-non-nil jnf-bwg-minor-mode
    ("b c" "Circles" jnf/qh--bwg-circles-obstacles)
    ("b d" "Difficulty (Absolute)" jnf/qh--bwg-absolute-difficulty)
    ("b e" "Exponent" jnf/qh--bwg-expertise-exponent)
    ("b s" "Steel" jnf/qh--bwg-steel-test-adjustments)
    ("b w" "Wises" jnf/qh--bwg-wises)]))

(minor-mode-maker :title "Stars without Number" :abbr "swn")
(minor-mode-maker :title "Worlds without Number" :abbr "wwn")
(minor-mode-maker :title "Take on Rules" :abbr "tor")

(defun minor-modes-menu-items ()
  "Return a `transient' compliant list for minor modes."
  (list
  ["Modes"
    ("-b" "Burning Wheel mode" jnf-bwg-minor-mode)
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
    :if-derived markdown-mode
    ("k h" "Kill slug version of given [h]eading…" jnf/kill-new-markdown-heading-as-slug)
    ("w a" "[A] link at point or region…" jnf/tor-wrap-link-active-region-dwim)
    ("w c" "[C]ite point or region…" jnf/tor-wrap-cite-active-region-dwim)
    ("w d" "[D]ate point or region…" jnf/tor-wrap-date)
    ("w f" "Wrap word or region in pseudo-d[f]n…" jnf/tor-wrap-as-pseudo-dfn)
    ("w m" "[M]argin-note line or region…" jnf/tor-wrap-as-marginnote-dwim)
    ("w p" "Wrap point or region as [P]oem…" jnf/tor-wrap-in-poem)
    ("w s" "[S]ide-note sentence or region…" jnf/tor-wrap-as-sidenote-dwim)
    ("w w" "[W]rap point or region in html…" jnf/tor-wrap-in-html-tag)
    ]
   ["Posts"
    :if-non-nil jnf-bwg-minor-mode
    ("p r" "[R]e-title post…" jnf/tor-retitle-post)
    ("p t" "[T]ag post…" jnf/tor-tag-post :transient t)
    ("p v" "[V]iew post…" jnf/tor-view-blog-post)
    ]
   ["Utilities"
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

(transient-append-suffix 'jnf/menu-dwim (list 0)
  `[,@(minor-modes-menu-items)
    ,@(burning-wheel-gold-menu-items)])

(transient-insert-suffix 'org-menu (list 0)
  `[,@(minor-modes-menu-items)
    ,@(burning-wheel-gold-menu-items)])

(global-set-key (kbd "C-c m") 'jnf/menu-dwim)