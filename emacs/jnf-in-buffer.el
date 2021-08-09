;;; jnf-in-buffer.el --- Summary
;;
;;; Commentary:
;;
;;  Packages that greatly assist with in-buffer editing:
;;
;;  * navigation
;;  * case manipulation
;;  * region highlighting (or compression)
;;
;;  As I've spent time thinking about this, it's hard to separate.
;;  But there's some kind of logic.  Namely, I'm operating only on
;;  this buffer.  Not navigating between buffers.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Write "kill" command inputs to disk
(use-package savekill
  :straight t)

;; https://github.com/shankar2k/math-at-point
(use-package math-at-point
  :straight (math-at-point :type git :host github :repo "shankar2k/math-at-point")
  :bind ("C-c =" . math-at-point))

(use-package which-key
  :config (which-key-mode)
  :straight t)

(use-package writeroom-mode
  :straight t)

(use-package vi-tilde-fringe
  :straight t
  :hook ((fundamental-mode) . vi-tilde-fringe-mode))

;; I don't use a lot of folding, this allows me to type C-RET and fold
;; the current block.  There's more it can do but for now that's
;; enough
(use-package yafolding
  :straight t
  :hook (prog-mode . yafolding-mode))

;; Using Hippie expand, I toggle through words already referenced.
(setq hippie-expand-try-functions-list '(try-expand-dabbrev-visible
                                         try-expand-dabbrev
                                         try-expand-list
                                         try-expand-all-abbrevs
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol
                                         ))
(global-set-key (kbd "M-SPC") 'hippie-expand)

;; Expand or contract point/region to next logical element.
;;
;; NOTE: I use this all the time.
(use-package expand-region
  :straight t
  :bind (("C-=" . er/expand-region)
         ("C-+" . er/contract-region)))

;; From https://old.reddit.com/r/orgmode/comments/eq76pv/question_about_orgsuperagenda/feok4ro/
;; And https://www.reddit.com/r/emacs/comments/n9q662/weekly_tipstricketc_thread/
(defun jnf/copy-indented-4-spaces (beg end)
  "Copy the region between BEG and END and add an indent of 4 spaces.

Useful for pasting code into Reddit's Markdown mode."
  (interactive "r")
  (let* ((mode major-mode)
         (buffer (current-buffer))
         (inhibit-read-only t))
    (kill-new (with-temp-buffer
                (funcall mode)
                (insert-buffer-substring buffer beg end)
                (when (derived-mode-p 'prog-mode)
                  (delete-trailing-whitespace)
                  (indent-region (point-min) (point-max) nil))
                (indent-rigidly (point-min) (point-max) 4)
                (buffer-string)))))
(global-set-key (kbd "C-c 4") 'jnf/copy-indented-4-spaces)

;; This package allows me to toggle between different string cases.
;;
;; - HELLO WORLD
;; - HelloWorld
;; - helloWorld
;; - hello-world
;; - Hello_World
;; - hellow_world
;; - HELLO_WORLD
(use-package string-inflection
  :bind (("C-M-u" . string-inflection-all-cycle)) ;; CTRL+OPT+u
  :straight (string-inflection :type git
                               :host github
                               :repo "akicho8/string-inflection"))

;; Allow to work with multipe cursors
;; https://melpa.org/#/multiple-cursors Aside from the
;; set-rectangular-region-anchor, there are several additional
;; features to practice
(use-package multiple-cursors
  :bind (("C-M-SPC" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-s-<mouse-1>" . mc/add-cursor-on-click)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/edit-lines)) ;; CTRL+CMD+c
  :straight t)

;; C-; to select current symbol and all matches; Then edit at multiple points.
(use-package iedit
  :straight t)

;; Not quite sexp navigation, but something alternative.  For vertical
;; movement, it looks at an empty line and moves there.  For
;; horizontal movement, moves between two spaces.
(use-package spatial-navigate
  :straight (spatial-navigate :type git
                              :host gitlab
                              :repo "ideasman42/emacs-spatial-navigate")
  :bind (("<M-s-up>" . #'spatial-navigate-backward-vertical-bar)
         ("<M-s-down>" . #'spatial-navigate-forward-vertical-bar)
         ("<M-s-left>" . #'spatial-navigate-backward-horizontal-bar)
         ("<M-s-right>" . #'spatial-navigate-forward-horizontal-bar)))

;; C-a goes to the first non-whitepsace character on the line. Type it
;; again, and go to the beginning of the line.
(use-package crux
  :straight t
  :bind (("C-a" . crux-move-beginning-of-line)
         ("<C-s-return>" . crux-smart-open-line-above)
         ("C-M-d" . crux-duplicate-current-line-or-region)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("<f9>" . crux-kill-other-buffers)))

;; Whitespace hygene package.  The author's documentation and
;; commentary echoes my sentiments
(use-package ethan-wspace
  :straight t
  :hook (before-save . delete-trailing-whitespace)
  :init (setq-default mode-require-final-newline nil)
  :config (global-ethan-wspace-mode 1))

;; A package that is a bit of the inverse of 'fill-paragraph
;; (e.g. M-q).
(use-package unfill
  :bind ("M-q" . unfill-toggle)
  :straight t)

;; Provides a UI for undo trees.  I'm not certain what I want to do
;; with this.
(use-package undo-tree
  :diminish
  :bind (("C-z" . undo)
         ("C-S-z" . undo-tree-redo))
  :config
  (global-undo-tree-mode +1)
  (unbind-key "M-_" undo-tree-map))

;; Delete multiple spaces in one delete stroke
(use-package hungry-delete
  :straight t
  :config (global-hungry-delete-mode))

;; Adding ability to move lines up and down
(use-package move-text
  :straight t
  :bind (([C-s-down] . move-text-down)
         ([C-s-up] . move-text-up)))

;; A quick and useful visual queue for paranthesis
(use-package rainbow-delimiters
  :straight t
  :hook ((prog-mode text-mode org-mode) . rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN EMOJIS
;;
;; Add emoji handling
(use-package emojify
  :straight t)

(use-package unicode-fonts
  :straight t
  :ensure t
  :config (unicode-fonts-setup))

(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
    ;; For Linux
    (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

;; For when Emacs is started in GUI mode:
(--set-emoji-font nil)
;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions '--set-emoji-font)
;;
;; END EMOJIS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jnf/kill-region-or-backward-word (&optional arg)
  "Kill selected region otherwise kill backwards the ARG number of words."
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
(global-set-key (kbd "C-w") 'jnf/kill-region-or-backward-word)

(defun jnf/kill-line-or-region (&optional ARG)
  "Kill the selected region otherwise kill the ARG number of lines."
  (interactive "P")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-line ARG)))
(global-set-key (kbd "C-k") 'jnf/kill-line-or-region)

;; For myself, it's easier to toggle between CTRL+f and CTRL+CMD+f or
;; b for moving words.
(global-set-key (kbd "C-s-f") 'forward-word)
(global-set-key (kbd "C-s-b") 'backward-word)


;; A rather convenient snippet manager.  When you create a snippet, it
;; understands the mode you're in and puts the snippet in the right
;; place.
(use-package yasnippet
  :straight t
  :init (setq yas-snippet-dirs '("~/git/dotzshrc/emacs/snippets"))
  (yas-global-mode 1))

;; I kind of like this little bit of visual feedback
(use-package goggles
  :straight t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing

;; https://blog.sumtypeofway.com/posts/emacs-config.html
(defun jnf/nab-file-name-to-clipboard ()
  "Nab, I mean copy, the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
(global-set-key (kbd "C-c n") 'jnf/nab-file-name-to-clipboard)

(use-package whole-line-or-region
  :straight t
  :config (whole-line-or-region-global-mode))

(use-package smartparens
  :straight t)

(global-set-key (kbd "<f5>") 'eval-region)
(global-set-key (kbd "M-DEL") 'backward-kill-paragraph)
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "s-w") 'kill-current-buffer)

;; Treat dashes and underscores as part of words for navigation
;; (global-superword-mode t)

(provide 'jnf-in-buffer.el)
;;; jnf-in-buffer.el ends here
