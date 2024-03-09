;;; jf-modeline --- A custom mode-line -*- lexical-binding: t -*-

;; Copyright (C) 2024 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Thank you Protesilaos <http://protesilaos.com> for your video on customizing
;; the modeline.

;;; Code:

(require 'vc-git)

(defvar-local jf/mode-line-format/kbd-macro
  '(:eval
     (when (and (mode-line-window-selected-p) defining-kbd-macro)
       (propertize " KMacro " 'face 'mode-line-highlight))))

(defvar-local jf/mode-line-format/buffer-name-and-status
  '(:eval
     (let ((name (buffer-name)))
       (propertize
         (if buffer-read-only
           ;; TODO: <2024-03-09 Sat> Create mouse clickability to review
           ;; filename
           (format " %s %s " (char-to-string #xE0A2) name)
           name)
         'face 'mode-line-buffer-id
         'local-map jf/mode-line-format/map-buffer-name))))

(defvar jf/mode-line-format/map-buffer-name
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] #'jf/filename-as-message)
    map)
  "Keymap to display on VC indicator.")

(defun jf/filename-as-message ()
  "Echo the full path of the filename."
  (interactive)
  (message "%s"
    (if (equal major-mode 'dired-mode)
      default-directory
      (buffer-file-name))))

(defun jf/mode-line-format/major-mode-indicator ()
  "Return appropriate propertized mode line indicator for the major mode."
  (let ((indicator (cond
                     ((derived-mode-p 'text-mode) "§")
                     ((derived-mode-p 'prog-mode) "λ")
                     ((derived-mode-p 'comint-mode) ">_")
                     (t "◦"))))
    (propertize indicator 'face 'jf/mode-line-format/face-shadow)))

(defun jf/mode-line-format/major-mode-name ()
  (propertize (capitalize (string-replace "-mode" "" (symbol-name major-mode)))
    'face 'mode-line))

(defvar-local jf/mode-line-format/major-mode
  '(:eval
     (concat
       (jf/mode-line-format/major-mode-indicator)
       " "
       (jf/mode-line-format/major-mode-name))))

(defvar-local jf/mode-line-format/narrow
  '(:eval
     (when (and (mode-line-window-selected-p)
             (buffer-narrowed-p)
             (not (derived-mode-p
                    'Info-mode
                    'help-mode
                    'special-mode
                    'message-mode)))
       (propertize " Narrow " 'face 'mode-line-highlight))))

(defvar-local jf/mode-line-format/misc-info
  '(:eval
     (when (mode-line-window-selected-p)
       mode-line-misc-info)))

(with-eval-after-load 'eglot
  (setq mode-line-misc-info
    (delete '(eglot--managed-mode
               (" [" eglot--mode-line-format "] "))
      mode-line-misc-info)))

(defvar-local jf/mode-line-format/eglot
  `(:eval
     (when (and (featurep 'eglot) (mode-line-window-selected-p))
       '(eglot--managed-mode eglot--mode-line-format))))

(defvar-local jf/mode-line-format/vc-branch
  '(:eval
     (when-let* (((mode-line-window-selected-p))
                  (file (if (equal major-mode 'dired-mode)
                          default-directory
                          (buffer-file-name)))
                  (backend (or (vc-backend file) 'git))
                  (branch (jf/mode-line-format/vc-branch-name file backend)))
       (jf/mode-line-format/vc-details file branch))))

(defface jf/mode-line-format/face-shadow
  '((t :foreground "#d0ffe0" :inherit shadow))
  "A face for symbols in the `mode-line'.")

(defun jf/mode-line-format/vc-details (file branch)
  "Return the FILE and BRANCH."
  (propertize
    (concat
      (propertize (char-to-string #xE0A0) 'face 'jf/mode-line-format/face-shadow)
      " "
      branch)
    'local-map jf/mode-line-format/map-vc
    'help-echo "mouse-1: magit-status"))

(defvar jf/mode-line-format/map-vc
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] #'magit-status)
    map)
  "Keymap to display on version control indicator.")

(defun jf/mode-line-format/vc-branch-name (file backend)
  "Return VC branch name for FILE with BACKEND."
  (when-let ((rev (vc-working-revision file backend))
              (branch (or (vc-git--symbolic-ref file)
                        (substring rev 0 7))))
    branch))

(defvar-local jf/mode-line-format/flymake
  '(:eval
     (when (and flymake--state
             (mode-line-window-selected-p))
       flymake-mode-line-format)))

(defvar-local jf/mode-line-format/project
  '(:eval
     (when (projectile-project-p)
       (propertize
         (concat " " (project-name (project-current)))
         'face 'jf/mode-line-format/face-shadow))))

(dolist (construct '(
                      jf/mode-line-format/buffer-name-and-status
                      jf/mode-line-format/eglot
                      jf/mode-line-format/flymake
                      jf/mode-line-format/kbd-macro
                      jf/mode-line-format/major-mode
                      jf/mode-line-format/misc-info
                      jf/mode-line-format/narrow
                      jf/mode-line-format/project
                      jf/mode-line-format/vc-branch
                      ))
  (put construct 'risky-local-variable t))

(use-package keycast
  :straight t
  :init
  (setq keycast-mode-line-insert-after
    'jf/mode-line-format/buffer-name-and-status)
  (setq keycast-mode-line-remove-tail-elements nil)
  (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
  (setq keycast-mode-line-format "%2s%k%2s(%c%R)"))

(setq-default mode-line-format
  '("%e" " "
     jf/mode-line-format/kbd-macro
     jf/mode-line-format/narrow
     jf/mode-line-format/buffer-name-and-status "  "
     jf/mode-line-format/major-mode "  "
     jf/mode-line-format/project "  "
     jf/mode-line-format/vc-branch "  "
     jf/mode-line-format/flymake "  "
     jf/mode-line-format/eglot "  "
     ;; jf/mode-line-format/misc-info
     ))
(provide 'jf-modeline)
;;; jf-modeline.el ends here
