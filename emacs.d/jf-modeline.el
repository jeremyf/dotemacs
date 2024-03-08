(require 'vc-git)

(setq-default mode-line-format '("%e" mode-line-front-space
                                  (:propertize
                                    ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
                                    display
                                    (min-width
                                      (5.0)))
                                  mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
                                  (vc-mode vc-mode)
                                  "  " minions-mode-line-modes mode-line-misc-info mode-line-end-spaces))

(defvar-local jf/mode-line-format/kbd-macro
  '(:eval
     (when (and (mode-line-window-selected-p) defining-kbd-macro)
       (propertize " KMacro " 'face 'mode-line-highlight))))

(setq-local jf/mode-line-format/read-only-status
  '(:eval
     (let ((name (buffer-name)))
       (propertize
         (if buffer-read-only
           ;; TODO: Create mouse clickability to review filename
           (format " %s %s " (char-to-string #xE0A2) name)
           name)
         'face 'mode-line-emphasis))))

(defvar-local jf/mode-line-format/major-mode-name
  '(:eval
     (capitalize (symbol-name major-mode))))

(defvar-local jf/mode-line-format/narrow
  '(:eval
     (when (and (mode-line-window-selected-p)
             (buffer-narrowed-p)
             (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
       (propertize " Narrow " 'face 'mode-line-highlight))))

(defvar-local jf/mode-line-format/misc-info
  '(:eval
     (when (mode-line-window-selected-p)
       mode-line-misc-info)))

(with-eval-after-load 'eglot
  (setq mode-line-misc-info
    (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info)))

(defvar-local jf/mode-line-format/eglot
  `(:eval
     (when (and (featurep 'eglot) (mode-line-window-selected-p))
       '(eglot--managed-mode eglot--mode-line-format))))

(defvar-local jf/mode-line-format/vc-branch
  '(:eval
     (when-let* (((mode-line-window-selected-p))
                  (file (buffer-file-name))
                  (backend (vc-backend file))
                  (branch (jf/mode-line-format/vc-branch-name file backend)))
       (jf/mode-line-format/vc-details file branch))))

(defun jf/mode-line-format/vc-details (file branch)
  "Return the FILE and BRANCH."
  (concat
    (propertize (char-to-string #xE0A0) 'face 'shadow)
    " "
    branch))

(defun jf/mode-line-format/vc-branch-name (file backend)
  "Return VC branch name for FILE with BACKEND."
  (when-let ((rev (vc-working-revision file backend))
              (branch (or (vc-git--symbolic-ref file)
                        (substring rev 0 7))))
    branch))

(defvar-local jf/mode-line-format/flymake
  '(:eval
     (when (mode-line-window-selected-p)
       flymake-mode-line-format)))

(dolist (construct '(
                      jf/mode-line-format/eglot
                      jf/mode-line-format/flymake
                      jf/mode-line-format/kbd-macro
                      jf/mode-line-format/major-mode-name
                      jf/mode-line-format/misc-info
                      jf/mode-line-format/narrow
                      jf/mode-line-format/read-only-status
                      jf/mode-line-format/vc-branch
                      ))
  (put construct 'risky-local-variable t))

(setq keycast-mode-line-insert-after 'jf/mode-line-format/read-only-status)
(setq keycast-mode-line-remove-tail-elements nil)
(setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
(setq keycast-mode-line-format "%2s%k%c%R")

(setq-default mode-line-format '("%e"
                                  " "
                                  jf/mode-line-format/kbd-macro
                                  jf/mode-line-format/narrow
                                  jf/mode-line-format/read-only-status
                                  "  "
                                  jf/mode-line-format/major-mode-name
                                  "  "
                                  jf/mode-line-format/flymake
                                  "  "
                                  jf/mode-line-format/vc-branch
                                  "  "
                                  jf/mode-line-format/eglot
                                  "  "
                                  jf/mode-line-format/misc-info
                                  ))
