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

(defvar-local jf/mode-line-format/read-only-status
  '(:eval
     (let ((name (buffer-name)))
       (propertize
         (if buffer-read-only
           (format " %s %s " (char-to-string #xE0A2) name)
           name)
         'face 'mode-line-buffer-id))))

(defvar-local jf/mode-line-format/major-mode-name
  '(:eval
     (propertize (capitalize (symbol-name major-mode)) 'face 'mode-line-emphasis)))

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

(dolist (construct '(
                      jf/mode-line-format/kbd-macro
                      jf/mode-line-format/narrow
                      jf/mode-line-format/read-only-status
                      jf/mode-line-format/major-mode-name
                      jf/mode-line-format/narrow
                      jf/mode-line-format/misc-info
                      jf/mode-line-format/eglot
                      ))
  (put construct 'risky-local-variable t))

(setq-default mode-line-format '("%e"
                                  jf/mode-line-format/kbd-macro
                                  jf/mode-line-format/narrow
                                  jf/mode-line-format/read-only-status
                                  jf/mode-line-format/major-mode-name
                                  flymake-mode-line-format
                                  jf/mode-line-format/eglot
                                  jf/mode-line-format/misc-info
                                  ))
