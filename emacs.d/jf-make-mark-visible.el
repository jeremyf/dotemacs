;;;; Make the mark visible, and the visibility toggleable. ('mmv' means 'make
;;;; mark visible'.) By Patrick Gundlach, Teemu Leisti, and Stefan.

(defface jf/make-mark-visible/face
  '((t :background "maroon2" :foreground "white"))
  "Face used for showing the mark's position.")

(defvar-local jf/make-mark-visible/mark-overlay nil
  "The overlay for showing the mark's position.")

(defvar-local jf/make-mark-visible/is-mark-visible t
  "The overlay is visible only when this variable's value is t.")

(defun jf/make-mark-visible/draw-mark (&rest _)
  "Make the mark's position stand out by means of a one-character-long overlay.
   If the value of variable `jf/make-mark-visible/is-mark-visible' is nil, the mark will be
   invisible."
  (unless jf/make-mark-visible/mark-overlay
    (setq jf/make-mark-visible/mark-overlay (make-overlay 0 0 nil t))
    (overlay-put jf/make-mark-visible/mark-overlay 'face 'jf/make-mark-visible/face))
  (let ((mark-position (mark t)))
    (cond
     ((null mark-position) (delete-overlay jf/make-mark-visible/mark-overlay))
     ((and (< mark-position (point-max))
           (not (eq ?\n (char-after mark-position))))
      (overlay-put jf/make-mark-visible/mark-overlay 'after-string nil)
      (move-overlay jf/make-mark-visible/mark-overlay mark-position (1+ mark-position)))
     (t
      ; This branch is called when the mark is at the end of a line or at the
      ; end of the buffer. We use a bit of trickery to avoid the higlight
      ; extending from the mark all the way to the right end of the frame.
      (overlay-put jf/make-mark-visible/mark-overlay 'after-string
                   (propertize " " 'face (overlay-get jf/make-mark-visible/mark-overlay 'face)))
      (move-overlay jf/make-mark-visible/mark-overlay mark-position mark-position)))))

(add-hook 'pre-redisplay-functions #'jf/make-mark-visible/draw-mark)

(defun jf/make-mark-visible/toggle-mark-visibility ()
  "Toggles the mark's visiblity and redraws it (whether invisible or visible)."
  (interactive)
  (setq jf/make-mark-visible/is-mark-visible (not jf/make-mark-visible/is-mark-visible))
  (if jf/make-mark-visible/is-mark-visible
      (set-face-attribute 'jf/make-mark-visible/face nil :background "maroon2" :foreground "white")
    (set-face-attribute 'jf/make-mark-visible/face nil :background 'unspecified :foreground 'unspecified))
  (jf/make-mark-visible/draw-mark))

(keymap-global-set "C-c v" 'jf/make-mark-visible/toggle-mark-visibility)
