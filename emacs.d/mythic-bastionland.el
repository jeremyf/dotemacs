;;; mythic-bastionland ---  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; In playing a solo game of Mystic Bastionland, I realized that I was
;; spending considerable energy dealing with the travel procedures.  And
;; I realized what I needed is an opaque to me (the player) map.  One
;; that I could ask the following questions:
;;
;; - Does this Hex show evidence of a Myth? (e.g. there's a Myth in this
;;   square).
;; - What, if any, Landmark is present?
;; - What is the nearest Myth?
;; - What is the direction towards a Myth?
;; - Get a Myth that is not the closest.
;; - Moving from Hex A to Hex B, do I encounter a Barrier?
;;
;; And I realized that I could randomly generate a map, encode it in an
;; illegible manner, and query it throughout game play.  Now, how to
;; encode that map?
;;
;; I begin my research: https://www.redblobgames.com/grids/parts/; which
;; leads me to https://www.redblobgames.com/grids/hexagons/.
;;
;; For this implementation, I making hard and fast assumptions:
;;
;; - The map is 12 x 12.
;; - The map orientation is jagged side on the left and right; which
;;   means I'm using double height coordinates.
;; - I'm not randomly placing holdings, instead relying on other maps.
;; - Coordinates start at 0,0, which is the top-most left-most hex.
;;
;;; Future considerations
;;
;; - Replace the label at a given coordinate.  You'll need to specify
;;   type and coordinate (or perhaps current string).
;;
;; Further I want to make it hard to read the map.
(defun populate-map (holdings)
  "Populate the map with HOLDINGS.

HOLDINGS is a list of `cons' with `car' as label and `cdr' as
coordinates, as expressed in `prompt-for-coord'."
  (let ((locations-to-place '())
         ;; The already placed locations, preventing us from placing
         ;; other features at those coordinates.
         (locations
          (mapcar (lambda (h) (cons (cdr h) (car h))) holdings))
         (myths '())
         (sanctums '())
         (dwellings '())
         (monuments '())
         (hazards '())
         (curses '())
         (ruins '()))
    (dotimes (i 6)
      (add-to-list 'locations-to-place
        (cons 'myths (format "Myth %d" (+ 1 i)))))
    (dolist (cell '((sanctums . "Sanctum")
                     (dwellings . "Dwelling")
                     (monuments . "Monument")
                     (hazards . "Hazard")
                     (curses . "Curse")
                     (ruins . "Ruin")))
      (dotimes (i (+ 3 (random 2)))
        (add-to-list 'locations-to-place
          (cons (car cell) (format "%s %d" (cdr cell) (+ 1 i))))))

    ;; Here I make a decision on the placement algorithm.  Loop through
    ;; each place, then generate a random coordinate, and test if its
    ;; occupied.  If not, fill and and move to the next place.  If so,
    ;; get a new random coordinate.
    (dolist (to-place locations-to-place)
      (let ((keep-trying t))
        (while keep-trying
          (let ((candidate
                  (cons (random 12) (random 12))))
            (unless (assoc candidate locations)
              (progn
                (setq keep-trying nil)
                (add-to-list 'locations (cons candidate (cdr to-place)))
                (add-to-list (car to-place) (cons (cdr to-place) candidate))
                ))))))
    (list
      (cons 'locations locations)
      (cons 'myths myths)
      (cons 'sanctums sanctums)
      (cons 'dwellings dwellings)
      (cons 'monuments monuments)
      (cons 'hazards hazards)
      (cons 'curses curses)
      (cons 'ruins ruins)
      (cons 'holdings holdings))))

(defun mythic-bastionland-map-write ()
  (f-write
    (base64-encode-string
    (format "%S" mythic-bastionland-map)
    )
    'utf-8-unix
    "~/.local/bastionland.base64")
  (shell-command "cd ~/.local; sha256sum bastionland.base64 > bastionland.checksum"))

(defun mythic-bastionland-map ()
  (or mythic-bastionland-map
    (setq mythic-bastionland-map (mythic-bastionland-map-read))))

(defun mythic-bastionland-map-read ()
  (if (string= "bastionland.base64: OK\n"
        (shell-command-to-string
          "cd ~/.local; sha256sum -c bastionland.checksum"))
    (read
      (base64-decode-string
        (f-read
          "~/.local/bastionland.base64"
          'utf-8-unix)
      ))
    (error "We have an unexpected bastionland.base64 file")))


;; (setq mythic-bastionland-map
;;   (populate-map '(("Castle" . (9 . 4)) ("Tower" . (0 . 1)) ("Castle" . (0 . 2)))))

(defvar mythic-bastionland-map
  nil)

(defun prompt-for-coord ()
  "Prompt for Column and Row returning a `cons' cell.

Where `car' is the column and `cdr' is the row."
  (cons
        (read-number "Column: ")
        (read-number "Row: ")))

(defun hex-feature (coord)
  "Echo the feature, if any, at the given COORD.

See `prompt-for-coord'."
  (interactive
    (list
      (prompt-for-coord)))
  (message "%s"
    (or
      (cdr (assoc coord (assoc 'locations (mythic-bastionland-map))))
      "Nothing")))

;; https://www.redblobgames.com/grids/hexagons/#distances-doubled
(defun hex-distance (to from)
  "Both TO and FROM are `cons' as per `prompt-for-coord'."
  ;; We're assuming double height that is left/right side is saw-blade.
  (let ((dcol
          (abs (- (car to) (car from))))
         (drow
           (abs (* 2 (- (cdr to) (cdr from))))))
    (+ dcol (max 0 (/ (- drow dcol) 2)))))

(defun nearest-myth (coord)
  "Echo the nearest myth to COORD.

See `prompt-for-coord'."
  (interactive
    (list (prompt-for-coord)))
  (let* ((distances
           (myth-distances-from-closest-to-furthest coord))
          (shortest-distance
            (caar distances)))
    (message "%s"
      (cdr
      (seq-random-elt
        (seq-filter (lambda (cell)
                      (= shortest-distance (car cell)))
          distances))))))

(defun myth-distances-from-closest-to-furthest (from)
  "Sort distance FROM each myth into a list of `cons' cells.

The `car' is the integer distance and the `cdr' is the name of the
myth.`

Note the FROM is conformant to the `prompt-for-coord'."
  (seq-sort (lambda (l r) (< (car l) (car r)))
    (mapcar (lambda (myth)
              (cons (hex-distance (cdr myth) from) (car myth)))
      (cdr (assoc 'myths (mythic-bastionland-map))))))

(defun direction-to-myth (from myth)
  "Calculate the direction FROM to MYTH.

See `prompt-for-coord' for details of FROM."
  (interactive
    (list
      (prompt-for-coord)
      (let ((myths (cdr (assoc 'myths (mythic-bastionland-map)))))
        (assoc (completing-read "Myth: " myths nil t) myths))))
  (message "%s"
    (let* ((to (cdr myth)))
      (cond
        ((equal to from)
          "Under your nose")
        ((= (car from) (car to))
          "North/South")
        (t (let ((slope (/
                          ;; With double-height grids we need to double
                          ;; the row distances.
                          (float (- (* 2 (cdr to)) (* 2 (cdr from))))
                          (float (- (car to) (car from))))))
             (cond
               ;; TODO: Determine which direction.
               ((or (>= slope 2) (<= slope -2))
                 "North/South")
               ((< 0.5 slope 2.0)
                 "Northeast/Southwest")
               ((<= -0.5 slope 0.5)
                 "East/West")
               ((< -2 slope -0.5)
                 "Northwest/Southeast"))))))))
;;; Code:

(provide 'mythic-bastionland)
;;; mythic-bastionland.el ends hereR
