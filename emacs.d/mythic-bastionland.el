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

;;; Code:
(defun mythic-bastionland-map-generate (holdings)
  "Populate the map with HOLDINGS.

HOLDINGS is a list of `cons' with `car' as label and `cdr' as
coordinates, as expressed in `mythic-bastionland--prompt-for-coord'."
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
    ;; occupied.
    ;;
    ;; When occupied, get a new random coordinate and try again.
    ;; When unoccupied, add to the list and move to the next feature.
    (dolist (to-place locations-to-place)
      (let ((keep-trying t))
        (while keep-trying
          (let ((coord
                  (cons (random 12) (random 12))))
            (unless (assoc coord locations)
              (let ((label (cdr to-place)))
                (setq keep-trying nil)
                ;; For locations we favor storing the (coord . label)
                (add-to-list 'locations
                  (cons coord label))
                ;; For the specific feature, we favor storing (label
                ;; . coord)
                (add-to-list (car to-place)
                  (cons label coord))))))))
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
      (format "%S" mythic-bastionland-map))
    'utf-8-unix
    "~/.local/bastionland.base64")
  (shell-command "cd ~/.local; sha256sum mythic-bastionland.base64 > mythic-bastionland.checksum"))

(defun mythic-bastionland-map ()
  (or mythic-bastionland-map
    (setq mythic-bastionland-map (mythic-bastionland-map-read))))

(defun mythic-bastionland-map-read ()
  (if (string= "bastionland.base64: OK\n"
        (shell-command-to-string
          "cd ~/.local; sha256sum -c mythic-bastionland.checksum"))
    (read
      (base64-decode-string
        (f-read
          "~/.local/bastionland.base64"
          'utf-8-unix)
        ))
    (error "We have an unexpected bastionland.base64 file")))


;; (setq mythic-bastionland-map
;;   (mythic-bastionland-map-generate '(("Castle" . (9 . 4)) ("Tower" . (0 . 1)) ("Castle" . (0 . 2)))))

(defvar mythic-bastionland-map
  nil)

(defun mythic-bastionland--prompt-for-coord ()
  "Prompt for Column and Row returning a `cons' cell.

Where `car' is the column and `cdr' is the row."
  (cons
    (read-number "Column: ")
    (read-number "Row: ")))

(defun mythic-bastionland-hex-feature (coord)
  "Echo the feature, if any, at the given COORD.

See `mythic-bastionland--prompt-for-coord'."
  (interactive
    (list
      (mythic-bastionland--prompt-for-coord)))
  (message "%s"
    (or
      (cdr (assoc coord (assoc 'locations (mythic-bastionland-map))))
      "Nothing")))

;; https://www.redblobgames.com/grids/hexagons/#distances-doubled
(defun mythic-bastionland--hex-distance (to from)
  "Both TO and FROM are `cons' as per
`mythic-bastionland--prompt-for-coord'."
  ;; We're assuming double height that is left/right side is saw-blade.
  (let ((dcol
          (abs (- (car to) (car from))))
         (drow
           (abs (* 2 (- (cdr to) (cdr from))))))
    (+ dcol (max 0 (/ (- drow dcol) 2)))))

(defun mythic-bastionland-nearest-myth (coord)
  "Echo the nearest myth to COORD.

See `mythic-bastionland--prompt-for-coord'."
  (interactive
    (list (mythic-bastionland--prompt-for-coord)))
  (mythic-bastionland--myth-by-strategy coord 'nearest))

(defun mythic-bastionland-not-nearest-myth (coord)
  "Echo the nearest myth to COORD.

See `mythic-bastionland--prompt-for-coord'."
  (interactive
    (list (mythic-bastionland--prompt-for-coord)))
  (mythic-bastionland--myth-by-strategy coord 'not-nearest))

(defun mythic-bastionland--myth-by-strategy (coord strategy)
  "Determine the myth that matches the STRATEGY relative to COORD.

Expected strategies are:
- nearest
- not-nearest."
  (let* ((distances
           (mythic-bastionland--myth-distances coord))
          (shortest-distance
            (caar distances)))
    (message "%s"
      (cdr
        (seq-random-elt
          (seq-filter
            (lambda (cell)
              (pcase strategy
                ('nearest (= shortest-distance (car cell)))
                ('not-nearest
                  (not (= shortest-distance (car cell))))
                (_
                  (user-error "Unknown stratgegy %s" strategy))))
            distances))))))

(defun mythic-bastionland--myth-distances (from)
  "Sort distance FROM each myth into a list of `cons' cells.

We return the closest first.

The `car' is the integer distance and the `cdr' is the name of the
myth.`

Note the FROM is conformant to the `mythic-bastionland--prompt-for-coord'."
  (seq-sort (lambda (l r) (< (car l) (car r)))
    (mapcar (lambda (myth)
              (cons (mythic-bastionland--hex-distance (cdr myth) from)
                (car myth)))
      (cdr (assoc 'myths (mythic-bastionland-map))))))

(defun mythic-bastionland-direction (from myth)
  "Provide the direction FROM coordinate to the MYTH.

See `mythic-bastionland--prompt-for-coord' for details of FROM."
  (interactive
    (list
      (mythic-bastionland--prompt-for-coord)
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
(provide 'mythic-bastionland)
;;; mythic-bastionland.el ends here
