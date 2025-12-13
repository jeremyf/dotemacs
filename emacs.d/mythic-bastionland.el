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
;; - What is the direction towards a Myth (or other features perhaps)?
;; - What is a Myth that is not the closest?
;; - Do I encounter a Barrier moving from Hex A to Hex B (or B to A)?
;;
;; And I realized that I could randomly generate a map, encode it in an
;; illegible manner, and query it throughout game play.  I considered
;; using a sqlite database, but opted for an in-memory structure that I
;; could read and write to the file system.  That does mean, while
;; playing, that I could "M-x describe-variable".  But I'll just not do
;; that.  While building out these features in the REPL having
;; a view of the variable proved valuable.
;;
;; Many thanks to https://www.redblobgames.com/grids/hexagons/ for a
;; useful crash course in the mathematics of hex mapping.
;;
;; For this implementation, I am making the following assumptions:
;;
;; - The map is 12 x 12.

;; - The map orientation is jagged side on the left and right; which
;;   means double height coordinates (using redbloggames.com's
;;   nomenclature)
;; - I'm not randomly placing holdings, instead relying
;;   on another visual map.
;; - Coordinates start at 0,0, which is the top-most left-most hex.
;; - Any prompts for coordinates use single height coordinates starting
;;   at 0,0.  That means 1,0 is the hex one to the right and half a hex
;;   down from the 0,0 coordinate.  The 1,0 coord will be translated to
;;   1,1 for storage and interactions.
;;
;; Further I want to make it hard to read the map.

;;; Examples of `mythic-bastionland-map' data:
;;
;; ((locations ((10 . 14) . "Myth 1") ((1 . 15) . "Myth 2") ((9 . 21) . "Myth 3")
;;             ((5 . 9) . "The Wall") ((3 . 5) . "Myth 5") ((1 . 13) . "Myth 6")
;;             ((4 . 0) . "Sanctum 1") ((8 . 22) . "Sanctum 2")
;;             ((4 . 2) . "Sanctum 3") ((10 . 6) . "Sanctum 4")
;;             ((0 . 10) . "Dwelling 1") ((2 . 14) . "Dwelling 2")
;;             ((0 . 2) . "Dwelling 3") ((11 . 5) . "Dwelling 4")
;;             ((5 . 19) . "Monument 1") ((0 . 20) . "Monument 2")
;;             ((1 . 11) . "Monument 3") ((2 . 22) . "Monument 4")
;;             ((8 . 10) . "Hazard 1") ((5 . 21) . "Hazard 2")
;;             ((7 . 13) . "Hazard 3") ((7 . 7) . "Curse 1") ((8 . 18) . "Curse 2")
;;             ((4 . 20) . "Curse 3") ((9 . 9) . "Ruin 1") ((10 . 8) . "Ruin 2")
;;             ((1 . 5) . "Ruin 3") ((11 . 17) . "Ruin 4") ((9 . 3) . "Tower")
;;             ((5 . 7) . "Castle") ((1 . 19) . "Fortress") ((8 . 16) . "Town"))
;;  (myths ("Myth 1" 10 . 14) ("Myth 2" 1 . 15) ("Myth 3" 9 . 21)
;;         ("The Wall" 5 . 9) ("Myth 5" 3 . 5) ("Myth 6" 1 . 13))
;;  (sanctums ("Sanctum 1" 4 . 0) ("Sanctum 2" 8 . 22) ("Sanctum 3" 4 . 2)
;;            ("Sanctum 4" 10 . 6))
;;  (dwellings ("Dwelling 1" 0 . 10) ("Dwelling 2" 2 . 14) ("Dwelling 3" 0 . 2)
;;             ("Dwelling 4" 11 . 5))
;;  (monuments ("Monument 1" 5 . 19) ("Monument 2" 0 . 20) ("Monument 3" 1 . 11)
;;             ("Monument 4" 2 . 22))
;;  (hazards ("Hazard 1" 8 . 10) ("Hazard 2" 5 . 21) ("Hazard 3" 7 . 13))
;;  (curses ("Curse 1" 7 . 7) ("Curse 2" 8 . 18) ("Curse 3" 4 . 20))
;;  (ruins ("Ruin 1" 9 . 9) ("Ruin 2" 10 . 8) ("Ruin 3" 1 . 5) ("Ruin 4" 11 . 17))
;;  (barriers (left (10 . 22) right (9 . 21)) (left (6 . 22) right (6 . 20))
;;            (left (5 . 1) right (6 . 0)) (left (10 . 16) right (10 . 14))
;;            (left (6 . 8) right (6 . 6)) (left (4 . 8) right (4 . 6))
;;            (left (7 . 11) right (6 . 12)) (left (5 . 7) right (5 . 5))
;;            (left (5 . 19) right (5 . 17)) (left (7 . 5) right (7 . 3))
;;            (left (4 . 16) right (3 . 17)) (left (8 . 4) right (7 . 5))
;;            (left (9 . 17) right (8 . 16)) (left (9 . 1) right (8 . 0))
;;            (left (1 . 19) right (1 . 17)) (left (6 . 22) right (5 . 21))
;;            (left (11 . 9) right (10 . 8)) (left (8 . 4) right (9 . 3))
;;            (left (7 . 9) right (6 . 8)) (left (8 . 12) right (8 . 10))
;;            (left (9 . 11) right (8 . 12)) (left (7 . 7) right (6 . 6))
;;            (left (11 . 5) right (10 . 6)) (left (9 . 1) right (10 . 0)))
;;  (holdings ("Tower" 9 . 3) ("Castle" 5 . 7) ("Fortress" 1 . 19) ("Town" 8 . 16)))

;;; Generating Map with Example Config:

;; (defalias 'mbc 'mythic-bastionland--col-row-to-coord)
;; ;; Based on the presented game logic and reviewing the map, there are 6
;; ;; places where the Mountain could be and still allow for the Beast to
;; ;; also be along the shores of the lake.
;; (let* (
;;        ;; This is where Sir Weydlyn encountered Seer Tompot.
;;        (sanctums
;;         `(("Tompot (Tangled Seer)" . ,(mbc 8 1))))
;;        ;; Based on Sir Weydlyn's encountered a "nearest myth" event,
;;        ;; These are the coordinates in which no myth can occur.  The
;;        ;; `car' (e.g. 1) means that The Beast myth is within one hex of
;;        ;; the place where Sir Weydlyn encountered that first sign.
;;        (myth-omits `((1 . (,(mbc 9 2) ,(mbc 10 1) ,(mbc 8 1)
;;                            ,(mbc 10 2) ,(mbc 9 0) ,(mbc 8 2)))
;;                      (2 . (,(mbc 8 0) ,(mbc 10 0) ,(mbc 11 0)
;;                            ,(mbc 11 1) ,(mbc 11 2) ,(mbc 10 3)
;;                            ,(mbc 9 3) ,(mbc 8 3) ,(mbc 7 2)
;;                            ,(mbc 7 1) ,(mbc 7 0)))
;;                      (3 . (,(mbc 6 0) ,(mbc 6 1) ,(mbc 6 2)
;;                            ,(mbc 6 3) ,(mbc 7 3) ,(mbc 8 4)
;;                            ,(mbc 9 4) ,(mbc 10 4) ,(mbc 11 3)))))
;;        ;; a list of lists; each element's `car' is where the Mountain is
;;        ;; and each elements `cdr' is a list of where the Beast could be,
;;        ;; based on the two points of information.
;;        (scenarios
;;         `((,(mbc 10 4) . ((myth . (,(mbc 9 4) ,(mbc 8 5)))
;;                           (omit . 2)))
;;           (,(mbc 8 4) . ((myth . (,(mbc 9 5) ,(mbc 8 5)))
;;                          (omit . 2)))
;;           (,(mbc 9 2) . ((myth . (,(mbc 8 3) ,(mbc 8 4) ,(mbc 8 5)
;;                                   ,(mbc 9 3) ,(mbc 9 4), (mbc 9 5)))
;;                          (omit . nil)))
;;           (,(mbc 9 3) . ((myth . (,(mbc 8 4) ,(mbc 8 5) ,(mbc 9 4)))
;;                          (omit . 1)))
;;           (,(mbc 9 4) .  ((myth . (,(mbc 8 4)))
;;                           (omit . 2)))
;;           (,(mbc 9 5) . ((myth . (,(mbc 8 5)))
;;                          (omit . 3)))))
;;        ;; Now pick a random scenario
;;        (scenario
;;         (seq-random-elt scenarios)))
;;   (mythic-bastionland-map-generate
;;    `(
;;      ;; With the chosen random scenario, we assign the Moutain, then
;;      ;; pick a random one for the Beast
;;      (myths .
;;       (("The Mountain" . ,(car scenario))
;;        ("The Beast" . ,(seq-random-elt
;;                         (alist-get 'myth (cdr scenario))))))
;;      ;; The random map player facing map I started with, had these
;;      ;; locations.
;;      (holdings .
;;       (("Tower" . (9 . 3)) ("Castle" . (5 . 7))
;;        ("Fortress" . (1 . 19)) ("Town" . (8 . 16))))
;;      (omit . (
;;             ;; Sir Wedylyn crossed between these two potential
;;             ;; barriers.
;;             (barriers .
;;                       ((,(mbc 8 1) . ,(mbc 9 1))
;;                        (,(mbc 9 1) . ,(mbc 8 2))))
;;             ;; With myths chosen, now assign where myths cannot be.
;;             (myths . ,(alist-get
;;                        (alist-get 'omit (cdr scenario))
;;                        myth-omits)))))))


;;
;;; Code:

(defvar mythic-bastionland-features
  '((myths . (6)) (holdings . (4))
     (sanctums . (3 4)) (monuments . (3 4)) (dwellings . (3 4))
     (hazards . (3 4)) (curses . (3 4)) (ruins . (3 4)))
  "Feature types that are labeled, and thus renameable.  We also encode
how many of these, in total, are to be placed.")

(defun mythic-bastionland-map-generate (config)
  "Generate and store `mythic-bastionland-map' via CONFIG.

See `mythic-bastionland-features' for some of the `car' values of
CONFIG.  Another is `barriers' (which are unamed).  Another is `omit',
itself an alist, with the same `car' values as those in CONFIG (except
`omit').

As of <2025-12-12 Fri> adding `barriers' is not handled.  It is a
feature I'd like to implement."
  (let ((barriers nil)
         (locations nil)
         (locations-to-place nil)
         (the-map nil))

    ;; First put the locations on the map...no effort is taken to avoid
    ;; location collisions.  Also, queue up further locations to place.
    (cl-loop for (feature . count) in mythic-bastionland-features do
      (let ((feat-locations
              (alist-get feature config)))
        ;; When we are given locations for this feature type, add it
        ;; to the placed list.
        (when feat-locations
          (cl-loop for (label . coord) in feat-locations do
            (cl-pushnew (cons coord label) locations)))

        ;; Next queue up placing the remainder of locations for the
        ;; feature type (accounting for what was already given).
        (dotimes (i (- (seq-random-elt count) (length feat-locations)))
          (cl-pushnew (cons feature
                        (format "%s %s" feature (+ i 1)))
            locations-to-place))

        (cl-pushnew (cons feature feat-locations) the-map)))

    ;; Now that we have our task list of what all needs adding.
    ;;
    ;; This involves avoiding collisions with other placed features as
    ;; well as heading the guidance of an omit coordinates for the given
    ;; feature.
    (cl-loop for (feature . label) in locations-to-place do
      (let ((keep-trying t)
             (omitted-feature-coordinates
               (alist-get feature (alist-get 'omit the-map))))
        (while keep-trying
          (let ((coord
                  (mythic-bastionland--col-row-to-coord
                    (random 12) (random 12))))
            (when (not (or
                         (assoc coord locations)
                         (member coord omitted-feature-coordinates)
                         ))
              (progn
                (setq keep-trying nil)
                ;; For locations we favor storing the (coord . label)
                (cl-pushnew (cons coord label) locations)
                ;; TODO add to list
                (cl-pushnew (cons label coord)
                  (alist-get feature the-map))))))))

    ;; And last, we handle the barriers as they are a bit of a different
    ;; creature.  We generate them by placing them between two
    ;; neighboring hexes.
    ;;
    ;; I have given special consideration for hexes on the edge of the
    ;; map; Namely don't create barriers on the edges.  And
    ;; proportionally reduce the chance of adding a barrier on those
    ;; edges proportional to the number sides that the hex has on-map
    ;; neighbors."
    (let ((omitted-barriers
            (mapcar
              (lambda (b)
                (mythic-bastionland--make-ordered-pair
                  (car b) (cdr b)))
                (alist-get 'barriers (alist-get 'omit config)))))
      (dotimes (i (+ 23 (random 3)))
        (let ((keep-trying t))
          (while keep-trying
            (let* ((coord
                     (mythic-bastionland--col-row-to-coord
                       (random 12) (random 12)))
                    (in-6-chance
                      (cond
                        ((member coord '((0 . 0) (11 . 22)))
                          ;; top-left, bottom-right
                          2)
                        ((member coord '((11 . 0) (0 . 22)))
                          ;; top-right, bottom-right
                          3)
                        ((member (car coord) '(0 11))
                          ;; from or to
                          4)
                        ((member (cdr coord) '(0 23))
                          ;; top of col that is taller; bottom of col
                          ;; that is shorter
                          3)
                        ((member (cdr coord) '(1 22))
                          ;; top of col that is shorter; bottom of col
                          ;; that is taller
                          5)
                        (t 6))))
              (when (<= (+ 1 (random 6)) in-6-chance)
                (progn
                  (let* ((neighbor
                           (seq-random-elt
                             (mythic-bastionland--neighbors coord)))
                          (pair
                            (mythic-bastionland--make-ordered-pair
                              coord neighbor)))
                    ;; Don't repeat barriers
                    (when
                      (not (or (member pair barriers)
                             (member pair omitted-barriers)))
                      (progn
                        (cl-pushnew pair barriers)
                        (setq keep-trying nil)))))))))))

    ;; PS...make sure we add the locations and barriers to the map.
    (cl-pushnew `(locations . ,locations) the-map)
    (cl-pushnew `(barriers . ,barriers) the-map)
    (setq mythic-bastionland-map the-map)
    (mythic-bastionland-map-write)))

(defun mythic-bastionland-feature-rename ()
  "Re-label `mythic-bastionland' feature."
  (interactive)
  (let* ((feature-type
           (intern
             (completing-read "Feature Type: "
               mythic-bastionland-features
               nil t)))
          (features
            (alist-get feature-type (mythic-bastionland-map)))
          (current-label
            (completing-read "Current Label: " features nil t))
          (new-label
            (read-string "New Label: " current-label)))
    (let ((feature
            (assoc current-label features))
           (locations
             (alist-get 'locations (mythic-bastionland-map))))
      ;; Update the feature's label
      (setf (car feature) new-label)
      ;; Update the corresponding location.
      (setf (cdr (assoc (cdr feature) locations)) new-label)
      ;; And persist that information.
      (mythic-bastionland-map-write))))

(defun mythic-bastionland-map-write ()
  "Write a base64 encoded version of the map."
  (f-write
    (base64-encode-string
      (format "%S" mythic-bastionland-map))
    'utf-8-unix
    "~/.local/state/mythic-bastionland.base64")
  (shell-command "cd ~/.local/state; sha256sum mythic-bastionland.base64 > mythic-bastionland.checksum"))

(defun mythic-bastionland-map ()
  "Use the in-memory map or load one from the file system."
  (or mythic-bastionland-map
    (setq mythic-bastionland-map (mythic-bastionland-map-read))))

(defun mythic-bastionland-map-read ()
  "Load the base64 encoded map into an object."
  (if (string= "bastionland.base64: OK\n"
        (shell-command-to-string
          "cd ~/.local/state/; sha256sum -c mythic-bastionland.checksum"))
    (read
      (base64-decode-string
        (f-read
          "~/.local/state/mythic-bastionland.base64"
          'utf-8-unix)
        ))
    (error "We have an unexpected bastionland.base64 file")))

(defvar mythic-bastionland-map
  nil
  "DO NOT SEEK THE TREASURE!

Here lies your working copy of the map.  Glancing at it could provide
a hint as to the secrets of your game.")

(defun mythic-bastionland--col-row-to-coord (&optional column row prefix)
  "Prompt for COLUMN and ROW returning a `cons' cell.

The coordinates are coerced using the \"double-height\" encoding.
Where `car' is the column and `cdr' is the row.

PREFIX is the prompt prefix."
  (let ((c (or column
             (read-number (concat prefix "Column: "))))
         (r (or row
              (read-number (concat prefix "Row: ")))))
    (if (= 0 (mod c 2))
      (cons c (* 2 r))
      (cons c (+ 1 (* 2 r))))))

(defun mythic-bastionland-hex-feature (coord)
  "Echo the feature, if any, at the given COORD.

See `mythic-bastionland--col-row-to-coord'."
  (interactive
    (list
      (mythic-bastionland--col-row-to-coord)))
  (message "%s"
    (or
      (cdr (assoc coord (assoc 'locations (mythic-bastionland-map))))
      "Nothing")))

;; https://www.redblobgames.com/grids/hexagons/#distances-doubled
(defun mythic-bastionland--hex-distance (to from)
  "Both TO and FROM are `cons' as per
`mythic-bastionland--col-row-to-coord'."
  ;; We're assuming double height that is left/right side is saw-blade.
  (let ((dcol
          (abs (- (car to) (car from))))
         (drow
           (abs (- (cdr to) (cdr from)))))
    (+ dcol (max 0 (/ (- drow dcol) 2)))))

(defun mythic-bastionland-nearest-myth (coord)
  "Echo the nearest myth to COORD.

See `mythic-bastionland--col-row-to-coord'."
  (interactive
    (list (mythic-bastionland--col-row-to-coord)))
  (message "%s"
    (mythic-bastionland--myth-by-strategy coord 'nearest)))

(defun mythic-bastionland-not-nearest-myth (coord)
  "Echo the nearest myth to COORD.

See `mythic-bastionland--col-row-to-coord'."
  (interactive
    (list (mythic-bastionland--col-row-to-coord)))
  (message "%s"
    (mythic-bastionland--myth-by-strategy coord 'not-nearest)))

(defun mythic-bastionland--myth-by-strategy (coord strategy)
  "Determine the myth that matches the STRATEGY relative to COORD.

Expected strategies are:
- nearest
- not-nearest."
  (let* ((distances
           (mythic-bastionland--myth-distances coord))
          (shortest-distance
            (caar distances)))
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
          distances)))))

(defun mythic-bastionland-barrier-between (from to)
  "Answer if there is a barrier goint FROM hex TO hex."
  (interactive
    (list (mythic-bastionland--col-row-to-coord nil nil "From ")
      (mythic-bastionland--col-row-to-coord nil nil "To ")))
  (if (member
        (mythic-bastionland--make-ordered-pair from to)
        (alist-get 'barriers (mythic-bastionland-map)))
    (user-error "You Shall Not Pass!")
    (message "Passthrough on through")))

(defun mythic-bastionland--myth-distances (from)
  "Sort distance FROM each myth into a list of `cons' cells.

We return the closest first.

The `car' is the integer distance and the `cdr' is the name of the
myth.`

Note the FROM is conformant to the
`mythic-bastionland--col-row-to-coord'."
  (seq-sort (lambda (l r) (< (car l) (car r)))
    (mapcar (lambda (myth)
              (cons (mythic-bastionland--hex-distance (cdr myth) from)
                (car myth)))
      (cdr (assoc 'myths (mythic-bastionland-map))))))

(defun mythic-bastionland--neighbors (&optional coord)
  "Calculate the neighbors of the given COORD.

Neighboring coordinates that are not on the map are omitted."
  ;; See https://www.redblobgames.com/grids/hexagons/#neighbors-doubled
  (let ((coord
          (or coord
            (mythic-bastionland--col-row-to-coord)))
         (neighbors nil))
    (dolist (delta '((0 . -2) (1 . -1)
                      (1 . 1) (0 . 2)
                      (-1 . 1) (-1 . -1)))
      (let ((col
              (+ (car coord) (car delta)))
             (row
               (+ (cdr coord) (cdr delta))))
        (when (and (<= 0 col 11) (<= 0 row 23))
          (cl-pushnew (cons col row) neighbors))))
    neighbors))

(defun mythic-bastionland-direction (from myth)
  "Provide the direction FROM coordinate to the MYTH.

See `mythic-bastionland--col-row-to-coord' for details of FROM."
  (interactive
    (list
      (mythic-bastionland--col-row-to-coord)
      (let ((myths (cdr (assoc 'myths (mythic-bastionland-map)))))
        (assoc (completing-read "Myth: " myths nil t) myths))))
  (message "%s is %s"
    (car myth)
    (mythic-bastionland--direction from (cdr myth))))

(defun mythic-bastionland--direction (&optional from to)
  "Get human-readable direction FROM TO."
  (let ((from
          (or from
            (mythic-bastionland--col-row-to-coord nil nil "From ")))
         (to
           (or to
             (mythic-bastionland--col-row-to-coord nil nil "To "))))
      (cond
        ((equal to from)
          "Under your nose")
        ((= (car from) (car to))
          (if (> (cdr from) (cdr to))
            "North" "South"))
        (t (let ((slope (/
                          (float (- (cdr to) (cdr from)))
                          (float (- (car to) (car from))))))
             (cond
               ;; After compass, protractor, marker, and spreadsheet
               ;; work, I'm happy with the direction calculations.
               ;; Remember, hex maps starting from top-left instead
               ;; of bottom right like Geometry means things get a
               ;; mind bending (at least for this old guy).
               ((or (> slope 4) (< slope -4))
                 (if (> (cdr from) (cdr to))
                   "North" "South"))
               ((<= 0.8 slope) (<= slope 4)
                 (if (> (cdr from) (cdr to))
                   "Northwest" "Southeast"))
               ((< -0.8 slope 0.8)
                 (if (> (car from) (car to))
                   "West" "East"))
               ((<= -4 slope -0.8)
                 (if (> (cdr from) (cdr to))
                   "Northeast" "Southwest"))))))))

(defun mythic-bastionland--make-ordered-pair (from to)
  "Provide a consistent sort order FROM and TO coordinates."
  (let ((from
          (or from
            (mythic-bastionland--col-row-to-coord nil nil "Left ")))
         (to
           (or to
             (mythic-bastionland--col-row-to-coord nil nil "Right "))))
    (if (> (car from) (car to))
      `(from ,from to ,to)
      (if (> (cdr from) (cdr to))
        `(from ,from to ,to)
        `(from ,to to ,from)))))

(provide 'mythic-bastionland)
;;; mythic-bastionland.el ends here
