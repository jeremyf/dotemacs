;;; org-charsheet --- An org-mode character sheet manager -*- lexical-binding: t -*-

;; Copyright (C) 2024 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; A package for managing character sheets via org-mode.  This code
;; implements aspects of "clocks" and "tracks" developed in Shawn
;; Tomkin's Ironsworn rules system.
;;
;;; Code:

(require 's)
(require 'org)

(defvar org-charsheet/clocks
  '("" "󰪞" "󰪟" "󰪠" "󰪡" "󰪢" "󰪣" "󰪤" "󰪥")
  "Useful progress tracking clocks in 1/8 intervals.")

(defvar org-charsheet/ticks-by-rank
  '(("troublesome" . 12)
     ("dangerous" . 8)
     ("formidable" . 4)
     ("extreme" . 2)
     ("epic" . 1))
  "An alist of track rank and the corresponding number of ticks.")

(defun org-charsheet/insert-clock (position)
  "Prompt for a clock POSITION then insert."
  (interactive (list
                 (completing-read
                   "Clock face: " org-charsheet/clocks nil t)))
  (insert position))

(defun org-charsheet/insert-clock-track (capacity)
  "Create a track of CAPACITY number of clocks."
  (interactive "nCapacity: ")
  (let ((track
          (s-repeat capacity "")))
    (insert
      (if current-prefix-arg
        (org-charsheet/clock-track-incremement
          :track track
          :ticks (read-number "Starting number of ticks: " 0))
        track))))

(defun org-charsheet/increment-progress (&optional element)
  "Increment 'PROGRESS' property for `org-mode' ELEMENT."
  (interactive)
  (save-excursion
    (if-let*
      ((title
         (if element
           (org-element-property :raw-value element)
           (completing-read "Progress Tracks: "
             (org-charsheet/headlines-with-property
               :property "PROGRESS")
             nil t)))
      (element
         (or element
           (car
             (org-charsheet/headlines-with-property
               :property "PROGRESS"
               :title title))))
        (new-rank
          (org-charsheet/increment-progress-for-element element)))
      (progn
        (message "Updating %s to %s" title new-rank)
        (org-entry-put element "PROGRESS" new-rank))
      (message "Headling %s; New Rank: %s" title new-rank))))

(cl-defun org-charsheet/headlines-with-property (&key property title)
  "Find all headlines with PROPERTY.

When you provide a TITLE limit the headlines to those titles that match.

Hopefully only one of them."
  (with-current-buffer (current-buffer)
    (org-element-map
      (org-element-parse-buffer)
      '(keyword node-property headline)
      (lambda (element)
        (and
          (org-element-type-p element 'headline)
          (not (s-blank? (org-entry-get element property)))
          (if title
            (string= (org-element-property :raw-value element) title)
            t)
          (org-element-property :title element))))))

(defun org-charsheet/increment-progress-for-element (element)
  "Given the ELEMENT adjust the 'PROGRESS' property.

Using the 'PROGRESS' and 'RANK' to calculate the incrementation."
  (if-let* ((progress
              (org-entry-get element "PROGRESS"))
             (rank
               (downcase (org-entry-get element "RANK")))
             (ticks
               (alist-get
                 (downcase rank)
                 org-charsheet/ticks-by-rank nil nil #'string=)))
    (org-charsheet/clock-track-incremement track ticks)
    (user-error "Element: %S; Progress: %S; Rank: %S"
      (org-element-property :raw-value element)
      progress rank)))

(defun org-charsheet/clock-track-incremement (track ticks)
  "Return a TRACK incremented by a number of TICKS."
  (let*((track
          ;; String empty spaces in the track
          (replace-regexp-in-string "[[:space:]]+" "" track))
         (len
           (length track))
         (capacity
           (* len 4))
         (initial-value
           (cl-reduce
             (lambda (acc clock)
               (+
                 acc
                 (cond
                   ((string= "" clock) 0)
                   ((string= "󰪟" clock) 1)
                   ((string= "󰪡" clock) 2)
                   ((string= "󰪣" clock) 3)
                   ((string= "󰪥" clock) 4)
                   (t
                     (user-error
                       "Expected %s to be a quarter-increment clock"
                       track)))))
             (split-string track "" t)
             :initial-value 0))
         (updated-value
           (+ initial-value ticks))
         (remainder (mod updated-value 4))
         (filled-clocks (/ updated-value 4)))
         (concat
             (s-repeat filled-clocks "󰪥")
             (cond
               ((= 0 remainder) "")
               ((= 1 remainder) "󰪟")
               ((= 2 remainder) "󰪡")
               ((= 3 remainder) "󰪣")
               ((= 4 remainder) "󰪥"))
             (s-repeat (- len filled-clocks 1) ""))))


(provide 'org-charsheet)
;;; org-charsheet.el ends here
