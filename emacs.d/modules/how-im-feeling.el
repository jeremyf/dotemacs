;;; how-im-feeling --- How I'm and What to Do -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;; A simple package to reflect on how I'm feeling and provide a
;; response.  With a goal of getting unstuck.

;;; Code:

(defvar how-im-feeling-responses
  '(("Angry" . "Lift weights")
    ("Stressed" . "Go for a walk")
    ("Procrastinating" . "Set a 10-minute timer")
    ("Sad" . "Get sunlight")
    ("Can’t Focus" . "Clean your workspace")
    ("Negative thoughts" . "Write 3 gratitudes")
    ("Stuck" . "Change your environment")
    ("Financial stress" . "Build an emergency fund")
    ("Low Energy" . "Fix your sleep")
    ("Overthinking" . "Journal it out")
    ("Lonely" . "Call someone")
    ("No motivation" . "Start with 2 minutes")
    ("Anxiety" . "Slow your breathing")
    ("Brain fog" . "Drink water and move")
    ("Low confidence" . "Keep small promises")
    ("Lost" . "Define one clear goal"))
  "How I'm feeling and a simple response to move with that feeling")

(defun how-im-feeling ()
  "Prompt for how I'm feeling and respond with what to do."
  (interactive)
  (let ((feels
         (completing-read-multiple "I'm Feeling: "
                                   how-im-feeling-responses
                                   nil
                                   t))
        (concatter
         (lambda (feel)
           (format "- *%s:* %s"
                   feel
                   (alist-get feel how-im-feeling-responses nil nil #'string=)))))
    (insert (format "I'm feeling:\n\n%s\n" (mapconcat concatter feels "\n")))))


(provide 'how-im-feeling)
;;; how-im-feeling.el ends here
