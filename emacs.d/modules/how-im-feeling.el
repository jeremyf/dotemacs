;;; how-im-feeling --- How I'm and What to Do -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;; A simple package to reflect on how I'm feeling and provide a
;; response.  With a goal of getting unstuck.

;;; Code:

(defvar how-im-processing-responses
  '((
     ;; Four response
     "Why are you so emotional?" .
     ("What are you hoping your partner understands that they may be missing?"
      "What need is underneath this emotion?"
      "When you increase the intensity, what are you hoping will happen?"
      "What feels most painful when your partner becomes quiet?"
      "What story do you tell yourself when they withdraw?"
      "What helps you feel emotionally chosen?"))
    ;; Nine response
    ("Why didn't you say anything?" .
     ("What happened inside you just before you became quiet?"
      "What were you hoping would happen if you stayed silent?"
      "What emotion feels hardest to bring into this relationship?"
      "What do you worry would happen if you expressed your disagreement?"
      "How do you know when you've disappeared in the relationship?"
      "When you shut down, what are you protecting?"
      "If conflict didn't threaten connection, what would you say")))
  "Prompts to help me reformulate questions.")

(defun how-im-processing ()
  "Short-circuit questions I'm processing with appropriate questions.

These are based on specific Enneagram types."
  (interactive)
  (let ((questions
         (completing-read-multiple "Questions I'm asking myself: "
                                   how-im-processing-responses
                                   #'completing-read-omit-p
                                   t))
        (concatter
         (lambda (question)
           (format "- *%s*\n  - %s"
                   question
                   (s-join "\n  - "
                   (alist-get question
                              how-im-processing-responses
                              nil nil #'string=))))))
    (insert
     (format "Questions I'm asking myself:\n\n%s\n"
             (mapconcat concatter questions "\n")))))

(defvar how-im-feeling-responses
  '(("Angry" . "Lift weights")
    ("Stressed" . "Go for a walk")
    ("Procrastinating" . "Set a 10-minute timer")
    ("Sad" . "Get sunlight")
    ("Can't focus" . "Clean your workspace")
    ("Negative thoughts" . "Write 3 gratitudes")
    ("Stuck" . "Change your environment")
    ("Financial stress" . "Build an emergency fund")
    ("Low energy" . "Fix your sleep")
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
                                   #'completing-read-omit-p
                                   t))
        (concatter
         (lambda (feel)
           (format "- *%s:%s"
                   feel
                   (alist-get feel
                              how-im-feeling-responses
                              nil nil #'string=)))))
    (insert
     (format "I'm feeling:\n\n%s\n"
             (mapconcat concatter feels "\n")))))

(defun completing-read-omit-p (thusfar)
  "Omit completions THUSFAR given."
  (let ((input
         (butlast (split-string
                   (minibuffer-contents-no-properties)
                   crm-separator)))
        (test
         (car (last (split-string thusfar crm-separator)))))
    (not (member test input))))

(provide 'how-im-feeling)
;;; how-im-feeling.el ends here
