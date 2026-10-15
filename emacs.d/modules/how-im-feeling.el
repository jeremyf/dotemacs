;;; how-im-feeling --- How I'm and What to Do -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;; A simple package to reflect on how I'm feeling and provide a
;; response.  With a goal of getting unstuck.

;;; Code:

(defvar how-im-processing-responses
  '(
    ;; Moving towards a one response.
    ("How can I ethically move towards my conscience?" .
     ("What action would be consistent with my values, even if I feel emotionally activated?"
      "What is my responsibility in this situation, and what belongs to my partner?"
      "Can I respond with honesty and integrity without needing to control the outcome?"
      "What would it look like to communicate clearly rather than reactively?"
      "Am I acting in a way that reflects the partner I want to be?"
      "What is the most constructive action available to me right now?"
      "Can I hold both the validity of my feelings and the possibility that my interpretation may be incomplete?"
      "What would intentionality look like here, rather than urgency?"))
     ;; Creating a positive type 2 response.
    ("Why am I continuing to pursue?" .
     ("What am I hoping will happen if I continue this conversation right now?"
      "Have I communicated my need clearly, or am I repeating myself because I haven't received the response I'm hoping for?"
      "Is my current approach creating more opportunity for connection, or is it increasing pressure on my partner?"
      "Can I give my partner space without abandoning my own needs or allowing the issue to disappear?"
      "What specific request could I make instead of continuing to explain or pursue?"
      "If we need a pause, what would a mutually respectful plan for reconnecting look like?"))
     ;; Four response
    ("Why are you so emotional?" .
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
         (completing-read-multiple "How I’m processing: "
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
     (format "How I’m processing:\n\n%s\n"
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
           (format "- *%s:* %s"
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
