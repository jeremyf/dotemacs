;;; jf-black-sword-hack --- Random Tables for the Black Sword Hack -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;;; Code:

(defconst jf/gaming/black-sword-hack/table/oracle-question-result
  '((1 . "No and…")
     (2 . "No")
     (3 . "No but…")
     (4 . "Yes but…")
     (5 . "Yes")
     (6 . "Yes and…"))
  "The oracle question's result.

  From page 98 of /The Black Sword Hack: Ultimate Chaos Edition/.")

(defconst jf/gaming/black-sword-hack/table/oracle-unexpected-event
  '((1 . "Very negative")
     (2 . "Negative")
     (3 . "Negative but…")
     (4 . "Positive but…")
     (5 . "Positive")
     (6 . "Very Positive"))
  "The type unexpected event when asking a question of the oracle.

  From page 98 of /The Black Sword Hack: Ultimate Chaos Edition/.")

(defconst jf/gaming/black-sword-hack/table/oracle-question-likelihood
  '(("Don't think so" . (lambda () (cl-sort (list (+ 1 (random 6)) (+ 1 (random 6)) (+ 1 (random 6))) #'<)))
     ("Unlikely" . (lambda () (cl-sort (list (+ 1 (random 6)) (+ 1 (random 6))) #'<)))
     ("Who knows?" . (lambda () (list (+ 1 (random 6)))))
     ("Probably" . (lambda () (cl-sort (list (+ 1 (random 6)) (+ 1 (random 6))) #'>)))
     ("Definitely". (lambda () (cl-sort (list (+ 1 (random 6)) (+ 1 (random 6)) (+ 1 (random 6))) #'>))))
  "The table of options and encoded dice rolls for question likelihoods.

From page 98 of /The Black Sword Hack: Ultimate Chaos Edition/.")

(defconst jf/gaming/black-sword-hack/table/oracle-theme
  '("Death" "Treachery" "Infiltration" "Desperation" "Instability" "Suspicion"
     "Escape" "Fear" "Hunt" "Division" "Falsehood" "Celebration"
     "Conquest" "Friendship" "Love" "Sacrifice" "Decay" "Exile"
     "Revenge" "Greed" "Isolation" "Preservation" "Loss" "Rebirth"
     "Oppression" "Destruction" "Ignorance" "Purification" "Scarcity" "Quest"
     "Stagnation" "Redemption" "Failure" "Help" "Corruption" "Rebellion")
  "See page 98 of /The Black Sword Hack/.")

(defconst jf/gaming/black-sword-hack/table/oracle-subject
  '("Army" "Church" "Ghost" "Nobility" "Otherworldly" "Plague"
     "Omen" "Ally" "Family" "Wizard" "Guild" "Architect"
     "Crusaders" "Vagrant" "Rival" "Artefact" "Messenger" "Inquisitors"
     "Ruins" "Knowledge" "Cave" "Dream" "Hamlet" "Outlaws"
     "Healers" "Cult" "Guardian" "Settlers" "Monument" "Food"
     "Judges" "Storm" "Demon" "Court" "Theatre" "Assassins")
  "See page 98 of /The Black Sword Hack: Ultimate Chaos Edition//.")

(defconst jf/gaming/black-sword-hack/table/travel-theme
  '("Aggression" "Exchange" "Discovery" "Revelation" "Pursuit"
     "Lost" "Isolation" "Death" "Escape" "Change")
  "See page 76 of /The Black Sword Hack: Ultimate Chaos Edition/")


(defconst jf/gaming/black-sword-hack/table/travel-theme
  '("Antagonist" "Animal" "Hermit" "Spirit" "Potentate"
     "Demon" "Explorer" "Merchant" "Caves" "Messenger"
     "Ruins" "Cult" "Community" "Ghost" "Outlaws"
     "Artists" "Soldiers" "Sorcerer" "Vagrant" "Natural disaster")
  "See page 76 of /The Black Sword Hack: Ultimate Chaos Edition/")

(transient-define-suffix jf/gaming/black-sword-hack/table/oracle-response (question likelihood)
  "The Dark God's Oracle answers the QUESTION for the LIKELIHOOD."
  :description "Dark God’s Oracle Answer…"
  (interactive (list
                 (read-string "Yes/No Question: ")
                 (completing-read "Likelihood: " jf/gaming/black-sword-hack/table/oracle-question-likelihood nil t)))
  (let* ((dice (funcall (alist-get likelihood jf/gaming/black-sword-hack/table/oracle-question-likelihood nil nil #'string=)))
         (answer (alist-get (car dice) jf/gaming/black-sword-hack/table/oracle-question-result))
          (unexpected (alist-get (car (list-utils-dupes dice)) jf/gaming/black-sword-hack/table/oracle-unexpected-event))
          (response (concat "- Question :: " question "\n"
                            "- Answer :: "  answer "\n"
                      (when unexpected (concat "- Unexpected Event :: " unexpected "\n"))
                      "- Dice :: " (format "%s" dice))))
    (kill-new response)
    (message response)))

(transient-define-suffix jf/gaming/black-sword-hack/roll/event ()
  "The Dark God's Oracle responds with an event."
  :description "Roll the Dark God's Oracle Event"
  (interactive)
  (let ((event (concat
                 "- Theme :: " (jf/roll-on-table jf/gaming/black-sword-hack/table/oracle-theme) "\n"
                 "- Subject :: " (jf/roll-on-table jf/gaming/black-sword-hack/table/oracle-subject))))
    (kill-new event)
    (message event)))

(provide 'jf-black-sword-hack)
;;; jf-black-sword-hack.el ends here
