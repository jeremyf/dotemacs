;;; jf-the-one-ring --- Tables and Rollers for The One Ring -*- lexical-binding: t -*-

;; Copyright (C) 2022 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;; This package contains tabular data from The One Ring™ tabletop role-playing
;; games.
;;
;; It also provides tools to "roll" on those tables.
;;
;; "Strider", "The One Ring", "Middle-earth", and "The Lord of the Rings" are
;; trademarks or registered trademarks of the Saul Zaentz Company d/b/a
;; Middle-earth Enterprises (SZC).

;; Many tables have 12 entries.  The 0th entry is for the eye rune, 1st through
;; 10th is their respictive numbers, and the 11th is the gandalf rune.  This is
;; a case where the programming convention of arrays/lists indexes start at 0
;; really shines!

;;; Code

(use-package transient :straight t)

;;;; Tables

;;;;; Core Rules

(defconst jf/gaming/the-one-ring/feat-die
  '("⏿"
    1 2 3 4 5 6 7 8 9 10
    "ᚠ")
  "By convention all Feat-die tables treat the initial list item as the \"⏿\" roll.")

(defconst jf/gaming/the-one-ring/feat-die-favourability
  '(("Favoured" . (lambda (table)
		    (nth (max (random (length table))
			      (random (length table)))
			 table)))
    ("Neutral" . (lambda (table)
		   (seq-random-elt table)))
    ("Ill-Favoured" . (lambda (table)
			(nth (min (random (length table))
				  (random (length table)))
			     table))))
  "Favourability options and associated roller.")

(defconst jf/gaming/the-one-ring/table/nameless-things
  '(:name-prefix
    ("The Bane" "The Scourge" "The Horror" "The Terror"
     "The Defiler" "The Devourer" "The Stalker" "The Hunter"
     "The Watcher" "The Crawler" "The Lurker" "The Flame")
    :name-suffix
    ("in the Dark" "of the Abyss" "in the Deep"
     "of the Pit" "of Udûn" "in the Water")
    :named-by
    ("by Men" "by Elves" "by Dwarves"
     "by Orcs" "by the Wise" "in ancient lore")
    :description-prefix
    ("Bat-like" "Spider-like" "Fish-like" "Slug-like"
     "Worm-like" "Centipede-like" "Insect-like" "Crustacean-like"
     "Octopus-like" "Fish-like" "Toad-like" "Troll-like")
    :description-suffix
    ("with remorseless eyes" "with great horns" "with luminous skin"
     "with a huge head" "with a swollen body" "yet greater")
    :before-you-see-it
    ("notice a deadly silence" "hear a sinister hissing"
     "hear a low growl" "see the bones of its victims"
     "feel your skin crawl" "hear a deafening sound or scream"
     "notice its tracks" "hear a terrifying scream"
     "smell a hideous stench" "feel a violent gust of air"
     "hear a piping sound" "feel a terrible cold")
    :what-you-first-see
    ("is a great shadow, in the middle of which is a dark form"
     "are its great claws"
     "are its eyes, glowing in the dark"
     "is that its body is flaccid and translucent, as if composed of gelatinous material"
     "is its gaping mouth, opening and closing as if gasping for air"
     "is a large maw, with fangs eerily similar to human teeth"
     "is that swarms of insects or other vermin are crawling before it"
     "is a long, sinuous tentacle, slithering towards you"
     "are its huge fangs, so big and long that it cannot close its mouth"
     "are its wide, blind eyes"
     "are many twisted horns of stained ivory"
     "is a vision of a beautiful creature, a phantom of the mind")))

;;;;; Strider Mode

(defconst jf/gaming/the-one-ring/strider-mode/event-table
  '(:table (:terrible-misfortune :despair :ill-choices :ill-choices
				:mishap :mishap :mishap :mishap
				:short-cut :short-cut :chance-meeting :joyful-sight)
	  :details
	  ( :terrible-misfortune
	    ( :consequence "If the roll fails, the target is Wounded."
	      :title "Terrible Misfortune"
	      :fatigue 3
	      :events
	      (("Dire confrontation" . "Noteworthy Encounter")
	       ("Rival Predator" . "HUNTING to avoid becoming the hunted")
	       ("Violent weather" . "EXPLORE to find shelter")
	       ("Hidden hazard" . "AWARENESS to avoid stumbling into danger")
	       ("Dangerous terrain" . "EXPLORE to find a safer route")
	       ("Stalking enemy" . "AWARENESS to spot the foul presence")))
	    :despair
	    ( :consequence "If the roll fails, gain 2 Shadow points (Dread)."
	      :title "Despair"
	      :fatigue 2
	      :events
	      (("Servants of the Enemy" . "Noteworthy Encounter")
	       ("Torrential weather" . "EXPLORE to find the least exposed path")
	       ("Nightmarish presence" . "AWARENESS to sense the danger")
	       ("Fading vigour" . "HUNTING to gain sustenance")
	       ("Corrupted site" . "EXPLORE to find your way out")
	       ("Grisly scene or foreboding portent" . "AWARENESS to be forewarned")))
	    :mishap
	    ( :consequence "If the roll fails, add 1 day to the length of the journey, and gain 1 additional Fatigue."
	      :title "Mishap"
	      :fatigue 2
	      :events
	      (("Sparse wildlife" . "HUNTING to forage what you can")
	       ("Lost direction" . "EXPLORE to find your way")
	       ("Obstructed path" . "AWARENESS to spot a way around")
	       ("Elusive quarry" . "HUNTING to track it down")
	       ("Rough terrain" . "EXPLORE to safely traverse")
	       ("Wandering enemies" . "AWARENESS to sense their coming")))
	    :ill-choices
	    ( :consequence "If the roll fails, gain 1 Shadow point (Dread)."
	      :title "Ill Choices"
	      :fatigue 2
	      :events
	      (("Mismanaged provisions" . "HUNTING to replenish stores")
	       ("Wayward path" . "EXPLORE to retrace your steps")
	       ("Overlooked hazard" . "AWARENESS to escape safely")
	       ("Lost quarry" . "HUNTING to follow its tracks")
	       ("Disorienting environs" . "EXPLORE to find your way")
	       ("Haunting visions" . "AWARENESS to over- come darkness")))
	    :short-cut
	    ( :consequence "If the roll succeeds, reduce the length of the journey by 1 day."
	      :title "Short Cut"
	      :fatigue 1
	      :events
	      (("Game trail" . "HUNTING to traverse the path")
	       ("Secluded path" . "EXPLORE to navigate the wilds")
	       ("Helpful tracks" . "AWARENESS to follow the tracks")
	       ("Animal guide" . "HUNTING to follow at a distance")
	       ("Favourable weather" . "EXPLORE to make the most of it")
	       ("Familiar waypoint" . "AWARENESS to recognize the landmark")))
	    :chance-meeting
	    ( :consequence "If the roll succeeds, no Fatigue is gained, and you may envision a favourable encounter."
	      :title "Chance Meeting"
	      :fatigue 1
	      :events
	      (("Lone hunter" . "HUNTING to trade stories")
	       ("Fellow traveller" . "EXPLORE to learn about the path ahead")
	       ("Discreet watcher" . "AWARENESS to spot them")
	       ("Noble beast" . "HUNTING to commune")
	       ("Secluded encampment" . "EXPLORE to find your way off the beaten path")
	       ("Auspicious gathering" . "Noteworthy Encounter")))
	    :joyful-sight
	    ( :consequence "If the roll succeeds, regain 1 Hope."
	      :title "Joyful Sight"
	      :fatigue 0
	      :events
	      (("Majestic creatures" . "HUNTING to observe without startling them")
	       ("Inspiring vista" . "EXPLORE to reach a vantage point")
	       ("Benevolent being" . "AWARENESS to sense their presence")
	       ("Abundant foraging" . "HUNTING to replenish your rations")
	       ("Ancient monument" . "AWARENESS to recognize its significance")
	       ("Peaceful sanctuary" . "Noteworthy Encounter"))))))

(defconst jf/gaming/the-one-ring/strider-mode/fortune-tables
  '(("ᚠ" .
     ("The Eye of the Enemy focuses elsewhere. Decrease Eye Awareness by 1."
      "You may bypass a threat without attracting notice"
      "You gain the attention of a potential ally"
      "An enemy inadvertently reveals their position"
      "You gain favoured ground"
      "Enemies run afoul of danger"
      "You locate or learn of a useful item"
      "Your success instils new hope or renewed resolve"
      "You find a moment of comfort or safety"
      "You learn or realize something which gives helpful insight into your mission"
      "You encounter an opportunity suited to your nature or abilities"
      "An unexpected ally appears or sends aid"))
    ("⏿" .
     ("Your actions catch the Eye of the Enemy. Increase Eye Awareness by 2."
      "You draw unwanted attention"
      "Your actions are observed by someone of ill-intent"
      "Unexpected enemies emerge or are sighted"
      "You are hindered by difficult terrain or an unfa- vourable environment"
      "You find yourself ill-equipped for the circumstances"
      "A favoured weapon or item is lost, broken, or sacrificed"
      "You are plagued by troubling visions or thoughts"
      "An old injury or stress resurfaces"
      "You learn or realize something which adds a new complication to your mission"
      "You face a test which is contrary to your nature or abilities"
      "An ally becomes a hindrance or liability"))))

(defconst jf/gaming/the-one-ring/strider-mode/lore-table
  '(
    :action ("⏿ Abandon" "⏿ Attack" "⏿ Betray" "⏿ Corrupt" "⏿ Defeat" "⏿ Weaken" ;; Eye of Sauron
	     "Aid" "Arrive" "Await" "Breach" "Break" "Capture" ;; 1
	     "Change" "Chase" "Command" "Control" "Create" "Defy" ;; 2
	     "Demand" "Discover" "Disguise" "Endure" "Escape" "Evade" ;; 3
	     "Explore" "Find" "Focus" "Gather" "Guard" "Guide" ;; 4
	     "Hide" "Hinder" "Hoard" "Hold" "Hunt" "Journey" ;; 5
	     "Lead" "Learn" "Leave" "Lose" "Mourn" "Move" ;; 6
	     "Persist" "Preserve" "Prevent" "Refuse" "Reject" "Remove" ;; 7
	     "Replenish" "Restore" "Scheme" "Search" "Seize" "Share" ;; 8
	     "Slay" "Steal" "Summon" "Surrender" "Surround" "Threaten" ;; 9
	     "Transform" "Trap" "Trick" "Uncover" "Uphold" "Withstand" ;; 10
	     "ᚠ Believe" "ᚠ Bolster" "ᚠ Defend" "ᚠ Forgive" "ᚠ Resist" "ᚠ Strengthen") ;; Rune of Gandalf
    :aspect ("⏿ Corrupted" "⏿ Cruel" "⏿ Deceptive" "⏿ Fell" "⏿ Ruined" "⏿ Treacherous" ;; Eye of Sauron
	     "Active" "Ancient" "Bold" "Bright" "Broken" "Cheerless" ;; 1
	     "Cold" "Concealed" "Dangerous" "Dark" "Dead" "Defended" ;; 2
	     "Desolate" "Destroyed" "Dreadful" "Empty" "Evil" "Faded" ;; 3
	     "Far-reaching" "Fierce" "Foreboding" "Forgotten" "Fragile" "Ghastly" ;; 4
	     "Gloomy" "Growing" "Hidden" "Ill-fated" "Impenetrable" "Inspiring" ;; 5
	     "Isolated" "Lofty" "Lost" "Menacing" "Mighty" "Mysterious" ;; 6
	     "Noble" "Obstructed" "Old" "Ominous" "Open" "Peaceful" ;; 7
	     "Restored" "Sheltered" "Silent" "Simple" "Small" "Sombre" ;; 8
	     "Stony" "Stout" "Stricken" "Stubborn" "Twisted" "Unnatural" ;; 9
	     "Veiled" "Vigorous" "Weary" "Wild" "Wretched" "Young" ;; 10
	     "ᚠ Flourishing" "ᚠ Beautiful" "ᚠ Good" "ᚠ Kind" "ᚠ Gentle" "ᚠ Wondrous") ;; Rune of Gandalf
    :focus ("⏿ Curse" "⏿ Despair" "⏿ Enemy" "⏿ Fear" "⏿ Shadow" "⏿ War" ;; Eye of Sauron
	    "Battle" "Border" "Burden" "Council" "Court" "Creature" ;; 1
	    "Darkness" "Death" "Defence" "Depths" "Doubt" "Dreams" ;; 2
	    "Fate" "Fire" "Folk" "Followers" "Greed" "Haven" ;; 3
	    "History" "Honour" "Journey" "Kindred" "Knowledge" "Land" ;; 4
	    "Leader" "Legend" "Life" "Light" "Luck" "Memory" ;; 5
	    "Message" "Might" "Nature" "Pain" "Path" "Patron" ;; 6
	    "Peril" "Plan" "Power" "Prophecy" "Quest" "Refuge" ;; 7
	    "Riddle" "Ruins" "Rumour" "Secret" "Skill" "Song" ;; 8
	    "Story" "Strength" "Time" "Tool" "Treasure" "Trust" ;; 9
	    "Truth" "Vengeance" "Wealth" "Weapon" "Wilds" "Wish" ;; 10
	    "ᚠ Courage" "ᚠ Duty" "ᚠ Fellowship" "ᚠ Hope" "ᚠ Love" "ᚠ Peace") ;; Rune of Gandalf
    )
  "From Strider Mode p11-12")

(defconst jf/gaming/the-one-ring/strider-mode/revelation-episode-table
  '("Internal strife or an external threat puts your Safe Haven in peril"
    "Unexpected danger arises on the path ahead, forcing you to seek a new route"
    "Nature is corrupted and turns against you"
    "Spies of the Enemy carry word of your mission"
    "Enemy minions launch an ambush or lay a trap"
    "Enemy minions pick up your trail"
    "An important location is overtaken by an enemy"
    "An item you carry holds a curse, or is hunted by an enemy"
    "You are tempted by something greatly desired, to the detriment of your mission"
    "Malicious lies cause others to mistrust or fear you"
    "Conflict brews between allies"
    "An important ally is put in danger")
  "Roll a 1d12; by convention the 0th is Sauron.")

(defconst jf/gaming/the-one-ring/strider-mode/telling-table
  ;; List contains 12 elements; 0th index is "Sauron", then 1st through 10th are
  ;; 1 through 10 and 11th is "Gandalf".
  '(("Certain" . ("⏿ No with an extreme result or twist"
		  "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes"
		  "ᚠ Yes with an extreme result or twist"))
    ("Likely" . ("⏿ No with an extreme result or twist"
		 "No" "No" "No" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes"
		 "ᚠ Yes with an extreme result or twist"))
    ("Middling" . ("⏿ No with an extreme result or twist"
		   "No" "No" "No" "No" "No" "Yes" "Yes" "Yes" "Yes" "Yes"
		   "ᚠ Yes with an extreme result or twist"))
    ("Doubtful" . ("⏿ No with an extreme result or twist"
		   "No" "No" "No" "No" "No" "No" "No" "Yes" "Yes" "Yes"
		   "ᚠ Yes with an extreme result or twist"))
    ("Unthinkable" . ("⏿ No with an extreme result or twist"
		      "No" "No" "No" "No" "No" "No" "No" "No" "No" "Yes"
		      "ᚠ Yes with an extreme result or twist")))
  "An translation of the Telling Table from Strider Mode.")


;;;;; Help

(jf/transient-quick-help jf/gaming/the-one-ring/strider-mode/experience-milestones
  :label "Strider: Experience Milestones"
  :header "Strider: Experience Milestones"
  :body
  (s-join
   "\n"
   '("| Milestone                                    | Adventure Point | Skill Point |"
     "|----------------------------------------------+-----------------+-------------|"
     "| Accept a mission from a patron               |               1 | -           |"
     "| Achieve a notable personal goal              |               1 | 1           |"
     "| Complete a patron’s mission                  |               1 | 1           |"
     "| Complete a meaningful journey                |               2 | -           |"
     "| Face a Noteworthy Encounter during a journey |               1 | -           |"
     "| Reveal a significant location or discovery   |               - | 1           |"
     "| Overcome a tricky obstacle                   |               1 | -           |"
     "| Participate in a Council                     |               1 | -           |"
     "| Survive a dangerous combat                   |               - | 1           |"
     "| Face a Revelation Episode                    |               - | 1           |")))

;;;; Rollers

(cl-defun jf/gaming/the-one-ring/roll/favorability-with-table (&key favorability table)
  "Roll on the TABLE using the FAVORABILITY."
  (funcall (alist-get favorability
		      jf/gaming/the-one-ring/feat-die-favourability
		      nil
		      nil
		      #'string=)
	   table))

(cl-defun jf/gaming/the-one-ring/roll/feat-die (favorability)
  "Prompt for the FAVORABILITY and roll the feat die."
  (interactive (list (completing-read "Favourability: "
				      jf/gaming/the-one-ring/feat-die-favourability)))
  (jf/gaming/the-one-ring/roll/favorability-with-table :favorability favorability
						       :table jf/gaming/the-one-ring/feat-die))

(cl-defun jf/gaming/the-one-ring/roll/lore-table
    (question
     &key
     (lore-table jf/gaming/the-one-ring/strider-mode/lore-table))
  "Prompt for a QUESTION and roll on the LORE-TABLE."
  (interactive (list
		(read-string "Open-ended Question: ")))
  (concat "{{{i(Lore Table)}}}:\n"
	  "\n"
	  "- Question :: “" question "”\n"
	  "- Action :: " (seq-random-elt (plist-get lore-table :action)) "\n"
	  "- Aspect :: " (seq-random-elt (plist-get lore-table :aspect)) "\n"
	  "- Focus :: " (seq-random-elt (plist-get lore-table :focus)) "\n"))

(cl-defun jf/gaming/the-one-ring/roll/skill-check (dice
						   favorability
						   &key
						   (is_weary
						    jf/gaming/the-one-ring/character-is-weary))
  "Roll the dice of DICE with the given FAVORABILITY for the feat die."
  (interactive (list
		(read-number "Number of D6s: ")
		(completing-read "Favourability: "
				 jf/gaming/the-one-ring/feat-die-favourability)))
  (let* ((feat-die (jf/gaming/the-one-ring/roll/feat-die favorability))
	 (success-dice (jf/gaming/the-one-ring/roll/success-dice :dice dice :is_weary is_weary))
	 (weary_message (if is_weary " with Weary condition" ""))
	 (prefix (format "%s %sd6%s [Feat: %s  Success: %s]"
			 favorability
			 dice
			 weary_message
			 feat-die
			 (plist-get success-dice :rolls))))
    (cond
     ((numberp feat-die)
      (format "%s: %s %sՇ "
	      prefix
	      (+ feat-die (plist-get success-dice :total))
	      (plist-get success-dice :sixes)))
     ((string= "⏿" feat-die)
      (format "%s: %s %s %sՇ"
	      prefix
	      feat-die
	      (plist-get success-dice :total)
	      (plist-get success-dice :sixes)))
     ((string= "ᚠ" feat-die)
      (format "%s: %s %sՇ"
	      prefix
	      feat-die
	      (plist-get success-dice :sixes))))))

(defun jf/gaming/the-one-ring/roll/solo-event-table (favorability)
  "Prompt for the FAVORABILITY and roll on the solo event table."
  (interactive (list (completing-read "Favourability: "
				      jf/gaming/the-one-ring/feat-die-favourability)))
  (let* ((subtable-name (jf/gaming/the-one-ring/roll/favorability-with-table
			 :favorability favorability
			 :table (plist-get jf/gaming/the-one-ring/strider-mode/event-table :table)))
	 (details (plist-get jf/gaming/the-one-ring/strider-mode/event-table :details))
	 (subtable (plist-get details subtable-name))
	 (subtable-events (seq-random-elt (plist-get subtable :events))))
    (format "%s: %s\n\n- Fatigue :: %s\n- Consequence :: %s\n- Task :: %s\n"
	    (plist-get subtable :title)
	    (car subtable-events)
	    (plist-get subtable :fatigue)
	    (plist-get subtable :consequence)
	    (cdr subtable-events))))

(cl-defun jf/gaming/the-one-ring/roll/success-dice (&key dice (is_weary nil))
  "Roll a number of \"The One Ring\" success DICE.  And reject some results when character IS_WEARY."
  (let ((total 0)
	(sixes 0)
	(rolls (list))
	(roll 0))
    (while (> dice 0)
      (setq roll (1+ (random 6)))
      (push roll rolls)
      (when (or (not is_weary) (> roll 3)) (setq total (+ total roll)))
      (when (= 6 roll) (setq sixes (+ 1 sixes)))
      (setq dice (1- dice)))
    (list :total total :sixes sixes :rolls rolls)))

(defun jf/gaming/the-one-ring/roll/telling-table (question likelihood)
  "Ask the QUESTION with the given LIKELIHOOD on the `jf/gaming/the-one-ring/strider-mode/telling-table'."
  (interactive (list
		(read-string "Yes/No Question: ")
		(completing-read "Likelihood of yes: " jf/gaming/the-one-ring/strider-mode/telling-table)))
  (concat "{{{i(Telling Table)}}}:\n"
	  "\n"
	  "- Question :: “" question "”\n"
	  "- Likelihood :: " likelihood "\n"
	  "- Answer :: “" (seq-random-elt (alist-get likelihood jf/gaming/the-one-ring/strider-mode/telling-table nil nil #'string=)) "”"
	  "\n"))

;;;; Session Tracking Functions
(cl-defmacro jf/gaming/the-one-ring/register-condition (&key condition)
  "Generate the `transient' suffix and variable for the given CONDITION."
  (let* ((var-sym (intern (concat "jf/gaming/the-one-ring/character-is-" condition)))
	 (var-docstring (concat "Is the current character is " condition "?"))
         (fun-sym (intern (concat "jf/gaming/the-one-ring/character-is-" condition "/set")))
	 (fun-docstring (concat "Toggle current character's \"" condition "\" condition status.")))
    `(progn
       (defvar ,var-sym
	 nil
	 ,var-docstring)
       (transient-define-suffix ,fun-sym ()
	 ,fun-docstring
	 :description '(lambda ()
			 (concat
			  (s-titleize ,condition) ": "
			  (propertize
			   (format "%s" (if ,var-sym "Yes" "No"))
			   'face 'transient-argument)))
	 (interactive)
	 (setq ,var-sym (not ,var-sym))))))

(jf/gaming/the-one-ring/register-condition :condition "miserable")
(jf/gaming/the-one-ring/register-condition :condition "weary")
(jf/gaming/the-one-ring/register-condition :condition "wounded")

;; TODO: Register: Travel Fatigue, Exhaustion, Hope, Shadow Points.
;;       And function to "write" that to character sheet.

;;;; Menu
(global-set-key (kbd "H-1") 'jf/gaming/the-one-ring/menu)
(transient-define-prefix jf/gaming/the-one-ring/menu ()
  ["The One Ring\n"
   ["Rolls"
    ("f" "Feat die…"
     (lambda ()
       (interactive)
       (insert
	(format "Feat Die: %s"
		(call-interactively 'jf/gaming/the-one-ring/roll/feat-die)))))
    ("j" "Journey event (Solo)"
     (lambda ()
       (interactive)
       (insert (call-interactively 'jf/gaming/the-one-ring/roll/solo-event-table))))
    ("l" "Lore table…"
     (lambda ()
       (interactive)
       (insert (call-interactively 'jf/gaming/the-one-ring/roll/lore-table))))
    ("r" "Revelation Episode…"
     (lambda ()
       (interactive)
       (insert (concat "Revelation Episode: "
		       (seq-random-elt jf/gaming/the-one-ring/revelation-episode-table)))))
    ("s" "Skill check…"
     (lambda ()
       (interactive)
       (insert (call-interactively 'jf/gaming/the-one-ring/roll/skill-check))))
    ("t" "Telling table…"
     (lambda ()
       (interactive)
       (insert (call-interactively 'jf/gaming/the-one-ring/roll/telling-table))))
    ]
   ["Conditions"
    ("-m" jf/gaming/the-one-ring/character-is-miserable/set :transient t)
    ("-w" jf/gaming/the-one-ring/character-is-weary/set :transient t)
    ("-W"  jf/gaming/the-one-ring/character-is-wounded/set :transient t)
    ]
   ["Help"
    ("h m" jf/gaming/the-one-ring/strider-mode/experience-milestones)]])

(provide 'jf-the-one-ring)
;;; jf-the-one-ring.el ends here
