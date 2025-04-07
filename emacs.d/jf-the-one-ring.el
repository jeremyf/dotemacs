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

(require 'random-table)
(require 'transient)
(require 'jf-quick-help)
(require 's)

;;;; Tables
(random-table/register :name "The One Ring > Lore"
  :data '("\n\t- Action :: {The One Ring > Lore > Action}\n\t- Aspect :: {The One Ring > Lore > Aspect}\n\t- Focus :: {The One Ring > Lore > Focus}"))

(random-table/register :name "The One Ring > Lore > Action"
  :private t
  :data '("⏿ Abandon" "⏿ Attack" "⏿ Betray" "⏿ Corrupt" "⏿ Defeat" "⏿ Weaken"
           "Aid" "Arrive" "Await" "Breach" "Break" "Capture"
           "Change" "Chase" "Command" "Control" "Create" "Defy"
           "Demand" "Discover" "Disguise" "Endure" "Escape" "Evade"
           "Explore" "Find" "Focus" "Gather" "Guard" "Guide"
           "Hide" "Hinder" "Hoard" "Hold" "Hunt" "Journey"
           "Lead" "Learn" "Leave" "Lose" "Mourn" "Move"
           "Persist" "Preserve" "Prevent" "Refuse" "Reject" "Remove"
           "Replenish" "Restore" "Scheme" "Search" "Seize" "Share"
           "Slay" "Steal" "Summon" "Surrender" "Surround" "Threaten"
           "Transform" "Trap" "Trick" "Uncover" "Uphold" "Withstand"
           "ᚠ Believe" "ᚠ Bolster" "ᚠ Defend" "ᚠ Forgive" "ᚠ Resist" "ᚠ Strengthen"))

(random-table/register :name "The One Ring > Lore > Aspect"
  :private t
  :data '("⏿ Corrupted" "⏿ Cruel" "⏿ Deceptive" "⏿ Fell" "⏿ Ruined" "⏿ Treacherous"
           "Active" "Ancient" "Bold" "Bright" "Broken" "Cheerless"
           "Cold" "Concealed" "Dangerous" "Dark" "Dead" "Defended"
           "Desolate" "Destroyed" "Dreadful" "Empty" "Evil" "Faded"
           "Far-reaching" "Fierce" "Foreboding" "Forgotten" "Fragile" "Ghastly"
           "Gloomy" "Growing" "Hidden" "Ill-fated" "Impenetrable" "Inspiring"
           "Isolated" "Lofty" "Lost" "Menacing" "Mighty" "Mysterious"
           "Noble" "Obstructed" "Old" "Ominous" "Open" "Peaceful"
           "Restored" "Sheltered" "Silent" "Simple" "Small" "Sombre"
           "Stony" "Stout" "Stricken" "Stubborn" "Twisted" "Unnatural"
           "Veiled" "Vigorous" "Weary" "Wild" "Wretched" "Young"
           "ᚠ Flourishing" "ᚠ Beautiful" "ᚠ Good" "ᚠ Kind" "ᚠ Gentle" "ᚠ Wondrous"))

(random-table/register :name "The One Ring > Lore > Focus"
  :private t
  :data '("⏿ Curse" "⏿ Despair" "⏿ Enemy" "⏿ Fear" "⏿ Shadow" "⏿ War"
           "Battle" "Border" "Burden" "Council" "Court" "Creature"
           "Darkness" "Death" "Defence" "Depths" "Doubt" "Dreams"
           "Fate" "Fire" "Folk" "Followers" "Greed" "Haven"
           "History" "Honour" "Journey" "Kindred" "Knowledge" "Land"
           "Leader" "Legend" "Life" "Light" "Luck" "Memory"
           "Message" "Might" "Nature" "Pain" "Path" "Patron"
           "Peril" "Plan" "Power" "Prophecy" "Quest" "Refuge"
           "Riddle" "Ruins" "Rumour" "Secret" "Skill" "Song"
           "Story" "Strength" "Time" "Tool" "Treasure" "Trust"
           "Truth" "Vengeance" "Wealth" "Weapon" "Wilds" "Wish"
           "ᚠ Courage" "ᚠ Duty" "ᚠ Fellowship" "ᚠ Hope" "ᚠ Love" "ᚠ Peace"))

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

(random-table/register :name "The One Ring > Distinctive Feature"
  :data
  '(((1 . 12) . "Synonym of a PC's distinctive feature")
     ((13 . 24) . "Antonym of a PC's distinctive feature")
     (25 . "Bold") (26 . "Cunning") (27 . "Eager") (28 . "Fair") (29 . "Fair-spoken") (30 . "Faithful")
     (31 . "Arrogant") (32 . "Brutal") (33 . "Cowardly") (34 . "Cruel") (35 . "Deceitful") (36 . "Fearful")
     (37 . "Fierce") (38 . "Generous") (39 . "Honourable") (40 . "Inquisitive") (41 . "Keen-eyed") (42 . "Lordly")
     (43 . "Forgetful") (44 . "Grasping") (45 . "Guilt-ridden") (46 . "Haughty") (47 . "Idle") (48 . "Mistrustful")
     (49 . "Merry") (50 . "Patient") (51 . "Proud") (52 . "Rustic") (53 . "Secretive") (54 . "Stern")
     (55 . "Murderous") (56 . "Overconfident") (57 . "Resentful") (58 . "Scheming") (59 . "Scornful") (60 . "Spiteful")
     (61 . "Subtle") (62 . "Swift") (63 . "Tall") (64 . "True-hearted") (65 . "Wary") (66 . "Wilful")
     (67 . "Thieving") (68 . "Traitorous") (69 . "Troubled") (70 . "Tyrannical") (71 . "Uncaring") (72 . "Wavering")))

(defun jf/gaming/the-one-ring/feat-die-with-favourability (_ &optional given-table)
  (let* ((favourabilities
           '(("Favoured" . (lambda (table)
                             (nth (max (random (length table))
                                    (random (length table)))
                               table)))
              ("Neutral" . (lambda (table)
                             (seq-random-elt table)))
              ("Ill-Favoured" . (lambda (table)
                                  (nth (min (random (length table))
                                         (random (length table)))
                                    table))))))
    (funcall
      (alist-get (completing-read "Favourability: "
                   favourabilities nil t)
        favourabilities nil nil #'string=)
      (or given-table jf/gaming/the-one-ring/feat-die))))


(random-table/register :name "The One Ring > Journey Event"
  :roller #'jf/gaming/the-one-ring/feat-die-with-favourability
  :data
  '(("⏿" . "\n\t- Terrible Misfortune :: if the roll fails, the target is Wounded.\n\t- Fatigue :: 3\n\t- Event :: {The One Ring > Journey Event > Terrible Misfortune}")
     (1 . "\n\t- Despair :: if the roll fails, gain 2 Shadow points (Dread).\n\t- Fatigue :: 2\n\t- Event :: {The One Ring > Journey Event > Despair}")
     ((2 . 3) . "\n\t- Ill-Choices :: if the roll fails, gain 1 Shadow point (Dread).\n\t- Fatigue :: 2\n\t- Event :: {The One Ring > Journey Event > Ill-Choices}")
     ((4 . 7) . "\n\t- Mishap :: if the roll fails, add 1 day to the length of the journey.\n\t- Fatigue :: 2\n\t- Event :: {The One Ring > Journey Event > Mishap}")
     ((8 . 9) . "\n\t- Short-cut :: if the roll succeeds, reduce the length of the journey by 1 day.\n\t- Fatigue :: 1\n\t- Event :: {The One Ring > Journey Event > Short-Cut}")
     (10 . "\n\t- Chance Meeting :: If the roll succeeds, no Fatigue is gained, and you may envision a favourable encounter.\n\t- Fatigue :: 1\n\t- Event :: {The One Ring > Journey Event > Chance Meeting}")
     ("ᚠ" . "\n\t- ᚠ Joyful Sight :: If the roll succeeds, regain 1 Hope.\n\t- Fatigue :: 0\n\t- Event :: {The One Ring > Journey Event > Joyful Sight}")))

(random-table/register :name "The One Ring > Journey Event > Terrible Misfortune"
  :private t
  :data
  '("Dire confrontation (Noteworthy Encounter)"
     "Rival Predator (HUNTING to avoid becoming the hunted)"
     "Violent weather (EXPLORE to find shelter)"
     "Hidden hazard (AWARENESS to avoid stumbling into danger)"
     "Dangerous terrain (EXPLORE to find a safer route)"
     "Stalking enemy(AWARENESS to spot the foul presence)"))
(random-table/register :name "The One Ring > Journey Event > Terrible Misfortune"
  :private t
  :data
  '("Servants of the Enemy (Noteworthy Encounter)"
     "Torrential weather (EXPLORE to find the least exposed path)"
     "Nightmarish presence (AWARENESS to sense the danger)"
     "Fading vigour (HUNTING to gain sustenance)"
     "Corrupted site (EXPLORE to find your way out)"
     "Grisly scene or foreboding portent (AWARENESS to be forewarned)"))
(random-table/register :name "The One Ring > Journey Event > Mishap"
  :private t
  :data
  '("Sparse wildlife (HUNTING to forage what you can)"
     "Lost direction (EXPLORE to find your way)"
     "Obstructed path (AWARENESS to spot a way around)"
     "Elusive quarry (HUNTING to track it down)"
     "Rough terrain (EXPLORE to safely traverse)"
     "Wandering enemies (AWARENESS to sense their coming)"))
(random-table/register :name "The One Ring > Journey Event > Ill-Choices"
  :private t
  :data
  '("Mismanaged provisions (HUNTING to replenish stores)"
     "Wayward path (EXPLORE to retrace your steps)"
     "Overlooked hazard (AWARENESS to escape safely)"
     "Lost quarry (HUNTING to follow its tracks)"
     "Disorienting environs (EXPLORE to find your way)"
     "Haunting visions (AWARENESS to overcome darkness)"))
(random-table/register :name "The One Ring > Journey Event > Short-Cut"
  :private t
  :data
  '("Game trail (HUNTING to traverse the path)"
     "Secluded path (EXPLORE to navigate the wilds)"
     "Helpful tracks (AWARENESS to follow the tracks)"
     "Animal guide (HUNTING to follow at a distance)"
     "Favourable weather (EXPLORE to make the most of it)"
     "Familiar waypoint (AWARENESS to recognize the landmark)"))
(random-table/register :name "The One Ring > Journey Event > Chance Meeting"
  :private t
  :data
  '("Lone hunter (HUNTING to trade stories)"
     "Fellow traveller (EXPLORE to learn about the path ahead)"
     "Discreet watcher (AWARENESS to spot them)"
     "Noble beast (HUNTING to commune)"
     "Secluded encampment (EXPLORE to find your way off the beaten path)"
     "Auspicious gathering (Noteworthy Encounter)"))
(random-table/register :name "The One Ring > Journey Event > Joyful Sight"
  :private t
  :data
  '("Majestic creatures (HUNTING to observe without startling them)"
     "Inspiring vista (EXPLORE to reach a vantage point)"
     "Benevolent being (AWARENESS to sense their presence)"
     "Abundant foraging (HUNTING to replenish your rations)"
     "Ancient monument (AWARENESS to recognize its significance)"
     "Peaceful sanctuary (Noteworthy Encounter)"))

(defconst jf/gaming/the-one-ring/strider-mode/fortune-tables
  '(("ᚠ" .
     ("⏿ The Eye of the Enemy focuses elsewhere. Decrease Eye Awareness by 1."
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
      "ᚠ An unexpected ally appears or sends aid"))
    ("⏿" .
     ("⏿ Your actions catch the Eye of the Enemy. Increase Eye Awareness by 2."
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
      "ᚠ An ally becomes a hindrance or liability"))))

(random-table/register :name "The One Ring > Revelation Episode"
  :data
  '("⏿ Internal strife or an external threat puts your Safe Haven in peril"
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
    "ᚠ An important ally is put in danger"))

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
  :label "Strider: XP Milestones"
  :header "Strider: Experience Milestones"
  :body
  (s-join
   "\n"
   '("| Milestone                                    | Adventure Pt | Skill Pt |"
     "|----------------------------------------------+--------------+----------|"
     "| Accept a mission from a patron               |            1 | -        |"
     "| Achieve a notable personal goal              |            1 | 1        |"
     "| Complete a patron’s mission                  |            1 | 1        |"
     "| Complete a meaningful journey                |            - | 2        |"
     "| Face a Noteworthy Encounter during a journey |            - | 1        |"
     "| Reveal a significant location or discovery   |            1 | -        |"
     "| Overcome a tricky obstacle                   |            - | 1        |"
     "| Participate in a Council                     |            - | 1        |"
     "| Survive a dangerous combat                   |            1 | -        |"
     "| Face a Revelation Episode                    |            1 | -        |")))

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
  "Return a random result of the feat die based on the given FAVORABILITY."
  (interactive (list (completing-read "Favourability: "
                                      jf/gaming/the-one-ring/feat-die-favourability)))
  (jf/gaming/the-one-ring/roll/favorability-with-table :favorability favorability
                                                       :table jf/gaming/the-one-ring/feat-die))

(defun jf/gaming/the-one-ring/roll/fortune-table (fortune_type)
  "Return a random fortune based given FORTUNE_TYPE."
  (interactive (list
                (completing-read "Fortune Type: "
                                 jf/gaming/the-one-ring/strider-mode/fortune-tables)))
  (format "Fortune %s: “%s”"
          fortune_type
          (jf/roll-on-table (alist-get fortune_type
                             jf/gaming/the-one-ring/strider-mode/fortune-tables
                             nil
                             nil
                             #'string=))))

(defvar jf/gaming/the-one-ring/skills
  '("Athletics" "Awareness" "Awe" "Axe"
     "Battle" "Bow" "Courtesy" "Craft"
     "Enhearten" "Explore" "Healing" "Hunting"
     "Insight" "Lore" "Persuade" "Riddle"
     "Scan" "Song" "Spear" "Stealth"
     "Sword" "Travel")
  "The skills of “The One Ring”")

(cl-defun jf/gaming/the-one-ring/roll/skill-check(skill
                                                   tn
                                                   favourability
                                                   dice
                                                   &key
                                                   (is_weary
                                                     jf/gaming/the-one-ring/character-is-weary)
                                                   (is_miserable
                                                     jf/gaming/the-one-ring/character-is-miserable))
  "Roll DICE for SKILL check against TN with feat die FAVOURABILITY.

Examples:

Success (Skill: Battle; TN: 12; Roll: 13; Favourability: Neutral; Feat Dice: 8; Success Dice: 2d6 {2 3})

Success 2Շ (Skill: Battle; TN: 12; Roll: 20; Favourability: Neutral; Feat Dice: 8; Success Dice: 2d6 {6 6})

Failure (Skill: Battle; TN: 12; Roll: 11 Favourability: Neutral; Feat Dice: 6; Success Dice: 2d6 {2 3})

Failure ⏿ (Skill: Battle; TN: 12; Roll: 5 Favourability: Neutral; Feat Dice: ⏿; Success Dice: 2d6 {2 3})

Success ᚠ  (Skill: Battle; TN: 12; Roll: ᚠ Favourability: Neutral; Feat Dice: ᚠ; Success Dice: 2d6 {2 3})

Success ᚠ (Skill: Battle; TN: 12; Roll: ᚠ Favourability: Neutral; Feat Dice: ᚠ; Weary: true; Success Dice: 2d6 {2 3})"
  (interactive (list
                 (completing-read "Skill: "
                   jf/gaming/the-one-ring/skills
                   nil
                   t)
                 (read-number "Target Number (TN): ")
                 (completing-read "Favourability: "
                   jf/gaming/the-one-ring/feat-die-favourability)
                 (read-number "Number of D6s: ")))
  (let* ((feat-die (jf/gaming/the-one-ring/roll/feat-die favourability))
          (success-dice (jf/gaming/the-one-ring/roll/success-dice :dice dice :is_weary is_weary))
          (tengwars (plist-get success-dice :sixes))
          (tengwar_msg (when (> tengwars 0) (format " %sՇ" tengwars)))
          (result_and_roll
            (cond
              ((numberp feat-die)
                (let ((roll (+ feat-die (plist-get success-dice :total))))
                  (cons
                    (if (>=  roll tn)
                      (concat "Success" tengwar_msg)
                      "Failure")
                    roll)))
              ((string= "⏿" feat-die)
                (if is_miserable
                  (cons "Failure ⏿" "⏿")
                  (let ((roll (plist-get success-dice :total)))
                    (cons
                      (if (>= roll tn)
                        (concat "Success ⏿" tengwar_msg)
                        "Failure ⏿")
                      roll))))
              ((string= "ᚠ" feat-die)
                (cons
                  (concat "Success ᚠ" tengwar_msg)
                  "ᚠ")))))
    (format "%s {Skill: %s, TN: %s, Roll: %s, Favourability: %s, Feat Dice: %s, Success Dice: %sd6 (%s)%s%s}"
      (car result_and_roll)
      skill
      tn
      (cdr result_and_roll)
      favourability
      feat-die dice
      (s-join ", " (mapcar (lambda (roll) (format "%s" roll)) (plist-get success-dice :rolls)))
      (if is_weary " Weary: true;" "")
      (if is_miserable " Miserable: true;" ""))))


(defun jf/gaming/the-one-ring/roll/solo-event-table (favourability)
  "Return the results of rolling on the solo event table with the given FAVORABILITY."
  (interactive (list (completing-read "Favourability: "
                                      jf/gaming/the-one-ring/feat-die-favourability)))
  (let* ((subtable-name (jf/gaming/the-one-ring/roll/favorability-with-table
                         :favorability favourability
                         :table (plist-get jf/gaming/the-one-ring/strider-mode/event-table :table)))
         (details (plist-get jf/gaming/the-one-ring/strider-mode/event-table :details))
         (subtable (plist-get details subtable-name))
         (subtable-events (jf/roll-on-table (plist-get subtable :events))))
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
  "Return the response from asking the telling table a yes/no QUESTION with a given LIKELIHOOD."
  (interactive (list
                (read-string "Yes/No Question: ")
                (completing-read "Likelihood of yes: " jf/gaming/the-one-ring/strider-mode/telling-table)))
  (concat "{{{i(Telling Table)}}}:\n"
          "\n"
          "- Question :: “" question "”\n"
          "- Likelihood :: " likelihood "\n"
          "- Answer :: “" (jf/roll-on-table (alist-get likelihood jf/gaming/the-one-ring/strider-mode/telling-table nil nil #'string=)) "”"
          "\n"))

;;;; Session Tracking
(defvar jf/gaming/the-one-ring/strider-mode/character-sheet-filename
  (file-truename "~/git/org/denote/melange/20221128T203953==the=travels=of=duinhir=tailwind--duinhir-tailwind__TheOneRing_rpgs.org"))

(defvar jf/gaming/the-one-ring/strider-mode/campaign-index-filename
  (file-truename "~/git/org/denote/indices/20221129T091857==the=travels=of=duinhir=tailwind--the-travels-of-duinhir-tailwind__projects_rpgs.org"))

(cl-defun jf/sidebar--build (&key
                             buffer-name
                             (body nil)
                             (read-only nil)
                             (position nil)
                             (mode nil))
  "Build the sidebar from the given buffer attributes.

Find or create the BUFFER_NAME with the given BODY and move to
the given POSITION and toggle on the MODE.  Then set the buffer
to READ_ONLY."
  (interactive)
  ;; If the buffer doesn't exist, create it and configure accordingly
  (unless (get-buffer buffer-name)
    (progn
      (get-buffer-create buffer-name)
      (with-current-buffer buffer-name
        (insert (or body (buffer-string)))
        (goto-char (or position (point-min)))
        (not-modified)
        (if mode (funcall mode) (special-mode))
        (when read-only (read-only-mode)))))
  (with-current-buffer buffer-name
    (local-set-key (kbd "s-w") 'kill-buffer-and-window)
    (let ((display-buffer-mark-dedicated t))
      (pop-to-buffer buffer-name '((display-buffer-in-side-window)
                                   (side . right)
                                   (window-width 72)
                                   (window-parameters
                                    (tab-line-format . none)
                                    (mode-line-format . none)
                                    (no-delete-other-windows . t)))))
    (message "s-q - Remove Window")
    (require 'pulsar)
    (pulsar-pulse-line)))

(cl-defun jf/gaming/the-one-ring/strider-mode/pop-open-filename
    (&key (filename jf/gaming/the-one-ring/strider-mode/character-sheet-filename))
  "Pop open for viewing the given FILENAME."
  (with-current-buffer (find-file-noselect filename)
    (jf/sidebar--build
     :buffer-name filename
     :read-only t
     :body (buffer-string)
     :mode 'org-mode)))

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
(keymap-global-set "H-1" 'jf/gaming/the-one-ring/menu)
(transient-define-prefix jf/gaming/the-one-ring/menu ()
  ["The One Ring\n"
   ["Rolls"
    ("r d" "Distinctive features…"
     (lambda ()
       (interactive)
       (let ((random-table/reporter
               '(random-table/reporter/as-child-window kill-new message))
              (random-table/report-formatter/function
               #'random-table/report-formatter/only-roll))
       (random-table/roll "Distinctive Features: ”{The One Ring > Distinctive Feature}” ”{The One Ring > Distinctive Feature}”"))))
    ("r f" "Feat die…"
     (lambda ()
       (interactive)
       // TODO: Get this working with random-table.
       (insert
        (format "Feat Die: %s"
                (call-interactively 'jf/gaming/the-one-ring/roll/feat-die)))))
    ("r F" "Fortune table"
     (lambda ()
       (interactive)
       // TODO: Get this working with random-table.
       (insert (call-interactively 'jf/gaming/the-one-ring/roll/fortune-table))))
     ("r j" "Journey event (Solo)"
       (lambda ()
       (interactive)
       (let ((random-table/reporter
               '(random-table/reporter/as-child-window kill-new message))
              (random-table/report-formatter/function
                #'random-table/report-formatter/only-roll))
         (random-table/roll "Journey Event: {The One Ring > Journey Event}"))))
     ("r l" "Lore table…"
       (lambda ()
         (interactive)
         (let ((random-table/reporter
                 #'random-table/reporter/as-child-window))
           (random-table/roll "The One Ring > Lore"))))
     ("r r" "Revelation Episode…"
       (lambda ()
         (interactive)
         (let ((random-table/report-formatter/function
                 #'random-table/report-formatter/only-roll))
           (random-table/roll "Revelation: {The One Ring > Revelation Episode}"))))
    ("r s" "Skill check…"
     (lambda ()
       (interactive)
       ;; I don't know how I'd shoe horn this into the random-table system.
       (insert (call-interactively 'jf/gaming/the-one-ring/roll/skill-check))))
    ("r t" "Telling table…"
     (lambda ()
       (interactive)
       ;; TODO: get this working using the random table.
       (insert (call-interactively 'jf/gaming/the-one-ring/roll/telling-table))))
    ]
   ["Conditions"
    ("-m" jf/gaming/the-one-ring/character-is-miserable/set :transient t)
    ("-w" jf/gaming/the-one-ring/character-is-weary/set :transient t)
    ("-W"  jf/gaming/the-one-ring/character-is-wounded/set :transient t)
    ]
   ["Jump To"
    ("j c" "Character Sheet"
     (lambda ()
       (interactive)
       (find-file-other-window jf/gaming/the-one-ring/strider-mode/character-sheet-filename)))
    ("j i" "Index of Campaign"
     (lambda ()
       (interactive)
       (find-file-other-window jf/gaming/the-one-ring/strider-mode/campaign-index-filename)))
    ("j x" jf/gaming/the-one-ring/strider-mode/experience-milestones)
    ]])

(provide 'jf-the-one-ring)
;;; jf-the-one-ring.el ends here
