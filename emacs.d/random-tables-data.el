(if (f-file?  "~/git/random-table.el/random-table.el")
  (require 'random-table "~/git/random-table.el/random-table.el")
  (use-package random-table
    :straight (:host github :repo "jeremyf/random-table.el")))

(setq random-table/reporter #'random-table/reporter/as-insert)

(keymap-global-set "H-r" #'random-table/roll)
(keymap-global-set "C-H-r" #'random-table/roll-region)

(defun random-table/roll/ironsworn (modifier)
  "Roll for an Ironsworn action with the given MODIFIER."
  (interactive "NModifier: ")
  (let* ((action_roll (+ (random 6) 1 (or modifier 0)))
          (challenge_roll_one (+ (random 10) 1))
          (challenge_roll_two (+ (random 10) 1))
          (result (cond
                    ((and (> action_roll challenge_roll_one)
                       (> action_roll challenge_roll_two))
                      "strong hit")
                    ((and (>= challenge_roll_one action_roll)
                       (>= challenge_roll_two action_roll))
                      "miss")
                    (t "weak hit"))))
    (insert (format "%s (%s vs [%s, %s])"
              result
              action_roll
              challenge_roll_one
              challenge_roll_two))))

(random-table/prompt "Charisma Modifier"
  :type #'read-number
  :default 0)

(random-table/prompt "Reaction Modifier"
  :type #'completing-read
  :range '(("0 Nothing" . 0)
            ("-3 for grave insults or risks to the life of self or loved ones" . -3)
            ("-2 for insults or risks to the NPC’s wealth or standing" . -2)
            ("-1 for the risk of significant cost to their actions" . -1)
            ("+1 for a modest bribe or when a favor is owed to a PC" . 1)
            ("+2 for a large bribe or significant service owed" . 2)
            ("+3 for a PC who saved their life or did similar service" . 3)))

(random-table/prompt "Saving Throw Score"
  :type #'read-number
  :default 15)

(defun jf/gaming/errant/movement-dice (prefix)
  "Calculate an Errant's movement dice.  When given a PREFIX open the HTML page."
  (interactive "P")
  (if prefix
    (shell-command (concat
                     "open ~/git/takeonrules.source/static/errant/index.html"))
    (let* ((range '("4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16"
                     "17" "18" "19" "20"))
            ;; Yeah yeah yeah, I could probably automate this creation; well I
            ;; did it in Ruby because that's my primary language.
            (slot-range '("0" "0.25" "0.5" "0.75" "1" "1.25" "1.5" "1.75" "2"
                           "2.25" "2.5" "2.75" "3" "3.25" "3.5" "3.75" "4"
                           "4.25" "4.5" "4.75" "5" "5.25" "5.5" "5.75" "6"
                           "6.25" "6.5" "6.75" "7" "7.25" "7.5" "7.75" "8"
                           "8.25" "8.5" "8.75" "9" "9.25" "9.5" "9.75" "10"
                           "10.25" "10.5" "10.75" "11" "11.25" "11.5" "11.75"
                           "12" "12.25" "12.5" "12.75" "13" "13.25" "13.5"
                           "13.75" "14" "14.25" "14.5" "14.75" "15" "15.25"
                           "15.5" "15.75" "16" "16.25" "16.5" "16.75" "17"
                           "17.25" "17.5" "17.75" "18" "18.25" "18.5" "18.75"
                           "19" "19.25" "19.5" "19.75" "20" "20.25" "20.5"
                           "20.75" "21" "21.25" "21.5" "21.75" "22" "22.25"
                           "22.5" "22.75" "23" "23.25" "23.5" "23.75" "24"))
            (phys (string-to-number (completing-read "Physique: " range nil t)))
            (skil (string-to-number (completing-read "Skill: " range nil t)))
            (slots-hand (string-to-number (completing-read "Slots in hand: "
                                            (subseq slot-range 0 9) nil t)))
            (slots-handy (string-to-number (completing-read "Slots in handy: "
                                             (subseq slot-range 0 17) nil t)))
            (slots-worn (string-to-number (completing-read "Slots in worn: "
                                            slot-range nil t)))
            (slots-pack (string-to-number (completing-read "Slots in pack: "
                                            slot-range nil t)))
            (text (format (concat "Errant Movement\n- Physique: %s · Skill: %s"
                            "\n- Slots Hand: %s · Handy: %s · Worn: %s · "
                            "Pack: %s")
                    phys skil slots-hand slots-handy slots-worn slots-pack)))
      (dolist (label-slots (list (cons "in hand, handy, worn, pack"
                                   (+ slots-hand slots-handy slots-worn
                                     slots-pack))
                             (cons "in hand, handy, worn"
                               (+ slots-hand slots-handy slots-worn))
                             (cons "handy, worn"
                               (+ slots-handy slots-worn))
                             (cons "worn" slots-worn)
                             (cons "naked and free" 0)))
        (let* ((slots (cdr label-slots))
                (label (car label-slots))
                (enc (if (>= phys slots)
                       (floor (* 4 slots) phys)
                       (+ 4 (- (floor slots) phys))))
                (spd (if (>= skil enc) (- skil enc) 0))
                (md (floor spd 4))
                (md-text (if (= 0 md) "0" (format "%sd4" md))))
          (setq-local text (format "%s\n- %s\n  ENC: %s · SPD: %s· MD: %s"
                             text label enc spd md-text))))
      (kill-new text)
      (message text))))

;;; Support Functions
(defun random-table/roller/saving-throw (table)
  "Prompt for rolling a saving throw for the given TABLE."
  (let ((score (random-table/prompt "Saving Throw Score"))
         (modifier (read-number
                     (format "%s\n> Modifier: "
                       (random-table-name table))
                     0))
         (roll (random-table/roller/string "1d20")))
    (cond
      ((= roll 1) "Fail")
      ((= roll 20) "Save")
      ((>= (+ roll modifier) score) "Save")
      (t "Fail"))))

(defun random-table/roller/prompt-from-table-data (table)
  "Prompt for picking a range from a TABLE."
  (completing-read
    (format "%s via:" (random-table-name table))
    (random-table-data table) nil t))

;;; Errant
(random-table/register :name "Errant > Henchman"
  :data (list
          (concat "\n- Archetype :: {Errant > Henchman > Archetype}"
            "\n- Renown :: {Errant > Henchman > Renown}"
            "\n- Morale :: {(Errant :: Henchman :: Morale Base) + (Errant :: Henchman :: Morale Variable)}"
            "\n{Errant > Character}")))

(random-table/register :name "Errant > Henchman > Archetype"
  :private t
  :roller "1d10"
  :data '(((1 . 5) . "Warrior")
           ((6 . 8) . "Professional")
           ((9 . 10) . "Magic User")))

(random-table/register :name "Errant > Henchman > Morale Base"
  :private t
  :roller (lambda (table) (read-number "Hiring PC's Presence Score: "))
  :data '(((3 . 4) . 5)
           ((5 . 8) . 6)
           ((9 . 13) . 7)
           ((14 . 16) . 8)
           ((17 . 18) . 9)
           ((19 . 20) . 10)))

(random-table/prompt "Additional Generosity of Offer"
  :type #'completing-read
  :range '(("Nothing" . 0)
            ("+25%" . 1)
            ("+50%" . 2)
            ("+75% or more" . 3)))

(random-table/register :name "Errant > Henchman > Morale Variable"
  :private t
  :roller '(+ "2d6" "Additional Generosity of Offer")
  :data '(((2) . -2)
           ((3 . 5) . -1)
           ((6 . 8) . 0)
           ((9 . 11) . 1)
           ((12 . 15) . 2)))

(random-table/prompt "Hiring location for Henchman"
  :type #'completing-read
  ;; TODO: Can I use "1d2" in this situation?
  :range '(("Hamlet" . (lambda (table) 1))
            ("Village" . (lambda (table) (random-table/roller/string "1d2")))
            ("Town" . (lambda (table) (random-table/roller/string "1d3")))
            ("City" . (lambda (table) (random-table/roller/string "1d4")))
            ("Metropolis" . (lambda (table) (random-table/roller/string "1d5")))))

(random-table/register :name "Errant > Henchman > Renown"
  :roller (lambda (table)
            (funcall (random-table/prompt "Hiring location for Henchman") table))
  :private t
  :data '(1 2 3 4 5))

(random-table/register :name "Errant > Reaction Roll"
  :roller '(+ "2d6" "-3/+3 Modifier" "Alignment Modifier")
  :data '(((-10 . 2) . "Hostile [DV +8] (Disposition {CURRENT_ROLL})")
           ((3 4 5) . "Unfriendly [DV +4] (Disposition {CURRENT_ROLL})")
           ((6 7 8) . "Unsure (Disposition {CURRENT_ROLL})")
           ((9 10 11) . "Amicable [DV -2] (Disposition {CURRENT_ROLL})")
           ((12 . 24) . "Friendly [DV -4] (Disposition {CURRENT_ROLL})")))

;; We're registering this to generate the correct Archetype based on the rolled
;; ability scores.  Hence the :reuse declaration and it's :private nature.
(dolist (ability '("Errant > Ability > Physique"
                    "Errant > Ability > Skill"
                    "Errant > Ability > Mind"
                    "Errant > Ability > Presence"))
  (random-table/register :name ability
    :store t
    :private t
    :reuse ability ;; Because we might roll the Archetype first, which is
    ;; informed by the ability scores rolled.
    :roller "4d4"
    :data '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))

(random-table/register :name "Errant > Character"
  :data (list (concat
                "\n- Archetype :: {Errant > Archetype}"
                "\n- Ancestry :: {Errant > Ancestry}"
                "\n"
                "\n- Physique :: {Errant > Ability > Physique}"
                "\n- Skill :: {Errant > Ability > Skill}"
                "\n- Mind :: {Errant > Ability > Mind}"
                "\n- Presence :: {Errant > Ability > Presence}"
                "\n"
                "\n- Failed Profession :: {Errant > Failed Professions}"
                "\n- Keepsakes :: {Errant > Keepsakes}"
                "\n"
                "\nEquipment"
                "{Errant > Archetype > Equipment}"
                "\n- A backpack."
                "\n- A medium weapon of their choice (1 item slot)."
                "\n- A quiver of ammunition, if needed (1 item slot, depletion 2)."
                "\n- A bedroll (1 item slot)."
                "\n- A torch (1⁄2 item slot, burn 2)."
                "\n- 50’ of rope (1⁄2 item slot)."
                "\n- A mess kit (1⁄4 item slot)."
                "\n- A tinderbox (1⁄4 item slot)."
                "\n- Rations (1⁄4 item slot, depletion 1)"
                "\n- A waterskin (1⁄4 item slot)."
                "\n- 4 supply (1⁄4 item slot each).")))

(random-table/register :name "Errant > Archetype > Equipment"
  :reuse "Errant > Archetype"
  :fetcher (lambda (data roll) (alist-get (car roll) data nil nil #'string=))
  :data '(("Violent" . "\n- Heavy weapon (2 item slots) or a Small shield (1 item slot, 4 blocks) or Large shield (2 item slots,
6 blocks)")
           ("Deviant" . "\n- Burglar’s tools (1 item slot) or an Alchemist’s kit (1 item slot)")
           ("Occult" . "{Errant > Grimoire}{Errant > Grimoire}{Errant > Grimoire}{Errant > Grimoire}")
           ("Zealot" . "\n- One relic (Blade, Wand, Talisman, Calice see p. 75)")))

(cl-defun random-table/roller/archetype (table &key (attribute-template "Errant > Ability > %s"))
  "Using the TABLE data determine Errant Archetype.

This function rolls (or uses the rolls) of the corresponding
tables derived from the given ATTRIBUTE-TEMPLATE.

This will return the CDR of a CONS pair of the given TABLE's data.  That way the
value we cache is the Archetype.

See “Errant :: Archetype” table."
  (let* ((attribute_archetypes (random-table-data table))
          ;; (("Physique" . 10) ("Skill" . 9) ("Mind" . 8) ("Presence" . 7)
          (attr_rolls
            (mapcar (lambda (attr_arch)
                      (cons (car attr_arch)
                        (string-to-number
                          (random-table/parse
                            (format attribute-template (car attr_arch))))))
              attribute_archetypes))
          ;; (10 9)
          (top_ability_scores
            (take 2 (seq-uniq (sort (mapcar #'cdr attr_rolls) #'>))))
          (candidates '())
          ;; We always want to add candidates to the list, hence the
          ;; compare is via no-comp
          (no-comp (lambda (a b) nil )))
    (dolist (attr_roll attr_rolls)
      ;; When the roll is part of the top two attributes, add the
      ;; Attribute's associated Archetype to the candidate list.
      (when (member (cdr attr_roll) top_ability_scores)
        (add-to-list 'candidates (car attr_roll) nil no-comp))
      ;; When the roll is the highest roll, skew the odds towards the
      ;; Attribute's associated Archetype.
      (when (= (cdr attr_roll) (nth 0 top_ability_scores))
        ;; Adding at least 1 more when we're working with the high
        ;; attribute score.
        (dotimes (i (/ (- (+ 1 (cdr attr_roll))
                         (nth 1 top_ability_scores))
                      2))
          (add-to-list 'candidates (car attr_roll) nil no-comp))))
    ;; candidates = ("Physique" "Physique" "Skill")
    (alist-get (seq-random-elt candidates) attribute_archetypes)))

(random-table/register :name "Errant > Archetype"
  :private t
  :store t
  :roller #'random-table/roller/archetype
  ;; We need to overload the fetcher to simply use the result derived from the
  ;; roller.  The data is provided as a map of classes.
  :fetcher (lambda (data result) (car result))
  :data '(("Physique" . "Violent")
           ("Skill" . "Deviant")
           ("Mind" . "Occult")
           ("Presence" . "Zealot")))

(random-table/register :name "Errant > Grimoire"
  :data '("\n- Grimoire (1⁄4 item slot)\n  - Essence :: {Errant > Grimoire > Essence}\n  - Sphere :: {Errant > Grimoire > Sphere}\n  - Theme :: {Errant > Grimoire > Theme}"))

(random-table/register :name "Errant > Grimoire > Essence"
  :private t
  :data '("Protect" "Summon" "Control" "Quicken" "Slow"
           "Comprehend" "Move" "Animate" "Link" "Command"
           "Curse" "Destroy" "Create" "Bless" "Take"
           "Transfer" "Switch" "Reveal" "Hide" "Restrict"
           "Liberate" "Reflect" "Seal" "Request" "Grow"
           "Shrink" "Open" "Close" "Transform" "Communicate"
           "Improve" "Diminish" "Incapacitate" "Return" "Send"
           "Enter" "Become" "Replace" "Convert" "Complete"
           "Attract" "Repulse" "Absorb" "Increase" "Reduce"
           "Receive" "Aid" "Hinder" "Interrupt" "Harm"))

(random-table/register :name "Errant > Grimoire > Sphere"
  :private t
  :data '("Magic" "Space" "Time" "Mind" "Spirit" "Body"
           "Elements" "Dimensions" "Life" "Death" "Objects" "Biota"))

(random-table/register :name "Errant > Grimoire > Theme"
  :private t
  :data '("reflection, mirror, prediction" "sound, music, resonance"
           "metal, restraint, imprisonment" "possession, betrayal, parasitism"
           "flesh, secrets, extortion" "oath, trust, witness"
           "assassination, retribution, target" "illumination, shadow, flicker"
           "miasma, contagion, breath" "clairvoyance, truth, fortune"
           "refraction, vision, geometry" "microcosm, voyage, homecoming"
           "sacrifice, wisdom, prophecy" "return, arc, gravity"
           "perception, foresight, awareness" "perspective, containment, curvature"
           "tone, air, catalyst" "ice, cessation, continuation"
           "fire, extinguish, forbiddance" "water, treachery, pressure"
           "earth, openings, weight" "storms, navigation, flight"
           "lightning, conductivity, thunder" "undeath, oppression, hierarchy"
           "nostalgia, haunting, denial" "identity, disguise, personality"
           "puzzle, solution, contradiction" "egocentrism, trinket, expression"
           "revival, rejuvenation, animation" "persuasion, manipulation, contempt"
           "silence, melody, listening" "replication, subversion, belief"
           "fluidity, doubt, cowardice" "slumber, consciousness, meditation"
           "ambiguity, confusion, hypnosis" "intent, purpose, narrative"
           "memory, archive, history" "threshold, passivity, motion"
           "emptiness, disparity, difference" "void, origin, periphery"
           "culture, tradition, artefact" "entrance, forbiddance, security"
           "rotation, perpetuation, cycle" "clarification, association, relativism"
           "heist, hold, prize" "potential, domestication, savagery"
           "odour, intoxication, unconsciousness" "torture, resilience, limit"
           "survival, adaptation, predation" "representation, fortitude, vitality"
           "authority, judgement, corruption" "escape, fate, crime"
           "celerity, delivery, contact" "benevolence, medicine, dosage"
           "record, temptation, repugnance" "armour, protection, integrity"
           "fortification, repair, construction" "reduction, multiplication, calculation"
           "signature, disappointment, division" "banishment, position, expulsion"
           "pact, willpower, malice" "pestilence, desperation, frailty"
           "completion, separation, origin" "fertility, reincarnation, gestation"
           "lust, chastity, obscenity" "gluttony, temperance, inebriation"
           "war, provocation, injury" "youth, redemption, forgiveness"
           "luck, probability, gamble" "causality, consequence, inevitability"
           "birth, shame, guilt" "famine, cruelty, deprivation"
           "death, overthrow, mortality" "hostility, recklessness, dedication"
           "sloth, diligence, rest" "chance, play, serendipity"
           "greed, charity, property" "creation, texture, destruction"
           "passion, diplomacy, power" "architecture, support, decay"
           "learning, loss, irony" "home, empathy, hunger"
           "toxin, psyche, affection" "shatter, sight, distortion"
           "leverage, rebellion, resistance" "exploration, discovery, Alignment"
           "ritual, compassion, community" "correction, purification, mystery"
           "recognition, honour, resourcefulness" "foolishness, reprisal, gender"
           "secrets, growth, heat" "dirt, focus, rebirth"
           "denial, filth, depression" "wrath, patience, violence"
           "envy, gratitude, theft" "pride, humility, hubris"
           "prudence, reason, discernment" "temperance, appetites, production"
           "fortitude, courage, defence" "justice, proportionality, impartiality"))

(random-table/register :name "Errant > Ancestry"
  :private t
  :data '("Tough (Once per session, when you would be reduced to 0 hp, you may choose to be reduced to 1 hp instead)."
           "Arcane (Once per session, you can attempt to perform a minor magic related to your ancestry: roll 2d6 and add your renown, on a 10+ you succeed, on a 7-9 a complication occurs, on a 6 or lower, failure)."
           "Cunning (Once per session, you may reroll any d20 roll)."
           "Adaptable (Once per session, you may choose to use one attribute for a check in lieu of another)."))

(random-table/register :name "Errant > Keepsakes"
  :data '("The sword of the hero Black Mask. Useless, but looks really cool."
           "Big, floppy cork hat. Waterproof."
           "Strange pair of boots, with four wheels attached to each sole."
           "Jar of pungent pickled eggs, given to you by a stranger on a carriage."
           "Pair of cosy, woollen socks."
           "Bucket filled with crabs."
           "Goblin child: it is convinced you are its mother."
           "Case of costume jewellery. Worthless, but convincing from a distance."
           "Deck of cards with an extra ace."
           "Banned edition of the major holy text of the land, filled with heretical dogma and apocryphal stories."
           "Large hoop skirt, big enough to hide a small child in."
           "Bagpipes."
           "Black leather boots, knee-high. Black leather gloves, elbow- length. A riding crop. A gag."
           "Just two guys, ready to help you out. They’re burly, they’re brawny, they’re best friends."
           "Coat you stole from a disgraced magician. Full of kerchiefs, dead doves, and other miscellanea."
           "The signet ring of an unknown king."
           "Dwarven treasure dog, loyal but cowardly."
           "Pouch of firecrackers."
           "A dolorous cow."
           "String of 12 hard sausage links."
           "Bottle of incredibly fine whiskey, which you clearly stole."
           "10’ spool of thin, copper wire."
           "Pincushion, filled with pins."
           "The finest ham in all the land, smoked by the man, Pitmaster Sam!"
           "Long, strong elastic cord."
           "Bowling ball."
           "Small vial of acid. Very corrosive."
           "Bag of chilli powder."
           "Needle and thread."
           "Wig of beautiful golden hair. Reaches down to your ankles."
           "Bag of beloved marbles that you won from a child."
           "Several small jars of bright acrylic paints."
           "Unnerving and upsettingly lifelike puppet."
           "Incredibly avant-garde and impractical clothes that no sane person would be willing to purchase."
           "Small bag of incredibly pungent and heady herbs. When burned, even smelling the smoke is enough to intoxicate someone."
           "Package, addressed to someone you don’t know, in some place you’ve never heard."
           "Rake."
           "Bottle of lubricant, suitable for internal, external, and industrial use."
           "Extremely springy spring."
           "Mechanically articulated hand attached to a stick. All of the fingers can be controlled independently, though it is quite confusing to operate."
           "Lump of clay."
           "Wind-up music box."
           "Tube of fast-drying, industrial-strength glue."
           "Pair of stilts."
           "Book of fiery, righteous, political polemic."
           "Pair of tinted spectacles."
           "Very fine squash."
           "Vial of medicine, syrupy and sweet. Makes one quite drowsy."
           "Bag of flour."
           "Plague doctor’s mask, stuffed with fragrant herbs."
           "Wheel of aged Grey Matter, the mouldiest cheese in the world. Causes intense hallucinations."
           "Pouch of laxative powder."
           "Snorkel."
           "Worn, dog-eared copy of the novel Lust & Larceny: The Trysts of the Amorous Elven Thief, Vol 1. While lowbrow, the book is incredibly engrossing; it’s hard to pull yourself away from it."
           "Glitter."
           "Jug of genuine wolf piss."
           "Fire-squirt."
           "Bottle of rat poison."
           "Pouch of beans."
           "Snake."
           "A few pamphlets of surprisingly convincing conspiracy theories."
           "Pot labelled ‘rice pudding’ that is actually filled with liquid cement."
           "Glass case of pinned butterflies."
           "Two magnetic spoons."
           "Collapsible walking cane."
           "Priest’s vestments."
           "Game with stone pieces and a cloth board. The accompanying instruction booklet is full of poorly worded, incomprehensible, and contradictory rules."
           "A trio of newborn puppies."
           "Small glass cylinder, rounded at the tips. Quite phallic."
           "Sachet of dried cooking herbs."
           "Packets of various coloured dye powders."
           "Thick, heavy blanket you’ve carried with you since childhood."
           "Hand-bound notebook, containing six quite touching love poems. The names of the beloved in each poem have been crossed out and rewritten multiple times."
           "Set of clothes lined with fleece. Very warm."
           "Dismembered pinky finger with a long painted red fingernail."
           "The flu."
           "Small sundial attached to a wrist strap."
           "Booklet of various fashionable hair, beard, and moustache styles."
           "Crystal monocle, also useful as a lens."
           "Polished metal hand mirror."
           "Delicious cake, baked for you by your sweetheart."
           "An incredibly belligerent goose."
           "A four-leaf clover."
           "Packet of saccharinely sweet lollipops."
           "Large bar of hard soap, floral scented."
           "Bag of small ceramic balls, which explode in a blinding flash of light when thrown."
           "Small tube of pale pink face paint."
           "Umbrella."
           "Tub of styling gel."
           "Rapidly decomposing fish."
           "Bottle of incredibly pungent perfume."
           "Trained messenger pigeon."
           "Fine-mesh net."
           "Pouch of itching powder."
           "Hand drum."
           "A dozen angry hornets in a jar."
           "Wind-up clockwork toy."
           "Your dad. Capable of criticizing anyone till they feel incompetent and worthless."
           "Jar of sweet, sticky honey."
           "Set of loaded dice."))

(random-table/register :name "Errant > Failed Professions"
  :data
  '("Acrobat" "Alewife" "Antiquarian" "Apothecary" "Armpit-hair plucker"
     "Baker" "Ball-fetcher" "Barber" "Barrel maker" "Beadle"
     "Bee exterminator" "Beekeeper" "Beggar" "Belt maker" "Busker"
     "Carcass collector" "Chandler" "Cheesemaker" "Cherry picker" "Chimney sweep"
     "Clockwinder" "Cobbler" "Confectioner" "Cooper" "Cordwainer"
     "Costermonger" "Cup bearer" "Cutlery vendor" "Cutpurse" "Ditch digger"
     "Dog walker" "Dog whipper" "Dollmaker" "Ewerer" "Executioner"
     "Fish gutter" "Flatulist" "Fletcher" "Florist" "Flyter"
     "Fortune teller" "Funeral clown" "Galley rower" "Gambler" "Glove maker"
     "Gongfarmer" "Grave digger" "Gymnasiarch" "Haberdasher" "Hoof trimmer"
     "Hunter" "Ice cutter" "Jester" "Jongleur" "Knock-knobber"
     "Knocker-upper" "Leech collector" "Market guard" "Messenger" "Mountebank"
     "Mushroom farmer" "Nanny" "Orgy planner" "Ostrich wrangler" "Owl vomit collector"
     "Palanquin bearer" "Peddler" "Pickpocket" "Poet" "Portraitist"
     "Powder monkey" "Purefinder" "Rat catcher" "Resurrectionist" "Roofer"
     "Sailor" "Scribe" "Scullion" "Seed counter" "Snake milker"
     "Smuggler" "Sophist" "Stablehand" "Stevedore" "Stone eater"
     "Sycophant" "Tanner" "Taster" "Taxidermist" "Tinker"
     "Toad doctor" "Tosher" "Town crier" "Urinatores" "Usurer"
     "Water carrier" "Wheelwright" "Whipping boy" "Whiffler" "Worm rancher"))

(random-table/register :name "Errant > General Event"
  :data '((1 . "Encounter")
           (2 . "Delay")
           (3 . "Resource use")
           (4 . "Local effect")
           (5 . "Clue")
           (6 . "Free from effect")))

(random-table/prompt "-3/+3 Modifier"
  :type 'bound-integer-range
  :range '(-3 -2 -1 0 1 2 3))

(random-table/prompt "Alignment Modifier"
  :type 'bound-integer-range
  :range '(-3 -2 -1 0 1 2 3))

(random-table/register :name "Errant > General Downtime Turn Action"
  :roller '(+ "2d6" "-3/+3 Modifier")
  :data '(((-10 . 6) . "Failure, no progress.")
           ((7 . 9) . "/Setback/, partial success, or progress.")
           ((10 . 22) . "Success, mark progress on /tracker/.")))

(random-table/register :name "Errant > Chase Developments"
  :data '((1 . "Hiding Spot - neither side has line of sight on the other. The character with the lowest spd makes a check to hide. If they succeed, they can’t be found and the chase ends; if they fail, the pursuers immediately make a movement roll.")
           (2 . "Throng - a crowd of people, a flock of animals, or some other group impedes progress. The characters on that side may attempt to convince the throng to assist them if possible, or else someone must make a check to clear a path. On a failed check, the opposing side immediately makes a movement roll.")
           (3 . "Dilemma - the characters face a decision between two unfavourable options, such as having to choose to divert to a more difficult path or plough through a crowd.")
           (4 . "Hazard – something threatens the side that rolled this result; they must make a check to avoid damage, or some other unfavourable situation such as being knocked prone.")
           (5 . "Obstacle - something impedes progress on the path; the character with the highest spd must make a check to bypass the obstacle, else the opposing side immediately makes a movement roll.")
           (6 . "Opportunity - a character on the side who rolled this result can immediately take an extra action, though they must decide what to do quickly.")
           (7 . "Paths Converge - a character on the side that rolled this result and a character on the opposing side cross paths momentarily, coming within a hair’s breadth of each other; they may each make an action before the trail separates them once more.")
           (8 . "Risky Shortcut - a risky shortcut presents itself. Characters on this side may take this shortcut, but must make a check to do so. If they succeed, they immediately sprint. If they fail, they are separated from the others on their side and taken out of the chase.")
           (9 . "Separated - a character on the side which rolled this result is separated from the rest of their side, and is tracked separately till they can reunite with their group. If the character was on the pursuing side, they must make a check or be taken out of the chase.")
           (10 . "Twist - the situation changes in some way; perhaps a new group joins the chase, or the side that is pursuing and the side that is being pursued switch; the environment might change, as might the conditions that end the chase.")))

(random-table/register :name "Errant > Downtime Event"
  :data '((1 . "Encounter: the COMPANY encounters an NPC(s). The guide may wish to have a list of random encounters prepared.")
           (2 . "Complication: a negative issue affects the region;\n- {Errant > Downtime Event > Complications}")
           (3 . "Expiration: any ongoing complications end. Any other temporary situations, arrangements, or benefits end.")
           (4 . "Trend: a positive or novel issue affects the region;\n- {Errant > Downtime Event > Trends}")
           (5 . "Intimation: the COMPANY receives some clue, perhaps relating to their next adventure, or to what the next encounter, complication, or trend may be.")
           (6 . "Free: nothing happens! The COMPANY gains a much needed reprieve and are allowed to complete their actions in peace.")))

(random-table/register :name "Errant > Downtime Event > Complications"
  :private t
  :roller "2d6"
  :data '((2 . "Natural disaster (e.g. a fire, a tornado, a meteor)." )
           (3 . "Ongoing disaster (e.g. a famine, a plague, a drought)." )
           (4 . "Major figure assassinated." )
           (5 . "Series of murders begins." )
           (6 . "A SCOURGE arises in the region." )
           (7 . "An ERRANT’s ESTATE, INSTITUTION, infrastructure project, DOMAIN, or other goal suffers a setback." )
           (8 . "Legal claims are brought against the COMPANY or they are publicly slandered." )
           (9 . "An ally of the COMPANY loses trust in or cuts ties with them." )
           (10 . "An insurrection or a siege occurs. If not dealt with in {1d4} [1d4] DOWNTIME TURNS it will be successful." )
           (11 . "Two or more fACTIONS begin to oppose each other or actively go to war." )
           (12 . "An ally of the COMPANY dies." )))

(random-table/register :name "Errant > Downtime Event > Trends"
  :private t
  :roller "2d4"
  :data '((2 . "Two or more FACTIONS announce an alliance.")
           (3 . "A religious event occurs (e.g. an omen or apparition).")
           (4 . "A scandal is revealed.")
           (5 . "New NPC arrives in the area.")
           (6 . "A rival COMPANY arrives in the area")
           (7 . "A discovery is made (e.g. new technology, new lands).")
           (8 . "A new FACTION emerges.")))

(random-table/register :name "Errant > Weather"
  :roller #'random-table/roller/prompt-from-table-data
  :data '(("Winter" . "Winter :: {Errant > Weather > Winter}")
           ("Spring" . "Spring :: {Errant > Weather > Spring}")
           ("Summer" . "Summer :: {Errant > Weather > Summer}")
           ("Autumn" . "Autumn :: {Errant > Weather > Autumn}")))

(random-table/prompt "Errant :: Weather :: Previous Day"
  :type #'completing-read
  :range '(("Overcast" . -2)
            ("Clear Skies" . 2)
            ("Other" . 0)))

(random-table/register :name "Errant > Weather > Winter"
  :private t
  :roller '(+ "2d6" "Errant :: Weather :: Previous Day")
  :data '(((0 . 2) . "/Severe weather/ (e.g. blizzard)")
           ((3 . 5) . "/Severe weather/ (e.g. hail storm)")
           ((6 . 8) . "/Inclement weather/ (e.g. sleet)")
           ((9 . 11) . "Overcast (-2 to next weather roll)")
           ((12 . 14) . "Clear skies (+2 to next weather roll)")))

(random-table/register :name "Errant > Weather > Autumn"
  :private t
  :roller '(+ "2d6" "Errant :: Weather :: Previous Day")
  :data '(((0 . 2) . "/Severe weather/ (e.g. hurricane)")
           ((3 . 5) . "/Inclement weather/ (e.g. fog)")
           ((6 . 8) . "Overcast (-2 to next weather roll)")
           ((9 . 11) .  "Cloudy")
           ((12 . 14) . "Clear skies (+2 to next weather roll)")))

(random-table/register :name "Errant > Weather > Summer"
  :private t
  :roller '(+ "2d6" "Errant :: Weather :: Previous Day")
  :data '(((0 . 2) . "/Severe weather/ (e.g. thunderstorm)")
           ((3 . 5) . "/Inclement weather/ (e.g. heat wave)")
           ((6 . 8) . "Sunny")
           ((9 . 11) .  "Clear skies (+2 to next weather roll)")
           ((12 . 14) . "Beautiful day (only need to spend one TRAVEL TURN /sleeping/.")))

(random-table/register :name "Errant > Weather > Spring"
  :private t
  :roller '(+ "2d6" "Errant :: Weather :: Previous Day")
  :data '(((0 . 2) . "/Inclement weather/ (e.g. down pour)")
           ((3 . 5) . "Cosmetic change (e.g. drizzle)")
           ((6 . 8) . "Cloudy")
           ((9 . 11) .  "Clear skies (+2 to next weather roll)")
           ((12 . 14) . "Beautiful day (only need to spend one TRAVEL TURN /sleeping/.")))

(random-table/register :name "Errant > Debt Holder"
  :data '("[unctuous/sententious/truculent/supercilious/fulsome/vainglorious] [eunuch/merchant/clergyman/madam/officer/intellectual/cultist]"))

(random-table/register :name "Errant > Conspicuous Consumption Failed Save"
  :roller "d20"
  :data '((1 . "Something gets burned down or destroyed; roll a d6 to see how bad it was: on a 1, confined to a single building; on a 6, a big part of town has gone up. Future CONSPICUOUS CONSUMPTION rolls receive a penalty equal to the d6 roll till it’s repaired.  Roll another d6 to see who knows: on a 4 or lower, just the COMPANY knows; on a 5, a blackmailer knows; on a 6, EVERYBODY knows.")
           (2 . "Beaten and robbed: lose half HP and all items in inventory.")
           (3 . "Magical affliction: someone or something has put a curse on you, or transformed you into an animal.")
           (4 . "You’ve gotten into legal trouble. You’re due to appear in court. Roll a d6 to see how bad the charges are; the lower the roll the worse it is.")
           (5 . "You’ve contracted a disease or infection.")
           (6 . "You’ve made an enemy; a random npc now hates you. Roll a d6 to see how bad it is: on a 1, they can’t stand your presence, on a 6, they’re after your head.")
           (7 . "You’ve insulted a local person or organisation of import. Lose {1d4} [d4] FACTION reputation.")
           (8 . "You wake up in a random adjacent hex, stark naked, in someone or something’s house/lair. Your friends have all your stuff. Roll a REACTION ROLL for your host.")
           (9 . "You get into a brawl. Lose d6 HP.")
           (10 . "You’ve got a hangover. All checks for the next two TRAVEL TURNS have DV +2.")
           (11 . "You’ve made a pact with a god, devil, or some other supernatural power, and have to do some quest or task for them.")
           (12 . "You’re betrothed. Calling off the marriage will incur the wrath of the family or your scorned lover. If you’re already married, this could get messy.")
           (13 . "You’ve earned notoriety as a gadabout. Your next CONSPICUOUS CONSUMPTION roll will be doubled.")
           (14 . "You’ve gotten a new tattoo or some other bodily alteration. Roll a d6 to see how bad it is: on a 1, it’s offensive to EVERYONE (-2 to all REACTION ROLLS while it’s visible); on a 6, it's actually pretty cool.")
           (15 . "You’ve made an ass of yourself in town. No one will take you seriously for the next {1d4} [d4] DOWNTIME TURNS.")
           (16 . "You’ve been initiated into a cult, secret society, or some other organisation.")
           (17 . "You’ve impressed someone, made an ally, or attracted a new retainer.")
           (18 . "You get a windfall. Receive half money spent on CONSPICUOUS CONSUMPTION back.")
           (19 . "You make a discovery or hear a rumour of some sort.")
           (20 . "You make an advancement. Increase rolled ATTRIBUTE by 1.")))


;;; Black Sword Hack
(random-table/register :name "Black Sword Hack > Demon Name"
  :data '("Beleth" "Abaddon" "Ulshedra" "Marduk" "Raum"
           "Halphas" "Ashurban" "Ordog" "Charun" "Surgat"
           "Ahriman" "Wissigo" "Furcas" "Keldim" "Gorgo"
           "Rahab" "Gaki" "Samnu" "Namtar" "Baalberith"))

(random-table/register :name "Black Sword Hack > Nickname"
  :data '("The Graceless" "Two beards" "the Killing Machine" "Blue Belly"
           "the Moon Child" "the Giggling Killer" "the Unwise" "Starwatcher"
           "Cruddy" "the Inflexible" "Many Tongues" "the Duteous"
           "the Questionable" "Bad Fortune" "the Half Great" "He Who Listens"
           "the Barbarous" "No Horse" "the Silent" "Devourer"
           "Smallpox" "the Night Beast" "the Skullmonger" "Godslayer"
           "the Chewer" "She Who Dances For No One" "the Shadow Maker" "the Painful"
           "the Pale Sinner" "the Greenish" "One Tooth" "the Absurdist"
           "the Chronicler" "the Unscrupulous" "Demonbane" "the Indomitable"
           "the Pain Dealer" "Wülf Hair" "the Silver Squirrel" "the Purple Usurper"
           "the Slavemaker" "the Sceptic" "the Uncrowned" "the Dark Rambler"
           "the Soulless" "the Golden" "the Unafraid" "the Creaky Marauder"
           "the Eternal Loser" "the Subjugator" "Non-Dead" "the Star King"
           "the Inscrutable" "the Glatisant" "Three Eyes" "the Walking Dread"
           "Kingmaker" "the Holy Mumbler" "He Who Loves The Dead" "Dogface"
           "the Rationalist" "the Ethereal Cannibal" "The Wind Wanderer" "She Who Comes From Below…"))

(random-table/register :name "Black Sword Hack > How to find the demon you seek"
  :data '("By killing its master, a prominent figure in this town"
           "In a well, posing as a divining spirit,spreading awful rumours"
           "In the queen’s secret jewel cabinet"
           "Hidden inside a map, on the shelves of a forgotten library"
           "Inside a small gem owned by a clueless noble"
           "In a cup used by a cult to drug their acolytes"
           "In the memories of an amnesiac Chaos priest"
           "In the broken sword of a banished Templar"
           "Trapped in the wedding ring of an undying king"
           "In a cryptic tome, sealed by another demon"
           "In the breastplate of a dead knight left on a battlefield"
           "Inside a stone circle, ready to serve whoever can best it in a duel."
           "Haunting the castle where its last master was murdered"
           "Enslaved by a powerful oracle"
           "Inside the heathen reliquary that releases it once per month"
           "Carved on the gravestone of an emperor"
           "Inside the only toy owned by an orphan"
           "In the liver of the Inquisitor it is possessing"
           "Hidden in its castle, on a nightmarish demonic plane"
           "In the mouth of a dead dragon, guarded by superstitious locals"))

(random-table/register :name "Black Sword Hack > Travel Event"
  :data "\n  - Subject :: {Black Sword Hack > Travel Event > Subject}\n  - Theme :: {Black Sword Hack > Travel Event > Theme}")

(random-table/register :name "Black Sword Hack > Travel Event > Theme"
  :private t
  :data
  '("Aggression" "Exchange" "Discovery" "Revelation" "Pursuit"
     "Lost" "Isolation" "Death" "Escape" "Change"))

(random-table/register :name "Black Sword Hack > Travel Event > Subject"
  :private t
  :data
  '("Antagonist" "Animal" "Hermit" "Spirit" "Potentate"
     "Demon" "Explorer" "Merchant" "Caves" "Messenger"
     "Ruins" "Cult" "Community" "Ghost" "Outlaws"
     "Artists" "Soldiers" "Sorcerer" "Vagrant" "Natural disaster"))

(random-table/register :name "Black Sword Hack > Oracle Event"
  :data '("\n  - Theme :: {Black Sword Hack > Oracle Event > Theme}\n  - Subject :: {Black Sword Hack > Oracle Event > Subject}"))

(random-table/register :name "Black Sword Hack > Oracle Event > Theme"
  :private t
  :data
  '("Death" "Treachery" "Infiltration" "Desperation" "Instability" "Suspicion"
     "Escape" "Fear" "Hunt" "Division" "Falsehood" "Celebration"
     "Conquest" "Friendship" "Love" "Sacrifice" "Decay" "Exile"
     "Revenge" "Greed" "Isolation" "Preservation" "Loss" "Rebirth"
     "Oppression" "Destruction" "Ignorance" "Purification" "Scarcity" "Quest"
     "Stagnation" "Redemption" "Failure" "Help" "Corruption" "Rebellion"))

(random-table/register :name "Black Sword Hack > Oracle Event > Subject"
  :private t
  :data
  '("Army" "Church" "Ghost" "Nobility" "Otherworldly" "Plague"
     "Omen" "Ally" "Family" "Wizard" "Guild" "Architect"
     "Crusaders" "Vagrant" "Rival" "Artefact" "Messenger" "Inquisitors"
     "Ruins" "Knowledge" "Cave" "Dream" "Hamlet" "Outlaws"
     "Healers" "Cult" "Guardian" "Settlers" "Monument" "Food"
     "Judges" "Storm" "Demon" "Court" "Theatre" "Assassins"))

(random-table/register :name "Black Sword Hack > Attributes"
  :data '("\n- Strength :: {Black Sword Hack > Attribute Score}\n- Dexterity :: {Black Sword Hack > Attribute Score}\n- Constitution :: {Black Sword Hack > Attribute Score}\n- Intelligence :: {Black Sword Hack > Attribute Score}\n- Wisdom :: {Black Sword Hack > Attribute Score}\n- Charisma :: {Black Sword Hack > Attribute Score}"))

(random-table/register :name "Black Sword Hack > Attribute Score"
  :private t
  :roller "2d6"
  :data '(((2 3) . "8")
           ((4 5) . "9")
           ((6 7) . "10")
           ((8 9) . "11")
           ((10 11) . "12")
           ((12) . "13")))

(defconst jf/gaming/black-sword-hack/table/oracle-question-likelihood
  '(("Don't think so" . (lambda () (cl-sort (list (+ 1 (random 6)) (+ 1 (random 6)) (+ 1 (random 6))) #'<)))
     ("Unlikely" . (lambda () (cl-sort (list (+ 1 (random 6)) (+ 1 (random 6))) #'<)))
     ("Who knows?" . (lambda () (list (+ 1 (random 6)))))
     ("Probably" . (lambda () (cl-sort (list (+ 1 (random 6)) (+ 1 (random 6))) #'>)))
     ("Definitely". (lambda () (cl-sort (list (+ 1 (random 6)) (+ 1 (random 6)) (+ 1 (random 6))) #'>))))
  "The table of options and encoded dice rolls for question likelihoods.

Note, that the first dice in the returned list is either the
greatest or least; the dice are correctly sorted for picking the
first most dice for results.  Othertimes, we inspect the dice
rolls.

From page 98 of /The Black Sword Hack: Ultimate Chaos Edition/.")

(defun random-table/roller/oracle-question (&optional table)
  "Prompt for likelihood and return corresponding roller for TABLE."
  (let ((likelihood (completing-read "Likelihood: " jf/gaming/black-sword-hack/table/oracle-question-likelihood nil t)))
    (funcall (alist-get likelihood jf/gaming/black-sword-hack/table/oracle-question-likelihood nil nil #'string=))))

(random-table/register :name "Black Sword Hack > Oracle Question"
  :roller #'random-table/roller/oracle-question
  :fetcher (lambda (data &rest args) (car data))
  :data '("{Black Sword Hack > Oracle Question > Answer}{Black Sword Hack > Oracle Question > Unexpected Event}")
  :store t)

(random-table/register :name "Black Sword Hack > Oracle Question > Answer"
  :reuse "Black Sword Hack > Oracle Question"
  :private t
  :filter (lambda (&rest dice) "We have a pool of dice to pick one." (car (-list dice)))
  :data '("No and…" "No" "No but…" "Yes but…" "Yes" "Yes and…"))

(random-table/register :name "Black Sword Hack > Oracle Question > Unexpected Event"
  :reuse "Black Sword Hack > Oracle Question"
  :private t
  :filter (lambda (&rest dice) "We have a pool of dice to determine if there are dupes."
            (car (list-utils-dupes (-list dice))))
  :fetcher (lambda (data &rest rest)
             (when-let ((index (car rest)))
               (concat " with unexpected “"
                 (nth (- (if (listp index) (car index) index) 1) data)
                 "” event")))
  :data '("Very negative" "Negative" "Negative but…" "Positive but…" "Positive" "Very Positive"))

;;; OSE
(random-table/register :name "OSE > Attributes > 3d6 Down the Line"
  :data '("\n- Strength :: {3d6}\n- Intelligence :: {3d6}\n- Wisdom :: {3d6}\n- Dexterity :: {3d6}\n- Constitution :: {3d6}\n- Charisma :: {3d6}"))

(random-table/register :name "OSE > Secondary Skill"
  :roller "1d100"
  :data '(((1 . 3) . "Animal trainer")
           ((4 . 5) . "Armourer")
           ((6 . 9) . "Baker")
           ((10 . 12) . "Blacksmith")
           (13 . "Bookbinder")
           ((14 . 16) . "Bowyer / fletcher")
           ((17 . 20) . "Brewer")
           ((21 . 23) . "Butcher")
           ((24 . 26) . "Carpenter")
           ((27 . 28) . "Chandler")
           ((29 . 33) . "Cooper")
           ((34 . 35) . "Coppersmith")
           ((36 . 46) . "Farmer")
           ((47 . 50) . "Fisher")
           ((51 . 54) . "Furrier")
           (55 . "Glassblower")
           ((56 . 59) . "Huntsman")
           ((60 . 62) . "Lapidary / jeweller")
           ((63 . 66) . "Lorimer")
           (67 .  "Mapmaker")
           ((68 . 69) . "Mason")
           ((70 . 73) . "Miner")
           ((74 . 76) . "Potter")
           ((77 . 78) . "Roper")
           ((79 . 81) . "Seafarer")
           ((82 . 84) . "Shipwright")
           ((85 . 87) . "Tailor")
           ((88 . 90) . "Tanner")
           ((91 . 93) . "Thatcher / roofer")
           ((94 . 96) . "Woodcutter")
           ((97 . 98) . "Vintner")
           ((99 . 100) .  "{OSE > Secondary Skill} and {OSE > Secondary Skill} ")))

;;; Random Names (from Stars without Number)
(random-table/register :name "Name"
  :exclude-from-prompt t
  :data '("{Arabic Name > Masculine Given Name} {Arabic Name > Surname} (Arabic)"
           "{Arabic Name > Feminine Given Name} {Arabic Name > Surname} (Arabic)"
           "{Chinese Name > Masculine Given Name} {Chinese Name > Surname} (Chinese)"
           "{Chinese Name > Feminine Given Name} {Chinese Name > Surname} (Chinese)"
           "{English Name > Masculine Given Name} {English Name > Surname} (English)"
           "{English Name > Feminine Given Name} {English Name > Surname} (English)"
           "{Greek Name > Masculine Given Name} {Greek Name > Surname} (Greek)"
           "{Greek Name > Feminine Given Name} {Greek Name > Surname} (Greek)"
           "{Indian Name > Masculine Given Name} {Indian Name > Surname} (Indian)"
           "{Indian Name > Feminine Given Name} {Indian Name > Surname} (Indian)"
           "{Japanese Name > Masculine Given Name} {Japanese Name > Surname} (Japanese)"
           "{Japanese Name > Feminine Given Name} {Japanese Name > Surname} (Japanese)"
           "{Latin Name > Masculine Given Name} {Latin Name > Surname} (Latin)"
           "{Latin Name > Feminine Given Name} {Latin Name > Surname} (Latin)"
           "{Nigerian Name > Masculine Given Name} {Nigerian Name > Surname} (Nigerian)"
           "{Nigerian Name > Feminine Given Name} {Nigerian Name > Surname} (Nigerian)"
           "{Russian Name > Masculine Given Name} {Russian Name > Surname} (Russian)"
           "{Russian Name > Feminine Given Name} {Russian Name > Surname} (Russian)"
           "{Spanish Name > Masculine Given Name} {Spanish Name > Surname} (Spanish)"
           "{Spanish Name > Feminine Given Name} {Spanish Name > Surname} (Spanish)"
           "{Norse Given Name} {Norse Surname} (Norse)"
           "{Norse Given Name} {Norse Surname} (Norse)"
           "{Breggle Name > Masculine Given Name} {Breggle Name > Surname} (Breggle)"
           "{Breggle Name > Feminine Given Name} {Breggle Name > Surname} (Breggle)"))

(random-table/register :name "Arabic Name > Masculine Given Name"
  :private t
  :data '("Aamir" "Ayub" "Binyamin" "Efraim" "Ibrahim" "Ilyas" "Ismail" "Jibril" "Jumanah" "Kazi" "Lut" "Matta" "Mohammed" "Mubarak" "Mustafa" "Nazir" "Rahim" "Reza" "Sharif" "Taimur" "Usman" "Yakub" "Yusuf" "Zakariya" "Zubair"))
(random-table/register :name "Arabic Name > Feminine Given Name"
  :private t
  :data '("Aisha" "Alimah" "Badia" "Bisharah" "Chanda" "Daliya" "Fatimah" "Ghania" "Halah" "Kaylah" "Khayrah" "Layla" "Mina" "Munisa" "Mysha" "Naimah" "Nissa" "Nura" "Parveen" "Rana" "Shalha" "Suhira" "Tahirah" "Yasmin" "Zulehka"))
(random-table/register :name "Arabic Name > Surname"
  :private t
  :data '("Abdel" "Awad" "Dahhak" "Essa" "Hanna" "Harbi" "Hassan" "Isa" "Kasim" "Katib" "Khalil" "Malik" "Mansoor" "Mazin" "Musa" "Najeeb" "Namari" "Naser" "Rahman" "Rasheed" "Saleh" "Salim" "Shadi" "Sulaiman" "Tabari"))
(random-table/register :name "Arabic Location Name"
  :private t
  :data '("Adan" "Magrit" "Ahsa" "Masqat" "Andalus" "Misr" "Asmara" "Muruni" "Asqlan" "Qabis" "Baqubah" "Qina" "Basit" "Rabat" "Baysan" "Ramlah" "Baytlahm" "Riyadh" "Bursaid" "Sabtah" "Dahilah" "Salalah" "Darasalam" "Sana" "Dawhah" "Sinqit" "Ganin" "Suqutrah" "Gebal" "Sur" "Gibuti" "Tabuk" "Giddah" "Tangah" "Harmah" "Tarifah" "Hartum" "Tarrakunah" "Hibah" "Tisit" "Hims" "Uman" "Hubar" "Urdunn" "Karbala" "Wasqah" "Kut" "Yaburah" "Lacant" "Yaman"))

(random-table/register :name "Chinese Name > Masculine Given Name"
  :private t
  :data '("Aiguo" "Bohai" "Chao" "Dai" "Dawei" "Duyi" "Fa" "Fu" "Gui" "Hong" "Jianyu" "Kang" "Li" "Niu" "Peng" "Quan" "Ru" "Shen" "Shi" "Song" "Tao" "Xue" "Yi" "Yuan" "Zian"))
(random-table/register :name "Chinese Name > Feminine Given Name"
  :private t
  :data '("Biyu" "Changying" "Daiyu" "Huidai" "Huiliang" "Jia" "Jingfei" "Lan" "Liling" "Liu" "Meili" "Niu" "Peizhi" "Qiao" "Qing" "Ruolan" "Shu" "Suyin" "Ting" "Xia" "Xiaowen" "Xiulan" "Ya" "Ying" "Zhilan"))
(random-table/register :name "Chinese Name > Surname"
  :private t
  :data '("Bai" "Cao" "Chen" "Cui" "Ding" "Du" "Fang" "Fu" "Guo" "Han" "Hao" "Huang" "Lei" "Li" "Liang" "Liu" "Long" "Song" "Tan" "Tang" "Wang" "Wu" "Xing" "Yang" "Zhang"))
(random-table/register :name "Chinese Location Name"
  :private t
  :data '("Andong" "Luzhou" "Anqing" "Ningxia" "Anshan" "Pingxiang" "Chaoyang" "Pizhou" "Chaozhou" "Qidong" "Chifeng" "Qingdao" "Dalian" "Qinghai" "Dunhuang" "Rehe" "Fengjia" "Shanxi" "Fengtian" "Taiyuan" "Fuliang" "Tengzhou" "Fushun" "Urumqi" "Gansu" "Weifang" "Ganzhou" "Wugang" "Guizhou" "Wuxi" "Hotan" "Xiamen" "Hunan" "Xian" "Jinan" "Xikang" "Jingdezhen" "Xining" "Jinxi" "Xinjiang" "Jinzhou" "Yidu" "Kunming" "Yingkou" "Liaoning" "Yuxi" "Linyi" "Zigong" "Lushun" "Zoige"))

(random-table/register :name "English Name > Masculine Given Name"
  :private t
  :data '("Adam" "Albert" "Alfred" "Allan" "Archibald" "Arthur" "Basil" "Charles" "Colin" "Donald" "Douglas" "Edgar" "Edmund" "Edward" "George" "Harold" "Henry" "Ian" "James" "John" "Lewis" "Oliver" "Philip" "Richard" "William"))
(random-table/register :name "English Name > Feminine Given Name"
  :private t
  :data '("Abigail" "Anne" "Beatrice" "Blanche" "Catherine" "Charlotte" "Claire" "Eleanor" "Elizabeth" "Emily" "Emma" "Georgia" "Harriet" "Joan" "Judy" "Julia" "Lucy" "Lydia" "Margaret" "Mary" "Molly" "Nora" "Rosie" "Sarah" "Victoria"))
(random-table/register :name "English Name > Surname"
  :private t
  :data '("Barker" "Brown" "Butler" "Carter" "Chapman" "Collins" "Cook" "Davies" "Gray" "Green" "Harris" "Jackson" "Jones" "Lloyd" "Miller" "Roberts" "Smith" "Taylor" "Thomas" "Turner" "Watson" "White" "Williams" "Wood" "Young"))
(random-table/register :name "English Location Name"
  :private t
  :data '("Aldington" "Kedington" "Appleton" "Latchford" "Ashdon" "Leigh" "Berwick" "Leighton" "Bramford" "Maresfield" "Brimstage" "Markshall" "Carden" "Netherpool" "Churchill" "Newton" "Clifton" "Oxton" "Colby" "Preston" "Copford" "Ridley" "Cromer" "Rochford" "Davenham" "Seaford" "Dersingham" "Selsey" "Doverdale" "Stanton" "Elsted" "Stockham" "Ferring" "Stoke" "Gissing" "Sutton" "Heydon" "Thakeham" "Holt" "Thetford" "Hunston" "Thorndon" "Hutton" "Ulting" "Inkberrow" "Upton" "Inworth" "Westhorpe" "Isfield" "Worcester"))

(random-table/register :name "Greek Name > Masculine Given Name"
  :private t
  :data '("Alexander" "Alexius" "Anastasius" "Christodoulos" "Christos" "Damian" "Dimitris" "Dysmas" "Elias" "Giorgos" "Ioannis" "Konstantinos" "Lambros" "Leonidas" "Marcos" "Miltiades" "Nestor" "Nikos" "Orestes" "Petros" "Simon" "Stavros" "Theodore" "Vassilios" "Yannis"))
(random-table/register :name "Greek Name > Feminine Given Name"
  :private t
  :data '("Alexandra" "Amalia" "Callisto" "Charis" "Chloe" "Dorothea" "Elena" "Eudoxia" "Giada" "Helena" "Ioanna" "Lydia" "Melania" "Melissa" "Nika" "Nikolina" "Olympias" "Philippa" "Phoebe" "Sophia" "Theodora" "Valentina" "Valeria" "Yianna" "Zoe"))
(random-table/register :name "Greek Name > Surname"
  :private t
  :data '("Andreas" "Argyros" "Dimitriou" "Floros" "Gavras" "Ioannidis" "Katsaros" "Kyrkos" "Leventis" "Makris" "Metaxas" "Nikolaidis" "Pallis" "Pappas" "Petrou" "Raptis" "Simonides" "Spiros" "Stavros" "Stephanidis" "Stratigos" "Terzis" "Theodorou" "Vasiliadis" "Yannakakis"))
(random-table/register :name "Greek Location Name"
  :private t
  :data '("Adramyttion" "Kallisto" "Ainos" "Katerini" "Alikarnassos" "Kithairon" "Avydos" "Kydonia" "Dakia" "Lakonia" "Dardanos" "Leros" "Dekapoli" "Lesvos" "Dodoni" "Limnos" "Efesos" "Lykia" "Efstratios" "Megara" "Elefsina" "Messene" "Ellada" "Milos" "Epidavros" "Nikaia" "Erymanthos" "Orontis" "Evripos" "Parnasos" "Gavdos" "Petro" "Gytheio" "Samos" "Ikaria" "Syros" "Ilios" "Thapsos" "Illyria" "Thessalia" "Iraia" "Thira" "Irakleio" "Thiva" "Isminos" "Varvara" "Ithaki" "Voiotia" "Kadmeia" "Vyvlos"))

;; Indian Name :: Masculine Given Name
(random-table/register :name "Indian Name > Masculine Given Name"
  :private t
  :data '("Amrit" "Ashok" "Chand" "Dinesh" "Gobind" "Harinder" "Jagdish" "Johar" "Kurien" "Lakshman" "Madhav" "Mahinder" "Mohal" "Narinder" "Nikhil" "Omrao" "Prasad" "Pratap" "Ranjit" "Sanjay" "Shankar" "Thakur" "Vijay" "Vipul" "Yash"))
(random-table/register :name "Indian Name > Feminine Given Name"
  :private t
  :data '("Amala" "Asha" "Chandra" "Devika" "Esha" "Gita" "Indira" "Indrani" "Jaya" "Jayanti" "Kiri" "Lalita" "Malati" "Mira" "Mohana" "Neela" "Nita" "Rajani" "Sarala" "Sarika" "Sheela" "Sunita" "Trishna" "Usha" "Vasanta"))
(random-table/register :name "Indian Name > Surname"
  :private t
  :data '("Achari" "Banerjee" "Bhatnagar" "Bose" "Chauhan" "Chopra" "Das" "Dutta" "Gupta" "Johar" "Kapoor" "Mahajan" "Malhotra" "Mehra" "Nehru" "Patil" "Rao" "Saxena" "Shah" "Sharma" "Singh" "Trivedi" "Venkatesan" "Verma" "Yadav"))
(random-table/register :name "Indian Location Name"
  :private t
  :data '("Ahmedabad" "Jaisalmer" "Alipurduar" "Jharonda" "Alubari" "Kadambur" "Anjanadri" "Kalasipalyam" "Ankleshwar" "Karnataka" "Balarika" "Kutchuhery" "Bhanuja" "Lalgola" "Bhilwada" "Mainaguri" "Brahmaghosa" "Nainital" "Bulandshahar" "Nandidurg" "Candrama" "Narayanadri" "Chalisgaon" "Panipat" "Chandragiri" "Panjagutta" "Charbagh" "Pathankot" "Chayanka" "Pathardih" "Chittorgarh" "Porbandar" "Dayabasti" "Rajasthan" "Dikpala" "Renigunta" "Ekanga" "Sewagram" "Gandhidham" "Shakurbasti" "Gollaprolu" "Siliguri" "Grahisa" "Sonepat" "Guwahati" "Teliwara" "Haridasva" "Tinpahar" "Indraprastha" "Villivakkam"))

(random-table/register :name "Japanese Name > Masculine Given Name"
  :private t
  :data '("Akira" "Daisuke" "Fukashi" "Goro" "Hiro" "Hiroya" "Hotaka" "Katsu" "Katsuto" "Keishuu" "Kyuuto" "Mikiya" "Mitsunobu" "Mitsuru" "Naruhiko" "Nobu" "Shigeo" "Shigeto" "Shou" "Shuji" "Takaharu" "Teruaki" "Tetsushi" "Tsukasa" "Yasuharu"))
(random-table/register :name "Japanese Name > Feminine Given Name"
  :private t
  :data '("Aemi" "Airi" "Ako" "Ayu" "Chikaze" "Eriko" "Hina" "Kaori" "Keiko" "Kyouka" "Mayumi" "Miho" "Namiko" "Natsu" "Nobuko" "Rei" "Ririsa" "Sakimi" "Shihoko" "Shika" "Tsukiko" "Tsuzune" "Yoriko" "Yorimi" "Yoshiko"))
(random-table/register :name "Japanese Name > Surname"
  :private t
  :data '("Abe" "Arakaki" "Endo" "Fujiwara" "Goto" "Ito" "Kikuchi" "Kinjo" "Kobayashi" "Koga" "Komatsu" "Maeda" "Nakamura" "Narita" "Ochi" "Oshiro" "Saito" "Sakamoto" "Sato" "Suzuki" "Takahashi" "Tanaka" "Watanabe" "Yamamoto" "Yamasaki"))
(random-table/register :name "Japanese Location Name"
  :private t
  :data '("Bando" "Mitsukaido" "Chikuma" "Moriya" "Chikusei" "Nagano" "Chino" "Naka" "Hitachi" "Nakano" "Hitachinaka" "Ogi" "Hitachiomiya" "Okaya" "Hitachiota" "Omachi" "Iida" "Ryugasaki" "Iiyama" "Saku" "Ina" "Settsu" "Inashiki" "Shimotsuma" "Ishioka" "Shiojiri" "Itako" "Suwa" "Kamisu" "Suzaka" "Kasama" "Takahagi" "Kashima" "Takeo" "Kasumigaura" "Tomi" "Kitaibaraki" "Toride" "Kiyose" "Tsuchiura" "Koga" "Tsukuba" "Komagane" "Ueda" "Komoro" "Ushiku" "Matsumoto" "Yoshikawa" "Mito" "Yuki"))

(random-table/register :name "Latin Name > Masculine Given Name"
  :private t
  :data '("Agrippa" "Appius" "Aulus" "Caeso" "Decimus" "Faustus" "Gaius" "Gnaeus" "Hostus" "Lucius" "Mamercus" "Manius" "Marcus" "Mettius" "Nonus" "Numerius" "Opiter" "Paulus" "Proculus" "Publius" "Quintus" "Servius" "Tiberius" "Titus" "Volescus"))
(random-table/register :name "Latin Name > Feminine Given Name"
  :private t
  :data '("Appia" "Aula" "Caesula" "Decima" "Fausta" "Gaia" "Gnaea" "Hosta" "Lucia" "Maio" "Marcia" "Maxima" "Mettia" "Nona" "Numeria" "Octavia" "Postuma" "Prima" "Procula" "Septima" "Servia" "Tertia" "Tiberia" "Titia" "Vibia"))
(random-table/register :name "Latin Name > Surname"
  :private t
  :data '("Antius" "Aurius" "Barbatius" "Calidius" "Cornelius" "Decius" "Fabius" "Flavius" "Galerius" "Horatius" "Julius" "Juventius" "Licinius" "Marius" "Minicius" "Nerius" "Octavius" "Pompeius" "Quinctius" "Rutilius" "Sextius" "Titius" "Ulpius" "Valerius" "Vitellius"))
(random-table/register :name "Latin Location Name"
  :private t
  :data '("Abilia" "Lucus" "Alsium" "Lugdunum" "Aquileia" "Mediolanum" "Argentoratum" "Novaesium" "Ascrivium" "Patavium" "Asculum" "Pistoria" "Attalia" "Pompeii" "Barium" "Raurica" "Batavorum" "Rigomagus" "Belum" "Roma" "Bobbium" "Salernum" "Brigantium" "Salona" "Burgodunum" "Segovia" "Camulodunum" "Sirmium" "Clausentum" "Spalatum" "Corduba" "Tarraco" "Coriovallum" "Treverorum" "Durucobrivis" "Verulamium" "Eboracum" "Vesontio" "Emona" "Vetera" "Florentia" "Vindelicorum" "Lactodurum" "Vindobona" "Lentia" "Vinovia" "Lindum" "Viroconium" "Londinium" "Volubilis"))

(random-table/register :name "Nigerian Name > Masculine Given Name"
  :private t
  :data '("Adesegun" "Akintola" "Amabere" "Arikawe" "Asagwara" "Chidubem" "Chinedu" "Chiwetei" "Damilola" "Esangbedo" "Ezenwoye" "Folarin" "Genechi" "Idowu" "Kelechi" "Ketanndu" "Melubari" "Nkanta" "Obafemi" "Olatunde" "Olumide" "Tombari" "Udofia" "Uyoata" "Uzochi"))
(random-table/register :name "Nigerian Name > Feminine Given Name"
  :private t
  :data '("Abike" "Adesuwa" "Adunola" "Anguli" "Arewa" "Asari" "Bisola" "Chioma" "Eduwa" "Emilohi" "Fehintola" "Folasade" "Mahparah" "Minika" "Nkolika" "Nkoyo" "Nuanae" "Obioma" "Olafemi" "Shanumi" "Sominabo" "Suliat" "Tariere" "Temedire" "Yemisi"))
(random-table/register :name "Nigerian Name > Surname"
  :private t
  :data '("Adegboye" "Adeniyi" "Adeyeku" "Adunola" "Agbaje" "Akpan" "Akpehi" "Aliki" "Asuni" "Babangida" "Ekim" "Ezeiruaku" "Fabiola" "Fasola" "Nwokolo" "Nzeocha" "Ojo" "Okonkwo" "Okoye" "Olaniyan" "Olawale" "Olumese" "Onajobi" "Soyinka" "Yamusa"))
(random-table/register :name "Nigerian Location Name"
  :private t
  :data '("Abadan" "Jere" "Ador" "Kalabalge" "Agatu" "Katsina" "Akamkpa" "Knoduga" "Akpabuyo" "Konshishatse" "Ala" "Kukawa" "Askira" "Kwande" "Bakassi" "Kwayakusar" "Bama" "Logo" "Bayo" "Mafa" "Bekwara" "Makurdi" "Biase" "Nganzai" "Boki" "Obanliku" "Buruku" "Obi" "Calabar" "Obubra" "Chibok" "Obudu" "Damboa" "Odukpani" "Dikwa" "Ogbadibo" "Etung" "Ohimini" "Gboko" "Okpokwu" "Gubio" "Otukpo" "Guzamala" "Shani" "Gwoza" "Ugep" "Hawul" "Vandeikya" "Ikom" "Yala"))

(random-table/register :name "Russian Name > Masculine Given Name"
  :private t
  :data '("Aleksandr" "Andrei" "Arkady" "Boris" "Dmitri" "Dominik" "Grigory" "Igor" "Ilya" "Ivan" "Kiril" "Konstantin" "Leonid" "Nikolai" "Oleg" "Pavel" "Petr" "Sergei" "Stepan" "Valentin" "Vasily" "Viktor" "Yakov" "Yegor" "Yuri"))
(random-table/register :name "Russian Name > Feminine Given Name"
  :private t
  :data '("Aleksandra" "Anastasia" "Anja" "Catarina" "Devora" "Dima" "Ekaterina" "Eva" "Irina" "Karolina" "Katlina" "Kira" "Ludmilla" "Mara" "Nadezdha" "Nastassia" "Natalya" "Oksana" "Olena" "Olga" "Sofia" "Svetlana" "Tatyana" "Vilma" "Yelena"))
(random-table/register :name "Russian Name > Surname"
  :private t
  :data '("Abelev" "Bobrikov" "Chemerkin" "Gogunov" "Gurov" "Iltchenko" "Kavelin" "Komarov" "Korovin" "Kurnikov" "Lebedev" "Litvak" "Mekhdiev" "Muraviov" "Nikitin" "Ortov" "Peshkov" "Romasko" "Shvedov" "Sikorski" "Stolypin" "Turov" "Volokh" "Zaitsev" "Zhukov"))
(random-table/register :name "Russian Location Name"
  :private t
  :data '("Amur" "Omsk" "Arkhangelsk" "Orenburg" "Astrakhan" "Oryol" "Belgorod" "Penza" "Bryansk" "Perm" "Chelyabinsk" "Pskov" "Chita" "Rostov" "Gorki" "Ryazan" "Irkutsk" "Sakhalin" "Ivanovo" "Samara" "Kaliningrad" "Saratov" "Kaluga" "Smolensk" "Kamchatka" "Sverdlovsk" "Kemerovo" "Tambov" "Kirov" "Tomsk" "Kostroma" "Tula" "Kurgan" "Tver" "Kursk" "Tyumen" "Leningrad" "Ulyanovsk" "Lipetsk" "Vladimir" "Magadan" "Volgograd" "Moscow" "Vologda" "Murmansk" "Voronezh" "Novgorod" "Vyborg" "Novosibirsk" "Yaroslavl"))

(random-table/register :name "Spanish Name > Masculine Given Name"
  :private t
  :data '("Alejandro" "Alonso" "Amelio" "Armando" "Bernardo" "Carlos" "Cesar" "Diego" "Emilio" "Estevan" "Felipe" "Francisco" "Guillermo" "Javier" "Jose" "Juan" "Julio" "Luis" "Pedro" "Raul" "Ricardo" "Salvador" "Santiago" "Valeriano" "Vicente"))
(random-table/register :name "Spanish Name > Feminine Given Name"
  :private t
  :data '("Adalina" "Aleta" "Ana" "Ascencion" "Beatriz" "Carmela" "Celia" "Dolores" "Elena" "Emelina" "Felipa" "Inez" "Isabel" "Jacinta" "Lucia" "Lupe" "Maria" "Marta" "Nina" "Paloma" "Rafaela" "Soledad" "Teresa" "Valencia" "Zenaida"))
(random-table/register :name "Spanish Name > Surname"
  :private t
  :data '("Arellano" "Arispana" "Borrego" "Carderas" "Carranzo" "Cordova" "Enciso" "Espejo" "Gavilan" "Guerra" "Guillen" "Huertas" "Illan" "Jurado" "Moretta" "Motolinia" "Pancorbo" "Paredes" "Quesada" "Roma" "Rubiera" "Santoro" "Torrillas" "Vera" "Vivero"))
(random-table/register :name "Spanish Location Name"
  :private t
  :data '("Aguascebas" "Loreto" "Alcazar" "Lujar" "Barranquete" "Marbela" "Bravatas" "Matagorda" "Cabezudos" "Nacimiento" "Calderon" "Niguelas" "Cantera" "Ogijares" "Castillo" "Ortegicar" "Delgadas" "Pampanico" "Donablanca" "Pelado" "Encinetas" "Quesada" "Estrella" "Quintera" "Faustino" "Riguelo" "Fuentebravia" "Ruescas" "Gafarillos" "Salteras" "Gironda" "Santopitar" "Higueros" "Taberno" "Huelago" "Torres" "Humilladero" "Umbrete" "Illora" "Valdecazorla" "Isabela" "Velez" "Izbor" "Vistahermosa" "Jandilla" "Yeguas" "Jinetes" "Zahora" "Limones" "Zumeta"))

(random-table/register :name "Norse Given Name"
  :private t
  :data '("Åge" "Alvar" "Anna" "Anri" "Ari" "Arne" "Arnkatla" "Åse" "Astrid" "Auður" "Birger" "Bjørn" "Bo" "Bodil" "Darri" "Edda" "Einar" "Emil" "Erik" "Estrik" "Flóki" "Flosi" "Freyja" "Frida" "Frode" "Funi" "Gertrud" "Gorm" "Gro" "Guðmundr" "Guðrún" "Gunhild" "Gunnar" "Halfdan" "Halbera" "Harald" "Harpa" "Hekla" "Helgi" "Hilda" "Hilmir" "Hrefna" "Inga" "Ingibjörg" "Ingólfur" "Jóhanna" "Kaðlín" "Kåre" "Karitas" "Katla" "Ketill" "Knud" "Kristín" "Kristján" "Leif" "Lind" "Liv" "Logi" "Lukka" "Magnús" "Margrét" "María" "Njal" "Nói" "Ødger" "Ólafur" "Orri" "Randi" "Revna" "Roar" "Rune" "Saga" "Salvar" "Sif" "Sigmar" "Signe" "Sigred" "Sigriður" "Sigrún" "Skarde" "Stefán" "Sten" "Sune" "Sunna" "Svend" "Thurid" "Þuríður" "Thyra" "Toke" "Tora" "Torsten" "Tove" "Troels" "Trygve" "Ulfhild" "Valgerður" "Vilmar" "Von" "Yrsa"))

(random-table/register :name "Norse Surname"
  :private t
  :data '("{Norse Given Name}son" "{Norse Given Name}dóttir"))

(random-table/register :name "Person"
  :data '("\n- Name :: {Name}\n- Physique :: {Person > Physique}\n- Skin :: {Person > Skin}\n- Hair :: {Person > Hair}\n- Face :: {Person > Face}\n- Speech :: {Person > Speech}\n- Virtue :: {Person > Virtue}\n- Vice :: {Person > Vice}"))

(random-table/register :name "Person > Physique"
  :private t
  :data '("Athletic" "Brawny" "Diminutive" "Lanky" "Rugged" "Slim" "Short" "Statuesque" "Stout" "Towering"))

(random-table/register :name "Person > Skin"
  :private t
  :data '("Birthmark" "Drawn" "Elogated" "Pockmarked" "Rosy" "Rough" "Smooth" "Freckled" "Scarred" "Weathered"))

(random-table/register :name "Person > Hair"
  :private t
  :data '("Shaved" "Braided" "Curly" "Matted" "Frizzy" "Flowing" "Luxurious" "Oily" "Wavy" "Wispy"))

(random-table/register :name "Person > Face"
  :private t
  :data '("Bony" "Damaged" "Chiselled" "Elongated" "Pale" "Symmetrical" "Fierce" "Sharp" "Square" "Sunken"))

(random-table/register :name "Person > Speech"
  :private t
  :data '("Blunt" "Booming" "Cryptic" "Droning" "Formal" "Gravelly" "Precise" "Squeaky" "Eloquent" "Whispery"))

(random-table/register :name "Person > Virtue"
  :private t
  :data '("Ambitious" "Cautious" "Courageous" "Discipline" "Gergarious" "Honourable" "Humble" "Merciful" "Serene" "Tolerant"))

(random-table/register :name "Person > Vice"
  :private t
  :data '("Aggressive" "Bitter" "Craven" "Deceitful" "Greedy" "Lazy" "Nervous" "Rude" "Vain" "Vengeful"))

(random-table/register :name "Item, Fantasy"
  :private t
  :data '("Weapon" "Potion" "Scroll" "Armor" "Book"
           "Map" "Jewel" "Device" "Note" "Container"
           "Letter" "Amulet" "Ring" "Promissory" "Glasses"
           "Key" "Ingredient" "Poison" "Drug" "Pet"))

;;; Stars without Number
(random-table/register :name "Corporation, Sci-Fi"
  :data '("\n- Name :: {Corporation, Sci-Fi > Prefix} {Corporation, Sci-Fi > Suffix}\n- Business :: {Corporation, Sci-Fi > Business}\n- Rumor :: {Corporation, Sci-Fi > Rumor}"))

(random-table/register :name "Corporation, Sci-Fi > Prefix"
  :private t
  :data '("Ad Astra" "Colonial" "Compass" "Daybreak" "Frontier"
           "Guo Yin" "Highbeam" "Imani" "Magnus" "Meteor"
           "Neogen" "New Dawn" "Omnitech" "Outertech" "Overwatch"
           "Panstellar" "Shogun" "Silverlight" "Spiker" "Stella"
           "Striker" "Sunbeam" "Terra Prime" "Wayfarer" "West Wind"))

(random-table/register :name "Corporation, Sci-Fi > Suffix"
  :private t
  :data '("Alliance" "Association" "Band" "Circle" "Clan"
           "Combine" "Company" "Cooperative" "Corporation" "Enterprises"
           "Faction" "Group" "Megacorp" "Multistellar" "Organization"
           "Outfit" "Pact" "Partnership" "Ring" "Society"
           "Sodality" "Syndicate" "Union" "Unity" "Zaibatsu"))

(random-table/register :name "Corporation, Sci-Fi > Business"
  :private t
  :data '("Aeronautics" "Agriculture" "Art" "Assassination" "Asteroid Mining"
           "Astrotech" "Biotech" "Bootlegging" "Computer Hardware" "Construction"
           "Cybernetics" "Electronics" "Energy Weapons" "Entertainment" "Espionage"
           "Exploration" "Fishing" "Fuel Refining" "Gambling" "Gemstones"
           "Gengineering" "Grav Vehicles" "Heavy Weapons" "Ideology" "Illicit Drugs"
           "Journalism" "Law Enforcement" "Liquor" "Livestock" "Maltech"
           "Mercenary Work" "Metallurgy" "Pharmaceuticals" "Piracy" "Planetary Mining"
           "Plastics" "Pretech" "Prisons" "Programming" "Projectile Guns"
           "Prostitution" "Psionics" "Psitech" "Robotics" "Security"
           "Shipyards" "Snacks" "Telcoms" "Transport" "Xenotech"))

(random-table/register :name "Corporation, Sci-Fi > Rumor"
  :private t
  :data
  '("Reckless with the lives of their employees"
     "Have a dark secret about their board of directors"
     "Notoriously xenophobic towards aliens"
     "Lost much money to an embezzler who evaded arrest"
     "Reliable and trustworthy goods"
     "Stole a lot of R&D from a rival corporation"
     "They have high-level political connections"
     "Rumored cover-up of a massive industrial accident"
     "Stodgy and very conservative in their business plans"
     "Stodgy and very conservative in their business plans"
     "The company’s owner is dangerously insane"
     "Rumored ties to a eugenics cult"
     "Said to have a cache of pretech equipment"
     "Possibly teetering on the edge of bankruptcy"
     "Front for a planetary government’s espionage arm"
     "Secretly run by a psychic cabal"
     "Secretly run by hostile aliens"
     "Secretly run by an unbraked AI"
     "They’ve turned over a new leaf with the new CEO"
     "Deeply entangled with the planetary underworl"))

(random-table/register :name "Heresy"
  :data '("\n- Founder :: {Heresy > Founder}\n- Major Heresy :: {Heresy > Major Heresy}\n- Attitude :: {Heresy > Attitude}\n- Quirk :: {Heresy > Quirk}"))

(random-table/register :name "Heresy > Founder"
  :private t
  :data '("Defrocked clergy: founded by a cleric outcast from the faith."
           "Frustrated layman: founded by a layman frustrated with the faith’s decadence rigidity or lack of authenticity."
           "Renegade prophet: founded by a revered holy figure who broke with the faith."
           "High prelate: founded by an important and powerful cleric to convey his or her beliefs."
           "Dissatisfied minor clergy: founded by a minor cleric frustrated with the faith’s current condition."
           "Outsider: founded by a member of another faith deeply influenced by the parent religion."
           "Academic: founded by a professor or theologian on intellectual grounds."
           "Accidental: the founder never meant their works to be taken that way."))

(random-table/register :name "Heresy > Major Heresy"
  :private t
  :data '("Manichaeanism: the sect believes in harsh austerities and rejection of matter as something profane and evil."
           "Donatism: the sect believes that clergy must be personally pure and holy in order to be legitimate."
           "Supercessionism: the sect believes the founder or some other source supercedes former scripture or tradition."
           "Antinomianism: the sect believes that their holy persons are above any earthly law and may do as they will."
           "Universal priesthood: the sect believes that there is no distinction between clergy and layman and that all functions of the faith may be performed by all members."
           "Conciliarism: the sect believes that the consensus of believers may correct or command even the clerical leadership of the faith."
           "Ethnocentrism: the sect believes that only a particular ethnicity or nationality can truly belong to the faith."
           "Separatism: the sect believes members should shun involvement with the secular world."
           "Stringency: the sect believes that even minor sins should be punished and major sins should be capital crimes."
           "Syncretism: the sect has added elements of another native faith to their beliefs."
           "Primitivism: the sect tries to recreate what they imagine was the original community of faith."
           "Conversion by the sword: unbelievers must be brought to obedience to the sect or be granted death."))

(random-table/register :name "Heresy > Attitude"
  :private t
  :data '("Filial: the sect honors and respects the orthodox faith, but feels it is substantially in error."
           "Anathematic: the orthodox are spiritually worse than infidels, and their ways must be avoided at all costs."
           "Evangelical: the sect feels compelled to teach the orthodox the better truth of their ways."
           "Contemptuous: the orthodox are spiritually lost and ignoble."
           "Aversion: the sect wishes to shun and avoid the orthodox."
           "Hatred: the sect wishes the death or conversion of the orthodox."
           "Indifference: the sect has no particular animus or love for the orthodox."
           "Obedience: the sect feels obligated to obey the orthodox hierarchy in all matters not related to their specific faith."
           "Legitimist: the sect views itself as the “true” orthodox faith and the present orthodox hierarchy as pretenders to their office."
           "Purificationist: the sect’s austerities, sufferings, and asceticisms are necessary to purify the orthodox."))

(random-table/register :name "Heresy > Quirk"
  :private t
  :data '("Clergy of only one gender"
           "Dietary prohibitions"
           "Characteristic item of clothing or jewelry"
           "Public prayer at set times or places"
           "Forbidden to do something commonly done"
           "Anti-intellectual deploring secular learning"
           "Mystical seeking union with God through meditation"
           "Lives in visibly distinct houses or districts"
           "Has a language specific to the sect"
           "Greatly respects learning and education"
           "Favors specific colors or symbols"
           "Has unique purification rituals"
           "Always armed"
           "Forbids marriage or romance outside the sect"
           "Will not eat with people outside the sect"
           "Must donate labor or tithe money to the sect"
           "Special friendliness toward another faith or ethnicity"
           "Favors certain professions for their membership"
           "Vigorous charity work among unbelievers"
           "Forbidden the use of certain technology"))

;;; Herbalist's Primer
(random-table/register :name "Herbalist's Primer > Plant"
  :data '("{Herbalist's Primer > Plant > Name Prefix}{Herbalist's Primer > Plant > Name Suffix} is {Herbalist's Primer > Plant > Rarity} {Herbalist's Primer > Plant > Habit}, mostly prized for its {Herbalist's Primer > Plant > Properties} value.  It is a native to the {Herbalist's Primer > Plant > Climate} {Herbalist's Primer > Plant > Biome}.  Interestingly, it {Herbalist's Primer > Plant > Quirk}.{Herbalist's Primer > Plant > Property Description}"))

(random-table/register :name "Herbalist's Primer > Plant > Name Prefix"
  :private t
  :data '("Arrow" "Blood" "Crimson" "Death" "Dragon" "Fire" "Gold" "Good" "Ice" "Life"
           "Raven" "Snake" "Spear" "Spirit" "Star" "Sword" "Truth" "Witch" "Wolf" "Worm"))

(random-table/register :name "Herbalist's Primer > Plant > Name Suffix"
  :private t
  :data '("bane" "bark" "bean" "berry" "bush" "fern" "flower" "fruit" "grass" "leaf"
           "nut" "plant" "root" "seed" "spice" "thorn" "tree" "weed" "wort" "wood"))

;; Todo consider altering the rarity
(random-table/register :name "Herbalist's Primer > Plant > Rarity"
  :private t
  :data '("a widespread" "an abundant" "a common" "a popular" "an uncommon"
           "a rare" "an endangered" "a near-extinct" "a legendary" "a mythical"))

(random-table/register :name "Herbalist's Primer > Plant > Habit"
  :private t
  :data '("herb" "shrub" "tree"))

(random-table/register :name "Herbalist's Primer > Plant > Properties"
  :private t
  :store t
  :data '("culinary" "industrial" "magical" "medicinal" "ornamental" "poisonous"))

(random-table/register :name "Herbalist's Primer > Plant > Climate"
  :private t
  :data '("artic" "arid" "boreal" "cold" "continental"
           "dry" "high-altitude" "hot" "humid" "ice-bound"
           "island" "marine" "monsoon" "oceanic" "polar"
           "subartic" "subtropical" "temperate" "tropical" "wet"))

(random-table/register :name "Herbalist's Primer > Plant > Biome"
  :private t
  :data '("caves" "deserts" "forests" "gardens" "hills"
           "lakes" "meadows" "mountains" "plains" "plantations"
           "riverbanks" "roadsides" "seas" "shores" "shrublands"
           "streams" "swamps" "urban areas" "volcanoes" "wastes"))

(random-table/register :name "Herbalist's Primer > Plant > Quirk"
  :private t
  :data '("is carnivorous" "is parasitic" "is symbiotic with another plant" "stores water in the stems" "has a strong, pleasant aroma"
           "is always warm to the touch" "is covered in sharp spikes" "is covered in a sticky sap" "smells of rotting meat" "grows in the tree crowns"
           "grows almost entirely underground" "only blooms at night" "is poisonous to other plants" "causes a strong allergic reaction" "produces a lot of pollen"
           "attracts all kinds of insects" "is a favorite snack of many animals" "often hosts bird nests" "has a lovely, sweet flavor" "grows incredibly fast"))

(random-table/register :name "Herbalist's Primer > Plant > Property Description"
  :private t
  :data
  '(nil ;; culinary
     nil ;; industrial
     "  Magical Property: the plant’s {Herbalist's Primer > Plant > Material} {Herbalist's Primer > Plant > Method} will {Herbalist's Primer > Plant > Effect > Magical}.  One complication is that {Herbalist's Primer > Plant > Complication}." ;; magical
     "  Medicinal Property: the plant’s {Herbalist's Primer > Plant > Material} {Herbalist's Primer > Plant > Method} will {Herbalist's Primer > Plant > Effect > Medicinal}.  One complication is that {Herbalist's Primer > Plant > Complication}." ;; magical
     nil ;; ornamental
     "  Poisonous Property: the plant’s {Herbalist's Primer > Plant > Material} {Herbalist's Primer > Plant > Method} will {Herbalist's Primer > Plant > Effect > Poisonous}.  One complication is that {Herbalist's Primer > Plant > Complication}." ;; magical
     ))

(random-table/register :name "Herbalist's Primer > Plant > Material"
  :private t
  :data '("balsam" "bark" "bulbs" "buds" "cones"
           "flowers" "fruits" "galls" "gum" "juice"
           "leaves" "petals" "pollen" "resin" "rhizomes"
           "root" "seeds" "stems" "timber" "tubers"))

(random-table/register :name "Herbalist's Primer > Plant > Method"
  :private t
  :data '("applied to an inanimate object" "burned as incenses" "carried" "chewed" "distilled"
           "drank as tea" "grown" "held under the tongue" "ingested" "juiced and injected"
           "mixed with alchohol" "place under a pillow" "powdered and inhaled" "rubbed on skin" "scattered on the wind"
           "sewn inside clothing" "swallowed whole" "thrown at a target" "torn or shredded" "worn"))

(random-table/register :name "Herbalist's Primer > Plant > Effect > Medicinal"
  :private t
  :data '("aid digestion" "alleviate allergy" "cure wounds" "destroy viruses" "fight the flu"
           "improve focus" "kill bacteria" "lower blood pressure" "mend bones" "neutralize poison"
           "reduce inflammation" "remove itching" "remove nausea" "remove pain" "sanitize the wound"
           "soothe the skin" "stop bleeding" "stop coughing" "strengthen the heart" "strenthen the immune system"))

(random-table/register :name "Herbalist's Primer > Plant > Effect > Poisonous"
  :private t
  :data '("cause acute pain" "cause bleeding" "cause coma" "cause contact allergy" "cause death"
           "cause diarrhea" "cause dizziness" "cause hallucinations" "cause itching"
           "cause nausea" "cause neurological damage" "cause paralysis" "cause swelling" "cause violent spasms"
           "raise blood pressure" "remove a sense" "stop breathing" "stop the heart" "turn blood to ichor"))

(random-table/register :name "Herbalist's Primer > Plant > Effect > Magical"
  :private t
  :data '("alert about danger" "allow seeing in the dark" "allow speaking with animals" "animate objects" "attract feyfolk"
           "attract love" "attract wealth" "bestow courage" "bestow luck" "bless the user"
           "break curses" "bring luck in games of chance" "bring prophetic visions" "calm animals" "cause fear"
           "conjure spirits" "create a circle of protection" "cure all diseases" "cure all wounds" "detect enemies"
           "detect lies" "divert lightning" "douse flames" "enforce telling the truth" "exorcise spirits"
           "explode" "find hidden treasures" "find water" "grant a sense of direction" "grant flight"
           "grant immortality" "grant invisibility" "grant safe travel" "grant second sight" "grant wishes"
           "keep wild animals at bay" "make the user beautiful" "open a portal to another realm" "open locks" "prolong life"
           "protect from compulsion" "protect from evil" "protect from harm" "purify the area" "put undead to rest"
           "raise the dead" "reflect malicious magic" "remove negative energy" "remove toxins" "stop shapeshifting"))

(random-table/register :name "Herbalist's Primer > Plant > Complication"
  :private t
  :data '("dangerous animals often protect it"
           "it grows in a desolate far-away place"
           "it is a deadly poison"
           "it is highly addictive"
           "it spontaneously combusts"
           "it is under the protection of the local law"
           "it is easy to mistake with a toxic plant"
           "it attracts mosquitoes"
           "it needs to be used within minutes of gathering"
           "it tastes like three-week-old garbage"
           "its juice stains everything it touches"
           "nobody has seen it in the last century"
           "possession is illegal"
           "special set of tools is needed for harvesting"
           "the stench sticks around for weeks"
           "the demand is higher than the supply"
           "the harvest time is just a single day"
           "the rumors say it brings bad luck"
           "touching it causes intense pain"
           "using it requires an obscure, elaborate ritual"))

;;; Solo GM-ing
(random-table/register :name "The “But” is related to"
  :private t
  :data '("A twist to the relationship between people in the situation"
           "An adjustment to the physical environment."
           "An error in an assumption some NPC is making."
           "A fact the hero thinks they know is actually wrong."
           "The failure of a piece of gear, either for the hero or an NPC."
           "Sublimely bad or good timing by a sudden event."))

(random-table/register :name "Oracle"
  :data '("- Action :: {Oracle > Action}\n- Theme :: {Oracle > Theme}"))

(random-table/register :name "Oracle > Action"
  :private t
  :data '("Scheme" "Clash" "Weaken" "Initiate" "Create"
           "Swear" "Avenge" "Guard" "Defeat" "Control"
           "Break" "Risk" "Surrender" "Inspect" "Raid"
           "Evade" "Assault" "Deflect" "Threaten" "Attack"
           "Leave" "Preserve" "Manipulate" "Remove" "Eliminate"
           "Withdraw" "Abandon" "Investigate" "Hold" "Focus"
           "Uncover" "Breach" "Aid" "Uphold" "Falter"
           "Suppress" "Hunt" "Share" "Destroy" "Avoid"
           "Reject" "Demand" "Explore" "Bolster" "Seize"
           "Mourn" "Reveal" "Gather" "Defy" "Transform"
           "Persevere" "Serve" "Begin" "Move" "Coordinate"
           "Resist" "Await" "Impress" "Take" "Oppose"
           "Capture" "Overwhelm" "Challenge" "Acquire" "Protect"
           "Finish" "Strengthen" "Restore" "Advance" "Command"
           "Refuse" "Find" "Deliver" "Hide" "Fortify"
           "Betray" "Secure" "Arrive" "Affect" "Change"
           "Defend" "Debate" "Support" "Follow" "Construct"
           "Locate" "Endure" "Release" "Lose" "Reduce"
           "Escalate" "Distract" "Journey" "Escort" "Learn"
           "Communicate" "Depart" "Search" "Charge" "Summon"))

(random-table/register :name "Oracle > Theme"
  :private t
  :data '("Risk" "Ability" "Price" "Ally" "Battle"
           "Safety" "Survival" "Weapon" "Wound" "Shelter"
           "Leader" "Fear" "Time" "Duty" "Secret"
           "Innocence" "Renown" "Direction" "Death" "Honor"
           "Labor" "Solution" "Tool" "Balance" "Love"
           "Barrier" "Creation" "Decay" "Trade" "Bond"
           "Hope" "Superstition" "Peace" "Deception" "History"
           "World" "Vow" "Protection" "Nature" "Opinion"
           "Burden" "Vengeance" "Opportunity" "Faction" "Danger"
           "Corruption" "Freedom" "Debt" "Hate" "Possession"
           "Stranger" "Passage" "Land" "Creature" "Disease"
           "Advantage" "Blood" "Language" "Rumor" "Weakness"
           "Greed" "Family" "Resource" "Structure" "Dream"
           "Community" "War" "Portent" "Prize" "Destiny"
           "Momentum" "Power" "Memory" "Ruin" "Mysticism"
           "Rival" "Problem" "Idea" "Revenge" "Health"
           "Fellowship" "Enemy" "Religion" "Spirit" "Fame"
           "Desolation" "Strength" "Knowledge" "Truth" "Quest"
           "Pride" "Loss" "Law" "Path" "Warning"
           "Relationship" "Wealth" "Home" "Strategy" "Supply"))

(random-table/register :name "Character Attitudes"
  :data '("willing" "excited" "reluctant" "resentful" "pleased" "outraged"
           "uncomfortable" "cautious" "powerless" "defiant" "frustrated" "obligated"
           "focused" "confident" "scared" "ambivalent" "wary" "amused"
           "reluctant" "suspicious" "worried" "fatigued" "obsessed" "curious"
           "focused" "optimistic" "determined" "worn down" "elated" "troubled"
           "uneasy" "thankful" "skeptical" "rebellious" "misunderstood" "daring"));;; D30 Sandbox Companion

(random-table/register :name "Location Adjectives of Disconnection"
  :data '("Deserted" "Claustrophobic" "Dismal" "Isolated" "Secluded" "Shadowy"
           "Neglected" "Shrouded" "Pallid" "Eerie" "Gloomy" "Towering"))

(random-table/register :name "Location Adjectives of Connection"
  :data '("Glorious" "Harmonious" "Dazzling" "Wonderful" "Delightful" "Inviting"
           "Mystical" "Magnificent" "Tranquil" "Silent" "Magical" "Charming"))

(random-table/register :name "Descriptive Words to Pair with Concrete Element"
  :data '("peaceful" "scary" "treacherous" "rejuvenating" "ancient" "broken"
           "idyllic"  "crumbling" "bucolic" "rocky" "gentle" "broken"
           "secret" "withering" "bleak" "fallen" "incandescent" "cataclysmic"
           "uncharted" "barren" "pastoral" "bleak" "misty" "delightful"
           "enchanted" "demolished" "rusted" "shimmering" "effervescent" "mysterious"
           "perilous" "sacred" "solitary" "eerie" "otherworldly" "wondrous"))

;; OSR Solo Rules written by Peter Rudin-Burgess Creative Common License Attribution 4.0 International (CC BY 4.0) https://creativecommons.org/licenses/by/4.0/
(random-table/register :name "Plot Twist"
  :data '("{Plot Twist > Subject} {Plot Twist > Event}"))

(random-table/register :name "Plot Twist > Subject"
  :private t
  :data '("An NPC" "Your PC" "An organization"
           "A physical event" "An emotional event" "An item"))

(random-table/register :name "Plot Twist > Event"
  :private t
  :data '("appears." "alters the location." "helps the hero."
           "hinders the hero." "changes the goal." "ends the scene."))

(random-table/register :name "How Far Away is a Thing > Same Place"
  :roller "2d4"
  :data '((2 . "Almost touching you")
           (3 . "Within arm's reach")
           (4 . "Just steps away")
           (5 . "just around the corner")
           (6 . "in the next room")
           (7 . "a few levels up (or down…)")
           (8 . "Somehwere in the dungeon")))

(random-table/register :name "How Far Away is a Thing > Distant Thing"
  :roller "2d4"
  :data '((2 . "The same patch of grass (or mountainside…)")
           (3 . "The same village (or lair or dungeon…)")
           (4 . "The same clan (or duchy or faction…)")
           (5 . "A mile or so away")
           (6 . "About 10-25 miles away")
           (7 . "About 100-200 miles away")
           (8 . "Very far away")))

(random-table/register :name "Intervention Type"
  :data '("New entity" "Entity positive" "Entity negative"
           "Advance plot" "Regress plot" "Wild"))

(random-table/register :name "Everything Is Not as Expected"
  :data '("Increase simple element"
           "Decrease simple element"
           "Add simple element"
           "Remove simple element"
           "Increase major element"
           "Decrease major element"
           "Add major element"
           "Remove major element"
           "Wild positive"
           "Wild negative"))

;; Ask the Stars” by Chris McDowall, available at www.bastionland.com.
(random-table/register :name "Ask the Stars"
  :data '("\n- Answer :: {Ask the Stars > Answer}\n- Symbol :: {Ask the Stars > Symbol}\n- Position :: {Ask the Stars > Position}"))

(random-table/register :name "Ask the Stars > Answer"
  :roller "1d12"
  :private t
  :data '(((1 . 3) . "Hard no")
           ((4 . 6) . "No")
           ((7 . 9) . "Yes")
           ((10 . 12) . "Hard yes")))

(random-table/register :name "Ask the Stars > Symbol"
  :private t
  :data '("The Fang (hostility/fear)" "The Wings (freedom/nature)"
           "The Cage (protection/obligation)" "The Hand (creation/misdirection)"
           "The Mask (persuasion/shame)" "The Eye (judgement/secrets)"
           "The Child (learning/greed)" "The Traveller (wandering/chance)"
           "The Elder (authority/tradition)" "The Fleet (direction/struggle)"
           "The Council (opposition/cycles)" "The Legion (unification/identity)"))

(random-table/register :name "Ask the Stars > Position"
  :private t
  :data '("Rising (growth/possibility)" "Entombed (memory/death)"
           "Twinned (intimacy/dependency)" "Waning (hunger/decay)"
           "Rooted (stability/plenty)" "Bowed (submission/mercy)"
           "Colliding (change/violence)" "Burning (honesty/pride)"
           "Veiled (faith/deceit)" "Exiled (guilt/autonomy)"
           "Crowned (ambition/ruin)" "Reflected (reversal/vanity)"))

;; https://yumdm.com/wp-content/uploads/2021/04/D12-Monthly-Issue-1.pdf
(random-table/register :name "What is the monster doing?"
  :roller "2d6"
  :data '((2 . "Caring for…")
           (3 . "Trapmaking/Marking territory")
           (4 . "Crafting/Sharpening weapons")
           (5 . "Resting/Sleeping/Recreation")
           (6 . "Moving through area")
           (7 . "Patrolling territory")
           (8 . "Eating/Cooking")
           (9 . "Searching for…")
           (10 . "Escaping from…")
           (11 . "Trading with…")
           (12 . "Fighting with…")))

(random-table/register :name "OSE > Reaction Roll"
  :roller (lambda (table)
            (+ (random-table/roller/string "2d6")
              (string-to-number (completing-read "Charisma Modifier: "
                                  '("-2" "-1" "0" "1" "2") nil t))
              (string-to-number (completing-read "Additional Modifier: "
                                  '("-2" "-1" "0" "1" "2") nil t "0"))
              ))
  :data '(((-2 . 2) . "\n- Reaction :: Attacks")
           ((3 . 5) . "\n- Reaction :: Hostile, may attack\n- Motivation :: {OSE > Monster Motivation}")
           ((6 . 8) . "\n- Reaction :: Uncertain, confused\n- Motivation :: {OSE > Monster Motivation}")
           ((9 . 11) . "\n- Reaction :: Indifferent, may negotiate\n- Motivation :: {OSE > Monster Motivation}")
           ((12 . 16) . "\n- Reaction :: Eager, friendly")))

(random-table/register :name "OSE > Monster Motivation"
  :data '("Food: They’re hungry. You can distract them with rations, point them towards corpses, cast a food illusion. They could be hurt and in need of healing."
           "Gold: They want d100 GP x their HD. This could be a tax, a toll, tribute, tithe, or they could just be greedy bastards."
           "Treasure/Magic Items: They want a number of items equal to their HD. Scrolls and potions count as well."
           "Random Item: Roll a random item from a random character’s sheet, they want that for some reason."
           "Territory: This is their territory, they will defend it, but mostly they just want you to leave or prove why you should be allowed to pass through."
           "Information: They want to know about a rival faction, nearby NPC or monster, or dungeon landmark or location."
           "Help: They need something from further in the dungeon, or from a nearby wilderness hex. They may want you to kill other monsters in the dungeon or clear out a hex. They may be haunting the area and can only leave when their quest is fulfilled."
           "Trade: They have a random item from each category on the equipment list (one piece of armor, one weapon, one piece of equipment, etc.) and they’re willing to trade those items or sell them. All trades made inside the dungeon are at a higher markup than you’ll find in town."
           "To Complete a Mission: They’re in service to the closest NPC in the dungeon and whatever that NPC wants, this monster is on a mission to help achieve that aim."
           "Directions: They’re lost and are looking for directions out, or for someone to escort them to a safe area."))

(random-table/register :name "OSE > Random Dungeon Content"
  :roller "d66"
  :data '((11 . "Contents Empty with Treasure")
           ((12 . 16) . "Contents Empty")
           (21 . "Contents Empty with Treasure")
           ((22 . 26) . "Contents Empty")
           ((31 . 33) . "Monster with Treasure")
           ((34 . 36) . "Monster")
           ((41 . 43) . "Monster with Treasure")
           ((44 . 46) . "Monster")
           ((51 . 56) . "Special")
           ((61 . 62) . "Trap with Treasure")
           ((63 . 66) . "Trap with Treasure")))

(random-table/register :name "The Anarchical Grimoire of Propylonic Discharges"
  :data '("The Immediate Removal of {AGoPD > d10}."
           "The Violent Eradicator of {AGoPD > d10}."
           "The Animation of {AGoPD > d10}."
           "Reverse {AGoPD > d8}."
           "Symmetrical yet Unstable {AGoPD > d8}."
           "Blatant and Ignited {AGoPD > d8}."
           "An Unexpected Summoning {AGoPD > d6}."
           "The Effulgent Translocation {AGoPD > d6}."
           "Pandemonic Ramming {AGoPD > d6}."
           "The Haunting of a Door by {AGoPD > d4}."
           "The Possessing of the Caster by {AGoPD > d4}."
           "The Efficacious Repelling of {AGoPD > d4}."))

(random-table/register :name "AGoPD > d10"
  :private t
  :data '("Locks & Bolts" "Hinges & Rails" "Handles & Knobs"
           "Glyphs & Runes" "Door-steps" "Door-panels" "Door-mats"
           "Door-frames" "Knockers" "Peep-holes"))

(random-table/register :name "AGoPD > d8"
  :private t
  :data '("By-passing of {AGoPD > d10}"
           "Disappearance of {AGoPD > d10}"
           "Transmutation of {AGoPD > d10}"
           "Impotence"
           "Exhibition"
           "Walling"
           "Dead-bolting"
           "Entrance"))

(random-table/register :name "AGoPD > d6"
  :private t
  :data
  '("of Helpful {AGoPD > d4}" "of Silver {AGoPD > d10}" "of the Magic-User"
     "Applied to One’s Enemies" "as Protection" "In a Delayed Fashion"))

(random-table/register :name "AGoPD > d4"
  :private t
  :data '("Cat-headed Things" "Heralds of the Termite People"
           "A Fae Porter-Knight" "The Ghost of Katastroph"))

(random-table/register :name "Scarlet Heroes > NPC"
  :data '("\n- Actor :: {Scarlet Heroes > Actor Type}\n- Relation :: {Scarlet Heroes > Actor > Relationship}\n- Desire :: {Scarlet Heroes > NPC > Immediate Desire}\n- Temperment :: {Scarlet Heroes > NPC > Ruling Temperament}\n- Memorable Trait :: {Scarlet Heroes > NPC > Memorable Traits}"))

(random-table/register :name "Scarlet Heroes > NPC > Immediate Desire"
  :private t
  :data '("Aiding a friend" "Avenging a slight" "Bribing an official"
           "Buying an object" "Collecting a bribe" "Collecting a debt"
           "Commit a crime" "Curing a sickness" "Destroying evidence"
           "Earning money" "Funding a funeral" "Getting a document"
           "Getting drunk" "Going home to rest" "Having a man/woman"
           "Helping a relative" "Impressing a lover" "Impressing the boss"
           "Paying a debt" "Recovering a lost item" "Revealing a secret"
           "Selling an object" "Spreading a faith" "Spying on a person"
           "Stealing from the boss"))

(random-table/register :name "Scarlet Heroes > NPC > Ruling Temperament"
  :private t
  :data '("Ambitious" "Bigoted" "Capricious"
           "Cautious" "Compassionate" "Deceitful"
           "Exhibitionistic" "Fearful" "Garrulous"
           "Greedy" "Indecisive" "Inquisitive"
           "Lazy" "Loyal" "Lustful"
           "Merciful" "Observant" "Patient"
           "Proud" "Scornful" "Shy"
           "Stubborn" "Valorous" "Vicious"
           "Wrathful"))

(random-table/register :name "Scarlet Heroes > NPC > Memorable Traits"
  :private t
  :data '("Always carries things" "Always hurried" "Asthmatic"
           "Blind in an eye" "Careless dresser" "Constantly watchful"
           "Dark, sober clothes" "Deaf or hard of hearing" "Elaborate tattoos"
           "Emphatic speech" "Facial scarring" "Gaudy jewelry"
           "Immaculate clothes" "Laconic speaker" "Magnificent hair"
           "Missing an appendage" "Numerous piercings" "Often drunk"
           "Out of shape" "Precise hands" "Shaven or balding"
           "Stutters" "Subtle fragrance" "Tends work constantly"
           "Twitches regularly"))

(random-table/register :name "Scarlet Heroes > Actor Type"
  :private t
  :roller #'random-table/roller/prompt-from-table-data
  :data '(("Commoner" . "{Scarlet Heroes > Actor > Commoner}")
           ("Underworld" . "{Scarlet Heroes > Actor > Underworld}")
           ("Elite or Noble" . "{Scarlet Heroes > Actor > Elite and Noble}")))

(random-table/register :name "Scarlet Heroes > Actor > Commoner"
  :private t
  :data '("Ambitious scholar" "Battered ex-adventurer" "Beautiful young mistress"
           "Bold ship captain" "Commercial moneylender" "Cunning shipwright"
           "Cynical investigator" "Doddering sage" "Drunken sailor"
           "Dubious spice merchant" "Earthy construction boss" "Elderly crafter"
           "Erudite bookseller" "Exquisite dancer" "Grasping merchant"
           "Grizzled border hunter" "Gross slave or labor trader" "Honest gravetender"
           "Humble day laborer" "Intrepid explorer" "Marriage-minded deb"
           "Minor government official" "Mistreated servant or slave" "Pitiless tax collector"
           "Polished tea house owner" "Promising artist" "Puzzled foreigner"
           "Rakehell family heir" "Relentless bounty hunter" "Reputable tong brother"
           "Retired mercenary officer" "Rough caravan master" "Sagacious alchemist"
           "Sincere priest" "Skillful artisan" "Stern market magistrate"
           "Suspicious farm owner" "Veteran soldier" "Wealthy bachelor"
           "Weary physician"))

(random-table/register :name "Scarlet Heroes > Actor > Underworld"
  :private t
  :data '("Ambitious guttersnipe" "Amoral assassin" "Bitter pretender to rank"
           "Black marketeer" "Callous blackmailer" "Careworn priest"
           "Cheap legbreaker" "Cheating merchant" "Cretinous street thug"
           "Crippled beggar child" "Cynical young whore" "Deceitful footpad"
           "Degraded former noble" "Depraved slumming noble" "Desperate young mother"
           "Diseased beggar" "Disguised con artist" "Dissipated physician"
           "Drug den proprietor" "Earnest charity worker" "Exiled foreigner"
           "Expert forger" "Grasping fence" "Hard-drinking labor boss"
           "Hardened burglar" "Haughty ninja" "Impartial info broker"
           "Impoverished ex-noble" "Knife-slinging gambler" "Menacing tong enforcer"
           "Migrant farmer" "Naive country girl" "Press gang boss"
           "Priest of a forbidden faith" "Ruthless moneylender" "Scrawny pickpocket"
           "Street-worn prostitute" "Suspicious urchin" "World-weary madame"
           "Wretched miser"))

(random-table/register :name "Scarlet Heroes > Actor > Elite and Noble"
  :private t
  :data '("Aged plutocrat"  "City magistrate" "Cynical watch leader" "Discreet banker"
           "Exiled pretender" "Famed courtesan" "Famous artist" "Favored concubine"
           "Feared court blackmailer" "Foreign diplomat" "Foreign nobleman" "Heartless noble matron"
           "High military officer" "High-ranking priest" "Instructor to nobles" "Master assassin"
           "Mighty war hero" "Minister of taxes" "Minister of trade" "Newlywed noble bride"
           "Ninja clan leader" "Official investigator" "Powerful sorcerer" "Provincial governor"
           "Purveyor of rare sins" "Rich moneylender" "Riotous young heir" "Scheming daifu"
           "Scion of a noble lineage" "Scion of past rulers" "Shipping fleet owner" "Social club leader"
           "Sorcerous “fixer”" "Steward of family lands" "Tong grandfather" "Veteran adventurer"
           "Wealthy heir" "Wealthy landowner" "Wealthy merchant prince" "Widely-sought maiden"))

(random-table/register :name "Scarlet Heroes > Actor > Relationship"
  :private t
  :data '("Business partner" "Child" "Childhood friend" "Co-workers" "Cousin"
           "Crime culprit" "Crime partner" "Crime victim" "Ex-lover" "Ex-spouse"
           "Grandparent" "Has blackmail" "Heir to something" "Inlaws" "Lover"
           "Old favor" "Parent" "Rival" "Schoolmates" "Sibling"
           "Society fellows" "Spouse" "Subordinate" "Superior" "Uncle/Aunt"))

(random-table/register :name "Scarlet Heroes > Reaction Roll"
  :roller #'random-table/roller/prompt-from-table-data
  :data '(("Friendly" . "{Scarlet Heroes > Reaction Roll > Friendly}")
           ("Stranger" . "{Scarlet Heroes > Reaction Roll > Stranger}")
           ("Unfriendly" . "{Scarlet Heroes > Reaction Roll > Unfriendly}")))

(random-table/register :name "Scarlet Heroes > Reaction Roll > Friendly"
  :private t
  :roller '(+ "2d6" "Charisma Modifier" "Reaction Modifier")
  :data '(((-9 . 1) . "{Scarlet Heroes > Reaction Roll > Stranger}")
           (2 . "Sneering contempt")
           (3 . "Flat dismissal")
           (4 . "Reasoned refusal")
           (5 . "Bribeable")
           (6 . "Persuadable")
           (7 . "Hesitant Agreement")
           (8 . "Quick consent")
           (9 . "Pleased consent")
           (10 . "Vigorous consent")
           (11 . "Firm commitment")
           ((12 . 23) . "Bold enthusiasm")))

(random-table/register :name "Scarlet Heroes > Reaction Roll > Stranger"
  :private t
  :roller '(+ "2d6" "Charisma Modifier" "Reaction Modifier")
  :data '(((-9 . 1) . "{Scarlet Heroes > Reaction Roll > Unfriendly}")
           (2 . "Anger")
           (3 . "Annoyance")
           (4 . "Dismissal")
           (5 . "Flat refusal")
           (6 . "Qualified refusal")
           (7 . "Bribeable")
           (8 . "Qualified consent")
           (9 . "Tentative agreement")
           (10 . "Full agreement")
           (11 . "Helpful consent")
           ((12 . 23) . "Admiring consent")))

(random-table/register :name "Scarlet Heroes > Reaction Roll > Unfriendly"
  :private t
  :roller '(+ "2d6" "Charisma Modifier" "Reaction Modifier")
  :data '(((-9 . 1) . "Violence or direct harm")
           (2 . "Anger/Violence")
           (3 . "Anger/Scorn")
           (4 . "Scorn")
           (5 . "Dismissal")
           (6 . "Flat rejection")
           (7 . "Considered refusal")
           (8 . "Bribeable")
           (9 . "Persuadable")
           (10 . "Qualified consent")
           (11 . "Grudging agreement")
           ((12 .23) . "Tolerant consent")))

;;; Dolmenwood

;; Common names
(random-table/register :name "Breggle Name > Masculine Given Name"
  :private t
  :data '("Addle" "Aele" "Blocke" "Braembel" "Broob"
           "Crump" "Curlip" "Eleye" "Grennigore" "Gripe"
           "Hrannick" "Lope" "Mashker" "Shadgore" "Shadwell"
           "Shadwicke" "Shank" "Snerd" "Snerg" "Windor"))

(random-table/register :name "Breggle Name > Surname"
  :private t
  :data '("Blathergripe" "Bluegouge" "Bockburrough" "Bockstump" "Elbowgen"
           "Forlocke" "Hwodlow" "Lankshorn" "Lockeshorn" "Longbeard"
           "Longshanks" "Shankwold" "Smallbuck" "Snicklebock" "Snidebleat"
           "Snoode" "Underbleat" "Underbuck" "Wolder" "Woldleap"))

(random-table/register :name "Breggle Name > Feminine Given Name"
  :private t
  :data '("Berrild" "Clover" "Crewwin" "Draed" "Ellip"
           "Fannigrew" "Frandorup" "Grendilore" "Grewigg" "Gwendl"
           "Hildrup" "Hraigl" "Hwendl" "Hwoldrup" "Lindor"
           "Maybel" "Merrild" "Myrkle" "Nannigrew" "Pettigrew"))

(random-table/register :name "Errant > Lock"
  :data '("- Type :: {Errant > Lock > Type}\n- Actions :: {Errant > Lock > Actions}\n- Modifier :: {Errant > Lock > Modifier}"))

(random-table/register :name "Errant > Lock > Type"
  :private t
  :store t
  :data '("Strange" "Adamantine" "Mythril" "Diamond" "Dwarven" "Elvish" "Steel" "Iron" "Brass" "Copper" "Tin" "Crude"))

(random-table/register :name "Errant > Lock > Actions"
  :roller (lambda (table)
            "Roll on TABLE, considering that types have same action in a campaign."
            ;; In Errant, once you encounter a given Lock Type (e.g. Elvish) it
            ;; should always have the same opening action sequence.  Hence the
            ;; prompt.
            (let ((data (random-table-data table)))
              (if (yes-or-no-p (format "Lock action already established for %s type? "
                                 (random-table/storage/results/get-text-for-roll "Errant > Lock > Type")))
                (completing-read "Lock’s Action: " data nil t)
                (random-table/roller/string (format "1d%s" (length data))))))
  :private t
  :fetcher (lambda (data &rest choice)
             "We may have picked the string...Or rolled."
             (if (numberp (car choice))
               (nth (- (car choice) 1) data)
               (car choice)))
  :data '("Twist, Tap, Twist" "Twist, Tap, Turn" "Twist, Turn, Twist"
           "Twist, Turn, Tap" "Tap, Twist, Tap" "Tap, Twist, Turn"
           "Tap, Turn, Twist" "Tap, Turn, Tap" "Turn, Twist, Tap"
           "Turn, Twist, Turn" "Turn, Tap, Twist" "Turn, Tap, Turn"))

(random-table/register :name "Errant > Lock > Modifier"
  :private t
  :data '("Spiked" "Spiked" "Secured" "Secured"
           "Weathered" "Weathered" "Cracked" "Cracked"
           "Normal" "Normal" "Normal" "Normal"))

(random-table/register :name "Errant > Death & Dying"
  :roller #'random-table/roller/prompt-from-table-data
  :data '(("Physical (Stabbing, Ripping, Crushing, etc.)" . "{Errant > Death & Dying > Physical}")
           ("Shocking (Electricity, Cold, Pyschic, etc.)" . "{Errant > Death & Dying > Shocking}")
           ("Burning (Fire, Acid, Lava, Digestive Enzymes, etc.)" . "{Errant > Death & Dying > Burning}")
           ("Toxic" . "{Errant > Death & Dying > Toxic}")))

(random-table/register :name "Errant > Death & Dying > Physical"
  :data '((1 . "Slow internal bleeding. /On death’s door/, but in EXPLORATION TURNS.")
           (2 . "Leg mangled. Can’t run. If both legs go, you can’t walk.")
           (3 . "Arm wrecked. If both arms go, you can’t hold anything.")
           (4 . "/On death's door/")
           (5 . "Leg destroyed (severed or hanging by sinews). Can’t run. If both legs go, you can’t walk. Also /on death’s door/.")
           (6 . "Arm destroyed (severed or hanging by sinews). If both arms go, you can’t hold any- thing. Also /on death’s door/.")
           (7 . "Head shot. /on death’s door/ and major brain trauma.")
           (8 . "Throat or lung torn open. /Consigned to the reaper/.")
           (9 . "Guts hanging out. /Consigned to the reaper/ and /out of action/.")
           ((10 . 15) . "Dead.")
           ((16 . 100) . "Deader than Dead (unable to be revived or properly buried.)")))

(random-table/register :name "Errant > Death & Dying > Shocking"
  :data '((1 . "Zapped. Stunned for an INITIATIVE TURN.")
           (2 . "Knocked out. Unconscious (DEPLETION 1).")
           (3 . "Concussed. Knocked out for d12 INITIATIVE TURNS and 1 point of EXHAUSTION.")
           (4 . "Cardiac arrest. /On death’s door/ and 1 point of EXHAUSTION.")
           (5 . "Scrambled. Major brain trauma and /on death’s door/.")
           (6 . "Deep fried. Unconscious (DEPLETION 1), major brain trauma, and /on death’s door/.")
           (7 . "Internal damage. Coughing up blood or bleeding from eyes and mouth. /Consigned to the reaper./")
           (8 . "Respiratory system failure. /Consigned to the reaper./")
           (9 . "Brain dead. /Consigned to the reaper/ and /out of action/.")
           ((10 . 15) . "Dead")
           ((16 . 100) . "Deader than Dead (unable to be revived or properly buried.)")))

(random-table/register :name "Errant > Death & Dying > Burning"
  :data '((1 . "Eye destroyed. If both eyes go, you’re blind.")
           (2 . "Mouth melted. Can’t speak, only grunt and moan (unable to cast SORCERIES or MIRACLES.)")
           (3 . "Face melted.")
           (4 . "Fingers burnt off.")
           (5 . "Suffocating. /On death’s door/ and 1 point of EXHAUSTION.")
           (6 . "Nose is burnt off, inner ears ruined. Deaf and can no longer smell or taste. Also /on death’s door/.")
           (7 . "Your lungs and face are burnt off. Also /on death’s door/ and 1 point of EXHAUSTION.")
           (8 . "Skin burned off. /Consigned to the reaper/.")
           (9 . "Burnt to a crisp. /Consigned to the reaper/ and /out of action/.")
           ((10 . 15) . "Dead")
           ((16 . 100) . "Deader than Dead (unable to be revived or properly buried.)")))

(random-table/register :name "Errant > Death & Dying > Toxic"
  :data '((1 . "Nauseous. 1 point of EXHAUSTION.")
           (2 . "Immune system compro- mised. HP halved.")
           (3 . "Blood tainted. Can’t recover HP.")
           (4 . "Bleeding from nose and eyes. /On death’s door/, but in exploration turns.")
           (5 . "Excreting blood from pores. /On death’s door/, but in EXPLORATION TURNS, and 1 point of EXHAUSTION.")
           (6 . "Rupture. You’re throwing up black acrid blood. /On death’s door/.")
           (7 . "Nervous system shutdown. Can’t move and /on death’s door/.")
           (8 . "Immune system shutdown. /Consigned to the reaper/.")
           (9 . "Total organ failure. /Consigned to the reaper/ and /out of action/.")
           ((10 . 15) . "Dead")
           ((16 . 100) . "Deader than Dead (unable to be revived or properly buried.)")))

;;; Frog God Games
(random-table/register :name "Inn Name"
  :data '("\n- Name :: The {Inn > Creature Adjective} {Inn > Creature}\n- Description :: {Inn > Description}"
           "\n- Name :: The {Inn > Creature} and {Inn > Creature}\n- Description :: {Inn > Description}"
           "\n- Name :: The {Inn > Creature}’s {Inn > Item}\n- Description :: {Inn > Description}"
           "\n- Name :: The {Inn > Creature}\n- Description :: {Inn > Description}"
           "\n- Name :: The {Inn > Item Adjective} {Inn > Item}\n- Description :: {Inn > Description}"
           "\n- Name :: The {Inn > Item} and {Inn > Item}\n- Description :: {Inn > Description}"))

(random-table/register :name "Inn > Creature Adjective"
  :private t
  :data '("{1d6 + 1}" "Blind" "Bloody" "Blue" "Bouncing"
           "Bronze" "Cheerful" "Copper" "Dancing" "Drunken"
           "Fat" "Flying" "Golden" "Good" "Green"
           "Grey" "Growling" "Happy" "Hunting" "Jaunty"
           "Jolly" "Leaping" "Lost" "Lucky" "Mad"
           "Merry" "Musical" "Odd" "Pale" "Pious"
           "Prancing" "Prowling" "Ragged" "Red" "Royal"
           "Shining" "Silver" "Singing" "Sleeping" "Smiling"
           "Spotted" "Stone" "Striped" "Timid" "Traveling"
           "Wandering" "Wanton" "Weary" "Woebegone" "Wooden"))

(random-table/register :name "Inn > Creature"
  :private t
  :data '("Abbot (or Man)" "Baron (or Serf or Inkeeper)" "Beggar" "Bird (or Parrot)"
           "Boar (or Pagan)" "Crab (or Acorn or Forester)" "Dragon (or Drake)" "Druid (or Bishop)"
           "Farmer" "Fish (or Heron)" "Fool (or Harper)" "Fox (or Husband)" "Frog (or Plover)"
           "Goose (or Gander)" "Griffon (or Hippogriff)" "Hangman (or Knight)"
           "Harper (or Duke or Harlequin)" "Harpy (or Trout)" "Jester" "Judge (or Princess)"
           "Knight (or Peacock)" "Lady (or Lord or Baroness)" "Lion (or Tiger)" "Magpie (or Knave)"
           "Manticore (or Mouse or Mandrake)" "Merchant" "Miller (or Smith)" "Minstrel"
           "Monk (or Friar)" "Peasant" "Pheasant (or Hog)" "Piper (or Swan)" "Pony (or Donkey)"
           "Ram (or Mermaid)" "Raven (or Crow)" "Reeve (or Bailiff)" "Robin (or Dog)"
           "Rooster (or Hen)" "Sage (or Scholar)" "Satyr (or Hawk)" "Shepherd (or Sheep)"
           "Sheriff (or Tree)" "Squire (or Pikeman)" "Tinker" "Toad (or Otter)" "Troll (or Hedgehog)"
           "Warhorse (or Destrier or Horse)" "Widow (or Wife)" "Wizard (or Mage)" "Wyvern (or Whale)"))

(random-table/register :name "Inn > Item"
  :private t
  :data '("Banner (or Flag)" "Bed (or Blanket)" "Bucket" "Candle (or Brand)" "Cauldron" "Chair"
           "Chest" "Circle (or Circlet or Crown)" "Cloak (or Doublet)" "Cup (or Chalice)"
           "Dagger" "Den (or Friend)" "Drum" "Fiddle (or Pipe)" "Foot (or Footprint or Hoof)"
           "Goblet" "Halberd" "Hand (or Claw)" "Harp (or Tambour)" "Hat" "Head"
           "Hearth" "Home" "Hood" "Kettle (or Pot)" "Key" "Lance (or Broadsword)"
           "Landing (or Forge)" "Lantern (or Lamp)" "Mace" "Mallet (or Pike)"
           "Mask (or Hammer)" "Moon" "Pillar (or Door)" "Purse" "Rope (or Harness)"
           "Scythe" "Shield (or Gauntlet)" "Shoe" "Staff (or Quarterstaff)" "Stone"
           "Sword (or Banquet)" "Table" "Tankard (or Hall)" "Tear (or Teardrop or Toes)"
           "Torch (or Castle)" "Tower" "Wagon" "Wand (or Talisman)" "Wheatsheaf (or Tail)"))

(random-table/register :name "Inn > Item Adjective"
  :private t
  :data '("Barking" "Black (or Copper)" "Blue" "Broken (or Bent or Fallen or Torn)"
           "Brown" "Bumpy (or Rugged)" "Cloven" "Covered" "Crooked (or Leaning or Diagonal)"
           "Dancing" "Double (or Triple)" "Dreaming" "Fancy" "Flying" "Giant's" "Glowing"
           "Golden" "Good" "Green" "Grey" "Hanging (or Checkered or Checked or Pied)"
           "Hidden" "High" "Large" "Lost" "Magic (or Enchanted)" "Mended" "Mounted (or Welcoming)"
           "Mystical (or Painted)" "New" "Old" "Perfect" "Proud" "Purple" "Red"
           "Royal" "Shining" "Sideways" "Silver (or Silvery)" "Simple" "Singing (or Fat or Changeable)"
           "Sleeping" "Small" "Talking" "Two-Handed (or Ample)" "Unusual (or Odd)"
           "Well-made" "Whistling" "White" "Yellow"))

(random-table/register :name "Inn > Description"
  :private t
  :roller "1d100"
  :data '(((1 . 20) . "Main inn building, stable, outhouse, barn for chickens and {1d4} [d4] cows. These are the least secure of roadside inns, and usually (but definitely not always) have the lowest quality food and lodgings. They do not have additional horses.")
           ((21 . 25) . "Religious hostel with chapel, stone curtain-wall, main inn building, stable, smithy, barn, outhouse, {1d3} [1d3] outbuildings (shed, dovecote, etc.), chicken coops and/or pigsties.")
           ((26 . 30) . "Small tower, stone curtain-wall, main inn building, stable, smithy, barn, outhouse, {1d3} [d3] outbuildings (shed, dovecote, etc.), chicken coops and/or pigsties, and {1d3} [d3] small houses within the curtilage. These are usually built on the ruins of, or using remaining structures from, ancient Hyperborean road-forts. These highly-secure inns tend to be the most prosperous and the highest quality. A few horses are almost always for sale at these large stopping-places, along with a tailor, leatherworker, and one or two other craftsmen who live here.")
           ((31 . 50) . "Stone curtain-wall, main inn building, stable, smithy, barn, outhouse, {1d3} [d3] outbuildings (shed, dovecote, etc.), chicken coops and/or pigsties. Fresh mounts will be available here.")
           ((51 . 75) . "Wooden palisade-wall, main inn building, stable, outhouse, {1d3} [d3] outbuildings (shed, dovecote, etc.), chicken coops and/or pigsties. With no smithy, an inn this size generally does not have any additional horses.")
           ((76 . 100) . "Wooden palisade-wall, main inn building, stable, smithy, barn, outhouse, {1d3} outbuildings (shed, dovecote, etc.), chicken coops and/or pigsties. The presence of a smithy indicates that this size inn probably has one or more horses available for travelers in need of fresh mounts.")))

(random-table/register :name "Borderlands > Place Names > Geographical > Suilley Rampart"
  :private t
  :data '("Ej" "Bassin" "Bas" "Hilsej" "Pont"
           "Ribiere" "Broc" "Cair" "Caer" "Castel"
           "Gleisa" "Chiria"  "Faletz" "Val" "Faletz"
           "Patiaj" "Travessar" "Val" "Boule" "Ronde"
           "Ej" "Ej" "Fair" "Fel" "Valt"
           "Guet" "Forett" "Bifurc" "Fort" "Petival"
           "Bocage" "Redenza" "Villa" "Stoc" "Lande"
           "Haut" "Colline" "Boule" "Holt" "Ostal"
           "Nel" "Stoc" "Nel" "Lac" "Barcasej"
           "Lane" "Pasture" "Bord" "Marc" "Mercat"
           "Maraiz" "Champ" "Molie" "Mine" "Fan"
           "Mire" "Tourbietz" "Mont" "Montagne" "Boc"
           "Mud" "Noc" "Champ" "Pec" "Pit"
           "Ponj" "Etaang" "Ridj" "Marc" "Riu"
           "Ruad" "Escarp" "Side" "Ej" "Val"
           "Slope" "Font" "Stoc" "Caer" "Stoc"
           "Rajol" "Valt" "Villa" "Ton" "Top"
           "Tor" "Vil" "Sentir" "Bifurc" "Val"
           "Voir" "Eau" "Puit" "Ferme" "Ferme"
           "Foret" "Granja" "Patiaj" ))

(random-table/register :name "Borderlands > Place Names > Geographical > Eastreach-Exeter"
  :private t
  :data '("Bank" "Basin" "Botham" "Bray" "Bridge"
           "Brook" "Brook" "Cairn" "Caster" "Castle"
           "Church" "Clearing" "Cliff" "Combe" "Crag"
           "Croft" "Cross" "Dean" "Dell" "Down"
           "Edge" "Fair" "Fell" "Firth" "Ford"
           "Forest" "Fork" "Fort" "Glen" "Grove"
           "Hall" "Ham" "Harrow" "Heath" "Heights"
           "Hill" "Hollow" "Holt" "House" "How"
           "Kirk" "Knoll" "Lake" "Landing" "Lane"
           "Lynch" "March" "Mark" "Market" "Marsh"
           "Meadow" "Mill" "Mine" "Minster" "Mire"
           "Moor" "Mound" "Mountain" "Mouth" "Mud"
           "Nook" "Pasture" "Peak" "Pit" "Pond"
           "Pool" "Ridge" "River" "Road" "Scarp"
           "Side" "Slade" "Slade" "Slope" "Spring"
           "Stoke" "Stone" "Stow" "Stream" "Thicket"
           "Thorpe" "Ton" "Top" "Tower" "Town"
           "Trail" "Twitchel" "Valley" "View" "Water"
           "Well" "Wich" "Wick" "Wold" "Worth"
           "Yard"))

;;; Knave

(random-table/register :name "Knave > Wilderness Region"
  :data '("Ashland" "Badland" "Bamboo Forest" "Basalt Columns" "Bay" "Beach" "Bluff" "Bog" "Boulder Field" "Brook"
           "Butte" "Caldera" "Canyon" "Cave" "Cliff" "Cloud Forest" "Coniferous Forest" "Copse" "Crag" "Crater"
           "Creek" "Crossing" "Crystals" "Deciduous Forest" "Delta" "Den" "Dunes" "Dust Bowl" "Fen" "Fjord"
           "Floodplain" "Gas Vent" "Geyser" "Glacier" "Gorge" "Grotto" "Grove" "Gulch" "Heath" "Highland"
           "Hollow" "Hoodoo" "Hot Spring" "Ice Sheet" "Jungle" "Knoll" "Lagoon" "Lair" "Lake" "Lakebed"
           "Lava Field" "Lava Tube" "Loch" "Mangrove Swamp" "Marsh" "Meadow" "Mesa" "Mire" "Moor" "Mountain"
           "Mud Plain" "Oasis" "Oil Seep" "Pass" "Pasture" "Petrified Forest" "Pit" "Plateau" "Pond" "Prairie"
           "Quicksand" "Rainforest" "Rapids" "Ravine" "Ridge" "River" "Riverland" "Rockslide" "Salt Flat" "Salt Marsh"
           "Savanna" "Scree Slope" "Scrubland" "Sinkhole" "Spring" "Steppe" "Stream" "Sulfur Spring" "Swamp" "Taiga"
           "Tar Pit" "Thicket" "Tundra" "Valley" "Volcanic Plain" "Volcano" "Wasteland" "Waterfall" "Wetlands" "Whirlpool"))

(random-table/register :name "Knave > Wilderness Structure"
  :data '("Abbey" "Altar" "Amphitheater" "Aqueduct" "Archive" "Asylum" "Bandit Camp" "Barn" "Battlefield" "Bell Tower"
           "Bonfire" "Bower" "Brazier" "Cairn" "Cart Track" "Castle" "Catacomb" "Chapel" "City" "Cistern"
           "Convent" "Crossroads" "Dam" "Dirt Road" "Dolmen" "Dungeon" "Farm" "Ferry" "Festival" "Fishing Hut"
           "Ford" "Forester Lodge" "Fort" "Gallows" "Garden" "Garrison" "Gate" "Gibbet" "Graveyard" "Hamlet"
           "Hedge" "Henge" "Hermitage" "Hideout" "Highway" "Hunter’s Camp" "Hunting Lodge" "Inn" "Keep" "Library"
           "Lighthouse" "Logging Camp" "Manor" "Market" "Memorial" "Mill" "Mine" "Monastery" "Monolith" "Monument"
           "Mosaic" "Mule Track" "Obelisk" "Orchard" "Outpost" "Paved Road" "Pen" "Pilgrim Camp" "Pillar" "Port"
           "Prison" "Pyramid" "Refugee Camp" "Road" "Ruin" "Shepherd Hut" "Shrine" "Signal Tower" "Stable" "Statue"
           "Stone Bridge" "Stone Circle" "Surveyor Camp" "Tavern" "Temple" "Toll House" "Tomb" "Tower" "Town" "Trader Camp"
           "Trail" "Trap" "Village" "Wall" "Watchtower" "Watermill" "Well" "Windmill" "Wizard Tower" "Wooden Bridge"))


(random-table/register :name "Knave > Wilderness Trait"
  :data '("Amber" "Ashen" "Bewitching" "Black" "Blasted" "Blessed" "Blighted" "Bloody" "Boiling" "Bright"
           "Broken" "Burning" "Cerulean" "Clouded" "Cracked" "Creeping" "Crimson" "Cursed" "Dark" "Dead"
           "Desolate" "Divine" "Doomed" "Echoing" "Eerie" "Elder" "Eldritch" "Emerald" "Endless" "Eternal"
           "Fallen" "Fearsome" "Fell" "Forbidden" "Forgotten" "Forsaken" "Frozen" "Ghostly" "Glittering" "Gloomy"
           "Golden" "Grim" "Haunted" "Hidden" "Holy" "Infested" "Iron" "Jade" "Jagged" "Loathsome"
           "Lonely" "Misty" "Murmuring" "Mysterious" "Oozing" "Overgrown" "Perilous" "Petrified" "Phantasmal" "Puzzling"
           "Ravaged" "Ravenous" "Restless" "Savage" "Scorching" "Screaming" "Shadowy" "Shattered" "Shifting" "Shining"
           "Shivering" "Shrouded" "Silent" "Silver" "Singing" "Sinister" "Sinking" "Sleeping" "Sliding" "Stoney"
           "Stormy" "Sunken" "Swarming" "Sweltering" "Thorny" "Thundering" "Torrential" "Twisting" "Unquiet" "Vanishing"
           "Vast" "Violet" "Wandering" "Watching" "Whispering" "White" "Withered" "Wondrous" "Writhing" "Yellow"))

(random-table/register :name "Knave > Wilderness Theme"
  :data '("Blessings" "Blindness" "Blood" "Bones" "Books" "Brains" "Chaos" "Children" "Collapse" "Combat"
           "Confusion" "Corpses" "Corruption" "Creation" "Criminal Activity" "Crows" "Cults" "Curses" "Death" "Decay"
           "Disease" "Divination" "Dragons" "Drowning" "Echoes" "Eyes" "Faces" "Feasting" "Fog" "Gateways"
           "Ghosts" "Gods" "Hands" "Holy War" "Hunger" "Hunting" "Imprisonment" "Invasion" "Invention" "Inversion"
           "Judgement" "Lies" "Light" "Locks" "Madness" "Memory" "Mirrors" "Mob Rule" "Mouths" "Music"
           "Mutation" "Outsiders" "Pageantry" "Paranoia" "Poison" "Priests" "Prophecy" "Rats" "Refugees" "Restless Dead"
           "Revenge" "Riches" "Rituals" "Rival Factions" "Sacrifice" "Savage Fury" "Secret Knowledge" "Serpents" "Shadows" "Skulls"
           "Slavery" "Slime" "Smoke" "Songs" "Souls" "Spiders" "Stasis" "Statues" "Summoning" "Survival"
           "Teeth" "Tentacles" "Tests and Trials" "The Moon" "The Night" "The Stars" "The Sun" "Thorns" "Trickery" "Tyranny"
           "Vampires" "Voids" "Water" "Whispers" "Wild Growth" "Wine" "Winter" "Wolves" "Worms" "Zealotry"))

(random-table/register :name "Knave > Divine Domain"
  :data '("Acid" "Alchemy" "Bees" "Beggars" "Betrayal" "Birds" "Blades" "Blood" "Blossoms" "Children"
           "Clay" "Clouds" "Commerce" "Courage" "Cowards" "Craftsmanship" "Crows" "Darkness" "Deserts" "Destruction"
           "Disease" "Doors" "Dreams" "Duels" "Eagles" "Earthquakes" "Fire" "Fish" "Forge" "Fungi"
           "Gluttony" "Gossip" "Greed" "Healing" "Horses" "Hunger" "Illusions" "Jealousy" "Language" "Lava"
           "Libraries" "Light" "Lightning" "Love" "Luck" "Machines" "Madness" "Mazes" "Miners" "Mirrors"
           "Mountains" "Murderers" "Music" "Oratory" "Performance" "Poison" "Priests" "Prisoners" "Rage" "Rain"
           "Revenge" "Revolution" "Roads" "Royalty" "Rust" "Salt" "Sand" "Secrets" "Serpents" "Silence"
           "Slaves" "Sleep" "Smoke" "Soldiers" "Spiders" "Swamps" "Tailors" "The Blind" "The Elderly" "The Future"
           "The Grave" "The Harvest" "The Hearth" "The Hunt" "The Law" "The Sea" "Thieves" "Thorns" "Travelers" "Trees"
           "Trickery" "Truth" "Tundra" "Tunnels" "Vermin" "Walls" "Wind" "Wine" "Winter" "Wolves"))

(random-table/register :name "Knave > Divine Symbol"
  :data '("Antlers" "Arrow" "Axe" "Bear" "Bell" "Bird" "Blood drop" "Book" "Boots" "Bow"
           "Bowl" "Branch" "Brazier" "Cauldron" "Chain" "Chariot" "Circle" "Cloud" "Coin" "Comb"
           "Constellation" "Crab" "Cross" "Crown" "Crystal" "Dagger" "Deer" "Diamond" "Dice" "Doll"
           "Eye" "Fangs" "Feather" "Fish" "Fist" "Flower" "Fountain" "Fox" "Frog" "Fruit"
           "Gate" "Goat" "Hammer" "Hand" "Heart" "Helmet" "Hook" "Horn" "Horse" "Hourglass"
           "Key" "Knot" "Lamp" "Leaf" "Lightning bolt" "Lock" "Mask" "Moon" "Nut" "Octopus"
           "Ox" "Pen" "Pincer" "Pine cone" "Planet" "Rabbit" "Rat" "Ring" "Roots" "Scorpion"
           "Scales" "Scroll" "Serpent" "Shield" "Sickle" "Skull" "Snail" "Snowflake" "Spear" "Spinning wheel"
           "Spiral" "Square" "Staff" "Star" "Sun" "Sword" "Talons" "Tentacle" "Throne" "Tooth"
           "Torch" "Tree" "Triangle" "Turtle" "Wave" "Web" "Whale" "Whip" "Wings" "Wolf"))

(random-table/register :name "Knave > Potion Effect"
  :data '("Telepathy"  "Telekinesis"  "Clairvoyance"  "True Poison"  "True Glue"  "True Acid"  "True Grease"  "Grow"  "Shrink"  "Healing"
           "Rot"  "Love"  "Hate"  "Rage"  "Fear"  "Joy"  "Paranoia"  "Prophecy"  "Courage"  "Invisibility"
           "Strength"  "Speed"  "Jumping"  "Climbing"  "Swimming"  "Intangibility"  "Forgetfulness"  "Petrification"  "Polymorph"  "Gills"
           "Webs"  "Claws"  "Long Tongue"  "Four Arms"  "Stinger"  "Zombie Blood"  "Vampirism"  "Burrowing"  "Cloudkill"  "Understanding"
           "Dark Vision"  "X-Ray Vision"  "Infravision"  "Ultravision"  "Sleep"  "False Life"  "Career"  "Grandeur"  "Gold Sense"  "Hearing"
           "Smelling"  "Taste"  "Eagle Vision"  "Micro Vision"  "Lycanthropy"  "Levitation"  "Flight"  "Gravity"  "Anti-Gravity"  "Repulsion"
           "Clone"  "Mutation"  "Raise Dead"  "Heartlessness"  "Silence"  "Loudness"  "Beast Speech"  "Bird Speech"  "Grub Speech"  "Dead Speech"
           "Fish Speech"  "Metal Speech"  "Plant Speech"  "Stone Speech"  "Hypnotism"  "Tongues"  "True Sight"  "Water Walk"  "Ventriloquism"  "Youth"
           "Age"  "Mold Stone"  "Mold Metal"  "Mold Flesh"  "Beast-form"  "Bird-Form"  "Fish-Form"  "Vermin-Form"  "Fire-form"  "Ice-form"
           "Gas-form"  "Ooze-form"  "Heat-Proof"  "Cold-Proof"  "Lightning Proof"  "Spell Proof"  "Fire-Breath"  "Ice-Breath"  "Acid-Breath"  "Lightning Breath"))

(random-table/register :name "Deity"
  :roller "1d6"
  :data '(((1 . 3) . "\n- Domains :: {Knave > Divine Domain} and {Knave > Divine Domain}\n- Symbol :: {Knave > Divine Symbol}")
           ((4 . 6) . "\n- Domains :: {Knave > Divine Domain}, {Knave > Divine Domain}, and {Knave > Divine Domain}\n- Symbol :: {Knave > Divine Symbol}")))

;; https://alldeadgenerations.blogspot.com/2019/07/time-risk-economy-part-ii.html
(random-table/register :name "Escape the Dungeon"
  :roller '(+ "1d10" "Number of Rooms from Any Entrance")
  :data '(
           (1 . "Escape without incident.")
           (2 . "Escape. Exhausted and harried, all party members begin next session with -1 HP per level.")
           (3 . "Escape. Forced to cache all treasure in a random room.")
           (4 . "Escape. Battered and injured by foes, hunger and thirst - 1d6 damage per PC (can kill).")
           (5 . "Escape. Only the intervention of some horrible old god or dark power allowed survival at the price of a grim pact to do its bidding or die.")
           (6 . "Escape. Permanently disturbed by the dungeon. -1 to all rolls on future expeditions within.")
           (7 . "Escape. Forced to drop all treasure gained due to terror and headlong flight")
           (8 . "Escape. You have no memory of reaching the entrance, but here you are naked and bruised.")
           (9 . "Escape. Stumble into the light with terrible wounds, all PCs 1d10 damage per level (can kill).")
           (10 . "Dead. Vanished utterly.")
           (11 . "Escape. The darkness is inside you now, and it’s screaming, all PCs lose 1D6 WIS.")
           (12 . "Escape, Injured Abandoned. PCs with injury must roll 4D6 vs. CON, those who roll over were abandoned wounded within the dungeon.")
           (13 . "Escape, Some Captured. Each PC must roll 4D6 v. INT, those that fail have been captured by the dungeon’s inhabitants.")
           (14 . "Captured. Entire party has been captured by one or more dungeon faction.")
           (15 . "Escaped, Some Dead. Each PC must roll a D100, any roll over current HP is Dead")
           (16 . "Dead, Lone Survivor. Each PC rolls a D20, PC with highest roll is lone survivor.")
           (17 . "Dead, Angry Dungeon. The party is dead, but the residents of the dungeon resent their intrusion and unify to attack any local haven.")
           (18 . "Dead, Haunting. The party is dead, but their spirits remain, angry and unsettled, haunting the dungeon as a new and dangerous faction.")
           (19 . "Dead, Hunting Spirits. While they have died in the dungeon, the uneasy spirits of the party now roam the land (as a wilderness random encounter) hunting adventurers and travellers.")
           ((20 . 100) . "Dead. The corpses of the party are lost within the dungeon and their supplies and equipment are within, abandoned or in the hands of the denizens.")))

;; https://www.drivethrurpg.com/product/308789/Castle-Xyntillan
(random-table/register :name "Random NPC Quirks"
  :data '("None"
           "{Random NPC Quirks > Data}"
           "{Random NPC Quirks > Data}  {Random NPC Quirks > Data}"))

(random-table/register :name "Random NPC Quirks > Data"
  :private t
  :data '("Has peg leg."
           "Owes 1d6*100 gp, tries to get sum by any means."
           "Drunkard."
           "Foul-mouthed."
           "Hunting dog: HD 1; AC 9 [10]; Atk bite 1d4; ML 9; AL N."
           "Pretends to know important secret."
           "Spy for Royal Tax Collectors."
           "Loyal to the last, ML +3."
           "Owns 1d3 potions."
           "Owns random magic item."
           "Shunned in civilisation."
           "Sharpshooter, +1 to ranged atk / damage."
           "Dishevelled appearance."
           "Picks teeth with dagger."
           "Uncanny talent for sniffing out alcohol."
           "Never surprised."
           "Can pick locks, has ring of false keys."
           "Shambling gait."
           "Unhealthy complexion."
           "Others have seen him die 1d3 times."
           "Cross-dresser."
           "Wears heirloom plate mail."
           "Very religious."
           "Never takes lead."
           "Has to be told everything twice."
           "Coward, ML -2."
           "Takes “sick leave” every other expedition."
           "Contrarian."
           "Delves into battle with cheerful “Huzzah!”, +1 to hit in first round."
           "Landed gentry, owns estate and small chateau, invites company after he gets through “these difficult times”."
           "Lackwit."
           "Always tries to press forward and pocket small valuables."
           "Steals from party if he can get away with it."
           "Spy for Royal Secret Police."
           "Good fashion-sense, spends all money on frivolities."
           "Gold teeth."
           "Golden heart."
           "Nervous, (1 in 6 chance) of skipping first round."
           "Strong, +1 damage."
           "Gambler."
           "Escaped convict."
           "Escaped friar/nun."
           "Lovestruck."
           "Obsessed with the secrets of the Underworld."
           "Heavy sleeper, never agrees to go on watch."
           "Bluffs about special abilities."
           "Bluffs about experience."
           "Modest about experience (+1 LVL)."
           "Artful dodger, -1 AC."
           "Skilled pickpocket."
           "Pet crow."
           "Hacking cough."
           "Ex-miner, good sense of direction, senses closest exit."
           "Meticulous, finds things others miss."
           "Equipped with dungeoneering gear: lantern, oil, coil of rope, hammer, spikes, iron rations, pole, and waterskin."
           "Stays behind and tends to get separated from company."
           "Panics in stressful situations (1 in 6 chance)."
           "Constantly begs company for a little extra."
           "Leaves to join rival company after first 1d3 expeditions, spills all."
           "Wants to form own company, encourages companions to join."
           "Keeps on going, +1 HD (to Hp only)."
           "Paranoid."
           "Libertine and free-thinker."
           "Staunch teetotaller."
           "Has heard rumours about location."
           "Adventures to care for sick relative."
           "Pretends to listen to orders, but always does own thing."
           "Owns random magic item."
           "Has own retainer paid out of his own pocket."
           "Always demands extra share from loot."
           "Pockets valuables when nobody is looking."
           "Spreads rumours about other companions."
           "Fat."
           "Expert at appraisal."
           "Skirt-chaser."
           "Outlaw."
           "Strikes twice in first round."
           "Leaves company in hazardous situations."
           "Proactive, tries to second-guess companions and act before they ask."
           "Secret nemesis (1-3 follows, 4-6 followed by)."
           "Obsessed by appearance, carries around box of perfumes and make-up."
           "Pet hawk."
           "Under vampire’s charm."
           "Party animal."
           "Binge-drinking on duty, -1 penalty, cumulative."
           "Never leaves a companion in peril, ML +2."
           "1d6*400 gp from past jobs."
           "Grumbler."
           "At the end of his wits, (1 in 6 chance) of berserk rage in critical situation, +2 to hit, but 1:3 attacks indiscriminately."
           "Escaped from the gallows."
           "Fencing instructor, 1d3*100 XP to one character on downtime after each expedition."
           "Wants to retire in style after pulling “that one big job”."
           "Pursues sworn enemy."
           "Has already been down there and lost most of his companions, -2 ML but knows a few places."
           "Scrounger, collects low-value items."
           "Outstanding warrant at constable."
           "Cynic."
           "Accursed."
           "Fanatical, +2 ML and +1 damage."
           "Princeling travelling incognito, LVL +2, departs after 1d3 expeditions with parting gift of 1d6*200 gp per companion."))

;; https://welshpiper.com/random-noble-houses/
(random-table/register :name "Noble House"
  :data (list (concat
                "\n- Atriarch Pronouns :: {Noble House > Atriarch Pronouns}"
                "\n- Atriarch Age :: {Noble House > Atriarch Age}"
                "\n- Alignment :: {Noble House > Alignment}"
                "\n- Influence :: {Noble House > Influence}"
                "\n- Holdings :: {Noble House > Holdings}"
                "\n- Relatives :: {Noble House > Relatives}"
                "\n- Recent Activity :: {Noble House > Recent Activity}"
                "\n- Current Ambition :: {Noble House > Current Ambition}"
                "\n- Closet Skeleton :: {Noble House > Closet Skeleton}")))

(random-table/register :name "Noble House > Atriarch Pronouns"
  :private t
  :data '("He/Him" "She/Her" "They/Them"))

(random-table/register :name "Noble House > Atriarch Age"
  :store t
  :data '((1 . "Young ({Noble House > Atriarch > Young Personality})")
           ((2 . 4) . "Middle-aged ({Noble House > Atriarch > Middle-aged Personality}})")
           ((5 . 6) . "Old ({Noble House > Atriarch > Old Personality}})")))

(random-table/register :name "Noble House > Atriarch > Young Personality"
  :private t
  :data '("naive" "bold" "confident" "fearful" "rash" "arrogant"))

(random-table/register :name "Noble House > Atriarch > Middle-aged Personality"
  :private t
  :data '("calculating" "assertive" "savvy" "cautious" "aggressive" "bullying"))

(random-table/register :name "Noble House > Atriarch > Old Personality"
  :private t
  :data '("cunning" "demanding" "prudent" "subtle" "detached" "domineering"))

(random-table/register :name "Noble House > Alignment"
  :private t
  :data '(((1 . 2) . "Law")
           ((3 . 5) . "Neutral")
           (6 . "Chaos")))

(random-table/register :name "Noble House > Influence"
  :private t
  :data '("None" "Scant" "Marginal" "Average" "Significant" "Considerable"))

(random-table/register :name "Noble House > Holdings"
  :private t
  :data '(((1 . 2) . "Modest ({1d10} 5-mile hexes)")
           ((3 . 5) . "Appreciable ({1d10 + 10} 5-mile hexes)")
           (6 . "Extensive ({1d10 + 10} 5-mile hexes)")))

(random-table/register :name "Noble House > Relatives"
  :private t
  :data '(((1 . 2) . "Small ({2d6} with claims to family resources)")
           ((3 . 5) . "Medium ({2d6} with claims to family resources)")
           (6 . "Large ({2d6 + 12} with claims to family resources)")))

(random-table/register :name "Noble House > Recent Activity"
  :private t
  :data '("Favoured by the king [staunch loyalty/the Midas touch/shrewd politicking]"
           "Achieved overwhelming military victory"
           "Achieved Pyrrhic military victory"
           "Supported a new vassal"
           "Driven out/slain marauding monster"
           "Returned after adventuring expedition to [local wilderness/king’s wilderness/foreign country]"
           "Captured [outlaws/spies/marauding monster/humanoid rabble]"
           "Brokered diplomatic agreement on king’s behalf [trade agreement/mutual defence pact/truce]"
           "Discovered valuable commodity [precious metal/industrial material/gemstones]"
           "Birth in the family"
           "Death in the family [natural causes/battle/accident/questionable circumstances]"
           "Wilderness within fief is plagued by [outlaws/wandering monster/refugees/humanoid band]"
           "Family member ransomed by [foreign enemy/outlaws/humanoids/unknown kidnapper]"
           "Participated in a duel [won/lost/draw]"
           "Afflicted by plague"
           "Adventuring family member(s) presumed lost or dead"
           "Losing money as a result of [stolen heirloom/bad business/raiders/rival noble/freak accident]"
           "Vassal settlement endangered by [attack/plague/low food supply/bandits]"
           "Suffered military defeat"
           "Snubbed by king for [poor military performance/late rents/causing trouble at court]"))

(random-table/register :name "Noble House > Current Ambition"
  :private t
  :data '("Support the king’s top priority plan [expansion/warfare/diplomacy/rooting out dissidents]"
           "Advance the [church/pet construction project/local commodities/military/population size]"
           "Acquire more land [strategic location/valuable resource/special feature]"
           "Dispose of [another noble/non-secular official/military officer/high-level bureaucrat/powerful adventurer]"
           "Marry into a particular family for [wealth/love/political advantage/lust/nefarious purposes]"
           "Bring about [political/economic/religious/judicial] reform"
           "Take a bite out of crime"
           "Build fortification [defensive wall/tower/keep/castle]"
           "Establish a new settlement"
           "Build infrastructure [road/bridge/watch tower/signal beacon/folly]"
           "Clear stain on family name"
           "Make a name for the family via [adventuring/exploring/amassing wealth/military conquest/political influence]"))

(random-table/register :name "Noble House > Closet Skeleton"
  :private t
  :data '("None (outwardly, things seem a little too perfect…)"
           "Engaged in treasonous activity with [independent actor/official body/foreign power/dissident element]"
           "Perpetrated and covered up a capital crime (e.g., murder, rape, arson)"
           "Under the enchantment of a [cursed item/spell caster/demon/demanding immortal]"
           "Fey creature"
           "Manipulates the system to avoid military service"
           "Supports a band of [outlaws/humanoids/organised thieves/dangerous mercenaries]"
           "Withholds the king’s fees to [amass personal hoard/divert funds/cover gambling debts/pay an extortionist]"
           "Sideline poacher [fish & game/natural resource/valuable commodity]"
           "Afflicted with lycanthropy (or similar disease)"
           "Family history of mental illness"
           "Member of [death cult/outlawed profession (e.g. sorcerer)/seditious faction]"
           "Addicted to [legal or widely available intoxicant/committing crimes/material possessions/exotic drug]"))

(random-table/register :name "Mortal Site"
  :data '("\n- What Is It :: {Mortal Site > What is it}\n- Who Is Here :: {Mortal Site > Who Is Here}"))

(random-table/register :name "Mortal Site > What Is It"
  :private t
  :data '("Campsite" "Hunting lodge" "Hermitage" "Church, monastery, or nunnery"
           "Workshop" "Farm" "Small fair" "Inn"
           "Forge" "Mill" "Woodcutter’s lodge" "Orphanage"
           "Pavilion" "Altar, statue, or shrine" "Road, path, or bridge" "Hamlet, village or towne"
           "Manse" "Tower or small keep" "Castle" "Disguised as {Mortal Site > What is it} but actually {Mortal Site > What is it}"))

(random-table/register :name "Mortal Site > Who Is Here"
  :private t
  :data '("Abandoned. No one is here; or roll on fey Who Is Here table (page 146)"
           "{1d6} explorers, surveyors, or cartographers"
           "{1d10} escaped slaves"
           "{1d4} knights and their retinues"
           "{1d6} poachers or outlaw loggers"
           "{4d4} bandits, footpads, or ne’er-do-wells"
           "A hermit"
           "{2d6} monks, nuns, or (gasp) both"
           "{1d4} runaways or lost children"
           "{1d20} poets, musicians, and/or artists"
           "{3d4} adventurers or fortune seekers"
           "{1d20} wizards, alchemists, and/or herbalists"
           "{1d20} settlers"
           "{1d6} prospectors or miners"
           "{1d20} revelers or lovers"
           "{1d4} witches, cabalists, or oracles"
           "{1d6} ghosts, wraiths, or revenants"
           "{2d6} nobles or aristocrats"
           "{1d4} monarchs and their retinues"
           "Disguised as {Mundane Role} but actually {Mortal Site > Who Is Here}"))

(random-table/register :name "Mundane Role"
  :private t
  :data '("[explorers/surveyors/cartographers]"
           "escaped slaves"
           "knights and their retinues"
           "[poachers/outlaw loggers]"
           "[bandits/footpads/ne’er-do-wells]"
           "hermit"
           "[monks/nuns/nuns and monks (gasp)"
           "[runaways/lost children]"
           "[poets/musicians/artists]"
           "[adventurers/fortune seekers]"
           "[wizards/alchemists/herbalists]"
           "settlers"
           "[prospectors/miners]"
           "[revelers/lovers]"
           "[witches/cabalists/oracles]"
           "[ghosts/wraiths/revenants]"
           "nobles or aristocrats"
           "witchfinders"
           "monarchs and their retinues"))

(random-table/register :name "Fey Site > What is it"
  :private t
  :data '("Maypole" "Bonfire" "Unicorn blind" "Tournament field"
           "Military outpost" "Market" "Fair" "Garden"
           "Mortal observation post" "Amphitheater" "Band shell" "Observatory"
           "Standing stones" "Circus" "Wildlife museum" "Mushroom library"
           "Holiday home (timeshare)" "Manse" "Castle" "Disguised as {Fey Site > What is it} but actually {Fey Site > What is it}"))

(random-table/register :name "Fey Site > Who is here"
  :private t
  :data '("Abandoned. No one is here; or roll on mortal Who Is Here table (page 144)"
           "{1d10} Seelies on a vision quest"
           "{1d10} Seelies banished from Faerie"
           "{1d6} Seelie aristocrats and their creature servants"
           "{1d6} Unseelie collectors of eyeballs"
           "{1d4} Unseelie trollriders"
           "A Seelie and an Unseelie trysting"
           "Unseelies spying on {1d10} Seelies having an important debate about something silly or a silly debate about something important"
           "{1d6} Seelie will-o’-the-wisp hunters"
           "A troupe of {1d10} Seelie actors that only perform Shakespearian death scenes"
           "A crowd of {1d20} bored Seelies waiting for something interesting to happen"
           "{1d10} Unseelie unicorn hunters"
           "{1d4} reformed Unseelies hoping to defect"
           "{1d4} Unseelie double agents meeting with their Seelie counterparts"
           "{2d6} mortal holy folk waiting to ambush fey folk, whom they consider unholy"
           "{1d4} mortal poets, musicians, and/or artists seeking out fey enchantments/inspiration"
           "A Seelie tournament ({1d100} in attendance)"
           "An Unseelie riot ({1d100} rioters)"
           "A royal procession of {1d100} Seelies"
           "Disguised as {Fey Site > Who is here} but actually {Fey Site > Who is here}"))

(random-table/register :name "In the Shadows of Mont Brun > In the stagecoach"
  :data '("{In the Shadows of Mont Brun > In the stagecoach > Adjective} {In the Shadows of Mont Brun > In the stagecoach > Person} {In the Shadows of Mont Brun > In the stagecoach > Activity}"))

(random-table/register :name "In the Shadows of Mont Brun > In the stagecoach > Adjective"
  :private t
  :data '("belligerent" "cold" "curious" "deceitful"
           "dying" "flirty" "hypochondriac" "indiscreet"
           "inquisitive" "jovial" "lustful" "naive"
           "obsessed" "pious" "quarrelsome" "ribald"
           "rueful" "scheming" "sickly" "taciturn"
           "talkative" "trusting" "worrisome" "wounded"))

(random-table/register :name "In the Shadows of Mont Brun > In the stagecoach > Person"
  :private t
  :data '("aristocrat" "charlatan" "child" "clergyman"
           "convict" "courtesan" "craftsman" "diplomat"
           "doctor" "friar" "gentleman" "lady"
           "maiden" "matron" "nun" "officer"
           "official" "rake" "secretary" "soldier"
           "student" "teacher" "underling" "vicar"))

(random-table/register :name "In the Shadows of Mont Brun > In the stagecoach > Activity"
  :private t
  :data '("running out of options" "in exile" "carrying important item" "fleeing from persecution"
           "fleeing from superiors" "fleeing heartbreak" "in adulterous affair" "on secret mission"
           "moving to relatives" "on hunting trip" "on pilgrimage" "on pleasure cruise"
           "on regular errand" "on state commission" "on wild goose chase" "pursued by enemies"
           "pursuing inheritance" "resolving family affairs" "seeking opportunities" "seeking trouble"
           "spying on company" "trying to forget" "untangling a mystery" "visiting relatives"))

(random-table/register :name "In the Shadows of Mont Brun > Names"
  :store t
  :data '("{German Name > Given Name > Masculine} {German Name > Family Name > General}"
           "{German Name > Given Name > Masculine} {German Name > Family Name > Swiss}"
           "{German Name > Given Name > Feminine} {German Name > Family Name > General}"
           "{German Name > Given Name > Feminine} {German Name > Family Name > Swiss}"
           "{Italian Name > Given Name > Masculine} {Italian Name > Family Name > General}"
           "{Italian Name > Given Name > Masculine} {Italian Name > Family Name > Swiss}"
           "{Italian Name > Given Name > Feminine} {Italian Name > Family Name > General}"
           "{Italian Name > Given Name > Feminine} {Italian Name > Family Name > Swiss}"
           "{French Name > Given Name > Masculine} {French Name > Family Name > General}"
           "{French Name > Given Name > Masculine} {French Name > Family Name > Swiss}"
           "{French Name > Given Name > Feminine} {French Name > Family Name > General}"
           "{French Name > Given Name > Feminine} {French Name > Family Name > Swiss}"))

(random-table/register :name "German Name > Given Name > Masculine"
  :private t
  :data '("Aaron" "Adolf" "Albert" "Albin" "Ambrosius" "Andreas" "Anselm" "Anton" "Augustus" "Balthazar"
           "Barnabas" "Benedict" "Bernd" "Bernhard" "Berthold" "Blasius" "Burkhard" "Casimir" "Christian" "Christoph"
           "Dietrich" "Eberhard" "Eilert" "Elias" "Engelbert" "Erasmus" "Erhard" "Ernest" "Eugene" "Ferdinand"
           "Franz" "Friedrich" "Gabriel" "Gaspar" "Gebhard" "Gedeon." "Gerhard" "Gernot" "Giinther" "Gottfried"
           "Gotthard" "Gotthelf" "Gottlieb" "Gotz" "Gregor" "Gregorius" "Gustav" "Heimpold" "Heinrich" "Helmut"
           "Hermann" "Hubert" "Ignatz" "Jakob" "Jan" "Jiirgen" "Joachim" "Jobst" "Johan" "Johannes" "Joist"
           "Jonathan" "Joseph" "Karl" "Klaus" "Konrad" "Lazarus" "Leopold" "Lorenz"
           "Ludwig" "Markus" "Matthias" "Maximillian" "Melchior" "Michael" "Moritz" "Nathaniel" "Nicodemus" "Otto"
           "Paul" "Richard" "Riidiger" "Rudolf" "Rupert" "Samuel" "Sebald" "Servatius" "Siegfried" "Sigismund"
           "Stephan" "Theobald" "Theodor" "Tobias" "Ulrich" "Volker" "Wendelin" "Werner" "Wilhelm" "Wilibald" "Zacharias"))

(random-table/register :name "German Name > Given Name > Feminine"
  :private t
  :data '("Abigail" "Adelheid" "Agatha" "Agnes" "Albrade" "Amalia" "Andrea" "Angela" "Angelica" "Anna"
           "Anneken" "Annelise" "Annemarie" "Astrid" "Aurelia" "Beatha" "Beatrix" "Bertha" "Bettina" "Birgit"
           "Brunnhilde" "Carolina" "Catharina" "Catrina" "Cecilia" "Charlotte" "Clara" "Edeltraud" "Eleonora" "Elisabeth"
           "Elsa" "Emilie" "Engell" "Erika" "Esther" "Eva" "Frida" "Friederica" "Helga" "Gerda"
           "Gertrude" "Greta" "Gretel" "Gude" "Gudrun" "Hedwig" "Heide" "Herta" "Hildegonde" "Hille"
           "Tlona" "Ilseke" "Ingrid" "Irma" "Irmel" "Kriemhilde" "Irmgard" "Isolde" "Johanna" "Julia"
           "Juliana" "Jutta" "Karoline" "Kristine" "Kunigunda" "Laura" "Lena" "Lisalotte" "Lisbeth" "Louisa"
           "Lucretia" "Lydia" "Lyse" "Margaretha" "Maria" "Magdalena" "Marianne" "Martha" "Mathilde" "Metten"
           "Metze" "Regina" "Ortrud" "Philippina" "Rike" "Rosa" "Rosamund" "Rosina" "Sabina" "Sara"
           "Sieglinde" "Sigrid" "Silke" "Susanna" "Theodora" "Theresia" "Ulrica" "Ursula" "Veronica" "Wilhelmina"))

(random-table/register :name "German Name > Family Name > General"
  :private t
  :data '("Albrecht" "Ammann" "App" "Bachmann" "Bader" "Bauer" "Baumann" "Beck" "Berger" "Braun."
           "Brunner" "Burkhard" "Dietrich" "Engelfrid" "Esslinger" "Felder" "Forster" "Frei" "Fuchs" "Furrer"
           "Ganz" "Geiger" "Gerach" "Gerber" "Giering" "Graf" "Grob" "Gross" "Gut" "Haas"
           "Hafliger" "Hafner" "Hartmann" "Hauser" "Hecht" "Herzog" "Hirt" "Hodel" "Kohler" "Hofer"
           "Holliger" "Hotz" "Huber" "Kammermann" "Kappeller" "Kessler" "Knecht" "Kunz" "Kiing" "Lang"
           "Laub" "Lehmann" "Maier" "Maurer" "Meister" "Richenbach" "Mettler" "Miiler" "Neuhaus" "Nusplinger"
           "Ortenberg" "Pfister" "Probst" "Ritter" "Roos" "Roth" "Schifer" "Schaub" "Schenk" "Scherer"
           "Schmid" "Schmidt" "Schneider" "Schuler" "Schumacher" "Schwarz" "Spielmann" "Spiefmacher" "Spreng" "Stalder"
           "Steiger" "Stocker" "Steiner" "Stilhartz" "Strub" "Studer" "Stiirzinger" "Tischmesser" "Vogel" "von Vilingen"
           "Walder" "Walter" "Wegmann" "Weiss" "Wicht" "Winkler" "Wirth" "Wolf" "Ziegler" "Zimmermann"))

(random-table/register :name "German Name > Family Name > Swiss"
  :private t
  :data '("Altherr" "Altenburger" "Attiger" "Baumgartner" "Binder" "Bircher" "Bissig" "Blatter" "Bléchlinger" "Bolliger"
           "Bosshard" "Bihler" "Dannacher" "Degen" "Dengler" "Dieth" "Dirler" "Eggler" "Eggwiller" "Ehrenzeller"
           "Eicher" "Engemann" "Epp" "Fehr" "Fritschi" "Frohlich" "Gebert" "Gerung" "Gfeller" "Gisler"
           "Glaus" "Glinz" "Gottlinger" "Greminger" "Gresch" "Grobli" "Gsell" "Gugelmann" "Hildbrand" "Gygax"
           "Hagenburger" "Halder" "Haltmeyer" "Hauselmann" "Hausknecht" "Heitz" "Herger" "Hofstetter" "Hirlimann" "Imhof"
           "Leu" "Locher" "Lutz" "Liischer" "Lithi" "Ort" "Maag" "Mattli" "Mehr" "Messerli"
           "Mettler" "Mittelholzer" "Miiller" "Rebmann" "Regli" "Reimann" "Riedweg" "Rietmann" "Rothmund" "Riigg"
           "Schaffner" "Scharer" "Schlatter" "Schlegel" "Schopfer" "Spéorri" "Stadelmann" "Stauffer" "Stéckli" "Stéssel"
           "Studer" "Truttmann" "Stutz" "Thommen" "Vogt" "Vollmar" "Vogeli" "Walder" "Wartmann" "Webrli"
           "Weniger" "Wettstein" "Weyermann" "Widmer" "Wipfli" "Wirth" "Wyrsch" "Wyss" "Zollinger" "Zweifel"))

(random-table/register :name "Italian Name > Given Name > Masculine"
  :private t
  :data '("Adamo" "Agnolo" "Agostino" "Alberico" "Aldo" "Alessandro" "Alessio" "Ambrogio" "Andrea" "Angelo"
           "Antonio" "Armando" "Arnaldo" "Arrigo" "Arturo" "Averardo" "Bartolomeo" "Bastiano" "Benedetto" "Benozzo"
           "Bertino" "Bindo" "Bruno" "Camillo" "Carlo" "Cesare" "Chimento" "Claudio" "Cristoforo" "Domenico"
           "Donato" "Ennio" "Enrico" "Ercole" "Ernesto" "Fabrizio" "Fausto" "Ferdinando" "Fiorentino" "Francesco"
           "Fulvio" "Gaetano" "Gasparo" "Giacinto" "Giacomo" "Giancarlo" "Gianfrancesco" "Gianluigi" "Gilberto" "Gioffre"
           "Giorgio" "Giovanni" "Girolamo" "Giuseppe" "Guglielmo" "Guido" "Jacopo" "Lazzaro" "Lodovico" "Lorenzo"
           "Luca" "Luciano" "Luigi" "Marcello" "Marco" "Mariano" "Mario" "Massimo" "Matteo" "Maurizio"
           "Mocenigo" "Moreno" "Niccolo" "Nunzio" "Orlando" "Ottavio" "Pandolfo" "Pasqua" "Pasquale" "Patrizio"
           "Pierluigi" "Pietro" "Silvestro" "Silvio" "Spinello" "Stefano" "Stoldo" "Taddeo" "Tommaso" "Ubaldo"
           "Uberto" "Ugo" "Umberto" "Virgilio"))

(random-table/register :name "Italian Name > Given Name > Feminine"
  :private t
  :data '("Adriana" "Agnella" "Agnese" "Albina" "Alessandra" "Alessia" "Annamaria" "Antonella" "Antonia" "Aurelia"
           "Barbara" "Beatrice" "Bella" "Bianca" "Camilla" "Carlotta" "Cassandra" "Cecilia" "Chiara" "Cristina"
           "Donatella" "Eleanora" "Elisabetta" "Euphemia" "Fabrizia" "Federica" "Fiametta" "Filippa" "Flavia" "Francesca"
           "Franceschina" "Francheta" "Gianna" "Ginevra" "Giovanna" "Giulia" "Giulietta" "Gloria" "Griselda" "Helena"
           "Ilaria" "Imelda" "Tolanda" "Ippolita" "Isabella" "Isotta" "Julia" "Laura" "Lauretta" "Letizia"
           "Lidia" "Livia" "Lorena" "Lorenza" "Loretta" "Lucia" "Lucrezia" "Luisa" "Madelena" "Marcella"
           "Maria" "Marietta" "Marissa" "Marta" "Martinella" "Melissa" "Monica" "Morena" "Morella" "Nezetta"
           "Monteleone" "Nina" "Olimpia" "Olivia" "Ornelia" "Paola" "Patrizia" "Pellitteri" "Paula" "Raffaella"
           "Regina" "Renata" "Sibilla" "Silvia" "Simona" "Simonetta" "Sophia" "Stella" "Valentina" "Valeria"
           "Verona" "Veronica" "Vincenza" "Vittoria"))

(random-table/register :name "Italian Name > Family Name > General"
  :private t
  :data '("Abagio" "Acerbi" "Acordolo" "Albarenno" "Albergati" "Albiso" "Augeri" "Avonal" "Babilonio" "Baldovinetti"
           "Barba" "Barbachollo" "Barbamachollo" "Barbazini" "Barberan" "Barbigia" "Barozzi" "Bellego" "Bellini" "Bernasconi"
           "Bianchi" "Bieri" "Bondimier" "Buglione" "Calandrini" "Calbi" "Calbo" "Calderara" "Caldogne" "Callocci"
           "Calvane" "Calvenzano" "Calza" "Canepa" "Contarini" "Corner" "Crivelli" "da Leze" "da Vigo" "de Abondiolis"
           "de Accora" "de Alberinis" "de Albiate" "de Calis" "de Calvino" "de Fidelis" "de Filago" "de Fino" "de Priuli" "de Rossi"
           "Donado" "Falier" "Farnese" "Filiarchi" "Fiolario" "Fioravanti" "Fiorino" "Fonte" "Foscarini" "Gioffre"
           "Giustiniani" "Gradenigo" "Grimaldi" "Grimani" "Gritti" "la Tella" "Lanzone" "Larocca" "Lonero" "Longo"
           "Morini" "Loredan" "Luppi" "Marcello" "Marchesani" "Marin" "Mariotti" "Porzio" "Mauroceno" "Mazzara"
           "Memo" "Menditto" "Riccoboni" "Roncone" "Rosso" "Scalera" "Spizega" "Tessitore" "Trevisan" "Vellucci"
           "Vetrone" "Vicelli" "Zambrano" "Zorzi"))

(random-table/register :name "Italian Name > Family Name > Swiss"
  :private t
  :data '("Alfieri" "Ambrosini" "Ballinari" "Balzari" "Barenco" "Beltrami" "Berla" "Bernardi" "Bernasocchi" "Bertina"
           "Besler" "Biasca" "Bodinoli" "Bonoli" "Bonzanigo" "Borghi" "Borgo-Caratti" "Brentini" "Bruga" "Bulloni"
           "Calanchini" "Capezzoli" "Carmine" "Cattaneo" "Cauzza" "Christofanini" "Cippa" "Cusa" "Delgrande" "Dolcini"
           "Farinelli" "Flori" "Gabani" "Galli" "Ghisla" "Gianini" "Giovanetti" "Grossi" "Guglielmini" "Guidi"
           "Guidinetti" "Laffranchi" "Laffranchini" "Lavizzari" "Lentini" "Leona" "Luis" "Lussi" "Malaguerra" "Maletti"
           "Mantelli" "Mariotti" "Martignoli" "Mazzucchelli" "Micheletti" "Minetta" "Minotti" "Molinari" "Morelli" "Moretti"
           "Muggiasca" "Nadi" "Negrini" "Odone" "Ostini" "Pacciorini" "Palla" "Paolini" "Pedotti" "Pedrazzoli"
           "Morosini" "Pellanda" "Pellini" "Peretti" "Pessi" "Pini" "Ponzio" "Querini" "Raspini" "Ravizza"
           "Re" "Respini" "Solari" "Taddei" "Taragnoli" "Tatti" "Tognacca" "Tonini" "Traversi" "Valentini"
           "Vanazzi" "Varrone" "Zanetti" "Zanini"))

(random-table/register :name "French Name > Given Name > Masculine"
  :private t
  :data '("Adenet" "Agénor" "Allain" "Ambrose" "Anselme" "Arnalt" "Aubin" "Bernard" "Bertin" "Blaise"
           "Bossu" "Bénoist" "Charles" "Claude" "Clément" "Cédric" "Denis" "Drouet" "Edmond" "Edouard"
           "Ernault" "Etienne" "Fabrice" "Felix" "Fernand" "Floquart" "Francois" "Frédéric" "Gaspard" "Gaston"
           "Gaultier" "Gerome" "Gilbert" "Gilet" "Gilles" "Guibert" "Guieffroy" "Guillaume" "Guiot" "Guy"
           "Gérard" "Heliot" "Hilaire" "Hiérome" "Honoré" "Huget" "Hugh" "Hughes" "Hugo" "Huguet"
           "Imbert" "Jacob" "Jacques" "Jacquet" "Jaquemin" "Jean" "Jean-XX" "Jean-XX" "Jean-XX" "Jean-XX"
           "Jehan" "Julien" "Justin" "Lazare" "Lothaire" "Louis" "Loys" "Luc" "Marc" "Marcel"
           "Mathelot" "Mathieu" "Maximilien" "Michel" "Médard" "Nicolaus" "Octave" "Odo" "Olivier" "Pascal"
           "Patrick" "Paul" "Perrin" "Philippe" "Pierre" "Raoul" "René" "Reynard" "Richart" "Robert"
           "Rogier" "Roland" "Rémy" "Simon" "Stéphane" "Theophile" "Victor" "Vincent" "Yves" "Yvonnet"))

(random-table/register :name "French Name > Given Name > Feminine"
  :private t
  :data '("Adele" "Agathe" "Agnes" "Alice" "Amélie" "Angélique" "Anne" "Annes" "Annette" "Arlette"
           "Aurélie" "Anne-Marie" "Belle" "Belsant" "Benedicta" "Béatrice" "Bénédicte" "Bertha" "Blanche" "Blanchefleur"
           "Brigitte" "Camille" "Cecilia" "Céline" "Charlotte" "Christine" "Claire" "Clara" "Claricia" "Clémence"
           "Colette" "Delphine" "Dominique" "Dorothée" "Edith" "Eleanor" "Eliane" "Elisabeth" "Elodie" "Emma"
           "Danielle" "Eugénie" "Eve" "Eva" "Fleurette" "Florine" "Francoise" "Geneviéve" "Géraldine" "Gillota"
           "Giselle" "Gisla" "Godelina" "Helena" "Heloise" "Hermine" "Isabel" "Jaqueline" "Jeanne" "Jeannette"
           "Joélle" "Jolene" "Julie" "Juliette" "Justine" "Leonore" "Lucie" "Madelaine" "Manon" "Marceline"
           "Margot" "Marianne" "Marie" "Marine" "Marie-Claude" "Maryse" "Matildis" "Michelle" "Monique" "Noémie"
           "Odette" "Odile" "Paulette" "Pauline" "Renée" "Sabine" "Simone" "Stéphanie" "Suzanne" "Sylvie"
           "Thérése" "Valérie" "Sévérine" "Véronique" "Violette" "Vivienne" "Ysoude" "Yvette" "Yvonne" "Zoé"))

(random-table/register :name "French Name > Family Name > General"
  :private t
  :data '("Favre" "Affré" "Allard" "Allemand" "Arceneaux" "Astier" "Bacque" "Barbet" "Baudet" "Beaumont"
           "Beauregard" "Badeux" "Besnard" "Bittencourt" "Blouet" "Berger" "Boissieu" "Bonhomme" "Bossuet" "Boulet"
           "Bourcier" "Bourgeois" "Brazier" "Bruneau" "Carpentier" "Carré" "Chapelle" "Choffard" "Clérisseau" "Cochin"
           "Cormier" "Coupain" "Courbet" "Crépin" "Cuvier" "D’Amboise" "de Périgon" "de Venteillon" "de Verley" "de Villepin"
           "Cortot" "Delannoy" "des Lys" "Delaplace" "Dimont" "Droz" "du Marais" "Duchemin" "Fouché" "Gachet"
           "Gérin" "Girardot" "Gounelle" "Granet" "Granier" "Grenier" "Jacquier" "Jeannin" "Kléber" "Lafaille"
           "Laffitte" "Lalande" "Lavaud" "Lazard" "le Chaudronnier" "le Morve" "Lecocq" "Leroux" "Levasseur" "Lévesque"
           "Levett" "Loup" "Maspier" "Mauvaissoir" "Masson." "Mazzal" "Ménétries" "Messier" "Moitessier" "Morand"
           "Piaget" "Picard" "Poussin" "Quint" "Renier" "Renou" "Rossignol" "Rouanet" "Rouzet" "Saint-Méran"
           "Saint-Yves" "Seyrés" "Rochette" "Thévenet" "Thibault" "Toussaint" "Trintignant" "Vaugrenard" "Vérany" "Villefort"))

(random-table/register :name "French Name > Family Name > Swiss"
  :private t
  :data '("Ausset" "Béchet" "Béranger" "Bérard" "Berdez" "Blanchenay" "Blanchet" "Borel" "Borgeaud" "Bossy"
           "Bressenel" "Boisot" "Burnat" "Carrard" "Chastellain" "Bridel" "Chauvet" "Chevallier" "Conod" "Correvon"
           "Creux" "Cuénoud" "Daccord" "de Blonay" "de Bons" "de Cerjat" "de Coppet" "de Crousaz" "de Dompierre" "de Joffrey"
           "de la Harpe" "de Morsier" "de Roguin" "Delajoux" "Delavigne" "Delisle" "Despland" "Destraz" "Doxat" "Duplessis"
           "de Loys" "Dupuget" "Duvoisin" "Duthon" "Fatio" "Favrat" "Fevot" "Fraisse" "Gantin" "Gaudard"
           "Gendroz" "Gilliard" "Grivel" "Groux" "Guex" "Henrioud" "Hugonnet" "Husson" "la Salle" "Lambert"
           "Leresche" "Maillard" "Mandrot" "Marcuard" "Maret" "Masset" "Mercier" "Michod" "Monnerat" "Monneron"
           "Monnier" "Morel" "Muret" "Nicollier" "Ney" "Panchaud" "Papon" "Paschoud" "Penel" "Péneveyre"
           "Perret" "Peytrignet" "Pilloud" "Reboul" "Régis" "Rosset" "Roulet" "Savary" "Sordet" "Specht"
           "Taillens" "Tapernoux" "Rossier" "Tarin" "Tornier" "Trincard" "Villard" "Villommet" "Vulliemin" "Willommet"))

(random-table/register :name "Savoy Region > Random Encounter"
  :roller "1d6"
  :data '(((1 . 3) . "{Savoy Region > Random Creature}")
           ((3 . 4) . "{Savoy Region > Random Creature} and {Savoy Region > Random Creature} [coming to blows/sharing a meal/conversing/bickering/conspiring/celebrating]")
           (5 . "{Savoy Region > Random Creature} pretending to be {Savoy Region > Mundane Role}")
           (6 . "{Savoy Region > Random Fantastical Creature}")))

(random-table/register :name "Savoy Region > Random Creature"
  :roller "1d6"
  :private t
  :data '((1 . "{1d6+3} [soldiers/brigands]")
           (2 . "{1d3} [Santémagîte/Catholic/Huegonot] [pilgrims/itenerant priests/monks]")
           ((3 . 4) . "[2/5/{1d3+1}/{2d4}/{1d20+1}/{1d100+1}] {Savoy Region > Mundane Role}")
           (5 . "1 [stage coach/merchant] and {1d3+1} guards")
           (6 . "{1d3+1} [Chaos cultists/Papal inquisitors] pretending to be {Savoy Region > Mundane Role}")))

(random-table/register :name "Savoy Region > Mundane Role"
  :private t
  :data '("[explorers/surveyors/cartographers]"
           "escaped slaves"
           "knights and their retinues"
           "[poachers/outlaw loggers]"
           "[bandits/footpads/ne’er-do-wells]"
           "[hermits/lepers]"
           "[monks/nuns/nuns and monks (gasp)]"
           "[runaways/lost children]"
           "[poets/musicians/artists]"
           "[adventurers/fortune seekers]"
           "[wizards/alchemists/herbalists/scholars]"
           "[settlers/roaming families]"
           "peddlers"
           "[prospectors/miners]"
           "[revelers/lovers]"
           "[witches/cabalists/oracles]"
           "[ghosts/wraiths/revenants]"
           "[nobles/aristocrats/courtiers]"
           "witchfinders"
           "monarchs and their retinues"))

(random-table/register :name "Mythical Castle Region Encounter"
  :data '("A mountain hermit out [basking naked/whispering prayers/singing a hedonistic song/crouched and munching on mushrooms]."
           "A now startled animal [foraging/hunting]."
           "Adventuring company [skulking towards/boisterously marching to/tattered and fleeing from/cautiously and wearly departing] the castle."
           "A [Papal/French/Holy Roman Empirian/regional] [company/individual/pair] [surveying/chasing/hunting/fleeing/seeking] something."
           "{1d6} of The Fox’s Brigand [found dead and mangled/lying in wait/lost/whispering around a cook fire]."
           "Something fantastical…{Mythical Castle Region Fantastical Encounter}"))

(random-table/register :name "Mythical Castle Region Fantastical Encounter"
  :data '("A manifestation of [Arioch/Mother Mary/Saint Boniface/Tambor/Alsidora] demanding [patronage/information/a sacrifice/help]."
           "A [treant/troll/ghost/troupe of goblins/Tatzelwurm/troupe of dwarfs]."
           "A goblin market [just opening/in full swing/closing down]."
           "A [coven of witches/trio of witches/lone witch] [performing a ritual/in flight/reciting epic poetry]."
           "A large boulder stirring and slowly shifting…as though waking from a long slumber."
           "[{4d6}/{3d20}] [skeletons/restless dead/apparations] marching to [bolster/exact revenge on] the castle."))


(random-table/register :name "Monster Overhaul > Reasons for Conflict"
  :private t
  :data '("Economic disparity" "Religious antipathy" "Local hostility" "Simmering rivalry" "Societal collapse"
           "Slow revenge" "Wild overconfidence" "Drunk or addled" "Outside influence" "General confusion"))

(random-table/register :name "Monster Overhaul > Obscure Motives"
  :private t
  :data '("Treasure division" "Academic bickering" "Legal requirement" "Death ritual" "Artistic disagreement"
           "Landscape upheaval" "Hasty sacred oath" "Fleeing something" "Brain parasites" "Divine command"))

;; Brian Eno's Oblique Strategies
(random-table/register :name "Oblique Strategy"
  :data '(
           "(Organic) machinery"
           "A line has two sides"
           "Abandon normal instruments"
           "Accept advice"
           "Accretion"
           "Allow an easement (an easement is the abandonment of a stricture)"
           "Always first steps"
           "Always give yourself credit for having more than personality"
           "Are there sections? Consider transitions"
           "Ask people to work against their better judgment"
           "Ask your body"
           "Assemble some of the instruments in a ngroup and treat the group"
           "Balance the consistency principle with the inconsistency principle"
           "Be extravagant"
           "Be less critical more often"
           "Blank cards - add your own!"
           "Breathe more deeply"
           "Bridges\n-build\n-burn"
           "Cascades"
           "Change instrument roles"
           "Change nothing and continue with immaculate consistency"
           "Children's voices\n-speaking\n-singing"
           "Cluster analysis"
           "Consider different fading systems"
           "Consult other sources"
           "Convert a melodic element into a rhythmic element"
           "Courage!"
           "Decorate, decorate"
           "Define an area as ‘safe’ and use it as an anchor"
           "Destroy\n-nothing\n-the most important thing"
           "Discard an axiom"
           "Disciplined self-indulgence"
           "Disconnect from desire"
           "Discover the recipes you are using and abandon them"
           "Distorting time"
           "Do nothing for as long as possible"
           "Do something boring"
           "Do the washing up"
           "Do the words need changing?"
           "Do we need holes?"
           "Don't be afraid of things because they're easy to do"
           "Don't be frightened of clichés"
           "Don't be frightened to display your talents"
           "Don't break the silence"
           "Don't stress one thing more than another"
           "Emphasize differences"
           "Emphasize repetitions"
           "Emphasize the flaws"
           "Faced with a choice, do both"
           "Feedback recordings into an acoustic situation"
           "Fill every beat with something"
           "From nothing to more than nothing"
           "Get your neck massaged"
           "Ghost echoes"
           "Give the game away"
           "Give way to your worst impulse"
           "Go outside. Shut the door."
           "Go slowly all the way round the outside"
           "Go to an extreme, move back to a more comfortable place"
           "Honor thy error as a hidden intention"
           "How would you have done it?"
           "Humanize something free of error"
           "Idiot glee"
           "Imagine the music as a moving chain or caterpillar"
           "Imagine the music as a set of disconnected events"
           "Imagine the piece as a set of disconnected events"
           "Incorporate."
           "Infinitesimal gradations"
           "Intentions\n-credibility of\n-nobility of\n-humility of"
           "Into the impossible"
           "Is it finished?"
           "Is the tuning appropriate?"
           "Is there something missing?"
           "It is quite possible (after all)"
           "Just carry on"
           "Listen in total darkness, or in a very large room, very quietly"
           "Listen to the quiet voice"
           "Look at a very small object, look at its centre"
           "Look at the order in which you do things"
           "Look closely at the most embarrassing details and amplify them"
           "Lowest common denominator check\n-single beat\n-single note\n-single riff"
           "Make a blank valuable by putting it in an exquisite frame"
           "Make a sudden, destructive unpredictable action; incorporate"
           "Make an exhaustive list of everything you might do and do the last thing on the list"
           "Mechanicalize something idiosyncratic"
           "Mute and continue"
           "Not building a wall but making a brick"
           "Once the search is in progress, something will be found"
           "Only a part, not the whole"
           "Only one element of each kind"
           "Overtly resist change"
           "Put in earplugs"
           "Question the heroic approach"
           "Remember those quiet evenings"
           "Remove ambiguities and convert to specifics"
           "Remove specifics and convert to ambiguities"
           "Repetition is a form of change"
           "Retrace your steps"
           "Revaluation (a warm feeling)"
           "Reverse"
           "Short circuit (example; a man eating peas with the idea that they will improve his virility shovels them straight into his lap)"
           "Shut the door and listen from outside"
           "Simple subtraction"
           "Simply a matter of work"
           "Spectrum analysis"
           "State the problem in words as clearly as possible"
           "Take a break"
           "Take away the elements in order of apparent non-importance"
           "Tape your mouth"
           "The inconsistency principle"
           "The most important thing is the thing most easily forgotten"
           "The tape is now the music"
           "Think of the radio"
           "Tidy up"
           "Towards the insignificant"
           "Trust in the you of now"
           "Turn it upside down"
           "Twist the spine"
           "Use an old idea"
           "Use an unacceptable color"
           "Use fewer notes"
           "Use filters"
           "Use unqualified people"
           "Water"
           "What are you really thinking about just now?"
           "What is the reality of the situation?"
           "What mistakes did you make last time?"
           "What would your closest friend do?"
           "What wouldn't you do?"
           "Work at a different speed"
           "Would anybody want it?"
           "You are an engineer"
           "You can only make one dot at a time"
           "You don't have to be ashamed of using your own ideas"))

(random-table/register :name "Lore24 Insipiration"
	:roller "2d4"
	:data '((2 . "History")
			     (3 . "Fantastical")
			     (4 . "Random Encounter")
			     (5 . "Local Effect")
			     (6 . "Downtime Trend")
			     (7 . "Monster")
			     (8 . "Covenant")))

(random-table/register :name "Ironsworn > Action Oracle"
  :data '("Scheme" "Clash" "Weaken" "Initiate" "Create" "Swear" "Avenge" "Guard" "Defeat" "Control"
           "Break" "Risk" "Surrender" "Inspect" "Raid" "Evade" "Assault" "Deflect" "Threaten" "Attack"
           "Leave" "Preserve" "Manipulate" "Remove" "Eliminate" "Withdraw" "Abandon" "Investigate" "Hold" "Focus"
           "Uncover" "Breach" "Aid" "Uphold" "Falter" "Suppress" "Hunt" "Share" "Destroy" "Avoid"
           "Reject" "Demand" "Explore" "Bolster" "Seize" "Mourn" "Reveal" "Gather" "Defy" "Transform"
           "Persevere" "Serve" "Begin" "Move" "Coordinate" "Resist" "Await" "Impress" "Take" "Oppose"
           "Capture" "Overwhelm" "Challenge" "Acquire" "Protect" "Finish" "Strengthen" "Restore" "Advance" "Command"
           "Refuse" "Find" "Deliver" "Hide" "Fortify" "Betray" "Secure" "Arrive" "Affect" "Change"
           "Defend" "Debate" "Support" "Follow" "Construct" "Locate" "Endure" "Release" "Lose" "Reduce"
           "Escalate" "Distract" "Journey" "Escort" "Learn" "Communicate" "Depart" "Search" "Charge" "Summon"))

(random-table/register :name "Ironsworn > Theme Oracle"
  :data '("Risk" "Ability" "Price" "Ally" "Battle" "Safety" "Survival" "Weapon" "Wound" "Shelter"
           "Leader" "Fear" "Time" "Duty" "Secret" "Innocence" "Renown" "Direction" "Death" "Honor"
           "Labor" "Solution" "Tool" "Balance" "Love" "Barrier" "Creation" "Decay" "Trade" "Bond"
           "Hope" "Superstition" "Peace" "Deception" "History" "World" "Vow" "Protection" "Nature" "Opinion"
           "Burden" "Vengeance" "Opportunity" "Faction" "Danger" "Corruption" "Freedom" "Debt" "Hate" "Possession"
           "Stranger" "Passage" "Land" "Creature" "Disease" "Advantage" "Blood" "Language" "Rumor" "Weakness"
           "Greed" "Family" "Resource" "Structure" "Dream" "Community" "War" "Portent" "Prize" "Destiny"
           "Momentum" "Power" "Memory" "Ruin" "Mysticism" "Rival" "Problem" "Idea" "Revenge" "Health"
           "Fellowship" "Enemy" "Religion" "Spirit" "Fame" "Desolation" "Strength" "Knowledge" "Truth" "Quest"
           "Pride" "Loss" "Law" "Path" "Warning" "Relationship" "Wealth" "Home" "Strategy" "Supply"))

(random-table/register :name "Laws of the Land"
  :data '("The land is {Laws of the Land > Nature's Nature} and {Laws of the Land > Nature's Nature}; and you must not {Laws of the Land > You Must Not} nor {Laws of the Land > You Must Not}"))

(random-table/register :name "Laws of the Land > Nature's Nature"
  :private t
  :data '("forested" "stony" "marshy" "humid" "foggy"
           "arid" "freezing" "volcanic" "windswept" "flowering"
           "steep" "riverside" "grassy" "fungal" "hilly"
           "flat" "oceanic" "freshwater" "sunken" "fruited"))

(random-table/register :name "Laws of the Land > You Must Not"
  :private t
  :data '("bring [animals or livestock/food/liquid]"
           "kill [plants/animals/people/anything]"
           "be quiet"
           "create [noise/light/fire]"
           "look at anything"
           "listen to anything"
           "stand [close to one another/on the ground]"
           "go off on your own"
           "stop traveling"
           "leave the trail"
           "travel [quickly/slowly]"
           "make sudden movements"
           "leave a trail"
           "stand upright"
           "fall"
           "speak [about the past/about the future/even a word]"
           "sleep"
           "breathe the unfiltered air"
           "drink water from here"
           "expose [skin/metal/leather/wood/blood/paper]"))

;;; Holy Roman Empire using Cepheus Engine
(dolist (ability '("HRE > Ability > STR"
                    "HRE > Ability > DEX"
                    "HRE > Ability > END"
                    "HRE > Ability > INT"))
  (random-table/register :name ability
    :store t
    :private t
    :reuse ability
    :roller "2d6"
    :data '(2 3 4 5 6 7 8 9 10 11 12)))

(random-table/register :name "HRE > Ability > SOC"
  :roller "2d6"
  :reuse "HRE > SOC"
  :store t
  :data '((2 . "{CURRENT_ROLL}\tUnclean")
           ((3 . 5) . "{CURRENT_ROLL}\tPeasant: Agriculture, Archery, Driving, Grappling, Watercraft, Survival.")
           ((6 . 7) . "{CURRENT_ROLL}\tTownsman: Carousing, Driving, Grappling, Streetwise, Trade (select), Watercraft.")
           ((8 . 9) . "{CURRENT_ROLL}\tBürgher: EDU + 1, Carousing, Fencing, Firearms, Streetwise, Statecraft, Trade (select).")
           ((10 . 11) . "{CURRENT_ROLL}\tGentry: EDU +1, Agriculture, Etiquette, Fencing, Language (Latin), Survival, Statecraft, Riding.")
           ((12 . 20) . "{CURRENT_ROLL}\tNobility: EDU +1, Art (any), Etiquette, Fencing, Language (Latin), Persuasion, Statecraft, Riding.")))

(random-table/register :name "HRE > Ability > SOC > Modifier to EDU"
  :reuse "HRE > SOC"
  :private t
  :exclude-from-prompt t
  :filter (lambda (&rest dice) (if (>= (car dice) 8) 0 1))
  :data '(0 1))

(random-table/register :name "HRE > Ability > EDU"
  :roller '(+ "2d6" "HRE > Ability > SOC > Modifier to EDU")
  :reuse "HRE > Ability > EDU"
  :store t
  :private t
  :data '(((2 . 7) . "{CURRENT_ROLL}\tSimple cultural knowledge.  Illiterate.")
           (8 . "{CURRENT_ROLL}\tBasic Grammar, Arithmetic, and Literacy.  Attended civic, private, or religious primary school; literate in native language.")
           (9 . "{CURRENT_ROLL}\tLatin, Grammar, Logic, Rhetoric.  University student to Bachelor of Arts (+2 Starting Age). Character is literate in any language known.")
           (10 . "{CURRENT_ROLL}\tArithmetic, Geometry, Music, Astronomy, Metaphysics, Occult.  Master of Arts (+4 Starting Age) Completion of University. Can earn money tutoring.")
           (11 . "{CURRENT_ROLL}\tLaw, Theology, Natural Sciences, Moral Philosophy, Philology.  Doctorate (+6 Starting Age). Begins specializing in a scholarly trade.")
           ((12 . 20) . "{CURRENT_ROLL}\tSpecialization in previous subjects.  May indicate a Rector or Chair position and a University staff position (+8 Starting Age).")))

(random-table/register :name "HRE"
  :data '("\n- STR :: {HRE > Ability > STR}\n- DEX :: {HRE > Ability > DEX}\n- END :: {HRE > Ability > END}\n- INT :: {HRE > Ability > INT}\n- EDU :: {HRE > Ability > EDU}\n- SOC :: {HRE > Ability > SOC}"))

(random-table/register :name "HRE > Contact"
  :data '("Kacker" "Thief" "Hanse merchange" "Barber-surgeon" "Musician" "Apothecary"
           "Executioner" "Cunning folk" "Beggar" "Ship's captain" "Scholar" "Military quartermaster"
           "Village schultz" "Hunter" "Monk/nun/friar" "Herbalist" "Lawyer" "Alchemist"
           "Craftsman" "Gang boss" "Caravan guard" "Laborer" "Brigand" "Prostitute"
           "Priest" "Midwife" "Bürghermeister" "Military officer" "Spy" "Magistrate"
           "Wealthy großbürger" "Courtier" "Actor" "Duke's chamberlain" "Bishop" "Guild master"))

(random-table/register :name "HRE > Random Encounter > Wilderness"
  :data '("Overgrown Village or Castle" "Bear with {1D6-3} cubs" "Forester thinks PCs poaching"
           "{1d6} Smugglers" "{1d6} Deer" "Merchant Caravan with {3d6} people"
           "{1d6} Brigand Camp" "{1d6} Wolves" "{2d6} Refugees asking alms"
           "Ambush by {1d6} Brigand" "{1d6} Wandering Lepers" "{1d3} Patch of useful herbs"
           "Snare, pit, or other trap" "Logger or Charcoaler Camp" "Boar"
           "{1d6} Pilgrims" "{1d6} Slain Bodies" "Lynx on the prowl"
           "Religious Hermit" "Caravan Under Attack" "Royal hunting party"
           "Räubreiter collecting tolls" "Peasants clearing land" "Impassable vegetation"
           "Shepherd with flock" "{1d3} Poachers" "Knight Errant seeking a duel"
           "Ancient pagan cairn or altar" "Heretics worshiping in secret" "A beautiful, peaceful glade"
           "Venomous Snake" "{1d6} Foreign soldiers raiding" "Mad Prophet"
           "{HRE > Random Encounter > Wilderness} and {HRE > Random Encounter > Wilderness}"
           "{HRE > Random Encounter > Wilderness} and {HRE > Random Encounter > Wilderness}"
           "{HRE > Random Encounter > Wilderness} and {HRE > Random Encounter > Wilderness}"))

(random-table/register :name "HRE > Random Encounter > Wetlands or Coastal"
  :data '("Sunken boat with skeleton" "Blinding fog for {2d6} hours" "{1d6+2} Wreckers looting ship"
           "Hunter’s Camp" "Venomous Snake" "Merchant river boats {1d6}"
           "Boar {1d6}" "Ambush by {1d6} Vagabond" "Cunning Folk Cottage"
           "Peasants draining wetland/bog" "{1d6} Patch of useful herbs" "{1d6} Fisherman"
           "{1d6} Deer" "Nest with {1d6} eggs" "Vagabond Camp {2d6}"
           "Pilgrims {1d6}" "Quicksand" "Pirate Camp {2d6}"
           "Refugee Camp {3d6}" "Fishing Encampment {2d6}" "Mendicant Monk preaching"
           "Gallows Tree & {1d6} Bodies" "{1d6} Wolves on the hunt" "{1d6} Black Sabbath \"witches\""
           "Hunting Party/Fowlers {1d6}" "{1d3} Rotting Corpses " "Flash Flood"
           "Lynx on the hunt" "{1d6} Brigand Ambush" "{1d6} Smugglers"
           "Sailor repairing boat" "{1d6} Foreign soldiers raiding" "Recently killed reiter w/horse"
           "{HRE > Random Encounter > Wetlands or Coastal} and {HRE > Random Encounter > Wetlands or Coastal}"
           "{HRE > Random Encounter > Wetlands or Coastal} and {HRE > Random Encounter > Wetlands or Coastal}"
           "{HRE > Random Encounter > Wetlands or Coastal} and {HRE > Random Encounter > Wetlands or Coastal}"))

(random-table/register :name "HRE > Random Encounter > Urban"
  :data '("Public execution" "Fire! Fire!" "Sewage leak"
           "Plague outbreak!" "Fechtschule / Carnival" "A fiery heretic preacher"
           "Convicts being whipped/exiled" "Pack of {1d6} mongrels attack!" "Urchins swarm the PCs"
           "Guild protests in street" "City Watch chasing a thief" "Street performers"
           "{2d6} Beggars accost party" "Pickpocket strikes!" "Swarm of beggars accost party"
           "Controversial pamphlets" "Priest selling indulgences, 1fl" "Religious procession"
           "Gang fight" "Bear baiting, place your bets" "Out of control horse charges!"
           "{1d6} Muggers attack" "Herald makes announcements" "Apprentice/student street brawl"
           "A PC is challenged to a duel" "Accosted by the watch" "Bear escapes from dog fight!"
           "Public execution" "Clergy demands donations" "Jester begins mocking party"
           "Market day" "Shops closed for local festival" "Royal Procession"
           "{HRE > Random Encounter > Urban} and {HRE > Random Encounter > Urban}"
           "{HRE > Random Encounter > Urban} and {HRE > Random Encounter > Urban}"
           "{HRE > Random Encounter > Urban} and {HRE > Random Encounter > Urban}"))

(random-table/register :name "Errant > Alignment"
  :roller "2d6"
  :data '((2 . "C3") (3 . "C2") (4 . "C1")
           ((5 . 8) . "N")
           ((9 . 10) . "L1") (11 . "L2") (12 . "L3")))

(random-table/register :name "Street Name"
  :data '("Alms Drinker Way" "Avenue of Sweet-Smells" "Be Late Way" "Bear-Baiter Lane" "Bee Gardens"
           "Belly-Knot Well" "Bent Nail Walk" "Bent Spear" "Black Eye Junction" "Bleak Heights"
           "Bleeder's Trace" "Bleeg's Cut Off" "Blind Fork" "Blister Branch" "Bloody Block"
           "Bloody Gums" "Blue Lip Tunnel" "Blue Stain Lane" "Blunt Knife Arcade" "Boulevard of Worms"
           "Bow Street" "Breakneck Run" "Brewer's Yard" "Bride's March" "Bright Pass"
           "Broken Breeches" "Bully's Alley" "Cager's Course" "Candle Heights" "Carter's Course"
           "Chainer's Way" "Char-Church Road" "Charity Row" "Cheater's Crossing" "Chopping Block"
           "Clang Street" "Clean Court" "Clipper's Way" "Cloud Alley" "Coal Smudge Street"
           "Cobble Pock Circle" "Cooper Lane" "Cricket Close" "Crockshard Way" "Crookhaven"
           "Cutler's Bridge" "Dead Canary" "Dead Haft Road" "Din Street" "Dipper's Drive"
           "Dredger's Rest" "Drink-Not Fountain" "Drunkard's Shrine" "Dump Street" "Dusty Rug"
           "Elbow Street" "Fat Bishop" "Filch Street" "Fletcher's Lane" "Foul Well Square"
           "Foundling Circle" "Four Finger Yard" "Full Basket Mews" "Gaol Lane" "Gavel's Gate"
           "Gill Market" "Goat Walk" "God-Stumper's Court" "Goodspring Way" "Green Corner"
           "Grifter's Alley" "Grinner's Wynd" "Groaning Rill" "Guild's Crossing" "Gutter's Rill"
           "Hair Shirt Close" "Hang-Basket Bridge" "Hanger's Row" "Hangover's Walk" "Hen Run"
           "Herb Monger's Way" "Hero's Walk" "Hide Blade Viaduct" "Highjack Way" "Holloway Passage"
           "Horse's Ferry" "Hound Alley" "Ink Drop Passage" "Jester's Knoll" "Jingler's Road"
           "Killer's Landing" "King's Thorn" "Knocker's Lodge" "Last Bell Yard" "Last Loaf Walk"
           "Last Queue" "Lean Hollow" "Leaper's Bridge" "Left Spur" "Leper Way"
           "Linen Shade" "Little Pearl Bend" "Loudkennel Street" "Lover's Lament" "Lower Marsh"
           "Mare Bridge" "Martyr's Wall" "Mender's Road" "Midden Flinger Square" "Mill Stench Street"
           "Mind-yer-Purse Lane" "Mire Gate" "Mouse Bolt" "New Dam Avenue" "New Mead"
           "Nit Scratcher's Road" "No-Nose Place" "Old Boot" "Olde Common" "Page's Promenade"
           "Parade Street" "Parade of Veils" "Pinch-Nose Road" "Piper's Place" "Porter's Avenue"
           "Priest Stabber's Alley" "Queen's Crossing" "Quick Branch" "Rag Picker Way" "Rake's Drag"
           "Ravenson Road" "Red Mud Road" "Red Whistler Way" "Reeker's Row" "Remnant Gate"
           "Rip-Off Way" "Ripe Pickings" "Roaster's Plaza" "Rooster's Run" "Rope Walk Street"
           "Rue of Masons" "Rumpsore Street" "Scab Street" "Scraper's Stink" "Scrivener's Bend"
           "Seeker's Circle" "Seven Inns" "Shambles" "Sheep's Bend" "Shout Street"
           "Silk Mews" "Skint Haven" "Slit Street" "Snake Walk" "Snuff Lamp"
           "Sour Mouth Circle" "Sour Wall" "Spider Alley" "Spur Shot Close" "Squab's Rest"
           "Stabler's Way" "Stag Commons" "Star Gazer Circle" "Steeple Fall" "Stifle-Song"
           "Stinker's Square" "Stumbler's Passage" "Sullied Cap" "Sweeper's Square" "Swindler's Run"
           "Swine Gong" "Taxer's Well" "Ten Prophets" "The Chain" "The Great Gap"
           "The Horn" "The Old Baths" "The Strangles" "Three Cups" "Thug Street"
           "Thumb-Scale Street" "Tidy's Trench" "Tomb Street" "Tomcat Promenade" "Tooth-Ache Alley"
           "Traitor's Row" "Trimmer's Mall" "Tunnel of Shills" "Under Route" "Vigil Street"
           "Vine Street" "Washer's Square" "Waterlung Ferry" "Weaver's Way" "Welcome Alley"
           "Wheel Bobber's Trace" "Wheel-Killer Road" "Whip Lane" "Winder" "Yammerer's Fork"))

(random-table/register :name "Street Feature"
  :data '("Adapted Aqueduct" "Adversarial Advertising" "Arabesque Arches" "Assassinates Axles" "Belligerent Beggars"
           "Bisected Boneyard" "Bollard Bordered" "Brittle Bridges" "Broad Breezewalks" "Burnt Bricks"
           "Cantankerous Canines" "Claustrophobically Close" "Confident Crows" "Convenient Canals" "Courteous Commuters"
           "Criminally Contested" "Crooked Crossings" "Crossroad Crime-Scene" "Croupy Crowds" "Crowding Cobbles"
           "Cut-throat Curves" "Deceiving Signs" "Deceptive Descent" "Decorated Doors" "Delaying Detours"
           "Distracting Dust" "Fancy Façades" "Ferocious Feuds" "Flung Fish" "Flustered Ferryman"
           "Frequent Furrows" "Frustrating Floods" "Gorgeous Graffiti" "Grooved Granite" "Grueling Grades"
           "Gurgling Gutters" "Hasty Hair-Pins" "Healthy Hedges" "Horse Hobbling Holes" "Hurried Hoofprints"
           "Inadequate Intersections" "Jarringly Jammed" "Jettisoned Junk" "Lichen-licked Limestone" "Linden-Lined Lanes"
           "Lithe Lamp-Posts" "Litter Lined" "Lousy with Loiterers" "Lovingly Laid" "Low-Hanging Laundry"
           "Manky Manure" "Marauding Mule" "Marred Marble" "Muddy Morass" "Naggingly Narrow"
           "Ominous Overhangs" "Painfully Packed" "Panicked Poultry" "Pant-Ruining Puddles" "Patiently Patrolled"
           "Petulant Pedestrians" "Poorly Planned" "Porous Pavers" "Proliferating Pot-holes" "Protracted Processions"
           "Pushy Peddlers" "Quirkily Quiet" "Recently Re-Paved" "Repurposed Rooftiles" "Ribald Ribbons"
           "Rickety Ramps" "Right-of-Way for Rats" "Rough Roots" "Rowdy Roundabout" "Rueful Repairs"
           "Scrumptious Smells" "Scrupulously Swept" "Second-Story Splashes" "Shadow Shrouded" "Sheltered Sidewalks"
           "Showy Shrines" "Shrill Shopkeepers"  "Slapdash Slats" "Sneaky Sinkhole" "Staccato Stops"
           "Stampeding Swine" "Stately Statues" "Steep Slopes" "Stingy with Stones" "Street-Fair Stricken"
           "Stubbornly Straight" "Thoroughly Thronged" "Tolerably Tolled" "Torturously Tight" "Troublesome Tree Trunks"
           "Truculent Traffic" "Urchins Underfoot" "Weakened by Weeds" "Welcomingly Wide" "Zealous Zig-Zags"))

;; (random-table/register :name "Black Dogs > Adventure > Location"
;;   :data '(((1 . 3) . "Traditional Farming")
;;            ((4 . 5) . "An important crossroad")
;;            ((6 . 7) . "Amidst ancient ruins")
;;            (8 . "On a remote mountain peak")
;;            ((9 . 11) . "Fisherman and boatman")
;;            ((12 . 13) . "Something huge (church, statue, etc)")
;;            ((14 . 15) . "Too close to the woods")
;;            (16 . "Built on a holy place for The Wild")
;;            ((17 . 19) . "Interbreeding families")
;;            ((20 . 21) . "Ancient druidic traditions")
;;            ((22 . 23) . "Church of a saint with relics")
;;            (24 . "Humans twisted The Wild")
;;            ((25 . 27) . "Religion enforced with violence")
;;            ((28 . 29) . "People have strong (true?) visions")
;;            ((30 . 31) . "Pagan temple still in use")
;;            (32 . "Next to a large river")
;;            ((33 . 35) . "Thriving market")
;;            ((36 . 37) . "Formerly important, now in decay")
;;            ((38 . 39) . "Populated by exiles and refugees")
;;            (40 . "Famine, or drought, or plague")
;;            ((41 . 43) . "Important merchants and nobles")
;;            ((44 . 45) . "Uprising nobility and mercenaries")
;;            ((46 . 47) . "Nest of new heresy udsed politically")
;;            (48 . "Capital city and dying king")))

;; (random-table/register :name "Black Dogs > Adventure > Feature"
;;   :data '(((1 . 3) . "Traditional farming")
;;            ((4 . 5) . "An important crossroad")
;;            ((6 . 7) . "Amidst ancient ruins")
;;            (8 . "On a remote mountain peak")
;;            ((9 . 11) . "Fisherman and boatman")
;;            ((12 . 13) . "Something huge (church, statue, etc)")
;;            ((14 . 15) . "Too close to the woods")
;;            (16 . "Built on a holy place for The Wild")
;;            ((17 . 19) . "Interbreeding families")
;;            ((20 . 21) . "Ancient druidic traditions")
;;            ((22 . 23) . "Church of a saint with relics")
;;            (24 . "Humans twisted by The Wild")
;;            ((25 . 27) . "Religion enforced with violence")
;;            ((28 . 29) . "People have strong (true?) visions")
;;            ((30 . 31) . "Pagan temple still in use")
;;            (32 . "Next to large important river")
;;            ((33 . 35) . "Thriving market")
;;            ((36 . 37) . "Formerly important, now in decay")
;;            ((38 . 39) . "Populated by exiles and refugees")
;;            (40 . "Famine, or drought, or plague")
;;            ((41 . 43) . "Important merchants and nobles")
;;            ((44 . 45) . "Uprising nobility and mercenaries")
;;            ((46 . 47) . "Nest of heresy used politically")
;;            (48 . "Capital city and dying king")))

;; (random-table/register :name "Black Dogs > Adventure > Trouble"
;;   :data '(((1 . 3) . "Abuse of the land")
;;            ((4 . 5) . "Need of stability")
;;            ((6 . 7) . "Lack of prudence")
;;            (8 . "Corruption (influence)")
;;            ((9 . 11) . "Some sort of fear")
;;            ((12 . 13) . "An old grudge")
;;            ((14 . 15) . "Inappropriate lust")
;;            (16 . "Need of money")
;;            ((17 . 19) . "Greed for power")
;;            ((20 . 21) . "Revenge")
;;            ((22 . 23) . "Abuse of drugs")
;;            (24 . "Greed of prestige")
;;            ((25 . 27) . "Fear; lust for money")
;;            ((28 . 29) . "Disposition to abuse")
;;            ((30 . 31) . "Demon's corruption")
;;            (32 . "Defense neglected")
;;            ((33 . 35) . "Seeing approval")
;;            ((36 . 37) . "Blackmailing/mailed")
;;            ((38 . 39) . "Wrong decision(s)")
;;            (40 . "Dirty secret(s)")
;;            ((41 . 43) . "Need for freedom")
;;            ((44 . 45) . "Need for more")
;;            ((46 . 47) . "Part of a cult")
;;            (48 . "Seeking company")))

(random-table/register :name "Mausritter > Mouse Given Name"
  :data '("Ada" "Agate" "Agnes" "Aloe" "April" "Azalea" "Bay" "Belladonna" "Blossom" "Brie"
           "Brynn" "Cherry" "Claire" "Crocus" "Dahlia" "Daisy" "Else" "Emerald" "Erin" "Grace"
           "Gwendoline" "Hazel" "Heather" "Hette" "Holly" "Hyacinth" "Iris" "Juniper" "Lavender" "Lily"
           "Magnolia" "Marigold" "Marjoram" "Myrtle" "Odette" "Olive" "Opal" "Pearl" "Pepper" "Poppy"
           "Rosemary" "Rue" "Saffron" "Sandy" "Sassafras" "Shale" "Susan" "Thistle" "Violet" "Willow"
           "Alder" "Ambrose" "Anise" "Annotto" "August" "Avens" "Basil" "Beryl" "Birch" "Boldo"
           "Bill" "Burdock" "Butter" "Cassia" "Chicory" "Clive" "Colby" "Dill" "Dock" "Eared"
           "Edmund" "Elmer" "Ernest" "Fennel" "Festus" "Francis" "Gil" "Hawthorn" "Heath" "Horatio"
           "Jack" "Jasper" "Konrad" "Larkspur" "Laurel" "Lorenz" "Mace" "Oliver" "Orin" "Reepicheep"
           "Rowan" "Simon" "Sorrel" "Stilton" "Tarragon" "Warren" "Wattle" "Whitacre" "Wormwood" "Yarrow"))

(random-table/register :name "Mausritter > Mouse Family Name"
  :data '("Baiter" "Black" "Buckthorne" "Burley" "Butterball" "Catreizen" "Danger" "Deerider" "Grant" "Halva"
           "Maker" "Pipp" "Seedfall" "Snow" "Summerholme" "Thorne" "Tunneler" "White" "Winterholme" "Witter"))

(random-table/register :name "Mausritter > Carried bric-a-brac"
  :data '("[d8] pips" "[d8] pips" "[d8] pips" "[d8] pips" "[d8] pips" "[d8] pips" "[d8] pips" "[d8] pips"
           "Dried five-leaf clover, carefully folded" "Stone pendant of the Mother" "Stub of a pencil" "Dried herbs in waterproof bag" "Wire bent into the shape of a moth" "Letter of writ from a noblemouse" "Smooth piece of coloured glass" "Half-eaten piece of cheese, wrapped in paper"
           "Smoke-blackened bat tooth" "Metal cup etched with hunting scenes" "Oddly shimmering opal in silver wire setting" "Knife cut from a tin can" "Clay jar of thick honey mead" "Bee stinger wired to wooden handle" "Piece of candied berry" "Butterfly wings pressed between parchment"
           "Map showing treasure hidden in a settlement" "Note from a cat lord regarding a player mouse" "Wooden idol of centipede eating its own tail" "Tooth of a human child" "Pot of bright paint" "Angry ant queen in a glass jar" "Rolled tapestry depicting ancient battle" "Ball of wet clay that never dries out"
           "Lock of a faerie's hair" "Vial of red ink" "Straw basket with leather carrying straps" "Fragment of a spell tablet" "Dried, poisonous mushroom" "Pink plastic furbrush" "Collection of dried leaves, bound with twine" "Pipe carved of shell"
           "Scrap of sheepskin" "Quiver of silver-tipped arrows" "Wreath of silver wire" "Very strong magnet" "Bouncy rubber ball" "Fish leather satchel" "Extremely spicy chilli pepper" "Fly preserved in tree sap"))

(random-table/register :name "Mausritter > Birthsign"
  :data '("Star (Brave / Reckless)"
           "Wheel (Industrious / Unimaginative)"
           "Acorn (Inquisitive / Stubborn)"
           "Storm (Generous / Wrathful)"
           "Moon (Wise / Mysterious)"
           "Mother (Nurturing / Worrying)"))

(random-table/register :name "Mausritter > Coat Color"
  :private t
  :data '("Chocolate" "Black" "White" "Tan" "Grey" "Blue"))

(random-table/register :name "Mausritter > Coat Pattern"
  :private t
  :data '("Solid" "Brindle" "Patchy" "Banded" "Marbled" "Flecked"))

(random-table/register :name "Mausritter > Physical Detail"
  :private t
  :data '("Scarred body" "Groomed fur" "Corpulent body" "Dreadlocks" "Skeletal body" "Dyed fur"
  "Willowy body" "Shaved fur" "Tiny body" "Frizzy fur" "Massive body" "Silky fur"
  "War paint" "Night black eyes" "Foreign clothes" "Eye patch" "Elegant clothes" "Blood red eyes"
  "Patched clothes" "Wise eyes" "Fashionable clothes" "Sharp eyes" "Unwashed clothes" "Luminous eyes"
  "Missing ear" "Cropped tail" "Lumpy face" "Whip-like tail" "Beautiful face" "Tufted tail"
  "Round face" "Stubby tail" "Delicate face" "Prehensile tail" "Elongated face" "Curly tail"))

(random-table/register :name "Mausritter > Weather > Spring"
  :roller "2d6"
  :data '((2 . "Rain Storm †")
           ((3 . 5) . "Drizzle")
           ((6 . 8) . "Overcast")
           ((9 . 11) . "Bright and sunny")
           ((12) . "Clear and warm")))

(random-table/register :name "Mausritter > Weather > Summer"
  :roller "2d6"
  :data '((2 . "Thunder storm †")
           ((3 . 5) . "Very hot  †")
           ((6 . 8) . "Clear, hot")
           ((9 . 11) . "Pleasantly sunny")
           ((12) . "Beautifully warm")))

(random-table/register :name "Mausritter > Weather > Autumn"
  :roller "2d6"
  :data '((2 . "Wild winds †")
           ((3 . 5) . "Heavy rain  †")
           ((6 . 8) . "Cool")
           ((9 . 11) . "Patchy rain")
           ((12) . "Clear and crisp")))

(random-table/register :name "Mausritter > Weather > Winter"
  :roller "2d6"
  :data '((2 . "Snow storm †")
           ((3 . 5) . "Sleet  †")
           ((6 . 8) . "Bitter cold †")
           ((9 . 11) . "Overcast")
           ((12) . "Clear and crisp")))

(random-table/register :name "Mausritter > Settlement > Inhabitants"
  :private t
  :data '("Shave elaborate patterns in their fur" "Intoxicated by strange plants"
           "Wary of doing business with outsiders" "Curious for news from afar"
           "Believe grooming their fur is bad luck" "Wear finely embroidered clothes"
           "Brew honey-mead, flavoured with pungent herbs" "Cover their faces with long hoods"
           "Impoverished by a cat lord’s tithes" "Ceremonially crop their tails"
           "Brave hunters of large beasts" "All descended from single matriarch"
           "Bake delicious berry pies" "Lab escapees, naive about the world"
           "Spend their days lazing by a stream" "Long-standing blood feud with another settlement"
           "Dig grand tunnels, overseen by the guild" "Wear large, wide-brimmed hats"
           "Have laws and customs confusing to outsiders" "On friendly terms with a predator"))

(random-table/register :name "Mausritter > Settlement > Notable Feature"
  :private t
  :data '("Maze of defensive, trap-filled tunnels" "Exceedingly comfortable, well-appointed inn"
           "Shrine carved of black wood" "Meditative mushroom garden"
           "Cow skull, repurposed as a guildhouse" "Mess of closely packed shanties"
           "Neat rows of hanging wooden houses" "Ornate gate, guarded by statues"
           "Secret bat cult temple" "Beetle racing rink"
           "Storehouse, stocked with preserves" "Hidden riverboat dock"
           "Crumbling marble palace, built by ancient mice" "Scavenged human machine, working"
           "Wooden bridge connects the settlement" "Unnervingly tall, twisting tower"
           "Beautiful flower garden" "Pigeon rider’s roost"
           "Overgrown statue of an ancient hero" "Spiral stairwell, leading deep underground"))

(random-table/register :name "Mausritter > Settlement > Settlement Size"
  :private t
  :store t
  :data '("Farm/manor (1-3 families)" "Crossroads (3-5 families)"
           "Hamlet (50-150 mice)" "Village (150-300 mice)"
           "Town (300-1000 mice)" "City (1000+ mice)"))

(random-table/register :name "Mausritter > Settlement > Governance"
  :private t
  :roller '(+ "1d6" "Mausritter > Settlement > Settlement Size")
  :data '(((2 . 3) . "Guided by village elders")
           ((4 . 5) . "Administered by a knight or lower-caste lord")
           ((6 . 7) . "Organised by a guild committee")
           ((8 . 9) . "Free settlement, governed by council of burghermice")
           ((10 . 11) . "House of an upper caste noblemouse")
           (12 . "Seat of baronial power")))

(random-table/register :name "Mausritter > Settlement > Industry"
  :roller "Mausritter > Settlement > Settlement Size"
  :data '("{Mausritter > Settlement > Possible Industry}"
           "{Mausritter > Settlement > Possible Industry}"
           "{Mausritter > Settlement > Possible Industry}"
           "{Mausritter > Settlement > Possible Industry}"
           "{Mausritter > Settlement > Possible Industry} and {Mausritter > Settlement > Possible Industry}"
           "{Mausritter > Settlement > Possible Industry} and {Mausritter > Settlement > Possible Industry}"))

(random-table/register :name "Mausritter > Settlement > Possible Industry"
  ;; Towns and cities have 2.
  :private t
  :data '("Farmers, tending to towering crops" "Woodcutters, with saws and harnesses"
           "Rough and scarred fishermice, with nets and rafts" "Dark and musty mushroom farm"
           "Grains drying on every flat surface" "Pungent cheese, cured for years"
           "Gardens of rare herbs. Drying racks are guarded" "Hive of bees and their veiled keepers"
           "Merchants and traders, often in need of guards" "Stonemasons, working a nearby quarry"
           "Flour mill, driven by a large water-wheel" "Deep mine for iron, silver or tin"
           "Keep silkworms and weave fine cloth" "Expert explorers of caves and tunnels"
           "Kiln-fired pottery, glazed in cheerful colours" "Wool mill, draped in bright cloth"
           "Excellent school, rowdy pupils" "Bustling, well-stocked market"
           "Smelly scavenged trash pile, carefully picked over" "Beautiful furniture of carved and polished wood"))

(random-table/register :name "Mausritter > Settlement > Event"
  :private t
  :data '("Disaster, everyone packing to leave" "Wedding, streets decked in flowers"
           "Preparing for grand seasonal feast" "An illness has struck"
           "Storehouse has been plundered by insects" "Market day, farmers flock to the settlement"
           "Mice are at each other’s throats" "Warband forming to defeat a beast"
           "Several children have gone missing" "Noblemouse makes a frivolous demand"
           "Traveling theatre troupe arrives" "Funeral, streets thick with smoke"
           "Conman whips up an irrational scheme" "Pet beetle gone mad, attacking mice"
           "Faerie emissary with an impossible request" "Strangely quick-growing plant nearby"
           "Valuable heirloom has a been stolen" "Cat lord demands a heavy tithe"
           "Coming of age ceremony for the young mice" "Wizard tower arrives on tortoise-back"))

(random-table/register :name "Mausritter > Tavern and Inn"
  :data '("The {Mausritter > Tavern and Inn > Name} serves a specialty of {Mausritter > Tavern and Inn > Specialty Meal}"))

(random-table/register :name "Mausritter > Tavern and Inn > Name"
  :private t
  :data '("[White/Green/Black/Red/Silver/Crooked/Friendly/Hidden/Wiley/Glass/Thorny/Broken] [Beetle/Fox/Wedge/Kernel/Rat/Cheese/Eagle/Worm/Bee/Lantern/Rose/Knight]"))

(random-table/register :name "Mausritter > Tavern and Inn > Specialty Meal"
  :private t
  :data '("Spiced baked carrot" "Boiled worm broth" "Blackberry pie"
           "Pungent aged cheese" "Barley porridge" "Thick river-fish steak"
           "Baked apple" "Fried, crumbed insect legs" "Fresh buttered bread"
           "Scavenged candy" "Honey-roasted seeds" "Mushroom stew"))
