(if (f-file?  "~/git/random-table.el/random-table.el")
  (require 'random-table "~/git/random-table.el/random-table.el")
  (use-package random-table
    :straight (:host github :repo "jeremyf/random-table.el")))

(global-set-key (kbd "H-r") #'random-table/roll)
(global-set-key (kbd "C-c C-r") #'random-table/roll)

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

;;; Support Functions
(defun random-table/roller/saving-throw (table)
  "Prompt for rolling a saving throw for the given TABLE."
  (let ((score (random-table/prompt "Saving Throw Score"))
         (modifier (read-number
                     (format "%s\n> Modifier: "
                       (random-table-name table))
                     0))
         (roll (random-table/roller/1d20 table)))
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
(random-table/register :name "Henchman (Errant)"
  :data (list
          (concat "\n- Archetype :: ${Henchman > Archetype (Errant)}"
            "\n- Renown :: ${Henchman > Renown}"
            "\n- Morale :: ${[Henchman > Morale Base] + [Henchman > Morale Variable]}"
            "\n${Character (Errant)}")))

(random-table/register :name "Henchman > Archetype (Errant)"
  :private t
  :roller #'random-table/roller/1d10
  :data '(((1 . 5) . "Warrior")
           ((6 . 8) . "Professional")
           ((9 . 10) . "Magic User")))

(random-table/register :name "Henchman > Morale Base"
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

(random-table/register :name "Henchman > Morale Variable"
  :private t
  :roller (lambda (table)
            (+ (random-table/prompt "Additional Generosity of Offer") (random-table/roller/2d6 table)))
  :data '(((2) . -2)
           ((3 . 5) . -1)
           ((6 . 8) . 0)
           ((9 . 11) . 1)
           ((12 . 15) . 2)))

(random-table/prompt "Hiring location for Henchman"
  :type #'completing-read
  :range '(("Hamlet" . (lambda (table) 1))
            ("Village" . random-table/roller/1d2)
            ("Town" . random-table/roller/1d3)
            ("City" . random-table/roller/1d4)
            ("Metropolis" . random-table/roller/1d5)))


(random-table/register :name "Henchman > Renown"
  :roller (lambda (table)
            (funcall (random-table/prompt "Hiring location for Henchman") table))
  :private t
  :data '(1 2 3 4 5))

(random-table/register :name "Reaction Roll (Errant)"
  :roller #'random-table/roller/2d6
  :data '(((2) . "Hostile [DV +8]")
           ((3 4 5) . "Unfriendly [DV +4]")
           ((6 7 8) . "Unsure")
           ((9 10 11) . "Amicable [DV -2]")
           ((12) . "Friendly [DV -4]")))

(random-table/register :name "Character (Errant)"
  :data (list (concat "\n- Physique :: ${4d4}\n- Skill :: ${4d4}"
                "\n- Mind :: ${4d4}\n- Presence :: ${4d4}"
                "\n- Failed Profession :: ${Failed Professions (Errant)}"
                "\n- Keepsakes :: ${Keepsakes (Errant)}"
                "\n- Ancestry :: ${Ancestry (Errant)}")))

(random-table/register :name "Grimoire (Errant)"
  :data '("\n- Essence :: ${Grimoire > Essence (Errant)}\n- Sphere :: ${Grimoire > Sphere (Errant)}"))

(random-table/register :name "Grimoire > Essence (Errant)"
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

(random-table/register :name "Grimoire > Sphere (Errant)"
  :private t
  :data '("Magic" "Space" "Time" "Mind" "Spirit" "Body"
           "Elements" "Dimensions" "Life" "Death" "Objects" "Biota"))

(random-table/register :name "Ancestry (Errant)"
  :private t
  :data '("Tough" "Arcane" "Cunning" "Adaptable"))

(random-table/register :name "Keepsakes (Errant)"
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

(random-table/register :name "Failed Professions (Errant)"
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

(random-table/register :name "General Event (Errant)"
  :data '((1 . "Encounter")
           (2 . "Delay")
           (3 . "Resource use")
           (4 . "Local effect")
           (5 . "Clue")
           (6 . "Free from effect")))

(random-table/prompt "Downtime Turn Action Modifier (Errant)"
  :type 'bound-integer-range
  :range '(-3 -2 -1 0 1 2 3))

(random-table/register :name "General Downtime Turn Action (Errant)"
  :roller (lambda (table)
            (+ (random-table/roller/2d6 table) (random-table/prompt "Downtime Turn Action Modifier (Errant)")))
  :data '(((-10 . 6) . "Failure, no progress.")
           ((7 . 9) . "/Setback/, partial success, or progress.")
           ((10 . 22) . "Success, mark progress on /tracker/.")))

(random-table/register :name "Downtime Event (Errant)"
  :data '((1 . "Encounter: the COMPANY encounters an NPC(s). The guide may wish to have a list of random encounters prepared.")
           (2 . "Complication: a negative issue affects the region;\n- ${Downtime Event > Complications (Errant)}")
           (3 . "Expiration: any ongoing complications end. Any other temporary situations, arrangements, or benefits end.")
           (4 . "Trend: a positive or novel issue affects the region;\n- ${Downtime Event > Trends (Errant)}")
           (5 . "Intimation: the COMPANY receives some clue, perhaps relating to their next adventure, or to what the next encounter, complication, or trend may be.")
           (6 . "Free: nothing happens! The COMPANY gains a much needed reprieve and are allowed to complete their actions in peace.")))

(random-table/register :name "Downtime Event > Complications (Errant)"
  :private t
  :roller #'random-table/roller/2d6
  :data '((2 . "Natural disaster (e.g. a fire, a tornado, a meteor)." )
           (3 . "Ongoing disaster (e.g. a famine, a plague, a drought)." )
           (4 . "Major figure assassinated." )
           (5 . "Series of murders begins." )
           (6 . "A SCOURGE arises in the region." )
           (7 . "An ERRANT’s ESTATE, INSTITUTION, infrastructure project, DOMAIN, or other goal suffers a setback." )
           (8 . "Legal claims are brought against the COMPANY or they are publicly slandered." )
           (9 . "An ally of the COMPANY loses trust in or cuts ties with them." )
           (10 . "An insurrection or a siege occurs. If not dealt with in ${1d4} [1d4] DOWNTIME TURNS it will be successful." )
           (11 . "Two or more fACTIONS begin to oppose each other or actively go to war." )
           (12 . "An ally of the COMPANY dies." )))

(random-table/register :name "Downtime Event > Trends (Errant)"
  :private t
  :roller #'random-table/roller/2d4
  :data '((2 . "Two or more FACTIONS announce an alliance.")
           (3 . "A religious event occurs (e.g. an omen or apparition).")
           (4 . "A scandal is revealed.")
           (5 . "New NPC arrives in the area.")
           (6 . "A rival COMPANY arrives in the area")
           (7 . "A discovery is made (e.g. new technology, new lands).")
           (8 . "A new FACTION emerges.")))

(random-table/register :name "Weather (Errant)"
  :roller #'random-table/roller/prompt-from-table-data
  :data '(("Winter" . "Winter :: ${Weather > Winter (Errant)}")
           ("Spring" . "Springer :: ${Weather > Spring (Errant)}")
           ("Summer" . "Summer :: ${Weather > Summer (Errant)}")
           ("Autumn" . "Autumn :: ${Weather > Autumn (Errant)}")))

(random-table/prompt "Weather > Previous Day (Errant)"
  :type #'completing-read
  :range '(("Overcast" . -2)
            ("Clear Skies" . 2)
            ("Other" . 0)))

(defun random-table/roller/errant-weather (table)
  (+ (random-table/prompt "Weather > Previous Day (Errant)")
    (random-table/roller/2d6 table)))

(random-table/register :name "Weather > Winter (Errant)"
  :private t
  :roller #'random-table/roller/errant-weather
  :data '(((0 . 2) . "/Severe weather/ (e.g. blizzard)")
           ((3 . 5) . "/Severe weather/ (e.g. hail storm)")
           ((6 . 8) . "/Inclement weather/ (e.g. sleet)")
           ((9 . 11) . "Overcast (-2 to next weather roll)")
           ((12 . 14) . "Clear skies (+2 to next weather roll)")))

(random-table/register :name "Weather > Autumn (Errant)"
  :private t
  :roller #'random-table/roller/errant-weather
  :data '(((0 . 2) . "/Severe weather/ (e.g. hurricane)")
           ((3 . 5) . "/Inclement weather/ (e.g. fog)")
           ((6 . 8) . "Overcast (-2 to next weather roll)")
           ((9 . 11) .  "Cloudy")
           ((12 . 14) . "Clear skies (+2 to next weather roll)")))

(random-table/register :name "Weather > Summer (Errant)"
  :private t
  :roller #'random-table/roller/errant-weather
  :data '(((0 . 2) . "/Severe weather/ (e.g. thunderstorm)")
           ((3 . 5) . "/Inclement weather/ (e.g. heat wave)")
           ((6 . 8) . "Sunny")
           ((9 . 11) .  "Clear skies (+2 to next weather roll)")
           ((12 . 14) . "Beautiful day (only need to spend one TRAVEL TURN /sleeping/.")))

(random-table/register :name "Weather > Spring (Errant)"
  :private t
  :roller #'random-table/roller/errant-weather
  :data '(((0 . 2) . "/Inclement weather/ (e.g. down pour)")
           ((3 . 5) . "Cosmetic change (e.g. drizzle)")
           ((6 . 8) . "Cloudy")
           ((9 . 11) .  "Clear skies (+2 to next weather roll)")
           ((12 . 14) . "Beautiful day (only need to spend one TRAVEL TURN /sleeping/.")))

(random-table/register :name "Debt Holder (Errant)"
  :data '("${Debt Holder > Adjective (Errant)} ${Debt Holder > Person (Errant)}"))

(random-table/register :name "Debt Holder > Adjective (Errant)"
  :private t
  :data '("unctuous" "sententious" "truculent" "supercilious" "fulsome" "vainglorious"))

(random-table/register :name "Debt Holder > Person (Errant)"
  :private t
  :data '("eunuch" "merchant" "clergyman" "madam" "officer" "intellectual"))

;;; Black Sword Hack
(random-table/register :name "Demon Name (Black Sword Hack)"
  :data '("Beleth" "Abaddon" "Ulshedra" "Marduk" "Raum"
           "Halphas" "Ashurban" "Ordog" "Charun" "Surgat"
           "Ahriman" "Wissigo" "Furcas" "Keldim" "Gorgo"
           "Rahab" "Gaki" "Samnu" "Namtar" "Baalberith"))

(random-table/register :name "Nickname (Black Sword Hack)"
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

(random-table/register :name "How to find the demon you seek (Black Sword Hack)"
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

(random-table/register :name "Travel Event (Black Sword Hack)"
  :data "\n  - Subject :: ${Travel Event > Subject (Black Sword Hack)}\n  - Theme :: ${Travel Event > Theme (Black Sword Hack)}")

(random-table/register :name "Travel Event > Theme (Black Sword Hack)"
  :private t
  :data
  '("Aggression" "Exchange" "Discovery" "Revelation" "Pursuit"
     "Lost" "Isolation" "Death" "Escape" "Change"))

(random-table/register :name "Travel Event > Subject (Black Sword Hack)"
  :private t
  :data
  '("Antagonist" "Animal" "Hermit" "Spirit" "Potentate"
     "Demon" "Explorer" "Merchant" "Caves" "Messenger"
     "Ruins" "Cult" "Community" "Ghost" "Outlaws"
     "Artists" "Soldiers" "Sorcerer" "Vagrant" "Natural disaster"))

(random-table/register :name "Oracle Event (Black Sword Hack)"
  :data '("\n  - Theme :: ${Oracle Event > Theme (Black Sword Hack)}\n  - Subject :: ${Oracle Event > Subject (Black Sword Hack)}"))

(random-table/register :name "Oracle Event > Theme (Black Sword Hack)"
  :private t
  :data
  '("Death" "Treachery" "Infiltration" "Desperation" "Instability" "Suspicion"
     "Escape" "Fear" "Hunt" "Division" "Falsehood" "Celebration"
     "Conquest" "Friendship" "Love" "Sacrifice" "Decay" "Exile"
     "Revenge" "Greed" "Isolation" "Preservation" "Loss" "Rebirth"
     "Oppression" "Destruction" "Ignorance" "Purification" "Scarcity" "Quest"
     "Stagnation" "Redemption" "Failure" "Help" "Corruption" "Rebellion"))

(random-table/register :name "Oracle Event > Subject (Black Sword Hack)"
  :private t
  :data
  '("Army" "Church" "Ghost" "Nobility" "Otherworldly" "Plague"
     "Omen" "Ally" "Family" "Wizard" "Guild" "Architect"
     "Crusaders" "Vagrant" "Rival" "Artefact" "Messenger" "Inquisitors"
     "Ruins" "Knowledge" "Cave" "Dream" "Hamlet" "Outlaws"
     "Healers" "Cult" "Guardian" "Settlers" "Monument" "Food"
     "Judges" "Storm" "Demon" "Court" "Theatre" "Assassins"))

(random-table/register :name "Attributes (Black Sword Hack)"
  :data '("\n- Strength :: ${Attribute Score (Black Sword Hack)}\n- Dexterity :: ${Attribute Score (Black Sword Hack)}\n- Constitution :: ${Attribute Score (Black Sword Hack)}\n- Intelligence :: ${Attribute Score (Black Sword Hack)}\n- Wisdom :: ${Attribute Score (Black Sword Hack)}\n- Charisma :: ${Attribute Score (Black Sword Hack)}"))

(random-table/register :name "Attribute Score (Black Sword Hack)"
  :private t
  :roller #'random-table/roller/2d6
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

(defun random-table/roller/oracle-question (table)
  "Prompt for likelihood and return corresponding roller for TABLE."
  (let ((likelihood (completing-read "Likelihood: " jf/gaming/black-sword-hack/table/oracle-question-likelihood nil t)))
    (funcall (alist-get likelihood jf/gaming/black-sword-hack/table/oracle-question-likelihood nil nil #'string=))))

(random-table/register :name "Oracle Question (Black Sword Hack)"
  :roller #'random-table/roller/oracle-question
  :fetcher (lambda (data index) (car data))
  :data '("${Oracle Question > Answer (Black Sword Hack)}${Oracle Question > Unexpected Event (Black Sword Hack)}")
  :store t)

(random-table/register :name "Oracle Question > Answer (Black Sword Hack)"
  :reuse "Oracle Question (Black Sword Hack)"
  :private t
  :filter (lambda (&rest dice) "We have a pool of dice to pick one." (car (-list dice)))
  :data '("No and…" "No" "No but…" "Yes but…" "Yes" "Yes and…"))

(random-table/register :name "Oracle Question > Unexpected Event (Black Sword Hack)"
  :reuse "Oracle Question (Black Sword Hack)"
  :private t
  :filter (lambda (&rest dice) "We have a pool of dice to determine if there are dupes."
            (car (list-utils-dupes (-list dice))))
  :fetcher (lambda (data index)
             (when index (concat " with unexpected “" (nth (- (car (-list index)) 1) data) "” event")))
  :data '("Very negative" "Negative" "Negative but…" "Positive but…" "Positive" "Very Positive"))

;;; OSE
(random-table/register :name "Attributes (OSR) (3d6 Down the Line)"
  :data '("\n- Strength :: ${3d6}\n- Intelligence :: ${3d6}\n- Wisdom :: ${3d6}\n- Dexterity :: ${3d6}\n- Constitution :: ${3d6}\n- Charisma :: ${3d6}"))

(random-table/register :name "Secondary Skill (OSE)"
  :roller #'random-table/roller/1d100
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
           ((99 . 100) .  "${Secondary Skill (OSE)} and ${Secondary Skill (OSE)} ")))

;;; Random Names (from Stars without Number)
(random-table/register :name "Name"
  :exclude-from-prompt t
  :data '("${Arabic Name > Male Given Name} ${Arabic Name > Surname}"
           "${Arabic Name > Female Given Name} ${Arabic Name > Surname}"
           "${Chinese Name > Male Given Name} ${Chinese Name > Surname}"
           "${Chinese Name > Female Given Name} ${Chinese Name > Surname}"
           "${English Name > Male Given Name} ${English Name > Surname}"
           "${English Name > Female Given Name} ${English Name > Surname}"
           "${Greek Name > Male Given Name} ${Greek Name > Surname}"
           "${Greek Name > Female Given Name} ${Greek Name > Surname}"
           "${Indian Name > Male Given Name} ${Indian Name > Surname}"
           "${Indian Name > Female Given Name} ${Indian Name > Surname}"
           "${Japanese Name > Male Given Name} ${Japanese Name > Surname}"
           "${Japanese Name > Female Given Name} ${Japanese Name > Surname}"
           "${Latin Name > Male Given Name} ${Latin Name > Surname}"
           "${Latin Name > Female Given Name} ${Latin Name > Surname}"
           "${Nigerian Name > Male Given Name} ${Nigerian Name > Surname}"
           "${Nigerian Name > Female Given Name} ${Nigerian Name > Surname}"
           "${Russian Name > Male Given Name} ${Russian Name > Surname}"
           "${Russian Name > Female Given Name} ${Russian Name > Surname}"
           "${Spanish Name > Male Given Name} ${Spanish Name > Surname}"
           "${Spanish Name > Female Given Name} ${Spanish Name > Surname}"
           "${Norse Given Name} ${Norse Surname}"
           "${Norse Given Name} ${Norse Surname}"))

(random-table/register :name "Arabic Name > Male Given Name"
  :private t
  :data '("Aamir" "Ayub" "Binyamin" "Efraim" "Ibrahim" "Ilyas" "Ismail" "Jibril" "Jumanah" "Kazi" "Lut" "Matta" "Mohammed" "Mubarak" "Mustafa" "Nazir" "Rahim" "Reza" "Sharif" "Taimur" "Usman" "Yakub" "Yusuf" "Zakariya" "Zubair"))
(random-table/register :name "Arabic Name > Female Given Name"
  :private t
  :data '("Aisha" "Alimah" "Badia" "Bisharah" "Chanda" "Daliya" "Fatimah" "Ghania" "Halah" "Kaylah" "Khayrah" "Layla" "Mina" "Munisa" "Mysha" "Naimah" "Nissa" "Nura" "Parveen" "Rana" "Shalha" "Suhira" "Tahirah" "Yasmin" "Zulehka"))
(random-table/register :name "Arabic Name > Surname"
  :private t
  :data '("Abdel" "Awad" "Dahhak" "Essa" "Hanna" "Harbi" "Hassan" "Isa" "Kasim" "Katib" "Khalil" "Malik" "Mansoor" "Mazin" "Musa" "Najeeb" "Namari" "Naser" "Rahman" "Rasheed" "Saleh" "Salim" "Shadi" "Sulaiman" "Tabari"))
(random-table/register :name "Arabic Location Name"
  :private t
  :data '("Adan" "Magrit" "Ahsa" "Masqat" "Andalus" "Misr" "Asmara" "Muruni" "Asqlan" "Qabis" "Baqubah" "Qina" "Basit" "Rabat" "Baysan" "Ramlah" "Baytlahm" "Riyadh" "Bursaid" "Sabtah" "Dahilah" "Salalah" "Darasalam" "Sana" "Dawhah" "Sinqit" "Ganin" "Suqutrah" "Gebal" "Sur" "Gibuti" "Tabuk" "Giddah" "Tangah" "Harmah" "Tarifah" "Hartum" "Tarrakunah" "Hibah" "Tisit" "Hims" "Uman" "Hubar" "Urdunn" "Karbala" "Wasqah" "Kut" "Yaburah" "Lacant" "Yaman"))

(random-table/register :name "Chinese Name > Male Given Name"
  :private t
  :data '("Aiguo" "Bohai" "Chao" "Dai" "Dawei" "Duyi" "Fa" "Fu" "Gui" "Hong" "Jianyu" "Kang" "Li" "Niu" "Peng" "Quan" "Ru" "Shen" "Shi" "Song" "Tao" "Xue" "Yi" "Yuan" "Zian"))
(random-table/register :name "Chinese Name > Female Given Name"
  :private t
  :data '("Biyu" "Changying" "Daiyu" "Huidai" "Huiliang" "Jia" "Jingfei" "Lan" "Liling" "Liu" "Meili" "Niu" "Peizhi" "Qiao" "Qing" "Ruolan" "Shu" "Suyin" "Ting" "Xia" "Xiaowen" "Xiulan" "Ya" "Ying" "Zhilan"))
(random-table/register :name "Chinese Name > Surname"
  :private t
  :data '("Bai" "Cao" "Chen" "Cui" "Ding" "Du" "Fang" "Fu" "Guo" "Han" "Hao" "Huang" "Lei" "Li" "Liang" "Liu" "Long" "Song" "Tan" "Tang" "Wang" "Wu" "Xing" "Yang" "Zhang"))
(random-table/register :name "Chinese Location Name"
  :private t
  :data '("Andong" "Luzhou" "Anqing" "Ningxia" "Anshan" "Pingxiang" "Chaoyang" "Pizhou" "Chaozhou" "Qidong" "Chifeng" "Qingdao" "Dalian" "Qinghai" "Dunhuang" "Rehe" "Fengjia" "Shanxi" "Fengtian" "Taiyuan" "Fuliang" "Tengzhou" "Fushun" "Urumqi" "Gansu" "Weifang" "Ganzhou" "Wugang" "Guizhou" "Wuxi" "Hotan" "Xiamen" "Hunan" "Xian" "Jinan" "Xikang" "Jingdezhen" "Xining" "Jinxi" "Xinjiang" "Jinzhou" "Yidu" "Kunming" "Yingkou" "Liaoning" "Yuxi" "Linyi" "Zigong" "Lushun" "Zoige"))

(random-table/register :name "English Name > Male Given Name"
  :private t
  :data '("Adam" "Albert" "Alfred" "Allan" "Archibald" "Arthur" "Basil" "Charles" "Colin" "Donald" "Douglas" "Edgar" "Edmund" "Edward" "George" "Harold" "Henry" "Ian" "James" "John" "Lewis" "Oliver" "Philip" "Richard" "William"))
(random-table/register :name "English Name > Female Given Name"
  :private t
  :data '("Abigail" "Anne" "Beatrice" "Blanche" "Catherine" "Charlotte" "Claire" "Eleanor" "Elizabeth" "Emily" "Emma" "Georgia" "Harriet" "Joan" "Judy" "Julia" "Lucy" "Lydia" "Margaret" "Mary" "Molly" "Nora" "Rosie" "Sarah" "Victoria"))
(random-table/register :name "English Name > Surname"
  :private t
  :data '("Barker" "Brown" "Butler" "Carter" "Chapman" "Collins" "Cook" "Davies" "Gray" "Green" "Harris" "Jackson" "Jones" "Lloyd" "Miller" "Roberts" "Smith" "Taylor" "Thomas" "Turner" "Watson" "White" "Williams" "Wood" "Young"))
(random-table/register :name "English Location Name"
  :private t
  :data '("Aldington" "Kedington" "Appleton" "Latchford" "Ashdon" "Leigh" "Berwick" "Leighton" "Bramford" "Maresfield" "Brimstage" "Markshall" "Carden" "Netherpool" "Churchill" "Newton" "Clifton" "Oxton" "Colby" "Preston" "Copford" "Ridley" "Cromer" "Rochford" "Davenham" "Seaford" "Dersingham" "Selsey" "Doverdale" "Stanton" "Elsted" "Stockham" "Ferring" "Stoke" "Gissing" "Sutton" "Heydon" "Thakeham" "Holt" "Thetford" "Hunston" "Thorndon" "Hutton" "Ulting" "Inkberrow" "Upton" "Inworth" "Westhorpe" "Isfield" "Worcester"))

(random-table/register :name "Greek Name > Male Given Name"
  :private t
  :data '("Alexander" "Alexius" "Anastasius" "Christodoulos" "Christos" "Damian" "Dimitris" "Dysmas" "Elias" "Giorgos" "Ioannis" "Konstantinos" "Lambros" "Leonidas" "Marcos" "Miltiades" "Nestor" "Nikos" "Orestes" "Petros" "Simon" "Stavros" "Theodore" "Vassilios" "Yannis"))
(random-table/register :name "Greek Name > Female Given Name"
  :private t
  :data '("Alexandra" "Amalia" "Callisto" "Charis" "Chloe" "Dorothea" "Elena" "Eudoxia" "Giada" "Helena" "Ioanna" "Lydia" "Melania" "Melissa" "Nika" "Nikolina" "Olympias" "Philippa" "Phoebe" "Sophia" "Theodora" "Valentina" "Valeria" "Yianna" "Zoe"))
(random-table/register :name "Greek Name > Surname"
  	:private t
  :data '("Andreas" "Argyros" "Dimitriou" "Floros" "Gavras" "Ioannidis" "Katsaros" "Kyrkos" "Leventis" "Makris" "Metaxas" "Nikolaidis" "Pallis" "Pappas" "Petrou" "Raptis" "Simonides" "Spiros" "Stavros" "Stephanidis" "Stratigos" "Terzis" "Theodorou" "Vasiliadis" "Yannakakis"))
(random-table/register :name "Greek Location Name"
  :private t
  :data '("Adramyttion" "Kallisto" "Ainos" "Katerini" "Alikarnassos" "Kithairon" "Avydos" "Kydonia" "Dakia" "Lakonia" "Dardanos" "Leros" "Dekapoli" "Lesvos" "Dodoni" "Limnos" "Efesos" "Lykia" "Efstratios" "Megara" "Elefsina" "Messene" "Ellada" "Milos" "Epidavros" "Nikaia" "Erymanthos" "Orontis" "Evripos" "Parnasos" "Gavdos" "Petro" "Gytheio" "Samos" "Ikaria" "Syros" "Ilios" "Thapsos" "Illyria" "Thessalia" "Iraia" "Thira" "Irakleio" "Thiva" "Isminos" "Varvara" "Ithaki" "Voiotia" "Kadmeia" "Vyvlos"))

;; Indian Name > Male Given Name
(random-table/register :name "Indian Name > Male Given Name"
  :private t
  :data '("Amrit" "Ashok" "Chand" "Dinesh" "Gobind" "Harinder" "Jagdish" "Johar" "Kurien" "Lakshman" "Madhav" "Mahinder" "Mohal" "Narinder" "Nikhil" "Omrao" "Prasad" "Pratap" "Ranjit" "Sanjay" "Shankar" "Thakur" "Vijay" "Vipul" "Yash"))
(random-table/register :name "Indian Name > Female Given Name"
  :private t
  :data '("Amala" "Asha" "Chandra" "Devika" "Esha" "Gita" "Indira" "Indrani" "Jaya" "Jayanti" "Kiri" "Lalita" "Malati" "Mira" "Mohana" "Neela" "Nita" "Rajani" "Sarala" "Sarika" "Sheela" "Sunita" "Trishna" "Usha" "Vasanta"))
(random-table/register :name "Indian Name > Surname"
  :private t
  :data '("Achari" "Banerjee" "Bhatnagar" "Bose" "Chauhan" "Chopra" "Das" "Dutta" "Gupta" "Johar" "Kapoor" "Mahajan" "Malhotra" "Mehra" "Nehru" "Patil" "Rao" "Saxena" "Shah" "Sharma" "Singh" "Trivedi" "Venkatesan" "Verma" "Yadav"))
(random-table/register :name "Indian Location Name"
  :private t
  :data '("Ahmedabad" "Jaisalmer" "Alipurduar" "Jharonda" "Alubari" "Kadambur" "Anjanadri" "Kalasipalyam" "Ankleshwar" "Karnataka" "Balarika" "Kutchuhery" "Bhanuja" "Lalgola" "Bhilwada" "Mainaguri" "Brahmaghosa" "Nainital" "Bulandshahar" "Nandidurg" "Candrama" "Narayanadri" "Chalisgaon" "Panipat" "Chandragiri" "Panjagutta" "Charbagh" "Pathankot" "Chayanka" "Pathardih" "Chittorgarh" "Porbandar" "Dayabasti" "Rajasthan" "Dikpala" "Renigunta" "Ekanga" "Sewagram" "Gandhidham" "Shakurbasti" "Gollaprolu" "Siliguri" "Grahisa" "Sonepat" "Guwahati" "Teliwara" "Haridasva" "Tinpahar" "Indraprastha" "Villivakkam"))

(random-table/register :name "Japanese Name > Male Given Name"
  :private t
  :data '("Akira" "Daisuke" "Fukashi" "Goro" "Hiro" "Hiroya" "Hotaka" "Katsu" "Katsuto" "Keishuu" "Kyuuto" "Mikiya" "Mitsunobu" "Mitsuru" "Naruhiko" "Nobu" "Shigeo" "Shigeto" "Shou" "Shuji" "Takaharu" "Teruaki" "Tetsushi" "Tsukasa" "Yasuharu"))
(random-table/register :name "Japanese Name > Female Given Name"
  :private t
  :data '("Aemi" "Airi" "Ako" "Ayu" "Chikaze" "Eriko" "Hina" "Kaori" "Keiko" "Kyouka" "Mayumi" "Miho" "Namiko" "Natsu" "Nobuko" "Rei" "Ririsa" "Sakimi" "Shihoko" "Shika" "Tsukiko" "Tsuzune" "Yoriko" "Yorimi" "Yoshiko"))
(random-table/register :name "Japanese Name > Surname"
  :private t
  :data '("Abe" "Arakaki" "Endo" "Fujiwara" "Goto" "Ito" "Kikuchi" "Kinjo" "Kobayashi" "Koga" "Komatsu" "Maeda" "Nakamura" "Narita" "Ochi" "Oshiro" "Saito" "Sakamoto" "Sato" "Suzuki" "Takahashi" "Tanaka" "Watanabe" "Yamamoto" "Yamasaki"))
(random-table/register :name "Japanese Location Name"
  :private t
  :data '("Bando" "Mitsukaido" "Chikuma" "Moriya" "Chikusei" "Nagano" "Chino" "Naka" "Hitachi" "Nakano" "Hitachinaka" "Ogi" "Hitachiomiya" "Okaya" "Hitachiota" "Omachi" "Iida" "Ryugasaki" "Iiyama" "Saku" "Ina" "Settsu" "Inashiki" "Shimotsuma" "Ishioka" "Shiojiri" "Itako" "Suwa" "Kamisu" "Suzaka" "Kasama" "Takahagi" "Kashima" "Takeo" "Kasumigaura" "Tomi" "Kitaibaraki" "Toride" "Kiyose" "Tsuchiura" "Koga" "Tsukuba" "Komagane" "Ueda" "Komoro" "Ushiku" "Matsumoto" "Yoshikawa" "Mito" "Yuki"))

(random-table/register :name "Latin Name > Male Given Name"
  :private t
  :data '("Agrippa" "Appius" "Aulus" "Caeso" "Decimus" "Faustus" "Gaius" "Gnaeus" "Hostus" "Lucius" "Mamercus" "Manius" "Marcus" "Mettius" "Nonus" "Numerius" "Opiter" "Paulus" "Proculus" "Publius" "Quintus" "Servius" "Tiberius" "Titus" "Volescus"))
(random-table/register :name "Latin Name > Female Given Name"
  :private t
  :data '("Appia" "Aula" "Caesula" "Decima" "Fausta" "Gaia" "Gnaea" "Hosta" "Lucia" "Maio" "Marcia" "Maxima" "Mettia" "Nona" "Numeria" "Octavia" "Postuma" "Prima" "Procula" "Septima" "Servia" "Tertia" "Tiberia" "Titia" "Vibia"))
(random-table/register :name "Latin Name > Surname"
  :private t
  :data '("Antius" "Aurius" "Barbatius" "Calidius" "Cornelius" "Decius" "Fabius" "Flavius" "Galerius" "Horatius" "Julius" "Juventius" "Licinius" "Marius" "Minicius" "Nerius" "Octavius" "Pompeius" "Quinctius" "Rutilius" "Sextius" "Titius" "Ulpius" "Valerius" "Vitellius"))
(random-table/register :name "Latin Location Name"
  :private t
  :data '("Abilia" "Lucus" "Alsium" "Lugdunum" "Aquileia" "Mediolanum" "Argentoratum" "Novaesium" "Ascrivium" "Patavium" "Asculum" "Pistoria" "Attalia" "Pompeii" "Barium" "Raurica" "Batavorum" "Rigomagus" "Belum" "Roma" "Bobbium" "Salernum" "Brigantium" "Salona" "Burgodunum" "Segovia" "Camulodunum" "Sirmium" "Clausentum" "Spalatum" "Corduba" "Tarraco" "Coriovallum" "Treverorum" "Durucobrivis" "Verulamium" "Eboracum" "Vesontio" "Emona" "Vetera" "Florentia" "Vindelicorum" "Lactodurum" "Vindobona" "Lentia" "Vinovia" "Lindum" "Viroconium" "Londinium" "Volubilis"))

(random-table/register :name "Nigerian Name > Male Given Name"
  :private t
  :data '("Adesegun" "Akintola" "Amabere" "Arikawe" "Asagwara" "Chidubem" "Chinedu" "Chiwetei" "Damilola" "Esangbedo" "Ezenwoye" "Folarin" "Genechi" "Idowu" "Kelechi" "Ketanndu" "Melubari" "Nkanta" "Obafemi" "Olatunde" "Olumide" "Tombari" "Udofia" "Uyoata" "Uzochi"))
(random-table/register :name "Nigerian Name > Female Given Name"
  :private t
  :data '("Abike" "Adesuwa" "Adunola" "Anguli" "Arewa" "Asari" "Bisola" "Chioma" "Eduwa" "Emilohi" "Fehintola" "Folasade" "Mahparah" "Minika" "Nkolika" "Nkoyo" "Nuanae" "Obioma" "Olafemi" "Shanumi" "Sominabo" "Suliat" "Tariere" "Temedire" "Yemisi"))
(random-table/register :name "Nigerian Name > Surname"
  :private t
  :data '("Adegboye" "Adeniyi" "Adeyeku" "Adunola" "Agbaje" "Akpan" "Akpehi" "Aliki" "Asuni" "Babangida" "Ekim" "Ezeiruaku" "Fabiola" "Fasola" "Nwokolo" "Nzeocha" "Ojo" "Okonkwo" "Okoye" "Olaniyan" "Olawale" "Olumese" "Onajobi" "Soyinka" "Yamusa"))
(random-table/register :name "Nigerian Location Name"
  :private t
  :data '("Abadan" "Jere" "Ador" "Kalabalge" "Agatu" "Katsina" "Akamkpa" "Knoduga" "Akpabuyo" "Konshishatse" "Ala" "Kukawa" "Askira" "Kwande" "Bakassi" "Kwayakusar" "Bama" "Logo" "Bayo" "Mafa" "Bekwara" "Makurdi" "Biase" "Nganzai" "Boki" "Obanliku" "Buruku" "Obi" "Calabar" "Obubra" "Chibok" "Obudu" "Damboa" "Odukpani" "Dikwa" "Ogbadibo" "Etung" "Ohimini" "Gboko" "Okpokwu" "Gubio" "Otukpo" "Guzamala" "Shani" "Gwoza" "Ugep" "Hawul" "Vandeikya" "Ikom" "Yala"))

(random-table/register :name "Russian Name > Male Given Name"
  :private t
  :data '("Aleksandr" "Andrei" "Arkady" "Boris" "Dmitri" "Dominik" "Grigory" "Igor" "Ilya" "Ivan" "Kiril" "Konstantin" "Leonid" "Nikolai" "Oleg" "Pavel" "Petr" "Sergei" "Stepan" "Valentin" "Vasily" "Viktor" "Yakov" "Yegor" "Yuri"))
(random-table/register :name "Russian Name > Female Given Name"
  :private t
  :data '("Aleksandra" "Anastasia" "Anja" "Catarina" "Devora" "Dima" "Ekaterina" "Eva" "Irina" "Karolina" "Katlina" "Kira" "Ludmilla" "Mara" "Nadezdha" "Nastassia" "Natalya" "Oksana" "Olena" "Olga" "Sofia" "Svetlana" "Tatyana" "Vilma" "Yelena"))
(random-table/register :name "Russian Name > Surname"
  :private t
  :data '("Abelev" "Bobrikov" "Chemerkin" "Gogunov" "Gurov" "Iltchenko" "Kavelin" "Komarov" "Korovin" "Kurnikov" "Lebedev" "Litvak" "Mekhdiev" "Muraviov" "Nikitin" "Ortov" "Peshkov" "Romasko" "Shvedov" "Sikorski" "Stolypin" "Turov" "Volokh" "Zaitsev" "Zhukov"))
(random-table/register :name "Russian Location Name"
  :private t
  :data '("Amur" "Omsk" "Arkhangelsk" "Orenburg" "Astrakhan" "Oryol" "Belgorod" "Penza" "Bryansk" "Perm" "Chelyabinsk" "Pskov" "Chita" "Rostov" "Gorki" "Ryazan" "Irkutsk" "Sakhalin" "Ivanovo" "Samara" "Kaliningrad" "Saratov" "Kaluga" "Smolensk" "Kamchatka" "Sverdlovsk" "Kemerovo" "Tambov" "Kirov" "Tomsk" "Kostroma" "Tula" "Kurgan" "Tver" "Kursk" "Tyumen" "Leningrad" "Ulyanovsk" "Lipetsk" "Vladimir" "Magadan" "Volgograd" "Moscow" "Vologda" "Murmansk" "Voronezh" "Novgorod" "Vyborg" "Novosibirsk" "Yaroslavl"))

(random-table/register :name "Spanish Name > Male Given Name"
  :private t
  :data '("Alejandro" "Alonso" "Amelio" "Armando" "Bernardo" "Carlos" "Cesar" "Diego" "Emilio" "Estevan" "Felipe" "Francisco" "Guillermo" "Javier" "Jose" "Juan" "Julio" "Luis" "Pedro" "Raul" "Ricardo" "Salvador" "Santiago" "Valeriano" "Vicente"))
(random-table/register :name "Spanish Name > Female Given Name"
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
  :data '("${Norse Given Name}son" "${Norse Given Name}dóttir"))

(random-table/register :name "Person"
  :data '("\n- Name :: ${Name}\n- Physique :: ${Person > Physique}\n- Skin :: ${Person > Skin}\n- Hair :: ${Person > Hair}\n- Face :: ${Person > Face}\n- Speech :: ${Person > Speech}\n- Virtue :: ${Person > Virtue}\n- Vice :: ${Person > Vice}"))

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
  :data '("\n- Name :: ${Corporation, Sci-Fi > Prefix} ${Corporation, Sci-Fi > Suffix}\n- Business :: ${Corporation, Sci-Fi > Business}\n- Rumor :: ${Corporation, Sci-Fi > Rumor}"))

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
    :data '("\n- Founder :: ${Heresy > Founder}\n- Major Heresy :: ${Heresy > Major Heresy}\n- Attitude :: ${Heresy > Attitude}\n- Quirk :: ${Heresy > Quirk}"))

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
             "Legitimist: the sect views itself as the \true\" orthodox faith and the present orthodox hierarchy as pretenders to their office."
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
  (random-table/register :name "Plant (Herbalist's Primer)"
    :data '("${Plant > Name Prefix (Herbalist's Primer)}${Plant > Name Suffix (Herbalist's Primer)} is ${Plant > Rarity (Herbalist's Primer)} ${Plant > Habit (Herbalist's Primer)}, mostly prized for its ${Plant > Properties (Herbalist's Primer)} value.  It is a native to the ${Plant > Climate (Herbalist's Primer)} ${Plant > Biome (Herbalist's Primer)}.  Interestingly, it ${Plant > Quirk (Herbalist's Primer)}.${Plant > Property Description (Herbalist's Primer)}"))

  (random-table/register :name "Plant > Name Prefix (Herbalist's Primer)"
    :private t
    :data '("Arrow" "Blood" "Crimson" "Death" "Dragon" "Fire" "Gold" "Good" "Ice" "Life"
             "Raven" "Snake" "Spear" "Spirit" "Star" "Sword" "Truth" "Witch" "Wolf" "Worm"))

  (random-table/register :name "Plant > Name Suffix (Herbalist's Primer)"
    :private t
    :data '("bane" "bark" "bean" "berry" "bush" "fern" "flower" "fruit" "grass" "leaf"
             "nut" "plant" "root" "seed" "spice" "thorn" "tree" "weed" "wort" "wood"))

  ;; Todo consider altering the rarity
  (random-table/register :name "Plant > Rarity (Herbalist's Primer)"
    :private t
    :data '("a widespread" "an abundant" "a common" "a popular" "an uncommon"
             "a rare" "an endangered" "a near-extinct" "a legendary" "a mythical"))

  (random-table/register :name "Plant > Habit (Herbalist's Primer)"
    :private t
    :data '("herb" "shrub" "tree"))

  (random-table/register :name "Plant > Properties (Herbalist's Primer)"
    :private t
    :store t
    :data '("culinary" "industrial" "magical" "medicinal" "ornamental" "poisonous"))

  (random-table/register :name "Plant > Climate (Herbalist's Primer)"
    :private t
    :data '("artic" "arid" "boreal" "cold" "continental"
             "dry" "high-altitude" "hot" "humid" "ice-bound"
             "island" "marine" "monsoon" "oceanic" "polar"
             "subartic" "subtropical" "temperate" "tropical" "wet"))

  (random-table/register :name "Plant > Biome (Herbalist's Primer)"
    :private t
    :data '("caves" "deserts" "forests" "gardens" "hills"
             "lakes" "meadows" "mountains" "plains" "plantations"
             "riverbanks" "roadsides" "seas" "shores" "shrublands"
             "streams" "swamps" "urban areas" "volcanoes" "wastes"))

  (random-table/register :name "Plant > Quirk (Herbalist's Primer)"
    :private t
    :data '("is carnivorous" "is parasitic" "is symbiotic with another plant" "stores water in the stems" "has a strong, pleasant aroma"
             "is always warm to the touch" "is covered in sharp spikes" "is covered in a sticky sap" "smells of rotting meat" "grows in the tree crowns"
             "grows almost entirely underground" "only blooms at night" "is poisonous to other plants" "causes a strong allergic reaction" "produces a lot of pollen"
             "attracts all kinds of insects" "is a favorite snack of many animals" "often hosts bird nests" "has a lovely, sweet flavor" "grows incredibly fast"))

  (random-table/register :name "Plant > Property Description (Herbalist's Primer)"
    :private t
    :data
    '(nil ;; culinary
       nil ;; industrial
       "  Magical Property: the plant’s ${Plant > Material (Herbalist's Primer)} ${Plant > Method (Herbalist's Primer)} will ${Plant > Effect > Magical (Herbalist's Primer)}.  One complication is that ${Plant > Complication (Herbalist's Primer)}." ;; magical
       "  Medicinal Property: the plant’s ${Plant > Material (Herbalist's Primer)} ${Plant > Method (Herbalist's Primer)} will ${Plant > Effect > Medicinal (Herbalist's Primer)}.  One complication is that ${Plant > Complication (Herbalist's Primer)}." ;; magical
       nil ;; ornamental
       "  Poisonous Property: the plant’s ${Plant > Material (Herbalist's Primer)} ${Plant > Method (Herbalist's Primer)} will ${Plant > Effect > Poisonous (Herbalist's Primer)}.  One complication is that ${Plant > Complication (Herbalist's Primer)}." ;; magical
       ))

  (random-table/register :name "Plant > Material (Herbalist's Primer)"
    :private t
    :data '("balsam" "bark" "bulbs" "buds" "cones"
             "flowers" "fruits" "galls" "gum" "juice"
             "leaves" "petals" "pollen" "resin" "rhizomes"
             "root" "seeds" "stems" "timber" "tubers"))

  (random-table/register :name "Plant > Method (Herbalist's Primer)"
    :private t
    :data '("applied to an inanimate object" "burned as incenses" "carried" "chewed" "distilled"
             "drank as tea" "grown" "held under the tongue" "ingested" "juiced and injected"
             "mixed with alchohol" "place under a pillow" "powdered and inhaled" "rubbed on skin" "scattered on the wind"
             "sewn inside clothing" "swallowed whole" "thrown at a target" "torn or shredded" "worn"))

  (random-table/register :name "Plant > Effect > Medicinal (Herbalist's Primer)"
    :private t
    :data '("aid digestion" "alleviate allergy" "cure wounds" "destroy viruses" "fight the flu"
             "improve focus" "kill bacteria" "lower blood pressure" "mend bones" "neutralize poison"
             "reduce inflammation" "remove itching" "remove nausea" "remove pain" "sanitize the wound"
             "soothe the skin" "stop bleeding" "stop coughing" "strengthen the heart" "strenthen the immune system"))

  (random-table/register :name "Plant > Effect > Poisonous (Herbalist's Primer)"
    :private t
    :data '("cause acute pain" "cause bleeding" "cause coma" "cause contact allergy" "cause death"
             "cause diarrhea" "cause dizziness" "cause hallucinations" "cause itching"
             "cause nausea" "cause neurological damage" "cause paralysis" "cause swelling" "cause violent spasms"
             "raise blood pressure" "remove a sense" "stop breathing" "stop the heart" "turn blood to ichor"))

  (random-table/register :name "Plant > Effect > Magical (Herbalist's Primer)"
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

  (random-table/register :name "Plant > Complication (Herbalist's Primer)"
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


;;; House Rules
;;;; Goblin Punch Death and Dismemberment
;; See https://drive.google.com/file/d/0BxVHEMMjLlZ4cFVJTEFEcW9WV0U/view?resourcekey=0-matru4XOnZc-kjaQtiEX3Q
(defun random-table/roller/death-and-dismemberment/damage (table)
  (+ (read-number "Number of Existing Injuries: " 0)
    (read-number "Lethal Damage: " 0)
    (random-table/roller/1d12 table)))

(random-table/register :name "Death and Dismemberment"
  :roller #'random-table/roller/prompt-from-table-data
  :data '(("Physical" . "${Death and Dismemberment > Physical}")
           ("Acid/Fire" . "${Death and Dismemberment > Acid/Fire}")
           ("Eldritch" . "${Death and Dismemberment > Eldritch}")
           ("Lightning" . "${Death and Dismemberment > Lightning}")
           ("Non-Lethal" . "${Death and Dismemberment > Non-Lethal}")))

(random-table/register :name "Death and Dismemberment > Eldritch"
  :roller #'random-table/roller/death-and-dismemberment/damage
  :private t
  :data '(((1 . 10) . "Rolled ${current_roll}\n- +1 Injury\n- Anathema for +${current_roll} day(s).")
           ((11 . 15) . "Rolled ${current_roll}\n- +1 Injury\n- Anathema for +${current_roll} day(s).\n- One Fatal Wound.\n- Save vs. Curse.")
           ((16 . 1000) . "Rolled ${current_roll}\n- +1 Injury\n- Anathema for +${current_roll} day(s).\n- ${current_roll} - 14 Fatal Wounds.\n- Save vs. Curse.")))

(random-table/register :name "Death and Dismemberment > Acid/Fire"
  :roller #'random-table/roller/death-and-dismemberment/damage
  :private t
  :data '(((1 . 10) . "Rolled ${current_roll}\n- +1 Injury\n- Burned for +${current_roll} day(s).")
           ((11 . 15) . "Rolled ${current_roll}\n- +1 Injury\n- Burned for +${current_roll} day(s).\n- One Fatal Wound.\n- Save vs. Blind.")
           ((16 . 1000) . "Rolled ${current_roll}\n- +1 Injury\n- Burned for +${current_roll} day(s)\n- ${current_roll} - 14 Fatal Wounds.\n- Save vs. Blind.")))

(random-table/register :name "Death and Dismemberment > Lightning"
  :roller #'random-table/roller/death-and-dismemberment/damage
  :private t
  :data '(((1 . 10) . "Rolled ${current_roll}\n- +1 Injury\n- Burned for +${current_roll} day(s).")
           ((11 . 15) . "Rolled ${current_roll}\n- +1 Injury\n- Burned for +${current_roll} day(s).\n- One Fatal Wound.\n- Save vs. Deaf.")
           ((16 . 1000) . "Rolled ${current_roll}\n- +1 Injury\n- Burned for +${current_roll} day(s)\n- ${current_roll} - 14 Fatal Wounds.\n- Save vs. Deaf.")))

(random-table/register :name "Death and Dismemberment > Physical"
  :roller #'random-table/roller/1d6
  :private t
  :data '(((1) . "Death and Dismemberment > Physical > Arm")
           ((2) . "Death and Dismemberment > Physical > Leg")
           ((3 . 4) . "Death and Dismemberment > Physical > Torso")
           ((5 . 6) . "Death and Dismemberment > Physical > Head")))

(random-table/register :name "Death and Dismemberment > Physical > Arm"
  :roller #'random-table/roller/death-and-dismemberment/damage
  :private t
  :data '(((1 . 10) . "Arm Injury; Rolled ${current_roll}\n- +1 Injury\n- Arm disabled for +${current_roll} day(s).")
           ((11 . 15) . "Arm Injury; Rolled ${current_roll}\n- +1 Injury\n- Arm disabled for +${current_roll} day(s).\n- One Fatal Wound.\n- ${Save vs. Mangled Arm}.")
           ((16 . 1000) . "Arm Injury; Rolled ${current_roll}\n- +1 Injury\n- Arm disabled for +${current_roll} day(s).\n- ${current_roll} - 14 Fatal Wounds.\n- ${Save vs. Mangled Arm}")))

(random-table/register :name "Death and Dismemberment > Physical > Leg"
  :roller #'random-table/roller/death-and-dismemberment/damage
  :private t
  :data '(((1 . 10) . "Leg Injury; Rolled ${current_roll}\n- +1 Injury\n- Leg disabled for +${current_roll} day(s).")
           ((11 . 15) . "Leg Injury; Rolled ${current_roll}\n- +1 Injury\n- Leg disabled for +${current_roll} day(s).\n- One Fatal Wound.\n- ${Save vs. Mangled Leg}")
           ((16 . 1000) . "Leg Injury; Rolled ${current_roll}\n- +1 Injury\n- Leg disabled for +${current_roll} day(s).\n- ${current_roll} - 14 Fatal Wounds.\n- ${Save vs. Mangled Leg}")))

(random-table/register :name "Death and Dismemberment > Physical > Torso"
  :roller #'random-table/roller/death-and-dismemberment/damage
  :private t
  :data '(((1 . 10) . "Torso Injury; Rolled ${current_roll}\n- +1 Injury\n- Blood loss for +${current_roll} day(s).")
           ((11 . 15) . "Torso Injury; Rolled ${current_roll}\n- +1 Injury\n- Blood loss for +${current_roll} day(s).\n- One Fatal Wound.\n- ${Save vs. Crushed Torso}")
           ((16 . 1000) . "Torso Injury; Rolled ${current_roll}\n- +1 Injury\n- Blood loss for +${current_roll} day(s).\n- ${current_roll} - 14 Fatal Wounds.\n- ${Save vs. Crushed Torso}")))

(random-table/register :name "Death and Dismemberment > Physical > Head"
  :roller #'random-table/roller/death-and-dismemberment/damage
  :private t
  :data '(((1 . 10) . "Head Injury; Rolled ${current_roll}\n- +1 Injury\n- Concussed for +${current_roll} day(s).")
           ((11 . 15) . "Head Injury; Rolled ${current_roll}\n- +1 Injury\n- Concussed for +${current_roll} day(s).\n- One Fatal Wound.\n- ${Save vs. Skullcracked}")
           ((16 . 1000) . "Head Injury; Rolled ${current_roll}\n- +1 Injury\n- Concussed for +${current_roll} day(s).\n- ${current_roll} - 14 Fatal Wounds.\n- ${Save vs. Skullcracked}")))

(random-table/register :name "Death and Dismemberment > Non-Lethal"
  :roller #'random-table/roller/death-and-dismemberment/damage
  :private t
  :data '(((1 . 1000) . "Non-Lethal Injury; Rolled ${current_roll}\n- +1 Injury\n- Knocked out for +${current_roll} round(s).")))

(random-table/register :name "Save vs Mangled Arm"
  :roller #'random-table/roller/saving-throw
  :private t
  :data '(("Save" . "Saved against losing an arm…lose a finger instead.")
          ("Fail" . "Failed to save against losing or permanently disabling an arm.")))

(random-table/register :name "Save vs. Mangled Leg"
  :roller #'random-table/roller/saving-throw
  :private t
  :data '(("Save" . "Saved against losing a leg…lose a toe instead.")
          ("Fail" . "Failed to save against losing or permanently disabling a leg.")))

(random-table/register :name "Save vs. Crushed Torso"
  :roller #'random-table/roller/saving-throw
  :private t
  :data '(("Save" . "Saved against crushed torso…gain a new scar.")
           ("Fail" . "Failed to save against crushed torso.  ${Save vs. Crushed Torso > Failure}")))

(random-table/register :name "Save vs. Crushed Torso > Failure"
  :private t
  :data '("Permanently lose 1 Strength." "Permanently lose 1 Dexterity." "Permanently lose 1 Constitution."
           "Crushed throat. You cannot speak louder than a whisper."
           "Crushed ribs. Treat Con as 4 when holding your breath."
           "Your spine is broken and you are paralyzed from the neck down. You can recover from this by making a Con check after 1d6 days, and again after 1d6 weeks if you fail the first check. If you fail both, it is permanent."))

(random-table/register :name "Save vs. Skullcracked"
  :roller #'random-table/roller/saving-throw
  :private t
  :data '(("Save" . "Saved against cracked skull…gain a new scar.")
           ("Fail" . "Failed to save against cracked skull.  ${Save vs. Skullcracked > Failure}")))

(random-table/register :name "Save vs. Skullcracked > Failure"
  :private t
  :data '("Permanently lose 1 Intelligence." "Permanently lose 1 Wisdom." "Permanently lose 1 Charisma."
           "Lose your left eye. -1 to Ranged Attack."
           "Lose your right eye. -1 to Ranged Attack."
           "Go into a coma. You can recover from a coma by making a Con check after 1d6 days, and again after 1d6 weeks if you fail the first check. If you fail both, it is permanent."))

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
  :data '("- Action :: ${Oracle > Action}\n- Theme :: ${Oracle > Theme}"))

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
  :data '("${Plot Twist > Subject} ${Plot Twist > Event}"))

(random-table/register :name "Plot Twist > Subject"
  :private t
  :data '("An NPC" "Your PC" "An organization"
           "A physical event" "An emotional event" "An item"))

(random-table/register :name "Plot Twist > Event"
  :private t
  :data '("appears." "alters the location." "helps the hero."
           "hinders the hero." "changes the goal." "ends the scene."))

(random-table/roller :label "2d4"
  (+ 2 (random 4) (random 4)))

(random-table/roller :label "d66"
  (+ (* 10 (+ 1 (random 6))) (+ 1 (random 6))))

(random-table/register :name "How Far Away is a Thing > Same Place"
  :roller #'random-table/roller/2d4
  :data '((2 . "Almost touching you")
           (3 . "Within arm's reach")
           (4 . "Just steps away")
           (5 . "just around the corner")
           (6 . "in the next room")
           (7 . "a few levels up (or down…)")
           (8 . "Somehwere in the dungeon")))

(random-table/register :name "How Far Away is a Thing > Distant Thing"
  :roller #'random-table/roller/2d4
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
  :data '("\n- Answer :: ${Ask the Stars > Answer}\n- Symbol :: ${Ask the Stars > Symbol}\n- Position :: ${Ask the Stars > Position}"))

(random-table/register :name "Ask the Stars > Answer"
  :roller #'random-table/roller/1d12
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
  :roller #'random-table/roller/2d6
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

(random-table/register :name "Reaction Roll (OSE)"
  :roller (lambda (table)
            (+ (random-table/roller/2d6 table)
              (string-to-number (completing-read "Charisma Modifier: "
                                  '("-2" "-1" "0" "1" "2") nil t))
              (string-to-number (completing-read "Additional Modifier: "
                                  '("-2" "-1" "0" "1" "2") nil t "0"))
              ))
  :data '(((-2 . 2) . "\n- Reaction :: Attacks")
           ((3 . 5) . "\n- Reaction :: Hostile, may attack\n- Motivation :: ${Monster Motivation (OSE)}")
           ((6 . 8) . "\n- Reaction :: Uncertain, confused\n- Motivation :: ${Monster Motivation (OSE)}")
           ((9 . 11) . "\n- Reaction :: Indifferent, may negotiate\n- Motivation :: ${Monster Motivation (OSE)}")
           ((12 . 16) . "\n- Reaction :: Eager, friendly")))

(random-table/register :name "Monster Motivation (OSE)"
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

(random-table/register :name "Random Dungeon Content (OSE)"
  :roller #'random-table/roller/d66
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
  :data '("The Immediate Removal of ${AGoPD (d10)}."
           "The Violent Eradicator of ${AGoPD (d10)}."
           "The Animation of ${AGoPD (d10)}."
           "Reverse ${AGoPD (d8)}."
           "Symmetrical yet Unstable ${AGoPD (d8)}."
           "Blatant and Ignited ${AGoPD (d8)}."
           "An Unexpected Summoning ${AGoPD (d6)}."
           "The Effulgent Translocation ${AGoPD (d6)}."
           "Pandemonic Ramming ${AGoPD (d6)}."
           "The Haunting of a Door by ${AGoPD (d4)}."
           "The Possessing of the Caster by ${AGoPD (d4)}."
           "The Efficacious Repelling of ${AGoPD (d4)}."))

(random-table/register :name "AGoPD (d10)"
  :private t
  :data '("Locks & Bolts" "Hinges & Rails" "Handles & Knobs"
     "Glyphs & Runes" "Door-steps" "Door-panels" "Door-mats"
     "Door-frames" "Knockers" "Peep-holes"))

(random-table/register :name "AGoPD (d8)"
  :private t
  :data '("By-passing of ${AGoPD (d10)}"
           "Disappearance of ${AGoPD (d10)}"
           "Transmutation of ${AGoPD (d10)}"
           "Impotence"
           "Exhibition"
           "Walling"
           "Dead-bolting"
           "Entrance"))

(random-table/register :name "AGoPD (d6)"
  :private t
  :data
  '("of Helpful ${AGoPD (d4)}" "of Silver ${AGoPD (d10)}" "of the Magic-User"
     "Applied to One’s Enemies" "as Protection" "In a Delayed Fashion"))

(random-table/register :name "AGoPD (d4)"
  :private t
  :data '("Cat-headed Things" "Heralds of the Termite People"
           "A Fae Porter-Knight" "The Ghost of Katastroph"))

(random-table/register :name "NPC > Immediate Desire (Scarlet Heroes)"
  :data '("Aiding a friend" "Avenging a slight" "Bribing an official"
           "Buying an object" "Collecting a bribe" "Collecting a debt"
           "Commit a crime" "Curing a sickness" "Destroying evidence"
           "Earning money" "Funding a funeral" "Getting a document"
           "Getting drunk" "Going home to rest" "Having a man/woman"
           "Helping a relative" "Impressing a lover" "Impressing the boss"
           "Paying a debt" "Recovering a lost item" "Revealing a secret"
           "Selling an object" "Spreading a faith" "Spying on a person"
           "Stealing from the boss"))

(random-table/register :name "NPC > Ruling Temperament (Scarlet Heroes)"
  :data '("Ambitious" "Bigoted" "Capricious"
           "Cautious" "Compassionate" "Deceitful"
           "Exhibitionistic" "Fearful" "Garrulous"
           "Greedy" "Indecisive" "Inquisitive"
           "Lazy" "Loyal" "Lustful"
           "Merciful" "Observant" "Patient"
           "Proud" "Scornful" "Shy"
           "Stubborn" "Valorous" "Vicious"
           "Wrathful"))

(random-table/register :name "NPC > Memorable Traits (Scarlet Heroes)"
  :data '("Always carries things" "Always hurried" "Asthmatic"
           "Blind in an eye" "Careless dresser" "Constantly watchful"
           "Dark, sober clothes" "Deaf or hard of hearing" "Elaborate tattoos"
           "Emphatic speech" "Facial scarring" "Gaudy jewelry"
           "Immaculate clothes" "Laconic speaker" "Magnificent hair"
           "Missing an appendage" "Numerous piercings" "Often drunk"
           "Out of shape" "Precise hands" "Shaven or balding"
           "Stutters" "Subtle fragrance" "Tends work constantly"
           "Twitches regularly"))

(random-table/register :name "Actor > Commoner (Scarlet Heroes)"
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

(random-table/register :name "Actor > Underworld (Scarlet Heroes)"
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

(random-table/register :name "Actor > Elite and Noble (Scarlet Heroes)"
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

(random-table/register :name "Actor > Relationship (Scarlet Heroes)"
  :data '("Business partner" "Child" "Childhood friend" "Co-workers" "Cousin"
           "Crime culprit" "Crime partner" "Crime victim" "Ex-lover" "Ex-spouse"
           "Grandparent" "Has blackmail" "Heir to something" "Inlaws" "Lover"
           "Old favor" "Parent" "Rival" "Schoolmates" "Sibling"
           "Society fellows" "Spouse" "Subordinate" "Superior" "Uncle/Aunt"))

(defvar random-table/prompts/reactions-scarlet-heroes
  '(("-3 for grave insults or risks to the life of self or loved ones" . -3)
     ("-2 for insults or risks to the NPC’s wealth or standing" . -2)
     ("-1 for the risk of significant cost to their actions" . -1)
     ("+1 for a modest bribe or when a favor is owed to a PC" . 1)
     ("+2 for a large bribe or significant service owed" . 2)
     ("+3 for a PC who saved their life or did similar service" . 3)))

(defun random-table/roller/reactions-scarlet-heroes (table)
  "Roller for the Scarlet Hereos TABLE."
  (let* ((charisma-modifier (random-table/prompt "Charisma Modifier"))
          (reaction-modifier (random-table/prompt "Reaction Modifier")))
    (+ reaction-modifier charisma-modifier (random-table/roller/2d6 table))))

(random-table/register :name "Reactions > Friendly (Scarlet Heroes)"
  :roller #'random-table/roller/reactions-scarlet-heroes
  :data '(((-9 . 1) . "${Reactions > Stranger (Scarlet Heroes)}")
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

(random-table/register :name "Reactions > Stranger (Scarlet Heroes)"
  :roller #'random-table/roller/reactions-scarlet-heroes
  :data '(((-9 . 1) . "${Reactions > Unfriendly (Scarlet Heroes)}")
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

(random-table/register :name "Reactions > Unfriendly (Scarlet Heroes)"
  :roller #'random-table/roller/reactions-scarlet-heroes
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
