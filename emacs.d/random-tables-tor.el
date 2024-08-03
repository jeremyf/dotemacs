(if (f-file?  "~/git/random-table.el/random-table.el")
  (require 'random-table "~/git/random-table.el/random-table.el")
  (use-package random-table
    :straight (:host github :repo "jeremyf/random-table.el")))

(setq random-table/reporter #'random-table/reporter/as-insert)

(random-table/register :name "The One Ring > Lore"
  :data '("{The One Ring > Lore @label}{The One Ring > Lore @options}")
  :label '(read-string "Question: ")
  :options '(("Action" . "- Action :: {The One Ring > Lore Table > Action}")
              ("Aspect" . "- Aspect :: {The One Ring > Lore Table > Aspect}")
              ("Feature" . "- Feature :: {The One Ring > Lore Table > Feature}")
              ("Focus" . "- Focus :: {The One Ring > Lore Table > Focus}")))

(random-table/register :name "The One Ring > Lore Table > Action"
  :private t
  :data '("⏿ Abandon" "⏿ Attack" "⏿ Betray" "⏿ Corrupt" "⏿ Defeat" "⏿ Weaken" ;; Eye of Sauron
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
           "ᚠ Believe" "ᚠ Bolster" "ᚠ Defend" "ᚠ Forgive" "ᚠ Resist" "ᚠ Strengthen"))

(random-table/register :name "The One Ring > Lore Table > Aspect"
  :private t
  :data '("⏿ Corrupted" "⏿ Cruel" "⏿ Deceptive" "⏿ Fell" "⏿ Ruined" "⏿ Treacherous" ;; Eye of Sauron
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
           "ᚠ Flourishing" "ᚠ Beautiful" "ᚠ Good" "ᚠ Kind" "ᚠ Gentle" "ᚠ Wondrous"))

(random-table/register :name "The One Ring > Lore Table > Feature"
  :private t
  :data '("⏿ Darkness" "⏿ Ruin" "⏿ Blood" "⏿ Bones" "⏿ Corpse" "⏿ Trap" ;; Eye of Sauron
                "Archive" "Armament" "Barricade" "Battlefield" "Bridge" "Cave-in"
                "Chill" "Container" "Creature" "Dead-end" "Debris" "Doorway"
                "Dust" "Echoes" "Encampment" "Enchantment" "Excavation" "Fire"
                "Fissure" "Fortification" "Gate" "Ghost" "Heat" "Heights"
                "Hideaway" "Illusion" "Inscription" "Lair" "Machinery" "Mist"
                "Monument" "Nest" "Opening" "Person" "Pillar" "Pit"
                "Provisions" "Puzzle" "Rubbish" "Runes" "Scratches" "Silence"
                "Smoke" "Sound" "Stairs" "Stench" "Stone" "Tomb"
                "Tool" "Trail" "Vault" "Viewpoint" "Vision" "Voice"
                "Wall" "Warren" "Water" "Whispers" "Wind" "Wood"
           "ᚠ Artefact" "ᚠ Artwork" "ᚠ Illumination" "ᚠ Plants" "ᚠ Shelter" "ᚠ Treasure"))

(random-table/register :name "The One Ring > Lore Table > Focus"
  :private t
  :data '("⏿ Curse" "⏿ Despair" "⏿ Enemy" "⏿ Fear" "⏿ Shadow" "⏿ War" ;; Eye of Sauron
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
           "ᚠ Courage" "ᚠ Duty" "ᚠ Fellowship" "ᚠ Hope" "ᚠ Love" "ᚠ Peace"))

(random-table/register :name "The One Ring > Chance Meeting"
  :data '("\n\t- Encounter :: {The One Ring > Chance Meeting > Encounter}\n\t- Temperament :: {The One Ring > Chance Meeting > Termperament}\n\t- Focus :: {The One Ring > Chance Meeting > Focus}"))

(random-table/register :name "The One Ring > Chance Meeting > Encounter"
  :private t
  :data '("A Deadly Foe" "Large Group of Adversaries" "Small Group of Adversaries"
           "Small Group of Adversaries" "Small Group of Adversaries" "Potential Adversaries"
           "Travelers" "Travelers" "Merchants"
           "Merchants" "Friendly Faces" "A Known Ally"))

(random-table/register :name "The One Ring > Chance Meeting > Termperament"
  :private t
  :data '("Raging" "Paranoid" "Shadowed" "Weary" "Wary" "Lost"
           "Mysterious" "Determined" "Peaceful" "Cheerful" "Corrupted" "Inspiring"))

(random-table/register :name "The One Ring > Chance Meeting > Focus"
  :private t
  :data '("Engaged in Combat" "Lying in Wait" "Hunting Something"
           "Engaged in Conversation" "On a Long March" "Wandering Aimlessly"
           "Exploring a Ruin" "Setting up Camp" "Eating a Meal"
           "Recovering from Battle" "In a Bout of Madness" "Resting Peacfully"))

(random-table/register :name "The One Ring > Event"
  ;; TODO Add a favorability selector
  :data '((1 . "Terrible Misfortune: {The One Ring > Event > Terrible Misfortune}\n\t- Consequence :: If the roll fails, the target is Wounded.\n\t- Fatigue :: 3")
           (2 . "Despair: {The One Ring > Event > Despair}\n\t- Consequence :: If the roll fails, gain 2 Shadow points (Dread).\n\t- Fatigue :: 2")
           ((3 . 4) . "Ill Choices: {The One Ring > Event > Ill Choices}\n\t- Consequence :: If the roll fails, gain 1 Shadow point (Dread).\n\t- Fatigue :: 2")
           ((5 . 8) . "Mishap: {The One Ring > Event > Mishap}\n\t- Consequence :: If the roll fails, add 1 day to the length of the journey, and gain 1 additional Fatigue.\n\t- Fatigue :: 2")
           ((9 . 10) . "Short Cut: {The One Ring > Event > Short Cut}\n\t- Consequence :: If the roll succeeds, reduce the length of the journey by 1 day.\n\t- Fatigue :: 1")
           (11 . "Chance Meeting: {The One Ring > Event > Chance Meeting}\n\t- Consequence :: If the roll succeeds, no Fatigue is gained, and you may envision a favourable encounter.\n\t- Fatigue :: 1")
           (12 . "Joyful Sight: {The One Ring > Event > Joyful Sight}\n\t- Consequence :: If the roll succeeds, regain 1 Hope.\n\t- Fatigue :: 0")))

(random-table/register :name "The One Ring > Event > Terrible Misfortune"
  :private t
  :data '("Dire confrontation\n\t- Task :: Noteworthy Encounter"
    "Rival Predator\n\t- Task :: HUNTING to avoid becoming the hunted"
    "Violent weather\n\t- Task :: EXPLORE to find shelter"
    "Hidden hazard\n\t- Task :: AWARENESS to avoid stumbling into danger"
    "Dangerous terrain\n\t- Task :: EXPLORE to find a safer route"
    "Stalking enemy\n\t- Task :: AWARENESS to spot the foul presence"))
(random-table/register :name "The One Ring > Event > Despair"
  :private t
  :data '("Servants of the Enemy\n\t- Task :: Noteworthy Encounter"
    "Torrential weather\n\t- Task :: EXPLORE to find the least exposed path"
    "Nightmarish presence\n\t- Task :: AWARENESS to sense the danger"
    "Fading vigour\n\t- Task :: HUNTING to gain sustenance"
    "Corrupted site\n\t- Task :: EXPLORE to find your way out"
    "Grisly scene or foreboding portent\n\t- Task :: AWARENESS to be forewarned"))
(random-table/register :name "The One Ring > Event > Mishap"
  :private t
  :data '("Sparse wildlife\n\t- Task :: HUNTING to forage what you can"
    "Lost direction\n\t- Task :: EXPLORE to find your way"
    "Obstructed path\n\t- Task :: AWARENESS to spot a way around"
    "Elusive quarry\n\t- Task :: HUNTING to track it down"
    "Rough terrain\n\t- Task :: EXPLORE to safely traverse"
    "Wandering enemies\n\t- Task :: AWARENESS to sense their coming"))
(random-table/register :name "The One Ring > Event > Ill Choices"
  :private t
  :data '("Mismanaged provisions\n\t- Task :: HUNTING to replenish stores"
    "Wayward path\n\t- Task :: EXPLORE to retrace your steps"
    "Overlooked hazard\n\t- Task :: AWARENESS to escape safely"
    "Lost quarry\n\t- Task :: HUNTING to follow its tracks"
    "Disorienting environs\n\t- Task :: EXPLORE to find your way"
    "Haunting visions\n\t- Task :: AWARENESS to over- come darkness"))
(random-table/register :name "The One Ring > Event > Short Cut"
  :private t
  :data '("Game trail\n\t- Task :: HUNTING to traverse the path"
    "Secluded path\n\t- Task :: EXPLORE to navigate the wilds"
    "Helpful tracks\n\t- Task :: AWARENESS to follow the tracks"
    "Animal guide\n\t- Task :: HUNTING to follow at a distance"
    "Favourable weather\n\t- Task :: EXPLORE to make the most of it"
    "Familiar waypoint\n\t- Task :: AWARENESS to recognize the landmark"))
(random-table/register :name "The One Ring > Event > Chance Meeting"
  :private t
  :data '("Lone hunter\n\t- Task :: HUNTING to trade stories"
    "Fellow traveller\n\t- Task :: EXPLORE to learn about the path ahead"
    "Discreet watcher\n\t- Task :: AWARENESS to spot them"
    "Noble beast\n\t- Task :: HUNTING to commune"
    "Secluded encampment\n\t- Task :: EXPLORE to find your way off the beaten path"
    "Auspicious gathering\n\t- Task :: Noteworthy Encounter"))
(random-table/register :name "The One Ring > Event > Joyful Sight"
  :private t
  :data '("Majestic creatures\n\t- Task :: HUNTING to observe without startling them"
    "Inspiring vista\n\t- Task :: EXPLORE to reach a vantage point"
    "Benevolent being\n\t- Task :: AWARENESS to sense their presence"
    "Abundant foraging\n\t- Task :: HUNTING to replenish your rations"
    "Ancient monument\n\t- Task :: AWARENESS to recognize its significance"
    "Peaceful sanctuary\n\t- Task :: Noteworthy Encounter"))
