;;; jf-tables --- Roll on some tables. -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package provides a means of registering random tables (see
;; `random-table' and `random-table/register') and then rolling on those tables
;; (see `random-table/roll').
;;
;; The `random-table/roll' is an `interactive' function that will prompt you to
;; select an expression.  You can choose from a list of registered public tables
;; or provide your own text.  This package uses the `s-format' to parse the
;; given expression.
;;
;; Examples:
;; - (random-table/roll "2d6") will roll 2 six-sided dice.
;; - (random-table/roll "There are ${2d6} orcs.") will roll 2 six-sided dice and
;;   output the sentence "There are 7 orcs."
;;
;; Tables can reference other tables, using the above string interpolation
;; (e.g. "Roll on ${Your Table}" where "Your Table" is the name of a registered
;; table.).  No considerations have been made to check for cyclical references,
;; you my dear human reader, must account for that.  An example is the
;; "Oracle Question (Black Sword Hack)" table.  It has one entry:
;;
;; "${Oracle Question > Answer (Black Sword Hack)}${Oracle Question > Unexpected Event (Black Sword Hack)}".
;;
;; When we roll "Oracle Question (Black Sword Hack)", we then roll on the two
;; sub-tables.  Which can also leverage the template interpolation.

;;; Code:

;;;; Requirements:
(require 'org-d20)
(require 'cl)

;;;; Data Structures and Storage
(cl-defstruct random-table
  "The definition of a structured random table.

I roll the dice, filter the results, and fetch from the table.
The `random-table/evaluate/table' defines the steps we take to
\"roll on the table.\"

The slots are:

- :name :: the human readable and reference-able name (used for
  completing read and the key for the table storage).
- :data :: the tabular data, often as a list of strings.  By
  design, those list of strings can have interpolation
 (e.g. \"${2d6}\" both of dice structures but also of other
  tables.
- :roller :: function to roll dice and return list of dice results.
- :filter :: function to filter the list of dice.
- :fetcher :: function that takes two positional arguments (see
  `random-table/fetcher/default'.); it is used to fetch the correct entry
  from the table.
- :private :: when true, do not show in list of rollable tables.
- :store :: When non-nil, we store the roller's value for the
  duration of the table evaluation.  Useful for when you have one
  roll that you use for multiple tables.
- :reuse :: the :name of a table's stored dice results.

About :reuse and :store

There are cases where we want to use one set of dice roles.  For
example, in the \"Oracle (Black Sword Hack)\" table we roll dice
and use those dice results to determine both the answer as well
as whether there are unexpected events.  All from the same roll."
  name
  data
  (roller #'random-table/roller/default)
  (filter #'random-table/filter/default)
  (fetcher #'random-table/fetcher/default)
  (private nil)
  (store nil)
  (reuse nil))

(defvar random-table/storage/results
  (make-hash-table)
  "An ephemeral storage for dice results of rolling for a table.

As part of the rolling, we both add to and remove those stored
values; that is to say functions are responsible for clean-up.
See `random-table' for discussion about storage and reuse.")

(defvar random-table/storage/tables
  (make-hash-table)
  "A hash-table of random tables.

The hash key is the \"human readable\" name of the table (as a symbol).
The hash value is the contents of the table.")

(defun random-table/roller/default (&rest data)
  "Roll for the given DATA."
  ;; Constant off by one errors are likely
  (+ 1 (random (length (-list data)))))

(defun random-table/filter/default (&rest rolls)
  "Filter the given ROLLS.

See `random-table/roller/default'."
  (apply #'+ (-list rolls)))

(defun random-table/fetcher/default (data &optional position)
  "Find POSITION on the given DATA.

When POSITION is not given, choose a random element from the TABLE."
  (if (integerp position)
    ;; Off by one errors are so very real.
    (nth (- position 1) data)
    (seq-random-elt data)))

(defvar random-table/reporter
  #'random-table/reporter/as-kill-and-message
  "The function takes two positional parameters:

- EXPRESSION :: The text to evaluate for \"rolling\"
- RESULT :: The results of those rolls.

See `random-table/reporter/as-kill-and-message'.")

(defun random-table/reporter/as-kill-and-message (expression result)
  "Responsible for reporting the EXPRESSION and RESULT.

See `random-table/reporter'."
  (let ((text (format "%s :: %s" expression result)))
    (kill-new text)
    (message text)))

;;;; Interactive
(defun random-table/roll (text)
  "Evaluate the given TEXT by \"rolling\" it.

This can either be a named table or a general text (e.g. 2d6).

Or a combination of multiple tables.

We report that function via `#'random-table/reporter'."
  (interactive (list (completing-read "Expression: "
                       random-table/storage/tables
                       ;; Predicate that filters out non-private tables.
                       (lambda (name table &rest args) (not (random-table-private table))))))
  ;; TODO: Consider allowing custom reporter as a function.  We already register
  ;; it in the general case.
  (apply random-table/reporter
    (list text (random-table/roll/text text))))

;; TODO Rename this; I'm not satisfied and want to refactor.
(defun random-table/roll/text (text)
  "Roll the given TEXT; either by evaluating as a `random-table' or via `s-format'."
  (if-let* ((table (random-table/get-table text :allow_nil t)))
    (random-table/evaluate/table table)
    ;; We have specified a non-table; roll the text.  We'll treat a non-escaped on as a dice text.
    (progn
      (s-format (if (string-match-p "\\${" text) text (concat "${" text "}"))
        #'random-table/roll/text/replacer))))

(defun random-table/roll/text/replacer (text)
  "Roll the TEXT; either from a table or as a dice-expression.

This is constructed as the replacer function of `s-format'."
  (if-let ((table (random-table/get-table text :allow_nil t)))
    (random-table/evaluate/table table)
    (let ((result (org-d20--roll text)))
      ;; Need to sniff out if org-d20--roll knew what to do.
      (if (or (not (s-present? (car result))) (string= (car result) "0"))
        text
        (format "%s" (cdr result))))))

(defun random-table/evaluate/table (table)
  "Evaluate the random TABLE.

See `random-table'.  "
  (let* ((rolled (random-table/evaluate/table/roll table))
          (name (random-table-name table))
          (data (random-table-data table))
          (filtered (apply (random-table-filter table) (-list rolled)))
          (entry (if filtered (apply (random-table-fetcher table) (list data (-list filtered)))
                   nil))
          (results (or (when entry (random-table/roll/text entry)) "")))
    (remhash (random-table-name table) random-table/storage/results)
    results))

(defun random-table/evaluate/table/roll (table)
  "Roll on the TABLE, favoring re-using and caching values.

Why cache values?  Some tables you roll one set of dice and then
use those dice to lookup on other tables."
  (let ((results
          (or (when-let ((reuse-table-name (random-table-reuse table)))
                (or
                  (gethash (intern reuse-table-name) random-table/storage/results)
                  (random-table/evaluate/table/roll
                    (random-table/get-table reuse-table-name))))
            (apply (random-table-roller table) (-list (random-table-data table))))))
    (when-let ((stored-table-name (random-table-store table)))
      (puthash (random-table-name table) results random-table/storage/results))
    results))

(cl-defun random-table/get-table (value &key allow_nil)
  "Coerce the given VALUE to a registered `random-table'.

When the given VALUE cannot be found in the
`random-table/stroage/tables' registry we look to ALLOW_NIL.

When ALLOW_NIL is non-nil, we return `nil' when no table is found
in `random-table/stroage/tables' registry.

When ALLOW_NIL is `nil' we raise an `error' when no table was
found in the `random-table/stroage/tables' registry."
  (if-let ((table (cond
    ((random-table-p value)
      value)
    ((symbolp value)
      (gethash value random-table/storage/tables))
    ((stringp value)
      (gethash (intern value) random-table/storage/tables))
    (t
      (error "Expected %s to be a `random-table', `symbol', or `string' got %s."
        value
        (type-of value))))))
    table
    (unless allow_nil
    (error "Could not find table %s; use `random-table/register'." value))))

;;; Register Tables
(cl-defun random-table/register (&rest kws &key name data &allow-other-keys)
  "Store the DATA, NAME, and KWS in a `random-table'."
  (let* ((key (intern name))
          (struct (apply #'make-random-table :name key :data (-list data) kws)))
    (puthash key struct random-table/storage/tables)))

;;;; Errant
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

;;;; Black Sword Hack
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
  (let ((likelihood (completing-read "Likelihood: " jf/gaming/black-sword-hack/table/oracle-question-likelihood nil t)))
    (funcall (alist-get likelihood jf/gaming/black-sword-hack/table/oracle-question-likelihood nil nil #'string=))))

(random-table/register :name "Attributes (OSR)"
  :data '("\n- Strength :: ${3d6}\n- Dexterity :: ${3d6}\n- Constitution :: ${3d6}\n- Intelligence :: ${3d6}\n- Wisdom :: ${3d6}\n- Charisma :: ${3d6}"))

(random-table/register :name "Attributes (Black Sword Hack)"
  :data '("\n- Strength :: ${4d4}\n- Dexterity :: ${4d4}\n- Constitution :: ${4d4}\n- Intelligence :: ${4d4}\n- Wisdom :: ${4d4}\n- Charisma :: ${4d4}"))

(random-table/register :name "Oracle Question (Black Sword Hack)"
  :roller #'random-table/roller/oracle-question
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
  :fetcher (lambda (table index)
             (when index (concat " with unexpected \"" (nth (- (car (-list index)) 1) table) "\" event")))
  :data '("Very negative" "Negative" "Negative but…" "Positive but…" "Positive" "Very Positive"))

(random-table/register :name "Arabic Name"
  :data '("${Arabic Name > Male Given Name} ${Arabic Name > Surname}"
           "${Arabic Name > Female Given Name} ${Arabic Name > Surname}"))

(random-table/register :name "Arabic Name > Male Given Name"
  :private t
  :data '("Aamir" "Ayub" "Binyamin" "Efraim" "Ibrahim"
           "Ilyas" "Ismail" "Jibril" "Jumanah" "Kazi"
           "Lut" "Matta" "Mohammed" "Mubarak" "Mustafa"
           "Nazir" "Rahim" "Reza" "Sharif" "Taimur"
           "Usman" "Yakub" "Yusuf" "Zakariya" "Zubair"))

(random-table/register :name "Arabic Name > Female Given Name"
  :private t
  :data '("Aisha" "Alimah" "Badia" "Bisharah" "Chanda"
           "Daliya" "Fatimah" "Ghania" "Halah" "Kaylah"
           "Khayrah" "Layla" "Mina" "Munisa" "Mysha"
           "Naimah" "Nissa" "Nura" "Parveen" "Rana"
           "Shalha" "Suhira" "Tahirah" "Yasmin" "Zulehka"))

(random-table/register :name "Arabic Name > Surname"
  :private t
  :data '("Abdel" "Awad" "Dahhak" "Essa" "Hanna"
           "Harbi" "Hassan" "Isa" "Kasim" "Katib"
           "Khalil" "Malik" "Mansoor" "Mazin" "Musa"
           "Najeeb" "Namari" "Naser" "Rahman" "Rasheed"
           "Saleh" "Salim" "Shadi" "Sulaiman" "Tabari"))

(random-table/register :name "Item, Fantasy"
  :private t
  :data '("Weapon" "Potion" "Scroll" "Armor" "Book"
           "Map" "Jewel" "Device" "Note" "Container"
           "Letter" "Amulet" "Ring" "Promissory" "Glasses"
           "Key" "Ingredient" "Poison" "Drug" "Pet"))

(random-table/register :name "Corporation, Sci-Fi"
  :data '("- Name :: ${Corporation, Sci-Fi > Prefix} ${Corporation, Sci-Fi > Suffix}\n- Rumor :: ${Corporation, Sci-Fi > Rumor}"))

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

;;;; Herbalist's Primer

(random-table/register :name "Plant (Herbalist's Primer)"
  :data '("${Plant > Name Prefix (Herbalist's Primer)}${Plant > Name Suffix (Herbalist's Primer)} is ${Plant > Rarity (Herbalist's Primer)} ${Plant > Habit (Herbalist's Primer)}, mostly prized for its ${Plant > Properties (Herbalist's Primer)} value.  It is a native to the ${Plant > Climate (Herbalist's Primer)} ${Plant > Biome (Herbalist's Primer)}.  Interestingly, it ${Plant > Quirk (Herbalist's Primer)}."))

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
           "streams" "swamps" "urban areas" "volanoes" "wastes"))

(random-table/register :name "Plant > Quirk (Herbalist's Primer)"
  :private t
  :data '("is carnivorous" "is parasitic" "is symbiotic with another plant" "stores water in the stems" "has a strong, pleasant aroma"
           "is always warm to the touch" "is covered in sharp spikes" "is covered in a sticky sap" "smells of rotting meat" "grows in the tree crowns"
           "grows almost entirely underground" "only blooms at night" "is poisonous to other plants" "causes a strong allergic reaction" "produces a lot of pollen"
           "attracts all kinds of insects" "is a favorite snack of many animals" "often hosts bird nests" "has a lovely, sweet flavor" "grows incredibly fast"))


(provide 'jf-tables)
;;; jf-tables.el ends here
