;; This file is about registering different tables.

(defun jf/tables/roll (table-name)
  (interactive (list (completing-read "Table name: " jf/tables)))
  (let ((table (gethash (intern table-name) jf/tables)))
    (message "%s :: %s" table-name (jf/tables/roll-on table))))

(defun jf/tables/roll-on (table &optional container)
  (unless container (setq container table))
  (cond
   ((-cons-pair? table)
    (cdr table))
   ((listp table)
    (jf/roll-on-table (seq-random-elt table) container))
   ((symbolp table)
    (jf/roll-on-table (symbol-value table) container))
   ((functionp table)
    (funcall table container))
   ((ad-lambda-p table)
    (funcall table container))
   ((numberp table)
    table)
   ;; Once I have a string; explode on tokens.  What do the tokens look like?
   ;; Inclined to go with the following: "On the horizon you see ${table-name}."
   ((stringp table)
    (s-format table #'jf/tables/roll-on-via-interpolation))
    (t (user-error (format "Unable to handle %s." table)))))

(defun jf/tables/roll-on-via-interpolation (table-name)
  (let ((table (gethash (intern table-name) jf/tables)))
    (jf/tables/roll-on table)))

(setq jf/tables (make-hash-table))
;;   "A hash-table of random tables.

;; The hash key is the \"human readable\" name of the table.
;; The hash value is the symbolic name of the table.")

(cl-defun jf/table (&key source name table)
  (puthash (intern (format "%s (%s)" name source)) table jf/tables))

(jf/table
  :name "Character"
  :source "Black Sword Hack"
  :table "Hello ${Unexpected Event (Black Sword Hack)}, I like ${Keepsakes (Errant)}")

(jf/table
  :name "Unexpected Event"
  :source "Black Sword Hack"
  :table
  '((1 . "Very negative")
     (2 . "Negative")
     (3 . "Negative but…")
     (4 . "Positive but…")
     (5 . "Positive")
     (6 . "Very Positive")))

(jf/table
  :name "Keepsakes"
  :source "Errant"
  :table
  '("The sword of the hero Black Mask. Useless, but looks really cool."
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

(jf/table
  :name "Failed Professions"
  :source "Errant"
  :table
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
