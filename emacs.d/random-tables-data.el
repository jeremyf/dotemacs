(require 'random-table)
;;; Support Functions

(defun random-table/roller/saving-throw (table)
  "Prompt for rolling a saving throw for the given TABLE."
  (let ((score (read-number
                 (format "%s\n> Enter Saving Throw Score: "
                   (random-table-name table))
                 15))
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
(random-table/register :name "Reaction Roll (Errant)"
  :roller #'random-table/roller/2d6
  :data '(((2) . "Hostile [DV +8]")
           ((3 4 5) . "Unfriendly [DV +4]")
           ((6 7 8) . "Unsure")
           ((9 10 11) . "Amicable [DV -2]")
           ((12) . "Friendly [DV -4]")))

(random-table/register :name "Character (Errant)"
  :data (list (concat "\n- Physique :: ${4d4}\n- Skill :: ${4d4}\n- Mind :: ${4d4}"
                "\n- Presence :: ${4d4}"
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
           "Elements" "Dimensions" "Life" "Death" "Objects" "BIota"))

(random-table/register :name "Ancestry (Errant)"
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

;;; Black Sword Hack
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

(random-table/register :name "Attributes (OSR)"
  :data '("\n- Strength :: ${3d6}\n- Intelligence :: ${3d6}\n- Wisdom :: ${3d6}\n- Dexterity :: ${3d6}\n- Constitution :: ${3d6}\n- Charisma :: ${3d6}"))

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
  "Prompt for likelihood and return corresponding roller."
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
             (when index (concat " with unexpected \"" (nth (- (car (-list index)) 1) data) "\" event")))
  :data '("Very negative" "Negative" "Negative but…" "Positive but…" "Positive" "Very Positive"))

;;; OSR
(random-table/register :name "Attributes (OSR) (3d6 Down the Line)"
  :data '("\n- Strength :: ${3d6}\n- Intelligence :: ${3d6}\n- Wisdom :: ${3d6}\n- Dexterity :: ${3d6}\n- Constitution :: ${3d6}\n- Charisma :: ${3d6}"))

;;; Random Names (from Stars without Number)
(random-table/register :name "Name"
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

(random-table/register :name "Save vss Mangled Arm"
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
