
(if (f-file?  "~/git/random-table.el/random-table.el")
  (require 'random-table "~/git/random-table.el/random-table.el")
  (use-package random-table
    :straight (:host github :repo "jeremyf/random-table.el")))

(setq random-table/reporter #'random-table/reporter/as-insert)

(random-table/register :name "Pendragon > Book of Sires > Cause of Death"
  :data '((1 . "In combat by personal fued")
           (2 . "In combat with [bandits/raiders/beast]")
           (3 . "[Hunting accident/Riding accident/Drowning/Other accident/Suspicious accident]")
           ((4 . 5) "Natural causes")
           (6 . "Unknown cause, just never came home")))

(random-table/register :name "Pendragon > Book of Sires > Bride's Homeland"
  :data '(((1 . 2) . "Same county homeland as groom's family")
           ((3 . 4) . "Same region as groom's family")
           (5 . "Neighboring region to groom's family")
           (6 . "{Pendragon > Book of Siress > Homeland Region}")))

(random-table/register :name "Pendragon > Book of Sires > Homeland Region"
  :data '(((1 . 4) . "{Pendragon > Book of Sires > Homeland Region > Logres - Saxon Shore}")
           ((5 . 9) . "{Pendragon > Book of Sires > Homeland Region > Logres - The Midlands}")
           ((10 . 13) . "{Pendragon > Book of Sires > Homeland Region > Logres - South Counties}")
           ((14 . 15) . "{Pendragon > Book of Sires > Homeland Region > Cambria}")
           ((16 . 17) . "{Pendragon > Book of Sires > Homeland Region > Cumbria}")
           (18 . "{Pendragon > Book of Sires > Homeland Region > Cornwall}")
           (19 . "{Pendragon > Book of Sires > Homeland Region > Brittany}")
           (20 . "{Pendragon > Book of Sires > Homeland Region > Aquitaine}")))

(random-table/register :name "Pendragon > Book of Sires > Homeland Region > Logres - Saxon Shore"
  :private t
  :data '(((1 . 2) . "Starting Tribe: Iceni; Homeland: Caerwent; Culture: Cyrmic; Religion: British Christian")
           ((3 . 4) . "Starting Tribe: Iceni; Homeland: Caerwent; Culture: Roman; Religion: Roman Christian")
           (5 . "Starting Tribe: Roman; Homeland: City of Norwich; Culture: Roman; Religion: Roman Christian")
           ((6 . 8) . "Starting Tribe: Cantii; Homeland: Kent; Culture: Cymric; Religion: British Christian")
           ((9 . 10) . "Starting Tribe: Trinovantes; Homeland: Caercolun; Culture: Cymric; Religion: British Christian")
           (11 . "Starting Tribe: Roman; Homeland: City of Colchest; Culture: Roman; Religion: Roman Christian")
           ((12 . 16) . "Starting Tribe: Catuvellauni; Homeland: Huntland; Culture: Cymric; Religion: British Christian")
           ((17 . 18) . "Starting Tribe: Trinovantes; Homeland: Thamesmouth; Culture: Cymric; Religion: British Christian")
           ((19 . 20) . "Starting Tribe: Roman; Homeland: City of London; Culture: Roman; Religion: Roman Christian")))

(random-table/register :name "Pendragon > Book of Sires > Homeland Region > Logres - The Midlands"
  :private t
  :data '((1 . "Starting Tribe: Catuvellauni; Homeland: Tribruit; Culture: Cyrmic; Religion: British Christian")
           ((2 . 3) . "Starting Tribe: Coritani; Homeland: Lambor; Culture: Cyrmic; Religion: Pagan")
           ((4 . 5) . "Starting Tribe: Coritani; Homeland: Lonazep; Culture: Cyrmic; Religion: Mix (British Pagan or Christian)")
           ((6 . 7) . "Starting Tribe: Dobunni; Homeland: Clarence; Culture: Cymric; Religion: Roman Christian")
           (8 . "Starting Tribe: Roman; Homeland: City of Cirencester; Culture: Roman; Religion: Roman Christian")
           (9 . "Starting Tribe: Catuvellauni; Homeland: Rydychan; Culture: Cymric; Religion: British Christian")
           (10 . "Starting Tribe: Coritani; Homeland: Wurensis; Culture: Cymric; Religion: Mix (British Christian or Pagan)")
           (11 . "Starting Tribe: Dobunni; Homeland: Wurensis; Culture: Cymric; Religion: Mix (British Christian or Pagan)")
           ((12 . 13) . "Starting Tribe: Brigantes; Homeland: Bedegraine; Culture: Cymric; Religion: British Christian")
           ((14 . 17) . "Starting Tribe: Coritani; Homeland: Linden; Culture: Cymric; Religion: British Christian")
           ((18 . 19) . "Starting Tribe: Dobunni; Homeland: Glevum; Culture: Cymric; Religion: British Christian")
           (20 . "Starting Tribe: Roman; Homeland: City of Gloucester; Culture: Roman; Religion: Roman Christian")))

(random-table/register :name "Pendragon > Book of Sires > Homeland Region > Logres - South Counties"
  :private t
  :data '(((1 . 2) . "Starting Tribe: Belgae; Homeland: Salisbury; Culture: Cyrmic; Religion: British Christian")
           (3 . "Starting Tribe: Belgae; Homeland: Salisbury; Culture: Cyrmic; Religion: Pagan")
           ((4 . 5) . "Starting Tribe: Durotriges; Homeland: Dorsette; Culture: Roman; Religion: Roman Christian")
           (6 . "Starting Tribe: Roman; Homeland: City of Dorchester; Culture: Roman; Religion: Roman Christian")
           ((7 . 8) . "Starting Tribe: Atrebates; Homeland: Gentian; Culture: Cymric; Religion: British Christian")
           ((9 . 10) . "Starting Tribe: Atrebates; Homeland: Silchester; Culture: Cymric; Religion: British Christian")
           (11 . "Starting Tribe: Roman; Homeland: City of Silchester; Culture: Roman; Religion: Roman Christian")
           (12 . "Starting Tribe: Atrebates; Homeland: Rydychan; Culture: Cymric; Religion: British Christian")
           ((13 . 15) . "Starting Tribe: Berroc Saxons; Homeland: Berroc; Culture: Saxon (Mix); Religion: British Christian or Wotanic")
           ((16 . 17) . "Starting Tribe: Regenses; Homeland: Hantonne; Culture: Cymric; Religion: British Christian or Roman Christian")
           (18 . "Starting Tribe: Regenses; Homeland: City of Chichester; Culture: Cymric; Religion: British Christian")
           ((19 . 20) . "Starting Tribe: Regenses; Homeland: Sussex; Culture: Cymric; Religion: British Christian")))

(random-table/register :name "Pendragon > Book of Sires > Homeland Region > Cambria"
  :private t
  :data '(((1 . 5) . "Starting Tribe: Irish; Homeland: Estregales; Culture: Irish; Religion: British Christian")
           ((6 . 8) . "Starting Tribe: Silures; Homeland: Escavalon; Culture: Cymric; Religion: British Christian")
           (9 . "Starting Tribe: Silures; Homeland: Escavalon; Culture: Cymric; Religion: Pagan")
           (10 . "Starting Tribe: Roman; Homeland: City of Carlion; Culture: Roman; Religion: Roman Christian")
           ((11 . 14) . "Starting Tribe: Ordovices; Homeland: Gomeret; Culture: Cymric; Religion: Pagan")
           ((15 . 16) . "Starting Tribe: Cornovii; Homeland: Cameliard; Culture: Cymric; Religion: British Christian")
           (17 . "Starting Tribe: Cornovii; Homeland: Cameliard; Culture: Cymric; Religion: Pagan")
           ((18 . 20) . "Starting Tribe: Deceangli; Homeland: Cheshire; Culture: Cymric; Religion: Pagan")))

(random-table/register :name "Pendragon > Book of Sires > Homeland Region > Cumbria"
  :private t
  :data '(((1 . 3) . "Starting Tribe: Brigantes; Homeland: Roestoc; Culture: Cymric; Religion: Pagan")
           ((4 . 6) . "Starting Tribe: Brigantes; Homeland: Maris; Culture: Cymric; Religion: Pagan")
           ((7 . 10) . "Starting Tribe: Brigantes; Homeland: Malahaut; Culture: Cymric; Religion: British Christian")
           ((11 . 12) . "Starting Tribe: Roman; Homeland: City of Eburacum; Culture: Roman; Religion: Roman Christian")
           ((13 . 14) . "Starting Tribe: Brigantes; Homeland: Cambenet; Culture: Cymric; Religion: British Christian")
           ((15 . 17) . "Starting Tribe: Brigantes; Homeland: Nohaut; Culture: Cymric; Religion: British Christian")
           ((18 . 20) . "Starting Tribe: Parisi; Homeland: Deira; Culture: Cymric; Religion: British Christian")))

(random-table/register :name "Pendragon > Book of Sires > Homeland Region > Cornwall"
  :private t
  :data '(((1 . 4) . "Starting Tribe: Jagent Picts; Homeland: Jagent; Culture: Pict; Religion: British Christian")
           ((5 . 6) . "Starting Tribe: Jagent Picts; Homeland: Jagent; Culture: Pict; Religion: Heathen")
           ((7 . 13) . "Starting Tribe: Dumnonii; Homeland: Ascalon; Culture: Cymric; Religion: British Christian")
           ((14 . 17) . "Starting Tribe: Dumnonii; Homeland: Tintagel; Culture: Cymric; Religion: Mix")
           ((18 . 20) . "Starting Tribe: Dumnonii; Homeland: Totnes; Culture: Cymric; Religion: British Christian")))

(random-table/register :name "Pendragon > Book of Sires > Homeland Region > Brittany"
  :private t
  :data '(((1 . 10) . "Homeland: Vannetais; Culture: Cymric; Religion: British Christian")
           ((11 . 12) . "Homeland: City of Rennes; Culture: Roman; Religion: Roman Christian")
           ((13 . 14) . "Homeland: City of Nantes; Culture: Roman; Religion: Roman Christian")
           ((15 . 16) . "Homeland: City of Nantes; Culture: Roman; Religion: Roman Christian")
           ((17 . 18) . "Homeland: Domnonie; Culture: Cymric; Religion: British Christian")
           ((19 . 20) . "Homeland: Cornouailles; Culture: Cymric; Religion: British Christian")))

(random-table/register :name "Pendragon > Book of Sires > Homeland Region > Aquitaine"
  :private t
  :data '(((1 . 3) . "Homeland: Ganis (Denoit); Culture: Aquitainian; Religion: Arian Christian")
           ((4 . 7) . "Homeland: Ganis; Culture: Aquitainian; Religion: Arian Christian")
           (8 . "Homeland: City of Trèbes; Culture: Roman; Religion: Roman Christian")
           (9 . "Homeland: City of Bayonne; Culture: Roman; Religion: Roman Christian")
           ((10 . 11) . "Homeland: City of Bordeaux; Culture: Roman; Religion: Roman Christian")
           ((12 . 18) . "Homeland: Kingdom of Toulouse; Culture: Aquitaine; Religion: Arian Christian")
           ((19 . 20) . "Homeland: City of Toulouse; Culture: Roman; Religion: Roman Christian")))

(random-table/register :name "Pendragon > Book of Sires > Gifts, Rewards, and Loot"
  :data '(
           ((1 . 2) "A decorated weapon £2")
           (3 . "An engraved golden ring £2")
           (4 . "A silver armband £2")
           (5 . "A fine cloak £2")
           (6 . "A decorated sword £4")
           (7 . "A golden ring with a precious stone £{1d6+4}")
           (8 . "A golden brooch £{1d6+4}")
           (9 . "A golden armband £{1d6+4}")
           (10 . "A magnificent sword (+1 modifier to Sword Skill) £20+")
           (11 . "A magnificent saddle (+1 modifier to Horsemanship Skill) £20+")
           (12 . "A large tapestry £{1d6+4}")
           (13 . "A silver belt £{1d6+4}")
           (14 . "A silver dining set for two (2 plates, 2 goblets) £6")
           (15 . "A large silver platter £5")
           (16 . "An engraved golden ring £2")
           (17 . "A tapestry commemorating the event £2")
           (18 . "A pair of silver plates £2")
           ((19 . 20) . "A silver goblet £2")))

(random-table/register :name "Pendragon > Book of Feasts > Courses"
  :data `(,(concat "\n  - First Course :: {Pendragon > Book of Feasts > First Course}"
            "\n  - Second Course :: {Pendragon > Book of Feasts > Second Course}"
            "\n  - Third Course :: {Pendragon > Book of Feasts > Third Course}")))

(random-table/register :name "Pendragon > Book of Feasts > First Course"
  :private t
  :data '("Meat with mustard" "Ale and cheese quiche" "Swan neck pudding" "Beef marrow fritters"
           "Eels in thick spicy puree" "Roast baby swan" "Fat capon" "Roast heron"
           "Roast pheasant" "Loach in cold green sauce" "Fruit tarts" "Cold meat slicese in ginger sauce"
           "Custard with dried fruit" "Porpoise and peas" "Parsley bread" "Haslet"
           "Roasted salmon in wine sauce" "Almond omelet" "Cod tails" "Pastries (plum, quince, and apple)"))

(random-table/register :name "Pendragon > Book of Feasts > Second Course"
  :private t
  :data '("Artichokes stuffed with blueberry rice" "Broth with bacon and peas" "Meat tiles" "Roasted seal"
           "Honey-glazed roast chicken rolled with mustard and pine nuts" "Heraldic emblem in meat jelly" "stuffed boar" "Peacock (redressed in its feathers before service)"
           "Astrological temperament herb cakes and cheses" "Capon pastries and chips" "Roasted crane" "Roasted coney"
           "Bream and eel pasties" "Boiled bittern" "Frumenty (boiled wheat custard)" "Pullets (similar to Cornish game hen)"
           "Beaver tails" "Cockentrice (pigs head on chicken body)" "Lampreys in hot sauce" "Fruit and salmon pie"))

(random-table/register :name "Pendragon > Book of Feasts > Third Course"
  :private t
  :data '("Herb fritters" "Roasted chicken and pheasant wings" "A selection of cheeses" "Quinces stewed in wine"
           "Roasted pigeon" "Elderberry divination cakes" "Roast larks" "Venison in frumenty"
           "White poultry meat stewed in wine" "Almond cakes served on roundels" "Glazed eggs" "Imported or baked fruits"
           "Doucette (custard and bone marrow pie)" "Roast eagle" "Roast bream served in a dariole (pastry mold)" "Turnips baked with cheese"
           "Hippocras" "Mushroom tarts" "Wafers" "Whole dry spices “to aid in digestion”"))

;; Starting Squire
;; Most often Age 14
;; Squire Skill: Age + 1 + first modifier
;; Knightly Skills: Age - 11 + second modifier
;; - these are Awareness, Battle, Courtesy, Horsemanship, First Aid, Lance, Hunting, Sword
;; Passion: Loyalty(Lord) 15
;; Each Winter Phase test all skills separately
(random-table/register :name "Pendragon > Book of the Entourage > New Squire"
  :data '(((1 . 5) . "Son of an Esquire (-2/-1)")
           ((6 . 10) . "Younger son (4th or more) of a vassal knight (-1/0)")
           ((11 . 15) . "2nd son (“spare”) of a vassal knight (0/0)")
           ((16 . 20) . "Heir of a vassal knight (0/0)")))

;; Wife: aged 14+1d6
;; The following skills start at 5:
;; - Stewardship, Chirurgery, First Aid, Courtesy, Industry, Intrigue, Flirting, Fashion, Dancing
;; Chirurgery, First and and Courtesy increase by 1 per years 16 through 20.
;; Passion Love (Family) 15
;;
;; Ladies improve Industry + two skills chosen by the player:
;; 1d6+(Age-10) to a max of 15.  These increase as play progresses.
;; Stewardship increases by father's status
;; (("Commoner" . -7)
;;   ("Esquire" . -2 )
;;   ("Household Knight". 0)
;;   ("Vassal Knight". 0)
;;   ("Estate Holder or Baron". +3)
;;   ("Officer". +1)
;;   ("Holy Man". -3)
;;   ("Foot Soldier". -5))

;; (random-table/register :name "Pendragon > Book of the Entourage > Wife and Dowry"
;;   :data
;;   '("Daughter of a wealthy esquire. Dowry: £{2d6+8} treasure."
;;      "Younger daughter of a vassal knight. Dowry: £{1d6} treasure."
;;      "Eldest daughter of a vassal knight or younger daughter of a rich vassal knight. Dowry: £{1d6+6} treasure."
;;      "Eldest daughter of a rich vassal knight or younger daughter of an estate holder. Dowry: £{2d6+1} treasure."
;;      "Eldest daughter of a rich vassal knight or younger daughter of an estate holder. Dowry: £{2d6+2} treasure."
;;      "Widow of a vassal knight. Dowry: £{2d6} treasure. Widow's Portion: £3.5 land"
;;      "Eldest daughter of an estate holder. Dowry: £{1d6 * 5 + 35} treasure."
;;      "Co-heiress (1 of 4) of a vassal knight. Dowry: £{2d6} treasure, £{1d2+1} land."
;;      "Widow of a rich vassal knight. Dowry: £{4d6} treasure. Widow's Portion: £{1d3+4} land."
;;      "Co-heiress (1 of 3) of a vassal knight. Dowry: £{3d6} treasure, £{1d2+2} land."
;;      "Eldest daughter of an estate holder. Dowry: £{5d6+60} treasure."
;;      "Eldest daughter of an estate holder. Dowry: £{5d6+85} treasure."
;;      "Widow of a rich vassal knight. Dowry: £{6d6} treasure. Widow's Portion: £{1d6+7} land."
;;      "Co-heiress (1 of 2) of a vassal knight. Dowry: £{3d6+5} treasure, £{1d3+3} land."
;;      "Widow of a rich vassal knight. Dowry: £{3d20+3} treasure. Widow's Portion: £{1d3+12} land."
;;      "Widow of an estate holder. Dowry: £{5d20+5} treasure. Widow's Portion: £{1d10+19} land."


;; TODO: Let's map this into the random-table, now that I have the
;; algorithm.
(defun rpgs/pendragon/roll (rank modifier roll)
  "Given RANK + MODIFIER and ROLL, determine success.

Return `cons' with `car' that is the result and `crd' the details
as a `plist' with properties of :roll :rank :modifier :critical_excess."
  (interactive (list
                 (read-number "Rank: ")
                 (read-number "Modifier: " 0)
                 (+ 1 (random 20))))
  (let* ((critical_excess nil)
          (modified_rank
           (+ rank modifier))
          (modified_roll
            (+ roll modifier))
          (result
            (if (>= modified_rank 20)
              (let ((overage (- modified_rank 20)))
                (if (>= (+ roll overage) 20)
                  (progn
                    (format "Critical (+%s)" (- (+ roll overage) 20))
                    (setq critical_excess (- (+ roll overage) 20)))
                  (format "Success" modified_roll)))
              (cond
                ((= 20 roll)
                  "Fumble")
                ((> roll modified_rank)
                  "Failure")
                ((= roll modified_rank)
                  "Critical")
                (t (format "Success" roll))))))
    (cons (format "%s" result)
      (list
        :roll roll
        :rank rank
        :modifier modifier
        :critical_excess critical_excess))))

(defconst rpgs/pendragon/traits
  '("Chaste" "Energetic" "Forgiving" "Generous" "Honest" "Just" "Merciful" "Modest" "Prudent"
     "Spiritual" "Temperate" "Trusting" "Valorous" "Lazy" "Vengeful" "Selfish" "Deceitful"
     "Arbitrary" "Cruel" "Proud" "Reckless" "Worldly" "Indulgent" "Suspicious" "Cowardly"))

(random-table/register :name "Pendragon > Random Trait"
  :data rpgs/pendragon/traits)

(random-table/register :name "Pendragon > Name > Irish > Female"
  :data '("Aibhlinn" "Aileen" "Beibhinn" "Bevan" "Blaithnaid" "Brigid" "Cait" "Cron" "Derbail" "Dunlaith" "Eithrie" "Finnguala" "Flann" "Gormlaith" "Grainne" "Lassar" "Mor" "Orlaith" "Sadb" "Siobhan" "Sinead" "Sorcha" "Una"))

(random-table/register :name "Pendragon > Name > Irish > Male"
  :data '("Aed" "Aedan" "Aeducan" "Ailgel,Ailill" "Airechtach" "Amalgaid" "Art" "Baetan" "Baeth" "Berach" "Berchan" "Brion" "Bruatur" "Carthach" "Cathal" "Cenn" "Cerball" "Colcu" "Comman" "Congal" "Cormacc" "Daig" "Diarmait" "Donngal" "Dunchad" "Echen" "Elodach" "Eogan" "Fachtna" "Fedelmid" "Finnchad" "Flann" "Guaire" "Imchad" "Laegaire" "Lorccan" "Maine" "Murchad" "Nathi" "Ronan" "Russ" "Senach" "Tadc" "Tuathal" "Ultan"))

(random-table/register :name "Pendragon > Name > Frankish > Female"
  :data '("Adeline" "Aelis" "Agnes" "Aiglante" "Alais" "Alicia" "Alienor" "Alix" "Amalon" "Amalgard" "Ameline" "Anseir" "Aregund" "Aude" "Basina" "Beatrix" "Belle" "Bellisent" "Berthild" "Blond" "Brunhild" "Brunissent" "Catherine" "Cecilia" "Clarissa" "Clothild" "Edith" "Elisabeth" "Erembourg" "Ermengart" "Esclarmonde" "Flore" "Fredegund" "Galienne" "Genevieve" "Guiborc" "Helissent" "Helouise" "Hermengart" "Hildegard" "Isabelle" "Jacqueline" "Jehanne" "Jeannette" "Joie" "Josiane" "Laurence" "Lubias" "Lutisse" "Marguerite" "Marie" "Mathilde" "Margalie" "Mirabel" "Nicole" "Nicolette" "Olive" "Oriabel" "Patronille" "Pernelle" "Poette" "Rosamonde" "Sigilind" "Sybylle" "Theudechild" "Wisigard" "Yde"))

(random-table/register :name "Pendragon > Name > Frankish > Male"
  :data '("Adalmund" "Aimon" "Amalric" "Arbogast" "Archembaud" "Arigius" "Aurel" "Baldric" "Bardrim" "Baudouin" "Bernier" "Bertmund" "Brantome" "Bretonnet" "Brunehaut" "Bruyant" "Carolus" "Childeric" "Chlodobert" "Clovis" "Ernaut" "Eustache" "Fierbras" "Fluvant" "Gaidon" "Galafre" "Galien" "Gaumadras" "Gautier" "Gilbert" "Gilles" "Girard" "Godfroi" "Grimoald" "Gui" "Guibert" "Guillame" "Guinemant" "Gundovald" "Gunthar" "Hardouin" "Harde" "Hernaudin" "Hernaut" "Hervis" "Hubert" "Huges" "Huidemar" "Ingund" "Isore" "Jacquelin" "Jean" "Marc" "Jerome" "Jourdain" "Julian" "Landri" "Leomund" "Leovigild" "Lothar" "Louis" "Maugis" "Mercadier" "Merovech" "Milon" "Naimes" "Namus" "Odovacer" "Pepin" "Piccolet" "Philippe" "Pierre" "Renaud" "Renier" "Renouart" "Richard" "Robert" "Roderic" "Samson" "Sigibrand" "Sigimund" "Tancred" "Thierry" "Theudebald" "Theuderic" "Varocher" "Vincent" "Vivien" "Yon" "Yves"))

(random-table/register :name "Pendragon > Name > Aquitanian > Female"
  :data '("Adeline" "Aelis" "Agnes" "Aiglante" "Alais" "Alicia" "Alienor" "Alix" "Amalon" "Amalgard" "Ameline" "Anseir" "Aregund" "Aude" "Basina" "Beatrix" "Belle" "Bellisent" "Berthild" "Blond" "Brunhild" "Brunissent" "Catherine" "Cecilia" "Clarissa" "Clothild" "Edith" "Elisabeth" "Erembourg" "Ermengart" "Esclarmonde" "Flore" "Fredegund" "Galienne" "Genevieve" "Guiborc" "Helissent" "Helouise" "Hermengart" "Hildegard" "Isabelle" "Jacqueline" "Jehanne" "Jeannette" "Joie" "Josiane" "Laurence" "Lubias" "Lutisse" "Marguerite" "Marie" "Mathilde" "Margalie" "Mirabel" "Nicole" "Nicolette" "Olive" "Oriabel" "Patronille" "Pernelle" "Poette" "Rosamonde" "Sigilind" "Sybylle" "Theudechild" "Wisigard" "Yde" "Armide" "Babette" "Bethilie" "Blandine" "Brienne" "Carelia" "Cecile" "Danielle" "Diane" "Edith" "Elianor" "Ethaine" "Felise" "Heloise" "Helena" "Irene" "Jeanne" "Liaze" "Liliane" "Lusiane" "Lynn" "Margot" "Olivia" "Priscilla" "Raisende" "Roxane" "Sibille" "Ursanne" "Verrine"))

(random-table/register :name "Pendragon > Name > Aquitanian > Male"
  :data '("Adalmund" "Aimon" "Amalric" "Arbogast" "Archembaud" "Arigius" "Aurel" "Baldric" "Bardrim" "Baudouin" "Bernier" "Bertmund" "Brantome" "Bretonnet" "Brunehaut" "Bruyant" "Carolus" "Childeric" "Chlodobert" "Clovis" "Ernaut" "Eustache" "Fierbras" "Fluvant" "Gaidon" "Galafre" "Galien" "Gaumadras" "Gautier" "Gilbert" "Gilles" "Girard" "Godfroi" "Grimoald" "Gui" "Guibert" "Guillame" "Guinemant" "Gundovald" "Gunthar" "Hardouin" "Harde" "Hernaudin" "Hernaut" "Hervis" "Hubert" "Huges" "Huidemar" "Ingund" "Isore" "Jacquelin" "Jean" "Marc" "Jerome" "Jourdain" "Julian" "Landri" "Leomund" "Leovigild" "Lothar" "Louis" "Maugis" "Mercadier" "Merovech" "Milon" "Naimes" "Namus" "Odovacer" "Pepin" "Piccolet" "Philippe" "Pierre" "Renaud" "Renier" "Renouart" "Richard" "Robert" "Roderic" "Samson" "Sigibrand" "Sigimund" "Tancred" "Thierry" "Theudebald" "Theuderic" "Varocher" "Vincent" "Vivien" "Yon" "Yves" "Aalard" "Acostant" "Alexis" "Argius" "Barnard" "Beranger" "Bovert" "Burcan" "Cadmar" "Corneus" "Danain" "Daniel" "Dragan" "Elad" "Emeric" "Evrard" "Gobert" "Gundahar" "Henri" "Jaufre" "Jules" "Lancel" "Lucan" "Lucas" "Morien" "Nicholas" "Patric" "Remi" "Renauld" "Roger" "Serin" "Sevin" "Thibaud" "Thierry" "Thomas" "Victor" "Xavier"))

(random-table/register :name "Pendragon > Name > Cymri > Female"
  :data '("Adwen" "Annest" "Angarad" "Arianwen" "Briant" "Duddug" "Collwen" "Dwynwen" "Eleri" "Ffraid" "Glesig" "Glesni" "Gwen" "Heledd" "Indeg" "Leri" "Lleucu" "Llio" "Melangell" "Meleri" "Nest" "Nia" "Tydfil"))

(random-table/register :name "Pendragon > Name > Cymri > Male"
  :data '("Addonwy" "Aeron" "Afan" "Aneirin" "Aeddan" "Amig" "Amlyn" "Athrwys" "Arddur" "Buddfannan" "Blaen" "Bledri" "Bradwen" "Bleddig" "Cadfannan" "Cadfael" "Cadwallon" "Cilydd" "Cynon" "Cynfan" "Cyfulch" "Cynrain" "Cunvelyn" "Caradoc" "Cibno" "Ceredig" "Cadlew" "Cynwal" "Clydno" "Cynhafal" "Dafydd" "Defi" "Dwyai" "Edar" "Edern" "Eiddef" "Erthgi" "Elad" "Eudaf" "Biffin" "Gwefrfawr" "Gwegon" "Gwion" "Gwyn" "Gwarddur" "Gwern" "Gwyleged" "Gwrien" "Gwraid" "Gorthyn" "Gwaednerth" "Gwengad" "Brugyn" "Gwenabwy" "Gwrfelling" "Gwair" "Graid" "Geriant" "Gwanon" "Hyfaidd" "Hywel" "Ieuan" "Llywel" "Marchlew" "Moried" "Morien" "Madog" "Morial" "Mynyddog" "Merin" "Neilyn" "Nwython" "Nai" "Nerthaid" "Neddig" "Nidian" "Owain" "Padern" "Pedrog" "Ricerch" "Rhodri" "Rhufon" "Rhun" "Sawel" "Seriol" "Sywno" "Tathal" "Tathan" "Tudfwlch" "Tyngyr" "Uren" "Uwain" "Ysgarran"))

(random-table/register :name "Pendragon > Name > Roman > Female"
  :data '("Albania" "Arcavia" "Avitia" "Belletor" "Burcania" "Caletia" "Caracturia" "Catiania" "Cunobarria" "Cervidia" "Dagwaldia" "Decmia" "Donicia" "Egbutia" "Galeria" "Gessia" "Ingenvinia" "Isatia" "Ivimaria" "Luonercia" "Litumaria" "Leddicia" "Lupinia" "Maccalia" "Macrinia" "Magunnia" "Marullinia" "Metunia" "Molacia" "Nemnogenia" "Nonia" "Novellia" "Olennia" "Pertacia" "Primania" "Nertomaria" "Sarimarcia" "Sudrenia" "Tanicia" "Taurinia" "Trenia" "Vepgenia" "Vibennia" "Vitalinia" "Ulpria"))

(random-table/register :name "Pendragon > Name > Roman > Male"
  :data '("Albanus" "Agorix" "Arcavius" "Avitus" "Belletor" "Burcanius" "Caletus" "Caracturus" "Catianus" "Cunobarrus" "Cervidus" "Dagwaldus" "Decmus" "Donicus" "Dumnorix" "Egbutius" "Elvorix" "Galerus" "Gessius" "Ingenvinus" "Isatis" "Ivimarus" "Luonercus" "Litumarus" "Leddicus" "Lupinus" "Maccalus" "Macrinus" "Magunnus" "Marullinus" "Metunus" "Molacus" "Nemnogenus" "Nonius" "Novellius" "Olennius" "Pertacus" "Primanus" "Nertomarus" "Sarimarcus" "Sudrenus" "Tanicus" "Taurinus" "Trenus" "Vepgenus" "Vibennis" "Vitalinus" "Ulprus" "Voteporix"))

(random-table/register :name "Pendragon > Name > Saxon > Female"
  :data '("Aelflaed" "Aelgifu" "Aethelred" "Burhred" "Cuthburh" "Cyneburh" "Eadgifu" "Eadgyth" "Eadhild" "Ealhred" "Eormenburh" "Hereswith" "Raedburh" "Sexburh" "Wihtburh"))

(random-table/register :name "Pendragon > Name > Saxon > Male"
  :data '("Aelfric" "Aescwine" "Bassa" "Beorhtric" "Caedwalla" "Caewlin" "Centwine" "Cenwalch" "Cerdic" "Coelred" "Coelric" "Coelwulf" "Coenhelm" "Conerad" "Conewalch" "Coenwulf" "Cuthbert" "Cuthred" "Cuthwulf" "Cyneagils" "Cynewulf" "Cynric" "Eadbald" "Eadberht" "Eadric" "Eardwulf" "Edwin" "Edgert" "Ethilfrith" "Ethelheard" "Ethelred" "Ethelwulf" "Hengest" "Hlothere" "Horsa" "Ine" "Octa" "Oeric" "Osric" "Oswald" "Oswine" "Oswulf" "Oswy" "Peada" "Penda" "Sigebryht" "Wihtred" "Wulfhere"))

(random-table/register :name "Pendragon > Named Knight > Name"
  :private t
  :data '("{Pendragon > Name > Cymri > Male}"
           "{Pendragon > Name > Cymri > Male}"
           "{Pendragon > Name > Roman > Male}"
           "{Pendragon > Name > Roman > Male}"
           "{Pendragon > Name > Frankish > Male}"
           "{Pendragon > Name > Aquitanian > Male}"
           "{Pendragon > Name > Saxon > Male}"
           "{Pendragon > Name > Irish > Male}"))

(random-table/register :name "Pendragon > Named Knight"
  :data '("{Pendragon > Named Knight > Name} (of) the {Pendragon > Named Knight > Person Adjective} {Pendragon > Named Knight > Person Type}"
           "{Pendragon > Named Knight > Name} of the {Pendragon > Named Knight > Equipment Adjective} {Pendragon > Named Knight > Equipment Type}"
           "{Pendragon > Named Knight > Name} of the {Pendragon > Named Knight > Place Adjective} {Pendragon > Named Knight > Place Type}"
           "{Pendragon > Named Knight > Name}, [Defender/Scourge] of the {Pendragon > Named Knight > Being Type}"
           "{Pendragon > Named Knight > Name} the {Pendragon > Named Knight > Person Type} without {Pendragon > Named Knight > Quality}"
           "{Pendragon > Named Knight > Name} the {Pendragon > Named Knight > Person Type} of the {Pendragon > Named Knight > Person Adjective} {Pendragon > Named Knight > Employer Type}"))

(random-table/register :name "Pendragon > Named Knight > Person Adjective"
  :private t
  :data '("Arbitrary" "Beautiful" "Brave" "Charitable" "Chaste"
           "Cowardly" "Cruel" "Dark" "Deceitful" "Energetic"
           "Fair" "False" "Fat" "Favoured" "Fearful"
           "Fearless" "Forgiving" "Generous" "Glorious" "Holy"
           "Hardy" "Honest" "Ill" "Ill-liked" "Indulgent"
           "Just" "Laughing" "Lazy" "Lowly" "Mercenary"
           "Merciful" "Mighty" "Modest" "Old" "Pitiful"
           "Pitiless" "Proud" "Prudent" "Reckless" "Robber"
           "Saintly" "Savage" "Selfish" "Spiritual" "Strong"
           "Suspicious" "Temperate" "Thief" "Thin" "True"
           "Trusting" "Ugly" "Unholy" "Untried" "Unworthy"
           "Valorous" "Vengeful" "Weak" "Weeping" "Well-loved"
           "Wise" "Worldly" "Young"
           ))

(random-table/register :name "Pendragon > Named Knight > Person Type"
  :private t
  :data '("Knight" "Lord" "Duke" "Chevalier" "Lover"
           "Bastard" "King" "Hunter" "Protector" "Guard"
           "Paladin" "Champion" "Prince" "Devil" "Dragon"
           "Lion" "Stranger" "Pilgrim" "Crusader" "Swordsman"
           "Armiger"
           "High {Pendragon > Named Knight > Person Type (d21)}"
           "Low {Pendragon > Named Knight > Person Type (d21)}"))

(random-table/register :name "Pendragon > Named Knight > Equipment Adjective"
  :private t
  :data '("Bright" "Hot" "Flaming" "Sharp" "Strong" "Undefeatied"
           "Striking" "Battered" "Trusty" "Failing" "Invisible" "Hiden"
           "Red" "Black" "Silver" "Gold" "Green" "Blue" "Violet" "Indigo"
           "Orange" "Stained" "Bloodstained" "Well-Worn" "Broken"
           "Sundered" "Steel" "Brass" "Copper" "Bone" "Leather" "Coral"
           "Horned" "Faerie" "Holy" "Unholy" "Twisted" "Ill-Ann" "Singing"
           "Iron" "Unblemished" "Ancient" "New" "Shining" "Strange" "Unknown"))

(random-table/register :name "Pendragon > Named Knight > Equipment Type"
  :private t
  :data '("Sword" "Lance" "Shield" "Buckler" "Mace" "Axe" "Separ" "Ring"
           "Helm" "Armour" "Banner" "Spurs" "Cheval" "Destrier" "Charger"
           "Heart" "Book" "Cross" "Crescent" "Jewel" "Scimitar" "Glvoes"
           "Gauntlet" "Cloak" "Dagger" "Bow" "Harp" "Horn" "Device" "Crest"))

(random-table/register :name "Pendragon > Named Knight > Place Adjective"
  :private t
  :data '("Dark" "Secret" "Hidden" "Enchanted" "Holy" "New" "Ancient" "Forgotten" "Misty" "Mist-shrouded" "Ill-starred" "Well-favoured" "Lucky" "Fortunate" "Bright" "Frozen" "Frosty" "Icy" "Snow-capped" "Highest" "Deepest" "Great" "Storm-tossed" "Wind-swept" "Renowned" "Well-defendend" "Unyielding" "Barren" "Bountiful" "Verdant" "Sun-baked" "Sun-bleached" "Well-endowed" "Fruitful" "Copper" "Gold" "Silver" "Bronze" "Tin" "Iron"))

(random-table/register :name "Pendragon > Named Knight > Place Type"
  :private t
  :data '("Lake" "Sea" "Castle" "Keep" "Tower" "Glade" "Wood" "Forest"
           "Mountain" "Moor" "Plains" "Fort" "Manor" "Fountain" "Spring"
           "River" "Ford" "Waterfall" "Marsh" "Well" "Stronghold"
           "Citadel" "Kingdom" "Land" "Island" "Temple" "Church" "Abbey"
           "Gate" "City" "Cliff" "Hill" "Rock" "Cave"))

(random-table/register :name "Pendragon > Named Knight > Quality"
  :private t
  :data '("Fear" "Mercy" "Compassion" "Charity" "Love" "Hope" "Joy"
           "Riches" "Beauty" "Family" "Home" "Land" "Loyalty" "Regrets"
           "Honour" "Piety" "Faith" "Luck" "Chance" "Law" "Justice"
           "Reason" "Care" "Trust" "Glorry" "Sin" "Armour" "Defense"
           "Hope of Redemption"))

(random-table/register :name "Pendragon > Named Knight > Being Type"
  :private t
  :data '("Oppressed" "Poor" "Peasants" "Fay" "Clergy" "Pilgrims" "Jews"
           "Usurers" "Dragons" "Rich" "Gluttons" "Drunkards" "Reprobates"
           "Worthy" "Unworthy" "King" "Patriarchs" "Prophets" "Women"
           "Children" "Men" "Sick" "Lepers" "Foreigners" "Travellers"
           "Magicians" "Enchanters" "Wizards" "Witches" "Nobles"
           "Martyrs" "Hunted" "Persecuted"))

(random-table/register :name "Pendragon > Named Knight > Employer Type"
  :private t
  :data '("Lady" "Queen" "Chatelaine" "Seneschal" "Steward"
           "Castellan" "Princess" "Bishop" "Abbot" "Commander"
           "King" "Emperor" "Prince" "Constable" "Marshall"
           "Champion" "Herald" "Chevalier" "Suzerain"
           "High {Pendragon > Named Knight > Employer Type (d19)}"
           "Low {Pendragon > Named Knight > Employer Type (d19)}"))

(random-table/register :name "Pendragon > Book of Uther > Courtly Event"
  :data '(
           ((1 . 2) . "Someone has disappeared from among {Pendragon > Book of Uther > Courtly Event > Who is Missing}")
           (3 . "Someone is picking a fight outside of the hall with {Pendragon > Book of Uther > Courtly Event > Who is Missing}")
           ((4 . 5) . "Someone is speaking ill of {Pendragon > Book of Uther > Courtly Event > Hostility}")
           (6 . "Someone is threatening a {Pendragon > Book of Uther > Courtly Event > Hostility}")
           (7 . "Someone is enraged about not getting to see {Pendragon > Book of Uther > Courtly Event > Hostility}")
           (8 . "Money is passing hands between people who out not to have money: {Pendragon > Book of Uther > Courtly Event > Impropriety}")
           (9 . "Someone appears to be stealing: {Pendragon > Book of Uther > Courtly Event > Impropriety}")
           (10 . "A stranger is suspiciously lurking around: {Pendragon > Book of Uther > Courtly Event > Unknown Person}")
           (11 . "An unknown person demands entry to court among the nobles: {Pendragon > Book of Uther > Courtly Event > Unknown Person}")
           (12 . "A stranger who asks others to get something from a courtier: {Pendragon > Book of Uther > Courtly Event > Unknown Person}")
           (13 . "An officer has been beaten, and cannot remember by whom: {Pendragon > Book of Uther > Courtly Event > Mysteries}")
           (14 . "A guard has collapsed at his post: {Pendragon > Book of Uther > Courtly Event > Mysteries}")
           (15 . "A woman tries to get a seat at court: {Pendragon > Book of Uther > Courtly Event > Woman Troubles > Who}")
           (16 . "Someone is having sex indiscreetly: {Pendragon > Book of Uther > Courtly Event > Woman Troubles > Who} {Pendragon > Book of Uther > Courtly Event > Woman Troubles > What}")
           (17 . "A woman makes a pass at a Player-knight: {Pendragon > Book of Uther > Courtly Event > Woman Troubles > Who} {Pendragon > Book of Uther > Courtly Event > Woman Troubles > What}")
           ((18 . 20) . "There are rumors swirling around one of the Player-knights or someone close: {Pendragon > Book of Uther > Courtly Event > Whispering Campaign}")))


(random-table/register :name "Pendragon > Book of Uther > Courtly Event > Who is Missing"
  :private t
  :data '(
           (1 . "Foreign emissaries")
           (2 . "The king’s officers")
           ((3 . 4) . "The king’s kitchen")
           ((5 . 6) . "The King’s Guard or garrison")
           ((7 . 8) . "The king’s hall staff")
           ((9 . 10) . "The king’s knights")
           (11 . "A group of visiting merchanges")
           (12 . "The officers of the Player-knight’s lord")
           ((13 . 14) . "A Player-knight’s kitchen")
           ((15 . 16) . "The guardsmen of the garrison of one of the the Player-knight’s lord")
           ((17 . 18) . "The hall staff of one of the Player-knights")
           ((19 . 20) . "The kniwhgts of the Player-knights’ lord")))

(random-table/register :name "Pendragon > Book of Uther > Courtly Event > Hostility"
  :private t
  :data '(
           (1 . "A foreign emissary")
           (2 . "A royal officer")
           ((3 . 4) . "A royal knight")
           ((5 . 6) . "A member of the King’s Guard")
           ((7 . 8) . "A visiting knight")
           ((9 . 10) . "A visiting lord")
           (11 . "A knight from the Player-knight’s household")
           (12 . "An officer of the Player-knight’s lord")
           (13 . "A servant from a Player-knight’s kitchen")
           (14 . "A monk")
           ((15 . 16) . "A royal servant")
           ((17 . 18) . "One of the Player-knights’s squires")
           (19 . "A visiting merchant")
           ((19 . 20) . "A wealthy commoner")))

(random-table/register :name "Pendragon > Book of Uther > Courtly Event > Impropriety"
  :private t
  :data '(
           ((1 . 5) . "A bribe")
           ((6 . 10) . "A gambling payoff officer")
           ((11 . 15) . "Friends helping each other out")
           ((16 . 20) . "Misinterpretation. Victims become enemies of the Player-knights")))

(random-table/register :name "Pendragon > Book of Uther > Courtly Event > Unknown Person"
  :private t
  :data '(
           "An assassin"
           "A Saxon spy; Gamemaster names his homeland"
           "A Caledonian spy; Gamemaster names his homeland"
           "A Cambrian spy; Gamemaster names his homeland"
           "A spy from Malahaut"
           "An angry clergyman preparing a curse"
           "A messenger with a message for only the king"
           "An angry enemy of the king"
           "An angry enemy of the lord of the Player-knights"
           "An angry enemy of a Player-knight"
           "An angry enemy of a royal knight"
           "A spy"
           "An entertainer looking for work"
           "An esquire seeking employment"
           "A freelance knight seeking employment"
           "A spy for another lord"
           "A foreign spy"
           "A revenge-seeking monk"
           "A phantom, which disappears after being discovered"))

(random-table/register :name "Pendragon > Book of Uther > Courtly Event > Mysteries"
  :private t
  :data '(((1 . 2) . "First Aid is needed; if successful, roll again")
          ((3 . 4) . "The Player-knights are accused of doing it")
          ((5 . 7) . "Evidence reveals it was an enemy of the king")
          ((8 . 12) . "He dies, which implicates the Player-knights")
          ((13 . 15) . "The knights are attacked by the same foe while rendering aid")
          ((16 . 18) . "The victim wakes and begs the Player-knights to hide him")
          ((19 . 20) . "The victim is embarrassed and refuses any more help, or even an explanation")))

(random-table/register :name "Pendragon > Book of Uther > Courtly Event > Woman Troubles > Who"
  :private t
  :data '(
          (1 . "One of the Player-knights’ wives")
          (2 . "One of the king’s concubines")
          (3 . "One of the royal officers’s wives")
          (4 . "One of King Uther’s vassals’ wives")
          (5 . "An unmarried maiden-in-waiting")
          (6 . "The daughter of a vassal knight who has been bragging about her purity")
          (7 . "The wife of a court officer")
          (8 . "A nun")
          (9 . "A spell-concocting old woman")
          (10 . "A life-sucking succubus")
          (11 . "A foreign lady visiting court")
          ((12 . 13) . "A lady one of the Player-knights has been courting, or desiring")
          ((14 . 15) . "An ambitious commoner wanting to better herself")
          ((16 . 17) . "A scheming serving wench")
          ((18 . 20) . "An innocent serving wench")))

(random-table/register :name "Pendragon > Book of Uther > Courtly Event > Woman Troubles > What"
  :private t
  :data '(
           (1 . "Silent embarrassment")
           ((2 . 3) . "Embarrassment—with demands the affair be kept secret")
           ((4 . 5) . "Shame—with a demand to keep silent about this or she will kill herself.  Roll again for her lover’s reaction")
           ((6 . 7) . "Denial")
           (8 . "Accusation of rape; he denies it")
           ((9 . 10) . "The accusation that the man seduced her while she is drunk; he denies it")
           (11 . "The accusation that the woman seduced him while he is drunk; she denise it")
           (12 . "To discard the man and try to seduce a Player-knight; her lover goes away quickly")
           ((13 . 14) . "Anger, threatening to ruin whoever reports it")
           ((15 . 16) . "Flight")
           (17 . "Laughter…")
           (18 . "Embarassment, beg the knight to keep silent")
           (19 . "An offer to pay money to keep this secret")
           (20 . "A threat to harm or kill the witness")))

(random-table/register :name "Pendragon > Book of Uther > Courtly Event > Whispering Campaign"
  :private t
  :data '(
           ((1 . 3) . "Is having an affair with {Pendragon > Book of Uther > Courtly Event > Woman Troubles > Who}")
           (4 . "Is an upstart with boundless ambition")
           (5 . "Is a braggart who doesn't deserve his status")
           (6 . "Is craven")
           (7 . "Is an oath-breaker")
           (8 . "Is really a base-born bastard")
           (9 . "Is really a noble-born bastard")
           (10 . "Is a lousy lover")
           (11 . "Is committing {Pendragon > Book of Uther > Courtly Event > Impropriety}")
           (12 . "Is a liar")
           (13 . "Is a drunk")
           (14 . "Is a thief")
           (15 . "Has been speaking ill of {Pendragon > Book of Uther > Courtly Event > Hostility}")
           (16 . "Has been meeting with {Pendragon > Book of Uther > Courtly Event > Unknown Person }")
           (17 . "Has been talking rebellion or treason")
           (18 . "Has renounced Christ")
           (19 . "Has a bastard child no one knows about")
           (20 . "Has an unnatural desire or appetite")))

(random-table/register :name "Pendragon > Winter Phase > Family Events"
  :data '(((1 . 2) . "Death in family")
           ((3 . 7) . "Marriage in family")
           ((8 . 12) . "Birth in family")
           ((13 . 15) . "Family member is missing or lost")
           ((16 . 18) . "No event")
           ((19 . 20) . "Scandalous rumor: {Pendragon > Scandalous Rumors}")))

(random-table/register :name "Pendragon > Winter Phase > Scandalous Rumors"
  :data '((1 . "Insulted a lord")
           ((2 . 3) . "Cheated at a tournament")
           (4 . "Impoverished, badly in debt")
           ((5 . 7) . "Adulterer")
           (8 . "Kidnapper")
           ((9 . 10) . "Horse-thief")
           ((11 . 16) . "Illicit love affair")
           (17 . "Murderer")
           (18 . "Heretic")
           (19 . "Necromancer")
           (20 . "{Pendragon > Scandalous Rumors (1d19)}…and its proven true!")))

(random-table/register :name "Pendragon > Winter Phase > Family Member"
  :data '(((1 . 3) . "Father")
           ((4 . 6) . "Mother")
           ((7 . 10) . "Brother")
           ((11 . 14) . "Sister")
           (15 . "Uncle")
           (16 . "Aunt")
           (17 . "Grandfather")
           (18 . "Grandmother")
           ((19 . 20) . "Cousin")))

(random-table/register :name "Pendragon > House Rule > Random Christian Trait"
  :roller "d20"
  :data '(
           (1 . "[Energetic/Lazy]")
           (2 . "[Generous/Selfish]")
           (3 . "[Honest/Deceitful]")
           (4 . "[Just/Arbitrary]")
           (5 . "[Prudent/Reckless]")
           (6 . "[Trusting/Suspicious]")
           (7 . "[Valorous/Cowardly]")
           ((8 . 9) . "{Pendragon > House Rule > Random Christian Trait > Opposing}")
           ((10 . 20) . "{Pendragon > House Rule > Random Christian Trait > Religious}")))
(random-table/register :name "Pendragon > House Rule > Random Christian Trait > Opposing"
  :private t
  :data '("Lustful" "Vengeful" "Cruel" "Proud" "Worldly" "Indulgent"))
(random-table/register :name "Pendragon > House Rule > Random Christian Trait > Religious"
  :private t
  :data '("Chaste" "Forgiving" "Merciful" "Modest" "Spiritual" "Temperate"))

(random-table/register :name "Pendragon > House Rule > Random Pagan Trait"
  :roller "d20"
  :data '((1 . "[Forgiving/Vengeful]")
           (2 . "[Just/Arbitrary]")
           (3 . "[Merciful/Cruel]")
           (4 . "[Prudent/Reckless]")
           (5 . "[Trusting/Suspicious]")
           (6 . "[Valorous/Cowardly]")
           (7 . "[Temperate/Indulgent]")
           ((8 . 9) . "{Pendragon > House Rule > Random Pagan Trait > Opposing}")
           ((10 . 20) . "{Pendragon > House Rule > Random Pagan Trait > Religious}")))
(random-table/register :name "Pendragon > House Rule > Random Pagan Trait > Opposing"
  :private t
  :data '("Lazy" "Selfish" "Deceitful" "Chaste" "Modest" "Worldly"))
(random-table/register :name "Pendragon > House Rule > Random Pagan Trait > Religious"
  :private t
  :data '("Energetic" "Generous" "Honest" "Lustful" "Proud" "Spiritual"))
