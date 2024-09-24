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

(random-table/register :name "Pendragon > Book of Feasts > First Course"
  :data '("Meat with mustard" "Ale and cheese quiche" "Swan neck pudding" "Beef marrow fritters"
           "Eels in thick spicy puree" "Roast baby swan" "Fat capon" "Roast heron"
           "Roast pheasant" "Loach in cold green sauce" "Fruit tarts" "Cold meat slicese in ginger sauce"
           "Custard with dried fruit" "Porpoise and peas" "Parsley bread" "Haslet"
           "Roasted salmon in wine sauce" "Almond omelet" "Cod tails" "Pastries (plum, quince, and apple)"))

(random-table/register :name "Pendragon > Book of Feasts > Second Course"
  :data '("Artichokes stuffed with blueberry rice" "Broth with bacon and peas" "Meat tiles" "Roasted seal"
           "Honey-glazed roast chicken rolled with mustard and pine nuts" "Heraldic emblem in meat jelly" "stuffed boar" "Peacock (redressed in its feathers before service)"
           "Astrological temperament herb cakes and cheses" "Capon pastries and chips" "Roasted crane" "Roasted coney"
           "Bream and eel pasties" "Boiled bittern" "Frumenty (boiled wheat custard)" "Pullets (similar to Cornish game hen)"
           "Beaver tails" "Cockentrice (pigs head on chicken body)" "Lampreys in hot sauce" "Fruit and salmon pie"))

(random-table/register :name "Pendragon > Book of Feasts > Third Course"
  :data '("Herb fritters" "Roasted chicken and pheasant wings" "A selection of cheeses" "Quinces stewed in wine"
           "Roasted pigeon" "Elderberry divination cakes" "Roast larks" "Venison in frumenty"
           "White poultry meat stewed in wine" "Almond cakes served on roundels" "Glazed eggs" "Imported or baked fruits"
           "Doucette (custard and bone marrow pie)" "Roast eagle" "Roast bream served in a dariole (pastry mold)" "Turnips baked with cheese"
           "Hippocras" "Mushroom tarts" "Wafers" "Whole dry spices “to aid in digestion”"))

(defconst rpgs/pendragon/traits
  '("Chaste" "Energetic" "Forgiving" "Generous" "Honest" "Just" "Merciful" "Modest" "Prudent"
     "Spiritual" "Temperate" "Trusting" "Valorous" "Lazy" "Vengeful" "Selfish" "Deceitful"
     "Arbitrary" "Cruel" "Proud" "Reckless" "Worldly" "Indulgent" "Suspicious" "Cowardly"))

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
