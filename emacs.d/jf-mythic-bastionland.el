(if (f-file?  "~/git/random-table.el/random-table.el")
  (require 'random-table "~/git/random-table.el/random-table.el")
  (use-package random-table
    :straight (:host github :repo "jeremyf/random-table.el")))

(random-table/register
  ;; From https://bit.ly/askthestars
  :name "Ask the Stars > Yes or No"
  :roller (lambda (&optional table)
            (if (yes-or-no-p "Is the answer likely yes?")
              (max (+ 1 (random 12)) (+ 1 (random 12)))
              (min (+ 1 (random 12)) (+ 1 (random 12)))))
  :data '("No" "No" "No"
           "No but" "No but" "No but"
           "Yes but" "Yes but" "Yes but"
           "Yes" "Yes" "Yes"))

(random-table/register
  ;; From https://bit.ly/askthestars
  :name "Ask the Stars > Signs & Positions"
  :data '("- Sign :: {Ask the Stars > Sign}\n- Position :: {Ask the Stars > Position}"))

(random-table/register
  :name "Ask the Stars > Sign"
  :private t
  :data '("The Fang (hostility - fear)"
           "The Wings (freedom - nature)"
           "The Cage (protection - obligation)"
           "The Hand (creation - misdirection)"
           "The Mask (persuasion - shame)"
           "The Eye (judgement - secrets)"
           "The Child (learning - greed)"
           "The Traveller (wandering - chance)"
           "The Elder (authority - tradition)"
           "The Ship (direciton - struggle)"
           "The Council (opposition - cycles)"
           "The Legion (unification - identity)"))

(random-table/register
  :name "Ask the Stars > Position"
  :private t
  :data '("Rising (growth - possibility)"
           "Entombed (memory - death)"
           "Twinned (intimacy - dependency)"
           "Waning (desire - decay)"
           "Rooted (stability - plenty)"
           "Bowed (submission - mercy)"
           "Colliding (change - violence)"
           "Burning (honesty - pride)"
           "Veiled (faith - deceit)"
           "Exiled (guilt - autonomy)"
           "Crowned (ambition - ruin)"
           "Reflected (reversal - vanity)"))

(random-table/scope
  "Mythic Bastionland"
  (random-table/register :name "Luck"
    :data
    '((1 . "Crisis: something immediately bad.")
       ((2 . 3) . "Problem: something potentially bad.")
       ((4 . 6) . "Blessing: a welcome result.")))
  (random-table/register :name "Passage of Time"
    :data
    '((1 . "Season or Age turns now.")
       ((2 . 3) . "Season or Age turns after next session.")
       ((4 . 6) . "The Season or Age continues.")))
  (random-table/register :name "Unresolved Situation"
    :data
    '((1 . "It goes as bad as it could possibly go.")
       ((2 . 3) . "It gets worse.")
       ((4 . 6) . "It gets better.")))
  (random-table/register :name "Travel > Wilderness Roll"
    :data
    '((1 . "Encounter the next Omen from a random Myth in this Realm.")
       ((2 . 3) . "Encounter the next Omen from the nearest Myth.")
       ((4 . 6) . "Encounter the Hex's Landmark.  Otherwise all clear")))
  (random-table/register :name "Travel > Travelling Blind"
    :data
    '((1 . "Circle back to where you started.")
       (2 . "Drift a hex to the left.")
       (3 . "Drift a hex to the right.")
       ((4 . 6) . "Progress as planned.")))
  (random-table/register :name "Travel > Dire Weather"
    :data
    '((1 . "Dire weather. You can't leave the Hex and can't get proper sleep.")
       ((2 . 3) . "Looming threat.  If this is rolled a second consecutive time, treat as dire weather.")
       ((4 . 6) . "Fine weather for travel.")))
  (random-table/register :name "Travel > Local Mood"
    :data
    '((1 . "Occupied by a looming or recent woe.")
       ((2 . 3) . "There is a sense of things in decline.")
       ((4 . 6) . "A fine mood and all seems well enough.")))
  (random-table/register :name "Dominion > Crisis Roll"
    :data
    '((1 . "Calamity: immediately gain 2 Crises.")
       ((2 . 3) . "Dilemma: choose between 2 Crises.")
       ((4 . 6) . "Prosperity: times are good.")))
  (random-table/register :name "Sparks > Nature > Land"
    :data
    '("Character: [Barren/Dry/Grey/Sparse/Sharp/Teeming/Still/Soft/Overgrown/Vivid/Sodden/Lush]; Landscape: [Marsh/Heath/Crags/Peaks/Forest/Valley/Hills/Meadow/Bog/Lakes/Glades/Plain]"))
  (random-table/register :name "Sparks > Nature > Sky"
    :data
    '("Tone: [Glittering/Violet/Sapphire/Pale/Fiery/Ivory/Slate/Pink/Golden/Bloody/Bright/Inky]; Texture: [Aurora/Haze/Marble/Glow/Billows/Swirl/Streaks/Dapple/Rays/Pillars/Shimmer/Swells]"))
  (random-table/register :name "Sparks > Nature > Water"
    :data
    '("Tone: [Crystal/Teal/Pearlescent/Mucky/Cobalt/Verdant/Frosted/Dark/Verdigris/Silver/Emerald/Jade]; Texture: [Silk/Ripples/Abyss/Churn/Froth/Mirror/Surge/Glass/Surf/Rapids/Spray/Bubbles]"))
  (random-table/register :name "Sparks > Nature > Weather"
    :data
    '("Description: [Gentle/Fleeting/Persistent/Bright/Thin/Cool/Hot/Solid/Dull/Faint/Abundant/Harsh]; Element: [Rain/Gusts/Cloud/Sunlight/Mist/Humidity/Thunder/Dust/Warmth/Drizzle/Breeze/Fog]"))

  (random-table/register :name "Sparks > Nature > Flora"
    :data
    '("Nature: [Aromatic/Ashen/Blooming/Twisted/Towering/Fruitful/Stinging/Vibrant/Brittle/Thorny/Sturdy/Resinous]; Form: [Grasses/Heather/Shrubs/Brambles/Canopy/Ferns/Trunks/Vines/Conifers/Sapllings/Reeds/Roots]"))

  (random-table/register :name "Sparks > Nature > Fauna"
    :data
    '("Nature: [Watchful/Helpful/Graceful/Loud/Mischievous/Deceitful/Enlightening/Placid/Beautiful/Mighty/Hostile/Aloof]; Form: [Ungulates/Songbirds/Canines/Rodents/Amphibians/Insects/Felines/Reptiles/Hawks/Mustelids/Fowl/Bears]"))

  (random-table/register :name "Sparks > Nature > Feature"
    :data
    '("Nature: [Buried/Colourful/Adorned/Spiked/Split/Entombed/Reflective/Veiled/Hot/Drowned/Descecrated/Isolated]; Form: [Brook/Seat/Pit/Cave/Monolith/Mound/Cairn/Pond/Waterfall/Spring/Arch/Henge]"))

  (random-table/register :name "Sparks > Nature > Wonder"
    :data
    '("Theme: [Pleasure/Secrets/Prophecy/Healing/Desire/Memory/Death/Strength/Temptation/Pain/Regret/Time]; Element: [Light/Flames/Stones/Beasts/Sparks/Trails/Mist/Colours/Plants/Wind/Water/Shadows]"))

  (random-table/register :name "Sparks > Nature > Otherworld"
    :data
    '("Character: [Acidic/Black/Smoke/Frozen/Dead/Broken/Colossal/Living/Burning/Sludge/White/Sweet]; Landscape: [Flats/Labyrinth/Ruins/Stairs/Desert/Craters/Cavern/Jungle/Dunes/Tunnels/Island/Mountain]"))

  (random-table/register :name "Sparks > Civilisation > Holding"
    :data
    '("Style: [Dark/Ruined/Hostile/Ancient/Ornate/Wild/Pristine/Fortified/Unfinished/Welcoming/Proud/Bright]; Feature: [Turrets/Tower/Wall/Battlements/Citadel/Gate/Spire/Dome/Beacons/Bridge/Pillars/Moat]"))

  (random-table/register :name "Sparks > Civilisation > Bailey"
    :data
    '("Style: [Filthy/Abandoned/Joyous/Sophisticated/Industrious/Humble/Majestic/Hallowed/Rustic/Solemn/Bustling/Immaculate]; Feature: [Marketplace/Forge/Library/Fountain/Temple/Forum/Tomb/Garden/Hall/Workshops/Arena/Garrison]"))

  (random-table/register :name "Sparks > Civilisation > Keep"
    :data
    '("Centerpiece: [Hearth/Throne/Musicians/Pool/Advisers/Servants/Shrine/Table/Reliquary/Cauldron/Chandelier/Guards]; Decoration: [Antlers/Silver/Heraldry/Bones/Flowers/Scripture/Jewels/Wreaths/Candles/Fur/Tapestries/Shields]"))

  (random-table/register :name "Sparks > Civilisation > Food"
    :data
    '("Quality: [Spiced/Herbal/Crunchy/Sour/Dry/Fermented/Salted/Wet/Fatty/Chewy/Sweet/Mild]; Type: [Fish/Fruit/Stew/Mushrooms/Pie/Cheese/Nuts/Cake/Porridge/Bread/Vegetables/Meat]"))

  (random-table/register :name "Sparks > Civilisation > Goods"
    :data
    '("Theme: [Military/Abundant/Traditional/Specialist/Industrious/Innovative/Secretive/Simple/Strong/Decorated/Fine/Lucky]; Type: [Textile/Livestock/Grain/Mead/Tools/Stone/Wood/Pottery/Metal/Leather/Honey/Herb]"))

  (random-table/register :name "Sparks > Civilisation > Luxuries"
    :data
    '("Rarity: [Antique/Intricate/Unique/Scarce/Hazardous/Flawless/Luminous/Lost/Esoteric/Sacred/Mythical/Beautiful]; Type: [Jewel/Wine/Spice/Fragrance/Silk/Fur/Artwork/Sword/Creature/Ore/Root/Scripture]"))

  (random-table/register :name "Sparks > Civilisation > Drama"
    :data
    '("Theme: [Betrayal/Jealousy/Rivalry/Infedility/Coup/Ambition/Redemption/Revelation/Wrath/Greed/Banishment/Manipulation]; Detail: [Brawl/Poison/Oath/Feast/Letters/Disguise/Inheritance/Assassin/Family/Alcohol/Blackmail/Gold]"))

  (random-table/register :name "Sparks > Civilisation > Woe"
    :data
    '("Description: [Secretive/Violent/Looming/Sudden/Ongoing/Prophecised/Mysterious/Sanctioned/Unseen/Vast/Escalating/Concealed]; Incident: [Disease/Famine/Raids/Invasion/Abduction/Storm/Fire/Revolt/Exodus/Beast/Killing/Theft]"))

  (random-table/register :name "Sparks > Civilisation > News"
    :data
    '("Subject: [Duel/Birth/Market/Trial/Ritual/Mercenaries/Festival/Tournament/Punishment/Performance/Death/Marriage]; Mood: [Pensive/Joyous/Content/Divided/Furious/Sceptical/Adoring/Nostalgic/Unified/Bleak/Solemn/Optimistic]"))

  (random-table/register :name "Sparks > People > Appearance"
    :data
    '("Physique: [Delicate/Short/Robust/Hard/Haggard/Cold/Warm/Youthful/Soft/Sickly/Tall/Rough]; Dress: [Armoured/Tattered/Vibrant/Crude/Eclectic/Traditioinal/Comfortable/Gaudy/Drab/Decorated/Functional/Elegant]"))

  (random-table/register :name "Sparks > People > Voice"
    :data
    '("Tone: [Whispering/Soothing/Smooth/Flat/Mumbled/Weak/Strong/Hesitant/Melodic/Gravelly/Erratic/Booming]; Manner: [Formal/Poetic/Precise/Intense/Rambling/Detached/Passionate/Terse/Relaxed/Blunt/Boisterous/Friendly]"))

  (random-table/register :name "Sparks > People > Personality"
    :data
    '("Demeanour: [Cautious/Spiritual/Intellectual/Ambitious/Serene/Righteous/Empathetic/Unstable/Prying/Melancholic/Cynical/Rash]; Interest: [Botany/History/Music/Gambling/Animals/Art/Cookery/Craft/Fishing/Fashion/Hunting/Stories]"))

  (random-table/register :name "Sparks > People > Relationship"
    :data
    '("State: [Adoring/Reluctant/Secret/Estranged/Hateful/Distant/Harmonious/Intimate/Recent/Sworn/Tumultuous/Resentful]; Connection: [Kin/Friend/Lover/Spouse/Supporter/Ally/Rival/Successor/Mentor/Peer/Enemy/Guardian]"))

  (random-table/register :name "Sparks > People > Desire"
    :data
    '("Ambition: [Escape/Wealth/Status/Knowledge/Mastery/Heirloom/Marriage/Truth/Travel/Power/Security/Forgiveness]; Motive: [Freedom/Love/Legacy/Recovery/Revenge/Duty/Fear/Guilt/Recognition/Defiance/Curiosity/Hatred]"))

  (random-table/register :name "Sparks > People > Task"
    :data
    '("Action: [Investigate/Capture/Destroy/Transport/Retrieve/Mend/Break/Guard/Aid/Salvage/Conceal/Hunt]; Subject: [Knight/Seer/Vassals/Livestock/Monument/Gold/Ruin/Animals/Dwelling/Holding/Bridge/Warband]"))

  (random-table/register :name "Sparks > People > Background"
    :data
    '("Upbringing: [Deprived/Pious/Outcast/Military/Insular/Nomadic/Drudgery/Mercantile/Feral/Prestigious/Academic/Pampered]; Memory: [War/Migration/Riding/Study/Exile/Joy/Sickness/Escape/Injury/Friendship/Execution/Romance]"))

  (random-table/register :name "Sparks > People > Ailment"
    :data
    '("Descriptor: [Hidden/Mild/Intermittent/Growing/Medicated/Denied/Unexplained/Constant/Diminishing/Permanent/Debilitating/Obvious]; Symptom: [Insomnia/Migraines/Arthritis/Nausea/Fixation/Blindness/Deafness/Melancholy/Shaking/Frailty/Coughing/Lethargy]"))

  (random-table/register :name "Sparks > People > Heraldry"
    :data
    '("Palette: [Light/Hot/Earhty/Rich/Metallic/Brilliant/Grey/Jewelled/Subdued/Airy/Cold/Dark]; Symbol: [Beast/Bird/Fish/Weapon/Crown/Tree/Flower/Bodypart/Structure/Ring/Tool/Star]"))

  (random-table/register :name "Sparks > Combat > Soldier"
    :data
    '("Quality: [Mobile/Reluctant/Mounted/Renowned/Zealous/Conscript/Cowardly/Heavy/Bloodthirsty/Fancy/Fearsome/Mercenary]; Type: [Skirmisher/Archer/Scout/Militia/Guard/Infilitrator/Raider/Veteran/Infantry/Rider/Charger/Knight]"))

  (random-table/register :name "Sparks > Combat > Weapon"
    :data
    '("Descriptor: [Short/Pole/Chain/Barbed/Forked/Curved/Weighted/Double/Crossed/Throwing/Thin/Long]; Feature: [Blade/Spear/Axe/Mace/Cleaver/Hammer/Spike/Hook/Club/Rod/Fang/Sword]"))

  (random-table/register :name "Sparks > Combat > Manoeuvers"
    :data
    '("Action: [Feint/Strike/Boast/Defend/Negotiate/Flurry/Rush/Taunt/Jab/Charge/Onslaught/Provoke]; Intent: [Demoralise/Confuse/Exploit/Stall/Relocate/Observe/Defeat/Expose/Surprise/Stagger/Weaken/Intimidate]"))

  (random-table/register :name "Sparks > Combat > Conflict"
    :data
    '("Dispute: [Border/Religion/Succession/Resource/Debt/Betrayal/Theft/Conquest/Marriage/Deceit/Waterway/Bloodfeud]; Status: [War/Raids/Animosity/Truce/Skirmishes/Standoff/Occupation/Stalemate/Blockade/Tension/Forgotten/Negotiations]"))

  (random-table/register :name "Sparks > Combat > Duel"
    :data
    '("Stipulation: [Joust/Swords/Partner/Team/Unmounted/Chained/Blood/Death/Surrender/Judged/Blunt/Javelins]; Twist: [Timed/Pit/Bridge/Immobile/Maze/Archers/Fire/Beasts/Mud/Night/Water/Cage]"))

  (random-table/register :name "Sparks > Combat > Battlefield"
    :data
    '("Feature: [River/Ruins/Hill/Forest/Lake/Outpost/Pass/Farm/Trail/Bridge/Wall/Dwelling]; Detail: [Smoke/Mud/Flies/Trenches/Tower/Boulders/Flowers/Streams/Thorns/Stink/Ravine/Tombs]"))

  (random-table/register :name "Sparks > Combat > Deployment"
    :data
    '("Style: [Aggressive/Mobile/Tight/Deceptive/Shielded/Rigid/Flexible/Open/Focused/Dispersed/Reinforced/Defensive]; Formation: [Line/Column/Chevron/Ranks/Square/Circle/Flank/Skirmish/Block/Square/Wedge/Scatter]"))

  (random-table/register :name "Sparks > Combat > Strategy"
    :data
    '("Plan: [Encirlce/Capture/Assault/Harass/Pillage/Outlast/Ambush/Overwhelm/Blockade/Divide/Focus/Counter]; Twist: [Darkness/Reserves/Diversion/Betrayal/Artillery/Camouflage/Bluff/Delay/Decoy/Bait/Sacrifice/Fire]"))

  (random-table/register :name "Sparks > Combat > Event"
    :data
    '("Subject: [Allies/Morale/Weather/Terrain/Strategy/Discipline/Animal/Opportunists/Deception/Leader/Loot/Weapons]; Event: [Collapse/Attack/Slaugher/Stalemate/Stall/Falter/Sabotage/Scatter/Charge/Confusion/Worsening/Discovery]"))

  (random-table/register :name "Holdings > Threat Roll"
    :data
    '((1 . "News arrives of the next Omen from the Seat of Power’s Holding Thread having come to pass.")
       ((2 . 3) . "Trigger the next Omen from this Holding's Thread.")
       ((4 . 6) . "Court is quiet enough, no big surprises.")))
  (random-table/register :name "Holdings > Progress Roll"
    :data
    '((1 . "You are informed about the next two Omens that occurred in your absence.")
       ((2 . 3) . "You are informed about the next Omen that occurred in your absence.")
       ((4 . 6) . "No Omens have passed.")))
  (random-table/register :name "Squires"
    :data
    '((1 . "They have brought {Mythic Bastionland > Squires > They Have Brought}")
       (2 . "They are adept at {Mythic Bastionland > Squires > They Are Adept At}")
       (3 . "They come from {Mythic Bastionland > Squires > They Come From}")
       (4 . "They are accompanied by {Mythic Bastionland > Squires > They Are Accompanied By}")
       (5 . "They possess an unusual {Mythic Bastionland > Squires > They Possess an Unusual}")
       (6 . "It is prophesied that they {Mythic Bastionland > Squires > It is Prophesied That They}")))
  (random-table/register :name "Squires > They Have Brought"
    :data
    '("a grand but ill-fitting helm (A1)." "A shrill flute played often."
       "fine clothes & a pouch of coins." "A homemade shield (A1)."
       "a small cask of pauper's mead." "Myriad layers of clothing (A1)."
       "somehow, a crossbow (2d8 slow)." "somehow, a coat of mail (A1)."
       "a tiny mirror of poor quality." "sustenance, loaded on by kin."
       "forage stimulant, to share." "a Seer-given sacrament."))
  (random-table/register :name "Squires > They Are Adept At"
    :data
    '("sprinting, swift as a hare." "climbing, sure as a squirrel."
       "singing, sweet as a nightingale." "swimming, as a fish in water."
       "eavesdropping, ears like a bat." "judging character."
       "storytelling, with the ability to enchant any audience." "sensing direction."
       "scholarship; a fountain of facts." "horse-whispering, be it race, pack, cart, or war."
       "herbalism, familiar with flora." "hunting and tracking."))
  (random-table/register :name "Sparks > Ceremony"
    :data
    "Mood: [Solemn/Celebratory/Poetci/Ostentatious/Charitable/Brutal/Nurturing/Vindictive/Humble/Formal/Despairing/Joyous]; Ritual: [Burial/Drinking/Oration/Music/Combat/Decoration/Breaking/Sacrifice/Pleading/Washing/Burning/Expedition]")

  (random-table/register :name "Sparks > Spectacle"
    :data
    "Activity: [Performance/Joust/Pyre/Artwork/Games/Battle/Wrestling/Relics/Gifts/Athletics/Construction/Hunt]; With: [Costumes/Vastness/Smoke/Death/Duration/Flowers/Blood/Altitude/Gold/Jewels/Ships/Soil]")

  (random-table/register :name "Sparks > Revelry"
    :data
    "Banquet: [Boozy/Imported/Sweet/Layered/Meaty/Sparkling/Sizzling/Aged/Fatty/Bubbling/Spiced/Fruited]; Amusement: [Stories/Dancing/Pranks/Masks/Gambling/Boasts/Excess/Ridicule/Stimulant/Bathing/Darkness/Destruction]"))
