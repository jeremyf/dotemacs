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

  (random-table/register :name "Holdings > Thread Roll"
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
       "herbalism, familiar with flora." "hunting and tracking.")))
