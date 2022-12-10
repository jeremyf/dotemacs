;;; jf-gaming.el --- Gaming related functions -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Code

;;;; Dependencies
(require 'jf-minor-mode-maker)
(require 'jf-quick-help)
(require 'jf-org-mode)

(use-package decide
  :straight (decide :host github :type git :repo "jeremyf/decide-mode"))

(use-package org-d20
  ;; I’m really only using this for the ~org-d20--roll~ function.
  :after org
  :bind  (("C-s-r" . jf/roll-expression-dwim))
  :config
  (defun jf/roll-expression-dwim (expression &optional)
    "Roll the EXPRESSION, check `thing-at-point' then prompt."
    (interactive (list (if (string-match
			    "[dD][0-9]"
			    (format "%s" (thing-at-point 'sexp t)))
			   (thing-at-point 'sexp t)
			 (read-string "Dice Expression: "))))
    (-let* (((rolls . result) (org-d20--roll expression)))
      (message "%s => %s" expression result)))
  :straight (org-d20 :host github :repo "spwhitton/org-d20"))

;;;; Burning Wheel Code
(jf/minor-mode-maker :title "Burning Wheel Gold"
		     ;; Being a programmer and someone who plays table top
		     ;; role-playing games (TTRPG), I’ve often used the TTRPG
		     ;; rules-set or systems to explore programming languages
		     ;; and processes.  After all, I understand the TTRPG rules
		     ;; well enough (or the algorithm’s description) that I can
		     ;; spend time thinking through my approach in a
		     ;; programming language.
                     :abbr "bwg"
                     :hooks (list 'org-mode-hook 'markdown-mode-hook))

(jf/transient-quick-help jf/bwg-qh-wises
  :label "Wises"
  :header "BWG Wises Obstacles (page 309)"
  :body
  (s-join
   "\n"
   '("Common knowledge .............. Ob 1"
     "An interesting fact ........... Ob 2"
     "Details ....................... Ob 3"
     "Uncommon knowledge ............ Ob 4"
     "Rare goods .................... Ob 5"
     "Bizarre or obscure ............ Ob 7"
     "Freaky details or specifics ... Ob 8")))

(jf/transient-quick-help jf/bwg-qh-expertise-exponent
  :label "Exponents"
  :header "BWG Expertise Exponent (page 12)"
  :body
  (s-join
   "\n"
   '("Exp 1  is naturally disinclined, crippled, or utterly incompetent."
     "Exp 2  is untrained, raw, weak, or unpracticed."
     "Exp 3  is nominally trained and practiced."
     "Exp 4  is competent; everday stuff doesn't pose a challenge."
     "Exp 5  is expert."
     "Exp 6  is near mastery."
     "Exp 7  is excellence."
     "Exp 8  is total mastery, complete understanding."
     "Exp 9  is uncanny; incomprehensibly good."
     "Exp 10 is as near perfection as the system allows.")))

(jf/transient-quick-help jf/bwg-qh-absolute-difficulty
  :label "Difficulty"
  :header "BWG Absolute Difficulty (page 15)"
  :body
  (s-join
   "\n"
   '("Ob 1  A simple act done with little thought."
     "Ob 2  An act performed routinely at your job."
     "Ob 3  An act you can accomplish if you concentrate."
     "Ob 4  A risky act."
     "Ob 5  An act that requires expertise."
     "Ob 6  An act that requires a heroic effort."
     "Ob 7  An improbable feat."
     "Ob 8  An act requiring preternatural ability or a lot of help."
     "Ob 9  An act deemed nearly impossible."
     "Ob 10 A miracle.")))

(jf/transient-quick-help jf/bwg-qh-circles-obstacles
  :label "Circles"
  :header "BWG Circles Obstacles (page 380-381)"
  :body
  (s-join
   "\n"
   '("Occupation"
     "  Broad occupation/profession, same life path ... +0 Ob"
     "  Uncommon occupation, or within same setting ... +2 Ob"
     "  Specific occupation, rare/unique occupation ... +3 Ob"
     ""
     "Station"
     "  Same station .................................. +0 Ob"
     "  Lower rank, station, or class ................. +1 Ob"
     "  Higher rank, station, or class ................ +2 Ob"
     "  Highest station or rank in the setting ........ +3 Ob"
     ""
     "Disposition and Knowledge"
     "  Common to circle .............................. +0 Ob"
     "  Different from circle members ................. +1-2 Ob"
     "  Specific, detailed, or rare ................... +3 Ob"
     ""
     "Time and Place"
     "  Doesn't matter ................................ +0 Ob"
     "  Unusual for this character .................... +1-2 Ob"
     "  Right here and now in the middle of trouble ... +3 Ob")))

(jf/transient-quick-help jf/bwg-qh-circles-alternate
  :header "BWG Circles 2006 (Burning Anthology p7)"
  :label "Circles 2006"
  :body
  (s-join
   "\n"
   '("Occupation"
     "  Broad/common .................... +0 Ob"
     "  Uncommon ........................ +2 Ob"
     "  Specific/rare/unique ............ +3 Ob"
     ""
     "Station"
     "  Same station .................... +0 Ob"
     "  Higher or lower rank ............ +1 Ob"
     "  Lowest .......................... +2 Ob"
     "  Highest ......................... +3 Ob"
     ""
     "Attitude"
     "  Neutral to PC ................... +0 Ob"
     "  Predisposed or opposed .......... +1 Ob"
     "  Proponent, loyal, or specific ... +3 Ob"
     ""
     "Knowledge"
     "  Unimportant ..................... +0 Ob"
     "  General for subject ............. +1 Ob"
     "  Specific subject ................ +3 Ob"
     ""
     "Skill"
     "  Typical (Exponent 3) ............ +0 Ob"
     "  Competent (Exponent 4) .......... +1 Ob"
     "  Expert (Expoonent 5) ............ +2 Ob"
     "  Master (Exponent 6) ............. +3 Ob"
     ""
     "Place/Time"
     "  Prior to important test ......... +0 Ob"
     "  Prior to conflict ............... +1 Ob"
     "  In midst of conflict ............ +3 Ob")))

(jf/transient-quick-help jf/bwg-qh-steel-test-adjustments
  :header "BWG Steel Test Adjustments (page 363)"
  :label "Steel"
  :body
  (s-join
   "\n"
   '("Conditions for Steel Advantags"
     "  Being startled by something mundane ......... +2D"
     "  Feeling safe in a group of friends/allies ... +1D"
     ""
     "Conditions for Steel Disadvantages"
     "  Being shot at ............................... +1 Ob"
     "  Being directly affect by magic .............. +1 Ob"
     "  Witnessing a person killed .................. +1 Ob"
     "  Small explosions ............................ +2 Ob"
     "  Committing murder ........................... +2 Ob"
     "  Explosions .................................. +3 Ob"
     "  Witnessing pronounced sorcery at play ....... +3 Ob"
     "  Seeing a ghost .............................. +3 Ob"
     "  Seeing the living dead ...................... +4 Ob"
     "  Volcanic eruptions, cataclysm ............... +4 Ob"
     "  Seeing horrible magic at work ............... +4 Ob"
     "  Being in the presence of the supernatural ... +5 Ob")))

(jf/transient-quick-help jf/bwg-qh-test-difficulty
  :header "BWG Difficulty of Test by Dice Rolled (p41)"
  :label "Test Difficulty"
  :body
  (s-join
   "\n"
   '("| Dice | Routine | Difficult | Challenging |
          |------+---------+-----------+-------------|
          |   1D | Ob 1    | Ob 1      | Ob 2+       |
          |   2D | Ob 1    | Ob 2      | Ob 3+       |
          |   3D | Ob 1-2  | Ob 3      | Ob 4+       |
          |   4D | Ob 1-2  | Ob 3-4    | Ob 5+       |
          |   5D | Ob 1-3  | Ob 4-5    | Ob 6+       |
          |   6D | Ob 1-4  | Ob 5-6    | Ob 7+       |
          |   7D | Ob 1-4  | Ob 5-7    | Ob 8+       |
          |   8D | Ob 1-5  | Ob 6-8    | Ob 9+       |
          |   9D | Ob 1-6  | Ob 7-9    | Ob 10+      |
          |  10D | Ob 1-7  | Ob 8-10   | Ob 11+      |
          |  11D | Ob 1-8  | Ob 9-11   | Ob 12+      |
          |  12D | Ob 1-9  | Ob 10-12  | Ob 13+      |
          |  13D | Ob 1-10 | Ob 11-13  | Ob 14+      |
          |  14D | Ob 1-11 | Ob 12-14  | Ob 15+      |
          |  15D | Ob 1-12 | Ob 13-15  | Ob 16+      |
          |  16D | Ob 1-13 | Ob 14-16  | Ob 17+      |
          |  17D | Ob 1-14 | Ob 15-17  | Ob 18+      |
          |  18D | Ob 1-15 | Ob 16-18  | Ob 19+      |")))

(defconst jf/bwg-mortal-wounds-scale
  ;; When running Burning Wheel Gold, on occassion I need to establish the PTGS
  ;; for a creature or person.
  ;;
  ;; Yes, I could’ve written out (0 "B1" "B2" "B3" "B4" "B5" "B6" "B7" "B8" "B9"
  ;; "B10" "B11" "B12" "B13" "B14" "B15" "B16" "G1" "G2" "G3" "G4" "G5" "G6"
  ;; "G7" "G8" "G9" "G10" "G11" "G12" "G13" "G14" "G15" "G16" "W1" "W2" "W3"
  ;; "W4" "W5" "W6" "W7" "W8" "W9" "W10" "W11" "W12" "W13" "W14" "W15" "W16")
  ;; faster than the following constant, but I wanted to learn a bit of
  ;; emacs-lisp, so I chose to write the following.

  ;; I copied that text string from the introspected variable.  Because if I
  ;; wasn't going to write it the first time, I sure wasn't going to do it if I
  ;; had already stored that value in a constant.
  (let* ((shades '("B" "G" "W"))
         (rank '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
         (scale (-flatten
                 (-map (lambda (s)
                         (-map (lambda (r)
                                 (format "%s%s" s r))
                               rank))
                       shades))))
    ;; I want B1 to have an index of 1.  Hence pre-pending the 0 to the list.
    (add-to-list 'scale 0)
    scale)
  "The BWG Physical Tolerance Grayscale (from B1 to W16).")

(defun jf/bwg-qh-ptgs (forte power &optional round-up)
  "Calculate Burning Wheel PTGS.

This requires FORTE, POWER, and ROUND-UP.

Note, this assumes Black or Grey shade only."
  (interactive "sForte: \nsPower: \nP")
  ;; Note the / function uses integer floor division.
  (let* ((forte-index (-elem-index (upcase forte) jf/bwg-mortal-wounds-scale))
         (power-index (-elem-index (upcase power) jf/bwg-mortal-wounds-scale)))
    (unless forte-index (throw 'invalid-entry (format "Invalid Forte %s" forte)))
    (unless power-index (throw 'invalid-entry (format "Invalid Power %s" power)))
    (let ((scale (cond ((and (< forte-index 17) (< power-index 17))
                        (jf/bwg-qh-ptgs--shade-black forte-index
                                                     power-index
                                                     round-up))
                       ((and (< forte-index 17) (< power-index 34))
                        (jf/bwg-qh-ptgs--shade-black forte-index
                                                     (- power-index 16)
                                                     round-up
                                                     :add 2))
                       ((and (< forte-index 34) (< power-index 17))
                        (jf/bwg-qh-ptgs--shade-black (- forte-index 16)
                                                     power-index
                                                     round-up
                                                     :add 2))
                       ((and (< forte-index 34) (< power-index 34))
                        (jf/bwg-qh-ptgs--shade-gray forte-index
                                                    power-index
                                                    round-up)))))
      (jf/bwg-qh-ptgs--render-popup :power power
                                    :forte forte
                                    :round-up round-up
                                    :scale scale))))

(cl-defun jf/bwg-qh-ptgs--render-popup (&key power forte round-up scale)
  "Render PTGS help for given POWER, FORTE, ROUND-UP, and SCALE

  Where SCALE is a list of 6 elements: Su, Li, Mi, Se, Tr, and Mo"
  (jf/quick-help
   :header (concat "BWG PTGS (p98, p546) Forte:"
		   forte ", Power: " power (when round-up " (Rounded up)"))
   :body (format (concat
                  "Su %s; Li %s; Mi %s; Se %s; Tr %s; Mo %s"
                  "\n\nForte: %s\nPower: %s"
                  (when round-up "\n(Rounded up)") "\n")
                 (nth (nth 0 scale) jf/bwg-mortal-wounds-scale)
                 (nth (nth 1 scale) jf/bwg-mortal-wounds-scale)
                 (nth (nth 2 scale) jf/bwg-mortal-wounds-scale)
                 (nth (nth 3 scale) jf/bwg-mortal-wounds-scale)
                 (nth (nth 4 scale) jf/bwg-mortal-wounds-scale)
                 (nth (nth 5 scale) jf/bwg-mortal-wounds-scale)
                 (upcase forte)
                 (upcase power))))

(cl-defun jf/bwg-qh-ptgs--shade-black (forte power
					     &optional round-up
					     &key (add 0))
  "Calculate Black shade PTGS (BWGR p98, p546).

This requires FORTE, POWER, and ROUND-UP.

Returns a list of 6 elements: Su, Li, Mi, Se, Tr, and Mo"
  (interactive "nForte: \nnPower: \nP")
  ;; Note the / function uses integer floor division.
  (let* ((mw-rnd (if round-up 1 0))
         (mo (+ 6 add (/ (+ forte power mw-rnd) 2)))
         (step (+ (/ (+ 1 forte) 2)))
         (su (+ 1 (/ forte 2)))
         (li (+ su step))
         (mi (+ li step))
         (se (+ mi step))
         (tr (+ se step))
         (tr (if (< tr mo) tr (- mo 1)))
         (se (if (< se tr) se (- tr 1)))
         (mi (if (< mi se) mi (- se 1)))
         (li (if (< li mi) li (- mi 1))))
    (list su li mi se tr mo)))

(defun jf/bwg-qh-ptgs--shade-gray (forte power &optional round-up)
  "Calculate Gray shade PTGS (BWGR p546).

This requires FORTE, POWER, and ROUND-UP.

Returns a list of 6 elements: Su, Li, Mi, Se, Tr, and Mo"
  (let* ((fort-exponent (- forte 16))
         (mw-rnd (if round-up 1 0))
         (mo (+ 6 (/ (+ forte power mw-rnd) 2)))
         (su fort-exponent)
         (li (+ su fort-exponent))
         (mi (- mo 4))
         (se (- mo 2))
         (tr (- mo 1)))
    (list su li mi se tr mo)))

(defconst jf/bwg-lifepath--path-to-html-file
  ;; While running (or playing) a game of Burning Wheel, it can be useful to have
  ;; access to character lifepaths.  These can give you insight into a quick NPC.
  ;;
  ;; I have transformed and edited the http://charred-black.herokuapp.com/#/ into
  ;; individual YAML files that sit on my local machine.  I also created a bit of
  ;; https://gohugo.io/ code to render lifepaths from those YAML files.  You can
  ;; see an example at
  ;; https://takeonrules.com/2018/10/10/burning-wheel-lifepaths-inspired-by-warhammer-fantasy/
  ;;
  ;; I’m thinking what would be useful to create a searchable index of those
  ;; lifepaths.  For now, I’ll search based on the stock, setting, and lifepath
  ;; name (all of which happen to be in the pathname of the YAML file).
  ;;
  ;; But instead of hopping to the YAML file, I’d like to jump to the spot on an
  ;; HTML page with that information.  This way when I “find” a lifepath, I can
  ;; see what other lifepaths are of comparable station (a common need when
  ;; testing Circles).
  "~/git/org/assets/burning-wheel.html"
  "The path to an HTML")

(defconst jf/bwg-lifepath--narrowing-regexp
  "data-lifepath="
  "All lines in `jf/bwg-lifepath--path-to-html-file' that have this substring
 contain filterable data.")

(global-set-key (kbd "C-M-s-b") 'jf/menu--bwg)
(transient-define-prefix jf/menu--bwg ()
  "Define the BWG help prefix."
  ["Burning Wheel"
   ("c" jf/bwg-qh-circles-obstacles)
   ("C" jf/bwg-qh-circles-alternate)
   ("d" jf/bwg-qh-absolute-difficulty)
   ("e" jf/bwg-qh-expertise-exponent)
   ("p" "PTGS" jf/bwg-qh-ptgs)
   ("s" jf/bwg-qh-steel-test-adjustments)
   ("t" jf/bwg-qh-test-difficulty)
   ("w" jf/bwg-qh-wises)
   ])

;;;; Core RPG

(jf/minor-mode-maker :title "CORE RPG"
                     :abbr "core"
                     :hooks (list 'org-mode-hook 'markdown-mode-hook))

(jf/transient-quick-help jf/rpg-core-qh-stat-scores
  :label "Stat Descriptors"
  :header "CORE Stat Descriptors"
  :body
  (s-join
   "\n"
   '("1 ... Average"
     "2 ... Remarkable"
     "3 ... Excellent"
     "4 ... Gifted"
     "5 ... Prodigy"
     "6 ... Apex")))

(jf/transient-quick-help jf/rpg-core-qh-skill-scores
  :label "Skill Descriptors"
  :header "CORE Skill Descriptors"
  :body
  (s-join
   "\n"
   '("1 ... Trained"
     "2 ... Competent"
     "3 ... Veteran"
     "4 ... Expert"
     "5 ... Innovator"
     "6 ... Legend")))

(jf/transient-quick-help jf/rpg-core-qh-item-bonuses
  :label "Item Bonus Levels"
  :header "CORE Item Bonus Levels"
  :body
  (s-join
   "\n"
   '("1 ... Trained"
     "2 ... Competent"
     "3 ... Veteran"
     "4 ... Expert"
     "5 ... Innovator"
     "6 ... Legend")))

(jf/transient-quick-help jf/rpg-core-qh-fame-levels
  :label "Fame Levels"
  :header "CORE Fame Levels"
  :body
  (s-join
   "\n"
   '("1 ... Local/Professional"
     "2 ... Regional/Subcultural"
     "3 ... National/Cultural"
     "4 ... International/Global"
     "5 ... Historical/Legendary"
     "6 ... Mythic Universal")))

(jf/transient-quick-help jf/rpg-core-qh-difficulty-levels
  :label "Difficulty Levels (DL)"
  :header "CORE Difficulty Levels"
  :body
  (s-join
   "\n"
   '("1 .... No-Brainer"
     "2 .... Easy"
     "3 .... Challenging"
     "4 .... Difficulut"
     "5 .... Hard"
     "6 .... Very Hard"
     "7 .... Unlikely"
     "8 .... Ridiculous"
     "9 .... Absurd"
     "10 ... Insane")))

(jf/transient-quick-help jf/rpg-core-qh-difficulty-levels
  :label "Difficulty Levels (DL) (RMSS)"
  :header "CORE Difficulty Levels (RMSS)"
  :body
  (s-join
   "\n"
   '("1 .... Routine"
     "2 .... Easy"
     "3 .... Challenging"
     "4 .... Difficult"
     "5 .... Hard"
     "6 .... Very Hard"
     "7 .... Extermely Hard"
     "8 .... Sheer Folly"
     "9 .... Absurd"
     "10 ... Nigh Impossible")))

(jf/transient-quick-help jf/rpg-core-qh-standard-modifiers
  :label "Standard Modifiers"
  :header "CORE Standard Modifiers"
  :body
  (s-join
   "\n"
   '("+N .... Skill level and items"
     "+1 .... Superior position/advantage"
     "+1 .... Character development from /Flash of Insight/"
     "-1 .... Wounded for 2 or more"
     "-1 .... Within skill but not specialty"
     "-1 .... Pro kit required but makeshift tools"
     "-2 .... Pro kit required but no tools"
     "-2 .... Attempting 2 Actions at once")))

(jf/transient-quick-help jf/rpg-core-qh-hit-locations
  :label "Hit Locations"
  :header "CORE Hit Locations; +2 to attacks from above."
  :body
  (s-join
   "\n"
   '("2 .... Left Leg"
     "3 .... Right Leg"
     "4 .... Crotch/Abdoment"
     "5 .... Left Arm"
     "6 .... Right Arm"
     "7 .... Belly/Lower Back"
     "8 .... Left Shoulder"
     "9 .... Right Shoulder"
     "10 ... Chest/Upper Back"
     "11 ... Neck"
     "12 ... Head")))

(jf/transient-quick-help jf/rpg-core-qh-lifeshaping-events
  :label "LifeShaping Events"
  :header "CORE LifeShaping Events (DayTrippers)"
  :body
  (s-join
   "\n"
   '("Belief ......... What the PC Believes"
     "Concept ........ What Ideas the PC Has"
     "Duty ........... What the PC Is Obliged to Do"
     "Goal ........... What the PC Wants to Do"
     "History ........ What the PC Has Learned in Life"
     "Mission ........ What the PC’s Orders Are"
     "Problem ........ Stuff the PC Has Issues With"
     "Relationship ... People the PC Interacts With"
     "Thing .......... The PC’s Most Personal Possessions")))

(jf/transient-quick-help jf/rpg-core-qh-help
  :label "Help"
  :header "CORE Help (DayTrippers)"
  :body
  (s-join
   "\n"
   '("Miss 2 or more ... Help gets -1")))

;;;; Eberron
(jf/minor-mode-maker :title "Eberron"
                     :abbr "eb"
                     :hooks (list 'org-mode-hook 'markdown-mode-hook))

(jf/transient-quick-help jf/eberron-qh-dragonmarks
  :label "Dragonmarks"
  :header "Eberron Dragonmarks, Houses, and Stock"
  :body
  (s-join
   "\n"
   '("| Name             | House               | Stock                 |"
     "|------------------+---------------------+-----------------------|"
     "| Detection        | Medani              | half-elf              |"
     "| Finding          | Tharashk            | half-orc,  human      |"
     "| Handling         | Vadalis             | human                 |"
     "| Healing          | Jorasco             | halfling              |"
     "| Hospitality      | Ghallanda           | halfling              |"
     "| Making           | Cannith             | human                 |"
     "| Passage          | Orien               | human                 |"
     "| Scribing         | Sivis               | gnome                 |"
     "| Sentinel         | Deneith             | human                 |"
     "| Shadow           | Phiarlan & Thuranni | elf                   |"
     "| Storm            | Lyrandar            | half-elf              |"
     "| Warding          | Kundarak            | dwarf                 |"
     "| Death            | Vol                 | elf, currently lost   |"
     "| Aberrant         | Tarkanan            | any                   |")))

(jf/transient-quick-help jf/eberron-qh-planes
  :label "Planes"
  :header "Eberron Planes"
  :body
  (s-join
   "\n"
   '("Daanvi, the Perfect Order"
     "Dal Quor, the Region of Dreams"
     "Dolurrh, the Realm of the Dead"
     "Fernia, the Sea of Fire"
     "Irian, the Eternal Day"
     "Kythri, the Churning Chaos"
     "Lamannia, the Twilight Forest"
     "Mabar, the Endless Night"
     "Risia, the Plain of Ice"
     "Shavarath, the Battleground"
     "Syrania, the Azure Sky"
     "Thelanis, the Faerie Court"
     "Xoriat, the Realm of Madness")))

(jf/transient-quick-help jf/eberron-qh-religion
  :label "Religion"
  :header "Eberron Religion"
  :body
  (s-join
   "\n"
   '("| Pantheon     | Deity             | Domain            |"
     "|--------------+-------------------+-------------------|"
     "| Sovereign    | Arawai            | Life, Love        |"
     "| Sovereign    | Aureon            | Law, Lore         |"
     "| Sovereign    | Balinor           | Horn, Hunt        |"
     "| Sovereign    | Boldrei           | Hall, Hearth      |"
     "| Sovereign    | Dol Arrah         | Sun, Sacrifice    |"
     "| Sovereign    | Dol Dorn          | Strength, Steel   |"
     "| Sovereign    | Kol Korran        | World, Wealth     |"
     "| Sovereign    | Olladra           | Feast, Fortune    |"
     "| Sovereign    | Onatar            | Fire, Forge       |"
     "| Silver Flame | Silver Flame, the | Goodness, Law     |"
     "| Dark Six     | Devourer, the     | Wave, Whelm       |"
     "| Dark Six     | Fury, the         | Rage, Ruin        |"
     "| Dark Six     | Keeper, the       | Death, Decay      |"
     "| Dark Six     | Mockery, the      | Betray, Bloodshed |"
     "| Dark Six     | Shadow, the       | Magic, Mayhem     |"
     "| Dark Six     | Traveler, the     | Chaos, Change     |")))

(transient-define-prefix jf/menu--eberron ()
  "Define the Eberron help prefix."
  ["Eberron"
   ("d" jf/eberron-qh-dragonmarks)
   ("p" jf/eberron-qh-planes)
   ("r" jf/eberron-qh-religion)
   ])

;;;; The One Ring

(jf/minor-mode-maker :title "The One Ring"
		     :abbr "TOR")

(jf/transient-quick-help jf/gaming/the-one-ring/strider-mode/experience-milestones
  :label "Strider: Experience Milestones"
  :header "Strider: Experience Milestones"
  :body
  (s-join
   "\n"
   '("| Milestone                                    | Adventure Point | Skill Point |"
     "|----------------------------------------------+-----------------+-------------|"
     "| Accept a mission from a patron               |               1 | -           |"
     "| Adventure Point and 1 Skill                  |               1 | 1           |"
     "| Complete a patron’s mission                  |               1 | 1           |"
     "| Complete a meaningful journey                |               2 | -           |"
     "| Face a Noteworthy Encounter during a journey |               1 | -           |"
     "| Reveal a significant location or discovery   |               - | 1           |"
     "| Overcome a tricky obstacle                   |               1 | -           |"
     "| Participate in a Council                     |               1 | -           |"
     "| Survive a dangerous combat                   |               - | 1           |"
     "| Face a Revelation Episode                    |               - | 1           |")))

(defvar jf/gaming/the-one-ring/strider-mode/telling-table
  ;; List contains 12 elements; 0th index is "Sauron", then 1st through 10th are
  ;; 1 through 10 and 11th is "Gandalf".
  '(("Certain" . ("⏿ No with an extreme result or twist"
		  "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes"
		  "ᚠ Yes with an extreme result or twist"))
    ("Likely" . ("⏿ No with an extreme result or twist"
		 "No" "No" "No" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes" "Yes"
		 "ᚠ Yes with an extreme result or twist"))
    ("Middling" . ("⏿ No with an extreme result or twist"
		   "No" "No" "No" "No" "No" "Yes" "Yes" "Yes" "Yes" "Yes"
		   "ᚠ Yes with an extreme result or twist"))
    ("Doubtful" . ("⏿ No with an extreme result or twist"
		   "No" "No" "No" "No" "No" "No" "No" "Yes" "Yes" "Yes"
		   "ᚠ Yes with an extreme result or twist"))
    ("Unthinkable" . ("⏿ No with an extreme result or twist"
		      "No" "No" "No" "No" "No" "No" "No" "No" "No" "Yes"
		      "ᚠ Yes with an extreme result or twist")))
  "An translation of the Telling Table from Strider Mode.")

(defvar jf/gaming/the-one-ring/strider-mode/lore-table
  (list
   :action '("⏿ Abandon" "⏿ Attack" "⏿ Betray" "⏿ Corrupt" "⏿ Defeat" "⏿ Weaken" ;; Eye of Sauron
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
	     "ᚠ Believe" "ᚠ Bolster" "ᚠ Defend" "ᚠ Forgive" "ᚠ Resist" "ᚠ Strengthen") ;; Rune of Gandalf
   :aspect '("⏿ Corrupted" "⏿ Cruel" "⏿ Deceptive" "⏿ Fell" "⏿ Ruined" "⏿ Treacherous" ;; Eye of Sauron
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
	     "ᚠ Flourishing" "ᚠ Beautiful" "ᚠ Good" "ᚠ Kind" "ᚠ Gentle" "ᚠ Wondrous") ;; Rune of Gandalf
   :focus '("⏿ Curse" "⏿ Despair" "⏿ Enemy" "⏿ Fear" "⏿ Shadow" "⏿ War" ;; Eye of Sauron
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
	    "ᚠ Courage" "ᚠ Duty" "ᚠ Fellowship" "ᚠ Hope" "ᚠ Love" "ᚠ Peace") ;; Rune of Gandalf
   )
  "From Strider Mode p11-12")

(defconst jf/gaming/the-one-ring/fortune-tables
  (list (cons "ᚠ"
	 '("The Eye of the Enemy focuses elsewhere. Decrease Eye Awareness by 1."
	   "You may bypass a threat without attracting notice"
	   "You gain the attention of a potential ally"
	   "An enemy inadvertently reveals their position"
	   "You gain favoured ground"
	   "Enemies run afoul of danger"
	   "You locate or learn of a useful item"
	   "Your success instils new hope or renewed resolve"
	   "You find a moment of comfort or safety"
	   "You learn or realize something which gives helpful insight into your mission"
	   "You encounter an opportunity suited to your nature or abilities"
	   "An unexpected ally appears or sends aid"))
	(cons "⏿"
	 '("Your actions catch the Eye of the Enemy. Increase Eye Awareness by 2."
	   "You draw unwanted attention"
	   "Your actions are observed by someone of ill-intent"
	   "Unexpected enemies emerge or are sighted"
	   "You are hindered by difficult terrain or an unfa- vourable environment"
	   "You find yourself ill-equipped for the circumstances"
	   "A favoured weapon or item is lost, broken, or sacrificed"
	   "You are plagued by troubling visions or thoughts"
	   "An old injury or stress resurfaces"
	   "You learn or realize something which adds a new complication to your mission"
	   "You face a test which is contrary to your nature or abilities"
	   "An ally becomes a hindrance or liability"))))

(defconst jf/gaming/the-one-ring/revelation-episode-table
  '("Internal strife or an external threat puts your Safe Haven in peril"
    "Unexpected danger arises on the path ahead, forcing you to seek a new route"
    "Nature is corrupted and turns against you"
    "Spies of the Enemy carry word of your mission"
    "Enemy minions launch an ambush or lay a trap"
    "Enemy minions pick up your trail"
    "An important location is overtaken by an enemy"
    "An item you carry holds a curse, or is hunted by an enemy"
    "You are tempted by something greatly desired, to the detriment of your mission"
    "Malicious lies cause others to mistrust or fear you"
    "Conflict brews between allies"
    "An important ally is put in danger")
  "Roll a 1d12; by convention the 0th is Sauron.")

(defun jf/gaming/the-one-ring/roll/solo-event-table (favorability)
  (interactive (list (completing-read "Favourability: "
				      jf/gaming/the-one-ring/feat-die-favourability)))
  (let* ((subtable-name (jf/gaming/the-one-ring/roll/favorability-with-table
			 :favorability favorability
			 :table (plist-get jf/gaming/the-one-ring/event-table :table)))
	 (details (plist-get jf/gaming/the-one-ring/event-table :details))
	 (subtable (plist-get details subtable-name))
	 (subtable-events (seq-random-elt (plist-get subtable :events))))
    (format "%s: %s\n\n- Fatigue :: %s\n- Consequence :: %s\n- Task :: %s\n"
	     (plist-get subtable :title)
	     (car subtable-events)
	     (plist-get subtable :fatigue)
	     (plist-get subtable :consequence)
	     (cdr subtable-events))))

;; (message "%s" (seq-random-elt (plist-get jf/gaming/the-one-ring/event-table :table)))
(defconst jf/gaming/the-one-ring/event-table
  (list :table '(:terrible-misfortune :despair :ill-choices :ill-choices
		 :mishap :mishap :mishap :mishap
		 :short-cut :short-cut :chance-meeting :joyful-sight)
	:details
	(list :terrible-misfortune
	      (list :consequence "If the roll fails, the target is Wounded."
		    :title "Terrible Misfortune"
		    :fatigue 3
		    :events
		    '(("Dire confrontation" . "Noteworthy Encounter")
		      ("Rival Predator" . "HUNTING to avoid becoming the hunted")
		      ("Violent weather" . "EXPLORE to find shelter")
		      ("Hidden hazard" . "AWARENESS to avoid stumbling into danger")
		      ("Dangerous terrain" . "EXPLORE to find a safer route")
		      ("Stalking enemy" . "AWARENESS to spot the foul presence")))
	      :despair
	      (list :consequence "If the roll fails, gain 2 Shadow points (Dread)."
		    :title "Despair"
		    :fatigue 2
		    :events
		    '(("Servants of the Enemy" . "Noteworthy Encounter")
		      ("Torrential weather" . "EXPLORE to find the least exposed path")
		      ("Nightmarish presence" . "AWARENESS to sense the danger")
		      ("Fading vigour" . "HUNTING to gain sustenance")
		      ("Corrupted site" . "EXPLORE to find your way out")
		      ("Grisly scene or foreboding portent" . "AWARENESS to be forewarned")))
	      :mishap
	      (list :consequence "If the roll fails, add 1 day to the length of the journey, and gain 1 additional Fatigue."
		    :title "Mishap"
		    :fatigue 2
		    :events
		    '(("Sparse wildlife" . "HUNTING to forage what you can")
		      ("Lost direction" . "EXPLORE to find your way")
		      ("Obstructed path" . "AWARENESS to spot a way around")
		      ("Elusive quarry" . "HUNTING to track it down")
		      ("Rough terrain" . "EXPLORE to safely traverse")
		      ("Wandering enemies" . "AWARENESS to sense their coming")))
	      :ill-choices
	      (list :consequence "If the roll fails, gain 1 Shadow point (Dread)."
		    :title "Ill Choices"
		    :fatigue 2
		    :events
		    '(("Mismanaged provisions" . "HUNTING to replenish stores")
		      ("Wayward path" . "EXPLORE to retrace your steps")
		      ("Overlooked hazard" . "AWARENESS to escape safely")
		      ("Lost quarry" . "HUNTING to follow its tracks")
		      ("Disorienting environs" . "EXPLORE to find your way")
		      ("Haunting visions" . "AWARENESS to over- come darkness")))
	      :short-cut
	      (list :consequence "If the roll succeeds, reduce the length of the journey by 1 day."
		    :title "Short Cut"
		    :fatigue 1
		    :events
		    '(("Game trail" . "HUNTING to traverse the path")
		      ("Secluded path" . "EXPLORE to navigate the wilds")
		      ("Helpful tracks" . "AWARENESS to follow the tracks")
		      ("Animal guide" . "HUNTING to follow at a distance")
		      ("Favourable weather" . "EXPLORE to make the most of it")
		      ("Familiar waypoint" . "AWARENESS to recognize the landmark")))
	      :chance-meeting
	      (list :consequence "If the roll succeeds, no Fatigue is gained, and you may envision a favourable encounter."
		    :title "Chance Meeting"
		    :fatigue 1
		    :events
		    '(("Lone hunter" . "HUNTING to trade stories")
		      ("Fellow traveller" . "EXPLORE to learn about the path ahead")
		      ("Discreet watcher" . "AWARENESS to spot them")
		      ("Noble beast" . "HUNTING to commune")
		      ("Secluded encampment" . "EXPLORE to find your way off the beaten path")
		      ("Auspicious gathering" . "Noteworthy Encounter")))
	      :joyful-sight
	      (list :consequence "If the roll succeeds, regain 1 Hope."
		    :title "Joyful Sight"
		    :fatigue 0
		    :events
		    '(("Majestic creatures" . "HUNTING to observe without startling them")
		      ("Inspiring vista" . "EXPLORE to reach a vantage point")
		      ("Benevolent being" . "AWARENESS to sense their presence")
		      ("Abundant foraging" . "HUNTING to replenish your rations")
		      ("Ancient monument" . "AWARENESS to recognize its significance")
		      ("Peaceful sanctuary" . "Noteworthy Encounter"))))))

(defconst jf/gaming/the-one-ring/feat-die
  '("⏿"
    1 2 3 4 5 6 7 8 9 10
    "ᚠ"))

(defvar jf/gaming/runes
  '(("ᚠ" . "Gandalf Rune for the One Ring") ;; (Runic Letter Fehu Feoh Fe F) Gandalf rune
    ("Շ" . "Success Icon for the One Ring") ;; (Armenian Capital Letter Sha) Success Icon
    ("⏿" . "Eye of Sauron for the One Ring") ;; (Observer Eye Symbol) Sauron symbol
    ))

(cl-defun jf/gaming/the-one-ring/roll/feat-die (favorability)
  (interactive (list (completing-read "Favourability: "
				      jf/gaming/the-one-ring/feat-die-favourability)))
  (jf/gaming/the-one-ring/roll/favorability-with-table :favorability favorability
					    :table jf/gaming/the-one-ring/feat-die))

(cl-defun jf/gaming/the-one-ring/roll/favorability-with-table (&key favorability table)
  "Roll on the TABLE using the FAVORABILITY."
  (funcall (alist-get favorability
		      jf/gaming/the-one-ring/feat-die-favourability
		      nil
		      nil
		      #'string=)
	   table))

(cl-defun jf/gaming/the-one-ring/roll/skill-check (dice
						   favorability
						   &key
						   (is_weary
						    jf/gaming/the-one-ring/character-is-weary))
  "Roll the dice of DICE with the given FAVORABILITY for the feat die."
  (interactive (list
		(read-number "Number of D6s: ")
		(completing-read "Favourability: "
				 jf/gaming/the-one-ring/feat-die-favourability)))
  (let* ((feat-die (jf/gaming/the-one-ring/roll/feat-die favorability))
	 (success-dice (jf/gaming/the-one-ring/roll/success-dice :dice dice :is_weary is_weary))
	 (weary_message (if is_weary " with Weary condition" ""))
	 (prefix (format "%s %sd6%s [Feat: %s  Success: %s]"
			 favorability
			 dice
			 weary_message
			 feat-die
			 (plist-get success-dice :rolls))))
    (cond
     ((numberp feat-die)
      (format "%s: %s %sՇ "
	      prefix
	      (+ feat-die (plist-get success-dice :total))
	      (plist-get success-dice :sixes)))
     ((string= "⏿" feat-die)
      (format "%s: %s %s %sՇ"
	      prefix
	      feat-die
	      (plist-get success-dice :total)
	      (plist-get success-dice :sixes)))
     ((string= "ᚠ" feat-die)
      (format "%s: %s %sՇ"
	      prefix
	      feat-die
	      (plist-get success-dice :sixes))))))

(cl-defun jf/gaming/the-one-ring/roll/success-dice (&key dice (is_weary nil))
  "Roll a number of \"The One Ring\" success DICE.  And reject some results when character IS_WEARY."
  (let ((total 0)
	(sixes 0)
	(rolls (list))
	(roll 0))
    (while (> dice 0)
      (setq roll (1+ (random 6)))
      (push roll rolls)
      (when (or (not is_weary) (> roll 3)) (setq total (+ total roll)))
      (when (= 6 roll) (setq sixes (+ 1 sixes)))
      (setq dice (1- dice)))
    (list :total total :sixes sixes :rolls rolls)))

(defvar jf/gaming/the-one-ring/feat-die-favourability
  '(("Favoured" . (lambda (table)
		    (nth (max (random (length table))
			      (random (length table)))
			 table)))
    ("Neutral" . (lambda (table)
		   (seq-random-elt table)))
    ("Ill-Favoured" . (lambda (table)
			(nth (min (random (length table))
				  (random (length table)))
			     table)))))

(defun jf/gaming/the-one-ring/roll/lore-table (question)
  (interactive (list
		(read-string "Open-ended Question: ")))
  (concat "{{{i(Lore Table)}}}:\n"
	  "\n"
	  "- Question :: “" question "”\n"
	  "- Action :: " (seq-random-elt (plist-get jf/gaming/the-one-ring/strider-mode/lore-table :action)) "\n"
	  "- Aspect :: " (seq-random-elt (plist-get jf/gaming/the-one-ring/strider-mode/lore-table :aspect)) "\n"
	  "- Focus :: " (seq-random-elt (plist-get jf/gaming/the-one-ring/strider-mode/lore-table :focus)) "\n"))

(defun jf/gaming/the-one-ring/roll/telling-table (question likelihood)
  "Ask the QUESTION with the given LIKELIHOOD on the `jf/gaming/the-one-ring/strider-mode/telling-table'."
  (interactive (list
		(read-string "Yes/No Question: ")
		(completing-read "Likelihood of yes: " jf/gaming/the-one-ring/strider-mode/telling-table)))
  (concat "{{{i(Telling Table)}}}:\n"
	  "\n"
	  "- Question :: “" question "”\n"
	  "- Likelihood :: " likelihood "\n"
	  "- Answer :: “" (seq-random-elt (alist-get likelihood jf/gaming/the-one-ring/strider-mode/telling-table nil nil #'string=)) "”"
	  "\n"))

(cl-defmacro jf/gaming/the-one-ring/create-condition (&key condition)
  (let* ((var-sym (intern (concat "jf/gaming/the-one-ring/character-is-" condition)))
	 (var-docstring (concat "Is the current character is " condition "?"))
         (fun-sym (intern (concat "jf/gaming/the-one-ring/character-is-" condition "/set")))
	 (fun-docstring (concat "Toggle current character's \"" condition "\" condition status.")))
    `(progn
       (defvar ,var-sym
	 nil
	 ,var-docstring)
       (transient-define-suffix ,fun-sym ()
	 ,fun-docstring
	 :description '(lambda ()
			 (concat
			  (s-titleize ,condition) ": "
			  (propertize
			   (format "%s" (if ,var-sym "Yes" "No"))
			   'face 'transient-argument)))
	 (interactive)
	 (setq ,var-sym (not ,var-sym))))))

(jf/gaming/the-one-ring/create-condition :condition "miserable")
(jf/gaming/the-one-ring/create-condition :condition "weary")
(jf/gaming/the-one-ring/create-condition :condition "wounded")

(global-set-key (kbd "H-1") 'jf/gaming/the-one-ring/menu)
(transient-define-prefix jf/gaming/the-one-ring/menu ()
  ["The One Ring\n"
   ["Rolls"
    ("f" "Feat die…"
     (lambda ()
       (interactive)
       (insert
	(format "Feat Die: %s"
		(call-interactively 'jf/gaming/the-one-ring/roll/feat-die)))))
    ("j" "Journey event (Solo)"
     (lambda ()
       (interactive)
       (insert (call-interactively 'jf/gaming/the-one-ring/roll/solo-event-table))))
    ("l" "Lore table…"
     (lambda ()
       (interactive)
       (insert (call-interactively 'jf/gaming/the-one-ring/roll/lore-table))))
    ("r" "Revelation Episode…"
     (lambda ()
       (interactive)
       (insert (concat "Revelation Episode: "
		       (seq-random-elt jf/gaming/the-one-ring/revelation-episode-table)))))
    ("s" "Skill check…"
     (lambda ()
       (interactive)
       (insert (call-interactively 'jf/gaming/the-one-ring/roll/skill-check))))
    ("t" "Telling table…"
     (lambda ()
       (interactive)
       (insert (call-interactively 'jf/gaming/the-one-ring/roll/telling-table))))
    ]
   ["Conditions"
    ("-m" jf/gaming/the-one-ring/character-is-miserable/set :transient t)
    ("-w" jf/gaming/the-one-ring/character-is-weary/set :transient t)
    ("-W"  jf/gaming/the-one-ring/character-is-wounded/set :transient t)
    ]])

;; TODO: Consider how I might "lookup" my character sheet for Solo Play.
(provide 'jf-gaming)
;;; jf-gaming.el ends here
