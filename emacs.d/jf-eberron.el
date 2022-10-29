;;; jf-eberron.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary

;;; Code
(require 'jf-minor-mode-maker)
(require 'jf-quick-help)

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
(provide 'jf-eberron)
;;; jf-eberron.el ends here
