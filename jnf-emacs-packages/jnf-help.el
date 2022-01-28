;;; Commentary:
;;
;; This file provides both a macro for generating quick-help and help buffers.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro quick-help (name buffer text)
  "Macro for creating callable functions that display help.
Where NAME is name of function, BUFFER is name of buffer, and TEXT is displayed."
  (declare (indent defun))
  `(progn
     (defun ,name nil
       ,buffer
       (interactive)
       (let ((qh-buff (concat "*Quick Help: " ,buffer "*"))
             (qh-text ,text))
         (get-buffer-create qh-buff)
         (with-current-buffer qh-buff
           (insert qh-text)
           (goto-char (point-min))
           (not-modified)
           (read-only-mode)
           (special-mode)
           ;; (local-set-key (kbd "C-g") (lambda () (interactive) (other-window -1)))
           (local-set-key (kbd "q") 'kill-buffer-and-window))
         (pop-to-buffer qh-buff '((display-buffer-below-selected)
                                  (window-parameters . ((no-other-window . nil)))
                                  (window-height . fit-window-to-buffer)))
         (message "q - Remove Window")))))


(cl-defmacro transient-quick-help (name buffer &key label body mode)
  "Macro for creating callable functions that display help.
Where NAME is name of function, BUFFER is name of buffer, and TEXT is displayed."
  (declare (indent defun))
  `(progn
     (transient-define-suffix ,name nil
       ,buffer
       :if-non-nil ,mode
       :description ,label
       (interactive)
       (let ((qh-buff (concat "*Quick Help: " ,buffer "*"))
             (qh-text ,body))
         (get-buffer-create qh-buff)
         (with-current-buffer qh-buff
           (insert qh-text)
           (goto-char (point-min))
           (not-modified)
           (read-only-mode)
           (special-mode)
           ;; (local-set-key (kbd "C-g") (lambda () (interactive) (other-window -1)))
           (local-set-key (kbd "q") 'kill-buffer-and-window))
         (pop-to-buffer qh-buff '((display-buffer-below-selected)
                                  (window-parameters . ((no-other-window . nil)))
                                  (window-height . fit-window-to-buffer)))
         (message "q - Remove Window")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; BEGIN Burning Wheel Gold Tables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(transient-quick-help jnf/qh--bwg-wises
  "BWG Wises Obstacles (page 309)"
  :label "Wises"
  :mode 'jnf-bwg-minor-mode
  :body
  (concat
   "Common knowledge ............. Ob 1\n"
   "An interesting fact .......... Ob 2\n"
   "Details ...................... Ob 3\n"
   "Uncommon knowledge ........... Ob 4\n"
   "Rare goods ................... Ob 5\n"
   "Bizarre or obscure ........... Ob 7\n"
   "Freaky details or specifics .. Ob 8"))

(transient-quick-help jnf/qh--bwg-expertise-exponent
  "BWG Expertise Exponent (page 12)"
  :label "Exponents"
  :mode 'jnf-bwg-minor-mode
  :body
  (concat
   "Exp 1  is naturally disinclined, crippled, or utterly incompetent.\n"
   "Exp 2  is untrained, raw, weak, or unpracticed.\n"
   "Exp 3  is nominally trained and practiced.\n"
   "Exp 4  is competent; everday stuff doesn't pose a challenge.\n"
   "Exp 5  is expert.\n"
   "Exp 6  is near mastery.\n"
   "Exp 7  is excellence.\n"
   "Exp 8  is total mastery, complete understanding.\n"
   "Exp 9  is uncanny; incomprehensibly good.\n"
   "Exp 10 is as near perfection as the system allows."))

(transient-quick-help jnf/qh--bwg-absolute-difficulty
  "BWG Absolute Difficulty (page 15)"
  :label "Difficulty"
  :mode 'jnf-bwg-minor-mode
  :body
  (concat
   "Ob 1  A simple act done with little thought.\n"
   "Ob 2  An act performed routinely at your job.\n"
   "Ob 3  An act you can accomplish if you concentrate.\n"
   "Ob 4  A risky act.\n"
   "Ob 5  An act that requires expertise.\n"
   "Ob 6  An act that requires a heroic effort.\n"
   "Ob 7  An improbable feat.\n"
   "Ob 8  An act requiring preternatural ability or a lot of help.\n"
   "Ob 9  An act deemed nearly impossible.\n"
   "Ob 10 A miracle."))

(transient-quick-help jnf/qh--bwg-circles-obstacles
  "BWG Circles Obstacles (page 380-381)"
  :label "Circles"
  :mode 'jnf-bwg-minor-mode
  :body
  (concat
   "Occupation\n"
   "  Broad occupation/profession, same life path ... +0 Ob\n"
   "  Uncommon occupation, or within same setting ... +2 Ob\n"
   "  Specific occupation, rare/unique occupation ... +3 Ob\n"
   "\n"
   "Station\n"
   "  Same station .................................. +0 Ob\n"
   "  Lower rank, station, or class ................. +1 Ob\n"
   "  Higher rank, station, or class ................ +2 Ob\n"
   "  Highest station or rank in the setting ........ +3 Ob\n"
   "\n"
   "Disposition and Knowledge\n"
   "  Common to circle .............................. +0 Ob\n"
   "  Different from circle members ................. +1-2 Ob\n"
   "  Specific, detailed, or rare ................... +3 Ob\n"
   "\n"
   "Time and Place\n"
   "  Doesn't matter ................................ +0 Ob\n"
   "  Unusual for this character .................... +1-2 Ob\n"
   "  Right here and now in the middle of trouble ... +3 Ob"))

(transient-quick-help jnf/qh--bwg-steel-test-adjustments
  "BWG Steel Test Adjustments (page 363)"
  :label "Steel"
  :mode 'jnf-bwg-minor-mode
  :body
  (concat
   "Conditions for Steel Advantags\n"
   "  Being startled by something mundane ........ +2D\n"
   "  Feeling safe in a group of friends/allies .. +1D\n"
   "\n"
   "Conditions for Steel Disadvantages\n"
   "  Being shot at .............................. +1 Ob\n"
   "  Being directly affect by magic ............. +1 Ob\n"
   "  Witnessing a person killed ................. +1 Ob\n"
   "  Small explosions ........................... +2 Ob\n"
   "  Committing murder .......................... +2 Ob\n"
   "  Explosions ................................. +3 Ob\n"
   "  Witnessing pronounced sorcery at play ...... +3 Ob\n"
   "  Seeing a ghost ............................. +3 Ob\n"
   "  Seeing the living dead ..................... +4 Ob\n"
   "  Volcanic eruptions, cataclysm .............. +4 Ob\n"
   "  Seeing horrible magic at work .............. +4 Ob\n"
   "  Being in the presence of the supernatural .. +5 Ob\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; END Burning Wheel Gold Tables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
