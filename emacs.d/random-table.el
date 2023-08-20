;;; random-table --- Roll on some tables. -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; A blog post on this package: https://takeonrules.com/2023/08/17/emacs-proto-package-for-random-tables/

;; This package provides a means of registering random tables (see
;; `random-table' and `random-table/register') and then rolling on those tables
;; (see the `random-table/roll').
;;
;; The `random-table/roll' is an `interactive' function that will prompt you to
;; select an expression.  You can choose from a list of registered public tables
;; or provide your own text.  This package uses the `s-format' to parse the
;; given expression.
;;
;; The guts of the logic is `random-table/evaluate/table' namely how we:
;;
;; - Gather the dice (represented by the :roller)
;; - Filter the dice to a value (represented by the :filter), we might pick a
;;          single dice rolled or sum them or whatever.
;; - Fetch the filtered result from the table.
;; - Evaluate the row.
;;
;; Examples:
;; - (random-table/roll "2d6") will roll 2 six-sided dice.
;; - (random-table/roll "There are ${2d6} orcs.") will roll 2 six-sided dice and
;;   output the sentence "There are 7 orcs."
;;
;; Tables can reference other tables, using the above string interpolation
;; (e.g. "Roll on ${Your Table}" where "Your Table" is the name of a registered
;; table.).  No considerations have been made to check for cyclical references,
;; you my dear human reader, must account for that.

;;; Code:

;;;; Requirements:
(require 'org-d20)
(require 'cl)

;;;; Data Structures and Storage and Defaults
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
- :exclude-from-prompt :: when true, ignore the prefix arg for
  prompting for dice roll.
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
  (exclude-from-prompt nil)
  (private nil)
  (store nil)
  (reuse nil))

(cl-defun random-table/register (&rest kws &key name data exclude-from-prompt &allow-other-keys)
  "Store the DATA, NAME, and all given KWS in a `random-table'."
  ;; We need to guard for a reserved character; which we use for operations.
  (if (string-match-p "\\(\\[\\|\\]\\)" name)
    (user-error "Attempt to register \"%s\" table failed.  You cannot include the following characters: \"[\", \"]\"." name)
    (let* ((key (intern name))
            (struct (apply #'make-random-table
                      :name key
                      :exclude-from-prompt (or exclude-from-prompt
                                             (= 1 (length (-list data))))
                      :data (-list data) kws)))
      (puthash key struct random-table/storage/tables))))

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

;;; Random Table Roller
(cl-defmacro random-table/roller (&rest body &key label &allow-other-keys)
  (let ((roller (intern (concat "random-table/roller/" label)))
         (docstring (format "Roll %s on given TABLE" label)))
    `(defun ,roller (table)
       ,docstring
       (if (and current-prefix-arg (not (random-table-exclude-from-prompt table)))
         (read-number (format "Roll %s for %s: " ,label (random-table-name table)))
         ,@body))))

(defun random-table/roller/default (table)
  "Given the TABLE roll randomly on it.
See `random-table/filter/default'.
See `random-table/roller' macro."
  ;; Constant off by one errors are likely
  (let ((faces (length (-list (random-table-data table)))))
    (if (and current-prefix-arg (not (random-table-exclude-from-prompt table)))
      (read-number (format "Roll 1d%s for %s: " faces (random-table-name table)))
      (+ 1 (random faces)))))

;; Perhaps not ideal to have one function per roll type.  But...having a
;; consistent interface.
(random-table/roller :label "1d6" (+ 1 (random 6)))
(random-table/roller :label "2d6" (+ 2 (random 6) (random 6)))
(random-table/roller :label "1d8" (+ 1 (random 8)))
(random-table/roller :label "1d10" (+ 1 (random 10)))
(random-table/roller :label "1d12" (+ 1 (random 12)))
(random-table/roller :label "1d20" (+ 1 (random 20)))

(defun random-table/filter/default (&rest rolls)
  "Filter the given ROLLS and return an integer.

See `random-table/roller/default'."
  (cond
    ;; Allows us to have table entries that are named.
    ((stringp (car rolls)) (car rolls))
    (t (apply #'+ (-list rolls)))))

(defun random-table/fetcher/default (data &optional roll)
  "Find ROLL on the given table's DATA.

When ROLL is not given, choose a random element from the TABLE."
  (if-let ((index (if (integerp roll) roll (car roll))))
    ;; Sniff out if the first element to see if we're dealing with a table that has ranges.
    (if (-cons-pair? (car data))
      ;; We have a cons-pair, meaning we have multiple rolls mapping to the same
      ;; result.
      (cdr (seq-find
             (lambda (row)
               (if (-cons-pair? row)
                 (let ((range (car row)))
                   (cond
                     ((-cons-pair? range)
                       (and (>= index (car range)) (<= index (cdr range))))
                     ((listp range)
                       (member index range))
                     ((integerp range)
                       (= index range))
                     ((stringp range)
                       (string= index range))
                     (t
                       (error "Expected `cons', `list', `string' or `integer' got %s for row %S." (type-of range) row))))
                 (member index (car row))))
             data))
      ;; Off by one errors are so very real.
      (nth (- index 1) data))
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
(global-set-key (kbd "H-r") #'random-table/roll)
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
    (list text (random-table/roll/parse-text text))))

(defun random-table/roll/parse-text (text)
  "Roll the given TEXT.

Either by evaluating as a `random-table' or via `s-format'."
  (if-let* ((table (random-table/get-table text :allow_nil t)))
    (random-table/evaluate/table table)
    ;; We have specified a non-table; roll the text.  We'll treat a non-escaped on as a dice text.
    (progn
      (let ((text (format "%s" text)))
        (s-format (if (string-match-p "\\${" text) (format "%s" text) (format "${%s}" text))
          #'random-table/roll/parse-text/replacer)))))

(defun random-table/roll/parse-text/replacer (text)
  "Roll the TEXT; either from a table or as a dice-expression.

This is constructed as the replacer function of `s-format'."
  (if-let ((table (random-table/get-table text :allow_nil t)))
    (random-table/evaluate/table table)
    (cond
      ((string-match "\\[\\(.*\\)\\][[:space:]]*\\(-\\|\\+\\|\\*\\)[[:space:]]*\\[\\(.*\\)\\]" text)
        (let* ((table-one (match-string-no-properties 1 text))
               (operator (match-string-no-properties 2 text))
                (table-two (match-string-no-properties 3 text)))
          (apply (intern operator)
            (list
              (string-to-number (random-table/roll/parse-text/replacer table-one))
              (string-to-number (random-table/roll/parse-text/replacer table-two))))))
      ((and random-table/current-roll (string-match "current_roll" text))
        random-table/current-roll)
      (t
        ;; Ensure that we have a dice expression
        (if (string-match-p "[0-9]?d[0-9]" text)
          (format "%s" (cdr (org-d20--roll text)))
          text)))))

(defun random-table/evaluate/table (table)
  "Evaluate the random TABLE.

See `random-table'."
  (let* ((data (random-table-data table))
          (name (random-table-name table))
          (rolled (random-table/evaluate/table/roll table)))
    (setq random-table/current-roll rolled)
    (let* ((filtered (apply (random-table-filter table) (-list rolled)))
            (row (if filtered (apply (random-table-fetcher table) (list data (-list filtered)))
                   nil))
            (results (or (when row (random-table/roll/parse-text row)) "")))
      (remhash (random-table-name table) random-table/storage/results)
      (setq random-table/current-roll nil)
      results)))

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
            (apply (random-table-roller table) (-list table)))))
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
                    ((integerp value)
                      nil)
                    (t
                      (error "Expected %s to be a `random-table', `symbol', `integer', or `string' got %s."
                        value
                        (type-of value))))))
    table
    (unless allow_nil
      (error "Could not find table %s; use `random-table/register'." value))))

;;; Probationary methods
;; TODO Work on removing the org-d20 dependency
(defun random-table/dice/parse-spec (spec)
  "Convert SPEC to list:

   - Number of dice
   - Face
   - Adder

  e.g. \"1d6\" -> (1 6 0) or \"2d10+2\" -> (2 10 2) or \"4dF\" -> (4 \"f\" 0)"
  (when (string-match
         "^\\([1-9][0-9]*\\)d\\([0-9a-zA-Z]*\\)\\([+-][0-9]*\\)?"
         spec)
    (list (random-table/dice/string-to-number (match-string 1 spec) 1)
          (random-table/dice/string-to-number (match-string 2 spec)
                                   decide-default-dice-facespec)
          (random-table/dice/string-to-number (match-string 3 spec) 0))))

(defun random-table/dice/string-to-number (spec default)
  (let ((n (if (stringp spec)
               (string-to-number spec)
             0)))
    (cond ((null spec) default)
          ((> n 0) n)
          ((string= "+" spec) 0)
          ((string= "-" spec) 0)
      (t spec))))

;; (let ((spec (random-table/dice/parse-spec spec-string)))
;;   (apply 'decide-roll-dice-spec (if spec spec '(1 6 0))))

(provide 'random-table)
;;; random-table.el ends here
