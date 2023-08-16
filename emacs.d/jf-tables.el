;;; jf-tables --- Roll on some tables. -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package provides a means of registering random tables (see
;; `random-table' and `random-table/register') and then rolling on those tables
;; (see `random-table/roll').
;;
;; The `random-table/roll' is an `interactive' function that will prompt you to
;; select an expression.  You can choose from a list of registered public tables
;; or provide your own text.  This package uses the `s-format' to parse the
;; given expression.
;;
;; Examples:
;; - (random-table/roll "2d6") will roll 2 six-sided dice.
;; - (random-table/roll "There are ${2d6} orcs.") will roll 2 six-sided dice and
;;   output the sentence "There are 7 orcs."
;;
;; Tables can reference other tables, using the above string interpolation
;; (e.g. "Roll on ${Your Table}" where "Your Table" is the name of a registered
;; table.).  No considerations have been made to check for cyclical references,
;; you my dear human reader, must account for that.  An example is the
;; "Oracle Question (Black Sword Hack)" table.  It has one entry:
;;
;; "${Oracle Question > Answer (Black Sword Hack)}${Oracle Question > Unexpected Event (Black Sword Hack)}".
;;
;; When we roll "Oracle Question (Black Sword Hack)", we then roll on the two
;; sub-tables.  Which can also leverage the template interpolation.

;;; Code:

;;;; Requirements:
(require 'org-d20)
(require 'cl)

;;;; Data Structures and Storage
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
  (private nil)
  (store nil)
  (reuse nil))

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

(defun random-table/roller/default (&rest data)
  "Roll for the given DATA."
  ;; Constant off by one errors are likely
  (+ 1 (random (length (-list data)))))

(defun random-table/filter/default (&rest rolls)
  "Filter the given ROLLS.

See `random-table/roller/default'."
  (apply #'+ (-list rolls)))

(defun random-table/fetcher/default (data &optional position)
  "Find POSITION on the given DATA.

When POSITION is not given, choose a random element from the TABLE."
  (if-let ((index (if (integerp position) position (car position))))
    (if (-cons-pair? (car data))
      (cdr (seq-find (lambda (row) (member index (car row))) data))
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
    (list text (random-table/roll/text text))))

;; TODO Rename this; I'm not satisfied and want to refactor.
(defun random-table/roll/text (text)
  "Roll the given TEXT; either by evaluating as a `random-table' or via `s-format'."
  (if-let* ((table (random-table/get-table text :allow_nil t)))
    (random-table/evaluate/table table)
    ;; We have specified a non-table; roll the text.  We'll treat a non-escaped on as a dice text.
    (progn
      (s-format (if (string-match-p "\\${" text) text (concat "${" text "}"))
        #'random-table/roll/text/replacer))))

(defun random-table/roll/text/replacer (text)
  "Roll the TEXT; either from a table or as a dice-expression.

This is constructed as the replacer function of `s-format'."
  (if-let ((table (random-table/get-table text :allow_nil t)))
    (random-table/evaluate/table table)
    ;; Ensure that we have a dice expression
    (if (string-match-p "[0-9]?d[0-9]" text)
      (format "%s" (cdr (org-d20--roll text)))
      text)))

(defun random-table/evaluate/table (table)
  "Evaluate the random TABLE.

See `random-table'.  "
  (let* ((rolled (random-table/evaluate/table/roll table))
          (name (random-table-name table))
          (data (random-table-data table))
          (filtered (apply (random-table-filter table) (-list rolled)))
          (entry (if filtered (apply (random-table-fetcher table) (list data (-list filtered)))
                   nil))
          (results (or (when entry (random-table/roll/text entry)) "")))
    (remhash (random-table-name table) random-table/storage/results)
    results))

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
            (apply (random-table-roller table) (-list (random-table-data table))))))
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
                      value)
                    (t
                      (error "Expected %s to be a `random-table', `symbol', `integer', or `string' got %s."
                        value
                        (type-of value))))))
    table
    (unless allow_nil
      (error "Could not find table %s; use `random-table/register'." value))))

;;; Register Tables
(cl-defun random-table/register (&rest kws &key name data &allow-other-keys)
  "Store the DATA, NAME, and KWS in a `random-table'."
  (let* ((key (intern name))
          (struct (apply #'make-random-table :name key :data (-list data) kws)))
    (puthash key struct random-table/storage/tables)))

(provide 'jf-tables)
;;; jf-tables.el ends here
