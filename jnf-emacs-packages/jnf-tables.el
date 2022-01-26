(defvar jnf/table-registry (list)
  "A registry of random tables.  Each table is a list of lists.

The inner list has 3 elements:

- beginning of range
- ending of range
- result")

(defun jnf/table-print--format-row (row)
  (let ((first (car row))
	(second (cadr row))
	(third (caddr row)))
    (if (equal first second)
	(format "| %s | %s |" first third)
      (format "| %s–%s | %s |" first second third))))

(cl-defun jnf/table-print (&key name (registry jnf/table-registry))
  "Print

The goal is to inject this into the `quick-help' text "
  (let* ((container (plist-get registry name))
	 (caption (plist-get container :caption))
	 (label (plist-get container :label))
	 (dice (plist-get container :dice))
	 (table (plist-get container :table)))
    (message "%s"
	     (concat caption "\n\n" "| " dice " | " label " |\n|---+---|\n"
		     (mapconcat 'jnf/table-print--format-row table "\n")))))

(cl-defun jnf/table-register (&key name table label dice caption)
  "Add the TABLE with NAME to registry with CAPTION, DICE and LABEL."
  (setq jnf/table-registry
	(plist-put jnf/table-registry name
		   (list :caption caption :dice dice :label label :table table))))

(cl-defun jnf/table-lookup (integer &key table (registry jnf/table-registry))
  "Lookup INTEGER in TABLE.

Return the result of the first match."
  (caddr
   (seq-find
    (lambda (x)
      (memq integer (number-sequence (car x) (cadr x))))
    (if (listp table) table (plist-get (plist-get registry table) :table)))))

(jnf/table-register :name 'swn-ability-modifier
		    :caption "SWN Ability Modifier"
		    :dice "3d6" :label "Modifier"
		    :table '((3 3 -2)
			     (4 7 -1)
			     (8 13 0)
			     (14 17 1)
			     (18 18 2)))
(jnf/table-register :name 'wartime-village-physical-condition
		    :caption "Wartime Village: Physical Condition"
		    :dice "1d6" :label "Result"
		    :table '((1 1 "Ruin")
			     (2 2 "Partial Ruins")
			     (3 3 "No Defenses")
			     (4 4 "Manor House")
			     (5 5 "Light Village-wide Fortification")
			     (6 6 "Heavy Village-wide Fortification")))

(jnf/table-register :name 'wartime-village-mental-condition
		    :caption "Wartime Village: Mental Condition"
		    :dice "1d6" :label "Result"
		    :table '((1 1 "Sympathetic to the enemy")
			     (2 2 "Preparing to evacuate")
			     (3 3 "Paranoid")
			     (4 4 "Indifferent")
			     (5 5 "Inviting")
			     (6 6 "Defiant")))

(message "%s" (jnf/table-lookup 3 :table 'swn-ability-modifier))
(message "%s" (jnf/table-lookup 3 :table 'wartime-village-physical-condition))
(jnf/table-print :name 'swn-ability-modifier)