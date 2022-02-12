(require 'cl-lib)

(transient-define-suffix pmx-show-prefix ()
  "Show the prefix that invoked this suffix"
  :description "prefix"
  (interactive)
  (message "Current prefix key: %s" transient-current-prefix))

(transient-define-suffix pmx-show-command ()
  "Show this command"
  :description "current command"
  (interactive)
  (message "Current command: %s" transient-current-command))

(transient-define-suffix pmx-show-suffixes ()
  "Show the current suffixes"
  :description "suffixes"
  (interactive)
  (message "Current suffixes: %s" (cl-mapcar
                                   (lambda (obj)
                                     (oref obj description))
                                   transient-current-suffixes)))

(transient-define-suffix pmx-show-args ()
  "Show current infix args"
  :description "infix args"
  (interactive)
  (message "Current infix args: %s" (transient-args transient-current-command)))

(transient-define-suffix pmx-send-message ()
  "Send message to minibuffer"
  :description "send message"
  :transient t
  (interactive)
  (message "Message sent at %s. Happy?" (shell-command-to-string "echo -n $(date)")))

(transient-define-argument pmx-affirmative ()
  "Are we affirmative?"
  :description "affirmative"
  :argument "affirmative")

(transient-define-argument pmx-yep-nope ()
  "Is it yep or is it nope?"
  :description "yep or nope"
  :class 'transient-option
  :shortarg "-y"
  :argument "--yepnope="
  :choices '("yep" "nope"))

(transient-define-argument pmx-abc ()
  "Which letters do you like?"
  :description "abc"
  :class 'transient-option
  :shortarg "-a"
  :argument "--abc="
  :choices '("A" "B" "C"))

(defvar pmx--variable "A string" "A variable brought to you by pmx")

(transient-define-argument pmx-set-lisp-variable ()
  "Set a lisp variable, pmx--variable.  Won't show up in infix arguments."
  :description "set pmx--variable"
  :class 'transient-lisp-variable
  :shortarg "-l"
  :variable 'pmx--variable
  :argument "--letters=")

(transient-define-suffix pmx-show-lisp-variable ()
  "Access pmx--variable"
  :description "show pmx--variable"
  (interactive)
  (message "Current value of pmx--variable: %s" pmx--variable))

(transient-define-suffix pmx-dynamic-suffix ()
  "Description depends on pmx--variable"
  :if-not '(lambda () (string-equal pmx--variable "abc"))
  :description '(lambda () (format "pmx %s" pmx--variable))
  (interactive)
  (message "Current value of pmx--variable: %s" pmx--variable))

(transient-define-prefix pmx-nested-transient ()
  "Some subcommands, like tree menus from the land of mice"
  ["Switches"
   ("-s" "another switch" ("-x" "--conflicting"))]
  ["Sub Command Introspection"
   ("i" pmx-show-args)
   ("p" pmx-show-prefix)
   ("s" pmx-show-suffixes)
   ("c" pmx-show-command)]
  ["Dynamic Commands"
   ("d" pmx-dynamic-suffix)])

(transient-define-prefix pmx-transient-toy ()
  "Figure out how to use transient's API properly"
  [:class transient-columns
	  ["Things"
           ("-w" "switch"  ("-w" "--switch"))]
	  ["Others"
           ("i" pmx-show-args)
           ("p" pmx-show-prefix)
           ("s" pmx-show-suffixes)
           ("c" pmx-show-command)
           ("m" pmx-send-message)]
	  ["More"
           ("f" pmx-affirmative)
           ("y" pmx-yep-nope)
           ("a" pmx-abc)
           ("l" pmx-set-lisp-variable)
           ("w" pmx-show-lisp-variable)]
	  ["Drilldown"
           ("d" "drilldown" pmx-nested-transient)]])

(global-set-key (kbd "M-o") 'pmx-transient-toy)

;;;

;; need these loaded
(use-package notifications :straight t)
(require 'transient)

;; We're going to construct a sentence with a transient.  This is where it's stored.
(defvar ikaruga--toy-sentence "let's transient!"
  "Sentence under construction.")

;; First we define a suffix with a dynamic description.  This allows us to
;; display the current value.  (the transient API could use some more options to
;; display arbitrary values without making a suffix)
;;
;; The interactive form returns a single element list, (SENTENCE) which is then
;; passed into this command.
;;
;; Use transient faces with `propertize' to make your prompts match the feel of
;; other transient behaviors such as switches.
(transient-define-suffix ikaruga-sentence (sentence)
  "Set the sentence from minibuffer read"
  :transient t
  :description '(lambda ()
                  (concat
                   "set sentence: "
                   (propertize
                    (format "%s" ikaruga--toy-sentence)
                    'face 'transient-argument)))
  (interactive (list (read-string "Sentence: " ikaruga--toy-sentence)))
  (setf ikaruga--toy-sentence sentence))

;; Next we define some update commands.  We don't want these commands to dismiss
;; the transient, so we set their `:transient' slot to t for `transient--do-stay'.
;; https://github.com/magit/transient/blob/master/docs/transient.org#transient-state
(transient-define-suffix ikaruga-append-dot ()
  "Append a dot to current sentence"
  :description "append dot"
  :transient t ; true equates to `transient--do-stay'
  (interactive)
  (setf ikaruga--toy-sentence (concat ikaruga--toy-sentence "•")))

(transient-define-suffix ikaruga-append-snowman ()
  "Append a snowman to current sentence"
  :description "append snowman"
  :transient t
  (interactive)
  (setf ikaruga--toy-sentence (concat ikaruga--toy-sentence "☃")))

(transient-define-suffix ikaruga-clear ()
  "Clear current sentence"
  :description "clear"
  :transient t
  (interactive)
  (setf ikaruga--toy-sentence ""))

;; Now we want to consume our sentence.  These commands are the terminal verbs
;; of our sentence construction, so they use the default `transient-do-exit'
;; behavior.
(transient-define-suffix ikaruga-message ()
  "Send the constructed sentence in a message"
  :description "show sentence"
					; :transient nil ; nil is default, `transient--do-exit' behavior
  (interactive)
  (message "constructed sentence: %s" (propertize ikaruga--toy-sentence 'face 'transient-argument))
  (setf ikaruga--toy-sentence ""))

(transient-define-suffix ikaruga-notify ()
  "Notify with constructed sentence"
  :description "notify sentence"
  (interactive)
  (notifications-notify :title "Constructed Sentence:" :body
                        ikaruga--toy-sentence)
  (setf ikaruga--toy-sentence ""))

;; To bind all of our transient commands into a full transient (a "prefix"), we
;; just need group names and key-command pairs.  To put the input sentence onto
;; its own line, we separate the next two groups into their own vector.  You can
;; set the classname key to `transient-columns' or `transient-row' etc for more
;; specific arrangements.
(transient-define-prefix ikaruga-sentence-toy ()
  "Create a sentence with several objects and a verb"
  ["Sentence Toy!"
   ("SPC" ikaruga-sentence)]
  [["Transient Suffixes"
    ("d" ikaruga-append-dot)
    ("s" ikaruga-append-snowman)
    "" ; empty string inserts a gap, visually separating the appends from the clear
    ("c" ikaruga-clear)]
   ["Non-Transient Suffixes"
    ("m" ikaruga-message)
    ("n" ikaruga-notify)]])

(global-set-key (kbd "M-p") 'ikaruga-sentence-toy)