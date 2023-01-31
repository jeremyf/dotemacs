;;; jf-experiments --- Where I put things that I'm exploring -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary

;;; Code

(use-package run-command
  :straight t
  :config
  (defun jf/run-command-recipes ()
    "Run command recipes"
    (list
     (let ((dir (projectile-project-root)))
       (when (f-exists? (f-join (projectile-project-root) "Gemfile.lock"))
	 (list :command-name "run-command-samvera-versions"
	       :command-line (format "cd %s; rg \"^ +((bulk|hy)rax|rails|qa|blacklight(-spotlight)?) \\(\\d+\\.\\d+\\.\\d+\" Gemfile.lock" dir)
	       :display (format "Samvera gem versions for %s" dir))))
     (list :command-name "run-command-takeonrules-server"
	   :command-line "cd ~/git/takeonrules.source/; bin/rake knowledge_manager:pull; hugo -D"
	   :display "Serve takeonrules.com locally")))
  (add-to-list 'run-command-recipes 'jf/run-command-recipes))

(provide 'jf-experiments)
;;; jf-experiments.el ends here
