;;; -*- lexical-binding: t; -*-
;;; package --- Summary
;;
;;; Commentary:
;;
;;  This package provides a modified latex class
;;
;;; Code:

;; From http://pragmaticemacs.com/emacs/org-mode-basics-v-exporting-your-notes/
;;
(add-to-list 'org-latex-classes
             '("jnfarticle"
               "\\documentclass{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{hyperref}
\\usepackage{natbib}
\\usepackage{amssymb}
\\usepackage{amsmath}
\\usepackage{geometry}
\\geometry{letter,left=2.5cm,top=2cm,right=2.5cm,bottom=2cm,marginparsep=7pt, marginparwidth=.6in}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;; (setq org-latex-default-class "jnfarticle")
(provide 'jnf-org-latex.el)
;;; jnf-org-latex.el ends here
