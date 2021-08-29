;;; jnf-hydra.el --- Summary -*- lexical-binding: t; -*-

(use-package pretty-hydra
  :straight (pretty-hydra
             :type git :host github :repo "jerrypnz/major-mode-hydra.el"
             :files (:defaults (:exclude "major-mode-hydra.el"))))

(provide 'jnf-hydra.el)
;;; jnf-hydra.el ends here
