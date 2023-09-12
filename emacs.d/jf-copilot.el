;;; jf-copilot --- A wrapper for Copilot -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jeremy Friesen
;; Author: Jeremy Friesen <jeremy@jeremyfriesen.com>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;; Based on https://github.com/rksm/copilot-emacsd/blob/master/init.el

;;; Code:


;; https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/#tab-key
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :commands (copilot-login copilot-mode)
  :bind (:map copilot-mode-map (("C-M-<down>" .  copilot-next-completion)
                                 ("C-M-<up>" . copilot-previous-completion)
                                 ("C-M-<right>" . copilot-accept-completion-by-word)))
  :config   ;; Do copilot-quit when pressing C-g
  ;; (advice-add 'keyboard-quit :before #'jf/copilot-quit)

  ;; complete by pressing right or tab but only when copilot completions are
  ;; shown. This means we leave the normal functionality intact.
  ;; (advice-add 'right-char :around #'jf/copilot-complete-if-active)
  ;; (advice-add 'indent-for-tab-command :around #'jf/copilot-complete-if-active)

  ;; deactivate copilot for certain modes
  ;; (add-to-list 'copilot-enable-predicates #'jf/copilot-enable-predicate)
  ;; (add-to-list 'copilot-disable-predicates #'jf/copilot-disable-predicate)
  :ensure t)

(provide 'jf-copilot)
;;; jf-copilot.el ends here
