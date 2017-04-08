;;; org --- Summary
;;; Commentary:
;;; Setup org mode

;;; Code:

(use-package org
  :ensure t
  :config
  ;; Set global todo list
  (setq org-agenda-files (list "~/org/school.org"
			       "~/org/home.org")
	org-enforce-todo-dependencies t))

(provide 'org)
;;; org.el ends here
