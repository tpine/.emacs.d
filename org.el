;;; org --- Summary
;;; Commentary:
;;; Setup org mode

;;; Code:

(use-package org
  :ensure t
  :config
  ;; Set global todo list
  (setq org-agenda-files (list "~/org/home.org")
	org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELED"))
	org-enforce-todo-dependencies t))

(use-package elfeed
  :ensure t)

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/org/feeds.org")))

(provide 'org)
;;; org.el ends here
