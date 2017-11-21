;;; org --- Summary
;;; Commentary:
;;; Setup org mode

;;; Code:

(use-package org
  :ensure t
  :init
  ;; Set global todo list
  (progn
    ;; active Babel languages
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((R . t)
       (emacs-lisp . t)
       (shell . t)
       (lisp . t)
       (latex . t)
       (php . t)
       (js . t)
       (mongo . t)))
    (setq org-babel-lisp-eval-fn "sly-eval")
    ;; General Org Config
    (setq org-agenda-files (list "~/org/home.org")
	  org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELED"))
	  org-enforce-todo-dependencies t)))

(use-package elfeed
  :ensure t)

(use-package elfeed-org
  :ensure t
  :init
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/org/feeds.org")))

(provide 'org)
;;; org.el ends here
