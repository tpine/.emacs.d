;;; org --- Summary
;;; Commentary:
;;; Setup org mode

;;; Code:

(use-package org
  :ensure t
  :bind (("C-c c" . org-capture)
	 ("C-c !" . org-time-stamp-inactive))
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
    (setq org-babel-lisp-eval-fn "sly-eval"
	  org-src-window-setup 'current-window)
    (require 'org-notmuch)
    ;; General Org Config
    (setq org-agenda-files (list "~/org/home.org" "~/org/work.org" "~/org/time-tracked.org")
	  org-refile-targets '((org-agenda-files :maxlevel . 3))
	  org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELED"))
	  org-enforce-todo-dependencies t
	  org-default-notes-file "~/org/global.org")))

(use-package elfeed
  :ensure t)

(use-package elfeed-org
  :ensure t
  :init
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/org/feeds.org")))

(defun newtimesheet ()
  "Create a new timesheet for the day."
  (interactive)
  (let ((default-clock-vars org-clock-clocktable-default-properties))
    (setf org-clock-clocktable-default-properties '(:maxlevel 2 :scope ("~/org/work.org") :block today))
    (find-file "~/org/timesheet.org")
    (org-insert-heading-respect-content)
    (org-time-stamp '(16) t)
    (newline)
    (org-clock-report)
    (save-current-buffer)
    (setf org-clock-clocktable-default-properties default-clock-vars)))

(run-at-time "7pm" (* 24 60 60) #'newtimesheet)

(provide 'org)
;;; org.el ends here
