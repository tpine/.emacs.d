;;; email --- Summary
;;; Commentary:

;;; Code:
(use-package notmuch
  :ensure t
  :config
  (setq notmuch-search-oldest-first nil
	mail-specify-envelope-from t
	message-sendmail-envelope-from 'header
	mail-envelope-from 'header
	notmuch-address-command 'internal))

(provide 'email)
;;; email.el ends here
