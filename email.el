;;; email --- Summary
;;; Commentary:

;;; Code:

(defun format-email ()
  (interactive)
  (beginning-of-buffer)
  (search-forward "--text follows this line--")
  (next-line nil)
  (message-beginning-of-line nil)
  (set-mark-command nil)
  (re-search-forward "^--")
  (previous-line nil)
  (move-end-of-line nil)
  (org-mime-htmlize)
  (set-mark-command nil)
  (search-backward "<#/multipart>\n<#/multipart>")
  (kill-region (point) (mark))
  (end-of-buffer)
  (insert "<#/multipart><#/multipart>"))

(use-package notmuch
  :ensure t
  :config
  (setq notmuch-search-oldest-first nil
	mail-specify-envelope-from t
	message-sendmail-envelope-from 'header
	mail-envelope-from 'header
	notmuch-address-command 'internal)
  (add-hook 'notmuch-message-mode-hook 'flyspell-mode)
  (add-hook 'notmuch-message-mode-hook 'flyspell-buffer)
  (add-hook 'message-send-hook 'format-email))



(provide 'email)
;;; email.el ends here
