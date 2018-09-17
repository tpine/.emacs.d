;;; LISP --- Summary
;;; Commentary:
;;; Config for Lisp editing

;;; Code:

;;; Add .lsp files to lisp-mode
(add-to-list 'auto-mode-alist '("\\.lsp" . lisp-mode))

(defun connect-to-stumpwm ()
  "Connect to stumpwm on localhost port 4004."
  (interactive)
  (sly-connect "127.0.0.1" 4004))

(use-package sly
  :ensure t
  :bind (("C-c s" . connect-to-stumpwm))
  :config
  (setq inferior-lisp-program "sbcl")
  (setq sly-auto-start 'ask))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode-hook . enable-paredit-mode)
	 (eval-expression-minibuffer-setup-hook . enable-paredit-mode)
	 (ielm-mode-hook . enable-paredit-mode)
	 (lisp-mode-hook . enable-paredit-mode)
	 (lisp-interaction-mode-hook . enable-paredit-mode)
	 (sly-mrepl-mode-hook . enable-paredit-mode)))

(provide 'lisp)
;;; lisp.el ends here
