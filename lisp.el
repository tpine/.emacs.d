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

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hooks 'rainbow-delimiters-mode '(emacs-lisp-mode-hook
					lisp-mode-hook
					sly-mrepl-mode-hook)))

(use-package paredit
  :ensure t
  :init
  (add-hooks 'enable-paredit-mode '(emacs-lisp-mode-hook
				    eval-expression-minibuffer-setup-hook
				    ielm-mode-hook
				    lisp-mode-hook
				    lisp-interaction-mode-hook
				    sly-mrepl-mode-hook)))

(provide 'lisp)
;;; lisp.el ends here
