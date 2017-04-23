;;; LISP --- Summary
;;; Commentary:
;;; Config for Lisp editing

;;; Code:

;;; Add .lsp files to lisp-mode
(add-to-list 'auto-mode-alist '("\\.lsp" . lisp-mode))

;; Rainbow Delimeters
;; (require 'rainbow-delimiters)
;; ;; Enables rainbow-delimiters-mode in Emacs Lisp buffers
;; (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode))

(use-package paredit
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode))

(use-package slime
  :ensure t
  :config
  (add-to-list 'load-path "~/.emacs.d/slime-contrib/slime")
  (setq inferior-lisp-program "sbcl")
  (setq slime-auto-start 'ask)
  (setq slime-contribs '(slime-fancy
			 slime-mrepl
			 slime-banner
			 slime-tramp
			 slime-xref-browser
			 slime-highlight-edits
			 slime-sprof))
  (add-hook 'slime-repl-mode-hook       #'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode))

(provide 'lisp)
;;; lisp.el ends here
