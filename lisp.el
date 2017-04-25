;;; LISP --- Summary
;;; Commentary:
;;; Config for Lisp editing

;;; Code:

;;; Add .lsp files to lisp-mode
(add-to-list 'auto-mode-alist '("\\.lsp" . lisp-mode))

(use-package sly
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl")
  (setq sly-auto-start 'ask))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'sly-mrepl-mode-hook #'rainbow-delimiters-mode))

(use-package paredit
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'sly-mrepl-mode-hook #'enable-paredit-mode))

(provide 'lisp)
;;; lisp.el ends here
