;;; Global --- Summary
;;; Commentary:
;;; Global Config

;;; Code:

;;; Add flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init (ivy-mode 1))

(use-package counsel
  :ensure t
  :bind (("C-c g" . counsel-git-grep)))

(use-package company
  ;; We do not add backends here do that on a per package basis
  :ensure t)

(use-package magit
  :ensure t
  :bind (("C-c m" . magit-status)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
	 ("C--" . er/contract-region)))

(use-package move-text
  :ensure t
  :bind (("M-<up>" . move-text-up)
	 ("M-<down>" . move-text-down)))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hooks 'rainbow-delimiters-mode '(emacs-lisp-mode-hook
					lisp-mode-hook
					sly-mrepl-mode-hook
					php-mode-hook)))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (progn
    (use-package yasnippet-snippets
      :ensure t)
    (yas-global-mode 1)))

(use-package avy
  :bind (("C-:" . avy-goto-char)
	 ("C-'" . avy-goto-char-2)
	 ("M-g f" . avy-goto-line)
	 ("M-g w" . avy-goto-word-1))
  :init (avy-setup-default))

(provide 'global)
;;; global.el ends here
