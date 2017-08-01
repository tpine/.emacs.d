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
  :init (ivy-mode 1))

(use-package company
  ;; We do not add backends here do that on a per package basis
  :ensure t)

(use-package magit
  :ensure t
  :bind (("C-c m" . magit-status)))

(provide 'global)
;;; global.el ends here
