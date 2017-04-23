;;; Global --- Summary
;;; Commentary:
;;; Global Config

;;; Code:

;;; Add flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package magit
  :ensure t
  :bind (("C-c m" . magit-status)))

(provide 'global)
;;; global.el ends here
