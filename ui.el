;;; UI --- Summary
;;; Commentary:
;;; Ui Config

;;; Code:

;;; Formatting

(setq c-default-style "linux"
      c-basic-offset 4)
(c-set-offset 'comment-intro 0)

;;; UI Tweaks
(global-linum-mode t)
(setq column-number-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(custom-set-variables '(custom-safe-themes t))

(use-package spacemacs-theme
  :ensure t
  :config
  (custom-set-variables '(custom-enabled-themes '(spacemacs-dark))))

;;; Doesn't work in above use-package
(load-theme 'spacemacs-dark t)
  
(use-package nyan-mode
  :ensure t
  :config
  ;; Fix up Nyan Cat cause she's pretty
  (setq nyan-animate-nyancat t
	nyan-wavy-trail t)
  (nyan-mode))

  ;;; Add powerline
  (use-package powerline
    :ensure t
    :config (powerline-default-theme))

  (use-package spaceline-config
    :ensure spaceline
    :config (spaceline-spacemacs-theme))

(provide 'ui)
;;; ui.el ends here
