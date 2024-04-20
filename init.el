;;; Commentary:
;;; Entry Point for Emacs customization

;;; Code:

;;; Begin initialization
;; Turn off mouse interface early in startup to avoid momentary display
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))
;; Add Line numbers onto a sidebar
(global-display-line-numbers-mode)

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;;; Load Custom Set Variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;; Set up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defun internet-up-p (&optional host)
  (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1" 
		     (if host host "www.google.com"))))

;;; Themeing
(use-package doom-themes
  :ensure t
  :init
  (progn 
    (require 'doom-themes)

    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	  doom-themes-enable-italic t
	  doom-vibrant-brighter-modeline nil
	  org-hide-leading-stars nil) ; if nil, italics is universally disabled
    (load-theme 'doom-gruvbox t)
    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)))

(use-package nyan-mode
  :ensure t
  :init
  ;; Fix up Nyan Cat cause she's pretty
  (setq nyan-animate-nyancat t
	nyan-wavy-trail t)
  (nyan-mode))

;;; Ivy and Consel
(use-package ivy
  :ensure t
  :bind (("C-s" . swiper)
	 ("C-S-s" . isearch-forward))
  :diminish ivy-mode
  :init (ivy-mode 1))

(use-package counsel
  :ensure t
  :bind (("C-c g" . counsel-rg)))

;;; Magit
(use-package magit
  :ensure t
  :bind (("C-c m" . magit-status)))

(provide 'init.el)
;;; init.el ends here
