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
(diminish 'abbrev-mode)

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


    ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
    ;; may have their own settings.
    (load-theme 'doom-vibrant t)

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)

    ;; Enable custom neotree theme
    (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)))

(use-package nyan-mode
  :ensure t
  :init
  ;; Fix up Nyan Cat cause she's pretty
  (setq nyan-animate-nyancat t
	nyan-wavy-trail t)
  (nyan-mode))

;;; Setup Zone for 5min
;;; That gives us 5min of zone before screen turns off
(require 'zone)
(zone-when-idle 300)

(setq inhibit-startup-screen t
      initial-major-mode 'org-mode
      initial-buffer-choice "~/org/global.org"
      initial-scratch-message "\
* Notes
- This is a initial scratch buffer
- Buffer is set to org mode
- Run snippetts like a iPython Notebook using org-bable

")


(provide 'ui)
;;; ui.el ends here
