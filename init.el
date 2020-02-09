;;; init.el --- Summary

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

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;;; Load Custom Set Variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;; Set up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((org-plus-contrib . "org"))))
(package-initialize)

;;; Bootstrap quelpa
;; Install quelpa if it's not already installed.
;; quelpa is used to configure the rest of the packages.
(if (require 'quelpa nil t)
    (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://framagit.org/steckerhalter/quelpa/raw/master/bootstrap.el")
    (eval-buffer)))

;;; Load the config
(org-babel-load-file (expand-file-name (concat user-emacs-directory "config.org")))

(provide 'init.el)
;;; init.el ends here
