;;; early-init.el -*- lexical-binding: t -*-

;;; Code

;; Initialize installed packages
(setq package-enable-at-startup t)

;; Allow loading from package cache
(setq package-quickstart t)

;; Do not resize the frame at this early stage
(setq frame-inhibit-implied-resize t)

;; Disable some GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-dialog-box t)
(setq use-file-dialog nil)

(setq inhibit-startup-echo-area-message "Power of CREATION!")
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)

;; Start initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq native-comp-async-report-warnings-errors 'silent)  ; emacs 28 with native compilation

;;; early-init.el ends here
