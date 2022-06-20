;;; em-misc.el -*- lexical-binding: t -*-

;;; Code

(straight-use-package 'gn-mode)
(straight-use-package 'dotenv-mode)
(straight-use-package 'groovy-mode)
(straight-use-package 'restclient)

;; Mode for gni? file
(require 'gn-mode)
(add-to-list 'auto-mode-alist '("\\.gni?\\'" . gn-mode))

;; Mode for .env file
(require 'dotenv-mode)
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode))

;; Mode for groovy file
(require 'groovy-mode)
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))

;; Mode for js, ts file
(require 'js)
(setq js-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . js-mode))

;; Mode for html file
(require 'sgml-mode)
(setq sgml-basic-offset 2)

;; Restclient
(require 'restclient)

(provide 'em-misc)
