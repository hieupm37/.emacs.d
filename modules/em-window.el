;;; em-window.el -*- lexical-binding: t -*-

;;; Code

(straight-use-package 'ace-window)

;; ace window
(require 'ace-window)
(global-set-key (kbd "M-o") #'ace-window)

(provide 'em-window)
