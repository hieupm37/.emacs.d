;;; em-vc.el -*- lexical-binding: t; -*-

;;; Code

(straight-use-package 'magit)

(require 'vc)
(setq vc-follow-symlinks t)

(require 'magit)

(setq magit-define-global-key-bindings nil)

(global-set-key (kbd "C-x g") #'magit-status)
(global-set-key (kbd "C-x M-g") #'magit-dispatch)
(global-set-key (kbd "C-c g") #'magit-file-dispatch)

;; Show fine differences for current hunk only.
(setq magit-diff-refine-hunk t)

(provide 'em-vc)
