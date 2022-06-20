;;; el-programming.el -*- lexical-binding: t; -*-

;;; Code:

(straight-use-package 'hl-todo)

;; Highlight comment keywords (TODO)
(require 'hl-todo)
(add-hook 'prog-mode-hook #'hl-todo-mode)

;; Show indicator at column 80 in prog modes.
(require 'display-fill-column-indicator)
(setq-default fill-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Recognize subwords
(require 'subword)
(add-hook 'prog-mode-hook #'subword-mode)

;; Comment line and region
(require 'newcomment)

(setq comment-empty-lines t)
(setq comment-fill-column nil)
(setq comment-multi-line t)
(setq commnet-style 'multi-line)

(defun em--comment-dwim (&optional arg)
  (interactive "P")
  (if (use-region-p)
      (comment-dwim arg)
    (save-excursion
      (comment-line arg))))

(global-set-key (kbd "C-x C-;") #'em--comment-dwim)

(provide 'em-programming)
