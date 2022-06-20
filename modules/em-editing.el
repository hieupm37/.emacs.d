;;; em-editing.el -*- lexical-binding: t; -*-

;;; Code:

;;; General commands for lines

(defun em--new-line-below ()
  "Create an empty new line below the current one. Indent if mode is auto indent."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun em--new-line-above ()
  "Create an empty line above the current one. Indent if mode is auto indent."
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-SPC") #'cycle-spacing)
(global-set-key (kbd "<C-return>") #'em--new-line-below)
(global-set-key (kbd "<C-S-return>") #'em--new-line-above)

(defun em--mark-symbol (&optional arg allow-extend)
  "Mark symbols at point."
  (interactive "P\np")
  (cond ((and allow-extend
              (or (and (eq last-command this-command) (mark t))
                  (region-active-p)))
         (setq arg (if arg (prefix-numeric-value arg)
                     (if (< (mark) (point)) -1 1)))
         (set-mark
          (save-excursion
            (goto-char (mark))
            (forward-symbol arg)
            (point))))
        (t
         (let ((bounds (bounds-of-thing-at-point 'symbol)))
           (unless (consp bounds)
             (user-error "No symbol at point."))
           (if (>= (prefix-numeric-value arg) 0)
               (goto-char (car bounds))
             (goto-char (cdr bounds)))
           (push-mark
            (save-excursion
              (forward-symbol (prefix-numeric-value arg))
              (point)))
           (activate-mark)))))

(defun em--mark-sexp-backward (&optional arg)
  "Mark previous or ARGs balanced expressions."
  (interactive "P")
  (if arg
      (mark-sexp (- arg) t)
    (mark-sexp (- 1) t)))

(defun em--mark-dwim (&optional arg)
  "Mark symbol or balanced expression at point."
  (interactive "P")
  (cond
   ((symbol-at-point)
    (my/mark-symbol arg t))
   ((eq (point) (cdr (bounds-of-thing-at-point 'sexp)))
    (my/mark-sexp-backward arg))
   (t
    (mark-sexp arg t))))

(global-set-key (kbd "C-M-SPC") #'em--mark-dwim)

;; Save existing clipboard text into the kill-ring before replacing it.
;; It can be retrived via C-y or M-y
(setq save-interprogram-paste-before-kill t)

;; Auto add empty newline for file ending when save.
(setq mode-require-final-newline t)

;; Prefer spaces over tab
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
;; First tab is indent and second tab is completion.
(setq-default tab-always-indent 'complete)

(add-hook 'before-save-hook #'delete-trailing-whitespace)


(provide 'em-editing)

