;;; em-search.el -*- lexical-binding: t; -*-

;;; Code:

;;; Configuration for search
(setq search-highlight t)
(setq search-whitespace-regexp ".*?")
(setq isearch-lax-whitespace t)
(setq isearch-regexp-lax-whitespace nil)
(setq isearch-lazy-highlight t)
;; From Emacs 27.1
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format " (%s/%s)")
(setq isearch-yank-on-move 'shift)
(setq isearch-allow-scroll 'unlimited)

(defun em--isearch-replace-symbol-at-point ()
  "Replace the symbol at point."
  (interactive)
  (isearch-forward-symbol-at-point)
  (isearch-query-replace-regexp))

(defmacro em--isearch-occurrence (name edge &optional doc)
  "Construct function for moving `isearch' occurence."
  `(defun ,name (&optional arg)
     ,doc
     (interactive "p")
     (let ((x (or arg 1))
           (command (intern (format "isearch-%s-of-buffer" ,edge))))
       (isearch-forward-symbol-at-point)
       (funcall command x))))

(em--isearch-occurrence
 em--isearch-beginning-of-buffer
 "beginning"
 "Run `isearch-beginning-of-buffer' for the symbol at point.")

(em--isearch-occurrence
 em--isearch-end-of-buffer
 "end"
 "Run `isearch-end-of-buffer' for the symbol at point.")

(global-set-key (kbd "M-s %") #'em--isearch-replace-symbol-at-point)
(global-set-key (kbd "M-s M-<") #'em--isearch-beginning-of-buffer)
(global-set-key (kbd "M-s M->") #'em--isearch-end-of-buffer)

(define-key isearch-mode-map (kbd "C-g") #'isearch-cancel)
(define-key isearch-mode-map (kbd "M-/") #'isearch-complete)

(provide 'em-search)
