;;; em-buffer.el -*- lexical-binding: t; -*-

;;; Code:

;;; General commands for buffers

(straight-use-package 'ibuffer-vc)
(straight-use-package 'beginend)
(straight-use-package 'goto-last-change)

(defun em--copy-filename-to-clipboard (&optional arg)
  "Copy the current buffer file name to clipboard.
With \\[universal-argument], copy relative path to project root."
  (interactive "P")
  (let ((filename (if (eq major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name)))
        (project-root (or (vc-root-dir)
                          (locate-dominating-file "." ".git"))))
    (when (and arg project-root)
      (setq filename (file-relative-name filename project-root)))
    (when filename
      (kill-new filename)
      (message "Copied %s to clipboard." filename))))

(defun em--rename-file-and-buffer ()
  "Rename current buffer and its file if avaiable."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-from-minibuffer "New name: " filename))
             (containing-dir (file-name-directory new-name)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(global-set-key (kbd "<f2>") #'em--copy-filename-to-clipboard)
(global-set-key (kbd "<f3>") #'em--rename-file-and-buffer)

;; Unique names for buffers
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-strip-common-suffix t)
(setq uniquify-after-kill-buffer-p t)

;; Auto update buffer if the file changed.
(require 'autorevert)
(setq auto-revert-verbose t)
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; ibuffer config
(require 'ibuffer)
(setq ibuffer-expert t)
(setq ibuffer-display-summary nil)
(setq ibuffer-use-other-window nil)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-movement-cycle nil)
(setq ibuffer-default-sorting-mode 'filename/process)
(setq ibuffer-use-header-line t)
(setq ibuffer-default-shrink-to-minimum-size nil)
(setq ibuffer-formats
      '((mark modified read-only locked " "
              (name 30 30 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))
(setq ibuffer-saved-filter-groups nil)
(setq ibuffer-old-time 48)

(add-hook 'ibuffer-mode-hook #'hl-line-mode)

(global-set-key (kbd "C-x C-b") #'ibuffer)

(define-key ibuffer-mode-map (kbd "* f") #'ibuffer-mark-by-file-name-regexp)
(define-key ibuffer-mode-map (kbd "* g") #'ibuffer-mark-by-content-regexp)
(define-key ibuffer-mode-map (kbd "* n") #'ibuffer-mark-by-name-regexp)
(define-key ibuffer-mode-map (kbd "s n") #'ibuffer-do-sort-by-alphabetic)
(define-key ibuffer-mode-map (kbd "/ g") #'ibuffer-filter-by-content)

(require 'ibuffer-vc)
(define-key ibuffer-mode-map (kbd "/ V") #'ibuffer-vc-set-filter-groups-by-vc-root)
(define-key ibuffer-mode-map (kbd "/ <deletechar>") #'ibuffer-clear-filter-groups)

;; Goto actionable beginning or end of buffer
(require 'beginend)
(beginend-global-mode)

;; Goto last change position
(require 'goto-last-change)
(global-set-key (kbd "C-z") #'goto-last-change)


(provide 'em-buffer)

