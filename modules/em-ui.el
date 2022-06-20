;;; em-ui.el -*- lexical-binding: t; -*-

;;; Code:

(straight-use-package 'diminish)

;;; Setup font based on screen dpi

(defun em--screen-dpi-of-frame (&optional frame)
  "Get the DPI of the frame (or the current if nil)."
  (cl-flet ((pyth (lambda (w h)
                    (sqrt (+ (* w w) (* h h)))))
            (mm2in (lambda (mm)
                     (/ mm 25.4))))
    (let* ((atts (frame-monitor-attributes frame))
           (pxw (cl-fourth (assoc 'geometry atts)))
           (pxh (cl-fifth (assoc 'geometry atts)))
           (pxd (pyth pxw pxh))
           (mmw (cl-second (assoc 'mm-size atts)))
           (mmh (cl-third (assoc 'mm-size atts)))
           (mmd (pyth mmw mmh)))
      (/ pxd (mm2in mmd)))))

(defun em-screen-dpi ()
  "Tell the DPI of current screen."
  (interactive)
  (message "Your DPI is %s" (my/screen-dpi-of-frame (selected-frame))))

(defun em-setup-font ()
  "Setup font for current frame."
  (interactive)
  (let* ((dpi (em--screen-dpi-of-frame (selected-frame)))
         (font-size (cond
                     ((< dpi 96) 12)
                     ((< dpi 160) 14)
                     (t 16))))
    (if (eq system-type 'windows-nt)
        (set-frame-font (format "Consolas %s" font-size))
      (set-frame-font (format "Source Code Pro %s" font-size)))))

(add-hook 'after-init-hook #'em-setup-font)

;; Format frame's title
(setq frame-title-format '("Emacs - " (:eval (if (buffer-file-name)
                                                 (abbreviate-file-name (buffer-file-name))
                                               "%b"))))

;; Show column number in modeline
(add-hook 'after-init-hook #'column-number-mode)

;; Hide minor mode in modeline
(require 'diminish)
(diminish 'eldoc)

;; Setup theme
(load-theme 'wombat)

;; Refine window borders
(setq window-divider-default-right-width 1)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-places 'right-only)
(add-hook 'after-init-hook #'window-divider-mode)

;; Don't use blink cursor
(setq-default cursor-type 'box)
(blink-cursor-mode -1)

;; Shorter yes-or-no
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;; Keep mouse away from input cursor
(require 'avoid)
(mouse-avoidance-mode 'animate)

;; Visualize matching parens
(require 'paren)
(add-hook 'after-init-hook #'show-paren-mode)

(provide 'em-ui)
