;;; em-defaults.el -*- lexical-binding: t -*-

;;; Code

;; Disable some useless default functions and modes
(dolist (cmd '(narrow-to-region
               upcase-region
               downcase-region
               dired-find-alternate-file
               narrow-to-page
               set-goal-column
               scroll-left
               scroll-right))
  (put cmd 'disabled nil))
(put 'suspend-frame 'disabled t)
(put 'overwrite-mode 'disabled t)

;;; Disable some annoying default key bindings.
(global-set-key (kbd "<insert>") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "C-h h") nil)
(global-set-key (kbd "M-`") nil)

;; Move custom variables to temporary file to make init.el clean.
(require 'cus-edit)
(setq custom-file (make-temp-file "emacs-custom-"))

;; Disable bidrectional writing might improve Emacs responsive some cases.
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Better handle very long lines in Emacs.
(require 'so-long)
(global-so-long-mode 1)

;; Typed text replace the selection
(require 'delsel)
(add-hook 'after-init-hook #'delete-selection-mode)

;; Desktop session
(require 'desktop)
(setq desktop-auto-save-timeout 300)  ; 5 min
(setq desktop-dirname user-emacs-directory)
(setq desktop-base-file-name "desktop")
(setq desktop-load-locked-desktop t) ; always load
(setq desktop-missing-file-warning nil)
(setq desktop-restore-eager 0) ; all files are lazy restored
(setq desktop-restore-frames nil) ; don't restore frame
(desktop-save-mode 1)

;; Minibuffer history
(require 'savehist)
(setq savehist-file (locate-user-emacs-file "savehist"))
(add-hook 'after-init-hook #'savehist-mode)

;; Record cursor positions
(require 'saveplace)
(setq save-place-file (locate-user-emacs-file "saveplace"))
(save-place-mode 1)

;; Move backup files to central location
(defvar em--backup-dir (expand-file-name "backup/" user-emacs-directory))
(setq backup-directory-alist `(("." . ,em--backup-dir)))
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq create-lockfiles nil)

;; ;; Use ssh agency to remember passphrase
;; (use-package ssh-agency
;;   :ensure t
;;   :config
;;   (setenv "SSH_ASKPASS" "git-gui--askpass"))

;; Start Emacs server
(require 'server)
(add-hook 'after-init-hook #'server-start)


(provide 'em-defaults)
