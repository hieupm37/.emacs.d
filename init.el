;;; init.el -*- lexical-binding: t -*-

;;; Code

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time))))))

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(defvar my/list-external-packages nil
  "List of external packages used by `my/ensure-packages-installed'")

(defun my/ensure-external-packages-installed ()
  "Install all `my/list-external-packages' packages if needed."
  (interactive)
  (when (yes-or-no-p (format "Try to install %d packages?"
                             (length my/list-external-packages)))
    (package-refresh-contents)
    (mapc (lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
          my/list-external-packages)))

(defmacro my/builtin-package (package &rest body)
  "Setup builtin PACKAGE with BODY"
  (declare (indent 1))
  `(progn
     (unless (require ,package nil 'noerror)
       (display-warning 'my (format "Loading `%s' failed." ,package) :warning))
     ,@body))

(defmacro my/external-package (package &rest body)
  "Setup PACKAGE from elpa and do configuration with BODY"
  (declare (indent 1))
  `(progn
     (when (not (package-installed-p ,package))
       (package-install ,package))
     (if (require ,package nil 'noerror)
         (progn ,@body)
       (display-warning 'my (format "Loading `%s' failed." ,package) :warning)
       (add-to-list 'my/list-external-packages ,package)
       (display-warning
        'my
        "Run `my/ensure-external-packages-installed' to manually install missing package"
        :warning))))

(my/external-package 'use-package
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  ;; Let imenu see `use-package' declarations.
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics nil)
  ;; Write hooks using their real name instead of shorter version.
  ;; For example: after-init => `after-init-hook'
  (setq use-package-hook-name-suffix nil))

;; Use to keep modeline clean.
(use-package diminish :ensure t)

(require 'vc)
(setq vc-follow-symlinks t)

;; Some basic settings
(setq ring-bell-function #'ignore)
(setq initial-buffer-choice t)  ; open *scratch* buffer at startup.

;; Use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

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
(use-package emacs
  :bind (("<insert>" . nil)
         ("C-z" . nil)
         ("C-x C-z" . nil)
         ("C-h h" . nil)
         ("M-`" . nil)))

;; Use ace-window to switch between windows
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(use-package eldoc :diminish)

;; Move custom variables to temporary file to make init.el clean.
(use-package cus-edit
  :config
  (setq custom-file (make-temp-file "emacs-custom-")))

;; Disable bidrectional writing might improve Emacs responsive some cases.
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Better handle very long lines in Emacs.
(use-package so-long
  :config
  (global-so-long-mode 1))

;; Typed text replace the selection
(use-package delsel
  :hook (after-init-hook . delete-selection-mode))

;; (use-package replace
;;   :config
;;   (setq list-matching-lines-jump-to-current-line t)
;;   :hook ((occur-mode-hook . hl-line-mode)
;;    (occur-mode-hook . (lambda ()
;;             (toggle-truncate-lines t))))
;;   :bind (("M-s M-o" . multi-occur)
;;    :map occur-mode-map
;;    ("t" . toggle-truncate-lines)))

;; Writable grep
;; TODO: Learn about wgrep and the working with embark export.
;; (use-package wgrep
;;   :ensure t
;;   :config
;;   (setq wgrep-auto-save-buffer t)
;;   (setq wgrep-change-readonly-file t)
;;   :bind (:map grep-mode-map
;;         ("e" . wgrep-change-to-wgrep-mode)
;;         ("C-x C-q" . wgrep-change-to-wgrep-mode)
;;         ("C-c C-c" . wgrep-finish-edit)))

;; TODO: Enable when using Emacs 28
;; (use-package xref
;;   :config
;;   (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
;;   (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep
;;   (setq xref-file-name-display 'project-relative)
;;   (setq xref-search-program 'gitgrep))

;; Prefer unix encoding when create new file.
(use-package emacs
  :config
  (prefer-coding-system 'utf-8-unix))

;;; History and state

;; Desktop session
(use-package desktop
  :config
  (setq desktop-auto-save-timeout 300)  ; 5 min
  (setq desktop-dirname user-emacs-directory)
  (setq desktop-base-file-name "desktop")
  (setq desktop-load-locked-desktop t) ; always load
  (setq desktop-missing-file-warning nil)
  (setq desktop-restore-eager 0) ; all files are lazy restored
  (setq desktop-restore-frames nil) ; don't restore frame
  (desktop-save-mode 1))

;; Minibuffer history
(use-package savehist
  :config
  (setq savehist-file (locate-user-emacs-file "savehist"))
  :hook (after-init-hook . savehist-mode))

;; Record cursor positions
(use-package saveplace
  :config
  (setq save-place-file (locate-user-emacs-file "saveplace"))
  (save-place-mode 1))

;; Move backup files to central location
(use-package emacs
  :config
  (defvar my/backup-dir (expand-file-name "backup/" user-emacs-directory))
  (setq backup-directory-alist `(("." . ,my/backup-dir)))
  (setq backup-by-copying t)
  (setq version-control t)
  (setq delete-old-versions t)
  (setq create-lockfiles nil))

;; Emacs server
(use-package server
  :hook (after-init-hook . server-start))

;;; Version control

;; Use ssh agency to remember passphrase
(use-package ssh-agency
  :ensure t
  :config
  (setenv "SSH_ASKPASS" "git-gui--askpass"))

;; Magit setup
(use-package magit
  :ensure t
  :init
  (setq magit-define-global-key-bindings nil)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c g" . magit-file-dispatch)))

;; Show fine differences for current hunk only.
(use-package magit-diff
  :after magit
  :config
  (setq magit-diff-refine-hunk t))

;; This is built-in package since Emacs 28
;; (use-package project
;;   :ensure t
;;   :config
;;   (setq my/project-commit-log-limit 50)

;;   (defun my/project-commit-log (&optional arg)
;;     "Print commit log for current project.
;; With \\[univeral-argument] shows expanded commit messages and corresponding diffs"
;;     (interactive "P")
;;     (let* ((pr (project-current t))
;;      (dir (cdr pr))
;;      (default-directory dir)
;;      (backend (vc-responsible-backend dir))
;;      (limit (if (= my/project-commit-log-limit 0) t my/project-commit-log-limit))
;;      (diffs (if arg 'with-diff nil))
;;      (vc-log-short-style (unless diffs '(directory))))
;;       (vc-print-log-internal backend (list dir) nil nil limit diffs)))

;;   (setq project-switch-commands
;;   '((?f "File" project-find-file)
;;     (?g "Grep" project-find-regexp)
;;     (?d "Dired" project-dired)
;;     (?b "Buffer" project-switch-to-buffer)
;;     (?q "Query Replace" project-query-replace-regexp)
;;     (?v "VC dir" project-vc-dir)
;;     (?l "Log VC" my/project-commit-log)
;;     (?s "Shell" project-shell)
;;     (?e "Eshell" project-eshell)))
;;   :bind (("C-x p q" . project-query-replace-regexp)
;;    ("C-x p l" . my/project-commit-log)))

;; Tie objects - actions with embark
;; Thanks to https://karthinks.com/software/fifteen-ways-to-use-embark/
(use-package embark
  :ensure t
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (use-package ace-window
    :config
    (defmacro my/embark-ace-action (fn)
      `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
         (interactive)
         (with-demoted-errors "%s"
           ;; (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn))))))

    (my/embark-ace-action find-file)
    (my/embark-ace-action switch-to-buffer)
    (my/embark-ace-action bookmark-jump)

    :bind (:map embark-file-map
                ("o" . my/embark-ace-find-file)
                :map embark-buffer-map
                ("o" . my/embark-ace-switch-to-buffer)
                :map embark-bookmark-map
                ("o" . my/embark-ace-bookmark-jump)))

  :bind (("C-," . embark-act)
         :map vertico-map
         ("C-M-," . embark-export)))

(use-package embark-consult
  :ensure t
  :after embark, consult)

;; Compile directly from Emacs
;; (use-package compile
;;   :config
;;   (setq compilation-scroll-output t)
;;
;;   (defun my/build_browser ()
;;     (interactive)
;;     (let ((project_root (locate-dominating-file default-directory ".git")))
;;       (when project_root
;;         (with-temp-buffer
;;           (cd project_root)
;;           (compile "ninja -C src\\out\\Debug chrome"))))))

;;; Applications and utilities

;; Restclient
(use-package restclient
  :ensure t)

;; Elfeed - RSS reader
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-db-directory (concat user-emacs-directory "elfeed/"))
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-show-unique-buffers t)
  (setq elfeed-feeds
        '(("https://news.ycombinator.com/rss" news)
          ("http://blog.chromium.org/atom.xml" blog chromium)
          ("https://chromereleases.googleblog.com/feeds/posts/default" release chromium)
          ("https://developer.chrome.com/feeds/blog.xml" chrome)
          ("https://blog.google/products/chrome/rss" blog chrome)
          ("https://blogs.windows.com/msedgedev/feed/" blog edge)
          ("https://randomascii.wordpress.com/feed/" blog)
          ("http://www.fluentcpp.com/feed/" cpp blog)
          ("https://www.joelonsoftware.com/feed/" software blog)
          ("https://herbsutter.com/feed/" cpp blog)
          ("http://www.modernescpp.com/index.php?format=feed" cpp blog)
          ("https://googleprojectzero.blogspot.com/feeds/posts/default" blog security)
          ("https://emacsredux.com/atom.xml" blog emacs)))

  (defvar my/saved-window-configuration nil
    "Current window configuration before opening elfeed search.")

  (defun my/elfeed-dwim ()
    "Open elfeed search buffer in maximized window."
    (interactive)
    (if (one-window-p)
        (elfeed)
      (setq my/saved-window-configuration (current-window-configuration))
      (delete-other-windows)
      (elfeed)))

  (defun my/elfeed-search-quit-dwim ()
    "Close search buffer, restore saved window configuration."
    (interactive)
    (elfeed-search-quit-window)
    (when my/saved-window-configuration
      (set-window-configuration my/saved-window-configuration)))

  (defun my/elfeed-entry-quit-dwim ()
    "Kill entry buffer and switch back to *elfeed-search* buffer."
    (interactive)
    (unless (one-window-p)
      (delete-other-windows))
    (elfeed-kill-buffer)
    (switch-to-buffer "*elfeed-search*"))

  :bind (("C-x w" . my/elfeed-dwim)
         :map elfeed-search-mode-map
         ("q" . my/elfeed-search-quit-dwim)
         :map elfeed-show-mode-map
         ("q" . my/elfeed-entry-quit-dwim)))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;;; Load emacs config from emacs-init.org file.

;; An "el" version of my Org configuration is created as a final step when
;; exiting Emacs. It helps to load the lastest version of my configuration
;; at startup.
(let* ((conf (expand-file-name "emacs-init" user-emacs-directory))
       (el (concat conf ".el"))
       (org (concat conf ".org")))
  (if (file-exists-p el)
      (load-file el)
    (require 'org)
    (org-babel-load-file org)))

;; Delete `emacs-init.el' when saving `emacs-init.org' file to update
;; the configuration in the next launch.
(defun my/delete-emacs-init-el-if-needed ()
  (when (string= (buffer-file-name) (expand-file-name "emacs-init.org" user-emacs-directory))
    (let ((filepath (expand-file-name "emacs-init.el" user-emacs-directory)))
      (when (file-exists-p filepath)
        (delete-file filepath)
        (message (format "%s has deleted" filepath))))))
(add-hook 'after-save-hook #'my/delete-emacs-init-el-if-needed)

;; Make GC pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
