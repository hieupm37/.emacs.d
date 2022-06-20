;;; init.el -*- lexical-binding: t -*-

;;; Code

(require 'package)

(add-to-list 'package-archives
       '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
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

(eval-when-compile
  (require 'use-package))

;; Use to keep modeline clean.
(use-package diminish :ensure t)

(require 'vc)
(setq vc-follow-symlinks t)

;; Change startup directory on Windows to HOME directory.
(when (eq system-type 'windows-nt)
  (cd "~/")
  (setenv "LANG" "en_US"))

;; Some basic settings
(setq frame-title-format '("Emacs - " (:eval (if (buffer-file-name)
                                                 (abbreviate-file-name (buffer-file-name))
                                               "%b"))))
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

;;; Setup font based on screen dpi
(use-package emacs
  :config
  ;; Setup font size based on the DPI of screen
  (defun my/screen-dpi-of-frame (&optional frame)
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

  (defun my/screen-dpi ()
    "Tell the DPI of current screen."
    (interactive)
    (message "Your DPI is %s" (my/screen-dpi-of-frame (selected-frame))))

  (defun my/setup-font ()
    "Setup font for current frame."
    (interactive)
    (let* ((dpi (my/screen-dpi-of-frame (selected-frame)))
           (font-size (cond
                       ((< dpi 96) 12)
                       ((< dpi 160) 14)
                       (t 16))))
      (if (eq system-type 'windows-nt)
          (set-frame-font (format "Consolas %s" font-size))
        (set-frame-font (format "Source Code Pro %s" font-size)))))

  ;; Setup font automatically for the initial frame. You need to M-x
  ;; `my/setup-font' manually after creating new frame or move frame to ohter screen.
  :hook ((after-init-hook . my/setup-font)
         (after-init-hook . column-number-mode)))

;; Configure the Modus Themes' appearance
(setq modus-themes-mode-line '(accented borderless)
      ; modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-fringes 'subtle
      modus-themes-tabs-accented t
      modus-themes-paren-match '(bold intense)
      modus-themes-prompts '(bold intense)
      modus-themes-completions 'opinionated
      modus-themes-org-blocks 'tinted-background
      modus-themes-scale-headings t
      modus-themes-region '(bg-only)
      modus-themes-headings
      '((1 . (rainbow overline background 1.4))
        (2 . (rainbow background 1.3))
        (3 . (rainbow bold 1.2))
        (t . (semilight 1.1))))

;; Load the dark theme by default
(load-theme 'modus-vivendi t)

;; Refine window borders
(use-package emacs
  :config
  (setq window-divider-default-right-width 1)
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-places 'right-only)
  :hook (after-init-hook . window-divider-mode))

;; Highlight comment keywords (TODO)
(use-package hl-todo
  :ensure t
  :hook (prog-mode-hook . hl-todo-mode))

;; Don't use blink cursor
(use-package emacs
  :config
  (setq-default cursor-type 'box)
  (blink-cursor-mode -1))

;; Keep mouse away from input cursor
(use-package avoid
  :config
  (mouse-avoidance-mode 'animate))

;; Use ace-window to switch between windows
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

;;; General commands for lines
(use-package emacs
  :config
  (defun my/new-line-below ()
    "Create an empty new line below the current one. Indent if mode is auto indent."
    (interactive)
    (end-of-line)
    (newline-and-indent))

  (defun my/new-line-above ()
    "Create an empty line above the current one. Indent if mode is auto indent."
    (interactive)
    (beginning-of-line)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode))

  :bind (("M-SPC" . cycle-spacing)
         ;; ("M-o" . delete-blank-lines)  ; alias for C-x C-o
         ("<C-return>" . my/new-line-below)
         ("<C-S-return>" . my/new-line-above)))

;;; General commands for buffers
(use-package emacs
  :config
  (defun my/copy-filename-to-clipboard (&optional arg)
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

  (defun my/rename-file-and-buffer ()
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

  :bind (("<f2>" . my/copy-filename-to-clipboard)
         ("<f3>" . my/rename-file-and-buffer)))

;;; General commands for marking objects
(use-package emacs
  :config
  (defun my/mark-symbol (&optional arg allow-extend)
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

  (defun my/mark-sexp-backward (&optional arg)
    "Mark previous or ARGs balanced expressions."
    (interactive "P")
    (if arg
        (mark-sexp (- arg) t)
      (mark-sexp (- 1) t)))

  (defun my/mark-dwim (&optional arg)
    "Mark symbol or balanced expression at point."
    (interactive "P")
    (cond
     ((symbol-at-point)
      (my/mark-symbol arg t))
     ((eq (point) (cdr (bounds-of-thing-at-point 'sexp)))
      (my/mark-sexp-backward arg))
     (t
      (mark-sexp arg t))))

  :bind (("C-M-SPC" . my/mark-dwim)))

(use-package eldoc :diminish)

;; Move custom variables to temporary file to make init.el clean.
(use-package cus-edit
  :config
  (setq custom-file (make-temp-file "emacs-custom-")))

;; Show indicator at column 80
(use-package display-fill-column-indicator
  :config
  (setq-default fill-column 80)
  :hook ((prog-mode-hook . display-fill-column-indicator-mode)))

(use-package paren
  :hook (after-init-hook . show-paren-mode))

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

;; Configure orderless
(use-package orderless
  :ensure t
  :config
  (defun my/orderless-literal-dispatcher (pattern _index _total)
    "Literal style dispatcher using equal sign as a suffix."
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun my/orderless-initialism-dispatcher (pattern _index _total)
    "Leading initalism dispatcher using comma sign as a suffix."
    (when (string-suffix-p "," pattern)
      `(orderless-initialism . ,(substring pattern 0 -1))))

  (setq orderless-matching-styles
        '(orderless-prefixes
          orderless-literal
          orderless-initialism
          orderless-regexp
          orderless-flex))
  (setq orderless-style-dispatchers
        '(my/orderless-literal-dispatcher
          my/orderless-initialism-dispatcher))

  (setq completion-styles '(partial-completion substring flex orderless)))

;; Use vertico for narrowing selections
(use-package vertico
  :ensure t
  :after orderless
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t)

  (defun my/up-directory (arg)
    "Move up a directory (delete backwards to /)."
    (interactive "p")
    (if (string-match-p "/." (minibuffer-contents))
        (zap-up-to-char (- arg) ?/)
      (delete-minibuffer-contents)))

  :bind (:map vertico-map
              ("C-j" . vertico-exit)
              ("<C-backspace>" . my/up-directory)))

;; Add more information to completion candidates.
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  :config
  (setq marginalia-max-relative-age 0))

;; Consulting completing-read
(use-package consult
  :ensure t
  :config
  (setq consult-narrow-key ">")
  (setq completion-in-region-function #'consult-completion-in-region)
  ;; Use completing-read-multiple provided by consult
  (advice-add #'completing-read-multiple
              :override #'consult-completing-read-multiple)

  (defun my/consult-project-root ()
    "Returns patht to project or `default-directory'."
    (or (vc-root-dir)
        (locate-dominating-file "." ".git")
        default-directory))
  (setq consult-project-root-function #'my/consult-project-root)
  ;; Using consult's complex-command instead of builtin
  (global-set-key [remap repeat-complex-command] #'consult-complex-command)
  :bind (;; C-x bindings (ctl-x-map)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ;; Custom bindings
         ("C-y" . yank)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g M" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g M-i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-git-grep)
         ("M-s G" . consult-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch intergration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)
         ("M-s e" . consult-isearch)))

;; Insert directory paths into minibuffer prompts.
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)))

;; Dynamic word completions
(use-package dabbrev
  :config
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines t)
  (setq dabbrev-upcase-means-case-search t)
  :bind (("C-M-/" . dabbrev-completion)))

(use-package hippie-exp
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-complete-file-name-partially
          try-complete-file-name))
  (setq hippie-expand-verbose t)
  (setq hippie-expand-dabbrev-skip-space nil)
  (setq hippie-expand-dabbrev-as-symbol t)
  (setq hippie-expand-no-restriction t)
  :bind (("M-/" . hippie-expand)))

;;; Configuration for search
(use-package isearch
  :config
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

  (defun my/isearch-replace-symbol-at-point ()
    "Replace the symbol at point."
    (interactive)
    (isearch-forward-symbol-at-point)
    (isearch-query-replace-regexp))

  (defmacro my/isearch-occurrence (name edge &optional doc)
    "Construct function for moving `isearch' occurence."
    `(defun ,name (&optional arg)
       ,doc
       (interactive "p")
       (let ((x (or arg 1))
             (command (intern (format "isearch-%s-of-buffer" ,edge))))
         (isearch-forward-symbol-at-point)
         (funcall command x))))

  (my/isearch-occurrence
   my/isearch-beginning-of-buffer
   "beginning"
   "Run `isearch-beginning-of-buffer' for the symbol at point.")

  (my/isearch-occurrence
   my/isearch-end-of-buffer
   "end"
   "Run `isearch-end-of-buffer' for the symbol at point.")

  :bind (("M-s %" . my/isearch-replace-symbol-at-point)
         ("M-s M-<" . my/isearch-beginning-of-buffer)
         ("M-s M->" . my/isearch-end-of-buffer)
         :map isearch-mode-map
         ("C-g" . isearch-cancel)
         ("M-/" . isearch-complete)))

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

;; Unique names for buffers
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

;; Auto update buffer if the file changed.
(use-package autorevert
  :config
  (setq auto-revert-verbose t)
  :hook (after-init-hook . global-auto-revert-mode))

(use-package ibuffer
  :config
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
  :hook (ibuffer-mode-hook . hl-line-mode)
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("* f" . ibuffer-mark-by-file-name-regexp)
         ("* g" . ibuffer-mark-by-content-regexp)
         ("* n" . ibuffer-mark-by-name-regexp)
         ("s n" . ibuffer-do-sort-by-alphabetic)
         ("/ g" . ibuffer-filter-by-content)))

(use-package ibuffer-vc
  :ensure t
  :after ibuffer, vc
  :bind (:map ibuffer-mode-map
              ("/ V" . ibuffer-vc-set-filter-groups-by-vc-root)
              ("/ <deletechar>" . ibuffer-clear-filter-groups)))

;; Goto actionable beginning or end of buffer
(use-package beginend
  :ensure t
  :config
  (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
    (diminish mode))
  (beginend-global-mode))

;; Goto last change position
(use-package goto-last-change
  :ensure t
  :bind ("C-z" . goto-last-change))

;; Recognize subwords
(use-package subword
  :hook (prog-mode-hook . subword-mode))

;; Save existing clipboard text into the kill-ring before replacing it.
;; It can be retrived via C-y or M-y
(use-package emacs
  :config
  (setq save-interprogram-paste-before-kill t))

;; Auto add empty newline for file ending when save.
(use-package emacs
  :config
  (setq mode-require-final-newline t))

;; Prefer spaces over tab
(use-package emacs
  :config
  (setq-default tab-width 2)
  (setq-default indent-tabs-mode nil)
  ;; First tab is indent and second tab is completion.
  (setq-default tab-always-indent 'complete)

  :hook (before-save-hook . delete-trailing-whitespace))

;; Prefer unix encoding when create new file.
(use-package emacs
  :config
  (prefer-coding-system 'utf-8-unix))

;;; History and state

;; Completion for recent files and directories
(use-package recentf
  :config
  (setq recentf-save-file (locate-user-emacs-file "recentf"))
  (setq recentf-max-saved-items 200)
  (setq recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))

  (defun my/recentf-keep-predicate (file)
    "Additional conditions for saving FILE in `recentf-list'."
    (cond
     ((file-directory-p file) (file-readable-p file))))

  (add-to-list 'recentf-keep #'my/recentf-keep-predicate)

  (defun my/recentf-select-files ()
    "Select item from `recentf-list' using completion."
    (interactive)
    (let* ((files (mapcar 'abbreviate-file-name recentf-list))
           (f (completing-read "Recent file: " files nil t)))
      (find-file f)))

  :hook (after-init-hook . recentf-mode)
  :bind ("C-x C-r" . my/recentf-select-files))

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


;;; Development settings

;; C++ development

;; Auto detect mode for header file
(use-package cc-mode
  :config
  ;; Ref: https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/cc/autoload.el
  (defvar my/+cc-default-header-file-mode 'c++-mode
    "Fallback major mode for header files if all heuristics fail.")

  (defun my/+cc--re-search-for (regexp)
    (save-excursion
      (save-restriction
        (save-match-data
          (widen)
          (goto-char (point-min))
          (re-search-forward regexp magic-mode-regexp-match-limit t)))))

  (defun my/+cc-c-c++-objc-mode ()
    "Uses heuristics to detect `c-mode', `objc-mode' or `c++-mode'.
1. Checks if there are nearby cpp/cc/m/mm files with the same name.
2. Checks for ObjC and C++-specific keywords and libraries.
3. Falls back to `+cc-default-header-file-mode', if set.
4. Otherwise, activates `c-mode'.
This is meant to replace `c-or-c++-mode' (introduced in Emacs 26.1), which
doesn't support specification of the fallback mode and whose heuristics are
simpler."
    (let ((base (file-name-sans-extension (buffer-file-name (buffer-base-buffer)))))
      (cond ((file-exists-p (or (concat base ".cpp")
                                (concat base ".cc")))
             (c++-mode))
            ((or (file-exists-p (or (concat base ".m")
                                    (concat base ".mm")))
                 (my/+cc--re-search-for
                  (concat "^[ \t\r]*\\(?:"
                          "@\\(?:class\\|interface\\|property\\|end\\)\\_>"
                          "\\|#import +<Foundation/Foundation.h>"
                          "\\|[-+] ([a-zA-Z0-9_]+)"
                          "\\)")))
             (objc-mode))
            ((my/+cc--re-search-for
              (let ((id "[a-zA-Z0-9_]+") (ws "[ \t\r]+") (ws-maybe "[ \t\r]*"))
                (concat "^" ws-maybe "\\(?:"
                        "using" ws "\\(?:namespace" ws "std;\\|std::\\)"
                        "\\|" "namespace" "\\(?:" ws id "\\)?" ws-maybe "{"
                        "\\|" "class"     ws id ws-maybe "[:{\n]"
                        "\\|" "template"  ws-maybe "<.*>"
                        "\\|" "#include"  ws-maybe "<\\(?:string\\|iostream\\|map\\)>"
                        "\\)")))
             (c++-mode))
            ((functionp my/+cc-default-header-file-mode)
             (funcall my/+cc-default-header-file-mode))
            ((c-mode)))))

  (add-to-list 'auto-mode-alist '("\\.h\\'" . my/+cc-c-c++-objc-mode))
  (add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode)))

;; Toggle between source and header file
(use-package find-file
  :config
  (defmacro my/cc-other-file (name fff &optional doc)
    "Toggle source/header file."
    `(defun ,name ()
       ,doc
       (interactive)
       (let* ((command (intern ,fff))
              (buf (current-buffer))
              (name (file-name-sans-extension (buffer-file-name)))
              (other-extens
               (cadr (assoc (concat "\\."
                                    (file-name-extension (buffer-file-name))
                                    "\\'")
                            cc-other-file-alist))))
         (dolist (e other-extens)
           (if (let ((f (concat name e)))
                 (and (file-exists-p f) (funcall command f)))
               (return))))))
  (my/cc-other-file
   my/cc-other-file-current
   "find-file"
   "Run `find-file' with other cc file.")

  (my/cc-other-file
   my/cc-other-file-other
   "find-file-other-window"
   "Run `find-file-other-window' with other cc file.")

  :bind (:map c++-mode-map
              ("C-c o" . my/cc-other-file-current)
              ("C-c 4 o" . my/cc-other-file-other)))

;; Use Google C++ style
(use-package google-c-style
  :ensure t
  :hook ((c-mode-common-hook . (lambda ()
                                 (google-set-c-style)
                                 (google-make-newline-indent)))))

;; Font lock for modern C++
(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode-hook . modern-c++-font-lock-mode))

;; Mode for gni? file
(use-package gn-mode
  :ensure t
  :mode "\\.gni?\\'")

;; Mode for .env file
(use-package dotenv-mode
  :ensure t
  :mode ("\\.env\\..*\\'"))

;; Mode for groovy file
(use-package groovy-mode
  :ensure t
  :mode ("\\.groovy\\'"))

;; Mode for js, ts file
(use-package js
  :config
  (setq indent-tabs-mode nil)
  (setq js-indent-level 2)
  :mode (("\\.js\\'" . js-mode)
         ("\\.ts\\'" . js-mode)))

;; Mode for html file
(use-package sgml-mode
  :config
  (setq indent-tabs-mode nil)
  (setq sgml-basic-offset 2))

;; Comment line and region
(use-package newcomment
  :config
  (setq comment-empty-lines t)
  (setq comment-fill-column nil)
  (setq comment-multi-line t)
  (setq commnet-style 'multi-line)

  (defun my/comment-dwim (&optional arg)
    (interactive "P")
    (if (use-region-p)
        (comment-dwim arg)
      (save-excursion
        (comment-line arg))))

  :bind (("C-x C-;" . my/comment-dwim)))

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

;; Org mode
(use-package org
  :config
  ;; Agenda and basic directory structure
  (setq org-directory "~/org/")
  (setq org-default-notes-file "~/org/notes.org")
  (setq org-agenda-files
        '("~/org/"))

  ;; Refile, todos
  (setq org-refile-targets
        '((org-agenda-files . (:maxlevel . 3))
          (nil . (:maxlevel . 3))))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache t)
  ;; Regenerate refile cache everytime Emacs has been idled for 5 minutes
  (run-with-idle-timer 300 t (lambda ()
                               (org-refile-cache-clear)
                               (org-refile-get-targets)))

  (setq org-reverse-note-order nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)" "CANCEL(c)")))

  ;; Interface
  (setq org-fontify-done-headline nil)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-heading-line nil)
  (setq org-fontify-whole-block-delimiter-line t)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-track-ordered-property-with-tag t)
  (setq org-priority-faces
        '((?A . '(org-scheduled-today org-priority))
          (?B . 'org-priority)
          (?C . '(shadow org-priority))))
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?A)

  ;; code blocks
  (setq org-confirm-babel-evaluate nil)

  ;; Log
  (setq org-log-done 'time)
  (setq org-log-note-clock-out nil)
  (setq org-log-redeadline nil)
  (setq org-log-reschedule nil)
  (setq org-read-date-prefer-future 'time)

  ;; General
  (setq org-adapt-indentation nil)
  (setq org-special-ctrl-a/e nil)
  (setq org-special-ctrl-k nil)
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-hide-emphasis-markers t)
  (setq org-hide-leading-stars nil)
  (setq org-structure-template-alist
        '(("s" . "src")
          ("E" . "src emacs-lisp")
          ("e" . "example")
          ("q" . "quote")))
  (setq org-catch-invisible-edits 'show)
  (setq org-return-follows-link nil)
  (setq org-loop-over-headlines-in-active-region 'start-level)
  (setq org-imenu-depth 7)

  :bind (:map org-mode-map
              ("<C-return>" . nil)
              ("<C-S-return>" . nil)))

(use-package org-capture
  :after org
  :config
  (setq org-capture-templates
        `(("i" "Inbox" entry
           (file+headline "inbox.org" "Tasks")
           ,(concat "* TODO [#A] %^{Title}\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%i%?"))
          ("n" "Note" entry
           (file+headline "notes.org" "Notes")
           ,(concat "* %^{Title}\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%i%?"))))

  (defun my/org-capture-no-delete-windows (oldfun args)
    (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
      (apply oldfun args)))

  (advice-add 'org-capture-place-template
              :around 'my/org-capture-no-delete-windows)

  (defun my/org-capture-inbox ()
    "Capture directly to inbox."
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "i"))

  :bind (("C-c c" . org-capture)
         ("C-c i" . my/org-capture-inbox)))

(use-package org-agenda
  :after org
  :config
  ;; Basic setup
  (setq org-agenda-span 14)
  (setq org-agenda-start-on-weekday 1) ; Monday
  (setq org-agenda-confirm-kill t)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-show-outline-path nil)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-skip-comment-trees t)
  (setq org-agenda-menu-show-matcher t)
  (setq org-agenda-menu-two-columns nil)
  (setq org-agenda-sticky nil)
  (setq org-agenda-custom-commands-contexts nil)
  (setq org-agenda-max-entries nil)
  (setq org-agenda-max-todos nil)
  (setq org-agenda-max-tags nil)
  (setq org-agenda-max-effort nil)

  ;; General view options
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo . " %i %-12:c")
          (tags . " %i %-12:c")
          (search . " %i %-12:c")))
  (setq org-agenda-sorting-strategy
        '(((agenda habit-down time-up priority-down category-keep)
           (todo priority-down category-keep)
           (tags priority-down category-keep)
           (search category-keep))))
  (setq org-agenda-breadcrumbs-separator "->")
  (setq org-agenda-todo-keyword-format "%-1s")
  (setq org-agenda-diary-sexp-prefix nil)
  (setq org-agenda-fontify-priorities 'cookies)
  (setq org-agenda-category-icon-alist nil)
  (setq org-agenda-remove-times-when-in-prefix nil)
  (setq org-agenda-remove-timeranges-from-blocks nil)
  (setq org-agenda-compact-blocks nil)
  (setq org-agenda-block-separator ?-)
  (setq org-agenda-format-date 'org-agenda-format-date-aligned)

  ;; Marks
  (setq org-agenda-bulk-mark-char "#")
  (setq org-agenda-persistent-marks nil)

  ;; Diary entries
  (setq org-agenda-insert-diary-strategy 'date-tree)
  (setq org-agenda-insert-diary-extract-time nil)
  (setq org-agenda-include-diary t)

  ;; Follow mode
  (setq org-agenda-start-with-follow-mode nil)
  (setq org-agenda-follow-indirect t)

  ;; Multi-item tasks
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-todo-list-sublevels t)

  ;; Filters and restricted views
  (setq org-agenda-persistent-filter nil)
  (setq org-agenda-restriction-lock-highlight-subtree t)

  ;; Items with deadline and scheduled timestamps
  (setq org-agenda-include-deadlines t)
  (setq org-deadline-warning-days 5)
  (setq org-agenda-skip-scheduled-if-done nil)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  (setq org-agenda-skip-deadline-if-done nil)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 1)
  (setq org-agenda-skip-scheduled-delay-if-deadline nil)
  (setq org-agenda-skip-additional-timestamps-same-entry nil)
  (setq org-agenda-skip-timestamp-if-done nil)
  (setq org-agenda-search-headline-for-time t)
  (setq org-scheduled-past-days 365)
  (setq org-deadline-past-days 365)
  (setq org-agenda-move-date-from-past-immediately-to-today t)
  (setq org-agenda-show-future-repeats t)
  (setq org-agenda-prefer-last-repeat nil)
  (setq org-agenda-timerange-leaders
        '("" "(%d/%d): "))
  (setq org-agenda-scheduled-leaders
        '("Scheduled: " "Sched.%2dx: "))
  (setq org-agenda-inactive-leader "[")
  (setq org-agenda-deadline-leaders
        '("Deadline: " "In %3d d.: " "%2d d. ago: "))

  ;; Time grid
  (setq org-agenda-time-leading-zero t)
  (setq org-agenda-timegrid-use-ampm nil)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-show-current-time-in-grid t)
  (setq org-agenda-current-time-string
        "Now -·-·-·-·-·-·-")
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (0600 0700 0800 0900 1000 1100
                1200 1300 1400 1500 1600
                1700 1800 1900 2000 2100)
          " ....." "-----------------"))
  (setq org-agenda-default-appointment-duration nil)

  ;; Global todo list
  (setq org-agenda-todo-ignore-with-date t)
  (setq org-agenda-todo-ignore-timestamp t)
  (setq org-agenda-todo-ignore-scheduled t)
  (setq org-agenda-todo-ignore-deadlines t)
  (setq org-agenda-todo-ignore-time-comparison-use-seconds t)
  (setq org-agenda-tags-todo-honor-ignore-options nil)

  ;; Tagged items
  (setq org-agenda-show-inherited-tags t)
  (setq org-agenda-use-tag-inheritance
        '(todo search agenda))
  (setq org-agenda-hide-tags-regexp nil)
  (setq org-agenda-remove-tags nil)
  (setq org-agenda-tags-column -120)

  ;; Agenda entry
  (setq org-agenda-start-with-entry-text-mode nil)
  (setq org-agenda-entry-text-maxlines 5)
  (setq org-agenda-entry-text-exclude-regexps nil)
  (setq org-agenda-entry-text-leaders "    > ")

  ;; Logging, clocking
  (setq org-agenda-log-mode-items '(closed clock))
  (setq org-agenda-clock-consistency-checks
        '((:max-duration "10:00" :min-duration 0 :max-gap "0:05" :gap-ok-around
                         ("4:00")
                         :default-face
                         ((:background "DarkRed")
                          (:foreground "white"))
                         :overlap-face nil :gap-face nil :no-end-time-face nil
                         :long-face nil :short-face nil)))
  (setq org-agenda-log-mode-add-notes t)
  (setq org-agenda-start-with-log-mode nil)
  (setq org-agenda-start-with-clockreport-mode nil)
  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2))
  (setq org-agenda-search-view-always-boolean nil)
  (setq org-agenda-search-view-force-full-words nil)
  (setq org-agenda-search-view-max-outline-level 0)
  (setq org-agenda-search-headline-for-time t)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-cmp-user-defined nil)
  (setq org-sort-agenda-notime-is-late t)
  (setq org-sort-agenda-noeffort-is-high t)

  ;; Agenda column view
  (setq org-agenda-view-columns-initially nil)
  (setq org-agenda-columns-show-summaries t)
  (setq org-agenda-columns-compute-summary-properties t)
  (setq org-agenda-columns-add-appointments-to-effort-sum nil)
  (setq org-agenda-auto-exclude-function nil)
  (setq org-agenda-bulk-custom-functions nil)

  :bind (("C-c a" . org-agenda)
         :map org-mode-map
         ("C-'" . nil)
         ("C-," . nil)))

(use-package org-src
  :after org
  :config
  (setq org-src-window-setup 'current-window)
  (setq org-edit-src-persistent-message nil)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;;; init.el ends here
