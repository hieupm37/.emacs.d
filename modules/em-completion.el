;;; em-completion.el -*- lexical-binding: t; -*-

;;; Code:

(straight-use-package 'vertico)
(straight-use-package 'consult)
(straight-use-package 'consult-dir)
(straight-use-package 'orderless)
(straight-use-package 'marginalia)
(straight-use-package 'embark)
(straight-use-package 'embark-consult)

;; Setup orderless
(require 'orderless)

(defun em--orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher using equal sign as a suffix."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

(defun em--orderless-initialism-dispatcher (pattern _index _total)
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
      '(em--orderless-literal-dispatcher
        em--orderless-initialism-dispatcher))

(setq completion-styles '(partial-completion substring flex orderless))

;; Setup vertico
(require 'vertico)
(setq vertico-cycle t)

(defun em--up-directory (arg)
  "Move up a directory (delete backwards to /)."
  (interactive "p")
  (if (string-match-p "/." (minibuffer-contents))
      (zap-up-to-char (- arg) ?/)
    (delete-minibuffer-contents)))

(define-key vertico-map (kbd "C-j") #'vertico-exit-input)
(define-key vertico-map (kbd "<C-backspace>") #'em--up-directory)

(vertico-mode 1)

;; Add more information to completion candidates.
(require 'marginalia)
(setq marginalia-max-relative-age 0)
(marginalia-mode 1)

;; Consulting completing-read
(require 'consult)

(setq consult-narrow-key ">")
(setq completion-in-region-function #'consult-completion-in-region)
;; Use completing-read-multiple provided by consult
(advice-add #'completing-read-multiple
            :override #'consult-completing-read-multiple)

(defun em--consult-project-root ()
  "Returns patht to project or `default-directory'."
  (or (vc-root-dir)
      (locate-dominating-file "." ".git")
      default-directory))
(setq consult-project-root-function #'em--consult-project-root)

;; Using consult's complex-command instead of builtin
(global-set-key [remap repeat-complex-command] #'consult-complex-command)

(global-set-key (kbd "C-x b") #'consult-buffer)
(global-set-key (kbd "C-x 4 b") #'consult-buffer-other-window)
(global-set-key (kbd "C-x 5 b") #'consult-buffer-other-frame)
(global-set-key (kbd "C-y") #'yank)
(global-set-key (kbd "M-y") #'consult-yank-pop)
(global-set-key (kbd "<help> a") #'consult-apropos)

(global-set-key (kbd "M-g e") #'consult-compile-error)
(global-set-key (kbd "M-g g") #'consult-goto-line)
(global-set-key (kbd "M-g M-g") #'consult-goto-line)
(global-set-key (kbd "M-g o") #'consult-outline)
(global-set-key (kbd "M-g m") #'consult-mark)
(global-set-key (kbd "M-g M") #'consult-global-mark)
(global-set-key (kbd "M-g i") #'consult-imenu)
(global-set-key (kbd "M-g M-i") #'consult-imenu)
(global-set-key (kbd "M-g I") #'consult-project-imenu)

(global-set-key (kbd "M-s f") #'consult-find)
(global-set-key (kbd "M-s L") #'consult-locate)
(global-set-key (kbd "M-s g") #'consult-git-grep)
(global-set-key (kbd "M-s G") #'consult-grep)
(global-set-key (kbd "M-s r") #'consult-ripgrep)
(global-set-key (kbd "M-s l") #'consult-line)
(global-set-key (kbd "M-s m") #'consult-multi-occur)
(global-set-key (kbd "M-s k") #'consult-keep-lines)
(global-set-key (kbd "M-s u") #'consult-focus-lines)
(global-set-key (kbd "M-s e") #'consult-isearch-history)
(define-key isearch-mode-map (kbd "M-s e") #'consult-isearch-history)

;; Insert directory paths into minibuffer prompts.
(require 'consult-dir)
(global-set-key (kbd "C-x C-d") #'consult-dir)
(define-key vertico-map (kbd "C-x C-d") #'consult-dir)

;; Tie objects - actions with embark
;; Thanks to https://karthinks.com/software/fifteen-ways-to-use-embark/
(require 'embark)
(require 'embark-consult)

;; Optionally replace the key help with a completing-read interface
(setq prefix-help-command #'embark-prefix-help-command)

;; Hide the mode line of the Embark live/completions buffers
(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

(global-set-key (kbd "C-,") #'embark-act)
(define-key vertico-map (kbd "C-M-,") #'embark-export)

  ;; (use-package ace-window
  ;;   :config
  ;;   (defmacro my/embark-ace-action (fn)
  ;;     `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
  ;;        (interactive)
  ;;        (with-demoted-errors "%s"
  ;;          ;; (require 'ace-window)
  ;;          (let ((aw-dispatch-always t))
  ;;            (aw-switch-to-window (aw-select nil))
  ;;            (call-interactively (symbol-function ',fn))))))
  ;;
  ;;   (my/embark-ace-action find-file)
  ;;   (my/embark-ace-action switch-to-buffer)
  ;;   (my/embark-ace-action bookmark-jump)
  ;;
  ;;   :bind (:map embark-file-map
  ;;               ("o" . my/embark-ace-find-file)
  ;;               :map embark-buffer-map
  ;;               ("o" . my/embark-ace-switch-to-buffer)
  ;;               :map embark-bookmark-map
  ;;               ("o" . my/embark-ace-bookmark-jump)))

;; Dynamic word completions
(require 'dabbrev)
(setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
(setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
(setq dabbrev-backward-only nil)
(setq dabbrev-case-distinction 'case-replace)
(setq dabbrev-case-fold-search nil)
(setq dabbrev-case-replace 'case-replace)
(setq dabbrev-check-other-buffers t)
(setq dabbrev-eliminate-newlines t)
(setq dabbrev-upcase-means-case-search t)
(global-set-key (kbd "C-M-/") #'dabbrev-completion)

(require 'hippie-exp)
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
(global-set-key (kbd "M-/") #'hippie-expand)

;; Completion for recent files and directories
(require 'recentf)
(setq recentf-save-file (locate-user-emacs-file "recentf"))
(setq recentf-max-saved-items 200)
(setq recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))

(defun em--recentf-keep-predicate (file)
  "Additional conditions for saving FILE in `recentf-list'."
  (cond
   ((file-directory-p file) (file-readable-p file))))

(add-to-list 'recentf-keep #'em--recentf-keep-predicate)

(defun em--recentf-select-files ()
  "Select item from `recentf-list' using completion."
  (interactive)
  (let* ((files (mapcar 'abbreviate-file-name recentf-list))
         (f (completing-read "Recent file: " files nil t)))
    (find-file f)))

(add-hook 'after-init-hook #'recentf-mode)
(global-set-key (kbd "C-x C-r") #'em--recentf-select-files)


(provide 'em-completion)
