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

;; Start Emacs server when done
(my/builtin-package 'server
  (add-hook 'after-init-hook #'server-start))

;; Make GC pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
