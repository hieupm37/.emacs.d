;;; early-init.el -*- lexical-binding: t -*-

;;; Code

;; Increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Prefer loading newest compiled .el file
(setq load-prefer-newer t)

;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors 'silent)

  ;; Make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t)

  ;; Set the right directory to store the native compilation cache
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; Initialize installed packages
(setq package-enable-at-startup t)

;; Allow loading from package cache
(setq package-quickstart t)

;; Do not resize the frame at this early stage
(setq frame-inhibit-implied-resize t)

;; Disable some GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-dialog-box t)
(setq use-file-dialog nil)

(setq inhibit-startup-echo-area-message "Power of CREATION!")
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)

;; Start initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Make the initial buffer load faster by setting its mode to fundamental-mode
(setq initial-major-mode 'fundamental-mode)

;; This fixes invoking gcc drive error on OS X
(if (eq system-type 'darwin)
    (setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/11:/usr/local/opt/libgccjit/lib/gcc/11:/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin20/11")
  )

;; Change startup directory on Windows to HOME directory.
(when (eq system-type 'windows-nt)
  (cd "~/")
  (setenv "LANG" "en_US"))

;;; early-init.el ends here
