;;; em-elfeed.el -*- lexical-binding: t; -*-

;;; Code:

(straight-use-package 'elfeed)

(require 'elfeed)

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

(defvar em--saved-window-configuration nil
  "Current window configuration before opening elfeed search.")

(defun em--elfeed-dwim ()
  "Open elfeed search buffer in maximized window."
  (interactive)
  (if (one-window-p)
      (elfeed)
    (setq em--saved-window-configuration (current-window-configuration))
    (delete-other-windows)
    (elfeed)))

(defun em--elfeed-search-quit-dwim ()
  "Close search buffer, restore saved window configuration."
  (interactive)
  (elfeed-search-quit-window)
  (when em--saved-window-configuration
    (set-window-configuration em--saved-window-configuration)))

(defun em--elfeed-entry-quit-dwim ()
  "Kill entry buffer and switch back to *elfeed-search* buffer."
  (interactive)
  (unless (one-window-p)
    (delete-other-windows))
  (elfeed-kill-buffer)
  (switch-to-buffer "*elfeed-search*"))

(global-set-key (kbd "C-x w") #'em--elfeed-dwim)

(define-key elfeed-search-mode-map (kbd "q") #'em--elfeed-search-quit-dwim)
(define-key elfeed-show-mode-map (kbd "q") #'em--elfeed-entry-quit-dwim)

(provide 'em-elfeed)
