;;; em-org.el -*- lexical-binding: t; -*-

;;; Code:

;; Org mode
(require 'org)

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

(define-key org-mode-map (kbd "<C-return>") nil)
(define-key org-mode-map (kbd "<C-S-return>") nil)

(require 'org-capture)

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

(defun em--org-capture-no-delete-windows (oldfun args)
  (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
    (apply oldfun args)))

(advice-add 'org-capture-place-template
            :around 'em--org-capture-no-delete-windows)

(defun em--org-capture-inbox ()
  "Capture directly to inbox."
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "i"))

(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c i") #'em--org-capture-inbox)


(require 'org-agenda)

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

(global-set-key (kbd "C-c a") #'org-agenda)
(define-key org-mode-map (kbd "C-'") nil)
(define-key org-mode-map (kbd "C-,") nil)


(require 'org-src)

(setq org-src-window-setup 'current-window)
(setq org-edit-src-persistent-message nil)
(setq org-src-fontify-natively t)
(setq org-src-preserve-indentation t)
(setq org-src-tab-acts-natively t)
(setq org-edit-src-content-indentation 0)


(provide 'em-org)
