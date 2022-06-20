;;; em-cc.el -*- lexical-binding: t; -*-

;;; Code:

(straight-use-package 'google-c-style)
(straight-use-package 'modern-cpp-font-lock)

;; Auto detect mode for header file
(require 'cc-mode)

;; Ref: https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/cc/autoload.el
(defvar em--+cc-default-header-file-mode 'c++-mode
  "Fallback major mode for header files if all heuristics fail.")

(defun em--+cc--re-search-for (regexp)
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (goto-char (point-min))
        (re-search-forward regexp magic-mode-regexp-match-limit t)))))

(defun em--+cc-c-c++-objc-mode ()
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
               (em--+cc--re-search-for
                (concat "^[ \t\r]*\\(?:"
                        "@\\(?:class\\|interface\\|property\\|end\\)\\_>"
                        "\\|#import +<Foundation/Foundation.h>"
                        "\\|[-+] ([a-zA-Z0-9_]+)"
                        "\\)")))
           (objc-mode))
          ((em--+cc--re-search-for
            (let ((id "[a-zA-Z0-9_]+") (ws "[ \t\r]+") (ws-maybe "[ \t\r]*"))
              (concat "^" ws-maybe "\\(?:"
                      "using" ws "\\(?:namespace" ws "std;\\|std::\\)"
                      "\\|" "namespace" "\\(?:" ws id "\\)?" ws-maybe "{"
                      "\\|" "class"     ws id ws-maybe "[:{\n]"
                      "\\|" "template"  ws-maybe "<.*>"
                      "\\|" "#include"  ws-maybe "<\\(?:string\\|iostream\\|map\\)>"
                      "\\)")))
           (c++-mode))
          ((functionp em--+cc-default-header-file-mode)
           (funcall em--+cc-default-header-file-mode))
          ((c-mode)))))

(add-to-list 'auto-mode-alist '("\\.h\\'" . em--+cc-c-c++-objc-mode))
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

;; Toggle between source and header file
(require 'find-file)
(defmacro em--cc-other-file (name fff &optional doc)
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
(em--cc-other-file
 em--cc-other-file-current
 "find-file"
 "Run `find-file' with other cc file.")
(em--cc-other-file
 em--cc-other-file-other
 "find-file-other-window"
 "Run `find-file-other-window' with other cc file.")

(define-key c++-mode-map (kbd "C-c o") #'em--cc-other-file-current)
(define-key c++-mode-map (kbd "C-c 4 o") #'em--cc-other-file-other)

;; Use Google C++ style
(require 'google-c-style)
(add-hook 'c-mode-common-hook #'google-set-c-style)
(add-hook 'c-mode-common-hook #'google-make-newline-indent)

;; Font lock for modern C++
(require 'modern-cpp-font-lock)
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

(provide 'em-cc)
