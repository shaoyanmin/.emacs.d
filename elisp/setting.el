;; placeing all backup files in one directory
(setq temporary-file-directory "~/.emacs.d/temp/")
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix  (concat temporary-file-directory "auto-saves-"))
(setq eshell-directory-name "~/.emacs.d/temp/eshell")

;; dired
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-listing-switches "-alh")

;; useful in ido-mode
;; (setq enable-recursive-minibuffers t)

;; hippie-expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(setq fill-column 72)

(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)


(toggle-truncate-lines t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
