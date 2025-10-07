(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; placeing all backup files in one directory
(setq temporary-file-directory "~/.emacs.d/temp/")
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix  (concat temporary-file-directory "auto-saves-"))
(setq eshell-directory-name "~/.emacs.d/temp/eshell")
(setq use-dialog-box nil)

;; auto save and resotre desktop sessions
;; (setq desktop-dirname             "~/.emacs.d/temp/desktop/"
;;             desktop-base-file-name      "emacs.desktop"
;;             desktop-base-lock-name      "lock"
;;             desktop-path                (list desktop-dirname)
;;             desktop-save                t
;;             desktop-files-not-to-save   "^$" ;reload tramp paths
;;             desktop-load-locked-desktop nil
;;             desktop-auto-save-timeout   30)
;; (desktop-save-mode 1)

;; dired
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-listing-switches "-alh")
(setq dired-dwim-target t)

;; useful in ido-mode
;; (ido-mode t)
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

(delete-selection-mode t)

(save-place-mode 1)

(toggle-truncate-lines t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; TRAMP
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(setq c-default-style "linux"
      c-basic-offset 4)

;; Keep Warnings in Messages, Don't Pop Up
(setq warning-minimum-log-level :warning
      warning-minimum-level :error)

;; Suppress Native Compilation Warnings
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-warning-on-missing-source nil)

;; Suppress specific warning types
(setq warning-suppress-types
      '((comp)           ; Native compilation warnings
        (bytecomp)       ; Byte compilation warnings
        (obsolete)       ; Obsolete function warnings
        (cl-functions)   ; Common Lisp function warnings
        ))
