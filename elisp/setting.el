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
(ido-mode t)
(setq enable-recursive-minibuffers t)

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

(cond
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    ;; Shell Env
    ;; (setq shell-command-switch "-ic")
    ;; Tree-Sitter
    (setq treesit-language-source-alist
          '((bash "https://github.com/tree-sitter/tree-sitter-bash")
            (cmake "https://github.com/uyha/tree-sitter-cmake")
            (css "https://github.com/tree-sitter/tree-sitter-css")
            (elisp "https://github.com/Wilfred/tree-sitter-elisp")
            ;; (go "https://github.com/tree-sitter/tree-sitter-go")
            ;; (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml")
            (html "https://github.com/tree-sitter/tree-sitter-html")
            (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
            (json "https://github.com/tree-sitter/tree-sitter-json")
            (make "https://github.com/alemuller/tree-sitter-make")
            (markdown "https://github.com/ikatyang/tree-sitter-markdown")
            ;; (python "https://github.com/tree-sitter/tree-sitter-python")
            ;; (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
            (toml "https://github.com/tree-sitter/tree-sitter-toml")
            (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
            (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
            (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

    ;; Run this whenever you need to get the latest version of TS modules
    ;; (mapc #'treesit-install-language-grammar
    ;;       (mapcar #'car treesit-language-source-alist))

    (setq major-mode-remap-alist
          '((yaml-mode . yaml-ts-mode)
            (bash-mode . bash-ts-mode)
            (js2-mode . js-ts-mode)
            (js-mode . js-ts-mode)
            ;; (csharp-mode . csharp-ts-mode)
            (typescript-mode . typescript-ts-mode)
            (json-mode . json-ts-mode)
            (css-mode . css-ts-mode)
            (python-mode . python-ts-mode)))

    (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.sh\\'" . bash-ts-mode))
    ;; (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-ts-mode))

    (message "Apply settings on Linux"))))
