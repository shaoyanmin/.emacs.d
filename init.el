(defun load-user-file (file)
  (interactive "f")
  (load-file (expand-file-name (concat file ".el") "~/.emacs.d/elisp/")))

(add-to-list 'load-path "~/.emacs.d/elisp/")
(load-user-file "env")
(load-user-file "editor")
(load-user-file "setting")

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; https://github.com/radian-software/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(set-language-environment "UTF-8")

(load-user-file "package-config")
(load-user-file "style")
(load-user-file "keybinding")

(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    (message "Microsoft Windows")
    (setq default-directory "~/")
    (set-frame-font "consolas 10")
    ;; (enable-desktop-auto-save)
    ))

 ((string-equal system-type "darwin")   ; Mac OS X
  (progn
    (message "Mac OS X")
    (setenv "PATH"
            (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
            )
    (setq exec-path (append exec-path '("/usr/local/bin")))
    ;; (set-default-font "Monaco 13")
    (setq mac-command-modifier 'control)
    ;; (enable-desktop-auto-save)
    ))

 ((string-equal system-type "gnu/linux") ; linux
   (progn
     ;; (set-frame-font "Source Code Pro 12")
     ;; (add-to-list 'default-frame-alist '(font . "Source Code Pro 12"))
     (set-frame-font "Iosevka 12")
     (add-to-list 'default-frame-alist '(font . "Iosevka 12"))
     (message "Linux"))))
