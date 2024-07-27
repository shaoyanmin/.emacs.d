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

(add-to-list 'load-path "~/.emacs.d/elisp/")
(defun load-user-file (file)
  (interactive "f")
  (load-file (expand-file-name (concat file ".el") "~/.emacs.d/elisp/")))

(set-language-environment "UTF-8")

(load-user-file "editor")
(load-user-file "package-config")
(load-user-file "style")
(load-user-file "setting")
(load-user-file "keybinding")

(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    (message "Microsoft Windows")
    (setq default-directory "~/")
    (set-frame-font "consolas 10")
    (enable-desktop-auto-save)
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
    (enable-desktop-auto-save)))

 ((string-equal system-type "gnu/linux") ; linux
   (progn
     (set-frame-font "Source Code Pro 12")
     (add-to-list 'default-frame-alist '(font . "Source Code Pro 12"))
     (message "Linux"))))
