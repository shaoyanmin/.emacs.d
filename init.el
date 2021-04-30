(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(mapc
 (lambda (package)
   (unless (package-installed-p package)
     (package-install package)))
 '(
   ace-jump-buffer
   ace-jump-mode
   ag
   autopair
   cycbuf
   doom-themes
   expand-region
   projectile
   flycheck
   helm
   key-chord
   magit
   markdown-mode
   multiple-cursors
   paredit
   rainbow-delimiters
   smart-tabs-mode
   smex
   textmate

   undo-tree
   use-package
   web-mode
   emmet-mode
   js2-mode
   json-mode
   neotree
   ))


(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    (message "Microsoft Windows")))
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    (message "Mac OS X")
    (setenv "PATH"
            (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
            )
    (setq exec-path (append exec-path '("/usr/local/bin")))
    ;; (set-default-font "Monaco 13")
    (setq mac-command-modifier 'control)))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    ;; (set-default-font "Source Code Pro")
    (message "Linux"))))



(add-to-list 'load-path "~/.emacs.d/elisp/")
(defun load-user-file (file)
  (interactive "f")
  (load-file (expand-file-name (concat file ".el") "~/.emacs.d/elisp/")))

(load-user-file "package-config")
(load-user-file "style")
(load-user-file "setting")
(load-user-file "editor")
(load-user-file "keybinding")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4 t)
 '(kill-ring-max 5000)
 '(kill-whole-line t)
 '(package-selected-packages
   '(web-mode web-beautify use-package undo-tree textmate smex smart-tabs-mode sass-mode rainbow-delimiters projectile paredit neotree multiple-cursors markdown-mode magit key-chord json-mode js2-mode helm flycheck expand-region emmet-mode doom-themes cycbuf counsel-world-clock counsel autopair ag ace-jump-mode ace-jump-buffer)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
