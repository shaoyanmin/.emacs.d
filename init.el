;;
;; MELPA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package) 
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives
             ;; '("popkit" . "http://elpa.popkit.org/packages/"))
(package-initialize)
(mapc
 (lambda (package)
   (unless (package-installed-p package)
     (package-install package)))
 '(use-package))


;;
;; Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/elisp/")
(defun load-user-file (file)
  (interactive "f")
  (load-file (expand-file-name (concat file ".el") "~/.emacs.d/elisp/")))
 
(load-user-file "package-config")
(load-user-file "style")
(load-user-file "setting")
(load-user-file "editor")
(load-user-file "keybinding")

(if (and (fboundp 'server-running-p) 
         (not (server-running-p)))
   (server-start))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(custom-safe-themes
   (quote
    ("398f0209bfd642cf7a5e3e03bdc20db2822fd6746225a4bd99ccf9b26d3059d0" default)))
 '(delete-selection-mode 1)
 '(geiser-chicken-load-init-file-p t)
 '(geiser-repl-query-on-exit-p t)
 '(geiser-repl-query-on-kill-p nil)
 '(kill-ring-max 5000)
 '(kill-whole-line t)
 '(package-selected-packages
   (quote
    (use-package web-mode undo-tree textmate smex rainbow-delimiters paredit multiple-cursors markdown-mode magit key-chord helm expand-region doom-themes cycbuf autopair ag ace-jump-mode ace-jump-buffer ac-geiser))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
