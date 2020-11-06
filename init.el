;; Proxy for GFW
;; (setq socks-override-functions 1)
;; (setq socks-noproxy '("^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)"))
;; (require 'socks)
;; (setq url-gateway-method 'socks)
;; (setq socks-server '("Default server" "127.0.0.1" 1080 5))


;;
;; MELPA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))


(package-initialize)
(mapc
 (lambda (package)
   (unless (package-installed-p package)
     (package-install package)))
 '(
   ace-jump-buffer
   ace-jump-mode
   ag
   ;; auto-complete
   autopair
   cycbuf
   doom-themes
   emmet-mode
   expand-region
   projectile
   flycheck
   helm
   js2-mode
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
   web-beautify
   web-mode

   sass-mode
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
    (progn
      (server-start)
      (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
      ))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   [("#1B2229" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#DFDFDF")])
 '(c-basic-offset 4)
 '(custom-safe-themes
   (quote
    ("90bd0eb20a1cb155b5a076f698b3c72cfe775aa7ea93b7bfbc171eb250db5e20" "4e21fb654406f11ab2a628c47c1cbe53bab645d32f2c807ee2295436f09103c6" "0f0022c8091326c9894b707df2ae58dd51527b0cf7abcb0a310fb1e7bda78cd2" "f67652440b66223b66a4d3e9c0ddeddbf4a6560182fa38693bdc4d940ce43a2e" "8d737627879eff1bbc7e3ef1e9adc657207d9bf74f9abb6e0e53a6541c5f2e88" "0eef522d30756a80b28333f05c7eed5721f2ba9b3eaaff244ea4c6f6a1b8ac62" "5310b88333fc64c0cb34a27f42fa55ce371438a55f02ac7a4b93519d148bd03d" "398f0209bfd642cf7a5e3e03bdc20db2822fd6746225a4bd99ccf9b26d3059d0" default)))
 '(delete-selection-mode 1)
 '(fci-rule-color "#5B6268")
 '(geiser-chicken-load-init-file-p t)
 '(geiser-repl-query-on-exit-p t)
 '(geiser-repl-query-on-kill-p nil)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(kill-ring-max 5000)
 '(kill-whole-line t)
 '(org-ellipsis "  ")
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (projectile deadgrep ripgrep plantuml-mode web-beautify use-package web-mode undo-tree textmate smex rainbow-delimiters paredit multiple-cursors markdown-mode magit key-chord helm expand-region doom-themes cycbuf autopair ag ace-jump-mode ace-jump-buffer ac-geiser)))
 '(vc-annotate-background "#1B2229")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
