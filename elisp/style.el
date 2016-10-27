;;
;; Theme & UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (load-theme 'misterioso)
;; (load-theme 'deeper-blue)

(require 'doom-themes)
(load-theme 'doom-one t)
(add-hook 'find-file-hook 'doom-buffer-mode)
;; brighter minibuffer when active
(add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
(require 'doom-neotree)

(scroll-bar-mode 0)
(menu-bar-mode 1)
(tool-bar-mode -1)

;; Don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

(set-default-font "Source Code Pro")


(column-number-mode)
(tooltip-mode -1)
(show-paren-mode t)

(setq default-indicate-empty-lines t)

(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

(setq-default tab-width 2)
(custom-set-variables '(c-basic-offset 4 t))
(setq-default indent-tabs-mode nil)

