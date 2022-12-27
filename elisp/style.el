(scroll-bar-mode 0)
(menu-bar-mode 1)
(tool-bar-mode -1)

;; Don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

(column-number-mode)
(tooltip-mode -1)
(show-paren-mode t)

(setq default-indicate-empty-lines t)
;; (global-display-line-numbers-mode)

(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

(setq-default tab-width 2)
(custom-set-variables '(c-basic-offset 4 t))
(setq-default indent-tabs-mode nil)
