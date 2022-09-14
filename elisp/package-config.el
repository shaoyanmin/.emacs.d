(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)

  ;; (add-hook 'find-file-hook 'doom-buffer-mode)
  ;; (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
  ;; (require 'doom-neotree)
  )


(use-package helm
  :config
  (set-face-attribute 'helm-selection nil
                      :background "#51afef"
                      :foreground "black")

  :bind
  (:map helm-map
        ("M-i" . helm-keyboard-quit)
        ("M-n" . helm-next-line)
        ("M-o" . helm-previous-line)
        ("M-p" . helm-previous-line)
        ("C-p" . helm-previous-line)
        ("C-o" . helm-previous-line)
        ))


(use-package magit)

(use-package undo-tree
  :config
  (global-undo-tree-mode t))

(use-package expand-region
  :config
  (require 'expand-region)
  (setq shift-select-mode nil))


;; Extensions
(use-package ace-jump-buffer)

(use-package ace-jump-mode)

(use-package ag)

(use-package smartparens
  :config
  (require 'smartparens-config)
  (add-hook 'text-mode-hook 'smartparens-mode)
  (add-hook 'org-mode-hook 'smartparens-mode)
  (add-hook 'markdown-mode-hook 'smartparens-mode))


(use-package cycbuf
  :config
  (setq cycbuf-dont-show-regexp  '("^ "
                                   "^\\*.*\\*$"
                                   "^\\*magit.*$"
                                   ".*Dired .*"
                                   "^\\*cycbuf\\*$"))
  (global-set-key (kbd "M-k") 'cycbuf-switch-to-next-buffer)
  (global-set-key (kbd "M-C-k") 'cycbuf-switch-to-previous-buffer))

(use-package key-chord
  :config
  (key-chord-mode t))

(use-package magit)

(use-package multiple-cursors
  :config
  (setq mc/always-run-for-all t))

(use-package paredit
  :config
  (add-hook 'scheme-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package paren-face
  :config
  (add-hook 'scheme-mode-hook 'paren-face-mode)
  (add-hook 'emacs-lisp-mode-hook 'paren-face-mode))

(use-package smex)

(use-package textmate)

(use-package rainbow-delimiters)

(use-package markdown-mode)

(use-package emmet-mode)

(use-package web-mode
  :mode ("\\.html" . web-mode) ("\\.ejs" . web-mode) ("\\.vue" . web-mode) ("\\.wxml" . web-mode)
  :config
  (require 'sgml-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  (setq web-mode-markup-indent-offset 2)
  :bind
  (:map web-mode-map
        ("C-M-f" . web-mode-tag-next)
        ("C-M-b" . web-mode-tag-previous)
        ("C-M-e" . web-mode-element-end)
        ("C-M-a" . web-mode-element-beginning)
        ("C-M-k" . web-mode-element-kill)
        ("C-M-n" . sgml-skip-tag-forward)
        ("C-M-p" . sgml-skip-tag-backward)
        ("C-M-o" . sgml-skip-tag-backward)
        ("C-M-u" . web-mode-element-parent)
        ("C-M-d" . web-mode-element-child)
        ("<return>" . newline-and-indent)
        ))

(use-package neotree
  :config
  (setq neo-smart-open t)
  ;; (require neotree)
  :bind (:map neotree-mode-map
              ("o" . neotree-previous-line))
  )


;; Others
(ido-mode t)
