(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabledk
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
  (require 'helm)
  (helm-mode 1)
  (set-face-attribute 'helm-selection nil
                      :background "#51afef"
                      :foreground "black")

  :bind
  (:map helm-map
        ("M-i" . helm-keyboard-quit)
        ("M-n" . helm-next-line)
        ("M-k" . helm-next-line)
        ("M-o" . helm-previous-line)
        ("M-p" . helm-previous-line)
        ("M-C-k" . helm-previous-line)
        ("M-K" . helm-previous-line)
        ("C-p" . helm-previous-line)
        ("C-o" . helm-previous-line)
        ))


(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package undo-tree
  :config
  (global-undo-tree-mode t)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (global-set-key (kbd "C-u") 'undo-tree-redo)
  (global-set-key (kbd "M-u") 'undo-tree-undo))

(use-package expand-region
  :config
  (require 'expand-region)
  (setq shift-select-mode nil)
  (global-set-key (kbd "M-c") 'er/expand-region)
  (global-set-key (kbd "C-M-c") 'er/contract-region))


;; Extensions
(use-package ace-jump-buffer)

(use-package ace-jump-mode
  :config
  (global-set-key (kbd "M-l") 'ace-jump-line-mode)
  (global-set-key (kbd "C-l") 'ace-jump-word-mode))

(use-package ag)

(use-package smartparens
  :config
  (require 'smartparens-config)
  :hook ((text-mode . smartparens-mode)
         (org-mode . smartparens-mode)
         (markdown-mode . smartparens-mode)))

(use-package key-chord
  :config
  (key-chord-mode t))

(use-package magit)

(use-package multiple-cursors
  :no-require t
  :config
  ;; (require 'multiple-cursors)
  (setq mc/always-run-for-all t)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click))

(use-package paredit
  :bind
  (:map paredit-mode-map
        ("M-C-n" . forward-list)
        )
  :hook ((scheme-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)))

(use-package paren-face
  :hook ((scheme-mode . paren-face-mode)
         (emacs-lisp-mode . paren-face-mode)))

(use-package scheme
  :init
  (load-library "cmuscheme")
  (setq scheme-program-name (getenv "SCHEME_CMD"))
  (setq comint-prompt-regexp "^([^>\n]*>+ *)|(#|kawa:[0-9]+|# )")
  :bind
  (:map scheme-mode-map
        ("M-o" . backward-paragraph)
        :map inferior-scheme-mode-map
        ("C-c C-e" . scheme-send-last-sexp-split-window)
        ("C-x C-e" . scheme-send-last-sexp-split-window)
        ("C-c C-r" . scheme-send-definition-split-window)
        )
  :config
  (add-to-list 'auto-mode-alist '("\\.sld\\'" . scheme-mode))
  (add-to-list 'auto-mode-alist '("\\.stub\\'" . scheme-mode))
  (add-hook 'kill-buffer-hook
            (lambda ()
              (scheme-safe-exit)))
  (add-hook 'kill-emacs-hook
            (lambda ()
              (if (scheme-proc-exist-p)
                  (progn (switch-to-buffer scheme-buffer)
                         (scheme-safe-exit)))))
  (add-hook 'scheme-mode-hook
            (lambda ()
              (define-key scheme-mode-map (kbd "<f5>") 'scheme-send-last-sexp-split-window)
              (define-key scheme-mode-map (kbd "<f6>") 'scheme-send-definition-split-window)
              (define-key scheme-mode-map (kbd "C-c C-t") 'scheme-send-region)
              (define-key scheme-mode-map (kbd "C-c C-r") 'scheme-send-definition-split-window)
              (define-key scheme-mode-map (kbd "C-x C-e") 'scheme-send-last-sexp-split-window)
              (define-key scheme-mode-map (kbd "C-c C-e") 'scheme-send-last-sexp-split-window))))

(use-package smex
  :config
  (global-set-key (kbd "M-x") 'smex))

(use-package textmate
  :config
  (global-set-key (kbd "S-<left>") 'textmate-shift-left)
  (global-set-key (kbd "S-<right>") 'textmate-shift-right))

(use-package rainbow-delimiters)

(use-package markdown-mode)

(use-package emmet-mode)

(use-package web-mode
  :mode ("\\.html" . web-mode) ("\\.ejs" . web-mode) ("\\.vue" . web-mode) ("\\.wxml" . web-mode)
  :hook emmet-mode
  :config
  (require 'sgml-mode)
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

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure))

;; Others
(ido-mode t)

(use-package delsel
  :ensure nil
  :init (delete-selection-mode 1))
