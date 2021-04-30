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


(use-package autopair
  :config
  (autopair-global-mode))

(use-package cycbuf
  :config
  (global-set-key (kbd "M-k") 'cycbuf-switch-to-next-buffer)
  (global-set-key (kbd "M-C-k") 'cycbuf-switch-to-previous-buffer))

(use-package key-chord
  :config
  (key-chord-mode t))

(use-package magit)

(use-package multiple-cursors)

(use-package paredit)

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

(use-package web-beautify
  :config
  (eval-after-load 'js2-mode '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
  (eval-after-load 'json-mode '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
  (eval-after-load 'sgml-mode '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
  (eval-after-load 'web-mode '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))
  (eval-after-load 'css-mode '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css)))

(use-package sass-mode
  :mode ("\\.scss" . css-mode)
  :config (add-hook 'sass-mode-hook (lambda () (setq comment-start "//"))))

(use-package js2-mode
  :mode ("\\.js" . js2-mode)
  :bind
  (:map js2-mode-map
        ("M-j" . other-window)
        ("M-<return>" . semicolon-endline)
        ("M-C-<return>" . js2-line-break)))


(use-package json-mode
  :mode ("\\.json" . json-mode)
  :config (add-hook 'json-mode-hook #'flycheck-mode))


(use-package neotree
  :config
  (setq neo-smart-open t)
  ;; (require neotree)
  :bind (:map neotree-mode-map
              ("o" . neotree-previous-line))
  )


;; Others
(ido-mode t)


(setq cycbuf-dont-show-regexp  '("^ "
                                 "^\\*.*\\*$"
                                 "^\\*magit.*$"
                                 ".*Dired .*"
                                 "^\\*cycbuf\\*$"))
