;; Main
(use-package doom-themes)

(use-package expand-region)

(use-package helm
  :bind
  (:map helm-major-mode-map ("M-i" . helm-keyboard-quit)))

(use-package magit)

(use-package undo-tree
  :config
  (global-undo-tree-mode t))


;; Extensions
(use-package ace-jump-buffer)

(use-package ace-jump-mode)

(use-package ag)

(use-package auto-complete
  :config
  (global-auto-complete-mode t))

(use-package autopair
  :config
  (autopair-global-mode))

(use-package cycbuf)

(use-package key-chord
  :config
  (key-chord-mode t))

(use-package magit)

(use-package multiple-cursors)

(use-package paredit)

(use-package smex)

(use-package textmate)


;; Development
(use-package ac-geiser)

(use-package geiser)

(use-package rainbow-delimiters)

(use-package markdown-mode)

(use-package web-mode)


;; Others
(ido-mode t)


(setq cycbuf-dont-show-regexp  '("^ "
                                 "^\\*.*\\*$"
                                 "^\\*magit.*$"
                                 "^\\*cycbuf\\*$"))




(require 'auto-complete-config)
(setq ac-use-fuzzy t)
(setq ac-ignore-case nil)
(define-key ac-completing-map (kbd "M-o") 'ac-previous)
(set-default 'ac-sources
               '(;;ac-source-semantic-raw
                 ac-source-yasnippet
                 ac-source-dictionary
                 ac-source-abbrev
                 ac-source-words-in-buffer
                 ac-source-words-in-same-mode-buffers
                 ac-source-files-in-current-dir
                 ac-source-filename))


(setq geiser-active-implementations '(chicken))

(add-hook 'scheme-mode-hook '(lambda ()
                               (interactive)
                               (enable-paredit-mode)
                               (rainbow-delimiters-mode-enable)))


(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))
