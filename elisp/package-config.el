;; Main
(use-package doom-themes
  :config
  (require 'doom-themes)
  (load-theme 'doom-one t)
  (add-hook 'find-file-hook 'doom-buffer-mode)
  ;; brighter minibuffer when active
  (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
  ;; (require 'doom-neotree)
  )


(use-package helm
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

(use-package auto-complete
  :config
  (global-auto-complete-mode t)
  (require 'auto-complete-config)
  (setq ac-use-fuzzy t)
  (setq ac-ignore-case nil)
  (define-key ac-completing-map (kbd "M-o") 'ac-previous)
  (set-default 'ac-sources
               '(;;ac-source-semantic-raw
                 ;; ac-source-yasnippet
                 ac-source-dictionary
                 ac-source-abbrev
                 ac-source-words-in-buffer
                 ac-source-words-in-same-mode-buffers
                 ac-source-files-in-current-dir
                 ;; ac-source-filename
                 ))
  )


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


;; Development
(use-package ac-geiser)

(use-package geiser)

(use-package rainbow-delimiters)

(use-package markdown-mode)

(use-package emmet-mode)

(use-package web-mode
  :mode ("\\.html" . web-mode) ("\\.ejs" . web-mode) ("\\.vue" . web-mode)
  :config
  (require 'sgml-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
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
  :bind
  (:map js2-mode-map
        ("C-M-l" . web-beautify-js)
        ("C-M-x" . js2-mark-defun)
        ))

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


(use-package tern
  :config
  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  (eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
  )


(use-package neotree
  :config
  (setq neo-smart-open t)
  (global-set-key [f8] 'neotree-toggle)
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


(setq geiser-active-implementations '(chicken))

(add-hook 'scheme-mode-hook '(lambda ()
                               (interactive)
                               (enable-paredit-mode)
                               (rainbow-delimiters-mode-enable)))


(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))
