(use-package exec-path-from-shell
  :if (string-equal system-type "gnu/linux")
  ;; :init
  ;; (require 'exec-path-from-shell)
  ;; (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
  ;;   (add-to-list 'exec-path-from-shell-variables var))
  :config
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(use-package nerd-icons
   ;; Use M-x nerd-icons-install-fonts to install Symbols Nerd Fonts Mono for you
  )

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabledk
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  :config
  (load-theme 'doom-tokyo-night t)
  (doom-themes-visual-bell-config)
  )

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  ;; Reference: https://seagle0128.github.io/doom-modeline/
  (setq doom-modeline-project-detection 'auto)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project))

(use-package ivy
  :diminish (ivy-mode . "")
  :init (ivy-mode 1) ; globally at startup
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)
  (setq ivy-count-format "%d/%d ")
  ;; :bind (:map ivy-minibuffer-map
  ;;             ("C-p" . ivy-previous-line))
  )

(use-package counsel
  :config
  (setq ivy-use-selectable-prompt t)
  (setq ivy-initial-inputs-alist nil)
  :bind* ; load when pressed
  (("M-x"     . counsel-M-x)
   ;; ("C-s"     . swiper)
   ("C-x C-f" . counsel-find-file)
   ;; ("C-c g"   . counsel-git)      ; search for files in git repo
   ;; ("M-c j"   . counsel-git-grep) ; search for regexp in git repo
   ("M-s M-s"   . counsel-ag)       ; Use ag for regexp
   ("M-i"     . counsel-buffer-or-recentf)
   ("M-k"     . ivy-switch-buffer)
   ;; ("C-x l"   . counsel-locate)
   ("C-x C-f" . counsel-find-file)
   ("C-x b" . counsel-bookmark)
   ("C-x C-b" . counsel-bookmark)
   ;; ("<f1> f"  . counsel-describe-function)
   ;; ("<f1> v"  . counsel-describe-variable)
   ;; ("<f1> l"  . counsel-find-library)
   ;; ("<f2> i"  . counsel-info-lookup-symbol)
   ;; ("<f2> u"  . counsel-unicode-char)
   ("C-c C-r" . ivy-resume)))

;; More: https://github.com/minad/marginalia
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package undo-tree
  :ensure t
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

;; https://github.com/ggreer/the_silver_searcher?tab=readme-ov-file#installing
(use-package ag)

(use-package smartparens
  :config
  (require 'smartparens-config)
  :hook ((text-mode . smartparens-mode)
         (org-mode . smartparens-mode)
         (markdown-mode . smartparens-mode)))

(use-package markdown-mode)

(use-package key-chord
  :config
  (key-chord-mode t))

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

(use-package textmate
  :config
  (global-set-key (kbd "S-<left>") 'textmate-shift-left)
  (global-set-key (kbd "S-<right>") 'textmate-shift-right))

(use-package rainbow-delimiters)

(use-package neotree
  :config
  (setq neo-smart-open t)
  ;; (require neotree)
  :bind (:map neotree-mode-map
              ("o" . neotree-previous-line))
  )

(use-package eglot
  :config
  ;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-to-list 'eglot-server-programs '((ocaml-mode ocaml-ts-mode) "ocamllsp"))
  ;; (add-hook 'c-mode-hook 'eglot-ensure)
  ;; (add-hook 'c++-mode-hook 'eglot-ensure)
  )

;; (use-package clang-format
;;   :ensure t
;;   :bind (:map c-mode-map
;;               ("C-M-l" . clang-format-buffer)))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-completion-system 'ivy)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)
              ("C-c p s" . counsel-projectile-ag)
              ("<f2>" . projectile-find-file)))

(use-package counsel-projectile)

;; (use-package jinja2-mode
;;   :mode ("\\.sls" . jinja2-mode))

(use-package salt-mode
  :mode ("\\.sls" . salt-mode))

;; PDF-tools
(use-package tablist)

;; Prerequisite pacakges:
;;   cd ~/opt
;;   git clone https://github.com/vedang/pdf-tools.git
;;   cd opt/pdf-tools/
;;   ./server/autobuild
(use-package pdf-tools
  :if (and
       (featurep 'tablist)
       (string-equal system-type "gnu/linux"))
  :pin manual
  :load-path "~/opt/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :hook
  (pdf-annot-list-mode . pdf-annot-list-follow-minor-mode)
  :config
  (setq pdf-info-epdfinfo-program "~/opt/pdf-tools/server/epdfinfo")
  (let* ((files   (directory-files "~/opt/pdf-tools/lisp/" nil "\\.el"))
	       (names   (seq-map #'(lambda (s) (string-trim-right s "\\.el")) files))
	       (symbols (seq-map #'intern names)))
    (seq-do #'require symbols))
  (pdf-tools-install :no-query))


;; gptel
(use-package gptel
  :if (string-equal system-type "gnu/linux")
  :straight t
  :config
  (setq gptel-model 'deepseek-chat
        gptel-backend
        (gptel-make-openai "DeepSeek"
          :host "api.deepseek.com"
          :endpoint "/chat/completions"
          :key (lambda () (exec-path-from-shell-copy-env "DEEP_SEEK_TOKEN"))
          :stream t
          :models '(deepseek-chat deepseek-coder)))
  ;; (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (global-set-key (kbd "<f11> s") 'gptel-send)
  (global-set-key (kbd "<f11> r") 'gptel-rewrite)
  :bind (:map gptel-mode-map
              ("<f2>" . gptel-send)))

(let ((plantuml-jar-file-path "~/opt/plantuml/plantuml.jar"))
 (when (file-exists-p plantuml-jar-file-path)
   (use-package plantuml-mode
     :mode "(\\.\\(plantuml?\\|uml\\|puml\\)\\'"
     :config
     (setq plantuml-default-exec-mode 'jar)
     (setq plantuml-jar-path plantuml-jar-file-path)
     (setq plantuml-indent-level 2)
     (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
     (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode)))))
