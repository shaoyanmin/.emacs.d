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


;; https://github.com/minad/vertico
(use-package vertico
  :custom
  (vertico-scroll-margin 2) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode)
  :config
  (setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)
  :bind (:map vertico-map
              ("?" . minibuffer-completion-help)
              ("M-o" . vertico-previous)
              ("M-n" . vertico-next)
              ("TAB" . minibuffer-complete)
              ("M-TAB" . minibuffer-complete)))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("M-k" . consult-buffer)        ;; orig. switch-to-buffer
         ("C-M-k" . consult-recent-file) ;; orig. switch-to-buffer
         ("C-x C-b" . consult-bookmark)  ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(use-package orderless
  :demand t
  :config

  (defun +orderless--consult-suffix ()
    "Regexp which matches the end of string with Consult tofu support."
    (if (boundp 'consult--tofu-regexp)
        (concat consult--tofu-regexp "*\\'")
      "\\'"))

  ;; Recognizes the following patterns:
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-consult-dispatch (word _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--consult-suffix))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--consult-suffix))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  ;; Certain dynamic completion tables (completion-table-dynamic) do not work
  ;; properly with orderless. One can add basic as a fallback.  Basic will only
  ;; be used when orderless fails, which happens only for these special
  ;; tables. Also note that you may want to configure special styles for special
  ;; completion categories, e.g., partial-completion for files.
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;;; Enable partial-completion for files.
        ;;; Either give orderless precedence or partial-completion.
        ;;; Note that completion-category-overrides is not really an override,
        ;;; but rather prepended to the default completion-styles.
        ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
        completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
                                        ;; enable initialism by default for symbols
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism)))
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers (list #'+orderless-consult-dispatch
                                          #'orderless-kwd-dispatch
                                          #'orderless-affix-dispatch)))

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
              ("C-c p s" . consult-git-grep)
              ("<f2>" . projectile-find-file)))

(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))

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
