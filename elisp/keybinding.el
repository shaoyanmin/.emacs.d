;;; unset these keys
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "<f1>"))
(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "<f11>"))
(global-unset-key (kbd "M-i"))
(global-set-key (kbd "<f1>") 'save-buffer)
(global-set-key (kbd "<f8>") 'neotree-toggle)
(global-set-key (kbd "<f9>") 'neotree-find)
(global-set-key (kbd "<f12>") 'xah-open-in-terminal)
(global-unset-key (kbd "C-<down-mouse-1>"))

;;;; gpt
(define-prefix-command 'gpt-prefix-map)
(global-set-key (kbd "M-i") 'gpt-prefix-map)
(define-key gpt-prefix-map (kbd "M-i") 'gptel-send)
(define-key gpt-prefix-map (kbd "M-j") 'gptel-rewrite)
(define-key gpt-prefix-map (kbd "M-k") 'gptel-abort)
(define-key gpt-prefix-map (kbd "M-o") 'gptel)


;;;; quick move or selection
(global-set-key (kbd "M-SPC") 'set-mark-command)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-o") 'backward-paragraph)
(global-set-key (kbd "C-o") 'previous-line)
(global-set-key (kbd "C-M-o") 'backward-list)
(global-set-key (kbd "M-H") 'mark-whole-buffer)

(global-set-key (kbd "C-x f") 'dired-jump)


;;; quick edit

(global-set-key "\M-h" 'comment-dwim-line)
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-.") 'open-previous-line)
(global-set-key (kbd "C-,") 'open-next-line)

(global-set-key (kbd "C-!") 'sudo-edit-current-file)

(global-set-key (kbd "M-U") 'upcase-word)
(global-set-key (kbd "M-D") 'downcase-word)
(global-set-key (kbd "M-C") 'capitalize-word)

(global-set-key (kbd "M-I") 'delete-horizontal-space)
(global-set-key (kbd "M-J") 'delete-indentation)
(global-set-key (kbd "M-T") 'transpose-chars)

(global-set-key (kbd "M-t") 'zap-up-to-char)
(global-set-key (kbd "C-t") 'ym-go-to-word)

(global-set-key (kbd "S-<down>") 'move-text-down)
(global-set-key (kbd "S-<up>") 'move-text-up)
(global-set-key (kbd "M-S-<down>") 'move-text-down)
(global-set-key (kbd "M-S-<up>") 'move-text-up)


;;; switch between buffers and files

(global-set-key (kbd "M-j") 'other-window)
(global-set-key (kbd "M-C-j") 'back-window)


;;; search files by silver searcher (alternate tool of grep and ack)

(global-set-key (kbd "M-s M-f") 'ag-project-at-point)
(global-set-key (kbd "M-s M-r") 'ag-regexp-project-at-point)

;;; window  or buffer

(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'ym-split-window-horizontally)
(global-set-key (kbd "C-3") 'ym-split-window-vertically)
(global-set-key (kbd "C-4") 'delete-window)
(global-set-key (kbd "C-5") 'close-current-buffer)
(global-set-key (kbd "C-6") 'open-last-closed)
(global-set-key (kbd "C-7") (lambda ()
                              (interactive)
                              (move-to-window-line-top-bottom 1)))
(global-set-key (kbd "C-8") 'recenter-top-bottom)
(global-set-key (kbd "C-9") (lambda ()
                              (interactive)
                              (move-to-window-line-top-bottom -1)))


(global-set-key (kbd "M-g o") 'previous-error)


;;; function keys

;; (global-set-key [f10] 'lookup-word-definition)
;; (global-set-key [f11] 'eshell)
;; (global-set-key [f12] '(lambda () (interactive)
;;                         (speedbar-change-initial-expansion-list "quick buffers")
;;                         (sr-speedbar-toggle)))
;;


;;; Stuff I had some trouble defining normally
(add-hook 'dired-mode-hook (lambda ()
  ;; (define-key dired-mode-map "f1" 'magit-status)
  (define-key dired-mode-map "U" 'dired-up-directory)
  (define-key dired-mode-map "o" 'dired-previous-line)
  (define-key dired-mode-map "/" 'dired-isearch-filenames)))


;;; Enhance the `ESC` function

(global-set-key [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)


;; KEY-CHORD - DOUBLE KEY COMBO
(key-chord-define-global "xx" '(lambda () (interactive)
                                 (exchange-point-and-mark)
                                 (deactivate-mark)))
(key-chord-define-global "yy" '(lambda () (interactive)
                                 (save-excursion
                                   (copy-region-as-kill
                                    (line-beginning-position)
                                    (if (= (forward-line 1) 0)
                                        (point)
                                      (progn (newline) (point))))
                                   (yank))
                                 (next-line)))
