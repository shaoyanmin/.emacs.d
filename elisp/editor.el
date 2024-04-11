;; -*- coding: utf-8; lexical-binding: t -*-

;;
;  Here's lisp snippets collected from the others
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; my customize functions
(defun back-window ()
  (interactive)
  (other-window -1))

;; super comment command
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (progn
        (comment-or-uncomment-region (line-beginning-position) (line-end-position))
        (next-line))
    (comment-dwim arg)))

(defvar recently-closed-buffers (cons nil nil)
  "A list of recently closed buffers. The max number to track is controlled by the variable recently-closed-buffers-max.")
(defvar recently-closed-buffers-max 10 "The maximum length for recently-closed-buffers.")

(defun next-user-buffer ()
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
      (setq i (1+ i)) (next-buffer))))

(defun semicolon-endline ()
  "enter semicolon at end of the line"
  (interactive)
  (progn
    (move-end-of-line 1)
    (insert ";")
    ))

(defun previous-user-buffer ()
  "Switch to the previous user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
      (setq i (1+ i)) (previous-buffer) )))

(defun close-current-buffer ()
  "Close the current buffer.
Similar to (kill-buffer (current-buffer)) with the following addition:
• prompt user to save if the buffer has been modified even if the buffer is not associated with a file.
• make sure the buffer shown after closing is a user buffer.
• if the buffer is a file, add the path to the list recently-closed-buffers.
A emacs buffer is one who's name starts with *.
Else it is a user buffer."
  (interactive)
  (let (emacsBuff-p isEmacsBufferAfter)
    (if (or (string-match "^*" (buffer-name))
            (string-match ".*[#@].*" (buffer-name)))
        (setq emacsBuff-p t)
      (setq emacsBuff-p nil))
    ;; offer to save buffers that are non-empty and modified, even for
    ;; non-file visiting buffer. (because kill-buffer does not offer to
    ;; save buffers that are not associated with files)
    (when (and (buffer-modified-p)
               (not emacsBuff-p)
               (not (string-equal major-mode "dired-mode"))
               (if (equal (buffer-file-name) nil)
                   (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                 t))
      (if (y-or-n-p
           (concat "Buffer " (buffer-name) " modified; Do you want to save?"))
          (save-buffer)
        (set-buffer-modified-p nil)))

    ;; save to a list of closed buffer
    (when (not (equal buffer-file-name nil))
      (setq recently-closed-buffers
            (cons (cons (buffer-name) (buffer-file-name)) recently-closed-buffers))
      (when (> (length recently-closed-buffers) recently-closed-buffers-max)
        (setq recently-closed-buffers (butlast recently-closed-buffers 1))
        )
      )

    ;; close
    (progn (kill-buffer (current-buffer))
           (if (> (length (window-list)) 1)
               (delete-window)))

    ;; if emacs buffer, switch to a user buffer
    (if (string-match "^*" (buffer-name))
        (setq isEmacsBufferAfter t)
      (setq isEmacsBufferAfter nil))
    (when isEmacsBufferAfter
      (next-user-buffer))))

(defun open-last-closed ()
  "Open the last closed file."
  (interactive)
  (find-file (cdr (pop recently-closed-buffers)) ) )

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun open-next-line (arg)
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

(defun open-previous-line (arg)
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")
;; Kill
;; ----------------------------------------------------------
(custom-set-variables
 '(kill-ring-max 5000)
 '(kill-whole-line t))

;; note - this should be after volatile-highlights is required
;; add the ability to copy and cut the current line, without marking it
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defun zap-up-to-char (arg char)
  "Kill up to, but not including ARGth occurrence of CHAR.
 Case is ignored if `case-fold-search' is non-nil in the current buffer.
 Goes backward if ARG is negative; error if CHAR not found.
 Ignores CHAR at point."
  (interactive "p\ncZap up to char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (kill-region (point)
                 (progn
                   (forward-char direction)
                   (unwind-protect
                       (search-forward (char-to-string char) nil nil arg)
                     (backward-char direction))
                   (point)))))

(defun fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

;; in honor of xah lee
;; http://ergoemacs.org/emacs/emacs_lookup_ref.html
(defun lookup-word-definition ()
  "Look up the current word's definition in a browser.
If a region is active (a phrase), lookup that phrase."
  (interactive)
  (let (myWord myUrl)
    (setq myWord
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (thing-at-point 'symbol)))

    (setq myWord (replace-regexp-in-string " " "%20" myWord))
    (setq myUrl (concat "http://www.google.com.hk/search?q=" myWord))

    (browse-url myUrl)
    ;; (w3m-browse-url myUrl) ;; if you want to browse using w3m
    ))


(defun ym-function-at-point ()
"Show the elisp function help at current point."
  (interactive)
  (describe-function (intern
                      (thing-at-point 'sexp))))

(defun ym-split-window-horizontally ()
  "open a window vertically and jump to it."
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun ym-split-window-vertically ()
  "open a window vertically and jump to it."
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun ym-go-to-word ()
  "Jump to a word start with a letter that user have inputed."
  (interactive)
  (let* ((char (read-char "Go to Word:"))
         (WORD-REGEXP (concat "\\<" (string char)))
         (prompt (lambda (c)
                   (concat "\"" (upcase (string c)) "\" for backward search ,\""
                           (string c) "\" for forward search: "))))
    (condition-case nil
        (when (search-forward-regexp WORD-REGEXP nil nil 1)
          (backward-char)
          (let ((readed-char (read-event (funcall prompt char))))
            (while (and (characterp readed-char)
                        (or (cond ((equal readed-char char)
                                   (search-forward-regexp WORD-REGEXP nil nil 2))
                                  ((equal (string-to-char (upcase (string char))) readed-char)
                                   (search-backward-regexp WORD-REGEXP nil nil 1))
                                  (t nil))))
              (if (equal char readed-char)
                  (backward-char))
              (setq readed-char (read-event (funcall prompt char)))))
          ;; save last input as command if it's not a character
          (setq unread-command-events (list last-input-event)))
      (error (message "Word start with \"%s\" Not found." (string char))))))


;; Scheme
(defun scheme-proc-exist-p ()
  (and scheme-buffer
       (get-buffer scheme-buffer)
       (comint-check-proc scheme-buffer)))

;; (defun run-remote-scheme ()
;;   (interactive)
;;   (run-scheme "plink.exe -load dev (cd projects; gsi)"))

(defun scheme-safe-exit ()
  (interactive)
  (if (and (scheme-proc-exist-p)
           (equal (buffer-name) scheme-buffer))
      (let ((proc (scheme-get-process)))
        (comint-send-string proc "(exit)\n"))))

;; bypass the interactive question and start the default interpreter
(defun scheme-proc ()
  "Return the current Scheme process, starting one if necessary."
  (unless (scheme-proc-exist-p)
    (save-window-excursion
      (run-scheme (getenv "SCHEME_CMD"))))
  (or (scheme-get-process)
      (error "No current process. See variable `scheme-buffer'")))

(defun switch-other-window-to-buffer (name)
    (other-window 1)
    (switch-to-buffer name)
    (end-of-buffer)
    (other-window 1))

(defun scheme-split-window ()
  (cond
   ;; ((= 1 (count-windows))
   ;;  (split-window-vertically (floor (* 0.68 (window-height))))
   ;;  ;; (split-window-horizontally (floor (* 0.5 (window-width))))
   ;;  (switch-other-window-to-buffer "*scheme*"))
   ((not (member "*scheme*"
                 (mapcar (lambda (w) (buffer-name (window-buffer w)))
                         (window-list))))
    (split-window-vertically (floor (* 0.68 (window-height))))
    (switch-other-window-to-buffer "*scheme*"))))

(defun scheme-send-last-sexp-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-last-sexp))

(defun scheme-send-definition-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-definition))

;; tramp helpers
(defun sudo-edit-current-file ()
  (interactive)
  (let ((my-file-name) ; fill this with the file to open
        (position))    ; if the file is already open save position
    (if (equal major-mode 'dired-mode) ; test if we are in dired-mode
        (progn
          (setq my-file-name (dired-get-file-for-visit))
          (find-alternate-file (prepare-tramp-sudo-string my-file-name)))
      (setq my-file-name (buffer-file-name); hopefully anything else is an already opened file
            position (point))
      (find-alternate-file (prepare-tramp-sudo-string my-file-name))
      (goto-char position))))

(defun prepare-tramp-sudo-string (tempfile)
  (if (file-remote-p tempfile)
      (letrec ((vec  (tramp-dissect-file-name tempfile))
               (user (tramp-file-name-user vec))
               (host (tramp-file-name-host vec)))
        (tramp-make-tramp-file-name
         "sudo"
         ""
         (tramp-file-name-domain vec)
         (tramp-file-name-host vec)
         (tramp-file-name-port vec)
         (tramp-file-name-localname vec)
         ;; Windows Only
         (if user
             (format "plink:%s@%s|" user host)
           (format "plink:%s|" host))))
    (concat "/sudo:root@localhost:" tempfile)))


;; desktop mode
(defun enable-desktop-auto-save ()
  (progn
    ;; auto save and resotre desktop sessions
    (setq desktop-dirname             "~/.emacs.d/temp/desktop/"
          desktop-base-file-name      "emacs.desktop"
          desktop-base-lock-name      "lock"
          desktop-path                (list desktop-dirname)
          desktop-save                t
          desktop-files-not-to-save   "^$" ;reload tramp paths
          desktop-load-locked-desktop nil
          desktop-auto-save-timeout   30)
    (desktop-save-mode 1)))
