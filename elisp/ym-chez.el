;;; ym-chez.el --- Jump between Chez Scheme source and test files -*- lexical-binding: t; -*-
(require 'projectile)

;; Chez Scheme Unit Test File Convention
;;
;;   lib/chez-http/server/core.ss -> lib/tests/unit/lib/chez-http/server/core.ss

(defconst ym-chez--source-pattern "^lib/.*\\.ss\\'"
  "Regex pattern matching Chez Scheme source files.")

(defconst ym-chez--test-pattern "^lib/tests/unit/.*\\.ss\\'"
  "Regex pattern matching Chez Scheme unit test files.")

(defconst ym-chez--source-prefix "lib/"
  "Path prefix for Chez Scheme source files.")

(defconst ym-chez--test-prefix "lib/tests/unit/"
  "Path prefix for Chez Scheme test files.")

(defun ym-chez--project-relative-path ()
  "Return the current file path relative to the project root."
  (let ((file (buffer-file-name)))
    (when file
      (file-relative-name file (projectile-project-root)))))

(defun ym-chez--make-related-path (relative-path direction)
  "Compute related file path given RELATIVE-PATH and DIRECTION.
DIRECTION should be either 'source->test or 'test->source."
  (pcase direction
    ('source->test
     (replace-regexp-in-string ym-chez--source-prefix ym-chez--test-prefix relative-path))
    ('test->source
     (replace-regexp-in-string ym-chez--test-prefix ym-chez--source-prefix relative-path))
    (_ (error "Invalid direction: %s" direction))))

(defun ym-chez--open-or-create-file (full-path create-message)
  "Open FULL-PATH if it exists. If not, prompt to create it with CREATE-MESSAGE."
  (if (file-exists-p full-path)
      (progn
        (message "Opening: %s" full-path)
        (find-file full-path))
    (if (yes-or-no-p (format "%s\n  %s? " create-message full-path))
        (progn
          (let ((dir (file-name-directory full-path)))
            (unless (file-exists-p dir)
              (when (yes-or-no-p (format "Directory does not exist:\n  %s\nCreate it? " dir))
                (make-directory dir t)
                (message "Created directory: %s" dir))))
          (message "Creating new file: %s" full-path)
          (find-file full-path))
      (message "Cancelled. Staying in current file."))))

(defun ym-chez--jump (direction)
  "Core logic for jumping between source and test files.
DIRECTION is either 'source->test or 'test->source."
  (let* ((relative-path (ym-chez--project-relative-path))
         (project-root (projectile-project-root))
         (related-relative-path (ym-chez--make-related-path relative-path direction))
         (related-full-path (expand-file-name related-relative-path project-root)))
    (ym-chez--open-or-create-file
     related-full-path
     (if (eq direction 'source->test)
         "Test file does not exist. Create it?"
       "Source file does not exist. Create it?"))))

;;;###autoload
(defun ym-chez-jump-between-source-and-test ()
  "Jump between Chez Scheme source and unit test files.
If current file is a source file, jump to its test file (create if missing).
If it's a test file, jump back to the source file."
  (interactive)
  (let ((relative-path (ym-chez--project-relative-path))
        (current-file (buffer-file-name)))
    (cond
     ;; No file associated with buffer
     ((not current-file)
      (message "Warning: Current buffer is not visiting a file.")
      (ding))

     ;; Source → Test
     ((and (string-match-p ym-chez--source-pattern relative-path)
           (not (string-match-p "^lib/tests/" relative-path)))
      (ym-chez--jump 'source->test))

     ;; Test → Source
     ((string-match-p ym-chez--test-pattern relative-path)
      (ym-chez--jump 'test->source))

     ;; Invalid file
     (t
      (message "Warning: File does not match recognized Chez Scheme source/test patterns:\n  %s\n  Expected: lib/**/*.ss or lib/tests/unit/**/*.ss"
               current-file)
      (ding)))))

(provide 'ym-chez)
