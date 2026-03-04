;;; egix-test-helpers.el --- Test helpers for magix -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Shared test utilities for magix tests.

;;; Code:

(defvar egix-test-repo-path nil
  "Path to test repository created for tests.")

(defvar egix-test-repo-shared nil
  "Whether the test repo is shared for the test suite.")

(defun egix-test--setup-test-repo ()
  "Create a temporary test repository and return its path."
  (let* ((temp-dir (make-temp-file "egix-test-" t))
         (default-directory temp-dir))
    ;; Initialize git repo
    (shell-command "git init")
    (shell-command "git config user.name \"Test User\"")
    (shell-command "git config user.email \"test@example.com\"")
    
    ;; Create initial commit
    (with-temp-file (expand-file-name "README.md" temp-dir)
      (insert "# Test Repository\n"))
    (shell-command "git add README.md")
    (shell-command "git commit -m \"Initial commit\"")
    
    ;; Create a test branch
    (shell-command "git checkout -b test-branch")
    
    ;; Add another file
    (with-temp-file (expand-file-name "test.txt" temp-dir)
      (insert "Test content\n"))
    (shell-command "git add test.txt")
    (shell-command "git commit -m \"Add test file\"")
    
    ;; Create a modified file
    (with-temp-file (expand-file-name "modified.txt" temp-dir)
      (insert "Modified content\n"))
    (shell-command "git add modified.txt")
    (shell-command "git commit -m \"Add modified file\"")
    (with-temp-file (expand-file-name "modified.txt" temp-dir)
      (insert "Changed content\n"))
    
    ;; Create a subdirectory with a file
    (make-directory (expand-file-name "subdir" temp-dir))
    (with-temp-file (expand-file-name "subdir/nested.txt" temp-dir)
      (insert "Nested file\n"))
    (shell-command "git add subdir/nested.txt")
    (shell-command "git commit -m \"Add nested file\"")
    
    temp-dir))

(defun egix-test--cleanup-test-repo (repo-path)
  "Clean up test repository at REPO-PATH."
  (when (and repo-path (file-exists-p repo-path))
    (delete-directory repo-path t)))

(defun egix-test--suite-setup ()
  "Create a shared test repository for the test suite."
  (unless egix-test-repo-path
    (setq egix-test-repo-path (egix-test--setup-test-repo))
    (setq egix-test-repo-shared t)))

(defun egix-test--suite-teardown ()
  "Clean up the shared test repository if it was created."
  (when (and egix-test-repo-shared egix-test-repo-path)
    (egix-test--cleanup-test-repo egix-test-repo-path)
    (setq egix-test-repo-path nil)
    (setq egix-test-repo-shared nil)))

(defmacro egix-test--with-test-repo (&rest body)
  "Execute BODY with a shared test repository."
  (declare (indent 0))
  `(let* ((created-repo nil)
          (repo-path (or egix-test-repo-path
                         (and (setq created-repo t)
                              (egix-test--setup-test-repo)))))
     (message (format "repo-path: %s" repo-path))
     (let ((egix-test-repo-path repo-path)
           (default-directory repo-path))
       (unwind-protect
           (progn ,@body)
         (when created-repo
           (egix-test--cleanup-test-repo repo-path))))))

(defmacro egix-test--with-fresh-test-repo (&rest body)
  "Execute BODY with a new temporary test repository."
  (declare (indent 0))
  `(let ((egix-test-repo-path (egix-test--setup-test-repo)))
     (unwind-protect
         (let ((default-directory egix-test-repo-path))
           ,@body)
       (egix-test--cleanup-test-repo egix-test-repo-path))))

(provide 'egix-test-helpers)

;;; egix-test-helpers.el ends here

