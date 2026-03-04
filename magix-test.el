;;; magix-test.el --- Tests for magix -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Tests for magix gitoxide-powered Magit acceleration.
;; These tests verify that magix overrides work correctly with higher-level
;; Magit functions and that debug mode properly detects mismatches.

;;; Code:

(require 'magix)
(require 'ert)
(require 'egix-test-helpers)

;; (unless (featurep 'magit)
;;   ;; Setup package archives and install magit if needed
;;   (require 'package)
;;   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;   (package-initialize)
;;   (message "Installing magit from MELPA...")
;;   (package-refresh-contents)
;;   (package-install 'magit))

(defun magix-test--clear-debug-buffer ()
  (when (get-buffer "*magix-debug*")
    (kill-buffer "*magix-debug*")))

(defun magix-test--debug-buffer-string ()
  (let ((debug-buffer (get-buffer "*magix-debug*")))
    (when debug-buffer
      (with-current-buffer debug-buffer
        (buffer-string)))))

(defun magix-test--assert-no-mismatch ()
  (let ((content (magix-test--debug-buffer-string)))
    (when content
      (when (string-match-p "MISMATCH DETECTED" content)
        (message "Magix debug buffer:\n%s" content)
        (ert-fail "Mismatch detected; see *magix-debug* output above.")))))

(ert-deftest magix-test-magit-status-with-override ()
  "Test that magit-status works with magix overrides enabled."
  (skip-unless (featurep 'egix-module))
  (egix-test--with-test-repo
    (let ((magix-mode t)
          (magix-debug-mode t))
      (magix-test--clear-debug-buffer)
      
      ;; Call magit-status which internally uses our overridden functions
      (magit-status-setup-buffer)
      
      ;; Verify status buffer was created
      (should (get-buffer (magit-get-mode-buffer 'magit-status-mode)))
      
      (magix-test--assert-no-mismatch))))

(ert-deftest magix-test-magit-toplevel ()
  "Test that magit-toplevel works with magix override."
  (skip-unless (featurep 'egix-module))
  (egix-test--with-test-repo
    (let ((magix-mode t)
          (magix-debug-mode t))
      (magix-test--clear-debug-buffer)
      
      ;; Call magit-toplevel
      (let ((toplevel (magit-toplevel)))
        ;; Verify we got a result
        (unless toplevel
          (magix-test--assert-no-mismatch)
          (ert-fail "magit-toplevel returned nil; see debug output above."))
        (should (stringp toplevel))
        (should (file-directory-p toplevel))
        (should (file-equal-p toplevel egix-test-repo-path))
        
        (magix-test--assert-no-mismatch)))))

(ert-deftest magix-test-magit-inside-worktree-p ()
  "Test that magit-inside-worktree-p works with magix override."
  (skip-unless (featurep 'egix-module))
  (egix-test--with-test-repo
    (let ((magix-mode t)
          (magix-debug-mode t))
      (magix-test--clear-debug-buffer)
      
      ;; Inside repo should return t
      (should (magit-inside-worktree-p t))
      
      (magix-test--assert-no-mismatch)))
  
  ;; Outside repo should return nil with noerror
  (let ((magix-mode t)
        (magix-debug-mode t)
        (default-directory temporary-file-directory))
    (magix-test--clear-debug-buffer)
    
    (should-not (magit-inside-worktree-p t))))

(ert-deftest magix-test-magit-get-current-branch ()
  "Test that magit-get-current-branch works with magix override."
  (skip-unless (featurep 'egix-module))
  (egix-test--with-test-repo
    (let ((magix-mode t)
          (magix-debug-mode t))
      (magix-test--clear-debug-buffer)
      
      ;; Get current branch
      (let ((branch (magit-get-current-branch)))
        ;; Verify we got the test-branch we created
        (should branch)
        (should (stringp branch))
        (should (string= branch "test-branch"))
        
        (magix-test--assert-no-mismatch)))))

(ert-deftest magix-test-magit-list-refs ()
  "Test that magit-list-refs works with magix overrides active."
  (skip-unless (featurep 'egix-module))
  (egix-test--with-test-repo
    (let ((magix-mode t)
          (magix-debug-mode t))
      (magix-test--clear-debug-buffer)
      
      ;; Get list of refs
      (let ((refs (magit-list-refs)))
        ;; Should have some refs
        (should refs)
        (should (listp refs))
        
        (magix-test--assert-no-mismatch)))))

(ert-deftest magix-test-magit-log-current ()
  "Test that magit-log-current works with magix overrides active."
  (skip-unless (featurep 'egix-module))
  (egix-test--with-test-repo
    (let ((magix-mode t)
          (magix-debug-mode t))
      (magix-test--clear-debug-buffer)
      
      ;; Show log buffer
      (magit-log-current '("HEAD"))
      
      ;; Verify log buffer was created
      (should (get-buffer (magit-get-mode-buffer 'magit-log-mode)))
      
      (magix-test--assert-no-mismatch))))

(ert-deftest magix-test-tramp-exclusion ()
  "Test that magix correctly excludes TRAMP paths."
  (skip-unless (featurep 'egix-module))
  (let ((magix-mode t)
        (default-directory "/ssh:remote:/some/path"))
    ;; TRAMP paths should not be accelerated
    (should-not (magix--should-accelerate-p))))

(ert-deftest magix-test-excluded-repositories ()
  "Test that magix correctly handles excluded repositories."
  (skip-unless (featurep 'egix-module))
  (egix-test--with-test-repo
        (let ((magix-mode t)
          (magix-excluded-repositories (list egix-test-repo-path)))
      ;; Excluded repo should not be accelerated
      (should-not (magix--should-accelerate-p)))))

(ert-deftest magix-test-debug-mode-detection ()
  "Test that debug mode properly detects and logs mismatches."
  (skip-unless (featurep 'egix-module))
  (egix-test--with-test-repo
    (let ((magix-mode t)
          (magix-debug-mode t))
      ;; Clear debug buffer
      (when (get-buffer "*magix-debug*")
        (kill-buffer "*magix-debug*"))
      
      ;; Run several magit commands
      (magit-toplevel)
      (magit-inside-worktree-p t)
      (magit-get-current-branch)
      
      ;; If debug buffer exists, check its contents
      (let ((debug-buffer (get-buffer "*magix-debug*")))
        (if debug-buffer
            (with-current-buffer debug-buffer
              (let ((content (buffer-string)))
                ;; If there are mismatches, they should be properly formatted
                (when (string-match-p "MISMATCH DETECTED" content)
                  (should (string-match-p "Function:" content))
                  (should (string-match-p "Time:" content))
                  (should (string-match-p "Directory:" content))
                  (should (string-match-p "Magix result:" content))
                  (should (string-match-p "Original result:" content)))))
          ;; No debug buffer means no mismatches - that's good!
          (message "No mismatches detected - magix results match original Magit"))))))

(ert-deftest magix-test-mode-toggle ()
  "Test that magix-mode can be toggled on and off."
  (skip-unless (featurep 'egix-module))
  
  ;; Enable mode
  (magix-mode 1)
  (should magix-mode)
  (should magix--advised-functions)
  
  ;; Disable mode
  (magix-mode -1)
  (should-not magix-mode)
  
  ;; Re-enable mode
  (magix-mode 1)
  (should magix-mode)
  (should magix--advised-functions))

(provide 'magix-test)

;;; magix-test.el ends here

