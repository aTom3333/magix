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
  (should magix-mode)
  (egix-test--with-test-repo
    (let ((magix-debug-mode t))
      (magix-test--clear-debug-buffer)
      
      ;; Call magit-status which internally uses our overridden functions
      (magit-status-setup-buffer)
      
      ;; Verify status buffer was created
      (should (get-buffer (magit-get-mode-buffer 'magit-status-mode)))
      
      (magix-test--assert-no-mismatch))))

(ert-deftest magix-test-magit-toplevel ()
  "Test that magit-toplevel works with magix override."
  (skip-unless (featurep 'egix-module))
  (should magix-mode)
  (egix-test--with-test-repo
    (let ((magix-debug-mode t))
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
  (should magix-mode)
  (egix-test--with-test-repo
    (let ((magix-debug-mode t))
      (magix-test--clear-debug-buffer)
      
      ;; Inside repo should return t
      (should (magit-inside-worktree-p t))
      
      (magix-test--assert-no-mismatch)))
  
  ;; Outside repo should return nil with noerror
  (let ((magix-debug-mode t)
        (default-directory temporary-file-directory))
    (magix-test--clear-debug-buffer)
    
    (should-not (magit-inside-worktree-p t))))

(ert-deftest magix-test-magit-bare-repo-p ()
  "Test `magit-bare-repo-p' with the magix --is-bare-repository override.
Includes the edge cases where `core.bare' disagrees with the repo layout."
  (skip-unless (featurep 'egix-module))
  (should magix-mode)
  ;; Dispatcher must intercept the common non-bare-layout case.
  (egix-test--with-test-repo
    (let ((magix-debug-mode t))
      (magix-test--clear-debug-buffer)
      (should (equal (magix--git-output-dispatch '("rev-parse" "--is-bare-repository"))
                     '("false\n")))
      ;; non-bare layout: default, then core.bare=true, then core.bare=false
      (magit-bare-repo-p)
      (shell-command "git config core.bare true")
      (magit-bare-repo-p)
      (shell-command "git config core.bare false")
      (magit-bare-repo-p)
      (magix-test--assert-no-mismatch)))
  ;; bare layout: magix falls through, debug-mode still cross-checks
  (let ((bare-dir (make-temp-file "magix-bare-edge-" t))
        (magix-debug-mode t))
    (unwind-protect
        (let ((default-directory (file-name-as-directory bare-dir)))
          (shell-command "git init -q --bare")
          (magix-test--clear-debug-buffer)
          (magit-bare-repo-p)
          (shell-command "git config core.bare false")
          (magit-bare-repo-p)
          (magix-test--assert-no-mismatch))
      (delete-directory bare-dir t))))

(ert-deftest magix-test-magit-get-current-branch ()
  "Test that magit-get-current-branch works with magix override."
  (skip-unless (featurep 'egix-module))
  (should magix-mode)
  (egix-test--with-test-repo
    (let ((magix-debug-mode t))
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
  (should magix-mode)
  (egix-test--with-test-repo
    (let ((magix-debug-mode t))
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
  (should magix-mode)
  (egix-test--with-test-repo
    (let ((magix-debug-mode t))
      (magix-test--clear-debug-buffer)
      
      ;; Show log buffer
      (magit-log-current '("HEAD"))
      
      ;; Verify log buffer was created
      (should (get-buffer (magit-get-mode-buffer 'magit-log-mode)))
      
      (magix-test--assert-no-mismatch))))

(ert-deftest magix-test-magit-abbrev-length ()
  "Test that `magit-abbrev-length' works with the magix --short override.
`magit-abbrev-length' runs `rev-parse --short' against HEAD and HEAD~,
exercising the dispatcher's `(\"rev-parse\" \"--short\" REF)' arm."
  (skip-unless (featurep 'egix-module))
  (should magix-mode)
  (egix-test--with-test-repo
    (let ((magix-debug-mode t))
      (magix-test--clear-debug-buffer)
      (magit-abbrev-length)
      (magix-test--assert-no-mismatch))))

(ert-deftest magix-test-magit-ref-abbrev ()
  "Test that `magit-ref-abbrev' works with the magix --verify --abbrev-ref
override (existing branch, fully-qualified ref, and a non-existent name)."
  (skip-unless (featurep 'egix-module))
  (should magix-mode)
  (egix-test--with-test-repo
    (let ((magix-debug-mode t))
      (magix-test--clear-debug-buffer)
      (magit-ref-abbrev "test-branch")
      (magit-ref-abbrev "refs/heads/test-branch")
      (magit-ref-abbrev "no-such-branch")
      (magix-test--assert-no-mismatch))))

(ert-deftest magix-test-magit-ref-abbrev-upstream ()
  "Test that `magit-ref-abbrev' works for BRANCH@{upstream} / @{u}, and
falls through correctly for unsupported `@{…}' shapes."
  (skip-unless (featurep 'egix-module))
  (should magix-mode)
  (egix-test--with-fresh-test-repo
    (egix-test--with-upstream-remote
      (let ((magix-debug-mode t))
        (magix-test--clear-debug-buffer)
        (magit-ref-abbrev "test-branch@{upstream}")
        (magit-ref-abbrev "test-branch@{u}")
        (magit-ref-abbrev "no-upstream-branch@{upstream}")
        ;; Unsupported reflog/push shapes must fall through to git
        (magit-ref-abbrev "HEAD@{1}")
        (magit-ref-abbrev "test-branch@{push}")
        (magix-test--assert-no-mismatch)))))

(ert-deftest magix-test-magit-gitdir ()
  "Test that `magit-gitdir' works with the magix --git-dir override across
every gitdir shape — normal, --separate-git-dir, linked worktree, submodule
and combinations thereof — exercised both at the toplevel and from a
subdirectory."
  (skip-unless (featurep 'egix-module))
  (should magix-mode)
  (let* ((root (make-temp-file "magix-gitdir-" t))
         (scenarios (egix-test--build-gitdir-scenarios root)))
    (unwind-protect
        (let ((magix-debug-mode t))
          (magix-test--clear-debug-buffer)
          (pcase-dolist (`(,_label . ,cwd) scenarios)
            (let ((default-directory (file-name-as-directory cwd)))
              (magit-gitdir)))
          (magix-test--assert-no-mismatch))
      (delete-directory root t))))

(ert-deftest magix-test-tramp-exclusion ()
  "Test that magix correctly excludes TRAMP paths."
  (skip-unless (featurep 'egix-module))
  (should magix-mode)
  (let ((default-directory "/ssh:remote:/some/path"))
    ;; TRAMP paths should not be accelerated
    (should-not (magix--should-accelerate-p))))

(ert-deftest magix-test-excluded-repositories ()
  "Test that magix correctly handles excluded repositories."
  (skip-unless (featurep 'egix-module))
  (should magix-mode)
  (egix-test--with-test-repo
    (let ((magix-excluded-repositories (list egix-test-repo-path)))
      ;; Excluded repo should not be accelerated
      (should-not (magix--should-accelerate-p)))))

(ert-deftest magix-test-debug-mode-detection ()
  "Test that debug mode properly detects and logs mismatches."
  (skip-unless (featurep 'egix-module))
  (should magix-mode)
  (egix-test--with-test-repo
    (let ((magix-debug-mode t))
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

