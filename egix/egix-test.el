; egix-test.el --- Tests for egix Emacs bindings -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Tests for egix that test the native module functions
;; without requiring Magit.

;;; Code:

(require 'ert)
(require 'egix-test-helpers)

(ert-deftest egix-test-module-loading ()
  "Test that the egix module is loaded."
  ;; Module should be loaded by (require 'egix)
  (should (featurep 'egix-module))
  
  ;; Check that functions are available
  (should (fboundp 'egix-repo-discover))
  (should (fboundp 'egix-repo-current-branch))
  (should (fboundp 'egix-repo-workdir))
  (should (fboundp 'egix-revparse-single)))

(ert-deftest egix-test-repo-discover ()
  "Test the egix-repo-discover function."
  (egix-test--with-test-repo
    ;; Should successfully discover repo
    (let ((repo (egix-repo-discover default-directory)))
      (should repo)
      ;; Repo should be a user-ptr object
      (should (user-ptrp repo)))))

(ert-deftest egix-test-repo-workdir ()
  "Test the egix-repo-workdir function."
  (egix-test--with-test-repo
    (let* ((repo (egix-repo-discover default-directory))
           (workdir (egix-repo-workdir repo)))
      (should workdir)
      (should (stringp workdir))
      (should (file-directory-p workdir))
      ;; Workdir should match our test repo path (normalize both to compare)
      (should (string= (file-name-as-directory (expand-file-name workdir))
                       (file-name-as-directory (expand-file-name egix-test-repo-path)))))))

(ert-deftest egix-test-repo-current-branch ()
  "Test the egix-repo-current-branch function."
  (egix-test--with-test-repo
    (let* ((repo (egix-repo-discover default-directory))
           (branch (egix-repo-current-branch repo)))
      (should branch)
      (should (stringp branch))
      ;; Should be on test-branch that we created
      (should (string= branch "test-branch")))))

(ert-deftest egix-test-error-handling ()
  "Test error handling for invalid repository paths."
  ;; repo-discover should signal error for non-repo path
  (should-error (egix-repo-discover temporary-file-directory)))

(ert-deftest egix-test-multiple-repos ()
  "Test working with multiple repositories."
  (egix-test--with-fresh-test-repo
    (let ((repo1 (egix-repo-discover default-directory))
          (repo1-path egix-test-repo-path))
     
      (egix-test--with-fresh-test-repo
        (let ((repo2 (egix-repo-discover default-directory))
              (repo2-path egix-test-repo-path))
          
          ;; Both repos should be valid
          (should (user-ptrp repo1))
          (should (user-ptrp repo2))
          
          ;; They should be different repos
          (should-not (string= repo1-path repo2-path))
          
          ;; Can still use repo1
          (should (stringp (egix-repo-current-branch repo1)))
          ;; Can still use repo2
          (should (stringp (egix-repo-current-branch repo2))))))))

(provide 'egix-test)

;;; egix-test.el ends here

