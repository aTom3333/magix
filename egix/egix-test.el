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
  (should (fboundp 'egix-repo-gitdir))
  (should (fboundp 'egix-revparse-single))
  (should (fboundp 'egix-revparse-short))
  (should (fboundp 'egix-revparse-abbrev-ref))
  (should (fboundp 'egix-symbolic-ref))
  (should (fboundp 'egix-symbolic-ref-short)))

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

(ert-deftest egix-test-repo-gitdir ()
  "Test the egix-repo-gitdir function."
  (egix-test--with-test-repo
    (let* ((repo (egix-repo-discover default-directory))
           (gitdir (egix-repo-gitdir repo)))
      (should gitdir)
      (should (stringp gitdir))
      (should (equal (expand-file-name ".git" egix-test-repo-path) gitdir)))))

(ert-deftest egix-test-repo-current-branch ()
  "Test the egix-repo-current-branch function."
  (egix-test--with-test-repo
    (let* ((repo (egix-repo-discover default-directory))
           (branch (egix-repo-current-branch repo)))
      (should branch)
      (should (stringp branch))
      ;; Should be on test-branch that we created
      (should (string= branch "test-branch")))))

(ert-deftest egix-test-revparse-single-nonexistent-ref ()
  "A ref that doesn't exist must yield nil, not an error — otherwise the
magix dispatcher falls through to git and wastes a process spawn."
  (egix-test--with-fresh-test-repo
    (let ((repo (egix-repo-discover default-directory)))
      ;; Fully-qualified refs that do not exist
      (should-not (egix-revparse-single repo "refs/tags/no-such-tag"))
      (should-not (egix-revparse-single repo "refs/heads/no-such-branch"))
      (should-not (egix-revparse-single repo "refs/stash"))
      ;; Bare name that doesn't resolve to anything
      (should-not (egix-revparse-single repo "no-such-rev-xyz"))
      ;; Sanity: a ref that DOES exist still returns its SHA
      (let ((head (egix-revparse-single repo "HEAD")))
        (should (stringp head))
        (should (= (length head) 40))))))

(ert-deftest egix-test-revparse-short ()
  "Test egix-revparse-short returns abbreviated object ids."
  (egix-test--with-test-repo
    (let* ((repo (egix-repo-discover default-directory))
           (short (egix-revparse-short repo "HEAD"))
           (full (egix-revparse-single repo "HEAD")))
      (should short)
      (should (stringp short))
      (should (string-match-p "^[0-9a-f]+$" short))
      ;; Full SHA-1 is 40 chars; the abbreviation must be strictly shorter
      ;; (otherwise the function is returning the full id and not actually
      ;; honoring core.abbrev / --short).
      (should (= (length full) 40))
      (should (< (length short) (length full)))
      ;; And the short prefix must obviously match the full id
      (should (string-prefix-p short full))
      ;; Unknown spec returns nil rather than raising
      (should-not (egix-revparse-short repo "no-such-ref-xyz")))))

(ert-deftest egix-test-revparse-abbrev-ref ()
  "Test egix-revparse-abbrev-ref returns shortened ref names."
  (egix-test--with-test-repo
    (let ((repo (egix-repo-discover default-directory)))
      ;; HEAD is symbolic -> short branch name (peeled one level)
      (should (string= (egix-revparse-abbrev-ref repo "HEAD") "test-branch"))
      ;; A plain branch name -> itself
      (should (string= (egix-revparse-abbrev-ref repo "test-branch") "test-branch"))
      ;; Fully qualified -> shortened
      (should (string= (egix-revparse-abbrev-ref repo "refs/heads/test-branch")
                       "test-branch"))
      ;; Non-existent ref -> nil
      (should-not (egix-revparse-abbrev-ref repo "no-such-branch"))
      ;; No upstream configured yet -> nil (definitive: there is no upstream)
      (should-not (egix-revparse-abbrev-ref repo "test-branch@{upstream}"))
      ;; Reflog / @{push} / other @{...} shapes are NOT implemented: they must
      ;; signal an error so the dispatcher falls back to git, rather than
      ;; pretending a nil answer is definitive.
      (should-error (egix-revparse-abbrev-ref repo "HEAD@{1}"))
      (should-error (egix-revparse-abbrev-ref repo "test-branch@{push}")))))

(ert-deftest egix-test-revparse-abbrev-ref-upstream ()
  "Test that BRANCH@{upstream} / BRANCH@{u} resolves to the tracking ref."
  (egix-test--with-fresh-test-repo
    (egix-test--with-upstream-remote
      (let ((repo (egix-repo-discover default-directory)))
        (should (string= (egix-revparse-abbrev-ref repo "test-branch@{upstream}")
                         "origin/test-branch"))
        ;; @{u} is the documented alias for @{upstream}
        (should (string= (egix-revparse-abbrev-ref repo "test-branch@{u}")
                         "origin/test-branch"))
        ;; A branch with no upstream still returns nil
        (should-not (egix-revparse-abbrev-ref repo "no-upstream-branch@{upstream}"))))))

(ert-deftest egix-test-symbolic-ref ()
  "Test egix-symbolic-ref / egix-symbolic-ref-short on a symbolic reference."
  (egix-test--with-test-repo
    (let ((repo (egix-repo-discover default-directory)))
      ;; HEAD is the canonical symbolic ref
      (should (string= (egix-symbolic-ref repo "HEAD") "refs/heads/test-branch"))
      (should (string= (egix-symbolic-ref-short repo "HEAD") "test-branch"))
      ;; A direct (non-symbolic) ref returns nil for both
      (should-not (egix-symbolic-ref repo "refs/heads/test-branch"))
      (should-not (egix-symbolic-ref-short repo "refs/heads/test-branch"))
      ;; A user-created symbolic ref resolves to its target
      (let ((default-directory egix-test-repo-path))
        (shell-command "git symbolic-ref refs/heads/sym-target refs/heads/test-branch"))
      (should (string= (egix-symbolic-ref repo "refs/heads/sym-target")
                       "refs/heads/test-branch"))
      (should (string= (egix-symbolic-ref-short repo "refs/heads/sym-target")
                       "test-branch"))
      ;; Non-existent ref returns nil
      (should-not (egix-symbolic-ref repo "refs/heads/nope"))
      (should-not (egix-symbolic-ref-short repo "refs/heads/nope")))))

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

