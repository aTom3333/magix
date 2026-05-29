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

;; Tests must never touch the user's real magix stats file. Point the global
;; default at a sandbox file under temporary-file-directory; the save/load
;; tests still let-bind their own temp files on top.
(setq magix-stats-file
      (expand-file-name "magix-test-stats.eld" temporary-file-directory))

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

(ert-deftest magix-test-stats-signature ()
  "Signature aggregator masks non-flag args but keeps flags verbatim."
  (should (equal (magix--stats-signature '("rev-parse" "--show-toplevel"))
                 "rev-parse --show-toplevel"))
  (should (equal (magix--stats-signature '("rev-parse" "HEAD"))
                 "rev-parse <arg>"))
  (should (equal (magix--stats-signature '("rev-parse" "--verify" "HEAD"))
                 "rev-parse --verify <arg>"))
  (should (equal (magix--stats-signature '("log" "--format=%h %s" "HEAD^{commit}"))
                 "log --format=%h %s <arg>")))

(ert-deftest magix-test-stats-records-invocations ()
  "When `magix-record-stats' is on, every git call lands in `magix--stats'
with the right intercept flag and a positive duration."
  (skip-unless (featurep 'egix-module))
  (should magix-mode)
  (egix-test--with-test-repo
    (let ((magix-record-stats t)
          (magix--stats (make-hash-table :test 'equal)))
      ;; Intercepted: rev-parse --show-toplevel is in the dispatcher.
      (magit-toplevel)
      (let* ((sig (magix--stats-signature '("rev-parse" "--show-toplevel")))
             (cell (gethash sig magix--stats)))
        (should cell)
        (should (= 1 (aref cell 0)))
        (should (= 1 (aref cell 1)))
        (should (floatp (aref cell 2))))
      ;; Non-intercepted: status -z --porcelain is deliberately not handled.
      (magit-git-string "status" "-z" "--porcelain")
      (let* ((sig (magix--stats-signature '("status" "-z" "--porcelain")))
             (cell (gethash sig magix--stats)))
        (should cell)
        (should (= 1 (aref cell 0)))
        (should (= 0 (aref cell 1)))))))

(ert-deftest magix-test-stats-not-recorded-when-off ()
  "With `magix-record-stats' nil, the hash stays empty."
  (skip-unless (featurep 'egix-module))
  (should magix-mode)
  (egix-test--with-test-repo
    (let ((magix-record-stats nil)
          (magix--stats (make-hash-table :test 'equal)))
      (magit-toplevel)
      (should (zerop (hash-table-count magix--stats))))))

(ert-deftest magix-test-stats-clear ()
  "`magix-clear-stats' empties memory and removes the persistence file."
  (let ((magix--stats (make-hash-table :test 'equal))
        (magix-stats-file (make-temp-file "magix-stats-")))
    (unwind-protect
        (progn
          (puthash "rev-parse" (vector 1 1 0.001 '("rev-parse")) magix--stats)
          (magix-save-stats)
          (should (file-exists-p magix-stats-file))
          (puthash "rev-parse" (vector 1 1 0.001 '("rev-parse")) magix--stats)
          (magix-clear-stats)
          (should (zerop (hash-table-count magix--stats)))
          (should-not (file-exists-p magix-stats-file)))
      (when (file-exists-p magix-stats-file)
        (delete-file magix-stats-file)))))

(ert-deftest magix-test-stats-debug-mode-excludes-orig-time ()
  "Debug-mode comparison cost should NOT be counted in the recorded duration.
The slow mock orig-func sleeps 100ms; the recorded duration should be well
under that since the orig-func portion is timed separately and subtracted."
  (skip-unless (featurep 'egix-module))
  (should magix-mode)
  (egix-test--with-test-repo
    (let ((magix-record-stats t)
          (magix-debug-mode t)
          (magix--stats (make-hash-table :test 'equal))
          (slow-orig (lambda (_destination &rest _args)
                       (sleep-for 0.100)
                       (insert (magix--normalize-path egix-test-repo-path))
                       (insert "\n")
                       0)))
      (magix-test--clear-debug-buffer)
      (with-temp-buffer
        (magix-magit-process-git slow-orig t "rev-parse" "--show-toplevel"))
      (magix-test--assert-no-mismatch)
      (let* ((sig (magix--stats-signature '("rev-parse" "--show-toplevel")))
             (cell (gethash sig magix--stats)))
        (should cell)
        (should (= 1 (aref cell 0)))
        (should (= 1 (aref cell 1)))
        ;; The 100ms sleep must not appear in the recorded duration.
        (should (< (aref cell 2) 0.050))))))

(ert-deftest magix-test-stats-non-intercepted-includes-orig-time ()
  "Non-intercepted calls record the full wall-clock duration — including
the git CLI fork — so the stats can identify what's worth speeding up next."
  (skip-unless (featurep 'egix-module))
  (should magix-mode)
  (egix-test--with-test-repo
    (let ((magix-record-stats t)
          (magix--stats (make-hash-table :test 'equal))
          (slow-orig (lambda (_destination &rest _args)
                       (sleep-for 0.100)
                       0)))
      (with-temp-buffer
        ;; status -z --porcelain is deliberately not in the dispatcher
        (magix-magit-process-git slow-orig t "status" "-z" "--porcelain"))
      (let* ((sig (magix--stats-signature '("status" "-z" "--porcelain")))
             (cell (gethash sig magix--stats)))
        (should cell)
        (should (= 1 (aref cell 0)))
        (should (= 0 (aref cell 1)))
        (should (>= (aref cell 2) 0.090))))))

(ert-deftest magix-test-stats-save-flushes-and-clears ()
  "`magix-save-stats' writes in-flight stats to disk and empties memory."
  (let ((magix--stats (make-hash-table :test 'equal))
        (magix-stats-file (make-temp-file "magix-stats-")))
    (unwind-protect
        (progn
          (magix--stats-record '("rev-parse" "HEAD") t 0.002)
          (magix--stats-record '("rev-parse" "main") t 0.003)
          (magix--stats-record '("status" "-z") nil 0.150)
          (magix-save-stats)
          (should (zerop (hash-table-count magix--stats)))
          (let* ((on-disk (magix--stats-read-file))
                 (sig-rp (magix--stats-signature '("rev-parse" "HEAD")))
                 (sig-st (magix--stats-signature '("status" "-z")))
                 (cell-rp (gethash sig-rp on-disk))
                 (cell-st (gethash sig-st on-disk)))
            (should cell-rp)
            (should (= 2 (aref cell-rp 0)))
            (should (= 2 (aref cell-rp 1)))
            (should (< (abs (- 0.005 (aref cell-rp 2))) 0.0001))
            (should cell-st)
            (should (= 1 (aref cell-st 0)))
            (should (= 0 (aref cell-st 1)))
            (should (< (abs (- 0.150 (aref cell-st 2))) 0.0001))))
      (when (file-exists-p magix-stats-file)
        (delete-file magix-stats-file)))))

(ert-deftest magix-test-stats-save-accumulates-across-sessions ()
  "Repeated saves are additive: each session's in-flight delta is merged
into the on-disk total, never replacing it."
  (let ((magix--stats (make-hash-table :test 'equal))
        (magix-stats-file (make-temp-file "magix-stats-")))
    (unwind-protect
        (progn
          ;; Session 1 flush: file has count=1.
          (magix--stats-record '("rev-parse" "HEAD") t 0.002)
          (magix-save-stats)
          ;; Session 2: nothing in memory from session 1 (save cleared it),
          ;; record one more then flush — file should reach count=2.
          (magix--stats-record '("rev-parse" "HEAD") t 0.003)
          (magix-save-stats)
          (let* ((on-disk (magix--stats-read-file))
                 (sig (magix--stats-signature '("rev-parse" "HEAD")))
                 (cell (gethash sig on-disk)))
            (should cell)
            (should (= 2 (aref cell 0)))
            (should (= 2 (aref cell 1)))
            (should (< (abs (- 0.005 (aref cell 2))) 0.0001))))
      (when (file-exists-p magix-stats-file)
        (delete-file magix-stats-file)))))

(ert-deftest magix-test-stats-save-noop-when-empty ()
  "`magix-save-stats' must not touch the file when memory is empty —
otherwise the timer firing on an idle Emacs would clobber a sibling
instance that just wrote fresh data."
  (let ((magix--stats (make-hash-table :test 'equal))
        (magix-stats-file (expand-file-name "magix-stats-noop.eld"
                                            temporary-file-directory)))
    (unwind-protect
        (progn
          (when (file-exists-p magix-stats-file)
            (delete-file magix-stats-file))
          (magix-save-stats)
          (should-not (file-exists-p magix-stats-file)))
      (when (file-exists-p magix-stats-file)
        (delete-file magix-stats-file)))))

(ert-deftest magix-test-stats-read-file-missing-is-empty ()
  "`magix--stats-read-file' returns an empty hash when no file exists."
  (let ((magix-stats-file (expand-file-name "magix-stats-does-not-exist.eld"
                                            temporary-file-directory)))
    (when (file-exists-p magix-stats-file)
      (delete-file magix-stats-file))
    (let ((h (magix--stats-read-file)))
      (should (hash-table-p h))
      (should (zerop (hash-table-count h))))))

(ert-deftest magix-test-stats-merge-into ()
  "`magix--stats-merge-into' sums shared keys and copies novel ones."
  (let ((target (make-hash-table :test 'equal))
        (source (make-hash-table :test 'equal)))
    (puthash "a" (vector 1 1 0.10 '("a")) target)
    (puthash "a" (vector 2 0 0.20 '("a")) source)
    (puthash "b" (vector 5 3 0.50 '("b")) source)
    (magix--stats-merge-into target source)
    ;; Shared key: counts added.
    (let ((a (gethash "a" target)))
      (should (= 3 (aref a 0)))
      (should (= 1 (aref a 1)))
      (should (< (abs (- 0.30 (aref a 2))) 0.0001)))
    ;; Novel key: copied verbatim.
    (let ((b (gethash "b" target)))
      (should (= 5 (aref b 0)))
      (should (= 3 (aref b 1)))
      (should (< (abs (- 0.50 (aref b 2))) 0.0001)))
    ;; Mutating the merged cell must not corrupt the source (copy-sequence).
    (let ((b-target (gethash "b" target))
          (b-source (gethash "b" source)))
      (aset b-target 0 999)
      (should (= 5 (aref b-source 0))))))

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

;;; Config interception

(defun magix-test--git (&rest args)
  "Run git with ARGS via call-process to bypass shell quoting.
Set up so values with `*' or other shell-special characters reach git
verbatim on Windows cmd as well as POSIX shells."
  (apply #'call-process "git" nil nil nil args))

(ert-deftest magix-test-magit-config-get-all-include ()
  "magit-get-all exercises the `config -z --get-all --include KEY' arm.
Covers single-value, multi-value, missing, and subsection-with-slash."
  (skip-unless (featurep 'egix-module))
  (should magix-mode)
  (egix-test--with-test-repo
    (magix-test--git "config" "core.abbrev" "7")
    (magix-test--git "config" "--add" "remote.origin.fetch"
                     "+refs/heads/*:refs/remotes/origin/*")
    (magix-test--git "config" "--add" "remote.origin.fetch"
                     "+refs/tags/*:refs/tags/*")
    (magix-test--git "config" "branch.Feature/X.pushRemote" "origin")
    (let ((magix-debug-mode t)
          (magit--refresh-cache nil))
      (magix-test--clear-debug-buffer)
      (should (equal (magit-get-all "core.abbrev") '("7")))
      (should (equal (magit-get-all "remote.origin.fetch")
                     '("+refs/heads/*:refs/remotes/origin/*"
                       "+refs/tags/*:refs/tags/*")))
      (should (equal (magit-get-all "branch.Feature/X.pushRemote") '("origin")))
      (should (null (magit-get-all "does.not.exist")))
      (magix-test--assert-no-mismatch))))

(ert-deftest magix-test-magit-config-get-all-include-local ()
  "magit-get-all with `--local' exercises the local-scope arm."
  (skip-unless (featurep 'egix-module))
  (should magix-mode)
  (egix-test--with-test-repo
    (magix-test--git "config" "status.showUntrackedFiles" "all")
    (let ((magix-debug-mode t)
          (magit--refresh-cache nil))
      (magix-test--clear-debug-buffer)
      (should (equal (magit-get-all "--local" "status.showUntrackedFiles") '("all")))
      (should (null (magit-get-all "--local" "missing.key")))
      (magix-test--assert-no-mismatch))))

(ert-deftest magix-test-magit-config-list-z ()
  "magit-git-items \"config\" \"--list\" \"-z\" exercises the `--list -z' arm."
  (skip-unless (featurep 'egix-module))
  (should magix-mode)
  (egix-test--with-test-repo
    (magix-test--git "config" "core.abbrev" "7")
    (magix-test--git "config" "--add" "remote.origin.fetch"
                     "+refs/heads/*:refs/remotes/origin/*")
    (let ((magix-debug-mode t))
      (magix-test--clear-debug-buffer)
      (let ((items (magit-git-items "config" "--list" "-z")))
        (should (listp items))
        (should (seq-find (lambda (s) (string-match-p "\\`core\\.abbrev\n7\\'" s)) items))
        (should (seq-find (lambda (s)
                            (string-match-p "\\`remote\\.origin\\.fetch\n\\+refs/heads/\\*"
                                            s))
                          items)))
      (magix-test--assert-no-mismatch))))

(ert-deftest magix-test-magit-config-global-scope-filter ()
  "`config --global KEY' must NOT see a key set only in local scope.
Verifies scope filtering without needing HOME isolation (which doesn't
reliably propagate to the in-process gitoxide on Windows). Both the
git CLI and magix should return nil here; debug-mode asserts parity."
  (skip-unless (featurep 'egix-module))
  (should magix-mode)
  (egix-test--with-test-repo
    ;; A name very unlikely to exist in the user's real global config.
    (magix-test--git "config" "magix.test.local.only" "from-local")
    (let ((magix-debug-mode t))
      (magix-test--clear-debug-buffer)
      (should (null (magit-git-string "config" "--global" "magix.test.local.only")))
      (magix-test--assert-no-mismatch))))

(ert-deftest magix-test-magit-config-default ()
  "magit-git-string \"config\" \"--default=X\" KEY exercises the default-arg arm."
  (skip-unless (featurep 'egix-module))
  (should magix-mode)
  (egix-test--with-test-repo
    (let ((magix-debug-mode t))
      (magix-test--clear-debug-buffer)
      ;; magix.no.such is unset; the default _ should win.
      (should (equal (magit-git-string "config" "--default=_" "magix.no.such") "_"))
      ;; Set it and verify the real value wins over the default.
      (magix-test--git "config" "magix.no.such" "found")
      (should (equal (magit-git-string "config" "--default=_" "magix.no.such") "found"))
      (magix-test--assert-no-mismatch))))

(ert-deftest magix-test-magit-config-include-directive ()
  "Reads of an included file's keys go through gitoxide's include processing.
Verifies parity with git when `[include]' is present in the local config."
  (skip-unless (featurep 'egix-module))
  (should magix-mode)
  (egix-test--with-test-repo
    (let* ((included (expand-file-name "extra-config" egix-test-repo-path))
           (local-config (expand-file-name ".git/config" egix-test-repo-path)))
      (with-temp-file included
        (insert "[core]\n\tabbrev = 9\n"))
      ;; Append an [include] section to .git/config so the dispatcher sees it.
      (with-temp-buffer
        (insert-file-contents local-config)
        (goto-char (point-max))
        (insert (format "[include]\n\tpath = %s\n" included))
        (write-region (point-min) (point-max) local-config))
      (let ((magix-debug-mode t)
            (magit--refresh-cache nil))
        (magix-test--clear-debug-buffer)
        (should (equal (magit-get-all "core.abbrev") '("9")))
        (magix-test--assert-no-mismatch)))))

(provide 'magix-test)

;;; magix-test.el ends here

