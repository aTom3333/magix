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

(defmacro egix-test--with-upstream-remote (&rest body)
  "Run BODY with a bare local remote configured as `origin' on the test repo.

Must be used inside `egix-test--with-fresh-test-repo' (or the shared variant,
provided no other test is concurrently using it). The remote lives at
`<repo>.remote', `test-branch' is pushed and tracking it, and an extra
`no-upstream-branch' is created for negative tests. The remote directory is
torn down on exit."
  (declare (indent 0))
  `(let ((remote-dir (concat egix-test-repo-path ".remote")))
     (make-directory remote-dir t)
     (let ((default-directory remote-dir))
       (shell-command "git init -q --bare"))
     (unwind-protect
         (progn
           (shell-command (format "git remote add origin %s" remote-dir))
           (shell-command "git push -q origin test-branch")
           (shell-command "git branch --set-upstream-to=origin/test-branch test-branch")
           (shell-command "git branch no-upstream-branch")
           ,@body)
       (delete-directory remote-dir t))))

(defun egix-test--shell (fmt &rest args)
  "Run a shell command formatted from FMT/ARGS, signalling on non-zero exit."
  (let* ((cmd (apply #'format fmt args))
         (rc (shell-command cmd)))
    (unless (zerop rc)
      (error "Shell command failed (rc=%d): %s" rc cmd))))

(defun egix-test--build-gitdir-scenarios (root)
  "Populate ROOT with the assorted gitdir layouts and return an alist of
\(LABEL . CWD-PATH) entries to exercise. The layouts cover normal repos,
--separate-git-dir (absolute and rewritten-relative gitfile), linked
worktrees, submodules, and combinations thereof — each at both the
toplevel and a subdirectory."
  (let ((g "git -c user.email=t@x -c user.name=t -c protocol.file.allow=always"))
    (make-directory (expand-file-name "normal/sub" root) t)
    (egix-test--shell
     "cd %s/normal && %s init -q && touch a && %s add a && %s commit -qm i"
     root g g g)

    (make-directory (expand-file-name "sgd-wt/sub" root) t)
    (egix-test--shell
     "%s init -q --separate-git-dir=%s/sgd-gitdir %s/sgd-wt && cd %s/sgd-wt && touch a && %s add a && %s commit -qm i"
     g root root root g g)

    (egix-test--shell
     "cd %s/normal && %s worktree add -q %s/wt-foo -b wt-branch"
     root g root)
    (make-directory (expand-file-name "wt-foo/sub" root) t)

    (make-directory (expand-file-name "sub-source" root) t)
    (egix-test--shell
     "cd %s/sub-source && %s init -q && touch s && %s add s && %s commit -qm s"
     root g g g)
    (make-directory (expand-file-name "parent" root) t)
    (egix-test--shell
     "cd %s/parent && %s init -q && touch p && %s add p && %s commit -qm i \
&& %s submodule add -q %s/sub-source sub && %s commit -qm sub"
     root g g g g root g)
    (make-directory (expand-file-name "parent/sub/inner" root) t)
    ;; A non-submodule subdir of the parent — exercises "from subdir of
    ;; submodule's parent" without crossing into the submodule itself.
    (make-directory (expand-file-name "parent/notmodule" root) t)

    (egix-test--shell
     "cd %s/sgd-wt && %s worktree add -q %s/sgd-wt-link -b sgd-link"
     root g root)
    (make-directory (expand-file-name "sgd-wt-link/sub" root) t)

    ;; --- Cases where the .git gitfile holds a RELATIVE gitdir path ---

    ;; (a) --separate-git-dir, gitfile manually rewritten to relative form.
    (make-directory (expand-file-name "sgd-rel-wt/sub" root) t)
    (egix-test--shell
     "%s init -q --separate-git-dir=%s/sgd-rel-gitdir %s/sgd-rel-wt"
     g root root)
    (with-temp-file (expand-file-name "sgd-rel-wt/.git" root)
      (insert "gitdir: ../sgd-rel-gitdir\n"))
    (egix-test--shell
     "cd %s/sgd-rel-wt && touch a && %s add a && %s commit -qm i"
     root g g)

    ;; (b) Submodule of a non-main (linked) worktree. The submodule's .git
    ;; gitfile path is naturally relative and points across the worktree dir.
    (make-directory (expand-file-name "wt-foo-sub-init/inner" root) t)
    (egix-test--shell
     "cd %s/normal && %s worktree add -q %s/wt-with-sub -b wt-sub-branch \
&& cd %s/wt-with-sub && %s submodule add -q %s/sub-source sub && %s commit -qm sub-in-wt"
     root g root root g root g)
    (make-directory (expand-file-name "wt-with-sub/sub/inner" root) t)

    `(("normal toplevel"          . ,(expand-file-name "normal" root))
      ("normal subdir"            . ,(expand-file-name "normal/sub" root))
      ("sgd toplevel"             . ,(expand-file-name "sgd-wt" root))
      ("sgd subdir"               . ,(expand-file-name "sgd-wt/sub" root))
      ("linked wt toplevel"       . ,(expand-file-name "wt-foo" root))
      ("linked wt subdir"         . ,(expand-file-name "wt-foo/sub" root))
      ("submodule toplevel"       . ,(expand-file-name "parent/sub" root))
      ("submodule subdir"         . ,(expand-file-name "parent/sub/inner" root))
      ("submodule-parent toplev"  . ,(expand-file-name "parent" root))
      ("submodule-parent subdir"  . ,(expand-file-name "parent/notmodule" root))
      ("linked wt of sgd toplev"  . ,(expand-file-name "sgd-wt-link" root))
      ("linked wt of sgd subdir"  . ,(expand-file-name "sgd-wt-link/sub" root))
      ;; .git gitfile holds a RELATIVE gitdir path:
      ("sgd-rel toplev"           . ,(expand-file-name "sgd-rel-wt" root))
      ("sgd-rel subdir"           . ,(expand-file-name "sgd-rel-wt/sub" root))
      ("submod in linked wt"      . ,(expand-file-name "wt-with-sub/sub" root))
      ("submod in linked wt sub"  . ,(expand-file-name "wt-with-sub/sub/inner" root)))))

(provide 'egix-test-helpers)

;;; egix-test-helpers.el ends here

