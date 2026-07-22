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
  (should (fboundp 'egix-repo-is-bare))
  (should (fboundp 'egix-revparse-single))
  (should (fboundp 'egix-revparse-short))
  (should (fboundp 'egix-revparse-abbrev-ref))
  (should (fboundp 'egix-revparse-symbolic-full-name))
  (should (fboundp 'egix-symbolic-ref))
  (should (fboundp 'egix-symbolic-ref-short))
  (should (fboundp 'egix-commit-format))
  (should (fboundp 'egix-for-each-ref))
  (should (fboundp 'egix-blob-content))
  (should (fboundp 'egix-ls-tree-entry))
  (should (fboundp 'egix-index-differs-from-head))
  (should (fboundp 'egix-log)))

(ert-deftest egix-test-repo-discover ()
  "Test the egix-repo-discover function."
  (egix-test--with-test-repo
    ;; Should successfully discover repo
    (let ((repo (egix-repo-discover default-directory)))
      (should repo)
      ;; Repo should be a user-ptr object
      (should (user-ptrp repo)))))

(ert-deftest egix-test-repo-discover-suppress-home ()
  "egix-repo-discover with SUPPRESS-HOME opens the repo and restores HOME.
The suppressed config behaviour itself is platform-dependent (it only changes
which global config gix reads), so this only asserts the open still succeeds
and that the process HOME is left untouched afterwards."
  (egix-test--with-test-repo
    (let ((home-before (getenv "HOME")))
      (let ((repo (egix-repo-discover default-directory t)))
        (should (user-ptrp repo))
        (should (egix-repo-workdir repo)))
      ;; HOME must be exactly what it was before the suppressed open.
      (should (equal (getenv "HOME") home-before)))))

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
      (should (equal (expand-file-name ".git" egix-test-repo-path)
                     (expand-file-name gitdir))))))

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

(ert-deftest egix-test-revparse-single-deleted-prev-checkout ()
  "gix's `rev_parse_single' returns a stale OID for `@{-1}' when the previous
checkout's branch has been deleted; we must NOT propagate that value (nil or
error are both fine — both let the dispatcher fall back to git)."
  (egix-test--with-fresh-test-repo
    ;; Build a `@{-1}' that targets a branch we'll then delete.
    (shell-command "git -c user.email=x -c user.name=x checkout -q -b prev-target")
    (shell-command "git -c user.email=x -c user.name=x commit --allow-empty -qm 'on prev-target'")
    (shell-command "git -c user.email=x -c user.name=x checkout -q -")
    (shell-command "git branch -D prev-target")
    (let ((repo (egix-repo-discover default-directory)))
      (should-not (ignore-errors (egix-revparse-single repo "@{-1}"))))))

(ert-deftest egix-test-revparse-short ()
  "Test egix-revparse-short returns abbreviated object ids."
  (egix-test--with-test-repo
    (let* ((repo (egix-repo-discover default-directory))
           (short (egix-revparse-short repo "HEAD" nil))
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
      (should-not (egix-revparse-short repo "no-such-ref-xyz" nil)))))

(ert-deftest egix-test-revparse-short-with-length ()
  "Test egix-revparse-short with an explicit minimum LENGTH."
  (egix-test--with-test-repo
    (let* ((repo (egix-repo-discover default-directory))
           (full (egix-revparse-single repo "HEAD"))
           (short4 (egix-revparse-short repo "HEAD" 4))
           (short11 (egix-revparse-short repo "HEAD" 11)))
      ;; LENGTH is a minimum: result has at least N chars and is a prefix of full.
      (should (>= (length short4) 4))
      (should (>= (length short11) 11))
      (should (string-prefix-p short4 full))
      (should (string-prefix-p short11 full))
      ;; Length beyond the hash size is clamped to the full hash.
      (should (string= (egix-revparse-short repo "HEAD" 999) full))
      ;; Unknown spec still returns nil.
      (should-not (egix-revparse-short repo "no-such-ref-xyz" 4)))))

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
      ;; A valid object that is not a ref (raw commit hash) -> empty string,
      ;; NOT nil: git prints nothing but exits 0 here, so the answer is "found,
      ;; no symbolic name" rather than "not found".
      (let ((sha (egix-revparse-single repo "HEAD")))
        (should (equal (egix-revparse-abbrev-ref repo sha) "")))
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

(ert-deftest egix-test-revparse-abbrev-ref-local-upstream ()
  "BRANCH@{upstream} resolves when the upstream is a local branch (remote=`.')."
  (egix-test--with-fresh-test-repo
    (egix-test--shell "git branch tracks-local")
    (egix-test--shell "git branch --set-upstream-to=test-branch tracks-local")
    (let ((repo (egix-repo-discover default-directory)))
      (should (string= (egix-revparse-abbrev-ref repo "tracks-local@{upstream}")
                       "test-branch"))
      (should (string= (egix-revparse-abbrev-ref repo "tracks-local@{u}")
                       "test-branch")))))

(ert-deftest egix-test-revparse-symbolic-full-name ()
  "Test egix-revparse-symbolic-full-name returns full ref names."
  (egix-test--with-test-repo
    (let ((repo (egix-repo-discover default-directory)))
      ;; HEAD is symbolic -> full target name (peeled one level)
      (should (string= (egix-revparse-symbolic-full-name repo "HEAD")
                       "refs/heads/test-branch"))
      ;; A plain branch name -> its full form
      (should (string= (egix-revparse-symbolic-full-name repo "test-branch")
                       "refs/heads/test-branch"))
      ;; Already fully qualified -> itself
      (should (string= (egix-revparse-symbolic-full-name repo "refs/heads/test-branch")
                       "refs/heads/test-branch"))
      ;; A valid object that is not a ref (raw commit hash) -> empty string,
      ;; NOT nil: git prints nothing but exits 0 here, so the answer is "found,
      ;; no symbolic name" rather than "not found".
      (let ((sha (egix-revparse-single repo "HEAD")))
        (should (equal (egix-revparse-symbolic-full-name repo sha) "")))
      ;; Non-existent ref -> nil
      (should-not (egix-revparse-symbolic-full-name repo "no-such-branch"))
      ;; No upstream configured -> nil (definitive)
      (should-not (egix-revparse-symbolic-full-name repo "test-branch@{upstream}"))
      ;; Unsupported reflog/push shapes signal an error (caller falls back to git)
      (should-error (egix-revparse-symbolic-full-name repo "HEAD@{1}"))
      (should-error (egix-revparse-symbolic-full-name repo "test-branch@{push}")))))

(ert-deftest egix-test-revparse-symbolic-full-name-upstream ()
  "BRANCH@{upstream} / @{u} resolves to the tracking ref's full name."
  (egix-test--with-fresh-test-repo
    (egix-test--with-upstream-remote
      (let ((repo (egix-repo-discover default-directory)))
        (should (string= (egix-revparse-symbolic-full-name repo "test-branch@{upstream}")
                         "refs/remotes/origin/test-branch"))
        (should (string= (egix-revparse-symbolic-full-name repo "test-branch@{u}")
                         "refs/remotes/origin/test-branch"))
        (should-not (egix-revparse-symbolic-full-name repo "no-upstream-branch@{upstream}"))))))

(ert-deftest egix-test-revparse-symbolic-full-name-local-upstream ()
  "BRANCH@{upstream} resolves with a local-branch upstream (remote=`.')."
  (egix-test--with-fresh-test-repo
    (egix-test--shell "git branch tracks-local")
    (egix-test--shell "git branch --set-upstream-to=test-branch tracks-local")
    (let ((repo (egix-repo-discover default-directory)))
      (should (string= (egix-revparse-symbolic-full-name repo "tracks-local@{upstream}")
                       "refs/heads/test-branch"))
      (should (string= (egix-revparse-symbolic-full-name repo "tracks-local@{u}")
                       "refs/heads/test-branch")))))

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

(ert-deftest egix-test-object-type ()
  "Test egix-object-type returns the object kind for each object type."
  (egix-test--with-fresh-test-repo
    (let ((default-directory egix-test-repo-path))
      (shell-command "git tag -a v1 -m \"annotated tag\""))
    (let* ((repo (egix-repo-discover default-directory))
           (commit (egix-revparse-single repo "HEAD")))
      ;; A commit resolved by full hash, abbreviation, and ref-ish revspec
      (should (string= (egix-object-type repo commit) "commit"))
      (should (string= (egix-object-type repo (substring commit 0 8)) "commit"))
      (should (string= (egix-object-type repo "HEAD") "commit"))
      ;; HEAD's tree, and a blob reached through the rev:path form
      (should (string= (egix-object-type repo "HEAD^{tree}") "tree"))
      (should (string= (egix-object-type repo "HEAD:README.md") "blob"))
      ;; An annotated tag object
      (should (string= (egix-object-type repo "v1") "tag"))
      ;; A full-length hash with no matching object resolves to nil, not an error
      (should-not (egix-object-type repo (make-string 40 ?0)))
      ;; A name that resolves to nothing also yields nil
      (should-not (egix-object-type repo "no-such-rev-xyz")))))

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

(ert-deftest egix-test-for-each-ref ()
  "egix-for-each-ref lists a namespace sorted by full refname, capturing
symbolic targets, with git-compatible short names."
  (egix-test--with-fresh-test-repo
    (egix-test--shell "git branch aaa")
    (egix-test--shell "git branch zzz")
    (egix-test--shell "git symbolic-ref refs/heads/mysym refs/heads/test-branch")
    (let* ((repo (egix-repo-discover default-directory))
           (entries (egix-for-each-ref repo "refs/heads"))
           (full-names (mapcar #'cadr entries)))
      ;; Each entry is (SYMREF FULL SHORT). A direct branch has a nil SYMREF...
      (should (member '(nil "refs/heads/aaa" "aaa") entries))
      (should (member '(nil "refs/heads/test-branch" "test-branch") entries))
      ;; ...and a symbolic ref carries the target's full refname (git's %(symref)).
      (should (member '("refs/heads/test-branch" "refs/heads/mysym" "mysym") entries))
      ;; Sorted ascending by full refname (git's default order).
      (should (equal full-names (sort (copy-sequence full-names) #'string<)))
      ;; Empty namespace -> nil (git exits 0 with no output).
      (should (null (egix-for-each-ref repo "refs/nonexistent")))
      ;; A trailing slash on the namespace is accepted and equivalent.
      (should (equal entries (egix-for-each-ref repo "refs/heads/"))))))

(ert-deftest egix-test-for-each-ref-remote-head-shortening ()
  "A remote's symbolic HEAD shortens to the remote name (git's %(refname:short)),
not gix's category strip `<remote>/HEAD'."
  (egix-test--with-fresh-test-repo
    (egix-test--shell "git update-ref refs/remotes/origin/main HEAD")
    (egix-test--shell "git update-ref refs/remotes/origin/feature HEAD")
    (egix-test--shell "git symbolic-ref refs/remotes/origin/HEAD refs/remotes/origin/main")
    (let* ((repo (egix-repo-discover default-directory))
           (entries (egix-for-each-ref repo "refs/remotes")))
      (should (member '(nil "refs/remotes/origin/main" "origin/main") entries))
      ;; origin/HEAD is symbolic; its short name is the remote, not `origin/HEAD'.
      (should (member '("refs/remotes/origin/main" "refs/remotes/origin/HEAD" "origin")
                      entries)))))

(ert-deftest egix-test-for-each-ref-ambiguous-short ()
  "An ambiguous abbreviation backs off to a longer form, matching git's
shortest-unambiguous rule."
  (egix-test--with-fresh-test-repo
    ;; `origin' branch collides with the remote HEAD's abbreviation; `shared'
    ;; exists as both a branch and a tag.
    (egix-test--shell "git branch origin")
    (egix-test--shell "git branch shared")
    (egix-test--shell "git tag shared")
    (egix-test--shell "git update-ref refs/remotes/origin/main HEAD")
    (egix-test--shell "git symbolic-ref refs/remotes/origin/HEAD refs/remotes/origin/main")
    (let* ((repo (egix-repo-discover default-directory))
           (heads (egix-for-each-ref repo "refs/heads"))
           (tags (egix-for-each-ref repo "refs/tags"))
           (remotes (egix-for-each-ref repo "refs/remotes")))
      ;; `origin' clashes with refs/remotes/origin/HEAD -> "heads/origin".
      (should (member '(nil "refs/heads/origin" "heads/origin") heads))
      ;; branch/tag `shared' clash -> "heads/shared" / "tags/shared".
      (should (member '(nil "refs/heads/shared" "heads/shared") heads))
      (should (member '(nil "refs/tags/shared" "tags/shared") tags))
      ;; The remote HEAD's "origin" clashes with the branch -> "origin/HEAD".
      (should (member '("refs/remotes/origin/main" "refs/remotes/origin/HEAD" "origin/HEAD")
                      remotes)))))

(ert-deftest egix-test-blob-content ()
  "egix-blob-content returns a blob's raw bytes, via rev:path and via oid."
  (egix-test--with-fresh-test-repo
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region "" nil (expand-file-name "empty.txt" egix-test-repo-path)))
    (egix-test--shell "git add empty.txt")
    (egix-test--shell "git commit -qm empty")
    (let ((repo (egix-repo-discover default-directory)))
      ;; README.md is created by the test-repo helper.
      (should (equal (egix-blob-content repo "HEAD:README.md")
                     "# Test Repository\n"))
      ;; Same blob addressed by its object id.
      (let ((oid (egix-revparse-single repo "HEAD:README.md")))
        (should (equal (egix-blob-content repo oid) "# Test Repository\n")))
      ;; An empty blob is the empty string (git prints nothing, exit 0).
      (should (equal (egix-blob-content repo "HEAD:empty.txt") "")))))

(ert-deftest egix-test-blob-content-defers ()
  "egix-blob-content signals (caller falls back to git) for CRLF, non-UTF-8,
non-blob and unresolved specs."
  (egix-test--with-fresh-test-repo
    (egix-test--shell "git config core.autocrlf false") ; keep CRLF in the blob
    (let ((coding-system-for-write 'no-conversion))
      (write-region "a\r\nb\r\n" nil (expand-file-name "crlf.txt" egix-test-repo-path))
      (write-region (apply #'unibyte-string '(0 1 255 10)) nil
                    (expand-file-name "bin.dat" egix-test-repo-path)))
    (egix-test--shell "git add -A")
    (egix-test--shell "git commit -qm blobs")
    (let ((repo (egix-repo-discover default-directory)))
      (should-error (egix-blob-content repo "HEAD:crlf.txt"))      ; CR in content
      (should-error (egix-blob-content repo "HEAD:bin.dat"))       ; non-UTF-8
      (should-error (egix-blob-content repo "HEAD^{tree}"))        ; not a blob
      (should-error (egix-blob-content repo "HEAD:no-such-file"))))) ; unresolved

(ert-deftest egix-test-ls-tree-entry ()
  "egix-ls-tree-entry formats one tree entry like `git ls-tree --full-tree'."
  (egix-test--with-test-repo
    (let* ((repo (egix-repo-discover default-directory))
           (blob (egix-revparse-single repo "HEAD:subdir/nested.txt"))
           (tree (egix-revparse-single repo "HEAD:subdir")))
      ;; Nested regular file.
      (should (equal (egix-ls-tree-entry repo "HEAD" "subdir/nested.txt")
                     (format "100644 blob %s\tsubdir/nested.txt\n" blob)))
      ;; Directory: a tree entry.
      (should (equal (egix-ls-tree-entry repo "HEAD" "subdir")
                     (format "040000 tree %s\tsubdir\n" tree)))
      ;; No entry -> "".
      (should (equal (egix-ls-tree-entry repo "HEAD" "no-such-file") ""))
      ;; Unresolved rev signals.
      (should-error (egix-ls-tree-entry repo "no-such-rev" "README.md")))))

(ert-deftest egix-test-log ()
  "egix-log walks HEAD newest-first, honours the limit, and expands %D
decorations (full names, HEAD first, symbolic refs under their own name) and
the mailmap author name."
  (egix-test--with-fresh-test-repo
    (egix-test--shell "git tag v1")
    (egix-test--shell "git branch feature")
    ;; A remote-tracking ref plus a symbolic remote HEAD pointing at it, both
    ;; on HEAD: git decorates the symbolic ref under its own name.
    (egix-test--shell "git update-ref refs/remotes/origin/main HEAD")
    (egix-test--shell "git symbolic-ref refs/remotes/origin/HEAD refs/remotes/origin/main")
    (let* ((repo (egix-repo-discover default-directory))
           (out (egix-log repo nil 2 "%h%x0c%D%x0c%aN%x0c%s"))
           (lines (split-string out "\n" t)))
      ;; -n limit honoured.
      (should (= (length lines) 2))
      (let ((fields (split-string (car lines) "\f")))
        ;; %h: an abbreviated hex hash.
        (should (string-match-p "\\`[0-9a-f]+\\'" (nth 0 fields)))
        ;; %D: HEAD's decorations, full names, HEAD -> first.
        (should (string-prefix-p "HEAD -> refs/heads/" (nth 1 fields)))
        (should (string-search "tag: refs/tags/v1" (nth 1 fields)))
        (should (string-search "refs/heads/feature" (nth 1 fields)))
        ;; The symbolic ref keeps its own name, not renamed to its target.
        (should (string-search "refs/remotes/origin/HEAD" (nth 1 fields)))
        (should-not (string-match-p "origin/main.*origin/main" (nth 1 fields)))
        ;; %aN: mailmap-resolved author name.
        (should (equal (nth 2 fields) "Test User")))
      ;; A range rev is unsupported (caller falls back to git).
      (should-error (egix-log repo "HEAD~1..HEAD" 10 "%h")))))

(ert-deftest egix-test-index-differs-from-head ()
  "egix-index-differs-from-head matches `git diff --quiet --cached -- FILE'."
  (egix-test--with-fresh-test-repo
    ;; Baseline files, then stage a variety of changes.
    (dolist (f '("clean.txt" "mod.txt" "del.txt" "mode.txt"))
      (write-region (concat f "\n") nil (expand-file-name f egix-test-repo-path)))
    (egix-test--shell "git add clean.txt mod.txt del.txt mode.txt")
    (egix-test--shell "git commit -qm baseline")
    (write-region "changed\n" nil (expand-file-name "mod.txt" egix-test-repo-path))
    (egix-test--shell "git add mod.txt")                 ; staged modification
    (egix-test--shell "git rm -q --cached del.txt")      ; staged deletion
    (egix-test--shell "git update-index --chmod=+x mode.txt") ; staged mode change
    (write-region "new\n" nil (expand-file-name "add.txt" egix-test-repo-path))
    (egix-test--shell "git add add.txt")                 ; staged addition
    (write-region "ita\n" nil (expand-file-name "ita.txt" egix-test-repo-path))
    (egix-test--shell "git add -N ita.txt")              ; intent-to-add
    (let ((repo (egix-repo-discover default-directory)))
      (should-not (egix-index-differs-from-head repo "clean.txt"))
      (should     (egix-index-differs-from-head repo "mod.txt"))
      (should     (egix-index-differs-from-head repo "del.txt"))
      (should     (egix-index-differs-from-head repo "mode.txt"))
      (should     (egix-index-differs-from-head repo "add.txt"))
      ;; Intent-to-add is NOT a staged change (git exits 0).
      (should-not (egix-index-differs-from-head repo "ita.txt"))
      ;; A path with no entry in index or HEAD.
      (should-not (egix-index-differs-from-head repo "no-such-file")))))

(ert-deftest egix-test-index-differs-from-head-unborn ()
  "With an unborn HEAD, a file in the index counts as staged."
  (egix-test--with-fresh-test-repo
    (let ((unborn (make-temp-file "egix-unborn-" t)))
      (unwind-protect
          (let ((default-directory (file-name-as-directory unborn)))
            (egix-test--shell "git init -q")
            (egix-test--shell "git config user.email t@e.com")
            (egix-test--shell "git config user.name t")
            (write-region "z\n" nil (expand-file-name "h.txt" unborn))
            (egix-test--shell "git add h.txt")
            (let ((repo (egix-repo-discover default-directory)))
              (should (egix-index-differs-from-head repo "h.txt"))))
        (delete-directory unborn t)))))

(ert-deftest egix-test-index-differs-from-head-defers ()
  "Submodule and unmerged entries signal, so the caller falls back to git."
  (egix-test--with-fresh-test-repo
    (let ((sub (make-temp-file "egix-sub-" t)))
      (unwind-protect
          (progn
            ;; A submodule at path "sub" (added before the conflict, since git
            ;; refuses `submodule add' with an unmerged index).
            (let ((default-directory (file-name-as-directory sub)))
              (egix-test--shell "git init -q")
              (egix-test--shell "git config user.email t@e.com")
              (egix-test--shell "git config user.name t")
              (write-region "s\n" nil (expand-file-name "s.txt" sub))
              (egix-test--shell "git add s.txt")
              (egix-test--shell "git commit -qm sub"))
            (egix-test--shell
             "git -c protocol.file.allow=always submodule add %s sub"
             (shell-quote-argument sub))
            (egix-test--shell "git commit -qm add-submodule")
            ;; An unmerged entry: provoke a conflict on conflict.txt.
            (write-region "base\n" nil (expand-file-name "conflict.txt" egix-test-repo-path))
            (egix-test--shell "git add conflict.txt")
            (egix-test--shell "git commit -qm base")
            (egix-test--shell "git switch -q -c other")
            (write-region "other\n" nil (expand-file-name "conflict.txt" egix-test-repo-path))
            (egix-test--shell "git commit -qam other")
            (egix-test--shell "git switch -q -")
            (write-region "mine\n" nil (expand-file-name "conflict.txt" egix-test-repo-path))
            (egix-test--shell "git commit -qam mine")
            ;; This merge conflicts; a non-zero exit is expected.
            (call-process "git" nil nil nil "-c" "core.editor=true" "merge" "other")
            (let ((repo (egix-repo-discover default-directory)))
              (should-error (egix-index-differs-from-head repo "sub"))
              (should-error (egix-index-differs-from-head repo "conflict.txt"))))
        (delete-directory sub t)))))

(provide 'egix-test)

;;; egix-test.el ends here

