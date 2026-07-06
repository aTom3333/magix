;;; magix.el --- Gitoxide-powered Magit acceleration -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Thomas Ferrand
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (magit "4.5"))
;; Keywords: git, magit, performance
;; URL: https://github.com/aTom3333/magix

;;; Commentary:

;;; Code:

;; add egix subfoler to load-path so egix can be loaded
(add-to-list 'load-path 
  (expand-file-name "egix" (file-name-directory (or load-file-name buffer-file-name))))

(require 'cl-lib)
(require 'magit)
(require 'egix)

(defgroup magix nil
  "Gitoxide-powered Magit acceleration."
  :group 'magit
  :prefix "magix-")

(defcustom magix-debug-mode nil
  "When non-nil, compare results from gitoxide and original Magit functions.
If results differ, a warning is logged to the *magix-debug* buffer."
  :type 'boolean
  :group 'magix)

(defcustom magix-excluded-repositories nil
  "List of repository roots where magix acceleration should be disabled.
Each entry should be an absolute path to a repository root directory."
  :type '(repeat directory)
  :group 'magix)

(defcustom magix-record-stats nil
  "When non-nil, record every git invocation seen by the magix advice.
In-flight stats accumulate in `magix--stats'; they are periodically flushed
to `magix-stats-file' (and on Emacs exit) by `magix-save-stats', which
also clears the in-memory hash. Inspect aggregated history with
`magix-dump-stats' or wipe everything with `magix-clear-stats'.
When `magix-debug-mode' is also on, the time spent running git for
comparison is excluded from the recorded duration."
  :type 'boolean
  :group 'magix)

(defcustom magix-stats-file (locate-user-emacs-file "magix-stats.eld")
  "File where magix persists accumulated stats across sessions.
This file is the canonical store. Each Emacs session keeps only the data
collected since its last save in memory; on save, the file is read,
merged with the in-flight data, written back, and the in-memory hash is
cleared. This narrows the race window when multiple Emacs instances
write concurrently."
  :type 'file
  :group 'magix)

(defcustom magix-stats-save-interval 60
  "Seconds between automatic flushes of in-flight stats to `magix-stats-file'.
The timer is started when `magix-mode' is enabled and stopped when it
is disabled. A flush is a no-op when nothing has been recorded since
the previous save."
  :type 'integer
  :group 'magix)

(defvar magix-strict-dispatch nil
  "When non-nil, the dispatcher only swallows `rust-error' signals from egix.
Other elisp errors raised inside a dispatcher arm (for instance
`wrong-number-of-arguments' from a mis-shaped egix call) propagate to
the caller. When nil, all errors are swallowed and the call falls back
to the git CLI.")

(defvar magix--advised-functions nil
  "List of functions that have been successfully advised.")

(defvar magix--stats (make-hash-table :test 'equal)
  "In-flight git invocation stats, keyed by signature string.
Each value is a vector [count intercepted-count total-duration example-args].
Only holds data collected since the last save; the canonical accumulated
history lives in `magix-stats-file'.")

(defvar magix--stats-save-timer nil
  "Repeating timer that periodically flushes `magix--stats' to disk.
Managed by `magix-mode'.")

(defun magix--normalize-path (path)
  "Canonicalize PATH (resolve symlinks/`..', uppercase Windows drive letter)."
  (let ((normalized (directory-file-name (file-truename (expand-file-name path)))))
    (if (string-match "^[a-zA-Z]:" normalized)
        (concat (upcase (substring normalized 0 1)) (substring normalized 1))
      normalized)))

(defun magix--check-function-signature (func-symbol expected-args)
  "Check if FUNC-SYMBOL exists and has compatible signature with EXPECTED-ARGS.
Returns t if compatible, nil otherwise."
  (when (fboundp func-symbol)
    (condition-case nil
        (let* ((args (help-function-arglist func-symbol))
               (required (seq-take-while (lambda (arg) 
                                          (not (memq arg '(&optional &rest)))) 
                                        args))
               (expected-required (seq-take-while (lambda (arg)
                                                   (not (memq arg '(&optional &rest))))
                                                 expected-args)))
          ;; Check that required args count matches
          (= (length required) (length expected-required)))
      (error nil))))

(defun magix--log-mismatch (func-name args magix-result original-result)
  "Log a mismatch between MAGIX-RESULT and ORIGINAL-RESULT for FUNC-NAME with ARGS."
  (let ((dir default-directory)) ; capture directory before working on another buffer
    (with-current-buffer (get-buffer-create "*magix-debug*")
      (goto-char (point-max))
      (insert (format "\n=== MISMATCH DETECTED ===\n"))
      (insert (format "Function: %s\n" func-name))
      (insert (format "Time: %s\n" (current-time-string)))
      (insert (format "Directory: %s\n" dir))
      (insert (format "Args: %S\n" args))
      (insert (format "Magix result: %S\n" magix-result))
      (insert (format "Original result: %S\n" original-result))
      (insert (format "========================\n\n"))))
  (message "Magix: Mismatch detected in %s - see *magix-debug* buffer" func-name)
  (error "Mismatch detected"))

(defun magix--should-accelerate-p ()
  "Return non-nil if magix acceleration should be active in current context.
Checks if current directory is in an excluded repository or accessed via TRAMP."
  (and magix-mode
       ;; Don't accelerate for remote (TRAMP) repositories
       (not (file-remote-p default-directory))
       ;; Don't accelerate for excluded repositories
       (not (seq-some (lambda (excluded-repo)
                       (string-prefix-p (expand-file-name excluded-repo)
                                      (expand-file-name default-directory)))
                     magix-excluded-repositories))))

(defun magix--home-is-process-local-p ()
  "Return non-nil when HOME is set for this process but not for subprocesses.
Emacs synthesizes HOME (e.g. from %APPDATA% on Windows) when the launching
environment did not provide one. That synthesized value lives in the C
runtime environment but is absent from `process-environment', so child
processes such as git never see it and fall back to their own home
resolution. egix runs in-process and would otherwise read the synthesized
value, disagreeing with git over which config files to read. Detect that
case here so the caller can ask egix to ignore it.

`getenv-internal' scoped to `process-environment' reflects exactly what a
subprocess inherits, unlike `getenv', which falls back to the C runtime."
  (null (getenv-internal "HOME" process-environment)))

(defun magix--repo-discover-if-not-inside-gitdir (&optional directory)
  "Discover repo, by usage of egix-repo-discover, in directory DIRECTORY
but only return it if DIRECTORY is not inside gitdir of the repo.
Default value for DIRECTORY is DEFAULT-DIRECTORY
Returns nil if DIRECTORY is inside the gitdir."
  (let* ((directory (or directory default-directory))
         ;; Work around behavior difference between git oxide and git
         ;; when giving a path to a symlink that is inside a git repo and
         ;; that points to a directory in another git repo, git works on
         ;; the repo containing the pointed to directory whereas gix
         ;; returns the repo containing the symlink
         (repo (egix-repo-discover (file-truename directory)
                                   (magix--home-is-process-local-p)))
         (gitdir (egix-repo-gitdir repo))
         (current-dir (expand-file-name directory)))
    (unless (file-in-directory-p current-dir gitdir)
      repo)))

(defun magix--not-option-p (s)
  "Check that s is a string that doesn't start with a -"
  (and (stringp s)
       (not (string-prefix-p "-" s))))

(defun magix--rev-parse-git-dir (repo)
  "Return what `git rev-parse --git-dir' would print for REPO."
  (let* ((gitdir (egix-repo-gitdir repo))
         (workdir (ignore-errors (egix-repo-workdir repo))))
    (cond
     ;; Bare repo: gitdir is the directory; from its root git prints ".".
     ((and (null workdir)
           (file-equal-p default-directory gitdir))
      ".")
     ;; Normal repo (.git is a real directory) and CWD is the worktree root.
     ((and workdir
           (file-equal-p default-directory workdir)
           (let ((attrs (file-attributes (expand-file-name ".git" workdir))))
             ;; file-attributes returns t in the first slot only for plain dirs;
             ;; gitfiles are regular files, symlinks return their target string.
             (eq (car attrs) t)))
      ".git")
     (t (magix--normalize-path gitdir)))))

(defmacro magix--with-repo (&rest body)
  "Run BODY with REPO bound to the discovered repository.

Returns a single-element list (VALUE) when BODY produces an answer — including
the case where VALUE is nil, which means \"definitively no such ref/value\"
(gix did the work and the right answer is nil); callers should treat this as
a final result and not call git.

Returns nil when the dispatcher cannot handle this query: no repository was
discovered, current directory is inside the gitdir, or BODY signalled an error
(typically because the underlying gix function does not implement this revspec
shape). Callers should fall back to the git CLI."
  (declare (indent 0))
  `(condition-case err
       (when-let ((repo (magix--repo-discover-if-not-inside-gitdir)))
         (list (progn ,@body)))
     (rust-error nil)
     (error (if magix-strict-dispatch (signal (car err) (cdr err)) nil))))

(defmacro magix--line (form)
  "Evaluate FORM; if it yields a non-empty string, append a trailing newline.
An empty string stays empty (git writes zero bytes but still exits 0 — e.g. a
revspec that names a valid object with no symbolic name); nil stays nil.
Helper for dispatcher arms that produce single-line git output."
  `(let ((v ,form)) (and v (if (string= v "") v (concat v "\n")))))

(defun magix--format-config-get-all-z (values)
  "Format VALUES as git's `-z --get-all KEY' output: VALUE\\0 per entry.
Returns nil when VALUES is empty so the dispatcher emits exit 1."
  (and values
       (mapconcat (lambda (v) (concat v "\0")) values "")))

(defun magix--format-config-list-z (alist)
  "Format ALIST as git's `-z --list' output: KEY\\nVAL\\0 per entry.
ALIST is a list of (KEY . VALUE) cons cells. Returns nil if empty."
  (and alist
       (mapconcat (lambda (pair) (concat (car pair) "\n" (cdr pair) "\0"))
                  alist "")))

(defun magix--format-remote-names (names)
  "Format NAMES (list of remote-name strings) as `git remote' output.
Each name on its own line. Empty list yields the empty string, so the
dispatcher reports exit 0 with no output (git's behaviour when no remotes
are configured)."
  (mapconcat (lambda (n) (concat n "\n")) names ""))

(defun magix--config-normalize-c-key (raw-key)
  "Apply git's casing rule to RAW-KEY: lowercase section + variable,
preserve subsection case. Returns nil for malformed keys."
  (let ((first-dot (string-search "." raw-key))
        (last-dot (cl-position ?. raw-key :from-end t)))
    (when (and first-dot
               (> first-dot 0)
               (< last-dot (1- (length raw-key))))
      (let ((section (downcase (substring raw-key 0 first-dot)))
            (variable (downcase (substring raw-key (1+ last-dot)))))
        (if (= first-dot last-dot)
            (concat section "." variable)
          (concat section "."
                  (substring raw-key (1+ first-dot) last-dot)
                  "." variable))))))

(defun magix--config-c-overrides (args)
  "Extract `-c KEY=VAL' overrides from ARGS as an alist in declared order.
Mirrors git's `--list' casing (section + variable lowercased, subsection
verbatim) so the overrides slot in cleanly behind the file-based entries."
  (let (result)
    (while args
      (when (and (equal (car args) "-c") (cdr args))
        (let* ((entry (cadr args))
               (eq-pos (string-search "=" entry)))
          (when (and eq-pos (> eq-pos 0))
            (when-let ((key (magix--config-normalize-c-key
                             (substring entry 0 eq-pos))))
              (push (cons key (substring entry (1+ eq-pos))) result)))))
      (setq args (cdr args)))
    (nreverse result)))

(defun magix--config-get-single (key scope default)
  "Dispatch result for the single-value `git config KEY' form.
SCOPE is \"local\", \"global\", \"system\", or nil for all scopes.
DEFAULT, when non-nil, is the string returned if KEY is unset (mirrors
`--default='); when nil, an unset KEY produces git's exit-1-no-output."
  (magix--with-repo
    (let ((value (egix-config-get repo key scope)))
      (cond
       (value (concat value "\n"))
       (default (concat default "\n"))
       (t nil)))))

(defun magix--git-output-dispatch (args)
  "Return raw git output for ARGS using egix, or nil if not handled.

A non-nil result is a single-element list (BYTES); BYTES is what git would
have written to stdout (matching its exact format, including trailing
newlines). BYTES may be nil to signal a definitive empty / not-found
result. nil means \"fall back to git\"."
  (pcase args
    (`("rev-parse" "--show-toplevel")
     (magix--with-repo
       (magix--line (magix--normalize-path (egix-repo-workdir repo)))))
    (`("rev-parse" "--git-dir")
     (magix--with-repo (magix--line (magix--rev-parse-git-dir repo))))
    (`("rev-parse" "--is-bare-repository")
     (magix--with-repo (if (egix-repo-workdir repo) "false\n" "true\n")))
    (`("rev-parse" "--short" ,(and ref (pred magix--not-option-p)))
     (magix--with-repo (magix--line (egix-revparse-short repo ref nil))))
    (`("rev-parse" ,(and arg (guard (string-prefix-p "--short=" arg)))
                   ,(and ref (pred magix--not-option-p)))
     (let ((len (string-to-number (substring arg (length "--short=")))))
       (and (> len 0)
            (magix--with-repo (magix--line (egix-revparse-short repo ref len))))))
    (`("rev-parse" "--verify" "--abbrev-ref" ,(and ref (pred magix--not-option-p)))
     (magix--with-repo (magix--line (egix-revparse-abbrev-ref repo ref))))
    (`("rev-parse" "--verify" "--symbolic-full-name" ,(and ref (pred magix--not-option-p)))
     (magix--with-repo (magix--line (egix-revparse-symbolic-full-name repo ref))))
    (`("rev-parse" ,(and ref (pred magix--not-option-p)))
     (magix--with-repo (magix--line (egix-revparse-single repo ref))))
    (`("rev-parse" "--verify" ,(and ref (pred magix--not-option-p)))
     (magix--with-repo (magix--line (egix-revparse-single repo ref))))
    (`("symbolic-ref" "--short" ,(and ref (pred magix--not-option-p)))
     (magix--with-repo (magix--line (egix-symbolic-ref-short repo ref))))
    (`("symbolic-ref" ,(and ref (pred magix--not-option-p)))
     (magix--with-repo (magix--line (egix-symbolic-ref repo ref))))
    (`("cat-file" "-t" ,(and obj (pred magix--not-option-p)))
     (magix--with-repo (magix--line (egix-object-type repo obj))))
    (`("log" "--no-walk" ,(and fmt (guard (string-prefix-p "--format=" fmt)))
                         ,(and rev (pred magix--not-option-p)) "--")
     (magix--with-repo
       ;; git terminates each entry with a newline even when the format
       ;; expands to nothing; an unresolved rev yields nil (empty/exit-1).
       (when-let ((out (egix-commit-format
                        repo rev (substring fmt (length "--format=")))))
         (concat out "\n"))))
    (`("remote")
     (magix--with-repo
       (magix--format-remote-names (egix-remote-names repo))))
    (`("remote" "get-url" ,(and name (pred magix--not-option-p)))
     (magix--with-repo (magix--line (egix-remote-get-url repo name))))
    (`("config" "-z" "--get-all" "--include" ,(and key (pred magix--not-option-p)))
     (magix--with-repo
       (magix--format-config-get-all-z (egix-config-get-all repo key nil))))
    (`("config" "--local" "-z" "--get-all" "--include" ,(and key (pred magix--not-option-p)))
     (magix--with-repo
       (magix--format-config-get-all-z (egix-config-get-all repo key "local"))))
    (`("config" "--list" "-z")
     (magix--with-repo
       (magix--format-config-list-z
        (append (egix-config-list repo nil)
                (magix--config-c-overrides magit-git-global-arguments)))))
    (`("config" "--global" ,(and key (pred magix--not-option-p)))
     (magix--config-get-single key "global" nil))
    (`("config" ,(and arg (guard (string-prefix-p "--default=" arg)))
                ,(and key (pred magix--not-option-p)))
     (magix--config-get-single key nil (substring arg (length "--default="))))
    (_ nil)))

(defun magix--destination-is-current-buffer-p (destination)
  "Return non-nil when DESTINATION causes stdout to land in the current buffer.
Recognises the forms used by magit on the hot path (t or (t ...))."
  (or (eq destination t)
      (and (consp destination) (eq (car destination) t))))

(defun magix-magit-process-git (orig-func destination &rest args)
  "Intercept `magit-process-git'. Single chokepoint for every git invocation
in magit (all wrappers funnel through here)."
  (let* ((flat-args (flatten-tree args))
         (start (and magix-record-stats (current-time)))
         (dispatched (and (magix--should-accelerate-p)
                          (magix--git-output-dispatch flat-args)))
         (writes-current (magix--destination-is-current-buffer-p destination))
         (intercepted (and dispatched writes-current))
         (debug-orig-duration 0.0)
         (result
          (cond
           ;; Not handled, or unfamiliar destination shape — pass through.
           ((not intercepted)
            (apply orig-func destination args))
           ;; Debug-mode: run git for real, capture what it inserted, compare.
           (magix-debug-mode
            (let* ((before (point))
                   (magix-bytes (or (car dispatched) ""))
                   (magix-exit (if (car dispatched) 0 1))
                   ;; Time orig-func separately so it can be excluded from
                   ;; the stats duration — debug-mode comparison cost is not
                   ;; the operation's real cost.
                   (orig-start (and start (current-time)))
                   (orig-exit (apply orig-func destination args))
                   (orig-bytes (buffer-substring-no-properties before (point))))
              (when orig-start
                (setq debug-orig-duration (float-time (time-since orig-start))))
              (unless (and (equal magix-bytes orig-bytes)
                           (eq (zerop magix-exit) (zerop orig-exit)))
                (magix--log-mismatch 'magit-process-git (list :args flat-args)
                                      (cons magix-bytes magix-exit)
                                      (cons orig-bytes orig-exit)))
              orig-exit))
           ;; Definitive not-found: write nothing, exit non-zero.
           ((null (car dispatched)) 1)
           ;; Found: write magix bytes, exit zero.
           (t (insert (car dispatched)) 0))))
    (when start
      (magix--stats-record flat-args (and intercepted t)
                            (- (float-time (time-since start))
                               debug-orig-duration)))
    result))


(defun magix--stats-signature (args)
  "Return a normalized signature string for ARGS.
The first token (the subcommand) is kept verbatim. After that, flag-shaped
tokens (starting with `-') are kept verbatim and every other token is
replaced with `<arg>', so calls that differ only in ref/path values
aggregate together."
  (let ((first t))
    (mapconcat (lambda (a)
                 (cond
                  (first (setq first nil) (format "%s" a))
                  ((and (stringp a) (string-prefix-p "-" a)) a)
                  ((stringp a) "<arg>")
                  (t (format "%S" a))))
               args " ")))

(defun magix--stats-record (args intercepted duration)
  "Aggregate one git invocation into `magix--stats'."
  (let* ((sig (magix--stats-signature args))
         (cell (gethash sig magix--stats)))
    (unless cell
      (setq cell (vector 0 0 0.0 args))
      (puthash sig cell magix--stats))
    (aset cell 0 (1+ (aref cell 0)))
    (when intercepted (aset cell 1 (1+ (aref cell 1))))
    (aset cell 2 (+ (aref cell 2) duration))))

(defun magix--stats-read-file ()
  "Return a fresh hash table built from `magix-stats-file'.
Empty hash if the file does not exist, is empty, or fails to parse."
  (let ((h (make-hash-table :test 'equal)))
    (when (and (file-exists-p magix-stats-file)
               (> (file-attribute-size (file-attributes magix-stats-file)) 0))
      (condition-case err
          (let ((entries (with-temp-buffer
                           (insert-file-contents magix-stats-file)
                           (goto-char (point-min))
                           (read (current-buffer)))))
            (dolist (entry entries)
              (let* ((sig (car entry))
                     (plist (cdr entry))
                     (count (or (plist-get plist :count) 0))
                     (interc (or (plist-get plist :intercepted) 0))
                     (total (or (plist-get plist :total) 0.0))
                     (example (plist-get plist :example)))
                (puthash sig (vector count interc total example) h))))
        (error
         (message "Magix: failed to read stats from %s: %s"
                  magix-stats-file (error-message-string err)))))
    h))

(defun magix--stats-merge-into (target source)
  "Add every cell in SOURCE hash to TARGET hash. Mutates TARGET."
  (maphash
   (lambda (sig src-cell)
     (let ((dst-cell (gethash sig target)))
       (if dst-cell
           (progn
             (aset dst-cell 0 (+ (aref dst-cell 0) (aref src-cell 0)))
             (aset dst-cell 1 (+ (aref dst-cell 1) (aref src-cell 1)))
             (aset dst-cell 2 (+ (aref dst-cell 2) (aref src-cell 2))))
         (puthash sig (copy-sequence src-cell) target))))
   source))

(defun magix--stats-write-file (table)
  "Persist hash TABLE to `magix-stats-file'."
  (let (entries)
    (maphash (lambda (sig cell)
               (push (list sig
                           :count (aref cell 0)
                           :intercepted (aref cell 1)
                           :total (aref cell 2)
                           :example (aref cell 3))
                     entries))
             table)
    (with-temp-file magix-stats-file
      (let ((print-length nil)
            (print-level nil))
        (prin1 entries (current-buffer))
        (insert "\n")))))

(defun magix-save-stats ()
  "Flush in-flight stats to `magix-stats-file', then clear them from memory.
Reads the file, additively merges `magix--stats' into it, writes it back,
and resets `magix--stats'. No-op when nothing has been recorded since the
last save.

Concurrent Emacs instances each hold only their since-last-save data, so
on a save each only contributes its own delta to the file. A race is still
possible between a read and a write here, but the window is narrow."
  (interactive)
  (when (> (hash-table-count magix--stats) 0)
    (let ((merged (magix--stats-read-file)))
      (magix--stats-merge-into merged magix--stats)
      (magix--stats-write-file merged)
      (clrhash magix--stats))))

(defun magix--save-stats-on-exit ()
  "Persist any in-flight stats from `kill-emacs-hook'."
  (ignore-errors (magix-save-stats)))

(defun magix--stats-start-timer ()
  "Start the periodic save timer if it is not already running."
  (unless magix--stats-save-timer
    (setq magix--stats-save-timer
          (run-at-time magix-stats-save-interval
                       magix-stats-save-interval
                       #'magix-save-stats))))

(defun magix--stats-stop-timer ()
  "Cancel the periodic save timer if running."
  (when magix--stats-save-timer
    (cancel-timer magix--stats-save-timer)
    (setq magix--stats-save-timer nil)))

(defun magix-clear-stats ()
  "Discard all accumulated stats, both in memory and on disk.
Empties `magix--stats' and deletes `magix-stats-file' if it exists."
  (interactive)
  (clrhash magix--stats)
  (when (file-exists-p magix-stats-file)
    (delete-file magix-stats-file))
  (message "Magix stats cleared"))

(defun magix-dump-stats ()
  "Display aggregated stats sorted by total time spent.
Shows the combined view of `magix-stats-file' plus any in-flight data
not yet saved. Rows are signatures (flags kept, ref/path arguments
masked as `<arg>') so calls differing only by ref aggregate together;
the top rows are the highest-value candidates to consider for
interception."
  (interactive)
  (let ((view (magix--stats-read-file))
        rows
        (total-count 0)
        (total-time 0.0)
        (total-intercepted 0))
    (magix--stats-merge-into view magix--stats)
    (maphash (lambda (sig cell)
               (push (cons sig cell) rows)
               (setq total-count (+ total-count (aref cell 0))
                     total-intercepted (+ total-intercepted (aref cell 1))
                     total-time (+ total-time (aref cell 2))))
             view)
    (setq rows (sort rows (lambda (a b)
                            (> (aref (cdr a) 2) (aref (cdr b) 2)))))
    (with-current-buffer (get-buffer-create "*magix-stats*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Magix git-invocation stats — %d calls, %.3fs total, %d intercepted (%.1f%%)\n\n"
                        total-count total-time total-intercepted
                        (if (zerop total-count) 0.0
                          (/ (* 100.0 total-intercepted) total-count))))
        (insert (format "%7s %8s %10s %10s  %s\n"
                        "Count" "Interc.%" "Total(s)" "Mean(ms)" "Signature  [example]"))
        (insert (make-string 78 ?-)) (insert "\n")
        (dolist (row rows)
          (let* ((sig (car row))
                 (cell (cdr row))
                 (count (aref cell 0))
                 (interc (aref cell 1))
                 (total (aref cell 2))
                 (example (aref cell 3)))
            (insert (format "%7d %7.1f%% %10.3f %10.2f  %s\n"
                            count
                            (/ (* 100.0 interc) count)
                            total
                            (* 1000.0 (/ total count))
                            sig))
            (when (not (equal sig (mapconcat #'identity example " ")))
              (insert (format "%41s  [%s]\n" "" (mapconcat #'identity example " "))))))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer "*magix-stats*")))

(define-minor-mode magix-mode
  "Toggle gitoxide-powered Magit acceleration.

When enabled, certain Magit operations will use the faster
gitoxide implementation instead of calling Git CLI commands."
  :global t
  :group 'magix
  :lighter " Magix"
  (if magix-mode
      (progn
        ;; Ensure egix is loaded
        (unless (featurep 'egix-module)
          (egix-load-module))

        ;; Add around advice to Magit functions with signature checking
        (setq magix--advised-functions nil)

        (when (magix--check-function-signature 'magit-process-git '(destination &rest args))
          (advice-add 'magit-process-git :around #'magix-magit-process-git)
          (push 'magit-process-git magix--advised-functions))

        (add-hook 'kill-emacs-hook #'magix--save-stats-on-exit)
        (magix--stats-start-timer)

        (if magix--advised-functions
            (message "Magix acceleration enabled (%d functions advised)"
                     (length magix--advised-functions))
          (message "Magix: Warning - No compatible Magit functions found. Your Magit version may be incompatible.")))
    ;; Remove advice from all advised functions
    (dolist (func magix--advised-functions)
      (advice-remove func (intern (format "magix-%s" func))))
    (setq magix--advised-functions nil)
    (magix--stats-stop-timer)
    (magix--save-stats-on-exit)
    (remove-hook 'kill-emacs-hook #'magix--save-stats-on-exit)
    (message "Magix acceleration disabled")))

(provide 'magix)

;;; magix.el ends here
