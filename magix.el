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
Records are kept in `magix--stats-log' and can be inspected with
`magix-dump-stats' or reset with `magix-clear-stats'."
  :type 'boolean
  :group 'magix)

(defvar magix--advised-functions nil
  "List of functions that have been successfully advised.")

(defvar magix--stats-log nil
  "List of recorded git invocations, most recent first.
Each entry is a plist with keys :args (list of strings),
:intercepted (boolean — whether magix served the answer itself),
and :duration (seconds, float).")

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
         (repo (egix-repo-discover (file-truename directory)))
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
  `(condition-case nil
       (when-let ((repo (magix--repo-discover-if-not-inside-gitdir)))
         (list (progn ,@body)))
     (error nil)))

(defmacro magix--line (form)
  "Evaluate FORM; if it yields a non-nil string, append a trailing newline.
Helper for dispatcher arms that produce single-line git output."
  `(let ((v ,form)) (and v (concat v "\n"))))

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
     (magix--with-repo (magix--line (egix-revparse-short repo ref))))
    (`("rev-parse" "--verify" "--abbrev-ref" ,(and ref (pred magix--not-option-p)))
     (magix--with-repo (magix--line (egix-revparse-abbrev-ref repo ref))))
    (`("rev-parse" ,(and ref (pred magix--not-option-p)))
     (magix--with-repo (magix--line (egix-revparse-single repo ref))))
    (`("rev-parse" "--verify" ,(and ref (pred magix--not-option-p)))
     (magix--with-repo (magix--line (egix-revparse-single repo ref))))
    (`("symbolic-ref" "--short" ,(and ref (pred magix--not-option-p)))
     (magix--with-repo (magix--line (egix-symbolic-ref-short repo ref))))
    (`("symbolic-ref" ,(and ref (pred magix--not-option-p)))
     (magix--with-repo (magix--line (egix-symbolic-ref repo ref))))
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
         (result
          (cond
           ;; Not handled, or unfamiliar destination shape — pass through.
           ((not intercepted)
            (apply orig-func destination args))
           ;; Debug-mode: run git for real, capture what it inserted, compare.
           (magix-debug-mode
            (let ((before (point))
                  (magix-bytes (or (car dispatched) ""))
                  (magix-exit (if (car dispatched) 0 1))
                  orig-exit orig-bytes)
              (setq orig-exit (apply orig-func destination args))
              (setq orig-bytes (buffer-substring-no-properties before (point)))
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
                            (float-time (time-since start))))
    result))


(defun magix--stats-record (args intercepted duration)
  "Push a stats record onto `magix--stats-log'."
  (push (list :args args :intercepted intercepted :duration duration)
        magix--stats-log))

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

(defun magix-clear-stats ()
  "Discard all recorded git invocation stats."
  (interactive)
  (setq magix--stats-log nil)
  (message "Magix stats cleared"))

(defun magix-dump-stats ()
  "Display aggregated stats for recorded git invocations.
Calls are grouped by a normalized signature (flags kept, ref/path
arguments masked as `<arg>'), then sorted by total time spent so the
top rows are the highest-value candidates to consider for interception."
  (interactive)
  (let ((records magix--stats-log)
        (groups (make-hash-table :test 'equal)))
    (dolist (rec records)
      (let* ((args (plist-get rec :args))
             (intercepted (plist-get rec :intercepted))
             (duration (plist-get rec :duration))
             (sig (magix--stats-signature args))
             (cell (gethash sig groups)))
        ;; cell = [count intercepted-count total-duration example-args]
        (unless cell
          (setq cell (vector 0 0 0.0 args))
          (puthash sig cell groups))
        (aset cell 0 (1+ (aref cell 0)))
        (when intercepted (aset cell 1 (1+ (aref cell 1))))
        (aset cell 2 (+ (aref cell 2) duration))))
    (let (rows
          (total-count (length records))
          (total-time 0.0)
          (total-intercepted 0))
      (dolist (rec records)
        (setq total-time (+ total-time (plist-get rec :duration)))
        (when (plist-get rec :intercepted)
          (setq total-intercepted (1+ total-intercepted))))
      (maphash (lambda (sig cell) (push (cons sig cell) rows)) groups)
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
      (display-buffer "*magix-stats*"))))

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
        
        (if magix--advised-functions
            (message "Magix acceleration enabled (%d functions advised)"
                     (length magix--advised-functions))
          (message "Magix: Warning - No compatible Magit functions found. Your Magit version may be incompatible.")))
    ;; Remove advice from all advised functions
    (dolist (func magix--advised-functions)
      (advice-remove func (intern (format "magix-%s" func))))
    (setq magix--advised-functions nil)
    (message "Magix acceleration disabled")))

(provide 'magix)

;;; magix.el ends here
