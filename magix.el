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

(defvar magix--advised-functions nil
  "List of functions that have been successfully advised.")

(defun magix--normalize-toplevel-path (path)
  "Normalize PATH for git toplevel comparisons.
Ensure the drive letter is uppercase and resolve to a true path." 
  ;; Supposedly this shouldn't be needed to get correct behavior
  ;; but it is there to get exact same paths when comparing implementations
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

(defmacro magix--advise-override-helper (func-name args orig-func orig-args &rest body)
  "Helper to implement a function that overrides a magit function.
Will run BODY is the repo should be accelerated (see magix--should-accelerate-p).
If magix-debug-mode in non-nil, will additionnaly run the original function
and compare the results.
FUNC-NAME is the symbol of the function for logging.
ARGS is a plist of arguments for logging.
ORIG-FUNC is the original function symbol.
ORIG-ARGS is a list of arguments to pass to the original function.
BODY should evaluate to the magix result.
Automatically checks if acceleration should be enabled via `magix--should-accelerate-p'."
  (declare (indent 4))
  `(if (magix--should-accelerate-p)
       (let ((magix-result (progn ,@body)))
         (if magix-debug-mode
             (let ((original-result (condition-case nil
                                        (apply ,orig-func ,orig-args)
                                      (error nil))))
               (unless (equal magix-result original-result)
                 (magix--log-mismatch ',func-name ,args magix-result original-result))
               original-result)
           magix-result))
     (apply ,orig-func ,orig-args)))

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
  "Return what `git rev-parse --git-dir' would print for REPO from `default-directory'.

Git prints the literal string `.git' only when `.git' in the worktree root is a
regular directory and CWD is that worktree root; bare repos at their own root
print `.'; every other shape (gitfile/--separate-git-dir, linked worktree,
submodule, subdir of a normal repo, etc.) prints the absolute gitdir."
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
     (t gitdir))))

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

(defun magix--git-string-dispatch (args)
  "Return a wrapped git-string result for ARGS using egix, or nil if not handled.

A non-nil result is always a single-element list (VALUE); VALUE may itself be
nil to signal a definitive \"no such ref\" answer. nil means \"fall back to git\"."
  (pcase args
    (`("rev-parse" "--show-toplevel")
     (magix--with-repo
       (magix--normalize-toplevel-path (egix-repo-workdir repo))))
    (`("rev-parse" "--git-dir")
     (magix--with-repo (magix--rev-parse-git-dir repo)))
    (`("rev-parse" "--short" ,(and ref (pred magix--not-option-p)))
     (magix--with-repo (egix-revparse-short repo ref)))
    (`("rev-parse" "--verify" "--abbrev-ref" ,(and ref (pred magix--not-option-p)))
     (magix--with-repo (egix-revparse-abbrev-ref repo ref)))
    (`("rev-parse" ,(and ref (pred magix--not-option-p)))
     (magix--with-repo (egix-revparse-single repo ref)))
    (`("rev-parse" "--verify" ,(and ref (pred magix--not-option-p)))
     (magix--with-repo (egix-revparse-single repo ref)))
    (`("symbolic-ref" "--short" ,(and ref (pred magix--not-option-p)))
     (magix--with-repo (egix-symbolic-ref-short repo ref)))
    (`("symbolic-ref" ,(and ref (pred magix--not-option-p)))
     (magix--with-repo (egix-symbolic-ref repo ref)))
    (_ nil)))

(defun magix-magit-git-string (orig-func &rest args)
  "Intercept `magit-git-string' to serve common queries via egix.
ORIG-FUNC is the original `magit-git-string' function.
ARGS are the git arguments passed to `magit-git-string'."
  (magix--advise-override-helper magit-git-string (list :args args) orig-func args
    (let ((dispatched (magix--git-string-dispatch args)))
      (if dispatched (car dispatched) (apply orig-func args)))))

(defun magix-magit-git-str (orig-func &rest args)
  "Intercept `magit-git-str' to serve common queries via egix.
ORIG-FUNC is the original `magit-git-str' function.
ARGS are the git arguments passed to `magit-git-str'."
  (let ((flat-args (flatten-tree args)))
    (magix--advise-override-helper magit-git-str (list :args flat-args) orig-func args
      (let ((dispatched (magix--git-string-dispatch flat-args)))
        (if dispatched (car dispatched) (apply orig-func args))))))


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
        
        (when (magix--check-function-signature 'magit-git-string '(&rest args))
          (advice-add 'magit-git-string :around #'magix-magit-git-string)
          (push 'magit-git-string magix--advised-functions))

        (when (magix--check-function-signature 'magit-git-str '(&rest args))
          (advice-add 'magit-git-str :around #'magix-magit-git-str)
          (push 'magit-git-str magix--advised-functions))
        
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
