;;; egix.el --- Emacs bindings for gitoxide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: git, vc
;; URL: https://github.com/yourusername/egix

;;; Commentary:

;; Egix provides Emacs Lisp bindings for gitoxide (a fast Rust-based
;; Git implementation) through an Emacs dynamic module.
;;
;; This package exposes core gitoxide functionality to Emacs without
;; any dependency on Magit or other Git interfaces.

;;; Code:

(defgroup egix nil
  "Emacs bindings for gitoxide."
  :group 'vc
  :prefix "egix-")

;; Because cargo automatically adds a "lib" prefix on linux (and macos?)
;; but emacs doesn't look for file starting with "lib" when doing (require)
;; we don't use (require) and use (module-load) manually instead
(defconst egix--module-name
  (let ((prefix (if (eq system-type 'windows-nt) "" "lib")))
    (concat prefix "egix_module" module-file-suffix))
  "Name of the dynamic module name")

(defconst egix--root
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst egix--module-directory
  (expand-file-name "target/release" egix--root)
  "Directory where the egix native module is built.")

(defun egix--module-filename ()
  (expand-file-name egix--module-name egix--module-directory))

;;;###autoload
(defun egix-load-module ()
  "Load the egix native module."
  (interactive)
  (unless (featurep 'egix-module)
    (condition-case err
        (module-load (egix--module-filename))
      (error
       (error "Egix module could not be loaded: %s" (error-message-string err))))))

;; TODO run the build in an async process
;;;###autoload
(defun egix-build-and-load ()
  "Build the module if it doesn't already exist.
cargo must be in the PATH."
  (if (executable-find "cargo")
      ;; Run cargo even if the module has already been built for now
      ;; cargo is smart enough to not do real work if not needed
      (let ((default-directory egix--root))
        (message "egix-module: building Rust module with cargo…")
        (unless (eq 0 (call-process "cargo" nil "*egix-module-build*" t
                                    "build" "--release"))
          (error "egix-module: cargo build failed")))
    (error "egix: cargo not found, the module cannot be built"))
  (egix-load-module))


;; Automatically load the module when this file is loaded
(egix-build-and-load)

(provide 'egix)

;;; egix.el ends here
