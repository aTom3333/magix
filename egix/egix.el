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

(defvar egix--build-process nil
  "Current egix cargo build process.")

(defun egix--module-filename ()
  (expand-file-name egix--module-name egix--module-directory))

(defun egix--build-in-progress-p ()
  "Returns t if a build of egix-module is in progress"
  (process-live-p egix--build-process))

(defun egix--wait-for-build ()
  "Wait for any in-progress build to finish.
Returns t if the module is now loaded, nil if build failed."
  (when (egix--build-in-progress-p)
    (while (egix--build-in-progress-p)
      (sit-for 0.1)))
  (featurep 'egix-module))

(defun egix--start-build ()
  "Start the egix cargo build process."
  (require 'ansi-color)
  (let* ((default-directory egix--root)
         (display-buffer-alist
          '(("\\*egix-module-build\\*" . (display-buffer-no-window))))
         (buf (compilation-start
               "cargo build --release --color=always"
               nil
               (lambda (_) "*egix-module-build*"))))

    (setq egix--build-process (get-buffer-process buf))

    (with-current-buffer buf
      (add-hook 'compilation-filter-hook
                #'ansi-color-compilation-filter
                nil t)
      (add-hook 'compilation-finish-functions
                (lambda (_buffer status)
                  (setq egix--build-process nil)
                  (if (string-prefix-p "finished" status)
                      (progn
                        (message "egix-module: build finished")
                        (egix-load-module))
                    (message
                     "egix-module: build failed, see *egix-module-build* buffer")))
                nil t))))

;;;###autoload
(defun egix-load-module ()
  "Load the egix native module."
  (interactive)
  (unless (featurep 'egix-module)
    (if (egix--build-in-progress-p)
        (message "egix-module: cannot load module while build is in progress")
      (condition-case err
          (module-load (egix--module-filename))
        (error
         (error "Egix module could not be loaded: %s" (error-message-string err)))))))


;;;###autoload
(defun egix-build-and-load ()
  "Build the module if it doesn't already exist.
cargo must be in the PATH."
  (interactive)
  (unless (executable-find "cargo")
    (error "egix: cargo not found, the module cannot be built"))

  (if (egix--build-in-progress-p)
      (message "egix-module: build already running")
    ;; Run cargo even if the module has already been built for now
    ;; cargo is smart enough to not do real work if not needed
    (egix--start-build)))

;; Automatically load the module when this file is loaded
(egix-build-and-load)

(provide 'egix)

;;; egix.el ends here
