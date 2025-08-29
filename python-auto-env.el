;;; python-auto-env.el --- Automatic Python virtual environment detection and activation -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Laluxx
;; Version: 1.0.0
;; Keywords: python, virtualenv, tools
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/laluxx/python-auto-env

;;; Commentary:

;; TODO Support multiple inferior python repl processes
;; and associate each one with a file and it's virutal enviroment

;; This package provides automatic detection and activation of Python virtual
;; environments using a two-pass strategy:
;; 1. Look for common virtualenv directory names in current working directory
;; 2. Look for directories that match expected virtualenv structure

;;; Code:

(defgroup python-auto-env nil
  "Automatic Python virtual environment detection and activation."
  :group 'python
  :prefix "python-auto-env-")

(defcustom python-auto-env-common-names
  '("env" "venv" ".env" ".venv" "virtualenv")
  "List of common virtual environment directory names to search for."
  :type '(repeat string)
  :group 'python-auto-env)

(defcustom python-auto-env-required-files
  '("pyvenv.cfg")
  "List of files that must be present in a valid virtual environment."
  :type '(repeat string)
  :group 'python-auto-env)

(defcustom python-auto-env-required-dirs
  '("bin" "lib")
  "List of directories that must be present in a valid virtual environment."
  :type '(repeat string)
  :group 'python-auto-env)

(defcustom python-auto-env-auto-activate t
  "Whether to automatically activate virtual environments when opening Python files."
  :type 'boolean
  :group 'python-auto-env)

(defcustom python-auto-env-message-level 'info
  "Level of messages to display when detecting virtual environments.
Options: 'silent, 'info, 'verbose"
  :type '(choice (const :tag "Silent" silent)
                 (const :tag "Info" info)
                 (const :tag "Verbose" verbose))
  :group 'python-auto-env)

(defcustom python-auto-env-search-parent-dirs t
  "Whether to search parent directories for virtual environments."
  :type 'boolean
  :group 'python-auto-env)

(defcustom python-auto-env-max-parent-depth 3
  "Maximum number of parent directories to search."
  :type 'integer
  :group 'python-auto-env)

(defvar python-auto-env--cache (make-hash-table :test 'equal)
  "Cache for detected virtual environments to avoid repeated filesystem checks.")

(defun python-auto-env--log (level message &rest args)
  "Log MESSAGE at LEVEL with ARGS if logging is enabled."
  (when (and (not (eq python-auto-env-message-level 'silent))
             (or (eq python-auto-env-message-level 'verbose)
                 (eq level 'info)))
    (apply #'message (concat "[python-auto-env] " message) args)))

(defun python-auto-env--is-valid-venv-p (directory)
  "Check if DIRECTORY contains a valid virtual environment structure."
  (and (file-directory-p directory)
       ;; Check for required files
       (cl-every (lambda (file)
                   (file-exists-p (expand-file-name file directory)))
                 python-auto-env-required-files)
       ;; Check for required directories
       (cl-every (lambda (dir)
                   (file-directory-p (expand-file-name dir directory)))
                 python-auto-env-required-dirs)))

(defun python-auto-env--find-by-name (base-dir)
  "Find virtual environment by common names in BASE-DIR.
This is the first pass of the detection strategy."
  (python-auto-env--log 'verbose "Searching for common venv names in: %s" base-dir)
  (catch 'found
    (dolist (name python-auto-env-common-names)
      (let ((venv-path (expand-file-name name base-dir)))
        (when (python-auto-env--is-valid-venv-p venv-path)
          (python-auto-env--log 'verbose "Found venv by name: %s" venv-path)
          (throw 'found venv-path))))
    nil))

(defun python-auto-env--find-by-structure (base-dir)
  "Find virtual environment by checking directory structure in BASE-DIR.
This is the second pass of the detection strategy."
  (python-auto-env--log 'verbose "Searching for venv by structure in: %s" base-dir)
  (when (file-directory-p base-dir)
    (catch 'found
      (dolist (entry (directory-files base-dir t "^[^.]"))
        (when (and (file-directory-p entry)
                   (python-auto-env--is-valid-venv-p entry))
          (python-auto-env--log 'verbose "Found venv by structure: %s" entry)
          (throw 'found entry)))
      nil)))

(defun python-auto-env--find-in-directory (directory)
  "Find virtual environment in DIRECTORY using two-pass strategy."
  (or
   ;; Pass 1: Look for common names
   (python-auto-env--find-by-name directory)
   ;; Pass 2: Look for valid structure
   (python-auto-env--find-by-structure directory)))

(defun python-auto-env--find-venv ()
  "Find virtual environment using configured search strategy."
  (let* ((current-dir (file-name-directory (or buffer-file-name default-directory)))
         (cache-key current-dir)
         (cached-result (gethash cache-key python-auto-env--cache)))
    
    ;; Return cached result if available
    (if cached-result
        (progn
          (python-auto-env--log 'verbose "Using cached result: %s" cached-result)
          cached-result)
      
      ;; Search for virtual environment
      (let ((found-venv (catch 'found
                          ;; Search current directory
                          (let ((venv (python-auto-env--find-in-directory current-dir)))
                            (when venv
                              (throw 'found venv)))
                          
                          ;; Search parent directories if enabled
                          (when python-auto-env-search-parent-dirs
                            (let ((parent-dir current-dir)
                                  (depth 0))
                              (while (and (< depth python-auto-env-max-parent-depth)
                                          (not (string= parent-dir (directory-file-name parent-dir))))
                                (setq parent-dir (file-name-directory (directory-file-name parent-dir)))
                                (setq depth (1+ depth))
                                (let ((venv (python-auto-env--find-in-directory parent-dir)))
                                  (when venv
                                    (throw 'found venv))))))
                          nil)))
        
        ;; Cache the result (even if nil)
        (puthash cache-key found-venv python-auto-env--cache)
        found-venv))))

(defun python-auto-env-activate ()
  "Activate virtual environment for current buffer."
  (interactive)
  (when (derived-mode-p 'python-mode)
    (let ((venv-path (python-auto-env--find-venv)))
      (if venv-path
          (progn
            (setq-local python-shell-virtualenv-root venv-path)
            (python-auto-env--log 'info "Activated virtual environment: %s" 
                                   (file-name-nondirectory (directory-file-name venv-path))))
        (python-auto-env--log 'info "No virtual environment found")))))

(defun python-auto-env-deactivate ()
  "Deactivate virtual environment for current buffer."
  (interactive)
  (when (derived-mode-p 'python-mode)
    (setq-local python-shell-virtualenv-root nil)
    (python-auto-env--log 'info "Deactivated virtual environment")))

(defun python-auto-env-clear-cache ()
  "Clear the virtual environment detection cache."
  (interactive)
  (clrhash python-auto-env--cache)
  (python-auto-env--log 'info "Cleared virtual environment cache"))

(defun python-auto-env-show-current ()
  "Show currently activated virtual environment."
  (interactive)
  (if python-shell-virtualenv-root
      (message "Current virtual environment: %s" python-shell-virtualenv-root)
    (message "No virtual environment activated")))

(defun python-auto-env--setup-buffer ()
  "Set up virtual environment for current Python buffer."
  (when python-auto-env-auto-activate
    (python-auto-env-activate)))

;;;###autoload
(define-minor-mode python-auto-env-mode
  "Minor mode for automatic Python virtual environment detection."
  :global nil
  :lighter " AutoEnv"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c v a") #'python-auto-env-activate)
            (define-key map (kbd "C-c v d") #'python-auto-env-deactivate)
            (define-key map (kbd "C-c v c") #'python-auto-env-clear-cache)
            (define-key map (kbd "C-c v s") #'python-auto-env-show-current)
            map)
  (if python-auto-env-mode
      (python-auto-env--setup-buffer)
    (python-auto-env-deactivate)))

;;;###autoload
(define-globalized-minor-mode python-auto-env-global-mode
  python-auto-env-mode
  (lambda ()
    (when (derived-mode-p 'python-mode)
      (python-auto-env-mode 1)))
  :group 'python-auto-env)

;; Integration with existing Python configuration
(defun python-auto-env--integrate-with-python-config ()
  "Integrate python-auto-env with existing Python configuration."
  (when (derived-mode-p 'python-mode)
    (python-auto-env--setup-buffer)))

;; Hook for manual activation
;;;###autoload
(defun python-auto-env-setup-hooks ()
  "Set up hooks for python-auto-env."
  (add-hook 'python-mode-hook #'python-auto-env--integrate-with-python-config))

(provide 'python-auto-env)
