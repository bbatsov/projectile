;;; projectile.el --- Manage and navigate projects in Emacs easily

;; Copyright © 2011-2013 Bozhidar Batsov

;; Author: Bozhidar Batsov
;; URL: https://github.com/bbatsov/projectile
;; Version: 0.8
;; Keywords: project, convenience
;; Package-Requires: ((s "1.0.0") (dash "1.0.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides easy project management and navigation.  The
;; concept of a project is pretty basic - just a folder containing
;; special file.  Currently git, mercurial and bazaar repos are
;; considered projects by default.  If you want to mark a folder
;; manually as a project just create an empty .projectile file in
;; it.  See the README for more details.
;;
;;; Code:

;; requires
(require 'cl)
(require 'easymenu)
(require 'thingatpt)
(require 's)
(require 'dash)
(require 'grep)

(defgroup projectile nil
  "Manage and navigate projects easily."
  :group 'tools
  :group 'convenience)

(defconst projectile-current-version "0.9.0-beta"
  "The current Projectile version.")

(defcustom projectile-use-native-indexing (eq system-type 'windows-nt)
  "Use native Emacs Lisp project indexing."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-enable-caching (eq system-type 'windows-nt)
  "Enable project files caching."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-require-project-root t
  "Require the presence of a project root to operate when true.
Otherwise consider the current directory the project root."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-completion-system 'ido
  "The completion system to be used by Projectile."
  :group 'projectile
  :type 'symbol
  :options '(ido default))

(defcustom projectile-ack-function 'ack-and-a-half
  "The ack function to use."
  :group 'projectile
  :type 'symbol
  :options '(ack-and-a-half default))

(defcustom projectile-keymap-prefix (kbd "C-c p")
  "Projectile keymap prefix."
  :group 'projectile
  :type 'sexp)

(defcustom projectile-cache-file
  (expand-file-name "projectile.cache" user-emacs-directory)
  "The name of Projectile's cache file."
  :group 'projectile
  :type 'string)

(defcustom projectile-tags-command "ctags -Re %s"
  "The command Projectile's going to use to generate a TAGS file."
  :group 'projectile
  :type 'string)

;; variables
(defvar projectile-project-root-files
  '(".projectile" "project.clj" ".git" ".hg" ".bzr" "_darcs"
    "rebar.config" "pom.xml" "build.sbt" "Gemfile")
  "A list of files considered to mark the root of a project.")

(defvar projectile-globally-ignored-files
  '("TAGS")
  "A list of files globally ignored by projectile.")

(defvar projectile-globally-ignored-directories
  '(".idea" ".eunit" ".git" ".hg" ".bzr" "_darcs")
  "A list of directories globally ignored by projectile.")

(defvar projectile-project-compilation-commands
  '(("./rebar compile" .
     (lambda (dir)
       (file-exists-p (expand-file-name "rebar" dir))))
    ("rebar compile" .
     (lambda (dir)
       (and (executable-find "rebar")
            (file-exists-p (expand-file-name "rebar.config" dir)))))
    ("make" .
     (lambda (dir)
       (file-exists-p (expand-file-name "Makefile" dir))))
    )
  "A list of pairs of commands and prerequisite lambdas to perform project compilation.")

(defvar projectile-project-test-commands
  '(("./rebar eunit skip_deps=true" .
     (lambda (dir)
       (file-exists-p (expand-file-name "rebar" dir))))
    ("rebar eunit skip_deps=true" .
     (lambda (dir)
       (and (executable-find "rebar")
            (file-exists-p (expand-file-name "rebar.config" dir)))))
    ("make test" .
     (lambda (dir)
       (file-exists-p (expand-file-name "Makefile" dir))))
    )
  "A list of pairs of commands and prerequisite lambdas to perform project compilation.")

(defvar projectile-projects-cache
  (if (file-exists-p projectile-cache-file)
    (with-temp-buffer
      (insert-file-contents projectile-cache-file)
      (read (buffer-string)))
    (make-hash-table :test 'equal))
  "A hashmap used to cache project file names to speed up related operations.")

(defun projectile-version ()
  "Reports the version of Projectile in use."
  (interactive)
  (message "Projectile (version %s) 2011-2013 Bozhidar Batsov <bozhidar@batsov.com>"
           projectile-current-version))

(defun projectile-invalidate-cache ()
  "Remove the current project's files from `projectile-projects-cache'."
  (interactive)
  (let ((project-root (projectile-project-root)))
    (remhash project-root projectile-projects-cache)
    (projectile-serialize-cache)
    (message "Invalidated Projectile cache for %s."
             (propertize project-root 'face 'font-lock-keyword-face))))

(defun projectile-cache-project (project files)
  "Cache PROJECT's FILES.
The cache is created both in memory and on the hard drive."
  (when projectile-enable-caching
    (puthash project files projectile-projects-cache)
    (projectile-serialize-cache)))

(defun projectile-project-root ()
  "Retrieves the root directory of a project if available.
The current directory is assumed to be the project's root otherwise."
  (or (->> projectile-project-root-files
        (--map (locate-dominating-file default-directory it))
        (-remove #'null)
        (car))
      (if projectile-require-project-root
          (error "You're not into a project")
        default-directory)))

(defun projectile-project-name ()
  "Return project name."
  (file-name-nondirectory (directory-file-name (projectile-project-root))))

(defun projectile-project-files (directory)
  "List the files in DIRECTORY and in its sub-directories."
  ;; check for a cache hit first if caching is enabled
  (let ((files-list (and projectile-enable-caching
                         (gethash directory projectile-projects-cache)))
        (patterns (projectile-rel-patterns)))
    ;; cache disabled or cache miss
    (unless files-list
      (message "Projectile is indexing %s. This may take a while."
               (propertize directory 'face 'font-lock-keyword-face))
      (if projectile-use-native-indexing
          (setq files-list (projectile-index-directory directory patterns))
        ;; use external tools to get the project files
        (let ((current-dir (if (buffer-file-name)
                               (file-name-directory (buffer-file-name))
                             default-directory)))
          ;; the shell commands need to invoked in the project's root dir
          (cd (projectile-project-root))
          (setq files-list (projectile-get-repo-files))
          ;; restore the original current directory
          (message current-dir)
          (cd current-dir)))
      ;; cache the resulting list of files
      (projectile-cache-project directory files-list))
    files-list))

(defcustom projectile-git-command "git ls-files -zco --exclude-standard"
  "Command used by projectile to get the files in a git project."
  :group 'projectile
  :type 'string)

(defcustom projectile-hg-command "hg locate -0"
  "Command used by projectile to get the files in a hg project."
  :group 'projectile
  :type 'string)

(defcustom projectile-bzr-command "bzr ls --versioned -0"
  "Command used by projectile to get the files in a bazaar project."
  :group 'projectile
  :type 'string)

(defcustom projectile-darcs-command "darcs show files -0"
  "Command used by projectile to get the files in a darcs project."
  :group 'projectile
  :type 'string)

(defcustom projectile-svn-command "find . -type f -print0"
  "Command used by projectile to get the files in a svn project."
  :group 'projectile
  :type 'string)

(defcustom projectile-generic-command "find . -type f -print0"
  "Command used by projectile to get the files in a generic project."
  :group 'projectile
  :type 'string)

(defun projectile-get-ext-command ()
  "Determine which external command to invoke based on the project's VCS."
  (let ((vcs (projectile-project-vcs)))
    (cond
     ((eq vcs 'git) projectile-git-command)
     ((eq vcs 'hg) projectile-hg-command)
     ((eq vcs 'bzr) projectile-bzr-command)
     ((eq vcs 'darcs) projectile-darcs-command)
     ((eq vcs 'svn) projectile-svn-command)
     (t projectile-generic-command))))

(defun projectile-get-repo-files ()
  "Get a list of the files in the project."
  (-map 'expand-file-name (projectile-files-via-ext-command (projectile-get-ext-command))))

(defun projectile-files-via-ext-command (command)
  "Get a list of relative file names in the project root by executing COMMAND."
  (split-string (shell-command-to-string command) "\0"))

(defun projectile-index-directory (directory patterns)
  "Index DIRECTORY taking into account PATTERNS.
The function calls itself recursively until all sub-directories
have been indexed."
  (let (files-list)
    (dolist (current-file (file-name-all-completions "" directory) files-list)
      (let ((absolute-file (expand-file-name current-file directory)))
        (cond
         ;; check for directories that are not ignored
         ((and (s-ends-with-p "/" current-file)
               ;; avoid loops & ignore some well known directories
               (not (-any? (lambda (file)
                            (string= (s-chop-suffix "/" current-file) file))
                          '("." ".." ".svn" ".cvs")))
               (not (projectile-ignored-directory-p absolute-file))
               (not (and patterns
                         (projectile-ignored-rel-p directory
                                                   absolute-file patterns))))
          (setq files-list (append files-list
                                   (projectile-index-directory
                                    (expand-file-name current-file directory)
                                    patterns))))
         ;; check for regular files that are not ignored
         ((and (not (s-ends-with-p "/" current-file))
               (not (projectile-ignored-file-p absolute-file))
               (not (and patterns
                         (projectile-ignored-rel-p directory
                                                   absolute-file patterns))))
          (setq files-list (cons
                            (expand-file-name current-file directory)
                            files-list))))))))

(defun projectile-project-buffers ()
  "Get a list of project buffers."
  (let ((project-files (projectile-project-files (projectile-project-root)))
        (buffer-files (-map 'buffer-file-name (buffer-list))))
    (-map 'get-file-buffer
            (intersection project-files buffer-files :test 'string=))))

(defun projectile-project-buffer-names ()
  "Get a list of project buffer names."
  (-map 'buffer-name (projectile-project-buffers)))

(defun projectile-prepend-project-name (string)
  "Prepend the current project's name to STRING."
  (format "[%s] %s" (projectile-project-name) string))

(defun projectile-switch-to-buffer ()
  "Switch to a project buffer."
  (interactive)
  (switch-to-buffer
   (projectile-completing-read
    "Switch to buffer: "
    (projectile-project-buffer-names))))

(defun projectile-multi-occur ()
  "Do a `multi-occur' in the project's buffers."
  (interactive)
  (multi-occur (projectile-project-buffers)
               (car (occur-read-primary-args))))

(defun projectile-hashify-files (files-list)
  "Make the list of project files FILES-LIST ido friendly."
  (let ((files-table (make-hash-table :test 'equal))
        (files-to-uniquify nil))
    (dolist (current-file files-list files-table)
      (let ((basename (file-name-nondirectory current-file)))
        (if (gethash basename files-table)
            (progn
              (puthash
               (projectile-uniquify-file current-file)
               current-file files-table)
              (when basename (push basename files-to-uniquify)))
          (puthash basename current-file files-table))))
    ;; uniquify remaining files
    (dolist (current-file (remove-duplicates files-to-uniquify :test 'string=))
      (puthash
       (projectile-uniquify-file (gethash current-file files-table))
       (gethash current-file files-table) files-table)
      (remhash current-file files-table))
    files-table))

(defun projectile-uniquify-file (filename)
  "Create an unique version of a FILENAME."
  (let ((filename-parts (reverse (split-string filename "/"))))
    (format "%s/%s" (second filename-parts) (car filename-parts))))

(defun projectile-ignored-directory-p (directory)
  "Check if DIRECTORY should be ignored."
  (member directory (projectile-ignored-directories)))

(defun projectile-ignored-file-p (file)
  "Check if FILE should be ignored."
  (member file (projectile-ignored-files)))

(defun projectile-ignored-rel-p (directory file patterns)
  "Check if FILE should be ignored relative to DIRECTORY according to PATTERNS."
  (let ((default-directory directory))
    (-any? (lambda (pattern)
             (or (s-ends-with? (s-chop-suffix "/" pattern)
                               (s-chop-suffix "/" file))
                 (member file (file-expand-wildcards pattern t))))
           patterns)))

(defun projectile-ignored-files ()
  "Return list of ignored files."
  (-map
   'projectile-expand-root
   (append
    projectile-globally-ignored-files
    (projectile-project-ignored-files))))

(defun projectile-ignored-directories ()
  "Return list of ignored directories."
  (-map
   'file-name-as-directory
   (-map
    'projectile-expand-root
    (append
     projectile-globally-ignored-directories
     (projectile-project-ignored-directories)))))

(defun projectile-project-ignored-files ()
  "Return list of project ignored files."
  (-remove 'file-directory-p (projectile-project-ignored)))

(defun projectile-project-ignored-directories ()
  "Return list of project ignored directories."
  (-filter 'file-directory-p (projectile-project-ignored)))

(defun projectile-abs-patterns ()
  "Return a list of absolute (starting with /) file patterns."
  (-map (lambda (pattern)
          (s-chop-prefix "/" pattern))
        (-filter (lambda (pattern)
                   (s-starts-with? "/" pattern))
                 (projectile-parse-ignore-file))))

(defun projectile-rel-patterns ()
  "Return a list of relative file patterns."
  (-remove (lambda (pattern)
             (s-starts-with? "/" pattern))
           (projectile-parse-ignore-file)))

(defun projectile-project-ignored ()
  "Return list of project ignored files/directories."
  (let ((patterns (projectile-abs-patterns))
        (default-directory (projectile-project-root)))
    (apply 'append
           (-map
            (lambda (pattern)
              (file-expand-wildcards pattern t))
            patterns))))

(defun projectile-ignore-file ()
  "Return the absolute path to the project's ignore file."
  (expand-file-name ".projectile" (projectile-project-root)))

(defun projectile-parse-ignore-file ()
  "Parse project ignore file and return list of patterns to ignore."
  (let ((ignore-file (projectile-ignore-file)))
    (when (file-exists-p ignore-file)
      (with-temp-buffer
        (insert-file-contents-literally ignore-file)
        (let ((split-string-default-separators "[\r\n]"))
          (-map 's-trim (delete "" (split-string (buffer-string)))))))))

(defun projectile-expand-root (name)
  "Expand NAME to project root.

Never use on many files since it's going to recalculate the
project-root for every file."
  (expand-file-name name (projectile-project-root)))

(defun projectile-completing-read (prompt choices)
  "Present a project tailored PROMPT with CHOICES."
  (let ((prompt (projectile-prepend-project-name prompt)))
    (cond
     ((eq projectile-completion-system 'ido) (ido-completing-read prompt choices))
     (t (completing-read prompt choices)))))

(defun projectile-current-project-files ()
  "Return a list of files for the current project."
  (projectile-project-files (projectile-project-root)))

(defun projectile-hash-keys (hash)
  "Return a list of all HASH keys."
  (let (allkeys)
    (maphash (lambda (k v) (setq allkeys (cons k allkeys))) hash)
    allkeys))

(defun projectile-find-file (arg)
  "Jump to a project's file using completion."
  (interactive "P")
  (when arg
    (projectile-invalidate-cache))
  (let* ((project-files (projectile-hashify-files
                         (projectile-current-project-files)))
         (file (projectile-completing-read "File file: "
                                           (projectile-hash-keys project-files))))
    (find-file (gethash file project-files))))

(defun projectile-find-test-file (arg)
  "Jump to a project's test file using completion."
  (interactive "P")
  (when arg
    (projectile-invalidate-cache))
  (let* ((test-files (projectile-hashify-files
                         (projectile-test-files (projectile-current-project-files))))
         (file (projectile-completing-read "File test file: "
                                           (projectile-hash-keys test-files))))
    (find-file (gethash file test-files))))

(defvar projectile-test-files-suffices '("_test" "_spec" "Test" "-test")
  "Some common suffices of test files.")

(defun projectile-test-files (files)
  "Return only the test FILES."
  (-filter 'projectile-test-file-p files))

(defun projectile-test-file-p (file)
  "Check if FILE is a test file."
  (-any? (lambda (suffix)
           (s-ends-with? suffix (file-name-sans-extension file)))
         projectile-test-files-suffices))

(defvar projectile-rails-rspec '("Gemfile" "app" "lib" "db" "config" "spec"))
(defvar projectile-rails-test '("Gemfile" "app" "lib" "db" "config" "test"))
(defvar projectile-maven '("pom.xml"))
(defvar projectile-lein '("project.clj"))

(defun projectile-project-type ()
  "Determine the project's type based on its structure."
  (let ((project-root (projectile-project-root)))
    (cond
     ((projectile-verify-files projectile-rails-rspec) 'rails-rspec)
     ((projectile-verify-files projectile-rails-test) 'rails-test)
     ((projectile-verify-files projectile-maven) 'maven)
     ((projectile-verify-files projectile-lein) 'lein)
     (t 'generic))))

(defun projectile-verify-files (files)
  "Check whether all FILES exist in the current project."
  (-all? 'projectile-verify-file files))

(defun projectile-verify-file (file)
  "Check whether FILE exists in the current project."
  (file-exists-p (projectile-expand-root file)))

(defun projectile-project-vcs ()
  "Determine the VCS used by the project if any."
  (let ((project-root (projectile-project-root)))
   (cond
    ((locate-dominating-file project-root ".git") 'git)
    ((locate-dominating-file project-root ".hg") 'hg)
    ((locate-dominating-file project-root ".bzr") 'bzr)
    ((locate-dominating-file project-root "_darcs") 'darcs)
    ((locate-dominating-file project-root ".svn") 'svn)
    (t 'none))))

(defun projectile-toggle-between-implemenation-and-test ()
  "Toggle between an implementation file and its test file."
  (interactive)
  (if (projectile-test-file-p (buffer-file-name))
      ;; find the matching impl file
      (let ((impl-file (projectile-compute-file-name (buffer-file-name))))
        (if impl-file
            (find-file impl-file)
          (error "No matching source file found")))
    ;; find the matching test file
    (let ((test-file (projectile-compute-test-file-name (buffer-file-name))))
      (if test-file
          (find-file test-file)
        (error "No matching test file found")))))

(defun projectile-compute-test-file-name (file)
  (let ((basename (file-name-sans-extension file))
        (extension (file-name-extension file)))
      (-first #'file-exists-p
              (-map (lambda (suffix)
                      (s-replace "/app/" "/spec/" (concat (s-append suffix basename) "." extension)))
                    projectile-test-files-suffices))))

(defun projectile-compute-file-name (test-file)
  (let ((basename (file-name-sans-extension test-file))
        (extension (file-name-extension test-file)))
    (-first #'file-exists-p
            (-map (lambda (suffix)
                    (s-replace "/spec/" "/app/" (concat (s-chop-suffix suffix basename) "." extension)))
                  projectile-test-files-suffices))))

(defun projectile-grep ()
  "Perform rgrep in the project."
  (interactive)
  (let ((search-regexp (if (and transient-mark-mode mark-active)
                           (buffer-substring (region-beginning) (region-end))
                         (read-string (projectile-prepend-project-name "Grep for: ") (thing-at-point 'symbol))))
        (root-dir (expand-file-name (projectile-project-root))))
    (require 'grep)
    ;; paths for find-grep should relative and without trailing /
    (let ((grep-find-ignored-directories (append (-map (lambda (dir) (s-chop-suffix "/" (s-replace root-dir "" dir))) (projectile-ignored-directories)) grep-find-ignored-directories))
          (grep-find-ignored-files (append (-map (lambda (file) (s-replace root-dir "" file)) (projectile-ignored-files)) grep-find-ignored-files)))
      (grep-compute-defaults)
      (rgrep search-regexp "* .*" root-dir))))

(defun projectile-ack ()
  "Run an `ack-and-a-half' search in the project."
  (interactive)
  (let ((ack-and-a-half-arguments
         (-map
          (lambda (path)
            (concat "--ignore-dir=" (file-name-nondirectory (directory-file-name path))))
          (projectile-ignored-directories))))
    (call-interactively projectile-ack-function)))


(defun projectile-regenerate-tags ()
  "Regenerate the project's etags."
  (interactive)
  (let ((current-dir default-directory)
        (project-root (projectile-project-root)))
    (cd project-root)
    (shell-command (format projectile-tags-command project-root))
    (cd current-dir)
    (visit-tags-table project-root)))

(defun projectile-replace ()
  "Replace a string in the project using perl."
  (interactive)
  (let ((old-text (read-string "Replace: " (thing-at-point 'symbol)))
        (new-text (read-string "With: ")))
    (tags-query-replace old-text new-text nil '(projectile-project-files (projectile-project-root)))))

(defun projectile-kill-buffers ()
  "Kill all project buffers."
  (interactive)
  (let* ((buffers (projectile-project-buffer-names))
         (question
          (format
           "Are you sure you want to kill %d buffer(s) for '%s'? "
           (length buffers)
           (projectile-project-name))))
    (if (yes-or-no-p question)
        (mapc 'kill-buffer buffers))))

(defun projectile-dired ()
  "Opens dired at the root of the project."
  (interactive)
  (dired (projectile-project-root)))

(defun projectile-recentf ()
  "Show a list of recently visited files in a project."
  (interactive)
  (if (boundp 'recentf-list)
      (let ((recent-project-files
             (projectile-hashify-files
              (intersection (projectile-current-project-files)
                            recentf-list
                            :test 'string=))))
        (find-file (gethash
                    (projectile-completing-read "Recently visited files: "
                                                (projectile-hash-keys recent-project-files))
                    recent-project-files)))
    (message "recentf is not enabled")))

(defun projectile-serialize-cache ()
  "Serializes the memory cache to the hard drive."
  (with-temp-buffer
    (insert (prin1-to-string projectile-projects-cache))
    (when (file-writable-p projectile-cache-file)
      (write-region (point-min)
                    (point-max)
                    projectile-cache-file))))

(defun projectile-run-project-command (checks)
  "Run command considering CHECKS."
  (let* ((dir (or (projectile-project-root)
                  (file-name-directory (buffer-file-name))))
         (pref (concat "cd " dir " && "))
         (cmd (projectile-get-project-compile-command dir checks)))
    (if cmd
        (compilation-start (concat pref cmd)))
    ))

(defun projectile-compile-project ()
  "Run project compilation command."
  (interactive)
  (projectile-run-project-command projectile-project-compilation-commands))

(defun projectile-test-project ()
  "Run project test command."
  (interactive)
  (projectile-run-project-command projectile-project-test-commands))

(defun projectile-get-project-compile-command (dir checks)
  "Retrieve compile command according to DIR and CHECKS."
  (loop for (command . check) in checks
        when (funcall check dir)
        do (return command)
        finally (return nil)))

(defvar projectile-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map (kbd "f") 'projectile-find-file)
      (define-key prefix-map (kbd "T") 'projectile-find-test-file)
      (define-key prefix-map (kbd "t") 'projectile-toggle-between-implemenation-and-test)
      (define-key prefix-map (kbd "g") 'projectile-grep)
      (define-key prefix-map (kbd "b") 'projectile-switch-to-buffer)
      (define-key prefix-map (kbd "o") 'projectile-multi-occur)
      (define-key prefix-map (kbd "r") 'projectile-replace)
      (define-key prefix-map (kbd "i") 'projectile-invalidate-cache)
      (define-key prefix-map (kbd "R") 'projectile-regenerate-tags)
      (define-key prefix-map (kbd "k") 'projectile-kill-buffers)
      (define-key prefix-map (kbd "d") 'projectile-dired)
      (define-key prefix-map (kbd "e") 'projectile-recentf)
      (define-key prefix-map (kbd "a") 'projectile-ack)
      (define-key prefix-map (kbd "l") 'projectile-compile-project)
      (define-key prefix-map (kbd "p") 'projectile-test-project)

      (define-key map projectile-keymap-prefix prefix-map))
    map)
  "Keymap for Projectile mode.")

(defun projectile-add-menu ()
  "Add Projectile's menu under Tools."
  (easy-menu-add-item nil '("Tools")
                      '("Projectile"
                        ["File file" projectile-find-file]
                        ["Switch to buffer" projectile-switch-to-buffer]
                        ["Kill project buffers" projectile-kill-buffers]
                        ["Recent files" projectile-recentf]
                        "--"
                        ["Open project in dired" projectile-dired]
                        ["Find in project (grep)" projectile-grep]
                        ["Find in project (ack)" projectile-ack]
                        ["Replace in project" projectile-replace]
                        ["Multi-occur in project" projectile-multi-occur]
                        "--"
                        ["Invalidate cache" projectile-invalidate-cache]
                        ["Regenerate etags" projectile-regenerate-tags]
                        "--"
                        ["Compile project" projectile-compile-project]
                        ["Test project" projectile-test-project]
                        "--"
                        ["About" projectile-version])
                      "Search Files (Grep)...")

  (easy-menu-add-item nil '("Tools") '("--") "Search Files (Grep)..."))

(defun projectile-remove-menu ()
  "Remove Projectile's menu."
  (easy-menu-remove-item nil '("Tools") "Projectile")
  (easy-menu-remove-item nil '("Tools") "--"))

;;; define minor mode

;;;###autoload
(define-minor-mode projectile-mode
  "Minor mode to assist project management and navigation.

\\{projectile-mode-map}"
  :lighter " Projectile"
  :keymap projectile-mode-map
  :group 'projectile
  (if projectile-mode
      ;; on start
      (projectile-add-menu)
    ;; on stop
    (projectile-remove-menu)))

;;;###autoload
(define-globalized-minor-mode projectile-global-mode
  projectile-mode
  projectile-on)

(defun projectile-on ()
  "Enable Projectile minor mode."
  (projectile-mode 1))

(defun projectile-off ()
  "Disable Projectile minor mode."
  (projectile-mode -1))

(provide 'projectile)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; projectile.el ends here
