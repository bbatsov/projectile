;;; projectile.el --- Manage and navigate projects in Emacs easily

;; Copyright © 2011-2013 Bozhidar Batsov <bozhidar@batsov.com>

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/projectile
;; Keywords: project, convenience
;; Version: 1.0.0-cvs
;; Package-Requires: ((s "1.6.0") (dash "1.5.0") (pkg-info "0.1"))

;; This file is NOT part of GNU Emacs.

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

(require 'thingatpt)
(require 's)
(require 'dash)
(require 'grep)           ; For `rgrep'
(require 'package)        ; For `package-buffer-info' and `package-version-join'


;;; Customization
(defgroup projectile nil
  "Manage and navigate projects easily."
  :group 'tools
  :group 'convenience)

(defcustom projectile-indexing-method (if (eq system-type 'windows-nt) 'native 'alien)
  "Specifies the indexing method used by Projectile.

There are two indexing methods - native and alien.

The native method is implemented in Emacs Lisp (therefore it is
native to Emacs).  It's advantage is that is portable and will
work everywhere that Emacs does.  It's disadvantage is that is a
bit slow (especially for large projects).  Generally it's a good
idea to pair the native indexing method with caching.

The alien indexing method uses external tools (e.g. git, find,
etc) to speed up the indexing process.  The disadvantage of this
method is that it's not well supported on Windows systems.

By default alien indexing is the default on all operating
systems, except Windows."
  :group 'projectile
  :type 'symbol
  :options '(native alien))

(defcustom projectile-enable-caching (eq projectile-indexing-method 'native)
  "When t enables project files caching.

Project caching is automatically enabled by default if you're
using the native indexing method."
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
  :options '(ido grizzl default))

(defcustom projectile-ack-function 'ack-and-a-half
  "The ack function to use."
  :group 'projectile
  :type 'symbol
  :options '(ack-and-a-half default))

(defcustom projectile-keymap-prefix (kbd "C-c p")
  "Projectile keymap prefix."
  :group 'projectile
  :type 'string)

(defcustom projectile-cache-file
  (expand-file-name "projectile.cache" user-emacs-directory)
  "The name of Projectile's cache file."
  :group 'projectile
  :type 'string)

(defcustom projectile-tags-command "ctags -Re %s %s"
  "The command Projectile's going to use to generate a TAGS file."
  :group 'projectile
  :type 'string)

(defcustom projectile-project-root-files
  '(".projectile"    ; projectile project marker
    ".git"           ; Git VCS root dir
    ".hg"            ; Mercurial VCS root dir
    ".fslckout"      ; Fossil VCS root dir
    ".bzr"           ; Bazaar VCS root dir
    "_darcs"         ; Darcs VCS root dir
    "rebar.config"   ; Rebar project file
    "project.clj"    ; Leiningen project file
    "pom.xml"        ; Maven project file
    "build.sbt"      ; SBT project file
    "Gemfile"        ; Bundler file
    "Makefile"       ; Make project file
    )
  "A list of files considered to mark the root of a project."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-globally-ignored-files
  '("TAGS")
  "A list of files globally ignored by projectile."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-globally-ignored-directories
  '(".idea"
    ".eunit"
    ".git"
    ".hg"
    ".fslckout"
    ".bzr"
    "_darcs")
  "A list of directories globally ignored by projectile."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-find-file-hook nil
  "Hooks run when a file is opened with `projectile-find-file'."
  :group 'projectile
  :type 'hook)

(defcustom projectile-find-dir-hook nil
  "Hooks run when a directory is opened with `projectile-find-dir'."
  :group 'projectile
  :type 'hook)

(defcustom projectile-switch-project-action 'projectile-find-file
  "Action invoked after switching projects with `projectile-switch-project'.

Any function that does not take arguments will do."
  :group 'projectile
  :type 'symbol)


;;; Serialization
(defun projectile-serialize (data filename)
  "Serialize DATA to FILENAME.

The saved data can be restored with `projectile-unserialize'."
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (prin1-to-string data)))))

(defun projectile-unserialize (filename)
  "Read data serialized by `projectile-serialize' from FILENAME."
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (read (buffer-string)))))

(defvar projectile-projects-cache
  (or (projectile-unserialize projectile-cache-file)
      (make-hash-table :test 'equal))
  "A hashmap used to cache project file names to speed up related operations.")

(defvar projectile-known-projects nil
  "List of locations where we have previously seen projects.
The list of projects is ordered by the time they have been accessed.")

(defcustom projectile-known-projects-file
  (expand-file-name "projectile-bookmarks.eld"
                    user-emacs-directory)
  "Name and location of the Projectile's known projects file."
  :group 'projectile
  :type 'string)


;;; Version information
(defun projectile-library-version ()
  "Get the version in the Projectile library header."
  (-when-let (version (pkg-info-defining-library-version 'projectile-mode))
    (pkg-info-format-version version)))

(defun projectile-package-version ()
  "Get the package version of Projectile.

This is the version number of the installed Projectile package."
  (-when-let (version (pkg-info-package-version 'projectile))
    (pkg-info-format-version version)))

(defun projectile-version (&optional show-version)
  "Get the Projectile version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list (not (or executing-kbd-macro noninteractive))))
  (let* ((lib-version (projectile-library-version))
         (pkg-version (projectile-package-version))
         (version (cond
                   ((and lib-version pkg-version
                         (not (string= lib-version pkg-version)))
                    (format "%s (package: %s)" lib-version pkg-version))
                   ((or pkg-version lib-version)
                    (format "%s" (or pkg-version lib-version))))))
    (when show-version
      (unless version
        (error "Could not find out Projectile version"))
      (message "Projectile version: %s" version))
    version))


;;; Caching
(defun projectile-invalidate-cache (arg)
  "Remove the current project's files from `projectile-projects-cache'.

With a prefix argument ARG prompts for the name of the project whose cache
to invalidate."
  (interactive "P")
  (let ((project-root
         (if arg
             (completing-read "Remove cache for: "
                              (projectile-hash-keys projectile-projects-cache))
             (projectile-project-root))))
    (remhash project-root projectile-projects-cache)
    (projectile-serialize-cache)
    (message "Invalidated Projectile cache for %s."
             (propertize project-root 'face 'font-lock-keyword-face))))

(defun projectile-cache-project (project files)
  "Cache PROJECTs FILES.
The cache is created both in memory and on the hard drive."
  (when projectile-enable-caching
    (puthash project files projectile-projects-cache)
    (projectile-serialize-cache)))

(defun projectile-file-cached-p (file project)
  "Check if FILE is already in PROJECT cache."
  (member file (gethash project projectile-projects-cache)))

(defun projectile-cache-current-file ()
  "Add the currently visited file to the cache."
  (interactive)
  (let* ((current-project (projectile-project-root))
        (current-file (file-relative-name (buffer-file-name (current-buffer)) current-project)))
    (unless (projectile-file-cached-p current-file current-project)
      (puthash current-project
               (cons current-file (gethash current-project projectile-projects-cache))
               projectile-projects-cache)
      (projectile-serialize-cache)
      (message "File %s added to project %s cache." current-file current-project))))

;; cache opened files automatically to reduce the need for cache invalidation
(defun projectile-cache-files-find-file-hook ()
  "Function for caching files with `find-file-hook'."
  (when (and (projectile-project-p) projectile-enable-caching)
    (projectile-cache-current-file)))

(defun projectile-cache-projects-find-file-hook ()
  "Function for caching projects with `find-file-hook'."
  (when (projectile-project-p)
    (projectile-add-known-project (projectile-project-root))
    (projectile-save-known-projects)))


;;; Project root related utilities
(defun projectile-project-root ()
  "Retrieves the root directory of a project if available.
The current directory is assumed to be the project's root otherwise."
  (let ((project-root
         (or (->> projectile-project-root-files
               (--map (locate-dominating-file default-directory it))
               (-remove #'null)
               (car)
               (projectile-expand-file-name))
             (if projectile-require-project-root
                 (error "You're not into a project")
               default-directory))))
    project-root))

(defun projectile-expand-file-name (file-name)
  "A thin wrapper around `expand-file-name' that handles nil.
Expand FILE-NAME using `default-directory'."
  (when file-name
    (expand-file-name file-name)))

(defun projectile-project-p ()
  "Check if we're in a project."
  (condition-case nil
      (projectile-project-root)
    (error nil)))

(defun projectile-project-name ()
  "Return project name."
  (let ((project-root
         (condition-case nil
             (projectile-project-root)
           (error default-directory))))
   (file-name-nondirectory (directory-file-name project-root))))


;;; Project indexing
(defun projectile-get-project-directories ()
  "Get the list of project directories that are of interest to the user."
  (-map (lambda (subdir) (concat (projectile-project-root) subdir))
        (or (car (projectile-parse-dirconfig-file)) '(""))))

(defun projectile-dir-files (directory)
  "List the files in DIRECTORY and in its sub-directories.
Files are returned as relative paths to the project root."
  ;; check for a cache hit first if caching is enabled
  (let ((files-list (and projectile-enable-caching
                         (gethash directory projectile-projects-cache)))
        (root (projectile-project-root)))
    ;; cache disabled or cache miss
    (or files-list
        (if (eq projectile-indexing-method 'native)
            (projectile-dir-files-native root directory)
          ;; use external tools to get the project files
          (projectile-remove-ignored (projectile-dir-files-external root directory))))))

(defun projectile-dir-files-native (root directory)
  "Get the files for ROOT under DIRECTORY using just Emacs Lisp."
  (message "Projectile is indexing %s. This may take a while."
           (propertize directory 'face 'font-lock-keyword-face))
  ;; we need the files with paths relative to the project root
  (-map (lambda (file) (s-chop-prefix root file))
        (projectile-index-directory directory (projectile-patterns-to-ignore))))

(defun projectile-dir-files-external (root directory)
  "Get the files for ROOT under DIRECTORY using external tools."
  (let ((default-directory directory)
        (files-list nil))
    (setq files-list (-map (lambda (f)
                             (s-chop-prefix root (expand-file-name f directory)))
                           (projectile-get-repo-files)))
    files-list))

(defcustom projectile-git-command "git ls-files -zco --exclude-standard"
  "Command used by projectile to get the files in a git project."
  :group 'projectile
  :type 'string)

(defcustom projectile-hg-command "hg locate -0 -I ."
  "Command used by projectile to get the files in a hg project."
  :group 'projectile
  :type 'string)

(defcustom projectile-fossil-command "fossil ls"
  "Command used by projectile to get the files in a fossil project."
  :group 'projectile
  :type 'string)

(defcustom projectile-bzr-command "bzr ls --versioned -0"
  "Command used by projectile to get the files in a bazaar project."
  :group 'projectile
  :type 'string)

(defcustom projectile-darcs-command "darcs show files -0 . "
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
     ((eq vcs 'fossil) projectile-fossil-command)
     ((eq vcs 'bzr) projectile-bzr-command)
     ((eq vcs 'darcs) projectile-darcs-command)
     ((eq vcs 'svn) projectile-svn-command)
     (t projectile-generic-command))))

(defun projectile-get-repo-files ()
  "Get a list of the files in the project."
  (projectile-files-via-ext-command (projectile-get-ext-command)))

(defun projectile-files-via-ext-command (command)
  "Get a list of relative file names in the project root by executing COMMAND."
  (split-string (shell-command-to-string command) "\0" t))

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
                         (projectile-ignored-rel-p absolute-file
                                                   directory patterns))))
          (setq files-list (append files-list
                                   (projectile-index-directory
                                    (expand-file-name current-file directory)
                                    patterns))))
         ;; check for regular files that are not ignored
         ((and (not (s-ends-with-p "/" current-file))
               (not (projectile-ignored-file-p absolute-file))
               (not (and patterns
                         (projectile-ignored-rel-p absolute-file
                                                   directory patterns))))
          (setq files-list (cons
                            (expand-file-name current-file directory)
                            files-list))))))))

(defun projectile-remove-ignored (files)
  "Remove ignored files and folders from FILES.

Operates on filenames relative to the project root."
  (let ((ignored (append (projectile-ignored-files-rel)
                         (projectile-ignored-directories-rel))))
    (-remove (lambda (file)
               (--any-p  (s-starts-with-p it file) ignored))
             files)))

(defun projectile-project-buffers ()
  "Get a list of project buffers."
  (let ((project-root (projectile-project-root)))
    (-filter (lambda (buffer)
               (projectile-project-buffer-p buffer project-root))
             (buffer-list))))

(defun projectile-project-buffer-p (buffer project-root)
  "Check if BUFFER is under PROJECT-ROOT."
  (with-current-buffer buffer
    (and (s-starts-with? project-root
                         (expand-file-name default-directory))
         ;; ignore hidden buffers
         (not (s-starts-with? " " (buffer-name buffer))))))

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

(defun projectile-ignored-directory-p (directory)
  "Check if DIRECTORY should be ignored."
  (member directory (projectile-ignored-directories)))

(defun projectile-ignored-file-p (file)
  "Check if FILE should be ignored."
  (member file (projectile-ignored-files)))

(defun projectile-ignored-rel-p (file directory patterns)
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

(defun projectile-ignored-directories-rel ()
  "Return list of ignored directories, relative to the root."
  (--map (s-chop-prefix (projectile-project-root) it) (projectile-ignored-directories)))

(defun projectile-ignored-files-rel ()
  "Return list of ignored files, relative to the root."
  (--map (s-chop-prefix (projectile-project-root) it) (projectile-ignored-files)))

(defun projectile-project-ignored-files ()
  "Return list of project ignored files."
  (-remove 'file-directory-p (projectile-project-ignored)))

(defun projectile-project-ignored-directories ()
  "Return list of project ignored directories."
  (-filter 'file-directory-p (projectile-project-ignored)))

(defun projectile-paths-to-ignore ()
  "Return a list of ignored project paths."
  (-map (lambda (pattern)
          (s-chop-prefix "/" pattern))
        (-filter (lambda (pattern)
                   (s-starts-with? "/" pattern))
                 (cdr (projectile-parse-dirconfig-file)))))

(defun projectile-patterns-to-ignore ()
  "Return a list of relative file patterns."
  (-remove (lambda (pattern)
             (s-starts-with? "/" pattern))
           (cdr (projectile-parse-dirconfig-file))))

(defun projectile-project-ignored ()
  "Return list of project ignored files/directories."
  (let ((paths (projectile-paths-to-ignore))
        (default-directory (projectile-project-root)))
    (apply 'append
           (-map
            (lambda (pattern)
              (file-expand-wildcards pattern t))
            paths))))

(defun projectile-dirconfig-file ()
  "Return the absolute path to the project's dirconfig file."
  (expand-file-name ".projectile" (projectile-project-root)))

(defun projectile-parse-dirconfig-file ()
  "Parse project ignore file and return directories to ignore and keep.

The return value will be a cons, the car being the list of
directories to keep, and the cdr being the list of files or
directories to ignore.

Strings starting with + will be added to the list of directories
to keep, and strings starting with - will be added to the list of
directories to ignore.  For backward compatibility, without a
prefix the string will be assumed to be an ignore string."
  (let ((dirconfig-file (projectile-dirconfig-file)))
    (when (file-exists-p dirconfig-file)
      (with-temp-buffer
        (insert-file-contents-literally dirconfig-file)
        (let* ((split-string-default-separators "[\r\n]")
               (strings (-map 's-trim (delete "" (split-string (buffer-string)))))
               (separated-vals (--separate (s-starts-with? "+" it) strings)))
          (cons (-map 'projectile-strip-dir-prefix (car separated-vals))
                (-map 'projectile-strip-dir-prefix (cadr separated-vals))))))))

(defun projectile-strip-dir-prefix (dir)
  "Strip + or - prefix from DIR."
  (s-chop-prefixes '("-" "+") dir))

(defun projectile-expand-root (name)
  "Expand NAME to project root.

Never use on many files since it's going to recalculate the
project-root for every file."
  (expand-file-name name (projectile-project-root)))

(defun projectile-completing-read (prompt choices)
  "Present a project tailored PROMPT with CHOICES."
  (let ((prompt (projectile-prepend-project-name prompt)))
    (cond
     ((eq projectile-completion-system 'ido)
      (ido-completing-read prompt choices))
     ((eq projectile-completion-system 'default)
      (completing-read prompt choices))
     ((eq projectile-completion-system 'grizzl)
      (if (and (fboundp 'grizzl-completing-read)
               (fboundp 'grizzl-make-index))
          (grizzl-completing-read prompt (grizzl-make-index choices))
        (user-error "Please install grizzl from \
https://github.com/d11wtq/grizzl")))
     (t (funcall projectile-completion-system prompt choices)))))

(defun projectile-current-project-files ()
  "Return a list of files for the current project."
  (let ((files (and projectile-enable-caching
                    (gethash (projectile-project-root) projectile-projects-cache))))
    ;; nothing is cached
    (unless files
      (setq files (-mapcat 'projectile-dir-files
                           (projectile-get-project-directories)))
      ;; cache the resulting list of files
      (when projectile-enable-caching
        (projectile-cache-project (projectile-project-root) files)))
    files))

(defun projectile-current-project-dirs ()
  "Return a list of dirs for the current project."
  (-remove 'null (-distinct
                  (-map 'file-name-directory
                        (projectile-current-project-files)))))

(defun projectile-hash-keys (hash)
  "Return a list of all HASH keys."
  (let (allkeys)
    (maphash (lambda (k v) (setq allkeys (cons k allkeys))) hash)
    allkeys))


;;; Interactive commands
(defun projectile-find-file (&optional arg)
  "Jump to a project's file using completion.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (when arg
    (projectile-invalidate-cache nil))
  (let ((file (projectile-completing-read "Find file: "
                                          (projectile-current-project-files))))
    (find-file (expand-file-name file (projectile-project-root)))
    (run-hooks 'projectile-find-file-hook)))

(defun projectile-find-dir (arg)
  "Jump to a project's directory using completion.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (when arg
    (projectile-invalidate-cache nil))
  (let ((dir (projectile-completing-read "Find dir: "
                                          (projectile-current-project-dirs))))
    (dired (expand-file-name dir (projectile-project-root)))
    (run-hooks 'projectile-find-dir-hook)))

(defun projectile-find-test-file (arg)
  "Jump to a project's test file using completion.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (when arg
    (projectile-invalidate-cache nil))
  (let ((file (projectile-completing-read "Find test file: "
                                          (projectile-current-project-test-files))))
    (find-file (expand-file-name file (projectile-project-root)))))

(defcustom projectile-test-files-suffices '("_test" "_spec" "Test" "-test")
  "Some common suffices of test files."
  :group 'projectile
  :type '(repeat string))

(defun projectile-test-files (files)
  "Return only the test FILES."
  (-filter 'projectile-test-file-p files))

(defun projectile-test-file-p (file)
  "Check if FILE is a test file."
  (-any? (lambda (suffix)
           (s-ends-with? suffix (file-name-sans-extension file)))
         projectile-test-files-suffices))

(defun projectile-current-project-test-files ()
  "Return a list of test files for the current project."
  (projectile-test-files (projectile-current-project-files)))

(defvar projectile-rails-rspec '("Gemfile" "app" "lib" "db" "config" "spec"))
(defvar projectile-rails-test '("Gemfile" "app" "lib" "db" "config" "test"))
(defvar projectile-symfony '("composer.json" "app" "src" "vendor"))
(defvar projectile-ruby-rspec '("Gemfile" "lib" "spec"))
(defvar projectile-ruby-test '("Gemfile" "lib" "test"))
(defvar projectile-maven '("pom.xml"))
(defvar projectile-lein '("project.clj"))
(defvar projectile-rebar '("rebar"))
(defvar projectile-make '("Makefile"))

(defun projectile-project-type ()
  "Determine the project's type based on its structure."
  (let ((project-root (projectile-project-root)))
    (cond
     ((projectile-verify-files projectile-rails-rspec) 'rails-rspec)
     ((projectile-verify-files projectile-rails-test) 'rails-test)
     ((projectile-verify-files projectile-ruby-rspec) 'ruby-rspec)
     ((projectile-verify-files projectile-ruby-test) 'ruby-test)
     ((projectile-verify-files projectile-symfony) 'symfony)
     ((projectile-verify-files projectile-maven) 'maven)
     ((projectile-verify-files projectile-lein) 'lein)
     ((projectile-verify-files projectile-rebar) 'rebar)
     ((projectile-verify-files projectile-make) 'make)
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
    ((file-exists-p (expand-file-name ".git" project-root)) 'git)
    ((file-exists-p (expand-file-name ".hg" project-root)) 'hg)
    ((file-exists-p (expand-file-name ".fossil" project-root)) 'fossil)
    ((file-exists-p (expand-file-name ".bzr" project-root)) 'bzr)
    ((file-exists-p (expand-file-name "_darcs" project-root)) 'darcs)
    ((file-exists-p (expand-file-name ".svn" project-root)) 'svn)
    ((locate-dominating-file project-root ".git") 'git)
    ((locate-dominating-file project-root ".hg") 'hg)
    ((locate-dominating-file project-root ".fossil") 'fossil)
    ((locate-dominating-file project-root ".bzr") 'bzr)
    ((locate-dominating-file project-root "_darcs") 'darcs)
    ((locate-dominating-file project-root ".svn") 'svn)
    (t 'none))))

(defun projectile-toggle-between-implemenation-and-test ()
  "Toggle between an implementation file and its test file."
  (interactive)
  (if (projectile-test-file-p (buffer-file-name))
      ;; find the matching impl file
      (let ((impl-file (projectile-find-matching-file (buffer-file-name))))
        (if impl-file
            (find-file (projectile-expand-root impl-file))
          (error "No matching source file found")))
    ;; find the matching test file
    (let ((test-file (projectile-find-matching-test (buffer-file-name))))
      (if test-file
          (find-file (projectile-expand-root test-file))
        (error "No matching test file found")))))

(defun projectile-test-suffix (project-type)
  "Find test files suffix based on PROJECT-TYPE."
  (cond
   ((member project-type '(rails-rspec ruby-rspec)) "_spec")
   ((member project-type '(rails-test ruby-test lein)) "_test")
   ((member project-type '(maven symfony)) "Test")
   (t (error "Project type not supported!"))))

(defun projectile-find-matching-test (file)
  "Compute the name of the test matching FILE."
  (let ((basename (file-name-nondirectory (file-name-sans-extension file)))
        (extension (file-name-extension file))
        (test-suffix (projectile-test-suffix (projectile-project-type))))
      (-first (lambda (current-file)
                (s-equals? (file-name-nondirectory (file-name-sans-extension current-file))
                           (concat basename test-suffix)))
              (projectile-current-project-files))))

(defun projectile-find-matching-file (test-file)
  "Compute the name of a file matching TEST-FILE."
  (let ((basename (file-name-nondirectory (file-name-sans-extension test-file)))
        (extension (file-name-extension test-file))
        (test-suffix (projectile-test-suffix (projectile-project-type))))
    (-first (lambda (current-file)
              (s-equals? (concat (file-name-nondirectory (file-name-sans-extension current-file)) test-suffix)
                         basename))
            (projectile-current-project-files))))

(defun projectile-grep ()
  "Perform rgrep in the project."
  (interactive)
  (let ((roots (projectile-get-project-directories))
        (search-regexp (if (and transient-mark-mode mark-active)
                           (buffer-substring (region-beginning) (region-end))
                         (read-string (projectile-prepend-project-name "Grep for: ")
                                      (projectile-symbol-at-point)))))
    (dolist (root-dir roots)
      (require 'grep)
      ;; paths for find-grep should relative and without trailing /
      (let ((grep-find-ignored-directories (-union (-map (lambda (dir) (s-chop-suffix "/" (s-chop-prefix root-dir dir)))
                                                         (cdr (projectile-ignored-directories))) grep-find-ignored-directories))
            (grep-find-ignored-files (-union (-map (lambda (file) (s-chop-prefix root-dir file)) (projectile-ignored-files)) grep-find-ignored-files)))
        (grep-compute-defaults)
        (rgrep search-regexp "* .*" root-dir)))))

(defun projectile-ack ()
  "Run an `ack-and-a-half' search in the project."
  (interactive)
  (let ((ack-and-a-half-arguments
         (-map
          (lambda (path)
            (concat "--ignore-dir=" (file-name-nondirectory (directory-file-name path))))
          (projectile-ignored-directories))))
    (call-interactively projectile-ack-function)))

(defun projectile-tags-exclude-patterns ()
  "Return a string with exclude patterns for ctags."
  (mapconcat (lambda (pattern) (format "--exclude=%s" pattern))
       (projectile-project-ignored-directories) " "))

(defun projectile-regenerate-tags ()
  "Regenerate the project's etags."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (tags-exclude (projectile-tags-exclude-patterns))
         (default-directory project-root))
    (shell-command (format projectile-tags-command tags-exclude project-root))
    (visit-tags-table project-root)))

(defun projectile-files-in-project-directory (directory)
  "Return a list of files in DIRECTORY."
  (let ((dir (s-chop-prefix (projectile-project-root) (expand-file-name directory))))
    (-filter (lambda (file) (s-starts-with-p dir file))
             (projectile-current-project-files))))

(defun projectile-replace (arg)
  "Replace a string in the project using `tags-query-replace'.

With a prefix argument ARG prompts you for a directory on which to run the replacement."
  (interactive "P")
  (let* ((old-text (read-string
                    (projectile-prepend-project-name "Replace: ")
                    (projectile-symbol-at-point)))
        (new-text (read-string
                   (projectile-prepend-project-name
                    (format "Replace %s with: " old-text)))))
    (if arg
        (tags-query-replace old-text new-text nil '(-map 'projectile-expand-root (projectile-files-in-project-directory (read-directory-name "Replace in directory: "))))
      (tags-query-replace old-text new-text nil '(-map 'projectile-expand-root (projectile-current-project-files))))))

(defun projectile-symbol-at-point ()
  "Get the symbol at point and strip its properties."
  (substring-no-properties (or (thing-at-point 'symbol) "")))

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
      (find-file (projectile-expand-root (projectile-completing-read "Recently visited files: " (projectile-recentf-files)))))
    (message "recentf is not enabled"))

(defun projectile-recentf-files ()
  "Return a list of recently visited files in a project."
  (if (boundp 'recentf-list)
      (let ((project-root (projectile-project-root)))
        (->> recentf-list
          (-filter (lambda (file) (s-starts-with-p project-root file)))
          (-map (lambda (file) (s-chop-prefix project-root file)))))
    nil))

(defun projectile-serialize-cache ()
  "Serializes the memory cache to the hard drive."
  (projectile-serialize projectile-projects-cache projectile-cache-file))

(defvar projectile-rails-compile-cmd "bundle exec rails server")
(defvar projectile-ruby-compile-cmd "bundle exec rake build")
(defvar projectile-ruby-test-cmd "bundle exec rake test")
(defvar projectile-ruby-rspec-cmd "bundle exec rspec")
(defvar projectile-symfony-compile-cmd "app/console server:run")
(defvar projectile-symfony-test-cmd "phpunit -c app ")
(defvar projectile-maven-compile-cmd "mvn clean install")
(defvar projectile-maven-test-cmd "mvn test")
(defvar projectile-lein-compile-cmd "lein compile")
(defvar projectile-lein-test-cmd "lein test")
(defvar projectile-rebar-compile-cmd "rebar")
(defvar projectile-rebar-test-cmd "rebar eunit")
(defvar projectile-make-compile-cmd "make")
(defvar projectile-make-test-cmd "make test")

(defvar projectile-compilation-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last compilation command used on them.")
(defvar projectile-test-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last test command used on them.")

(defun projectile-default-compilation-command (project-type)
  "Retrieve default compilation command for PROJECT-TYPE."
  (cond
   ((member project-type '(rails-rspec rails-test)) projectile-rails-compile-cmd)
   ((member project-type '(ruby-rspec ruby-test)) projectile-ruby-compile-cmd)
   ((eq project-type 'symfony) projectile-symfony-compile-cmd)
   ((eq project-type 'lein) projectile-lein-compile-cmd)
   ((eq project-type 'make) projectile-make-compile-cmd)
   ((eq project-type 'rebar) projectile-rebar-compile-cmd)
   ((eq project-type 'maven) projectile-maven-compile-cmd)
   (t projectile-make-compile-cmd)))

(defun projectile-default-test-command (project-type)
  "Retrieve default test command for PROJECT-TYPE."
  (cond
   ((member project-type '(rails-rspec ruby-rspec)) projectile-ruby-rspec-cmd)
   ((member project-type '(rails-test ruby-test)) projectile-ruby-test-cmd)
   ((eq project-type 'symfony) projectile-symfony-test-cmd)
   ((eq project-type 'lein) projectile-lein-test-cmd)
   ((eq project-type 'make) projectile-make-test-cmd)
   ((eq project-type 'rebar) projectile-rebar-test-cmd)
   ((eq project-type 'maven) projectile-maven-test-cmd)
   (t projectile-make-test-cmd)))

(defun projectile-compilation-command (project)
  "Retrieve the compilation command for PROJECT."
  (or (gethash project projectile-compilation-cmd-map)
      (projectile-default-compilation-command (projectile-project-type))))

(defun projectile-test-command (project)
  "Retrieve the test command for PROJECT."
  (or (gethash project projectile-test-cmd-map)
      (projectile-default-test-command (projectile-project-type))))

(defun projectile-compile-project (arg)
  "Run project compilation command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let* ((project-root (projectile-project-root))
         (default-cmd (projectile-compilation-command project-root))
         (compilation-cmd (if (or compilation-read-command arg)
                              (compilation-read-command default-cmd)
                            default-cmd))
         (default-directory project-root))
    (puthash project-root compilation-cmd projectile-compilation-cmd-map)
    (compilation-start compilation-cmd)))

;; TODO - factor this duplication out
(defun projectile-test-project ()
  "Run project test command."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (test-cmd (compilation-read-command (projectile-test-command project-root)))
         (default-directory project-root))
    (puthash project-root test-cmd projectile-test-cmd-map)
    (compilation-start test-cmd)))

(defun projectile-relevant-known-projects ()
  "Return a list of known projects except the current one (if present)."
  (if (projectile-project-p)
      (-difference projectile-known-projects
                   (list (abbreviate-file-name (projectile-project-root))))
    projectile-known-projects))

(defun projectile-switch-project ()
  "Switch to a project we have seen before."
  (interactive)
  (let* ((project-to-switch
         (projectile-completing-read "Switch to project: "
                                     (projectile-relevant-known-projects)))
         (default-directory project-to-switch))
    (funcall projectile-switch-project-action)
    (let ((project-switched project-to-switch))
      (run-hooks 'projectile-switch-project-hook))))

(defun projectile-find-file-in-directory ()
  "Jump to a file in a (maybe regular) directory.

This command will first prompt for the directory the file is in."
  (interactive)
  (let* ((directory (read-directory-name "Find file in directory: "))
         (default-directory directory)
         (projectile-require-project-root nil))
    (projectile-find-file)))

(defcustom projectile-switch-project-hook nil
  "Hooks run when project is switched.

The path to the opened project is available as PROJECT-SWITCHED"
  :group 'projectile
  :type 'hook)

(defun projectile-clear-known-projects ()
  "Clear both `projectile-known-projects' and `projectile-known-projects-file'."
  (interactive)
  (setq projectile-known-projects nil)
  (projectile-save-known-projects))

(defun projectile-remove-known-project ()
  "Remove a projected from the list of known projects."
  (interactive)
  (let ((project-to-remove
          (projectile-completing-read "Remove from known projects: "
                                      projectile-known-projects)))
    (setq projectile-known-projects
          (--reject (string= project-to-remove it) projectile-known-projects))
    (projectile-save-known-projects)
    (message "Project %s removed from the list of known projects." project-to-remove)))

(defun projectile-add-known-project (project-root)
  "Add PROJECT-ROOT to the list of known projects."
  (setq projectile-known-projects
        (-distinct
         (cons (abbreviate-file-name project-root) projectile-known-projects))))

(defun projectile-load-known-projects ()
  "Load saved projects from `projectile-known-projects-file'.
Also set `projectile-known-projects'."
  (setq projectile-known-projects
        (projectile-unserialize projectile-known-projects-file)))

;; load the known projects
(projectile-load-known-projects)

(defun projectile-save-known-projects ()
  "Save PROJECTILE-KNOWN-PROJECTS to PROJECTILE-KNOWN-PROJECTS-FILE."
  (projectile-serialize projectile-known-projects projectile-known-projects-file))


;;; Minor mode
(defvar projectile-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map (kbd "f") 'projectile-find-file)
      (define-key prefix-map (kbd "T") 'projectile-find-test-file)
      (define-key prefix-map (kbd "l") 'projectile-find-file-in-directory)
      (define-key prefix-map (kbd "t") 'projectile-toggle-between-implemenation-and-test)
      (define-key prefix-map (kbd "g") 'projectile-grep)
      (define-key prefix-map (kbd "b") 'projectile-switch-to-buffer)
      (define-key prefix-map (kbd "o") 'projectile-multi-occur)
      (define-key prefix-map (kbd "r") 'projectile-replace)
      (define-key prefix-map (kbd "i") 'projectile-invalidate-cache)
      (define-key prefix-map (kbd "R") 'projectile-regenerate-tags)
      (define-key prefix-map (kbd "k") 'projectile-kill-buffers)
      (define-key prefix-map (kbd "d") 'projectile-find-dir)
      (define-key prefix-map (kbd "D") 'projectile-dired)
      (define-key prefix-map (kbd "e") 'projectile-recentf)
      (define-key prefix-map (kbd "a") 'projectile-ack)
      (define-key prefix-map (kbd "c") 'projectile-compile-project)
      (define-key prefix-map (kbd "p") 'projectile-test-project)
      (define-key prefix-map (kbd "z") 'projectile-cache-current-file)
      (define-key prefix-map (kbd "s") 'projectile-switch-project)

      (define-key map projectile-keymap-prefix prefix-map))
    map)
  "Keymap for Projectile mode.")

(easy-menu-change
 '("Tools") "Projectile"
 '(["Find file" projectile-find-file]
   ["Find test file" projectile-find-test-file]
   ["Find directory" projectile-find-dir]
   ["Find file in directory" projectile-find-file-in-directory]
   ["Switch to buffer" projectile-switch-to-buffer]
   ["Jump between implementation file and test file" projectile-toggle-between-implemenation-and-test]
   ["Kill project buffers" projectile-kill-buffers]
   ["Recent files" projectile-recentf]
   "--"
   ["Open project in dired" projectile-dired]
   ["Switch to project" projectile-switch-project]
   ["Find in project (grep)" projectile-grep]
   ["Find in project (ack)" projectile-ack]
   ["Replace in project" projectile-replace]
   ["Multi-occur in project" projectile-multi-occur]
   "--"
   ["Cache current file" projectile-cache-current-file]
   ["Invalidate cache" projectile-invalidate-cache]
   ["Regenerate etags" projectile-regenerate-tags]
   "--"
   ["Compile project" projectile-compile-project]
   ["Test project" projectile-test-project]
   "--"
   ["About" projectile-version])
 "Search Files (Grep)...")

(easy-menu-change '("Tools") "--" nil "Search Files (Grep)...")

;;;###autoload
(define-minor-mode projectile-mode
  "Minor mode to assist project management and navigation.

\\{projectile-mode-map}"
  :lighter " Projectile"
  :keymap projectile-mode-map
  :group 'projectile)

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

(defun projectile-global-on ()
  "Enable Projectile global minor mode."
  (add-hook 'find-file-hook 'projectile-cache-files-find-file-hook)
  (add-hook 'find-file-hook 'projectile-cache-projects-find-file-hook)
  (add-hook 'projectile-find-dir-hook 'projectile-cache-projects-find-file-hook))

(defun projectile-global-off ()
  "Disable Projectile global minor mode."
  (remove-hook 'find-file-hook 'projectile-cache-files-find-file-hook)
  (remove-hook 'find-file-hook 'projectile-cache-projects-find-file-hook))

(defadvice projectile-global-mode (after projectile-setup-hooks activate)
  "Add/remove `find-file-hook' functions within `projectile-global-mode'."
  (if projectile-global-mode
      (projectile-global-on)
    (projectile-global-off)))

(provide 'projectile)

;;; projectile.el ends here
