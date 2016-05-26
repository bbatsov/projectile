;;; projectile.el --- Manage and navigate projects in Emacs easily -*- lexical-binding: t -*-

;; Copyright © 2011-2016 Bozhidar Batsov <bozhidar@batsov.com>

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/projectile
;; Keywords: project, convenience
;; Version: 0.14.0-cvs
;; Package-Requires: ((dash "2.11.0") (pkg-info "0.4"))

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
(require 'dash)
(require 'ibuffer)
(require 'ibuf-ext)
(require 'compile)
(require 'grep)

(eval-when-compile
  (defvar ag-ignore-list)
  (defvar ggtags-completion-table)
  (defvar tags-completion-table)
  (defvar tags-loop-scan)
  (defvar tags-loop-operate)
  (defvar eshell-buffer-name)
  (defvar explicit-shell-file-name))

(declare-function ggtags-ensure-project "ggtags")
(declare-function ggtags-update-tags "ggtags")
(declare-function pkg-info-version-info "pkg-info")
(declare-function tags-completion-table "etags")
(declare-function make-term "term")
(declare-function term-mode "term")
(declare-function term-char-mode "term")

(defvar grep-files-aliases)
(defvar grep-find-ignored-directories)
(defvar grep-find-ignored-files)

;;;; Compatibility
(eval-and-compile
  ;; Added in Emacs 24.3.
  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
      (declare (debug defvar) (doc-string 3))
      `(progn
         (defvar ,var ,val ,docstring)
         (make-variable-buffer-local ',var))))

  ;; Added in Emacs 24.4
  (unless (fboundp 'string-suffix-p)
    (defun string-suffix-p (suffix string  &optional ignore-case)
      "Return non-nil if SUFFIX is a suffix of STRING.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
      (let ((start-pos (- (length string) (length suffix))))
        (and (>= start-pos 0)
             (eq t (compare-strings suffix nil nil
                                    string start-pos nil ignore-case))))))

  ;; Improved (no more stack overflows) in Emacs 24.5
  (eval-after-load 'etags
    '(when (< emacs-major-version 25)
       (defvar etags--table-line-limit 500)
       (defun etags-tags-completion-table ()
         (let ((table (make-vector 511 0))
               (progress-reporter
                (make-progress-reporter
                 (format "Making tags completion table for %s..." buffer-file-name)
                 (point-min) (point-max))))
           (save-excursion
             (goto-char (point-min))
             (while (not (eobp))
               (if (not (re-search-forward
                         "[\f\t\n\r()=,; ]?\177\\\(?:\\([^\n\001]+\\)\001\\)?"
                         (+ (point) etags--table-line-limit) t))
                   (forward-line 1)
                 (intern (prog1 (if (match-beginning 1)
                                    (buffer-substring (match-beginning 1) (match-end 1))
                                  (goto-char (match-beginning 0))
                                  (skip-chars-backward "^\f\t\n\r()=,; ")
                                  (prog1
                                      (buffer-substring (point) (match-beginning 0))
                                    (goto-char (match-end 0))))
                           (progress-reporter-update progress-reporter (point)))
                         table))))
           table))))

  )

(defun projectile-trim-string (string)
  "Remove whitespace at the beginning and end of STRING."
  (->> string
       (replace-regexp-in-string "\\`[ \t\n\r]+" "")
       (replace-regexp-in-string "[ \t\n\r]+\\'" "")))


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
  :type '(radio
          (const :tag "Native" native)
          (const :tag "Alien" alien)))

(defcustom projectile-enable-caching (eq projectile-indexing-method 'native)
  "When t enables project files caching.

Project caching is automatically enabled by default if you're
using the native indexing method."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-file-exists-local-cache-expire nil
  "Number of seconds before file existence cache expires for a
file on a local file system.

 A value of nil disables this cache."

  :group 'projectile
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds")))

(defcustom projectile-file-exists-remote-cache-expire (* 5 60)
  "Number of seconds before file existence cache expires for a
file on a remote file system such as tramp.

 A value of nil disables this cache."
  :group 'projectile
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds")))

(defcustom projectile-require-project-root t
  "Require the presence of a project root to operate when true.
Otherwise consider the current directory the project root."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-completion-system 'ido
  "The completion system to be used by Projectile."
  :group 'projectile
  :type '(radio
          (const :tag "Ido" ido)
          (const :tag "Grizzl" grizzl)
          (const :tag "Helm" helm)
          (const :tag "Ivy" ivy)
          (const :tag "Default" default)
          (function :tag "Custom function")))

(defcustom projectile-keymap-prefix (kbd "C-c p")
  "Projectile keymap prefix."
  :group 'projectile
  :type 'string)

(defcustom projectile-cache-file
  (expand-file-name "projectile.cache" user-emacs-directory)
  "The name of Projectile's cache file."
  :group 'projectile
  :type 'string)

(defcustom projectile-tags-file-name "TAGS"
  "The tags filename Projectile's going to use."
  :group 'projectile
  :type 'string)

(defcustom projectile-tags-command "ctags -Re -f \"%s\" %s"
  "The command Projectile's going to use to generate a TAGS file."
  :group 'projectile
  :type 'string)

(defcustom projectile-sort-order 'default
  "The sort order used for a project's files."
  :group 'projectile
  :type '(radio
          (const :tag "default" default)
          (const :tag "recentf" recentf)
          (const :tag "recently active" recently-active)
          (const :tag "access time" access-time)
          (const :tag "modification time" modification-time)))

(defcustom projectile-verbose t
  "Echo messages that are not errors."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-buffers-filter-function nil
  "A function used to filter the buffers in `projectile-project-buffers'.

The function should accept and return a list of Emacs buffers.
Two example filter functions are shipped by default -
`projectile-buffers-with-file' and
`projectile-buffers-with-file-or-process'."
  :group 'projectile
  :type 'symbol)

(defcustom projectile-project-name nil
  "If this value is non-nil, it will be used as project name.

It has precedence over function `projectile-project-name-function'."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-project-name-function 'projectile-default-project-name
  "A function that receives the project-root and returns the project name.

If variable `projectile-project-name' is non-nil, this function will not be used."
  :group 'projectile
  :type 'symbol
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-project-root-files
  '("rebar.config"       ; Rebar project file
    "project.clj"        ; Leiningen project file
    "build.boot"         ; Boot-clj project file
    "SConstruct"         ; Scons project file
    "pom.xml"            ; Maven project file
    "build.sbt"          ; SBT project file
    "gradlew"            ; Gradle wrapper script
    "build.gradle"       ; Gradle project file
    "Gemfile"            ; Bundler file
    "requirements.txt"   ; Pip file
    "setup.py"           ; Setuptools file
    "tox.ini"            ; Tox file
    "package.json"       ; npm package file
    "gulpfile.js"        ; Gulp build file
    "Gruntfile.js"       ; Grunt project file
    "bower.json"         ; Bower project file
    "composer.json"      ; Composer project file
    "Cargo.toml"         ; Cargo project file
    "mix.exs"            ; Elixir mix project file
    "stack.yaml"         ; Haskell's stack tool based project
    "info.rkt"           ; Racket package description file
    "TAGS"               ; etags/ctags are usually in the root of project
    "GTAGS"              ; GNU Global tags
    )
  "A list of files considered to mark the root of a project.
The topmost match has precedence."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-project-root-files-bottom-up
  '(".projectile" ; projectile project marker
    ".git"        ; Git VCS root dir
    ".hg"         ; Mercurial VCS root dir
    ".fslckout"   ; Fossil VCS root dir
    ".bzr"        ; Bazaar VCS root dir
    "_darcs"      ; Darcs VCS root dir
    )
  "A list of files considered to mark the root of a project.
The bottommost (parentmost) match has precedence."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-project-root-files-top-down-recurring
  '(".svn" ; Svn VCS root dir
    "CVS"  ; Csv VCS root dir
    "Makefile")
  "A list of files considered to mark the root of a project.
The search starts at the top and descends down till a directory
that contains a match file but its parent does not.  Thus, it's a
bottommost match in the topmost sequence of directories
containing a root file."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-project-root-files-functions
  '(projectile-root-local
    projectile-root-bottom-up
    projectile-root-top-down
    projectile-root-top-down-recurring)
  "A list of functions for finding project roots."
  :group 'projectile
  :type '(repeat function))

(defcustom projectile-globally-ignored-files
  (list projectile-tags-file-name)
  "A list of files globally ignored by projectile."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-globally-unignored-files nil
  "A list of files globally unignored by projectile."
  :group 'projectile
  :type '(repeat string)
  :package-version '(projectile "0.14.0"))

(defcustom projectile-globally-ignored-file-suffixes
  nil
  "A list of file suffixes globally ignored by projectile."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-globally-ignored-directories
  '(".idea"
    ".eunit"
    ".git"
    ".hg"
    ".fslckout"
    ".bzr"
    "_darcs"
    ".tox"
    ".svn"
    ".stack-work")
  "A list of directories globally ignored by projectile."
  :safe (lambda (x) (not (remq t (mapcar #'stringp x))))
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-globally-unignored-directories nil
  "A list of directories globally unignored by projectile."
  :group 'projectile
  :type '(repeat string)
  :package-version '(projectile "0.14.0"))

(defcustom projectile-globally-ignored-modes
  '("erc-mode"
    "help-mode"
    "completion-list-mode"
    "Buffer-menu-mode"
    "gnus-.*-mode"
    "occur-mode")
  "A list of regular expressions for major modes ignored by projectile.

If a buffer is using a given major mode, projectile will ignore
it for functions working with buffers."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-globally-ignored-buffers nil
  "A list of buffer-names ignored by projectile.

If a buffer is in the list projectile will ignore
it for functions working with buffers."
  :group 'projectile
  :type '(repeat string)
  :package-version '(projectile . "0.12.0"))

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

(defcustom projectile-find-dir-includes-top-level nil
  "If true, add top-level dir to options offered by `projectile-find-dir'."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-use-git-grep nil
  "If true, use `vc-git-grep' in git projects."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-grep-finished-hook nil
  "Hooks run when `projectile-grep' finishes."
  :group 'projectile
  :type 'hook
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-test-prefix-function 'projectile-test-prefix
  "Function to find test files prefix based on PROJECT-TYPE."
  :group 'projectile
  :type 'function)

(defcustom projectile-test-suffix-function 'projectile-test-suffix
  "Function to find test files suffix based on PROJECT-TYPE."
  :group 'projectile
  :type 'function)


;;; Idle Timer
(defvar projectile-idle-timer nil
  "The timer object created when `projectile-enable-idle-timer' is non-nil.")

(defcustom projectile-idle-timer-seconds 30
  "The idle period to use when `projectile-enable-idle-timer' is non-nil."
  :group 'projectile
  :type 'number)

(defcustom projectile-idle-timer-hook '(projectile-regenerate-tags)
  "The hook run when `projectile-enable-idle-timer' is non-nil."
  :group 'projectile
  :type '(repeat symbol))

(defcustom projectile-enable-idle-timer nil
  "Enables idle timer hook `projectile-idle-timer-functions'.

When `projectile-enable-idle-timer' is non-nil, the hook
`projectile-idle-timer-hook' is run each time Emacs has been idle
for `projectile-idle-timer-seconds' seconds and we're in a
project."
  :group 'projectile
  :set (lambda (symbol value)
         (set symbol value)
         (when projectile-idle-timer
           (cancel-timer projectile-idle-timer))
         (setq projectile-idle-timer nil)
         (when projectile-enable-idle-timer
           (setq projectile-idle-timer (run-with-idle-timer
                                        projectile-idle-timer-seconds t
                                        (lambda ()
                                          (when (projectile-project-p)
                                            (run-hooks 'projectile-idle-timer-hook)))))))
  :type 'boolean)

;;; Serialization
(defun projectile-serialize (data filename)
  "Serialize DATA to FILENAME.

The saved data can be restored with `projectile-unserialize'."
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (let (print-length) (prin1-to-string data))))))

(defun projectile-unserialize (filename)
  "Read data serialized by `projectile-serialize' from FILENAME."
  (with-demoted-errors
      "Error during file deserialization: %S"
    (when (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        ;; this will blow up if the contents of the file aren't
        ;; lisp data structures
        (read (buffer-string))))))

(defvar projectile-projects-cache nil
  "A hashmap used to cache project file names to speed up related operations.")

(defvar projectile-project-root-cache (make-hash-table :test 'equal)
  "Cached value of function `projectile-project-root`.")

(defvar projectile-project-type-cache (make-hash-table :test 'equal)
  "A hashmap used to cache project type to speed up related operations.")

(defvar projectile-known-projects nil
  "List of locations where we have previously seen projects.
The list of projects is ordered by the time they have been accessed.

See also `projectile-remove-known-project',
`projectile-cleanup-known-projects' and `projectile-clear-known-projects'.")

(defvar projectile-known-projects-on-file nil
  "List of known projects reference point.

Contains a copy of `projectile-known-projects' when it was last
synchronized with `projectile-known-projects-file'.")

(defcustom projectile-known-projects-file
  (expand-file-name "projectile-bookmarks.eld"
                    user-emacs-directory)
  "Name and location of the Projectile's known projects file."
  :group 'projectile
  :type 'string)

(defcustom projectile-ignored-projects nil
  "A list of projects not to be added to `projectile-known-projects'."
  :group 'projectile
  :type '(repeat :tag "Project list" directory)
  :package-version '(projectile . "0.11.0"))

(defcustom projectile-ignored-project-function nil
  "Function to decide if a project is added to `projectile-known-projects'.

Can be either nil, or a function that takes the truename of the
project root as argument and returns non-nil if the project is to
be ignored or nil otherwise.

This function is only called if the project is not listed in
`projectile-ignored-projects'.

A suitable candidate would be `file-remote-p' to ignore remote
projects."
  :group 'projectile
  :type '(choice
          (const :tag "Nothing" nil)
          (const :tag "Remote files" file-remote-p)
          function)
  :package-version '(projectile . "0.13.0"))


;;; Version information

;;;###autoload
(defun projectile-version (&optional show-version)
  "Get the Projectile version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (if (require 'pkg-info nil t)
      (let ((version (pkg-info-version-info 'projectile)))
        (when show-version
          (message "Projectile version: %s" version))
        version)
    (error "Cannot determine version without package pkg-info")))


;;; Caching
(defvar projectile-file-exists-cache
  (make-hash-table :test 'equal)
  "Cached `projectile-file-exists-p' results.")

(defvar projectile-file-exists-cache-timer nil
  "Timer for scheduling`projectile-file-exists-cache-cleanup'.")

(defun projectile-file-exists-cache-cleanup ()
  "Removed timed out cache entries and reschedules or remove the
timer if no more items are in the cache."
  (let ((now (current-time)))
    (maphash (lambda (key value)
               (if (time-less-p (cdr value) now)
                   (remhash key projectile-file-exists-cache)))
             projectile-file-exists-cache)
    (setq projectile-file-exists-cache-timer
          (if (> (hash-table-count projectile-file-exists-cache) 0)
              (run-with-timer 10 nil 'projectile-file-exists-cache-cleanup)))))

(defun projectile-file-exists-p (filename)
  "Return t if file FILENAME exist.
A wrapper around `file-exists-p' with additional caching support."
  (let* ((file-remote (file-remote-p filename))
         (expire-seconds
          (if file-remote
              (and projectile-file-exists-remote-cache-expire
                   (> projectile-file-exists-remote-cache-expire 0)
                   projectile-file-exists-remote-cache-expire)
            (and projectile-file-exists-local-cache-expire
                 (> projectile-file-exists-local-cache-expire 0)
                 projectile-file-exists-local-cache-expire)))
         (remote-file-name-inhibit-cache (if expire-seconds
                                             expire-seconds
                                           remote-file-name-inhibit-cache)))
    (if (not expire-seconds)
        (file-exists-p filename)
      (let* ((current-time (current-time))
             (cached (gethash filename projectile-file-exists-cache))
             (cached-value (if cached (car cached)))
             (cached-expire (if cached (cdr cached)))
             (cached-expired (if cached (time-less-p cached-expire current-time) t))
             (value (or (and (not cached-expired) cached-value)
                        (if (file-exists-p filename) 'found 'notfound))))
        (when (or (not cached) cached-expired)
          (puthash filename
                   (cons value (time-add current-time (seconds-to-time expire-seconds)))
                   projectile-file-exists-cache))
        (unless projectile-file-exists-cache-timer
          (setq projectile-file-exists-cache-timer
                (run-with-timer 10 nil 'projectile-file-exists-cache-cleanup)))
        (equal value 'found)))))

;;;###autoload
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
    (setq projectile-project-root-cache (make-hash-table :test 'equal))
    (remhash project-root projectile-project-type-cache)
    (remhash project-root projectile-projects-cache)
    (projectile-serialize-cache)
    (when projectile-verbose
      (message "Invalidated Projectile cache for %s."
               (propertize project-root 'face 'font-lock-keyword-face))))
  (when (fboundp 'recentf-cleanup)
    (recentf-cleanup)))

(defun projectile-cache-project (project files)
  "Cache PROJECTs FILES.
The cache is created both in memory and on the hard drive."
  (when projectile-enable-caching
    (puthash project files projectile-projects-cache)
    (projectile-serialize-cache)))

;;;###autoload
(defun projectile-purge-file-from-cache (file)
  "Purge FILE from the cache of the current project."
  (interactive
   (list (projectile-completing-read
          "Remove file from cache: "
          (projectile-current-project-files))))
  (let* ((project-root (projectile-project-root))
         (project-cache (gethash project-root projectile-projects-cache)))
    (if (projectile-file-cached-p file project-root)
        (progn
          (puthash project-root (remove file project-cache) projectile-projects-cache)
          (projectile-serialize-cache)
          (when projectile-verbose
            (message "%s removed from cache" file)))
      (error "%s is not in the cache" file))))

;;;###autoload
(defun projectile-purge-dir-from-cache (dir)
  "Purge DIR from the cache of the current project."
  (interactive
   (list (projectile-completing-read
          "Remove directory from cache: "
          (projectile-current-project-dirs))))
  (let* ((project-root (projectile-project-root))
         (project-cache (gethash project-root projectile-projects-cache)))
    (puthash project-root
             (--filter (string-prefix-p dir it) project-cache)
             projectile-projects-cache)))

(defun projectile-file-cached-p (file project)
  "Check if FILE is already in PROJECT cache."
  (member file (gethash project projectile-projects-cache)))

;;;###autoload
(defun projectile-cache-current-file ()
  "Add the currently visited file to the cache."
  (interactive)
  (let ((current-project (projectile-project-root)))
    (when (and (buffer-file-name) (gethash (projectile-project-root) projectile-projects-cache))
      (let* ((abs-current-file (file-truename (buffer-file-name)))
             (current-file (file-relative-name abs-current-file current-project)))
        (unless (or (projectile-file-cached-p current-file current-project)
                    (projectile-ignored-directory-p (file-name-directory abs-current-file))
                    (projectile-ignored-file-p abs-current-file))
          (puthash current-project
                   (cons current-file (gethash current-project projectile-projects-cache))
                   projectile-projects-cache)
          (projectile-serialize-cache)
          (message "File %s added to project %s cache."
                   (propertize current-file 'face 'font-lock-keyword-face)
                   (propertize current-project 'face 'font-lock-keyword-face)))))))

;; cache opened files automatically to reduce the need for cache invalidation
(defun projectile-cache-files-find-file-hook ()
  "Function for caching files with `find-file-hook'."
  (when (and projectile-enable-caching (projectile-project-p))
    (projectile-cache-current-file)))

(defun projectile-cache-projects-find-file-hook ()
  "Function for caching projects with `find-file-hook'."
  (when (projectile-project-p)
    (let ((known-projects (and (sequencep projectile-known-projects)
                               (copy-sequence projectile-known-projects))))
      (projectile-add-known-project (projectile-project-root))
      (unless (equal known-projects projectile-known-projects)
        (projectile-merge-known-projects)))))


(defun projectile-maybe-invalidate-cache (force)
  "Invalidate if FORCE or project's dirconfig newer than cache."
  (when (or force (file-newer-than-file-p (projectile-dirconfig-file)
                                          projectile-cache-file))
    (projectile-invalidate-cache nil)))


(defadvice delete-file (before purge-from-projectile-cache (filename &optional trash))
  (if (and projectile-enable-caching (projectile-project-p))
      (let* ((project-root (projectile-project-root))
             (true-filename (file-truename filename))
             (relative-filename (file-relative-name true-filename project-root)))
        (if (projectile-file-cached-p relative-filename project-root)
            (projectile-purge-file-from-cache relative-filename)))))


;;; Project root related utilities
(defun projectile-parent (path)
  "Return the parent directory of PATH.
PATH may be a file or directory and directory paths may end with a slash."
  (directory-file-name (file-name-directory (directory-file-name (expand-file-name path)))))

(defun projectile-locate-dominating-file (file name)
  "Look up the directory hierarchy from FILE for a directory containing NAME.
Stop at the first parent directory containing a file NAME,
and return the directory.  Return nil if not found.
Instead of a string, NAME can also be a predicate taking one argument
\(a directory) and returning a non-nil value if that directory is the one for
which we're looking."
  ;; copied from files.el (stripped comments) emacs-24 bzr branch 2014-03-28 10:20
  (setq file (abbreviate-file-name file))
  (let ((root nil)
        try)
    (while (not (or root
                    (null file)
                    (string-match locate-dominating-stop-dir-regexp file)))
      (setq try (if (stringp name)
                    (projectile-file-exists-p (expand-file-name name file))
                  (funcall name file)))
      (cond (try (setq root file))
            ((equal file (setq file (file-name-directory
                                     (directory-file-name file))))
             (setq file nil))))
    (and root (expand-file-name (file-name-as-directory root)))))

(defvar-local projectile-project-root nil
  "Defines a custom Projectile project root.
This is intended to be used as a file local variable.")

(defun projectile-root-local (_dir)
  "A simple wrapper around `projectile-project-root'."
  projectile-project-root)

(defun projectile-root-top-down (dir &optional list)
  "Identify a project root in DIR by top-down search for files in LIST.
If LIST is nil, use `projectile-project-root-files' instead.
Return the first (topmost) matched directory or nil if not found."
  (projectile-locate-dominating-file
   dir
   (lambda (dir)
     (--first (projectile-file-exists-p (expand-file-name it dir))
              (or list projectile-project-root-files)))))

(defun projectile-root-bottom-up (dir &optional list)
  "Identify a project root in DIR by bottom-up search for files in LIST.
If LIST is nil, use `projectile-project-root-files-bottom-up' instead.
Return the first (bottommost) matched directory or nil if not found."
  (--some (projectile-locate-dominating-file dir it)
          (or list projectile-project-root-files-bottom-up)))

(defun projectile-root-top-down-recurring (dir &optional list)
  "Identify a project root in DIR by recurring top-down search for files in LIST.
If LIST is nil, use `projectile-project-root-files-top-down-recurring'
instead.  Return the last (bottommost) matched directory in the
topmost sequence of matched directories.  Nil otherwise."
  (--some (projectile-locate-dominating-file
           dir
           (lambda (dir)
             (and (projectile-file-exists-p (expand-file-name it dir))
                  (or (string-match locate-dominating-stop-dir-regexp (projectile-parent dir))
                      (not (projectile-file-exists-p (expand-file-name it (projectile-parent dir))))))))
          (or list projectile-project-root-files-top-down-recurring)))

(defun projectile-project-root ()
  "Retrieves the root directory of a project if available.
The current directory is assumed to be the project's root otherwise."
  (let ((dir default-directory))
    (or (--some (let* ((cache-key (format "%s-%s" it dir))
                       (cache-value (gethash cache-key projectile-project-root-cache)))
                  (if (and cache-value (file-exists-p cache-value))
                      cache-value
                    (let ((value (funcall it (file-truename dir))))
                      (puthash cache-key value projectile-project-root-cache)
                      value)))
                projectile-project-root-files-functions)
        (if projectile-require-project-root
            (error "You're not in a project")
          default-directory))))

(defun projectile-file-truename (file-name)
  "Return the truename of FILE-NAME.
A thin wrapper around `file-truename' that handles nil."
  (when file-name
    (file-truename file-name)))

(defun projectile-project-p ()
  "Check if we're in a project."
  (condition-case nil
      (projectile-project-root)
    (error nil)))

(defun projectile-default-project-name (project-root)
  "Default function used create project name to be displayed based on the value of PROJECT-ROOT."
  (file-name-nondirectory (directory-file-name project-root)))

(defun projectile-project-name ()
  "Return project name."
  (if projectile-project-name
      projectile-project-name
    (let ((project-root
           (condition-case nil
               (projectile-project-root)
             (error nil))))
      (if project-root
          (funcall projectile-project-name-function project-root)
        "-"))))


;;; Project indexing
(defun projectile-get-project-directories ()
  "Get the list of project directories that are of interest to the user."
  (-map (lambda (subdir) (concat (projectile-project-root) subdir))
        (or (nth 0 (projectile-parse-dirconfig-file)) '(""))))

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
          (projectile-adjust-files (projectile-dir-files-external root directory))))))

(defun projectile-dir-files-native (root directory)
  "Get the files for ROOT under DIRECTORY using just Emacs Lisp."
  (let ((progress-reporter
         (make-progress-reporter
          (format "Projectile is indexing %s"
                  (propertize directory 'face 'font-lock-keyword-face)))))
    ;; we need the files with paths relative to the project root
    (-map (lambda (file) (file-relative-name file root))
          (projectile-index-directory directory (projectile-filtering-patterns)
                                      progress-reporter))))

(defun projectile-dir-files-external (root directory)
  "Get the files for ROOT under DIRECTORY using external tools."
  (let ((default-directory directory))
    (-map (lambda (file)
            (file-relative-name (expand-file-name file directory) root))
          (projectile-get-repo-files))))

(defcustom projectile-git-command "git ls-files -zco --exclude-standard"
  "Command used by projectile to get the files in a git project."
  :group 'projectile
  :type 'string)

(defcustom projectile-git-submodule-command "git submodule --quiet foreach 'echo $path' | tr '\\n' '\\0'"
  "Command used by projectile to get the files in git submodules."
  :group 'projectile
  :type 'string)

(defcustom projectile-git-ignored-command "git ls-files -zcoi --exclude-standard"
  "Command used by projectile to get the ignored files in a git project."
  :group 'projectile
  :type 'string
  :package-version '(projectile "0.14.0"))

(defcustom projectile-hg-command "hg locate -0 -I ."
  "Command used by projectile to get the files in a hg project."
  :group 'projectile
  :type 'string)

(defcustom projectile-fossil-command "fossil ls | tr '\\n' '\\0'"
  "Command used by projectile to get the files in a fossil project."
  :group 'projectile
  :type 'string)

(defcustom projectile-bzr-command "bzr ls -R --versioned -0"
  "Command used by projectile to get the files in a bazaar project."
  :group 'projectile
  :type 'string)

(defcustom projectile-darcs-command "darcs show files -0 . "
  "Command used by projectile to get the files in a darcs project."
  :group 'projectile
  :type 'string)

(defcustom projectile-svn-command "svn list -R . | grep -v '$/' | tr '\\n' '\\0'"
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

(defun projectile-get-sub-projects-command ()
  (let ((vcs (projectile-project-vcs)))
    (cond
     ((eq vcs 'git) projectile-git-submodule-command)
     (t ""))))

(defun projectile-get-ext-ignored-command ()
  "Determine which external command to invoke based on the project's VCS."
  (let ((vcs (projectile-project-vcs)))
    (cond
     ((eq vcs 'git) projectile-git-ignored-command)
     (t (error "VCS command for ignored files not implemented yet")))))

(defun projectile-get-all-sub-projects (project)
  "Get all sub-projects for a given project.

PROJECT is base directory to start search recursively."
  (let ((submodules (projectile-get-immediate-sub-projects project)))
    (cond
     ((null submodules)
      nil)
     (t
      (nconc submodules (-flatten
                         ;; recursively get sub-projects of each sub-project
                         (mapcar (lambda (s)
                                   (projectile-get-all-sub-projects s)) submodules)))))))

(defun projectile-get-immediate-sub-projects (path)
  "Get immediate sub-projects for a given project without recursing.

PATH is the vcs root or project root from which to start
searching, and should end with an appropriate path delimiter, such as
'/' or a '\\'.

If the vcs get-sub-projects query returns results outside of path,
they are excluded from the results of this function."
  (let* ((default-directory path)
         ;; search for sub-projects under current project `project'
         (submodules (mapcar
                      (lambda (s)
                        (file-name-as-directory (expand-file-name s default-directory)))
                      (projectile-files-via-ext-command (projectile-get-sub-projects-command))))
         (project-child-folder-regex
          (concat "\\`"
                  (regexp-quote path))))

    ;; If project root is inside of an VCS folder, but not actually an
    ;; VCS root itself, submodules external to the project will be
    ;; included in the VCS get sub-projects result. Let's remove them.
    (-filter (lambda (submodule)
               (string-match-p project-child-folder-regex
                               submodule))
             submodules)))

(defun projectile-get-sub-projects-files ()
  "Get files from sub-projects recursively."
  (-flatten
   (mapcar (lambda (s)
             (let ((default-directory s))
               (mapcar (lambda (f)
                         (concat s f))
                       (projectile-files-via-ext-command projectile-git-command))))
           (condition-case nil
               (projectile-get-all-sub-projects (projectile-project-root))
             (error nil)))))

(defun projectile-get-repo-files ()
  "Get a list of the files in the project, including sub-projects."
  (cond
   ((eq (projectile-project-vcs) 'git)
    (nconc (projectile-files-via-ext-command (projectile-get-ext-command))
           (projectile-get-sub-projects-files)))
   (t (projectile-files-via-ext-command (projectile-get-ext-command)))))

(defun projectile-get-repo-ignored-files ()
  "Get a list of the files ignored in the project."
  (let ((cmd (projectile-get-ext-ignored-command)))
    (projectile-files-via-ext-command cmd)))

(defun projectile-files-via-ext-command (command)
  "Get a list of relative file names in the project root by executing COMMAND."
  (split-string (shell-command-to-string command) "\0" t))

(defun projectile-index-directory (directory patterns progress-reporter)
  "Index DIRECTORY taking into account PATTERNS.
The function calls itself recursively until all sub-directories
have been indexed.  The PROGRESS-REPORTER is updated while the
function is executing."
  (--mapcat
   (unless (or (and patterns (projectile-ignored-rel-p it directory patterns))
               (member (file-name-nondirectory (directory-file-name it))
                       '("." ".." ".svn" ".cvs")))
     (progress-reporter-update progress-reporter)
     (if (file-directory-p it)
         (unless (projectile-ignored-directory-p
                  (file-name-as-directory it))
           (projectile-index-directory it patterns progress-reporter))
       (unless (projectile-ignored-file-p it)
         (list it))))
   (directory-files directory t)))

(defun projectile-adjust-files (files)
  "First remove ignored files from FILES, then add back unignored files."
  (projectile-add-unignored (projectile-remove-ignored files)))

(defun projectile-remove-ignored (files &optional subdirectories)
  "Remove ignored files and folders from FILES.

Operates on filenames relative to the project root.  Optionally,
you can filter ignored files in subdirectories by setting
SUBDIRECTORIES to a non-nil value."
  (let ((ignored (append (projectile-ignored-files-rel)
                         (projectile-ignored-directories-rel))))
    (-remove (lambda (file)
               (or (--any-p (string-prefix-p it (if subdirectories
                                                    (file-name-nondirectory file)
                                                  file))
                            ignored)
                   (--any-p (string-suffix-p it file) projectile-globally-ignored-file-suffixes)))
             files)))

(defun projectile-keep-ignored-files (files)
  "Filter FILES to retain only those that are ignored."
  (when files
    (-filter (lambda (file)
               (--some (string-prefix-p it file) files))
             (projectile-get-repo-ignored-files))))

(defun projectile-add-unignored (files)
  "This adds unignored files to FILES.

Useful because the VCS may not return ignored files at all.  In
this case unignored files will be absent from FILES."
  (let ((unignored-files (projectile-keep-ignored-files
                          (projectile-unignored-files-rel)))
        (unignored-paths (projectile-remove-ignored
                          (projectile-keep-ignored-files
                           (projectile-unignored-directories-rel))
                          'subdirectories)))
    (append files unignored-files unignored-paths)))

(defun projectile-buffers-with-file (buffers)
  "Return only those BUFFERS backed by files."
  (--filter (buffer-file-name it) buffers))

(defun projectile-buffers-with-file-or-process (buffers)
  "Return only those BUFFERS backed by files or processes."
  (--filter (or (buffer-file-name it)
                (get-buffer-process it)) buffers))

(defun projectile-project-buffers ()
  "Get a list of project buffers."
  (let* ((project-root (projectile-project-root))
         (all-buffers (-filter (lambda (buffer)
                                 (projectile-project-buffer-p buffer project-root))
                               (buffer-list))))
    (if projectile-buffers-filter-function
        (funcall projectile-buffers-filter-function all-buffers)
      all-buffers)))

(defun projectile-process-current-project-buffers (action)
  "Process the current project's buffers using ACTION."
  (let ((project-buffers (projectile-project-buffers)))
    (dolist (buffer project-buffers)
      (funcall action buffer))))

(defun projectile-project-buffer-files ()
  "Get a list of project buffer files."
  (let ((project-root (projectile-project-root)))
    (->> (projectile-buffers-with-file (projectile-project-buffers))
         (-map (lambda (buffer)
                 (file-relative-name (buffer-file-name buffer) project-root))))))

(defun projectile-project-buffer-p (buffer project-root)
  "Check if BUFFER is under PROJECT-ROOT."
  (with-current-buffer buffer
    (and (not (string-prefix-p " " (buffer-name buffer)))
         (not (projectile-ignored-buffer-p buffer))
         (string-equal (file-remote-p default-directory)
                       (file-remote-p project-root))
         (not (string-match-p "^http\\(s\\)?://" default-directory))
         (string-prefix-p project-root (file-truename default-directory)))))

(defun projectile-ignored-buffer-p (buffer)
  "Check if BUFFER should be ignored."
  (or
   (member (buffer-name buffer) projectile-globally-ignored-buffers)
   (with-current-buffer buffer
     (--any-p (string-match-p (concat "^" it "$")
                              (symbol-name major-mode))
              projectile-globally-ignored-modes))))

(defun projectile-recently-active-files ()
  "Get list of recently active files.

Files are ordered by recently active buffers, and then recently
opened through use of recentf."
  (let ((project-buffer-files (projectile-project-buffer-files)))
    (append project-buffer-files
            (-difference (projectile-recentf-files)
                         project-buffer-files))))

(defun projectile-project-buffer-names ()
  "Get a list of project buffer names."
  (-map #'buffer-name (projectile-project-buffers)))

(defun projectile-prepend-project-name (string)
  "Prepend the current project's name to STRING."
  (format "[%s] %s" (projectile-project-name) string))

(defun projectile-read-buffer-to-switch (prompt)
  "Read the name of a buffer to switch to, prompting with PROMPT.

This function excludes the current buffer from the offered
choices."
  (projectile-completing-read
   prompt
   (-remove-item (buffer-name (current-buffer))
                 (projectile-project-buffer-names))))

;;;###autoload
(defun projectile-switch-to-buffer ()
  "Switch to a project buffer."
  (interactive)
  (switch-to-buffer
   (projectile-read-buffer-to-switch "Switch to buffer: ")))

;;;###autoload
(defun projectile-switch-to-buffer-other-window ()
  "Switch to a project buffer and show it in another window."
  (interactive)
  (switch-to-buffer-other-window
   (projectile-read-buffer-to-switch "Switch to buffer: ")))

;;;###autoload
(defun projectile-display-buffer ()
  "Display a project buffer in another window without selecting it."
  (interactive)
  (display-buffer
   (projectile-completing-read
    "Display buffer: "
    (projectile-project-buffer-names))))

;;;###autoload
(defun projectile-project-buffers-other-buffer ()
  "Switch to the most recently selected buffer project buffer.
Only buffers not visible in windows are returned."
  (interactive)
  (switch-to-buffer (car (projectile-project-buffers-non-visible))) nil t)

(defun projectile-project-buffers-non-visible ()
  "Get a list of non visible project buffers."
  (-filter (lambda (buffer)
             (not (get-buffer-window buffer 'visible)))
           (projectile-project-buffers)))

;;;###autoload
(defun projectile-multi-occur ()
  "Do a `multi-occur' in the project's buffers."
  (interactive)
  (multi-occur (projectile-project-buffers)
               (car (occur-read-primary-args))))

(defun projectile-normalise-paths (patterns)
  "Remove leading `/' from the elements of PATTERNS."
  (-non-nil (--map (and (string-prefix-p "/" it)
                        ;; remove the leading /
                        (substring it 1))
                   patterns)))

(defun projectile-expand-paths (paths)
  "Expand the elements of PATHS.

Elements containing wildcards are expanded and spliced into the
resulting paths.  The returned PATHS are absolute, based on the
projectile project root."
  (let ((default-directory (projectile-project-root)))
    (-flatten (-map
               (lambda (pattern)
                 (or (file-expand-wildcards pattern t)
                     (projectile-expand-root pattern)))
               paths))))

(defun projectile-normalise-patterns (patterns)
  "Remove paths from PATTERNS."
  (--remove (string-prefix-p "/" it) patterns))

(defun projectile-make-relative-to-root (files)
  "Make FILES relative to the project root."
  (let ((project-root (projectile-project-root)))
    (--map (file-relative-name it project-root) files)))

(defun projectile-ignored-directory-p (directory)
  "Check if DIRECTORY should be ignored."
  (member directory (projectile-ignored-directories)))

(defun projectile-ignored-file-p (file)
  "Check if FILE should be ignored."
  (member file (projectile-ignored-files)))

(defun projectile-check-pattern-p (file pattern)
  "Check if FILE meets PATTERN."
  (or (string-suffix-p (directory-file-name pattern)
                       (directory-file-name file))
      (member file (file-expand-wildcards pattern t))))

(defun projectile-ignored-rel-p (file directory patterns)
  "Check if FILE should be ignored relative to DIRECTORY
according to PATTERNS: (ignored . unignored)"
  (let ((default-directory directory))
    (and (--any-p (projectile-check-pattern-p file it) (car patterns))
         (--none-p (projectile-check-pattern-p file it) (cdr patterns)))))

(defun projectile-ignored-files ()
  "Return list of ignored files."
  (-difference
   (-map
    #'projectile-expand-root
    (append
     projectile-globally-ignored-files
     (projectile-project-ignored-files)))
   (projectile-unignored-files)))

(defun projectile-ignored-directories ()
  "Return list of ignored directories."
  (-difference
   (-map
    #'file-name-as-directory
    (-map
     #'projectile-expand-root
     (append
      projectile-globally-ignored-directories
      (projectile-project-ignored-directories))))
   (projectile-unignored-directories)))

(defun projectile-ignored-directories-rel ()
  "Return list of ignored directories, relative to the root."
  (projectile-make-relative-to-root (projectile-ignored-directories)))

(defun projectile-ignored-files-rel ()
  "Return list of ignored files, relative to the root."
  (projectile-make-relative-to-root (projectile-ignored-files)))

(defun projectile-project-ignored-files ()
  "Return list of project ignored files. Unignored files are not
included."
  (-remove 'file-directory-p (projectile-project-ignored)))

(defun projectile-project-ignored-directories ()
  "Return list of project ignored directories. Unignored
directories are not included."
  (-filter 'file-directory-p (projectile-project-ignored)))

(defun projectile-paths-to-ignore ()
  "Return a list of ignored project paths."
  (projectile-normalise-paths (nth 1 (projectile-parse-dirconfig-file))))

(defun projectile-patterns-to-ignore ()
  "Return a list of relative file patterns."
  (projectile-normalise-patterns (nth 1 (projectile-parse-dirconfig-file))))

(defun projectile-project-ignored ()
  "Return list of project ignored files/directories. Unignored
files/directories are not included."
  (let ((paths (projectile-paths-to-ignore)))
    (projectile-expand-paths paths)))

(defun projectile-unignored-files ()
  "Return list of unignored files."
  (-map
   #'projectile-expand-root
   (append
    projectile-globally-unignored-files
    (projectile-project-unignored-files))))

(defun projectile-unignored-directories ()
  "Return list of unignored directories."
  (-map
   #'file-name-as-directory
   (-map
    #'projectile-expand-root
    (append
     projectile-globally-unignored-directories
     (projectile-project-unignored-directories)))))

(defun projectile-unignored-directories-rel ()
  "Return list of unignored directories, relative to the root."
  (projectile-make-relative-to-root (projectile-unignored-directories)))

(defun projectile-unignored-files-rel ()
  "Return list of unignored files, relative to the root."
  (projectile-make-relative-to-root (projectile-unignored-files)))

(defun projectile-project-unignored-files ()
  "Return list of project unignored files."
  (-remove 'file-directory-p (projectile-project-unignored)))

(defun projectile-project-unignored-directories ()
  "Return list of project unignored directories."
  (-filter 'file-directory-p (projectile-project-unignored)))

(defun projectile-paths-to-ensure ()
  "Return a list of unignored project paths."
  (projectile-normalise-paths (nth 2 (projectile-parse-dirconfig-file))))

(defun projectile-files-to-ensure ()
  (-flatten (--map (file-expand-wildcards it t)
                   (projectile-patterns-to-ensure))))

(defun projectile-patterns-to-ensure ()
  "Return a list of relative file patterns."
  (projectile-normalise-patterns (nth 2 (projectile-parse-dirconfig-file))))

(defun projectile-filtering-patterns ()
  (cons (projectile-patterns-to-ignore)
        (projectile-patterns-to-ensure)))

(defun projectile-project-unignored ()
  "Return list of project ignored files/directories."
  (delete-dups (append (projectile-expand-paths (projectile-paths-to-ensure))
                       (projectile-expand-paths (projectile-files-to-ensure)))))


(defun projectile-dirconfig-file ()
  "Return the absolute path to the project's dirconfig file."
  (expand-file-name ".projectile" (projectile-project-root)))

(defun projectile-parse-dirconfig-file ()
  "Parse project ignore file and return directories to ignore and keep.

The return value will be a list of three elements, the car being
the list of directories to keep, the cadr being the list of files
or directories to ignore, and the caddr being the list of files
or directories to ensure.

Strings starting with + will be added to the list of directories
to keep, and strings starting with - will be added to the list of
directories to ignore.  For backward compatibility, without a
prefix the string will be assumed to be an ignore string."
  (let (keep ignore ensure (dirconfig (projectile-dirconfig-file)))
    (when (projectile-file-exists-p dirconfig)
      (with-temp-buffer
        (insert-file-contents dirconfig)
        (while (not (eobp))
          (pcase (char-after)
            (?+ (push (buffer-substring (1+ (point)) (line-end-position)) keep))
            (?- (push (buffer-substring (1+ (point)) (line-end-position)) ignore))
            (?! (push (buffer-substring (1+ (point)) (line-end-position)) ensure))
            (_  (push (buffer-substring     (point)  (line-end-position)) ignore)))
          (forward-line)))
      (list (--map (file-name-as-directory (projectile-trim-string it))
                   (delete "" (reverse keep)))
            (-map  #'projectile-trim-string
                   (delete "" (reverse ignore)))
            (-map  #'projectile-trim-string
                   (delete "" (reverse ensure)))))))

(defun projectile-expand-root (name)
  "Expand NAME to project root.

Never use on many files since it's going to recalculate the
project-root for every file."
  (expand-file-name name (projectile-project-root)))

(defun projectile-completing-read (prompt choices &optional initial-input)
  "Present a project tailored PROMPT with CHOICES."
  (let ((prompt (projectile-prepend-project-name prompt)))
    (cond
     ((eq projectile-completion-system 'ido)
      (ido-completing-read prompt choices nil nil initial-input))
     ((eq projectile-completion-system 'default)
      (completing-read prompt choices nil nil initial-input))
     ((eq projectile-completion-system 'helm)
      (if (fboundp 'helm-comp-read)
          (helm-comp-read prompt choices
                          :initial-input initial-input
                          :candidates-in-buffer t
                          :must-match 'confirm)
        (user-error "Please install helm from \
https://github.com/emacs-helm/helm")))
     ((eq projectile-completion-system 'grizzl)
      (if (and (fboundp 'grizzl-completing-read)
               (fboundp 'grizzl-make-index))
          (grizzl-completing-read prompt (grizzl-make-index choices))
        (user-error "Please install grizzl from \
https://github.com/d11wtq/grizzl")))
     ((eq projectile-completion-system 'ivy)
      (if (fboundp 'ivy-read)
          (ivy-read prompt choices
                    :initial-input initial-input
                    :caller 'projectile-completing-read)
        (user-error "Please install ivy from \
https://github.com/abo-abo/swiper")))
     (t (funcall projectile-completion-system prompt choices)))))

(defun projectile-current-project-files ()
  "Return a list of files for the current project."
  (let ((files (and projectile-enable-caching
                    (gethash (projectile-project-root) projectile-projects-cache))))
    ;; nothing is cached
    (unless files
      (when projectile-enable-caching
        (message "Empty cache. Projectile is initializing cache..."))
      (setq files (-mapcat #'projectile-dir-files
                           (projectile-get-project-directories)))
      ;; cache the resulting list of files
      (when projectile-enable-caching
        (projectile-cache-project (projectile-project-root) files)))
    (projectile-sort-files files)))

(defun projectile-process-current-project-files (action)
  "Process the current project's files using ACTION."
  (let ((project-files (projectile-current-project-files))
        (default-directory (projectile-project-root)))
    (dolist (filename project-files)
      (funcall action filename))))

(defun projectile-current-project-dirs ()
  "Return a list of dirs for the current project."
  (-remove #'null (-distinct
                   (-map #'file-name-directory
                         (projectile-current-project-files)))))

(defun projectile-hash-keys (hash)
  "Return a list of all HASH keys."
  (let (allkeys)
    (maphash (lambda (k _v) (setq allkeys (cons k allkeys))) hash)
    allkeys))


;;; Interactive commands
(defcustom projectile-other-file-alist
  '(;; handle C/C++ extensions
    ("cpp" . ("h" "hpp" "ipp"))
    ("ipp" . ("h" "hpp" "cpp"))
    ("hpp" . ("h" "ipp" "cpp" "cc"))
    ("cxx" . ("h" "hxx" "ixx"))
    ("ixx" . ("h" "hxx" "cxx"))
    ("hxx" . ("h" "ixx" "cxx"))
    ("c" . ("h"))
    ("m" . ("h"))
    ("mm" . ("h"))
    ("h" . ("c" "cpp" "ipp" "hpp" "cxx" "ixx" "hxx" "m" "mm"))
    ("cc" . ("hh" "hpp"))
    ("hh" . ("cc"))

    ;; vertex shader and fragment shader extensions in glsl
    ("vert" . ("frag"))
    ("frag" . ("vert"))

    ;; handle files with no extension
    (nil . ("lock" "gpg"))
    ("lock" . (""))
    ("gpg" . (""))
    )
  "Alist of extensions for switching to file with the same name, using other extensions based on the extension of current file.")

;;;###autoload
(defun projectile-find-other-file (&optional flex-matching)
  "Switch between files with the same name but different extensions.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'."
  (interactive "P")
  (-if-let (other-files (projectile-get-other-files (buffer-file-name) (projectile-current-project-files) flex-matching))
      (if (= (length other-files) 1)
          (find-file (expand-file-name (car other-files) (projectile-project-root)))
        (find-file (expand-file-name (projectile-completing-read "Switch to: " other-files) (projectile-project-root))))
    (error "No other file found")))

;;;###autoload
(defun projectile-find-other-file-other-window (&optional flex-matching)
  "Switch between files with the same name but different extensions in other window.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'."
  (interactive "P")
  (-if-let (other-files (projectile-get-other-files (buffer-file-name) (projectile-current-project-files) flex-matching))
      (if (= (length other-files) 1)
          (find-file-other-window (expand-file-name (car other-files) (projectile-project-root)))
        (find-file-other-window (expand-file-name (projectile-completing-read "Switch to: " other-files) (projectile-project-root))))
    (error "No other file found")))

(defun projectile--file-name-sans-extensions (file-name)
  "Return FILE-NAME sans any extensions.
The extensions, in a filename, are what follows the first '.', with the exception of a leading '.'"
  (setq file-name (file-name-nondirectory file-name))
  (substring file-name 0 (string-match "\\..*" file-name 1)))

(defun projectile--file-name-extensions (file-name)
  "Return FILE-NAME's extensions.
The extensions, in a filename, are what follows the first '.', with the exception of a leading '.'"
  ;;would it make sense to return nil instead of an empty string if no extensions are found?
  (setq file-name (file-name-nondirectory file-name))
  (substring file-name (-if-let (extensions-start (string-match "\\..*" file-name 1)) (1+ extensions-start) (length file-name))))

(defun projectile-associated-file-name-extensions (file-name)
  "Return projectile-other-file-extensions associated to FILE-NAME's extensions.
If no associated other-file-extensions for the complete (nested) extension are found, remove subextensions from FILENAME's extensions until a match is found."
  (let ((current-extensions (projectile--file-name-extensions (file-name-nondirectory file-name))))
    (catch 'break
      (while (not (string= "" current-extensions))
        (-if-let (associated-extensions (cdr (assoc current-extensions projectile-other-file-alist)))
            (throw 'break associated-extensions))
        (setq current-extensions (projectile--file-name-extensions current-extensions))))))

(defun projectile-get-other-files (current-file project-file-list &optional flex-matching)
  "Narrow to files with the same names but different extensions.
Returns a list of possible files for users to choose.

With FLEX-MATCHING, match any file that contains the base name of current file"
  (let* ((file-ext-list (projectile-associated-file-name-extensions current-file))
         (fulldirname  (if (file-name-directory current-file)
                           (file-name-directory current-file) "./"))
         (dirname  (file-name-nondirectory (directory-file-name fulldirname)))
         (filename (projectile--file-name-sans-extensions current-file))
         (file-list (mapcar (lambda (ext)
                              (if flex-matching
                                  (concat ".*" filename ".*" "\." ext "\\'")
                                (concat "^" filename
                                        (unless (equal ext "")
                                          (concat  "\." ext))
                                        "\\'")))
                            file-ext-list))
         (candidates (-filter (lambda (project-file)
                                (string-match filename project-file))
                              project-file-list))
         (candidates
          (-flatten (mapcar
                     (lambda (file)
                       (-filter (lambda (project-file)
                                  (string-match file
                                                (concat (file-name-base project-file)
                                                        (unless (equal (file-name-extension project-file) nil)
                                                          (concat  "\." (file-name-extension project-file))))))
                                candidates))
                     file-list)))
         (candidates
          (-sort (lambda (file _)
                   (let ((candidate-dirname (file-name-nondirectory (directory-file-name (file-name-directory file)))))
                     (unless (equal fulldirname (file-name-directory file))
                       (equal dirname candidate-dirname))))
                 candidates)))
    candidates))

(defun projectile-select-files (project-files &optional arg)
  "Select a list of files based on filename at point.

With a prefix ARG invalidates the cache first."
  (projectile-maybe-invalidate-cache arg)
  (let* ((file (if (region-active-p)
                   (buffer-substring (region-beginning) (region-end))
                 (or (thing-at-point 'filename) "")))
         (file (if (string-match "\\.?\\./" file)
                   (file-relative-name (file-truename file) (projectile-project-root))
                 file))
         (files (if file
                    (-filter (lambda (project-file)
                               (string-match file project-file))
                             project-files)
                  nil)))
    files))

;;;###autoload
(defun projectile-find-file-dwim (&optional arg)
  "Jump to a project's files using completion based on context.

With a prefix ARG invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file' still switches to \"projectile/projectile.el\" immediately
 because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename like
 \"projectile/a\", a list of files with character 'a' in that directory is presented.

- If it finds nothing, display a list of all files in project for selecting."
  (interactive "P")
  (let* ((project-files (projectile-current-project-files))
         (files (projectile-select-files project-files arg)))
    (cond
     ((= (length files) 1)
      (find-file (expand-file-name (car files) (projectile-project-root))))
     ((> (length files) 1)
      (find-file (expand-file-name (projectile-completing-read "Switch to: " files) (projectile-project-root))))
     (t (find-file (expand-file-name (projectile-completing-read "Switch to: " project-files) (projectile-project-root)))))
    (run-hooks 'projectile-find-file-hook)))

;;;###autoload
(defun projectile-find-file-dwim-other-window (&optional arg)
  "Jump to a project's files using completion based on context in other window.

With a prefix ARG invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file' still switches to \"projectile/projectile.el\"
immediately because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename
like \"projectile/a\", a list of files with character 'a' in that directory
is presented.

- If it finds nothing, display a list of all files in project for selecting."
  (interactive "P")
  (let* ((project-files (projectile-current-project-files))
         (files (projectile-select-files project-files arg)))
    (cond
     ((= (length files) 1)
      (find-file-other-window (expand-file-name (car files) (projectile-project-root))))
     ((> (length files) 1)
      (find-file-other-window (expand-file-name (projectile-completing-read "Switch to: " files) (projectile-project-root))))
     (t (find-file-other-window (expand-file-name (projectile-completing-read "Switch to: " project-files) (projectile-project-root)))))
    (run-hooks 'projectile-find-file-hook)))

;;;###autoload
(defun projectile-find-file (&optional arg)
  "Jump to a project's file using completion.
With a prefix ARG invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache arg)
  (let ((file (projectile-completing-read "Find file: "
                                          (projectile-current-project-files))))
    (find-file (expand-file-name file (projectile-project-root)))
    (run-hooks 'projectile-find-file-hook)))

;;;###autoload
(defun projectile-find-file-other-window (&optional arg)
  "Jump to a project's file using completion and show it in another window.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache arg)
  (let ((file (projectile-completing-read "Find file: "
                                          (projectile-current-project-files))))
    (find-file-other-window (expand-file-name file (projectile-project-root)))
    (run-hooks 'projectile-find-file-hook)))

(defun projectile-sort-files (files)
  "Sort FILES according to `projectile-sort-order'."
  (pcase projectile-sort-order
    (`default files)
    (`recentf (projectile-sort-by-recentf-first files))
    (`recently-active (projectile-sort-by-recently-active-first files))
    (`modification-time (projectile-sort-by-modification-time files))
    (`access-time (projectile-sort-by-access-time files))))

(defun projectile-sort-by-recentf-first (files)
  "Sort FILES by a recent first scheme."
  (let ((project-recentf-files (projectile-recentf-files)))
    (append project-recentf-files
            (-difference files project-recentf-files))))

(defun projectile-sort-by-recently-active-first (files)
  "Sort FILES by most recently active buffers or opened files."
  (let ((project-recently-active-files (projectile-recently-active-files)))
    (append project-recently-active-files
            (-difference files project-recently-active-files))))

(defun projectile-sort-by-modification-time (files)
  "Sort FILES by modification time."
  (let ((default-directory (projectile-project-root)))
    (-sort (lambda (file1 file2)
             (let ((file1-mtime (nth 5 (file-attributes file1)))
                   (file2-mtime (nth 5 (file-attributes file2))))
               (not (time-less-p file1-mtime file2-mtime))))
           files)))

(defun projectile-sort-by-access-time (files)
  "Sort FILES by access time."
  (let ((default-directory (projectile-project-root)))
    (-sort (lambda (file1 file2)
             (let ((file1-atime (nth 4 (file-attributes file1)))
                   (file2-atime (nth 4 (file-attributes file2))))
               (not (time-less-p file1-atime file2-atime))))
           files)))

;;;###autoload
(defun projectile-find-dir (&optional arg)
  "Jump to a project's directory using completion.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache arg)
  (let ((dir (projectile-complete-dir)))
    (dired (expand-file-name dir (projectile-project-root)))
    (run-hooks 'projectile-find-dir-hook)))

;;;###autoload
(defun projectile-find-dir-other-window (&optional arg)
  "Jump to a project's directory in other window using completion.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (when arg
    (projectile-invalidate-cache nil))
  (let ((dir (projectile-complete-dir)))
    (dired-other-window (expand-file-name dir (projectile-project-root)))
    (run-hooks 'projectile-find-dir-hook)))

(defun projectile-complete-dir ()
  (projectile-completing-read
   "Find dir: "
   (if projectile-find-dir-includes-top-level
       (append '("./") (projectile-current-project-dirs))
     (projectile-current-project-dirs))))

;;;###autoload
(defun projectile-find-test-file (&optional arg)
  "Jump to a project's test file using completion.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache arg)
  (let ((file (projectile-completing-read "Find test file: "
                                          (projectile-current-project-test-files))))
    (find-file (expand-file-name file (projectile-project-root)))))

(defun projectile-test-files (files)
  "Return only the test FILES."
  (-filter 'projectile-test-file-p files))

(defun projectile-test-file-p (file)
  "Check if FILE is a test file."
  (or (--any? (string-prefix-p it (file-name-nondirectory file))
              (-non-nil (list (funcall projectile-test-prefix-function (projectile-project-type)))))
      (--any? (string-suffix-p it (file-name-sans-extension (file-name-nondirectory file)))
              (-non-nil (list (funcall projectile-test-suffix-function (projectile-project-type)))))))

(defun projectile-current-project-test-files ()
  "Return a list of test files for the current project."
  (projectile-test-files (projectile-current-project-files)))

(defvar projectile-project-types (make-hash-table)
  "A hash table holding all project types that are known to Projectile.")

(defun projectile-register-project-type
    (project-type marker-files &optional compile-cmd test-cmd run-cmd)
  "Register a project type with projectile.

A project type is defined by PROJECT-TYPE, a set of MARKER-FILES,
a COMPILE-CMD, a TEST-CMD, and a RUN-CMD."
  (puthash project-type (list 'marker-files marker-files
                              'compile-command compile-cmd
                              'test-command test-cmd
                              'run-command run-cmd)
           projectile-project-types))

(projectile-register-project-type 'emacs-cask '("Cask") "cask install")
(projectile-register-project-type 'rails-rspec '("Gemfile" "app" "lib" "db" "config" "spec") "bundle exec rails server" "bundle exec rspec")
(projectile-register-project-type 'rails-test '("Gemfile" "app" "lib" "db" "config" "test") "bundle exec rails server" "bundle exec rake test")
(projectile-register-project-type 'symfony '("composer.json" "app" "src" "vendor") "app/console server:run" "phpunit -c app ")
(projectile-register-project-type 'ruby-rspec '("Gemfile" "lib" "spec") "bundle exec rake" "bundle exec rspec")
(projectile-register-project-type 'ruby-test '("Gemfile" "lib" "test") "bundle exec rake" "bundle exec rake test")
(projectile-register-project-type 'django '("manage.py") "python manage.py runserver" "python manage.py test")
(projectile-register-project-type 'python-pip '("requirements.txt") "python setup.by build" "python -m unittest discover")
(projectile-register-project-type 'python-pkg '("setup.py") "python setup.py build" "python -m unittest discover")
(projectile-register-project-type 'python-tox '("tox.ini") nil "tox")
(projectile-register-project-type 'scons '("SConstruct") "scons" "scons test")
(projectile-register-project-type 'maven '("pom.xml") "mvn clean install" "mvn test")
(projectile-register-project-type 'gradle '("build.gradle") "gradle build" "gradle test")
(projectile-register-project-type 'gradlew '("gradlew") "./gradlew build" "./gradlew test")
(projectile-register-project-type 'grails '("application.properties" "grails-app") "grails package" "grails test-app")
(projectile-register-project-type 'lein-test '("project.clj") "lein compile" "lein test")
(projectile-register-project-type 'lein-midje '("project.clj" ".midje.clj") "lein compile" "lein midje")
(projectile-register-project-type 'boot-clj '("build.boot") "boot aot" "boot test")
(projectile-register-project-type 'rebar '("rebar.config") "rebar" "rebar eunit")
(projectile-register-project-type 'sbt '("build.sbt") "sbt compile" "sbt test")
(projectile-register-project-type 'make '("Makefile") "make" "make test")
(projectile-register-project-type 'grunt '("Gruntfile.js") "grunt" "grunt test")
(projectile-register-project-type 'gulp '("gulpfile.js") "gulp" "gulp test")
(projectile-register-project-type 'haskell-stack '("stack.yaml") "stack build" "stack build --test")
(projectile-register-project-type 'haskell-cabal #'projectile-cabal "cabal build" "cabal test")
(projectile-register-project-type 'rust-cargo '("Cargo.toml") "cargo build" "cargo test")
(projectile-register-project-type 'r '("DESCRIPTION") "R CMD INSTALL --with-keep.source ." (concat "R CMD check -o " temporary-file-directory " ."))
(projectile-register-project-type 'go #'projectile-go "go build ./..." "go test ./...")
(projectile-register-project-type 'racket '("info.rkt") nil "raco test .")

(defun projectile-cabal ()
  "Check if a project contains *.cabal files but no stack.yaml file."
  (and (projectile-verify-file "*.cabal")
       (not (projectile-verify-file "stack.yaml"))))

(defun projectile-go ()
  "Check if a project contains Go source files."
  (-any? (lambda (file)
           (string= (file-name-extension file) "go"))
         (projectile-current-project-files)))

(defcustom projectile-go-function 'projectile-go
  "Function to determine if project's type is go."
  :group 'projectile
  :type 'function)

(defvar-local projectile-project-type nil
  "Buffer local var for overriding the auto-detected project type.
Normally you'd set this from .dir-locals.el.")

(defun projectile-detect-project-type ()
  "Detect the type of the current project."
  (let ((project-type (-first (lambda (project-type)
                                (let ((marker (plist-get (gethash project-type projectile-project-types) 'marker-files)))
                                  (if (listp marker)
                                      (and (projectile-verify-files marker) project-type)
                                    (and (funcall marker) project-type))))
                              (projectile-hash-keys projectile-project-types))))
    (when project-type
      (puthash (projectile-project-root) project-type projectile-project-type-cache))
    project-type))

(defun projectile-project-type ()
  "Determine the project's type based on its structure."
  (if projectile-project-type
      projectile-project-type
    (or (gethash (projectile-project-root) projectile-project-type-cache)
        (projectile-detect-project-type)
        'generic)))

;;;###autoload
(defun projectile-project-info ()
  "Display info for current project."
  (interactive)
  (message "Project dir: %s ## Project VCS: %s ## Project type: %s"
           (projectile-project-root)
           (projectile-project-vcs)
           (projectile-project-type)))

(defun projectile-verify-files (files)
  "Check whether all FILES exist in the current project."
  (-all? 'projectile-verify-file files))

(defun projectile-verify-file (file)
  "Check whether FILE exists in the current project.
Expands wildcards using `file-expand-wildcards' before checking."
  (file-expand-wildcards (projectile-expand-root file)))

(defun projectile-project-vcs (&optional project-root)
  "Determine the VCS used by the project if any.
PROJECT-ROOT is the targeted directory.  If nil, use
`projectile-project-root'."
  (or project-root (setq project-root (projectile-project-root)))
  (cond
   ((projectile-file-exists-p (expand-file-name ".git" project-root)) 'git)
   ((projectile-file-exists-p (expand-file-name ".hg" project-root)) 'hg)
   ((projectile-file-exists-p (expand-file-name ".fossil" project-root)) 'fossil)
   ((projectile-file-exists-p (expand-file-name ".bzr" project-root)) 'bzr)
   ((projectile-file-exists-p (expand-file-name "_darcs" project-root)) 'darcs)
   ((projectile-file-exists-p (expand-file-name ".svn" project-root)) 'svn)
   ((projectile-locate-dominating-file project-root ".git") 'git)
   ((projectile-locate-dominating-file project-root ".hg") 'hg)
   ((projectile-locate-dominating-file project-root ".fossil") 'fossil)
   ((projectile-locate-dominating-file project-root ".bzr") 'bzr)
   ((projectile-locate-dominating-file project-root "_darcs") 'darcs)
   ((projectile-locate-dominating-file project-root ".svn") 'svn)
   (t 'none)))

(defun projectile--test-name-for-impl-name (impl-file-path)
  (let* ((project-type (projectile-project-type))
         (impl-file-name (file-name-sans-extension (file-name-nondirectory impl-file-path)))
         (impl-file-ext (file-name-extension impl-file-path))
         (test-prefix (funcall projectile-test-prefix-function project-type))
         (test-suffix (funcall projectile-test-suffix-function project-type)))
    (cond
     (test-prefix (concat test-prefix impl-file-name "." impl-file-ext))
     (test-suffix (concat impl-file-name test-suffix "." impl-file-ext))
     (t (error "Project type not supported!")))))

(defun projectile-create-test-file-for (impl-file-path)
  (let* ((test-file (projectile--test-name-for-impl-name impl-file-path))
         (test-dir (replace-regexp-in-string "src/" "test/" (file-name-directory impl-file-path))))
    (unless (file-exists-p (expand-file-name test-file test-dir))
      (progn (unless (file-exists-p test-dir)
               (make-directory test-dir :create-parents))
             (concat test-dir test-file)))))

(defcustom projectile-create-missing-test-files nil
  "During toggling, if non-nil enables creating test files if not found.

When not-nil, every call to projectile-find-implementation-or-test-*
creates test files if not found on the file system. Defaults to nil.
It assumes the test/ folder is at the same level as src/."
  :group 'projectile
  :type 'boolean)

(defun projectile-find-implementation-or-test (file-name)
  "Given a FILE-NAME return the matching implementation or test filename."
  (unless file-name (error "The current buffer is not visiting a file"))
  (if (projectile-test-file-p file-name)
      ;; find the matching impl file
      (let ((impl-file (projectile-find-matching-file file-name)))
        (if impl-file
            (projectile-expand-root impl-file)
          (error "No matching source file found")))
    ;; find the matching test file
    (let ((test-file (projectile-find-matching-test file-name)))
      (if test-file
          (projectile-expand-root test-file)
        (if projectile-create-missing-test-files
            (projectile-create-test-file-for file-name)
          (error "No matching test file found"))))))

;;;###autoload
(defun projectile-find-implementation-or-test-other-window ()
  "Open matching implementation or test file in other window."
  (interactive)
  (find-file-other-window
   (projectile-find-implementation-or-test (buffer-file-name))))

;;;###autoload
(defun projectile-toggle-between-implementation-and-test ()
  "Toggle between an implementation file and its test file."
  (interactive)
  (find-file
   (projectile-find-implementation-or-test (buffer-file-name))))

(defun projectile-test-prefix (project-type)
  "Find default test files prefix based on PROJECT-TYPE."
  (cond
   ((member project-type '(django python-pip python-pkg python-tox)) "test_")
   ((member project-type '(emacs-cask)) "test-")
   ((member project-type '(lein-midje)) "t_")))

(defun projectile-test-suffix (project-type)
  "Find default test files suffix based on PROJECT-TYPE."
  (cond
   ((member project-type '(rebar)) "_SUITE")
   ((member project-type '(emacs-cask)) "-test")
   ((member project-type '(rails-rspec ruby-rspec)) "_spec")
   ((member project-type '(rails-test ruby-test lein-test boot-clj go)) "_test")
   ((member project-type '(scons)) "test")
   ((member project-type '(maven symfony)) "Test")
   ((member project-type '(gradle gradlew grails)) "Spec")))

(defun projectile-dirname-matching-count (a b)
  "Count matching dirnames ascending file paths."
  (setq a (reverse (split-string (or (file-name-directory a) "") "/" t))
        b (reverse (split-string (or (file-name-directory b) "") "/" t)))
  (let ((common 0))
    (while (and a b (string-equal (pop a) (pop b)))
      (setq common (1+ common)))
    common))

(defun projectile-group-file-candidates (file candidates)
  "Group file candidates by dirname matching count."
  (--sort (> (car it) (car other))
          (--group-by (projectile-dirname-matching-count file it) candidates)))

(defun projectile-find-matching-test (file)
  "Compute the name of the test matching FILE."
  (let* ((basename (file-name-nondirectory (file-name-sans-extension file)))
         (test-prefix (funcall projectile-test-prefix-function (projectile-project-type)))
         (test-suffix (funcall projectile-test-suffix-function (projectile-project-type)))
         (candidates
          (-filter (lambda (current-file)
                     (let ((name (file-name-nondirectory
                                  (file-name-sans-extension current-file))))
                       (or (when test-prefix
                             (string-equal name (concat test-prefix basename)))
                           (when test-suffix
                             (string-equal name (concat basename test-suffix))))))
                   (projectile-current-project-files))))
    (cond
     ((null candidates) nil)
     ((= (length candidates) 1) (car candidates))
     (t (let ((grouped-candidates (projectile-group-file-candidates file candidates)))
          (if (= (length (car grouped-candidates)) 2)
              (-last-item (car grouped-candidates))
            (projectile-completing-read "Switch to: " (--mapcat (cdr it) grouped-candidates))))))))

(defun projectile-find-matching-file (test-file)
  "Compute the name of a file matching TEST-FILE."
  (let* ((basename (file-name-nondirectory (file-name-sans-extension test-file)))
         (test-prefix (funcall projectile-test-prefix-function (projectile-project-type)))
         (test-suffix (funcall projectile-test-suffix-function (projectile-project-type)))
         (candidates
          (-filter (lambda (current-file)
                     (let ((name (file-name-nondirectory
                                  (file-name-sans-extension current-file))))
                       (or (when test-prefix
                             (string-equal (concat test-prefix name) basename))
                           (when test-suffix
                             (string-equal (concat name test-suffix) basename)))))
                   (projectile-current-project-files))))
    (cond
     ((null candidates) nil)
     ((= (length candidates) 1) (car candidates))
     (t (let ((grouped-candidates (projectile-group-file-candidates test-file candidates)))
          (if (= (length (car grouped-candidates)) 2)
              (-last-item (car grouped-candidates))
            (projectile-completing-read "Switch to: " (--mapcat (cdr it) grouped-candidates))))))))

(defun projectile-grep-default-files ()
  "Try to find a default pattern for `projectile-grep'.
This is a subset of `grep-read-files', where either a matching entry from
`grep-files-aliases' or file name extension pattern is returned."
  (when buffer-file-name
    (let* ((fn (file-name-nondirectory buffer-file-name))
           (default-alias
             (let ((aliases (remove (assoc "all" grep-files-aliases)
                                    grep-files-aliases))
                   alias)
               (while aliases
                 (setq alias (car aliases)
                       aliases (cdr aliases))
                 (if (string-match (mapconcat
                                    #'wildcard-to-regexp
                                    (split-string (cdr alias) nil t)
                                    "\\|")
                                   fn)
                     (setq aliases nil)
                   (setq alias nil)))
               (cdr alias)))
           (default-extension
             (let ((ext (file-name-extension fn)))
               (and ext (concat "*." ext)))))
      (or default-alias default-extension))))

(defun projectile--globally-ignored-file-suffixes-glob ()
  "Return ignored file suffixes as a list of glob patterns."
  (--map (concat "*" it) projectile-globally-ignored-file-suffixes))

;;;###autoload
(defun projectile-grep (&optional regexp arg)
  "Perform rgrep in the project.

With a prefix ARG asks for files (globbing-aware) which to grep in.
With prefix ARG of `-' (such as `M--'), default the files (without prompt),
to `projectile-grep-default-files'.

With REGEXP given, don't query the user for a regexp."
  (interactive "i\nP")
  (require 'grep) ;; for `rgrep'
  (let* ((roots (projectile-get-project-directories))
         (search-regexp (or regexp
                            (read-string (projectile-prepend-project-name "Grep for: ")
                                         (projectile-symbol-or-selection-at-point))))
         (files (and arg (or (and (equal current-prefix-arg '-)
                                  (projectile-grep-default-files))
                             (read-string (projectile-prepend-project-name "Grep in: ")
                                          (projectile-grep-default-files))))))
    (dolist (root-dir roots)
      (require 'vc-git) ;; for `vc-git-grep'
      ;; in git projects users have the option to use `vc-git-grep' instead of `rgrep'
      (if (and (eq (projectile-project-vcs) 'git)
               projectile-use-git-grep
               (fboundp 'vc-git-grep))
          (vc-git-grep search-regexp (or files "") root-dir)
        ;; paths for find-grep should relative and without trailing /
        (let ((grep-find-ignored-directories
               (-union (--map (directory-file-name (file-relative-name it root-dir))
                              (projectile-ignored-directories))
                       grep-find-ignored-directories))
              (grep-find-ignored-files
               (-union (append (-map (lambda (file)
                                       (file-relative-name file root-dir))
                                     (projectile-ignored-files))
                               (projectile--globally-ignored-file-suffixes-glob))
                       grep-find-ignored-files)))
          (grep-compute-defaults)
          (rgrep search-regexp (or files "* .*") root-dir))))
    (run-hooks 'projectile-grep-finished-hook)))

;;;###autoload
(defun projectile-ag (search-term &optional arg)
  "Run an ag search with SEARCH-TERM in the project.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression."
  (interactive
   (list (read-from-minibuffer
          (projectile-prepend-project-name (format "Ag %ssearch for: " (if current-prefix-arg "regexp " "")))
          (projectile-symbol-or-selection-at-point))
         current-prefix-arg))
  (if (require 'ag nil 'noerror)
      (let ((ag-command (if arg 'ag-regexp 'ag))
            (ag-ignore-list (unless (eq (projectile-project-vcs) 'git)
                              ;; ag supports git ignore files
                              (-union ag-ignore-list
                                      (append
                                       (projectile-ignored-files-rel) (projectile-ignored-directories-rel)
                                       (projectile--globally-ignored-file-suffixes-glob)
                                       grep-find-ignored-files grep-find-ignored-directories))))
            ;; reset the prefix arg, otherwise it will affect the ag-command
            (current-prefix-arg nil))
        (funcall ag-command search-term (projectile-project-root)))
    (error "Package 'ag' is not available")))

(defun projectile-tags-exclude-patterns ()
  "Return a string with exclude patterns for ctags."
  (mapconcat (lambda (pattern) (format "--exclude=\"%s\""
                                       (directory-file-name pattern)))
             (projectile-ignored-directories-rel) " "))

;;;###autoload
(defun projectile-regenerate-tags ()
  "Regenerate the project's [e|g]tags."
  (interactive)
  (if (boundp 'ggtags-mode)
      (progn
        (let* ((ggtags-project-root (projectile-project-root))
               (default-directory ggtags-project-root))
          (ggtags-ensure-project)
          (ggtags-update-tags t)))
    (let* ((project-root (projectile-project-root))
           (tags-exclude (projectile-tags-exclude-patterns))
           (default-directory project-root)
           (tags-file (expand-file-name projectile-tags-file-name))
           (command (format projectile-tags-command tags-file tags-exclude))
           shell-output exit-code)
      (with-temp-buffer
        (setq exit-code
              (call-process-shell-command command nil (current-buffer))
              shell-output (projectile-trim-string
                            (buffer-substring (point-min) (point-max)))))
      (unless (zerop exit-code)
        (error shell-output))
      (visit-tags-table tags-file))))

(defun projectile-visit-project-tags-table ()
  "Visit the current project's tags table."
  (when (projectile-project-p)
    (let ((tags-file (projectile-expand-root projectile-tags-file-name)))
      (when (file-exists-p tags-file)
        (with-demoted-errors "Error loading tags-file: %s"
          (visit-tags-table tags-file t))))))

;;;###autoload
(defun projectile-find-tag ()
  "Find tag in project."
  (interactive)
  (projectile-visit-project-tags-table)
  ;; Auto-discover the user's preference for tags
  (let ((find-tag-fn (cond
                      ((fboundp 'ggtags-find-tag-dwim)
                       'ggtags-find-tag-dwim)
                      ((fboundp 'etags-select-find-tag)
                       'etags-select-find-tag)
                      (t
                       'find-tag))))
    (call-interactively find-tag-fn)))

(defmacro projectile-with-default-dir (dir &rest body)
  "Invoke in DIR the BODY."
  (declare (debug t) (indent 1))
  `(let ((default-directory ,dir))
     ,@body))

;;;###autoload
(defun projectile-run-command-in-root ()
  "Invoke `execute-extended-command' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (call-interactively 'execute-extended-command)))

;;;###autoload
(defun projectile-run-shell-command-in-root ()
  "Invoke `shell-command' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (call-interactively 'shell-command)))

;;;###autoload
(defun projectile-run-async-shell-command-in-root ()
  "Invoke `async-shell-command' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (call-interactively 'async-shell-command)))

;;;###autoload
(defun projectile-run-shell ()
  "Invoke `shell' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (shell (concat "*shell " (projectile-project-name) "*"))))

;;;###autoload
(defun projectile-run-eshell ()
  "Invoke `eshell' in the project's root."
  (interactive)
  (let ((eshell-buffer-name (concat "*eshell " (projectile-project-name) "*")))
     (projectile-with-default-dir (projectile-project-root)
       (eshell))))

;;;###autoload
(defun projectile-run-term (program)
  "Invoke `term' in the project's root."
  (interactive (list nil))
  (let* ((term (concat "term " (projectile-project-name)))
         (buffer (concat "*" term "*")))
    (unless (get-buffer buffer)
      (require 'term)
      (let ((program (or program
                         (read-from-minibuffer "Run program: "
                                               (or explicit-shell-file-name
                                                   (getenv "ESHELL")
                                                   (getenv "SHELL")
                                                   "/bin/sh")))))
        (projectile-with-default-dir (projectile-project-root)
          (set-buffer (make-term term program))
          (term-mode)
          (term-char-mode))))
    (switch-to-buffer buffer)))

(defun projectile-files-in-project-directory (directory)
  "Return a list of files in DIRECTORY."
  (let ((dir (file-relative-name (expand-file-name directory)
                                 (projectile-project-root))))
    (--filter (string-prefix-p dir it)
              (projectile-current-project-files))))

(defun projectile-unixy-system-p ()
  "Check to see if unixy text utilities are installed."
  (--all? (executable-find it) '("grep" "cut" "uniq")))

(defun projectile-files-from-cmd (cmd directory)
  "Use a grep-like CMD to search for files within DIRECTORY.

CMD should include the necessary search params and should output
equivalently to grep -HlI (only unique matching filenames).
Returns a list of expanded filenames."
  (let ((default-directory directory))
    (--map (concat directory
                   (if (string-prefix-p "./" it) (substring it 2) it))
           (-> (shell-command-to-string cmd)
               projectile-trim-string
               (split-string "\n+" t)))))

(defun projectile-files-with-string (string directory)
  "Return a list of all files containing STRING in DIRECTORY.

Tries to use ag, ack, git-grep, and grep in that order.  If those
are impossible (for instance on Windows), returns a list of all
files in the project."
  (if (projectile-unixy-system-p)
      (let* ((search-term (shell-quote-argument string))
             (cmd (cond ((executable-find "ag")
                         (concat "ag --literal --nocolor --noheading -l -- "
                                 search-term))
                        ((executable-find "ack")
                         (concat "ack --literal --noheading --nocolor -l -- "
                                 search-term))
                        ((and (executable-find "git")
                              (eq (projectile-project-vcs) 'git))
                         (concat "git grep -HlI " search-term))
                        (t
                         ;; -r: recursive
                         ;; -H: show filename for each match
                         ;; -l: show only file names with matches
                         ;; -I: no binary files
                         (format "grep -rHlI %s ." search-term)))))
        (projectile-files-from-cmd cmd directory))
    ;; we have to reject directories as a workaround to work with git submodules
    (-reject #'file-directory-p
             (-map #'projectile-expand-root (projectile-dir-files directory)))))

;;;###autoload
(defun projectile-replace (&optional arg)
  "Replace literal string in project using non-regexp `tags-query-replace'.

With a prefix argument ARG prompts you for a directory on which
to run the replacement."
  (interactive "P")
  (let* ((old-text (read-string
                    (projectile-prepend-project-name "Replace: ")
                    (projectile-symbol-or-selection-at-point)))
         (new-text (read-string
                    (projectile-prepend-project-name
                     (format "Replace %s with: " old-text))))
         (directory (if arg
                        (file-name-as-directory
                         (read-directory-name "Replace in directory: "))
                      (projectile-project-root)))
         (files (projectile-files-with-string old-text directory)))
    ;; Adapted from `tags-query-replace' for literal strings (not regexp)
    (setq tags-loop-scan `(let ,(unless (equal old-text (downcase old-text))
                                  '((case-fold-search nil)))
                            (if (search-forward ',old-text nil t)
                                ;; When we find a match, move back to
                                ;; the beginning of it so
                                ;; perform-replace will see it.
                                (goto-char (match-beginning 0))))
          tags-loop-operate `(perform-replace ',old-text ',new-text t nil nil
                                              nil multi-query-replace-map))
    (tags-loop-continue (or (cons 'list files) t))))

;;;###autoload
(defun projectile-replace-regexp (&optional arg)
  "Replace a regexp in the project using `tags-query-replace'.

With a prefix argument ARG prompts you for a directory on which
to run the replacement."
  (interactive "P")
  (let* ((old-text (read-string
                    (projectile-prepend-project-name "Replace regexp: ")
                    (projectile-symbol-or-selection-at-point)))
         (new-text (read-string
                    (projectile-prepend-project-name
                     (format "Replace regexp %s with: " old-text))))
         (directory (if arg
                        (file-name-as-directory
                         (read-directory-name "Replace regexp in directory: "))
                      (projectile-project-root)))
         (files
          ;; We have to reject directories as a workaround to work with git submodules.
          ;;
          ;; We can't narrow the list of files with
          ;; `projectile-files-with-string' because those regexp tools
          ;; don't support Emacs regular expressions.
          (-reject #'file-directory-p
                   (-map #'projectile-expand-root (projectile-dir-files directory)))))
    (tags-query-replace old-text new-text nil (cons 'list files))))

(defun projectile-symbol-or-selection-at-point ()
  "Get the symbol or selected text at point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (projectile-symbol-at-point)))

(defun projectile-symbol-at-point ()
  "Get the symbol at point and strip its properties."
  (substring-no-properties (or (thing-at-point 'symbol) "")))

;;;###autoload
(defun projectile-kill-buffers ()
  "Kill all project buffers."
  (interactive)
  (let ((name (projectile-project-name))
        (buffers (projectile-project-buffers)))
    (if (yes-or-no-p
         (format "Are you sure you want to kill %d buffer(s) for '%s'? "
                 (length buffers) name))
        ;; we take care not to kill indirect buffers directly
        ;; as we might encounter them after their base buffers are killed
        (mapc #'kill-buffer (-remove 'buffer-base-buffer buffers)))))

;;;###autoload
(defun projectile-save-project-buffers ()
  "Save all project buffers."
  (interactive)
  (--each (projectile-project-buffers)
    (with-current-buffer it
      (when buffer-file-name
        (save-buffer)))))

;;;###autoload
(defun projectile-dired ()
  "Open `dired' at the root of the project."
  (interactive)
  (dired (projectile-project-root)))

;;;###autoload
(defun projectile-vc (&optional project-root)
  "Open `vc-dir' at the root of the project.

For git projects `magit-status-internal' is used if available.
For hg projects `monky-status' is used if available."
  (interactive)
  (or project-root (setq project-root (projectile-project-root)))
  (let ((vcs (projectile-project-vcs project-root)))
    (pcase vcs
      (`git
       (cond ((fboundp 'magit-status-internal)
              (magit-status-internal project-root))
             ((fboundp 'magit-status)
              (with-no-warnings (magit-status project-root)))
             (t
              (vc-dir project-root))))
      (`hg
       (if (fboundp 'monky-status)
           (monky-status project-root)
         (vc-dir project-root)))
      (_ (vc-dir project-root)))))

;;;###autoload
(defun projectile-recentf ()
  "Show a list of recently visited files in a project."
  (interactive)
  (if (boundp 'recentf-list)
      (find-file (projectile-expand-root (projectile-completing-read "Recently visited files: " (projectile-recentf-files))))
    (message "recentf is not enabled")))

(defun projectile-recentf-files ()
  "Return a list of recently visited files in a project."
  (and (boundp 'recentf-list)
       (let ((project-root (projectile-project-root)))
         (->> recentf-list
              (--filter (string-prefix-p project-root it))
              (--map (file-relative-name it project-root))))))

(defun projectile-serialize-cache ()
  "Serializes the memory cache to the hard drive."
  (projectile-serialize projectile-projects-cache projectile-cache-file))

(defvar projectile-compilation-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last compilation command used on them.")

(defvar projectile-test-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last test command used on them.")

(defvar projectile-run-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last run command used on them.")

(defvar projectile-project-compilation-cmd nil
  "The command to use with `projectile-compile-project'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el.")

(defvar projectile-project-compilation-dir nil
  "The directory to use with `projectile-compile-project'.
The directory path is relative to the project root.
Should be set via .dir-locals.el.")

(defvar projectile-project-test-cmd nil
  "The command to use with `projectile-test-project'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el.")

(defvar projectile-project-run-cmd nil
  "The command to use with `projectile-run-project'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el.")

(defun projectile-default-compilation-command (project-type)
  "Retrieve default compilation command for PROJECT-TYPE."
  (plist-get (gethash project-type projectile-project-types) 'compile-command))

(defun projectile-default-test-command (project-type)
  "Retrieve default test command for PROJECT-TYPE."
  (plist-get (gethash project-type projectile-project-types) 'test-command))

(defun projectile-default-run-command (project-type)
  "Retrieve default run command for PROJECT-TYPE."
  (plist-get (gethash project-type projectile-project-types) 'run-command))

(defun projectile-compilation-command (compile-dir)
  "Retrieve the compilation command for COMPILE-DIR."
  (or (gethash compile-dir projectile-compilation-cmd-map)
      projectile-project-compilation-cmd
      (projectile-default-compilation-command (projectile-project-type))))

(defun projectile-test-command (project)
  "Retrieve the test command for PROJECT."
  (or (gethash project projectile-test-cmd-map)
      projectile-project-test-cmd
      (projectile-default-test-command (projectile-project-type))))

(defun projectile-run-command (project)
  "Retrieve the run command for PROJECT."
  (or (gethash project projectile-run-cmd-map)
      projectile-project-run-cmd
      (projectile-default-run-command (projectile-project-type))))

(defun projectile-read-command (prompt command)
  "Adapted from `compilation-read-command'."
  (read-shell-command prompt command
                      (if (equal (car compile-history) command)
                          '(compile-history . 1)
                        'compile-history)))

(defun projectile-compilation-dir ()
  "Choose the directory to use for project compilation."
  (if projectile-project-compilation-dir
      (file-truename
       (concat (file-name-as-directory (projectile-project-root))
               (file-name-as-directory projectile-project-compilation-dir)))
    (projectile-project-root)))

(defun projectile-maybe-read-command (arg default-cmd prompt)
  "Prompt user for command unless DEFAULT-CMD is an Elisp function."
  (if (and (or (stringp default-cmd) (null default-cmd))
           (or compilation-read-command arg))
      (projectile-read-command prompt default-cmd)
    default-cmd))

(defun projectile-run-compilation (cmd)
  "Run external or Elisp compilation command CMD."
  (if (functionp cmd)
      (funcall cmd)
    (compile cmd)))

;;;###autoload
(defun projectile-compile-project (arg &optional dir)
  "Run project compilation command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let* ((project-root (projectile-project-root))
         (default-directory (or dir (projectile-compilation-dir)))
         (default-cmd (projectile-compilation-command default-directory))
         (compilation-cmd (projectile-maybe-read-command arg default-cmd "Compile command: ")))
    (puthash default-directory compilation-cmd projectile-compilation-cmd-map)
    (save-some-buffers (not compilation-ask-about-save)
                       (lambda ()
                         (projectile-project-buffer-p (current-buffer)
                                                      project-root)))
    (projectile-run-compilation compilation-cmd)))

(defadvice compilation-find-file (around projectile-compilation-find-file)
  "Try to find a buffer for FILENAME, if we cannot find it,
fallback to the original function."
  (let ((filename (ad-get-arg 1)))
    (ad-set-arg 1
                (or
                 (if (file-exists-p (expand-file-name filename))
                     filename)
                 ;; Try to find the filename using projectile
                 (and (projectile-project-p)
                      (let ((root (projectile-project-root))
                            (dirs (cons "" (projectile-current-project-dirs))))
                        (-when-let (full-filename (->> dirs
                                                       (--map (expand-file-name filename (expand-file-name it root)))
                                                       (-filter #'file-exists-p)
                                                       (-first-item)))
                          full-filename)))
                 ;; Fall back to the old argument
                 filename))
    ad-do-it))

;; TODO - factor this duplication out
;;;###autoload
(defun projectile-test-project (arg)
  "Run project test command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let* ((project-root (projectile-project-root))
         (default-cmd (projectile-test-command project-root))
         (test-cmd (projectile-maybe-read-command arg default-cmd "Test command: "))
         (default-directory project-root))
    (puthash project-root test-cmd projectile-test-cmd-map)
    (projectile-run-compilation test-cmd)))

;;;###autoload
(defun projectile-run-project (arg)
  "Run project run command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let* ((project-root (projectile-project-root))
         (default-cmd (projectile-run-command project-root))
         (run-cmd (projectile-maybe-read-command arg default-cmd "Run command: "))
         (default-directory project-root))
    (puthash project-root run-cmd projectile-run-cmd-map)
    (projectile-run-compilation run-cmd)))

(defun projectile-open-projects ()
  "Return a list of all open projects.
An open project is a project with any open buffers."
  (-distinct
   (-non-nil
    (-map (lambda (buffer)
            (with-current-buffer buffer
              (when (projectile-project-p)
                (abbreviate-file-name (projectile-project-root)))))
          (buffer-list)))))

(defun projectile--remove-current-project (projects)
  "Remove the current project (if any) from the list of PROJECTS."
  (if (projectile-project-p)
      (-difference projects
                   (list (abbreviate-file-name (projectile-project-root))))
    projects))

(defun projectile-relevant-known-projects ()
  "Return a list of known projects except the current one (if present)."
  (projectile--remove-current-project projectile-known-projects))

(defun projectile-relevant-open-projects ()
  "Return a list of open projects except the current one (if present)."
  (projectile--remove-current-project (projectile-open-projects)))

;;;###autoload
(defun projectile-switch-project (&optional arg)
  "Switch to a project we have visited before.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'"
  (interactive "P")
  (-if-let (projects (projectile-relevant-known-projects))
      (projectile-switch-project-by-name
       (projectile-completing-read "Switch to project: " projects)
       arg)
    (error "There are no known projects")))

;;;###autoload
(defun projectile-switch-open-project (&optional arg)
  "Switch to a project we have currently opened.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'"
  (interactive "P")
  (-if-let (projects (projectile-relevant-open-projects))
      (projectile-switch-project-by-name
       (projectile-completing-read "Switch to open project: " projects)
       arg)
    (error "There are no open projects")))

(defun projectile-switch-project-by-name (project-to-switch &optional arg)
  "Switch to project by project name PROJECT-TO-SWITCH.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'"
  (let* ((default-directory project-to-switch)
         (switch-project-action (if arg
                                    'projectile-commander
                                  projectile-switch-project-action)))
    (run-hooks 'projectile-before-switch-project-hook)
    (funcall switch-project-action)
    (run-hooks 'projectile-after-switch-project-hook)))

;;;###autoload
(defun projectile-find-file-in-directory (&optional directory)
  "Jump to a file in a (maybe regular) DIRECTORY.

This command will first prompt for the directory the file is in."
  (interactive "DFind file in directory: ")
  (let ((default-directory directory)
        (projectile-require-project-root nil))
    (if (projectile-project-p)
        ;; target directory is in a project
        (let ((file (projectile-completing-read "Find file: "
                                                (projectile-dir-files directory))))
          (find-file (expand-file-name file (projectile-project-root)))
          (run-hooks 'projectile-find-file-hook))
      ;; target directory is not in a project
      (projectile-find-file))))

(defun projectile-all-project-files ()
  "Get a list of all files in all projects."
  (-mapcat (lambda (project)
             (when (file-exists-p project)
               (let ((default-directory project))
                 (-map (lambda (file)
                         (expand-file-name file project))
                       (projectile-current-project-files)))))
           projectile-known-projects))

;;;###autoload
(defun projectile-find-file-in-known-projects ()
  "Jump to a file in any of the known projects."
  (interactive)
  (let ((projectile-require-project-root nil))
    (find-file (projectile-completing-read "Find file in projects: " (projectile-all-project-files)))))

(defcustom projectile-after-switch-project-hook nil
  "Hooks run right after project is switched."
  :group 'projectile
  :type 'hook)

(defcustom projectile-before-switch-project-hook nil
  "Hooks run when right before project is switched."
  :group 'projectile
  :type 'hook)

(defun projectile-keep-project-p (project)
  "Determine whether we should cleanup (remove) PROJECT or not.

It handles the case of remote projects as well.
See `projectile-cleanup-known-projects'."
  ;; Taken from from `recentf-keep-default-predicate'
  (cond
   ((file-remote-p project nil t) (file-readable-p project))
   ((file-remote-p project))
   ((file-readable-p project))))

;;;###autoload
(defun projectile-cleanup-known-projects ()
  "Remove known projects that don't exist anymore."
  (interactive)
  (projectile-merge-known-projects)
  (let* ((separated-projects
          (-separate #'projectile-keep-project-p projectile-known-projects))
         (projects-kept (car separated-projects))
         (projects-removed (cadr separated-projects)))
    (setq projectile-known-projects projects-kept)
    (projectile-merge-known-projects)
    (if projects-removed
        (message "Projects removed: %s"
                 (mapconcat #'identity projects-removed ", "))
      (message "No projects needed to be removed."))))

;;;###autoload
(defun projectile-clear-known-projects ()
  "Clear both `projectile-known-projects' and `projectile-known-projects-file'."
  (interactive)
  (setq projectile-known-projects nil)
  (projectile-save-known-projects))

;;;###autoload
(defun projectile-remove-known-project (&optional project)
  "Remove PROJECT from the list of known projects."
  (interactive (list (projectile-completing-read "Remove from known projects: "
                                                 projectile-known-projects)))
  (setq projectile-known-projects
        (--reject (string= project it) projectile-known-projects))
  (projectile-merge-known-projects)
  (when projectile-verbose
    (message "Project %s removed from the list of known projects." project)))

;;;###autoload
(defun projectile-remove-current-project-from-known-projects ()
  "Remove the current project from the list of known projects."
  (interactive)
  (projectile-remove-known-project (abbreviate-file-name (projectile-project-root))))

(defun projectile-ignored-projects ()
  "A list of projects that should not be save in `projectile-known-projects'."
  (-map #'file-truename projectile-ignored-projects))

(defun projectile-ignored-project-p (project-root)
  "Return t if PROJECT-ROOT should not be added to `projectile-known-projects'."
  (or (member project-root (projectile-ignored-projects))
      (and (functionp projectile-ignored-project-function)
           (funcall projectile-ignored-project-function project-root))))

(defun projectile-add-known-project (project-root)
  "Add PROJECT-ROOT to the list of known projects."
  (unless (projectile-ignored-project-p project-root)
    (setq projectile-known-projects
          (-distinct
           (cons (abbreviate-file-name project-root)
                 projectile-known-projects)))))

(defun projectile-load-known-projects ()
  "Load saved projects from `projectile-known-projects-file'.
Also set `projectile-known-projects'."
  (setq projectile-known-projects
        (projectile-unserialize projectile-known-projects-file))
  (setq projectile-known-projects-on-file
        (and (sequencep projectile-known-projects)
             (copy-sequence projectile-known-projects))))

;; load the known projects
(projectile-load-known-projects)

(defun projectile-save-known-projects ()
  "Save PROJECTILE-KNOWN-PROJECTS to PROJECTILE-KNOWN-PROJECTS-FILE."
  (projectile-serialize projectile-known-projects
                        projectile-known-projects-file)
  (setq projectile-known-projects-on-file
        (and (sequencep projectile-known-projects)
             (copy-sequence projectile-known-projects))))

(defun projectile-merge-known-projects ()
  "Merge any change from `projectile-known-projects-file' and save to disk.

This enables multiple Emacs processes to make changes without
overwriting each other's changes."
  (let* ((known-now projectile-known-projects)
         (known-on-last-sync projectile-known-projects-on-file)
         (known-on-file
          (projectile-unserialize projectile-known-projects-file))
         (removed-after-sync (-difference known-on-last-sync known-now))
         (removed-in-other-process
          (-difference known-on-last-sync known-on-file))
         (result (-distinct
                  (-difference
                   (-concat known-now known-on-file)
                   (-concat removed-after-sync removed-in-other-process)))))
    (setq projectile-known-projects result)
    (projectile-save-known-projects)))

(define-ibuffer-filter projectile-files
  "Show Ibuffer with all buffers in the current project."
  (:reader (read-directory-name "Project root: " (ignore-errors (projectile-project-root)))
           :description nil)
  (with-current-buffer buf
    (equal (file-name-as-directory (expand-file-name qualifier))
           (ignore-errors (projectile-project-root)))))

(defun projectile-ibuffer-by-project (project-root)
  "Open an IBuffer window showing all buffers in PROJECT-ROOT."
  (let ((project-name (funcall projectile-project-name-function project-root)))
    (ibuffer nil (format "*%s Buffers*" project-name)
             (list (cons 'projectile-files project-root)))))

;;;###autoload
(defun projectile-ibuffer (prefix)
  "Open an IBuffer window showing all buffers in the current project.

Let user choose another project when PREFIX is supplied."
  (interactive "p")
  (let ((project-root (if (= prefix 4)
                          (projectile-completing-read
                           "Project name: "
                           (projectile-relevant-known-projects))
                        (projectile-project-root))))

    (projectile-ibuffer-by-project project-root)))

;;;; projectile-commander

(defconst projectile-commander-help-buffer "*Commander Help*")

(defvar projectile-commander-methods nil
  "List of file-selection methods for the `projectile-commander' command.
Each element is a list (KEY DESCRIPTION FUNCTION).
DESCRIPTION is a one-line description of what the key selects.")

;;;###autoload
(defun projectile-commander ()
  "Execute a Projectile command with a single letter.
The user is prompted for a single character indicating the action to invoke.
The `?' character describes then
available actions.

See `def-projectile-commander-method' for defining new methods."
  (interactive)
  (-let* ((choices (-map #'car projectile-commander-methods))
          (prompt (concat "Commander [" choices "]: "))
          (ch (read-char-choice prompt choices))
          ((_ _ fn) (assq ch projectile-commander-methods)))
    (funcall fn)))

(defmacro def-projectile-commander-method (key description &rest body)
  "Define a new `projectile-commander' method.

KEY is the key the user will enter to choose this method.

DESCRIPTION is a one-line sentence describing how the method.

BODY is a series of forms which are evaluated when the find
is chosen."
  (let ((method `(lambda ()
                   ,@body)))
    `(setq projectile-commander-methods
           (--sort (< (car it) (car other))
                   (cons (list ,key ,description ,method)
                         (assq-delete-all ,key projectile-commander-methods))))))

(def-projectile-commander-method ?? "Commander help buffer."
  (ignore-errors (kill-buffer projectile-commander-help-buffer))
  (with-current-buffer (get-buffer-create projectile-commander-help-buffer)
    (insert "Projectile Commander Methods:\n\n")
    (--each projectile-commander-methods
      (-let [(key line _) it]
        (insert (format "%c:\t%s\n" key line))))
    (goto-char (point-min))
    (help-mode)
    (display-buffer (current-buffer) t))
  (projectile-commander))

(defun projectile-commander-bindings ()
  (def-projectile-commander-method ?A
    "Find ag on project."
    (call-interactively 'projectile-ag))

  (def-projectile-commander-method ?f
    "Find file in project."
    (projectile-find-file))

  (def-projectile-commander-method ?T
    "Find test file in project."
    (projectile-find-test-file))

  (def-projectile-commander-method ?b
    "Switch to project buffer."
    (projectile-switch-to-buffer))

  (def-projectile-commander-method ?d
    "Find directory in project."
    (projectile-find-dir))

  (def-projectile-commander-method ?D
    "Open project root in dired."
    (projectile-dired))

  (def-projectile-commander-method ?v
    "Open project root in vc-dir or magit."
    (projectile-vc))

  (def-projectile-commander-method ?r
    "Replace a string in the project."
    (projectile-replace))

  (def-projectile-commander-method ?R
    "Regenerate the project's [e|g]tags."
    (projectile-regenerate-tags))

  (def-projectile-commander-method ?g
    "Run grep on project."
    (projectile-grep))

  (def-projectile-commander-method ?s
    "Switch project."
    (projectile-switch-project))

  (def-projectile-commander-method ?o
    "Run multi-occur on project buffers."
    (projectile-multi-occur))

  (def-projectile-commander-method ?j
    "Find tag in project."
    (projectile-find-tag))

  (def-projectile-commander-method ?k
    "Kill all project buffers."
    (projectile-kill-buffers))

  (def-projectile-commander-method ?e
    "Find recently visited file in project."
    (projectile-recentf)))

(projectile-commander-bindings)

(defun projectile-read-variable ()
  "Prompt for a variable and return its name."
  (completing-read "Variable: "
                   obarray
                   '(lambda (v)
                      (and (boundp v) (not (keywordp v))))
                   t))

(define-skeleton projectile-skel-variable-cons
  "Insert a variable-name and a value in a cons-cell."
  "Value: "
  "("
  (projectile-read-variable)
  " . "
  str
  ")")

(define-skeleton projectile-skel-dir-locals
  "Insert a .dir-locals.el template."
  nil
  "((nil . ("
  ("" '(projectile-skel-variable-cons) \n)
  resume:
  ")))")

;;;###autoload
(defun projectile-edit-dir-locals ()
  "Edit or create a .dir-locals.el file of the project."
  (interactive)
  (let ((file (expand-file-name ".dir-locals.el" (projectile-project-root))))
    (find-file file)
    (when (not (file-exists-p file))
      (unwind-protect
          (projectile-skel-dir-locals)
        (save-buffer)))))

;;; Minor mode
(defvar projectile-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "4 a") #'projectile-find-other-file-other-window)
    (define-key map (kbd "4 b") #'projectile-switch-to-buffer-other-window)
    (define-key map (kbd "4 C-o") #'projectile-display-buffer)
    (define-key map (kbd "4 d") #'projectile-find-dir-other-window)
    (define-key map (kbd "4 f") #'projectile-find-file-other-window)
    (define-key map (kbd "4 g") #'projectile-find-file-dwim-other-window)
    (define-key map (kbd "4 t") #'projectile-find-implementation-or-test-other-window)
    (define-key map (kbd "!") #'projectile-run-shell-command-in-root)
    (define-key map (kbd "&") #'projectile-run-async-shell-command-in-root)
    (define-key map (kbd "a") #'projectile-find-other-file)
    (define-key map (kbd "b") #'projectile-switch-to-buffer)
    (define-key map (kbd "c") #'projectile-compile-project)
    (define-key map (kbd "d") #'projectile-find-dir)
    (define-key map (kbd "D") #'projectile-dired)
    (define-key map (kbd "e") #'projectile-recentf)
    (define-key map (kbd "E") #'projectile-edit-dir-locals)
    (define-key map (kbd "f") #'projectile-find-file)
    (define-key map (kbd "g") #'projectile-find-file-dwim)
    (define-key map (kbd "F") #'projectile-find-file-in-known-projects)
    (define-key map (kbd "i") #'projectile-invalidate-cache)
    (define-key map (kbd "I") #'projectile-ibuffer)
    (define-key map (kbd "j") #'projectile-find-tag)
    (define-key map (kbd "k") #'projectile-kill-buffers)
    (define-key map (kbd "l") #'projectile-find-file-in-directory)
    (define-key map (kbd "m") #'projectile-commander)
    (define-key map (kbd "o") #'projectile-multi-occur)
    (define-key map (kbd "p") #'projectile-switch-project)
    (define-key map (kbd "q") #'projectile-switch-open-project)
    (define-key map (kbd "P") #'projectile-test-project)
    (define-key map (kbd "r") #'projectile-replace)
    (define-key map (kbd "R") #'projectile-regenerate-tags)
    (define-key map (kbd "s g") #'projectile-grep)
    (define-key map (kbd "s s") #'projectile-ag)
    (define-key map (kbd "S") #'projectile-save-project-buffers)
    (define-key map (kbd "t") #'projectile-toggle-between-implementation-and-test)
    (define-key map (kbd "T") #'projectile-find-test-file)
    (define-key map (kbd "u") #'projectile-run-project)
    (define-key map (kbd "v") #'projectile-vc)
    (define-key map (kbd "x e") #'projectile-run-eshell)
    (define-key map (kbd "x t") #'projectile-run-term)
    (define-key map (kbd "x s") #'projectile-run-shell)
    (define-key map (kbd "z") #'projectile-cache-current-file)
    (define-key map (kbd "ESC") #'projectile-project-buffers-other-buffer)
    map)
  "Keymap for Projectile commands after `projectile-keymap-prefix'.")
(fset 'projectile-command-map projectile-command-map)

(defvar projectile-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map projectile-keymap-prefix 'projectile-command-map)
    map)
  "Keymap for Projectile mode.")

(easy-menu-change
 '("Tools") "Projectile"
 '(["Find file" projectile-find-file]
   ["Find file in known projects" projectile-find-file-in-known-projects]
   ["Find test file" projectile-find-test-file]
   ["Find directory" projectile-find-dir]
   ["Find file in directory" projectile-find-file-in-directory]
   ["Find other file" projectile-find-other-file]
   ["Switch to buffer" projectile-switch-to-buffer]
   ["Jump between implementation file and test file" projectile-toggle-between-implementation-and-test]
   ["Kill project buffers" projectile-kill-buffers]
   ["Recent files" projectile-recentf]
   ["Edit .dir-locals.el" projectile-edit-dir-locals]
   "--"
   ["Open project in dired" projectile-dired]
   ["Switch to project" projectile-switch-project]
   ["Switch to open project" projectile-switch-open-project]
   ["Search in project (grep)" projectile-grep]
   ["Search in project (ag)" projectile-ag]
   ["Replace in project" projectile-replace]
   ["Multi-occur in project" projectile-multi-occur]
   "--"
   ["Run shell" projectile-run-shell]
   ["Run eshell" projectile-run-eshell]
   ["Run term" projectile-run-term]
   "--"
   ["Cache current file" projectile-cache-current-file]
   ["Invalidate cache" projectile-invalidate-cache]
   ["Regenerate [e|g]tags" projectile-regenerate-tags]
   "--"
   ["Compile project" projectile-compile-project]
   ["Test project" projectile-test-project]
   ["Run project" projectile-run-project]
   "--"
   ["Project info" projectile-project-info]
   ["About" projectile-version])
 "Search Files (Grep)...")

(easy-menu-change '("Tools") "--" nil "Search Files (Grep)...")

;;;###autoload
(defcustom projectile-mode-line
  '(:eval (if (file-remote-p default-directory)
              " Projectile"
            (format " Projectile[%s]" (projectile-project-name))))
  "Mode line lighter for Projectile.

The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for
details about mode line templates.

Customize this variable to change how Projectile displays its
status in the mode line.  The default value displays the project
name.  Set this variable to nil to disable the mode line
entirely."
  :group 'projectile
  :type 'sexp
  :risky t
  :package-version '(projectile "0.12.0"))

;;;###autoload
(define-minor-mode projectile-mode
  "Minor mode to assist project management and navigation.

When called interactively, toggle `projectile-mode'.  With prefix
ARG, enable `projectile-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `projectile-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `projectile-mode'.
Otherwise behave as if called interactively.

\\{projectile-mode-map}"
  :lighter projectile-mode-line
  :keymap projectile-mode-map
  :group 'projectile
  :require 'projectile
  (cond
   (projectile-mode
    ;; initialize the projects cache if needed
    (unless projectile-projects-cache
      (setq projectile-projects-cache
            (or (projectile-unserialize projectile-cache-file)
                (make-hash-table :test 'equal))))
    (add-hook 'find-file-hook #'projectile-cache-files-find-file-hook t t)
    (add-hook 'find-file-hook #'projectile-cache-projects-find-file-hook t t)
    (add-hook 'projectile-find-dir-hook #'projectile-cache-projects-find-file-hook)
    (add-hook 'find-file-hook #'projectile-visit-project-tags-table t t)
    (add-hook 'dired-before-readin-hook #'projectile-cache-projects-find-file-hook t t)
    (ad-activate 'compilation-find-file)
    (ad-activate 'delete-file))
   (t
    (remove-hook 'find-file-hook #'projectile-cache-files-find-file-hook t)
    (remove-hook 'find-file-hook #'projectile-cache-projects-find-file-hook t)
    (remove-hook 'find-file-hook #'projectile-visit-project-tags-table t)
    (remove-hook 'dired-before-readin-hook #'projectile-cache-projects-find-file-hook t)
    (ad-deactivate 'compilation-find-file)
    (ad-deactivate 'delete-file))))

;;;###autoload
(define-globalized-minor-mode projectile-global-mode
  projectile-mode
  projectile-mode)

(provide 'projectile)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; projectile.el ends here
