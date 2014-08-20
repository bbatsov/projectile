;;; projectile.el --- Manage and navigate projects in Emacs easily -*- lexical-binding: t -*-

;; Copyright © 2011-2014 Bozhidar Batsov <bozhidar@batsov.com>

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/projectile
;; Keywords: project, convenience
;; Version: 0.11.0
;; Package-Requires: ((s "1.6.0") (dash "1.5.0") (pkg-info "0.4"))

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
(require 'pkg-info)       ; For `pkg-info-version-info'
(require 'ibuffer)
(require 'ibuf-ext)

(eval-when-compile
  (defvar ack-and-a-half-arguments)
  (defvar ggtags-completion-table))

;;;; Compatibility
(eval-and-compile
  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
      (declare (debug defvar) (doc-string 3))
      `(progn
         (defvar ,var ,val ,docstring)
         (make-variable-buffer-local ',var)))))


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
  :type 'symbol
  :options '(ido grizzl helm default))

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

(defcustom projectile-tags-command "ctags -Re -f %s %s"
  "The command Projectile's going to use to generate a TAGS file."
  :group 'projectile
  :type 'string)

(defcustom projectile-sort-order 'default
  "The sort order used for a project's files."
  :group 'projectile
  :type 'symbol
  :options '(default recentf recently-active access-time modification-time))

(defcustom projectile-buffers-filter-function nil
  "A function used to filter the buffers in `projectile-project-buffers'.

The function should accept and return a list of Emacs buffers.
Two example filter functions are shipped by default - `projectile-buffers-with-file'
and `projectile-buffers-with-file-or-process'."
  :group 'projectile
  :type 'symbol)

(defcustom projectile-project-root-files
  '("rebar.config"       ; Rebar project file
    "project.clj"        ; Leiningen project file
    "SConstruct"         ; Scons project file
    "pom.xml"            ; Maven project file
    "build.sbt"          ; SBT project file
    "build.gradle"       ; Gradle project file
    "Gemfile"            ; Bundler file
    "requirements.txt"   ; Pip file
    "package.json"       ; npm package file
    "gulpfile.js"        ; Gulp build file
    "Gruntfile.js"       ; Grunt project file
    "bower.json"         ; Bower project file
    "composer.json"      ; Composer project file
    "Cargo.toml"         ; Cargo project file
    "mix.exs"            ; Elixir mix project file
    )
  "A list of files considered to mark the root of a project."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-project-root-files-top-down-recurring
  '(".svn" ; Svn VCS root dir
    "CVS"  ; Csv VCS root dir
    )
  "A list of files considered to mark the root of a project.
This root files pattern stops at the parentmost match."
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
This root files pattern overrides discovery of any root files
pattern that would have found a project root in a subdirectory."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-project-root-files-functions
  '(projectile-root-bottom-up
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
    "build")
  "A list of directories globally ignored by projectile."
  :group 'projectile
  :type '(repeat string))

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

(defcustom projectile-remember-window-configs nil
  "If true, restore the last window configuration when switching projects.
If no configuration exists, just run `projectile-switch-project-action' as usual."
  :group 'projectile
  :type 'boolean)

;;; Idle Timer
(defvar projectile-idle-timer nil
  "The timer object created when `project-enable-idle-timer' is non-nil.")

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

(defcustom projectile-ignored-projects nil
  "A list of projects not to be added to `projectile-known-projects'."
  :group 'projectile
  :type 'list
  :package-version '(projectile . "0.11.0"))


;;; Version information

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
  (let ((version (pkg-info-version-info 'projectile)))
    (when show-version
      (message "Projectile version: %s" version))
    version))


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
                   (remhash key  projectile-file-exists-cache)))
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
          (message "%s removed from cache" file))
      (error "%s is not in the cache" file))))

(defun projectile-purge-dir-from-cache (dir)
  "Purge DIR from the cache of the current project."
  (interactive
   (list (projectile-completing-read
          "Remove directory from cache: "
          (projectile-current-project-dirs))))
  (let* ((project-root (projectile-project-root))
         (project-cache (gethash project-root projectile-projects-cache)))
    (puthash project-root
             (-filter (lambda (file)
                        (s-starts-with-p dir file))
                      project-cache)
             projectile-projects-cache)))

(defun projectile-file-cached-p (file project)
  "Check if FILE is already in PROJECT cache."
  (member file (gethash project projectile-projects-cache)))

(defun projectile-cache-current-file ()
  "Add the currently visited file to the cache."
  (interactive)
  (let* ((current-project (projectile-project-root))
         (abs-current-file (buffer-file-name (current-buffer)))
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
               (propertize current-project 'face 'font-lock-keyword-face)))))

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

(defun projectile-maybe-invalidate-cache (force)
  "Invalidate if FORCE or project's dirconfig newer than cache."
  (when (or force (file-newer-than-file-p (projectile-dirconfig-file)
                                          projectile-cache-file))
    (projectile-invalidate-cache nil)))


;;; Window configurations
(defvar projectile-window-config-map
  (make-hash-table :test 'equal)
  "A mapping from project names to their latest window configurations.")

(defun projectile-save-window-config (project-name)
  "Save PROJECT-NAME's configuration to `projectile-window-config-map'."
  (puthash project-name (current-window-configuration) projectile-window-config-map))

(defun projectile-get-window-config (project-name)
  "Return the window configuration corresponding to PROJECT-NAME.
If no such window configuration exists,
returns nil."
  (gethash project-name projectile-window-config-map))

(defun projectile-restore-window-config (project-name)
  "Restore the window configuration corresponding to the PROJECT-NAME.
Returns nil if no window configuration was found"
  (let ((window-config (projectile-get-window-config project-name)))
    (when window-config
      (set-window-configuration window-config))))

(defadvice projectile-switch-project (before projectile-save-window-configuration-before-switching-projects activate)
  "Save the current project's window configuration before switching projects."
  (when (and projectile-remember-window-configs
             (projectile-project-p))
    (projectile-save-window-config (projectile-project-name))))

(defadvice projectile-kill-buffers (before projectile-remove-window-configuration-before-kill-buffers activate)
  "Remove's this project's window configuration from the table before killing buffers."
  (remhash (projectile-project-name) projectile-window-config-map))

(defadvice projectile-kill-buffers (after projectile-restore-window-configuration-after-kill-buffers activate)
  "Restore previous (if any) project's window configuration after killing a project's buffers."
  (when (and projectile-remember-window-configs
             (projectile-project-p))
    (projectile-restore-window-config (projectile-project-name))))


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
    (if root (file-name-as-directory root))))

(defun projectile-root-bottom-up (dir &optional list)
  "Identify a project root in DIR by looking at `projectile-project-root-files-bottom-up'.
Returns a project root directory path or nil if not found."
  (--reduce-from
   (or acc
       (projectile-locate-dominating-file dir it))
   nil
   (or list projectile-project-root-files-bottom-up (list))))

(defun projectile-root-top-down (dir &optional list)
  "Identify a project root in DIR by looking at `projectile-project-root-files-top-down'.
Returns a project root directory path or nil if not found."
  (projectile-locate-dominating-file
   dir
   (lambda (dir)
     (--first (projectile-file-exists-p (expand-file-name it dir))
              (or list projectile-project-root-files (list))))))

(defun projectile-root-top-down-recurring (dir &optional list)
  "Identify a project root in DIR by looking at `projectile-project-root-files-top-down-recurring'.
Returns a project root directory path or nil if not found."
  (--reduce-from
   (or acc
       (projectile-locate-dominating-file
        dir
        (lambda (dir)
          (and (projectile-file-exists-p (expand-file-name it dir))
               (or (string-match locate-dominating-stop-dir-regexp (projectile-parent dir))
                   (not (projectile-file-exists-p (expand-file-name it (projectile-parent dir)))))))))
   nil
   (or list projectile-project-root-files-top-down-recurring (list))))

(defun projectile-project-root ()
  "Retrieves the root directory of a project if available.
The current directory is assumed to be the project's root otherwise."
  (file-truename
   (let ((dir (file-truename default-directory)))
     (or (--reduce-from
          (or acc (funcall it dir)) nil
          projectile-project-root-files-functions)
         (if projectile-require-project-root
             (error "You're not in a project")
           default-directory)))))

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
  (-map (lambda (file) (file-relative-name file root))
        (projectile-index-directory directory (projectile-patterns-to-ignore))))

(defun projectile-dir-files-external (root directory)
  "Get the files for ROOT under DIRECTORY using external tools."
  (let ((default-directory directory)
        (files-list nil))
    (setq files-list (-map (lambda (f)
                             (file-relative-name (expand-file-name f directory) root))
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
    (and (not (s-starts-with? " " (buffer-name buffer)))
         (not (projectile-ignored-buffer-p buffer))
         (s-equals? (file-remote-p default-directory) (file-remote-p project-root))
         (s-starts-with? project-root (file-truename default-directory)))))

(defun projectile-ignored-buffer-p (buffer)
  "Check if BUFFER should be ignored."
  (or
   (member (buffer-name buffer) projectile-globally-ignored-buffers)
   (with-current-buffer buffer
     (--any-p (s-matches? (concat "^" it "$")
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

(defun projectile-switch-to-buffer-other-window ()
  "Switch to a project buffer and show it in another window."
  (interactive)
  (switch-to-buffer-other-window
   (projectile-completing-read
    "Switch to buffer: "
    (projectile-project-buffer-names))))

(defun projectile-display-buffer ()
  "Display a project buffer in another window without selecting it."
  (interactive)
  (display-buffer
   (projectile-completing-read
    "Display buffer: "
    (projectile-project-buffer-names))))

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
  (let ((project-root (projectile-project-root)))
    (--map (file-relative-name it project-root) (projectile-ignored-directories))))

(defun projectile-ignored-files-rel ()
  "Return list of ignored files, relative to the root."
  (let ((project-root (projectile-project-root)))
    (--map (file-relative-name it project-root) (projectile-ignored-files))))

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
    (-flatten (-map
               (lambda (pattern)
                 (or (file-expand-wildcards pattern t)
                     (projectile-expand-root pattern)))
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
    (when (projectile-file-exists-p dirconfig-file)
      (with-temp-buffer
        (insert-file-contents-literally dirconfig-file)
        (let* ((split-string-default-separators "[\r\n]")
               (strings (-map 's-trim (delete "" (split-string (buffer-string)))))
               (separated-vals (--separate (s-starts-with? "+" it) strings)))
          (cons (-map (lambda (dir) (projectile-ensure-trailing-slash
                                     (projectile-strip-dir-prefix dir))) (car separated-vals))
                (-map 'projectile-strip-dir-prefix (cadr separated-vals))))))))

(defun projectile-ensure-trailing-slash (dir)
  "Append / to DIR if missing."
  (if (s-ends-with? "/" dir) dir
    (concat dir "/")))

(defun projectile-strip-dir-prefix (dir)
  "Strip + or - prefix from DIR."
  (s-chop-prefixes '("-" "+") dir))

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
    (projectile-sort-files files)))

(defun projectile-process-current-project-files (action)
  "Process the current project's files using ACTION."
  (let ((project-files (projectile-current-project-files))
        default-directory (projectile-project-root))
    (dolist (filename project-files)
     (funcall action filename))))

(defun projectile-current-project-dirs ()
  "Return a list of dirs for the current project."
  (-remove 'null (-distinct
                  (-map 'file-name-directory
                        (projectile-current-project-files)))))

(defun projectile-hash-keys (hash)
  "Return a list of all HASH keys."
  (let (allkeys)
    (maphash (lambda (k _v) (setq allkeys (cons k allkeys))) hash)
    allkeys))


;;; Interactive commands
(defun projectile-find-file (&optional arg)
  "Jump to a project's file using completion.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache arg)
  (let ((file (projectile-completing-read "Find file: "
                                          (projectile-current-project-files))))
    (find-file (expand-file-name file (projectile-project-root)))
    (run-hooks 'projectile-find-file-hook)))

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

(defun projectile-find-dir (&optional arg)
  "Jump to a project's directory using completion.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache arg)
  (let ((dir (projectile-complete-dir)))
    (dired (expand-file-name dir (projectile-project-root)))
    (run-hooks 'projectile-find-dir-hook)))

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

(defun projectile-find-test-file (&optional arg)
  "Jump to a project's test file using completion.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache arg)
  (let ((file (projectile-completing-read "Find test file: "
                                          (projectile-current-project-test-files))))
    (find-file (expand-file-name file (projectile-project-root)))))

(defcustom projectile-test-files-prefixes '("test_")
  "Some common prefixes of test files."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-test-files-suffices '("_test" "_spec" "Spec" "Test" "-test")
  "Some common suffices of test files."
  :group 'projectile
  :type '(repeat string))

(defun projectile-test-files (files)
  "Return only the test FILES."
  (-filter 'projectile-test-file-p files))

(defun projectile-test-file-p (file)
  "Check if FILE is a test file."
  (or (-any? (lambda (prefix)
               (s-starts-with? prefix (file-name-nondirectory file)))
             projectile-test-files-prefixes)
      (-any? (lambda (suffix)
               (s-ends-with? suffix (file-name-sans-extension file)))
             projectile-test-files-suffices)))

(defun projectile-current-project-test-files ()
  "Return a list of test files for the current project."
  (projectile-test-files (projectile-current-project-files)))

(defvar projectile-rails-rspec '("Gemfile" "app" "lib" "db" "config" "spec"))
(defvar projectile-rails-test '("Gemfile" "app" "lib" "db" "config" "test"))
(defvar projectile-symfony '("composer.json" "app" "src" "vendor"))
(defvar projectile-ruby-rspec '("Gemfile" "lib" "spec"))
(defvar projectile-ruby-test '("Gemfile" "lib" "test"))
(defvar projectile-django '("manage.py"))
(defvar projectile-python-pip '("requirements.txt"))
(defvar projectile-python-egg '("setup.py"))
(defvar projectile-scons '("SConstruct"))
(defvar projectile-maven '("pom.xml"))
(defvar projectile-gradle '("build.gradle"))
(defvar projectile-grails '("application.properties" "grails-app"))
(defvar projectile-lein '("project.clj"))
(defvar projectile-rebar '("rebar"))
(defvar projectile-sbt '("build.sbt"))
(defvar projectile-make '("Makefile"))
(defvar projectile-grunt '("Gruntfile.js"))
(defvar projectile-gulp '("gulpfile.js"))

(defun projectile-go ()
  (-any? (lambda (file)
           (string= (file-name-extension file) "go")) (projectile-current-project-files)))

(defcustom projectile-go-function 'projectile-go
  "Function to determine if project's type is go."
  :group 'projectile
  :type 'function)

(defun projectile-project-type ()
  "Determine the project's type based on its structure."
  (cond
   ((projectile-verify-files projectile-rails-rspec) 'rails-rspec)
   ((projectile-verify-files projectile-rails-test) 'rails-test)
   ((projectile-verify-files projectile-ruby-rspec) 'ruby-rspec)
   ((projectile-verify-files projectile-ruby-test) 'ruby-test)
   ((projectile-verify-files projectile-django) 'django)
   ((projectile-verify-files projectile-python-pip)'python)
   ((projectile-verify-files projectile-python-egg) 'python)
   ((projectile-verify-files projectile-symfony) 'symfony)
   ((projectile-verify-files projectile-lein) 'lein)
   ((projectile-verify-files projectile-scons) 'scons)
   ((projectile-verify-files projectile-maven) 'maven)
   ((projectile-verify-files projectile-gradle) 'gradle)
   ((projectile-verify-files projectile-grails) 'grails)
   ((projectile-verify-files projectile-rebar) 'rebar)
   ((projectile-verify-files projectile-sbt) 'sbt)
   ((projectile-verify-files projectile-make) 'make)
   ((projectile-verify-files projectile-gulp) 'gulp)
   ((projectile-verify-files projectile-grunt) 'grunt)
   ((funcall projectile-go-function) 'go)
   (t 'generic)))

(defun projectile-verify-files (files)
  "Check whether all FILES exist in the current project."
  (-all? 'projectile-verify-file files))

(defun projectile-verify-file (file)
  "Check whether FILE exists in the current project."
  (projectile-file-exists-p (projectile-expand-root file)))

(defun projectile-project-vcs ()
  "Determine the VCS used by the project if any."
  (let ((project-root (projectile-project-root)))
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
     (t 'none))))

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
        (error "No matching test file found")))))

(defun projectile-find-implementation-or-test-other-window ()
  "Open matching implementation or test file in other window."
  (interactive)
  (find-file-other-window
   (projectile-find-implementation-or-test (buffer-file-name))))

(defun projectile-toggle-between-implementation-and-test ()
  "Toggle between an implementation file and its test file."
  (interactive)
  (find-file
   (projectile-find-implementation-or-test (buffer-file-name))))

(defun projectile-test-affix (project-type)
  "Find test files affix based on PROJECT-TYPE."
  (or (funcall projectile-test-prefix-function project-type)
      (funcall projectile-test-suffix-function project-type)
      (error "Project type not supported!")))

(defcustom projectile-test-prefix-function 'projectile-test-prefix
  "Function to find test files prefix based on PROJECT-TYPE."
  :group 'projectile
  :type 'function)

(defcustom projectile-test-suffix-function 'projectile-test-suffix
  "Function to find test files suffix based on PROJECT-TYPE."
  :group 'projectile
  :type 'function)

(defun projectile-test-prefix (project-type)
  "Find default test files prefix based on PROJECT-TYPE."
  (cond
   ((member project-type '(django python)) "test_")))

(defun projectile-test-suffix (project-type)
  "Find default test files suffix based on PROJECT-TYPE."
  (cond
   ((member project-type '(rails-rspec ruby-rspec)) "_spec")
   ((member project-type '(rails-test ruby-test lein go)) "_test")
   ((member project-type '(scons)) "test")
   ((member project-type '(maven symfony)) "Test")
   ((member project-type '(gradle grails)) "Spec")))

(defun projectile-find-matching-test (file)
  "Compute the name of the test matching FILE."
  (let ((basename (file-name-nondirectory (file-name-sans-extension file)))
        (test-affix (projectile-test-affix (projectile-project-type))))
    (-first (lambda (current-file)
              (let ((current-file-basename (file-name-nondirectory (file-name-sans-extension current-file))))
                (or (s-equals? current-file-basename (concat test-affix basename))
                    (s-equals? current-file-basename (concat basename test-affix)))))
            (projectile-current-project-files))))

(defun projectile-find-matching-file (test-file)
  "Compute the name of a file matching TEST-FILE."
  (let ((basename (file-name-nondirectory (file-name-sans-extension test-file)))
        (test-affix (projectile-test-affix (projectile-project-type))))
    (-first (lambda (current-file)
              (let ((current-file-basename (file-name-nondirectory (file-name-sans-extension current-file))))
                (or (s-equals? (concat test-affix current-file-basename) basename)
                    (s-equals? (concat current-file-basename test-affix) basename))))
            (projectile-current-project-files))))

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
                                    'wildcard-to-regexp
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

(defun projectile-grep (&optional arg)
  "Perform rgrep in the project.

With a prefix ARG asks for files (globbing-aware) which to grep in.
With prefix ARG of `-' (such as `M--'), default the files (without prompt),
to `projectile-grep-default-files'."
  (interactive "P")
  (let* ((roots (projectile-get-project-directories))
         (search-regexp (if (and transient-mark-mode mark-active)
                            (buffer-substring (region-beginning) (region-end))
                          (read-string (projectile-prepend-project-name "Grep for: ")
                                       (projectile-symbol-at-point))))
         (files (and arg (or (and (equal current-prefix-arg '-)
                                  (projectile-grep-default-files))
                             (read-string (projectile-prepend-project-name "Grep in: ")
                                          (projectile-grep-default-files))))))
    (dolist (root-dir roots)
      (require 'grep)
      ;; in git projects users have the option to use `vc-git-grep' instead of `rgrep'
      (if (and (eq (projectile-project-vcs) 'git) projectile-use-git-grep)
          (vc-git-grep search-regexp (or files "") root-dir)
        ;; paths for find-grep should relative and without trailing /
        (let ((grep-find-ignored-directories (-union (-map (lambda (dir) (s-chop-suffix "/" (file-relative-name dir root-dir)))
                                                           (cdr (projectile-ignored-directories))) grep-find-ignored-directories))
              (grep-find-ignored-files (-union (-map (lambda (file) (file-relative-name file root-dir)) (projectile-ignored-files)) grep-find-ignored-files)))
          (grep-compute-defaults)
          (rgrep search-regexp (or files "* .*") root-dir))))))

(defun projectile-ack (regexp &optional arg)
  "Run an ack search with REGEXP in the project.

With a prefix argument ARG prompts you for a directory on which the search is performed ."
  (interactive
   (list (read-from-minibuffer
          (projectile-prepend-project-name "Ack search for: ")
          (projectile-symbol-at-point))
         current-prefix-arg))
  (if (require 'ack-and-a-half nil 'noerror)
      (let* ((saved-arguments ack-and-a-half-arguments)
             (root (if arg
                       (expand-file-name (projectile-complete-dir) (projectile-project-root))
                     (projectile-project-root)))
             (ack-and-a-half-arguments
              (append saved-arguments
                      (-union (-map (lambda (path)
                                      (concat "--ignore-dir=" (file-name-nondirectory (directory-file-name path))))
                                    (projectile-ignored-directories))
                              (-map (lambda (path)
                                      (concat "--ignore-file=is:" (file-relative-name path root)))
                                    (projectile-ignored-files))))))
        (ack-and-a-half regexp t root))
    (error "ack-and-a-half not available")))

(defun projectile-ag (search-term &optional arg)
  "Run an ag search with SEARCH-TERM in the project.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression."
  (interactive
   (list (read-from-minibuffer
          (projectile-prepend-project-name (format "Ag %ssearch for: " (if current-prefix-arg "regexp " "")))
          (projectile-symbol-at-point))
         current-prefix-arg))
  (if (fboundp 'ag-regexp)
      (let ((ag-command (if arg 'ag-regexp 'ag))
            ;; reset the prefix arg, otherwise it will affect the ag-command
            (current-prefix-arg nil))
        (funcall ag-command search-term (projectile-project-root)))
    (error "Ag is not available")))

(defun projectile-tags-exclude-patterns ()
  "Return a string with exclude patterns for ctags."
  (mapconcat (lambda (pattern) (format "--exclude=%s"
                                       (directory-file-name pattern)))
             (projectile-ignored-directories-rel) " "))

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
              shell-output
              (s-trim (buffer-substring (point-min) (point-max)))))
      (unless (zerop exit-code)
        (error shell-output))
      (visit-tags-table tags-file))))

(defun projectile-visit-project-tags-table ()
  "Visit the current project's tags table."
  (when (projectile-project-p)
    (let ((tags-file (projectile-expand-root projectile-tags-file-name)))
      (when (file-exists-p tags-file)
        (with-demoted-errors
          "Error loading tags-file: %s"
          (visit-tags-table tags-file t))))))

(defun projectile-find-tag ()
  "Find tag in project."
  (interactive)
  (let ((find-tag-function (if (boundp 'ggtags-mode) 'ggtags-find-tag 'find-tag))
        (tags (if (boundp 'ggtags-mode)
                  (projectile--tags (all-completions "" ggtags-completion-table))
                ;; we have to manually reset the tags-completion-table every time
                (setq tags-completion-table nil)
                (tags-completion-table)
                (projectile--tags tags-completion-table))))
    (funcall find-tag-function (projectile-completing-read "Find tag: "
                                                            tags
                                                            (projectile-symbol-at-point)))))

(defun projectile--tags (completion-table)
  "Find tags using COMPLETION-TABLE."
  (-reject 'null
           (-map (lambda (x)
                   (unless (integerp x)
                     (prin1-to-string x t)))
                 completion-table)))

(defmacro projectile-with-default-dir (dir &rest body)
  "Invoke in DIR the BODY."
  (declare (debug t) (indent 1))
  `(let ((default-directory ,dir))
     ,@body))

(defun projectile-run-command-in-root ()
  "Invoke `execute-extended-command' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (call-interactively 'execute-extended-command)))

(defun projectile-run-shell-command-in-root ()
  "Invoke `shell-command' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (call-interactively 'shell-command)))

(defun projectile-run-async-shell-command-in-root ()
  "Invoke `async-shell-command' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (call-interactively 'async-shell-command)))

(defun projectile-files-in-project-directory (directory)
  "Return a list of files in DIRECTORY."
  (let ((dir (file-relative-name (expand-file-name directory) (projectile-project-root))))
    (-filter (lambda (file) (s-starts-with-p dir file))
             (projectile-current-project-files))))

(defun projectile-unixy-system-p ()
  "Check to see if unixy text utilities are installed."
  (--all? (executable-find it) '("grep" "cut" "uniq")))

(defun projectile-files-from-cmd (cmd directory)
  "Use a grep-like CMD to search for files within DIRECTORY.

CMD should include the necessary search params and should output
equivalently to grep -H (colon-deliminated, with the relative
filename as the first column).  Returns a list of expanded
filenames."
  (let ((default-directory directory))
    (->> (s-trim (shell-command-to-string
                  (concat cmd " | cut -d: -f1 | uniq")))
      (s-split "\n+")
      (-filter 's-present?)
      (--map (concat directory (s-chop-prefix "./" it))))))

(defun projectile-files-with-string (string directory)
  "Return a list of all files containing STRING in DIRECTORY.

Tries to use ag, ack, git-grep, and grep in that order.  If those
are impossible (for instance on Windows), returns a list of all
files in the project."
  (if (projectile-unixy-system-p)
      (let* ((search-term (shell-quote-argument string))
             (cmd (cond ((executable-find "ag")
                         (concat "ag --literal --nocolor --noheading -- "
                                 search-term))
                        ((executable-find "ack")
                         (concat "ack --noheading --nocolor -- "
                                 search-term))
                        ((and (executable-find "git")
                              (eq (projectile-project-vcs) 'git))
                         (concat "git grep -H "
                                 search-term))
                        (t
                         (concat "grep -rH "
                                 search-term
                                 " .")))))
        (projectile-files-from-cmd cmd directory))
    ;; we have to reject directories as a workaround to work with git submodules
    (-reject 'file-directory-p
             (-map 'projectile-expand-root (projectile-dir-files directory)))))

(defun projectile-replace (arg)
  "Replace a string in the project using `tags-query-replace'.

With a prefix argument ARG prompts you for a directory on which
to run the replacement."
  (interactive "P")
  (let* ((old-text (read-string
                    (projectile-prepend-project-name "Replace: ")
                    (projectile-symbol-at-point)))
         (new-text (read-string
                    (projectile-prepend-project-name
                     (format "Replace %s with: " old-text))))
         (directory (if arg
                        (read-directory-name "Replace in directory: ")
                      (projectile-project-root)))
         (files (projectile-files-with-string old-text directory)))
    (tags-query-replace old-text new-text nil (cons 'list files))))

(defun projectile-symbol-at-point ()
  "Get the symbol at point and strip its properties."
  (substring-no-properties (or (thing-at-point 'symbol) "")))

(defun projectile-kill-buffers ()
  "Kill all project buffers."
  (interactive)
  (let* ((buffers (projectile-project-buffers))
         (question
          (format
           "Are you sure you want to kill %d buffer(s) for '%s'? "
           (length buffers)
           (projectile-project-name))))
    (if (yes-or-no-p question)
        ;; we take care not to kill indirect buffers directly
        ;; as we might encounter them after their base buffers are killed
        (mapc 'kill-buffer (-remove 'buffer-base-buffer buffers)))))

(defun projectile-save-project-buffers ()
  "Save all project buffers."
  (interactive)
  (--each (projectile-project-buffers)
    (with-current-buffer it
      (when buffer-file-name
        (save-buffer)))))

(defun projectile-dired ()
  "Open `dired' at the root of the project."
  (interactive)
  (dired (projectile-project-root)))

(defun projectile-vc ()
  "Open `vc-dir' at the root of the project.

For git projects `magit-status' is used if available."
  (interactive)
  (cond
   ((and (eq (projectile-project-vcs) 'git) (fboundp 'magit-status))
    (magit-status (projectile-project-root)))
   (t (vc-dir (projectile-project-root)))))

(defun projectile-recentf ()
  "Show a list of recently visited files in a project."
  (interactive)
  (if (boundp 'recentf-list)
      (find-file (projectile-expand-root (projectile-completing-read "Recently visited files: " (projectile-recentf-files))))
    (message "recentf is not enabled")))

(defun projectile-recentf-files ()
  "Return a list of recently visited files in a project."
  (if (boundp 'recentf-list)
      (let ((project-root (projectile-project-root)))
        (->> recentf-list
          (-filter (lambda (file) (s-starts-with-p project-root file)))
          (-map (lambda (file) (file-relative-name file project-root)))))
    nil))

(defun projectile-serialize-cache ()
  "Serializes the memory cache to the hard drive."
  (projectile-serialize projectile-projects-cache projectile-cache-file))

(defvar projectile-rails-compile-cmd "bundle exec rails server")
(defvar projectile-ruby-compile-cmd "bundle exec rake build")
(defvar projectile-ruby-test-cmd "bundle exec rake test")
(defvar projectile-ruby-rspec-cmd "bundle exec rspec")
(defvar projectile-django-compile-cmd "python manage.py runserver")
(defvar projectile-django-test-cmd "python manage.py test")
(defvar projectile-python-compile-cmd "python setup.py build")
(defvar projectile-python-test-cmd "python -m unittest discover")
(defvar projectile-symfony-compile-cmd "app/console server:run")
(defvar projectile-symfony-test-cmd "phpunit -c app ")
(defvar projectile-scons-compile-cmd "scons")
(defvar projectile-scons-test-cmd "scons test")
(defvar projectile-maven-compile-cmd "mvn clean install")
(defvar projectile-maven-test-cmd "mvn test")
(defvar projectile-gradle-compile-cmd "gradle build")
(defvar projectile-gradle-test-cmd "gradle test")
(defvar projectile-grails-compile-cmd "grails package")
(defvar projectile-grails-test-cmd "grails test-app")
(defvar projectile-lein-compile-cmd "lein compile")
(defvar projectile-lein-test-cmd "lein test")
(defvar projectile-rebar-compile-cmd "rebar")
(defvar projectile-rebar-test-cmd "rebar eunit")
(defvar projectile-sbt-compile-cmd "sbt compile")
(defvar projectile-sbt-test-cmd "sbt test")
(defvar projectile-make-compile-cmd "make")
(defvar projectile-make-test-cmd "make test")
(defvar projectile-grunt-compile-cmd "grunt")
(defvar projectile-grunt-test-cmd "grunt test")
(defvar projectile-gulp-compile-cmd "gulp")
(defvar projectile-gulp-test-cmd "gulp test")
(defvar projectile-go-compile-cmd "go build ./...")
(defvar projectile-go-test-cmd "go test ./...")

(cl-dolist (var '(projectile-rails-compile-cmd
                  projectile-ruby-compile-cmd
                  projectile-ruby-test-cmd
                  projectile-ruby-rspec-cmd
                  projectile-django-compile-cmd
                  projectile-django-test-cmd
                  projectile-python-compile-cmd
                  projectile-python-test-cmd
                  projectile-symfony-compile-cmd
                  projectile-symfony-test-cmd
                  projectile-scons-compile-cmd
                  projectile-scons-test-cmd
                  projectile-maven-compile-cmd
                  projectile-maven-test-cmd
                  projectile-lein-compile-cmd
                  projectile-lein-test-cmd
                  projectile-rebar-compile-cmd
                  projectile-rebar-test-cmd
                  projectile-sbt-compile-cmd
                  projectile-sbt-test-cmd
                  projectile-make-compile-cmd
                  projectile-make-test-cmd
                  projectile-grunt-compile-cmd
                  projectile-grunt-test-cmd))
  (put var 'safe-local-variable #'stringp))


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
   ((eq project-type 'django) projectile-django-compile-cmd)
   ((eq project-type 'python) projectile-python-compile-cmd)
   ((eq project-type 'symfony) projectile-symfony-compile-cmd)
   ((eq project-type 'lein) projectile-lein-compile-cmd)
   ((eq project-type 'make) projectile-make-compile-cmd)
   ((eq project-type 'rebar) projectile-rebar-compile-cmd)
   ((eq project-type 'scons) projectile-scons-compile-cmd)
   ((eq project-type 'maven) projectile-maven-compile-cmd)
   ((eq project-type 'gradle) projectile-gradle-compile-cmd)
   ((eq project-type 'grails) projectile-grails-compile-cmd)
   ((eq project-type 'sbt) projectile-sbt-compile-cmd)
   ((eq project-type 'grunt) projectile-grunt-compile-cmd)
   ((eq project-type 'gulp) projectile-gulp-compile-cmd)
   ((eq project-type 'go) projectile-go-compile-cmd)
   (t projectile-make-compile-cmd)))

(defun projectile-default-test-command (project-type)
  "Retrieve default test command for PROJECT-TYPE."
  (cond
   ((member project-type '(rails-rspec ruby-rspec)) projectile-ruby-rspec-cmd)
   ((member project-type '(rails-test ruby-test)) projectile-ruby-test-cmd)
   ((eq project-type 'django) projectile-django-test-cmd)
   ((eq project-type 'python) projectile-python-test-cmd)
   ((eq project-type 'symfony) projectile-symfony-test-cmd)
   ((eq project-type 'lein) projectile-lein-test-cmd)
   ((eq project-type 'make) projectile-make-test-cmd)
   ((eq project-type 'rebar) projectile-rebar-test-cmd)
   ((eq project-type 'scons) projectile-scons-test-cmd)
   ((eq project-type 'maven) projectile-maven-test-cmd)
   ((eq project-type 'gradle) projectile-gradle-test-cmd)
   ((eq project-type 'grails) projectile-grails-test-cmd)
   ((eq project-type 'sbt) projectile-sbt-test-cmd)
   ((eq project-type 'grunt) projectile-grunt-test-cmd)
   ((eq project-type 'gulp) projectile-gulp-test-cmd)
   ((eq project-type 'go) projectile-go-test-cmd)
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

(defadvice compilation-find-file (around projectile-compilation-find-file)
  "Try to find a buffer for FILENAME, if we cannot find it,
fallback to the original function."
  (let ((filename (ad-get-arg 1)))
    (setf ad-return-value
          (or
           (if (file-exists-p (expand-file-name filename))
               (find-file-noselect filename))
           ;; Try to find the filename using projectile
           (and (projectile-project-p)
                (loop with root = (projectile-project-root)
                      for dir in (cons "" (projectile-current-project-dirs))
                      for file = (expand-file-name filename
                                                   (expand-file-name dir root))
                      if (file-exists-p file)
                      return (find-file-noselect file)))
           ;; Fall back to the old function `compilation-find-file'
           ad-do-it))))

;; TODO - factor this duplication out
(defun projectile-test-project (arg)
  "Run project test command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let* ((project-root (projectile-project-root))
         (default-cmd (projectile-test-command project-root))
         (test-cmd (if (or compilation-read-command arg)
                       (compilation-read-command default-cmd)
                     default-cmd))
         (default-directory project-root))
    (puthash project-root test-cmd projectile-test-cmd-map)
    (compilation-start test-cmd)))

(defun projectile-relevant-known-projects ()
  "Return a list of known projects except the current one (if present)."
  (if (projectile-project-p)
      (-difference projectile-known-projects
                   (list (abbreviate-file-name (projectile-project-root))))
    projectile-known-projects))

(defun projectile-switch-project (&optional arg)
  "Switch to a project we have visited before.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'"
  (interactive "P")
  (let ((relevant-projects (projectile-relevant-known-projects)))
    (if relevant-projects
        (projectile-switch-project-by-name
         (projectile-completing-read "Switch to project: " relevant-projects)
         arg)
      (error "There are no known projects"))))

(defun projectile-switch-project-by-name (project-to-switch &optional arg)
  "Switch to project by project name PROJECT-TO-SWITCH.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'"
  (let* ((default-directory project-to-switch)
         (switch-project-action (if arg
                                    'projectile-commander
                                  projectile-switch-project-action)))
    (if projectile-remember-window-configs
        (unless (projectile-restore-window-config (projectile-project-name))
          (funcall switch-project-action)
          (delete-other-windows))
      (funcall switch-project-action))
    (run-hooks 'projectile-switch-project-hook)))


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

(defun projectile-find-file-in-known-projects ()
  "Jump to a file in any of the known projects."
  (interactive)
  (let ((projectile-require-project-root nil)
        (all-files nil))
    (-each projectile-known-projects
      (lambda (project)
        (when (file-exists-p project)
          (let ((default-directory project))
            (setq all-files (append all-files (-map (lambda (file)
                                                      (expand-file-name file project))
                                                    (projectile-current-project-files))))))))
    (find-file (projectile-completing-read "Find file in projects: " all-files))))

(defcustom projectile-switch-project-hook nil
  "Hooks run when project is switched."
  :group 'projectile
  :type 'hook)


(defun projectile-cleanup-known-projects ()
  "Remove known projects that don't exist anymore."
  (interactive)
  (setq projectile-known-projects (--filter (projectile-file-exists-p it) projectile-known-projects))
  (projectile-save-known-projects))

(defun projectile-clear-known-projects ()
  "Clear both `projectile-known-projects' and `projectile-known-projects-file'."
  (interactive)
  (setq projectile-known-projects nil)
  (projectile-save-known-projects))

(defun projectile-remove-known-project (&optional project)
  "Remove PROJECT from the list of known projects."
  (interactive (list (projectile-completing-read "Remove from known projects: "
                                                 projectile-known-projects)))
  (setq projectile-known-projects
        (--reject (string= project it) projectile-known-projects))
  (projectile-save-known-projects)
  (message "Project %s removed from the list of known projects." project))

(defun projectile-remove-current-project-from-known-projects ()
  "Remove the current project from the list of known projects."
  (interactive)
  (projectile-remove-known-project (abbreviate-file-name (projectile-project-root))))

(defun projectile-ignored-projects ()
  "A list of projects that should not be save in `projectile-known-projects'."
  (-map 'file-truename projectile-ignored-projects))

(defun projectile-add-known-project (project-root)
  "Add PROJECT-ROOT to the list of known projects."
  (unless (member project-root (projectile-ignored-projects))
    (setq projectile-known-projects
          (-distinct
           (cons (abbreviate-file-name project-root)
                 projectile-known-projects)))))

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

(define-ibuffer-filter projectile-files
  "Show Ibuffer with all buffers in the current project."
  (:reader (read-directory-name "Project root: " (ignore-errors (projectile-project-root)))
           :description nil)
  (with-current-buffer buf
    (equal (file-name-as-directory (expand-file-name qualifier))
           (ignore-errors (projectile-project-root)))))

(defun projectile-ibuffer-by-project (project-root)
  "Open an IBuffer window showing all buffers in PROJECT-ROOT."
  (let ((project-name (file-name-nondirectory (directory-file-name project-root))))
    (ibuffer nil (format "*%s Buffers*" project-name)
             (list (cons 'projectile-files project-root)))))

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
  (message "Commander [%s]: "
           (apply #'string (mapcar #'car projectile-commander-methods)))
  (let* ((ch (save-window-excursion
               (select-window (minibuffer-window))
               (read-char)))
         (method (cl-find ch projectile-commander-methods :key #'car)))
    (cond (method
           (funcall (cl-caddr method)))
          (t
           (message "No method for character: ?\\%c" ch)
           (ding)
           (sleep-for 1)
           (discard-input)
           (projectile-commander)))))

(defmacro def-projectile-commander-method (key description &rest body)
  "Define a new `projectile-commander' method.

KEY is the key the user will enter to choose this method.

DESCRIPTION is a one-line sentence describing how the method.

BODY is a series of forms which are evaluated when the find
is chosen."
  (let ((method `(lambda ()
                   ,@body)))
    `(setq projectile-commander-methods
           (cl-sort (cons (list ,key ,description ,method)
                          (cl-remove ,key projectile-commander-methods :key #'car))
                    #'< :key #'car))))

(def-projectile-commander-method ?? "Commander help buffer."
  (ignore-errors (kill-buffer projectile-commander-help-buffer))
  (with-current-buffer (get-buffer-create projectile-commander-help-buffer)
    (insert "Projectile Commander Methods:\n\n")
    (loop for (key line nil) in projectile-commander-methods
          do (insert (format "%c:\t%s\n" key line)))
    (goto-char (point-min))
    (help-mode)
    (display-buffer (current-buffer) t))
  (projectile-commander))

(def-projectile-commander-method ?a
  "Run ack on project."
  (call-interactively 'projectile-ack))

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
  (projectile-recentf))

;;; Minor mode
(defvar projectile-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "4 b") 'projectile-switch-to-buffer-other-window)
    (define-key map (kbd "4 C-o") 'projectile-display-buffer)
    (define-key map (kbd "4 d") 'projectile-find-dir-other-window)
    (define-key map (kbd "4 f") 'projectile-find-file-other-window)
    (define-key map (kbd "4 t") 'projectile-find-implementation-or-test-other-window)
    (define-key map (kbd "!") 'projectile-run-shell-command-in-root)
    (define-key map (kbd "&") 'projectile-run-async-shell-command-in-root)
    (define-key map (kbd "b") 'projectile-switch-to-buffer)
    (define-key map (kbd "c") 'projectile-compile-project)
    (define-key map (kbd "d") 'projectile-find-dir)
    (define-key map (kbd "D") 'projectile-dired)
    (define-key map (kbd "e") 'projectile-recentf)
    (define-key map (kbd "f") 'projectile-find-file)
    (define-key map (kbd "F") 'projectile-find-file-in-known-projects)
    (define-key map (kbd "i") 'projectile-invalidate-cache)
    (define-key map (kbd "I") 'projectile-ibuffer)
    (define-key map (kbd "j") 'projectile-find-tag)
    (define-key map (kbd "k") 'projectile-kill-buffers)
    (define-key map (kbd "l") 'projectile-find-file-in-directory)
    (define-key map (kbd "m") 'projectile-commander)
    (define-key map (kbd "o") 'projectile-multi-occur)
    (define-key map (kbd "p") 'projectile-switch-project)
    (define-key map (kbd "P") 'projectile-test-project)
    (define-key map (kbd "r") 'projectile-replace)
    (define-key map (kbd "R") 'projectile-regenerate-tags)
    (define-key map (kbd "s a") 'projectile-ack)
    (define-key map (kbd "s g") 'projectile-grep)
    (define-key map (kbd "s s") 'projectile-ag)
    (define-key map (kbd "S") 'projectile-save-project-buffers)
    (define-key map (kbd "t") 'projectile-toggle-between-implementation-and-test)
    (define-key map (kbd "T") 'projectile-find-test-file)
    (define-key map (kbd "v") 'projectile-vc)
    (define-key map (kbd "z") 'projectile-cache-current-file)
    (define-key map (kbd "ESC") 'projectile-project-buffers-other-buffer)
    map)
  "Keymap for Projectile commands after `projectile-keymap-prefix'")
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
   ["Switch to buffer" projectile-switch-to-buffer]
   ["Jump between implementation file and test file" projectile-toggle-between-implementation-and-test]
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
   ["Regenerate [e|g]tags" projectile-regenerate-tags]
   "--"
   ["Compile project" projectile-compile-project]
   ["Test project" projectile-test-project]
   "--"
   ["About" projectile-version])
 "Search Files (Grep)...")

(easy-menu-change '("Tools") "--" nil "Search Files (Grep)...")

;;;###autoload
(defcustom projectile-mode-line
  '(:eval (format " Projectile[%s]" (projectile-project-name)))
  "Mode line ligher for Projectile.

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
    (add-hook 'find-file-hook 'projectile-cache-files-find-file-hook t t)
    (add-hook 'find-file-hook 'projectile-cache-projects-find-file-hook t t)
    (add-hook 'projectile-find-dir-hook 'projectile-cache-projects-find-file-hook)
    (add-hook 'find-file-hook 'projectile-visit-project-tags-table t t)
    (ad-activate 'compilation-find-file))
   (t
    (remove-hook 'find-file-hook 'projectile-cache-files-find-file-hook t)
    (remove-hook 'find-file-hook 'projectile-cache-projects-find-file-hook t)
    (remove-hook 'find-file-hook 'projectile-visit-project-tags-table t)
    (ad-deactivate 'compilation-find-file))))

;;;###autoload
(define-globalized-minor-mode projectile-global-mode
  projectile-mode
  projectile-mode)

(provide 'projectile)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; projectile.el ends here
