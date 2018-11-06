;;; projectile.el --- Manage and navigate projects in Emacs easily -*- lexical-binding: t -*-

;; Copyright © 2011-2018 Bozhidar Batsov <bozhidar@batsov.com>

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/projectile
;; Keywords: project, convenience
;; Version: 1.1.0-snapshot
;; Package-Requires: ((emacs "25.1") (pkg-info "0.4"))

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

(require 'cl-lib)
(require 'thingatpt)
(require 'ibuffer)
(require 'ibuf-ext)
(require 'compile)
(require 'grep)
(eval-when-compile
  (require 'subr-x))

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
(declare-function eshell-search-path "esh-ext")
(declare-function vc-dir "vc-dir")
(declare-function vc-dir-busy "vc-dir")
(declare-function ripgrep-regexp "ripgrep")
(declare-function string-trim "subr-x")

(defvar grep-files-aliases)
(defvar grep-find-ignored-directories)
(defvar grep-find-ignored-files)


;;; Customization
(defgroup projectile nil
  "Manage and navigate projects easily."
  :group 'tools
  :group 'convenience
  :link '(url-link :tag "GitHub" "https://github.com/bbatsov/projectile")
  :link '(url-link :tag "Online Manual" "https://docs.projectile.mx/")
  :link '(emacs-commentary-link :tag "Commentary" "projectile"))

(defcustom projectile-indexing-method (if (eq system-type 'windows-nt) 'native 'alien)
  "Specifies the indexing method used by Projectile.

There are three indexing methods - native, hybrid and alien.

The native method is implemented in Emacs Lisp (therefore it is
native to Emacs).  Its advantage is that it is portable and will
work everywhere that Emacs does.  Its disadvantage is that it is a
bit slow (especially for large projects).  Generally it's a good
idea to pair the native indexing method with caching.

The hybrid indexing method uses external tools (e.g. git, find,
etc) to speed up the indexing process.  Still, the files will be
post-processed by Projectile for sorting/filtering purposes.
In this sense that approach is a hybrid between native and indexing
and alien indexing.

The alien indexing method optimizes to the limit the speed
of the hybrid indexing method.  This means that Projectile will
not do any processing of the files returned by the external
commands and you're going to get the maximum performance
possible.  This behaviour makes a lot of sense for most people,
as they'd typically be putting ignores in their VCS config and
won't care about any additional ignores/unignores/sorting that
Projectile might also provide.

The disadvantage of the hybrid and alien methods is that they are not well
supported on Windows systems.  That's why by default alien indexing is the
default on all operating systems, except Windows."
  :group 'projectile
  :type '(radio
          (const :tag "Native" native)
          (const :tag "Hybrid" hybrid)
          (const :tag "Alien" alien)))

(defcustom projectile-enable-caching (eq projectile-indexing-method 'native)
  "When t enables project files caching.

Project caching is automatically enabled by default if you're
using the native indexing method."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-kill-buffers-filter 'kill-all
  "Determine which buffers are killed by `projectile-kill-buffers'.

When the kill-all option is selected, kills each buffer.

When the kill-only-files option is selected, kill only the buffer
associated to a file.

Otherwise, it should be a predicate that takes one argument: the buffer to
be killed."
  :group 'projectile
  :type '(radio
          (const :tag "All project buffers" kill-all)
          (const :tag "Project file buffers" kill-only-files)
          (function :tag "Predicate")))

(defcustom projectile-file-exists-local-cache-expire nil
  "Number of seconds before the local file existence cache expires.
Local refers to a file on a local file system.

A value of nil disables this cache.
See `projectile-file-exists-p' for details."
  :group 'projectile
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds")))

(defcustom projectile-file-exists-remote-cache-expire (* 5 60)
  "Number of seconds before the remote file existence cache expires.
Remote refers to a file on a remote file system such as tramp.

A value of nil disables this cache.
See `projectile-file-exists-p' for details."
  :group 'projectile
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds")))

(defcustom projectile-files-cache-expire nil
  "Number of seconds before project files list cache expires.

A value of nil means the cache never expires."
  :group 'projectile
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds")))

(defcustom projectile-require-project-root 'prompt
  "Require the presence of a project root to operate when true.
When set to 'prompt Projectile will ask you to select a project
directory if you're not in a project.

When nil Projectile will consider the current directory the project root."
  :group 'projectile
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (const :tag "Prompt for project" prompt)))

(defcustom projectile-completion-system 'ido
  "The completion system to be used by Projectile."
  :group 'projectile
  :type '(radio
          (const :tag "Ido" ido)
          (const :tag "Helm" helm)
          (const :tag "Ivy" ivy)
          (const :tag "Default" default)
          (function :tag "Custom function")))

(defcustom projectile-keymap-prefix nil
  "Projectile keymap prefix."
  :group 'projectile
  :type 'string)

(make-obsolete-variable 'projectile-keymap-prefix "Use (define-key projectile-mode-map (kbd ...) 'projectile-command-map) instead." "1.1.0")

(defcustom projectile-cache-file
  (expand-file-name "projectile.cache" user-emacs-directory)
  "The name of Projectile's cache file."
  :group 'projectile
  :type 'string)

(defcustom projectile-tags-file-name "TAGS"
  "The tags filename Projectile's going to use."
  :group 'projectile
  :type 'string)

(defcustom projectile-tags-command "ctags -Re -f \"%s\" %s \"%s\""
  "The command Projectile's going to use to generate a TAGS file."
  :group 'projectile
  :type 'string)

(defcustom projectile-tags-backend 'auto
  "The tag backend that Projectile should use.

If set to 'auto', `projectile-find-tag' will automatically choose
which backend to use.  Preference order is ggtags -> xref
-> etags-select -> `find-tag'.  Variable can also be set to specify which
backend to use.  If selected backend is unavailable, fall back to
`find-tag'.

If this variable is set to 'auto' and ggtags is available, or if
set to 'ggtags', then ggtags will be used for
`projectile-regenerate-tags'.  For all other settings
`projectile-tags-command' will be used."
  :group 'projectile
  :type '(radio
          (const :tag "auto" auto)
          (const :tag "xref" xref)
          (const :tag "ggtags" ggtags)
          (const :tag "etags" etags-select)
          (const :tag "standard" find-tag))
  :package-version '(projectile . "0.14.0"))

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
  :type 'function)

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
  :type 'function
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-project-root-files
  '("rebar.config"       ; Rebar project file
    "project.clj"        ; Leiningen project file
    "build.boot"         ; Boot-clj project file
    "deps.edn"           ; Clojure CLI project file
    "SConstruct"         ; Scons project file
    "pom.xml"            ; Maven project file
    "build.sbt"          ; SBT project file
    "gradlew"            ; Gradle wrapper script
    "build.gradle"       ; Gradle project file
    ".ensime"            ; Ensime configuration file
    "Gemfile"            ; Bundler file
    "requirements.txt"   ; Pip file
    "setup.py"           ; Setuptools file
    "tox.ini"            ; Tox file
    "composer.json"      ; Composer project file
    "Cargo.toml"         ; Cargo project file
    "mix.exs"            ; Elixir mix project file
    "stack.yaml"         ; Haskell's stack tool based project
    "info.rkt"           ; Racket package description file
    "DESCRIPTION"        ; R package description file
    "TAGS"               ; etags/ctags are usually in the root of project
    "GTAGS"              ; GNU Global tags
    "configure.in"       ; autoconf old style
    "configure.ac"       ; autoconf new style
    "cscope.out"         ; cscope
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
    "_FOSSIL_"    ; Fossil VCS root DB on Windows
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
  "A list of files globally unignored by projectile.

Regular expressions can be used."
  :group 'projectile
  :type '(repeat string)
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-globally-ignored-file-suffixes
  nil
  "A list of file suffixes globally ignored by projectile."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-globally-ignored-directories
  '(".idea"
    ".ensime_cache"
    ".eunit"
    ".git"
    ".hg"
    ".fslckout"
    "_FOSSIL_"
    ".bzr"
    "_darcs"
    ".tox"
    ".svn"
    ".stack-work")
  "A list of directories globally ignored by projectile.

Regular expressions can be used."
  :safe (lambda (x) (not (remq t (mapcar #'stringp x))))
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-globally-unignored-directories nil
  "A list of directories globally unignored by projectile."
  :group 'projectile
  :type '(repeat string)
  :package-version '(projectile . "0.14.0"))

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

You can use either exact buffer names or regular expressions.
If a buffer is in the list projectile will ignore it for
functions working with buffers."
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
  :type 'function)

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

(defcustom projectile-dynamic-mode-line t
  "If true, update the mode-line dynamically.
Only file buffers are affected by this, as the update happens via
`find-file-hook'.

See also `projectile-mode-line-function' and `projectile-update-mode-line'."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "1.1.0"))

(defcustom projectile-mode-line-function 'projectile-default-mode-line
  "The function to use to generate project-specific mode-line.
The default function adds the project name and type to the mode-line.
See also `projectile-update-mode-line'."
  :group 'projectile
  :type 'function
  :package-version '(projectile . "1.1.0"))


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

(defvar projectile-projects-cache nil
  "A hashmap used to cache project file names to speed up related operations.")

(defvar projectile-projects-cache-time nil
  "A hashmap used to record when we populated `projectile-projects-cache'.")

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

(defcustom projectile-track-known-projects-automatically t
  "Controls whether Projectile will automatically register known projects.

When set to nil you'll have always add projects explicitly with
`projectile-add-known-project'."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "1.0.0"))

(defcustom projectile-project-search-path nil
  "List of folders where projectile is automatically going to look for projects.
You can think of something like $PATH, but for projects instead of executables.
Examples of such paths might be ~/projects, ~/work, etc."
  :group 'projectile
  :type 'list
  :package-version '(projectile . "1.0.0"))

(defcustom projectile-git-command "git ls-files -zco --exclude-standard"
  "Command used by projectile to get the files in a git project."
  :group 'projectile
  :type 'string)

(defcustom projectile-git-submodule-command "git submodule --quiet foreach 'echo $path' | tr '\\n' '\\0'"
  "Command used by projectile to list submodules of a given git repository.
Set to nil to disable listing submodules contents."
  :group 'projectile
  :type 'string)

(defcustom projectile-git-ignored-command "git ls-files -zcoi --exclude-standard"
  "Command used by projectile to get the ignored files in a git project."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-hg-command "hg locate -f -0 -I ."
  "Command used by projectile to get the files in a hg project."
  :group 'projectile
  :type 'string)

(defcustom projectile-fossil-command (concat "fossil ls | "
                                             (when (string-equal system-type
                                                                 "windows-nt")
                                               "dos2unix | ")
                                             "tr '\\n' '\\0'")
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

(defcustom projectile-vcs-dirty-state '("edited" "unregistered" "needs-update" "needs-merge" "unlocked-changes" "conflict")
  "List of states checked by `projectile-browse-dirty-projects'.
Possible checked states are:
\"edited\", \"unregistered\", \"needs-update\", \"needs-merge\",
\"unlocked-changes\" and \"conflict\",
as defined in `vc.el'."
  :group 'projectile
  :type '(repeat (string))
  :package-version '(projectile . "1.0.0"))

(defcustom projectile-other-file-alist
  '( ;; handle C/C++ extensions
    ("cpp" . ("h" "hpp" "ipp"))
    ("ipp" . ("h" "hpp" "cpp"))
    ("hpp" . ("h" "ipp" "cpp" "cc"))
    ("cxx" . ("h" "hxx" "ixx"))
    ("ixx" . ("h" "hxx" "cxx"))
    ("hxx" . ("h" "ixx" "cxx"))
    ("c"   . ("h"))
    ("m"   . ("h"))
    ("mm"  . ("h"))
    ("h"   . ("c" "cc" "cpp" "ipp" "hpp" "cxx" "ixx" "hxx" "m" "mm"))
    ("cc"  . ("h" "hh" "hpp"))
    ("hh"  . ("cc"))

    ;; vertex shader and fragment shader extensions in glsl
    ("vert" . ("frag"))
    ("frag" . ("vert"))

    ;; handle files with no extension
    (nil    . ("lock" "gpg"))
    ("lock" . (""))
    ("gpg"  . (""))
    )
  "Alist of extensions for switching to file with the same name,
  using other extensions based on the extension of current
  file."
  :type 'alist)

(defcustom projectile-create-missing-test-files nil
  "During toggling, if non-nil enables creating test files if not found.

When not-nil, every call to projectile-find-implementation-or-test-*
creates test files if not found on the file system.  Defaults to nil.
It assumes the test/ folder is at the same level as src/."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-after-switch-project-hook nil
  "Hooks run right after project is switched."
  :group 'projectile
  :type 'hook)

(defcustom projectile-before-switch-project-hook nil
  "Hooks run when right before project is switched."
  :group 'projectile
  :type 'hook)

(defcustom projectile-current-project-on-switch 'remove
  "Determines whether to display current project when switching projects.

When set to 'remove current project is not included, 'move-to-end
will display current project and the end of the list of known
projects, 'keep will leave the current project at the default
position."
  :group 'projectile
  :type '(radio
          (const :tag "Remove" remove)
          (const :tag "Move to end" move-to-end)
          (const :tag "Keep" keep)))


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
          (message "Projectile %s" version))
        version)
    (error "Cannot determine version without package pkg-info")))

;;; Misc utility functions
(defun projectile-difference (list1 list2)
  (cl-remove-if
   (lambda (x) (member x list2))
   list1))

(defun projectile-unixy-system-p ()
  "Check to see if unixy text utilities are installed."
  (cl-every
   (lambda (x) (executable-find x))
   '("grep" "cut" "uniq")))

(defun projectile-symbol-or-selection-at-point ()
  "Get the symbol or selected text at point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (projectile-symbol-at-point)))

(defun projectile-symbol-at-point ()
  "Get the symbol at point and strip its properties."
  (substring-no-properties (or (thing-at-point 'symbol) "")))



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
(defun projectile-invalidate-cache (prompt)
  "Remove the current project's files from `projectile-projects-cache'.

With a prefix argument PROMPT prompts for the name of the project whose cache
to invalidate."
  (interactive "P")
  (let ((project-root
         (if prompt
             (completing-read "Remove cache for: "
                              (hash-table-keys projectile-projects-cache))
           (projectile-ensure-project (projectile-project-root)))))
    (setq projectile-project-root-cache (make-hash-table :test 'equal))
    (remhash project-root projectile-project-type-cache)
    (remhash project-root projectile-projects-cache)
    (remhash project-root projectile-projects-cache-time)
    (projectile-serialize-cache)
    (when projectile-verbose
      (message "Invalidated Projectile cache for %s."
               (propertize project-root 'face 'font-lock-keyword-face))))
  (when (fboundp 'recentf-cleanup)
    (recentf-cleanup)))

(defun projectile-time-seconds ()
  "Return the number of seconds since the unix epoch."
  (cl-destructuring-bind (high low _usec _psec) (current-time)
    (+ (lsh high 16) low)))

(defun projectile-cache-project (project files)
  "Cache PROJECTs FILES.
The cache is created both in memory and on the hard drive."
  (when projectile-enable-caching
    (puthash project files projectile-projects-cache)
    (puthash project (projectile-time-seconds) projectile-projects-cache-time)
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
             (cl-remove-if (lambda (str) (string-prefix-p dir str)) project-cache)
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
  (let ((project-root (projectile-project-p)))
    (when (and projectile-enable-caching
               project-root
               (not (projectile-ignored-project-p project-root)))
      (projectile-cache-current-file))))

(defun projectile-track-known-projects-find-file-hook ()
  "Function for caching projects with `find-file-hook'."
  (when (and projectile-track-known-projects-automatically (projectile-project-p))
    (projectile-add-known-project (projectile-project-root))))

(defun projectile-maybe-invalidate-cache (force)
  "Invalidate if FORCE or project's dirconfig newer than cache."
  (when (or force (file-newer-than-file-p (projectile-dirconfig-file)
                                          projectile-cache-file))
    (projectile-invalidate-cache nil)))

;;;###autoload
(defun projectile-discover-projects-in-directory (directory)
  "Discover any projects in DIRECTORY and add them to the projectile cache.
This function is not recursive and only adds projects with roots
at the top level of DIRECTORY."
  (interactive
   (list (read-directory-name "Starting directory: ")))
  (let ((subdirs (directory-files directory t)))
    (mapcar
     (lambda (dir)
       (when (and (file-directory-p dir)
                  (not (member (file-name-nondirectory dir) '(".." "."))))
         (when (projectile-project-p dir)
           (projectile-add-known-project dir))))
     subdirs)))

;;;###autoload
(defun projectile-discover-projects-in-search-path ()
  "Discover projects in `projectile-project-search-path'.
Invoked automatically when `projectile-mode' is enabled."
  (interactive)
  (mapcar #'projectile-discover-projects-in-directory projectile-project-search-path))


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
     (cl-find-if (lambda (f) (projectile-file-exists-p (expand-file-name f dir)))
                 (or list projectile-project-root-files)))))

(defun projectile-root-bottom-up (dir &optional list)
  "Identify a project root in DIR by bottom-up search for files in LIST.
If LIST is nil, use `projectile-project-root-files-bottom-up' instead.
Return the first (bottommost) matched directory or nil if not found."
  (cl-some (lambda (name) (projectile-locate-dominating-file dir name))
           (or list projectile-project-root-files-bottom-up)))

(defun projectile-root-top-down-recurring (dir &optional list)
  "Identify a project root in DIR by recurring top-down search for files in LIST.
If LIST is nil, use `projectile-project-root-files-top-down-recurring'
instead.  Return the last (bottommost) matched directory in the
topmost sequence of matched directories.  Nil otherwise."
  (cl-some
   (lambda (f)
     (projectile-locate-dominating-file
      dir
      (lambda (dir)
        (and (projectile-file-exists-p (expand-file-name f dir))
             (or (string-match locate-dominating-stop-dir-regexp (projectile-parent dir))
                 (not (projectile-file-exists-p (expand-file-name f (projectile-parent dir)))))))))
   (or list projectile-project-root-files-top-down-recurring)))

(defun projectile-project-root (&optional dir)
  "Retrieves the root directory of a project if available.
If DIR is not supplied its set to the current directory by default."
  ;; the cached value will be 'none in the case of no project root (this is to
  ;; ensure it is not reevaluated each time when not inside a project) so use
  ;; cl-subst to replace this 'none value with nil so a nil value is used
  ;; instead
  (let ((dir (or dir default-directory)))
    (cl-subst nil 'none
              ;; The `is-local' and `is-connected' variables are
              ;; used to fix the behavior where Emacs hangs
              ;; because of Projectile when you open a file over
              ;; TRAMP. It basically prevents Projectile from
              ;; trying to find information about files for which
              ;; it's not possible to get that information right
              ;; now.
              (or (let ((is-local (not (file-remote-p dir)))      ;; `true' if the file is local
                        (is-connected (file-remote-p dir nil t))) ;; `true' if the file is remote AND we are connected to the remote
                    (when (or is-local is-connected)
                      (cl-some
                       (lambda (func)
                         (let* ((cache-key (format "%s-%s" func dir))
                                (cache-value (gethash cache-key projectile-project-root-cache)))
                           (if (and cache-value (file-exists-p cache-value))
                               cache-value
                             (let ((value (funcall func (file-truename dir))))
                               (puthash cache-key value projectile-project-root-cache)
                               value))))
                       projectile-project-root-files-functions)))
                  ;; set cached to none so is non-nil so we don't try
                  ;; and look it up again
                  'none))))

(defun projectile-ensure-project (dir)
  "Ensure that DIR is non-nil.
Useful for commands that expect the presence of a project.
Controlled by `projectile-require-project-root'."
  (if dir
      dir
    (cond
     ((eq projectile-require-project-root 'prompt) (projectile-completing-read
                                                    "Switch to project: " projectile-known-projects))
     (projectile-require-project-root (error "Projectile can't find a project definition in %s" dir))
     (t default-directory))))

(defun projectile-project-p (&optional dir)
  "Check if DIR is a project.
Defaults to the current directory if not provided
explicitly."
  (projectile-project-root (or dir default-directory)))

(defun projectile-default-project-name (project-root)
  "Default function used create project name to be displayed based on the value of PROJECT-ROOT."
  (file-name-nondirectory (directory-file-name project-root)))

(defun projectile-project-name (&optional project)
  "Return project name.
If PROJECT is not specified acts on the current project."
  (or projectile-project-name
      (let ((project-root (or project (projectile-project-root))))
        (if project-root
            (funcall projectile-project-name-function project-root)
          "-"))))


;;; Project indexing
(defun projectile-get-project-directories (project-dir)
  "Get the list of PROJECT-DIR directories that are of interest to the user."
  (mapcar (lambda (subdir) (concat project-dir subdir))
          (or (nth 0 (projectile-parse-dirconfig-file)) '(""))))

(defun projectile--directory-p (directory)
  "Checks if DIRECTORY is a string designating a valid directory."
  (and (stringp directory) (file-directory-p directory)))

(defun projectile-dir-files (directory)
  "List the files in DIRECTORY and in its sub-directories.
Files are returned as relative paths to DIRECTORY."
  (unless (projectile--directory-p directory)
    (error "Directory %S does not exist" directory))
  ;; check for a cache hit first if caching is enabled
  (let ((files-list (and projectile-enable-caching
                         (gethash directory projectile-projects-cache))))
    ;; cache disabled or cache miss
    (or files-list
        (let ((vcs (projectile-project-vcs directory)))
          (pcase projectile-indexing-method
           ('native (projectile-dir-files-native directory))
           ;; use external tools to get the project files
           ('hybrid (projectile-adjust-files directory vcs (projectile-dir-files-alien directory)))
           ('alien (projectile-dir-files-alien directory))
           (_ (user-error "Unsupported indexing method `%S'" projectile-indexing-method)))))))

;;; Native Project Indexing
;;
;; This corresponds to `projectile-indexing-method' being set to native.
(defun projectile-dir-files-native (directory)
  "Get the files for ROOT under DIRECTORY using just Emacs Lisp."
  (let ((progress-reporter
         (make-progress-reporter
          (format "Projectile is indexing %s"
                  (propertize directory 'face 'font-lock-keyword-face)))))
    ;; we need the files with paths relative to the project root
    (mapcar (lambda (file) (file-relative-name file directory))
            (projectile-index-directory directory (projectile-filtering-patterns)
                                        progress-reporter))))

(defun projectile-index-directory (directory patterns progress-reporter)
  "Index DIRECTORY taking into account PATTERNS.
The function calls itself recursively until all sub-directories
have been indexed.  The PROGRESS-REPORTER is updated while the
function is executing."
  (apply 'append
         (mapcar
          (lambda (f)
            (unless (or (and patterns (projectile-ignored-rel-p f directory patterns))
                        (member (file-name-nondirectory (directory-file-name f))
                                '("." ".." ".svn" ".cvs")))
              (progress-reporter-update progress-reporter)
              (if (file-directory-p f)
                  (unless (projectile-ignored-directory-p
                           (file-name-as-directory f))
                    (projectile-index-directory f patterns progress-reporter))
                (unless (projectile-ignored-file-p f)
                  (list f)))))
          (directory-files directory t))))

;;; Alien Project Indexing
;;
;; This corresponds to `projectile-indexing-method' being set to hybrid or alien.
;; The only difference between the two methods is that alien doesn't do
;; any post-processing of the files obtained via the external command.
(defun projectile-dir-files-alien (directory)
  "Get the files for DIRECTORY using external tools."
  (let ((vcs (projectile-project-vcs directory)))
    (cond
    ((eq vcs 'git)
     (nconc (projectile-files-via-ext-command directory (projectile-get-ext-command vcs))
            (projectile-get-sub-projects-files directory vcs)))
    (t (projectile-files-via-ext-command directory (projectile-get-ext-command vcs))))))

(define-obsolete-function-alias 'projectile-dir-files-external 'projectile-dir-files-alien "1.1")
(define-obsolete-function-alias 'projectile-get-repo-files 'projectile-dir-files-alien "1.1")

(defun projectile-get-ext-command (vcs)
  "Determine which external command to invoke based on the project's VCS.
Fallback to a generic command when not in a VCS-controlled project."
  (pcase vcs
   ('git projectile-git-command)
   ('hg projectile-hg-command)
   ('fossil projectile-fossil-command)
   ('bzr projectile-bzr-command)
   ('darcs projectile-darcs-command)
   ('svn projectile-svn-command)
   (_ projectile-generic-command)))

(defun projectile-get-sub-projects-command (vcs)
  "Get the sub-projects command for VCS.
Currently that's supported just for Git (sub-projects being Git
sub-modules there)."
  (pcase vcs
   ('git projectile-git-submodule-command)
   (_ "")))

(defun projectile-get-ext-ignored-command (vcs)
  "Determine which external command to invoke based on the project's VCS."
  (pcase vcs
   ('git projectile-git-ignored-command)
   ;; TODO: Add support for other VCS
   (_ nil)))

(defun projectile-flatten (lst)
  "Take a nested list LST and return its contents as a single, flat list."
  (if (and (listp lst) (listp (cdr lst)))
      (cl-mapcan 'projectile-flatten lst)
    (list lst)))

(defun projectile-get-all-sub-projects (project)
  "Get all sub-projects for a given project.

PROJECT is base directory to start search recursively."
  (let ((submodules (projectile-get-immediate-sub-projects project)))
    (cond
     ((null submodules)
      nil)
     (t
      (nconc submodules (projectile-flatten
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
  (let* ((vcs (projectile-project-vcs path))
         ;; search for sub-projects under current project `project'
         (submodules (mapcar
                      (lambda (s)
                        (file-name-as-directory (expand-file-name s path)))
                      (projectile-files-via-ext-command path (projectile-get-sub-projects-command vcs))))
         (project-child-folder-regex
          (concat "\\`"
                  (regexp-quote path))))

    ;; If project root is inside of an VCS folder, but not actually an
    ;; VCS root itself, submodules external to the project will be
    ;; included in the VCS get sub-projects result. Let's remove them.
    (cl-remove-if-not
     (lambda (submodule)
       (string-match-p project-child-folder-regex
                       submodule))
     submodules)))

(defun projectile-get-sub-projects-files (project-root vcs)
  "Get files from sub-projects for PROJECT-ROOT recursively."
  (projectile-flatten
   (mapcar (lambda (sub-project)
             (mapcar (lambda (file)
                       (concat sub-project file))
                     ;; TODO: Seems we forgot git hardcoded here
                     (projectile-files-via-ext-command sub-project projectile-git-command)))
           (projectile-get-all-sub-projects project-root))))

(defun projectile-get-repo-ignored-files (project vcs)
  "Get a list of the files ignored in the PROJECT using VCS."
  (let ((cmd (projectile-get-ext-ignored-command vcs)))
    (when cmd
      (projectile-files-via-ext-command project cmd))))

(defun projectile-get-repo-ignored-directory (project dir vcs)
  "Get a list of the files ignored in the PROJECT in the directory DIR.
VCS is the VCS of the project."
  (let ((cmd (projectile-get-ext-ignored-command vcs)))
    (when cmd
      (projectile-files-via-ext-command project (concat cmd " " dir)))))

(defun projectile-files-via-ext-command (root command)
  "Get a list of relative file names in the project ROOT by executing COMMAND.

If `command' is nil or an empty string, return nil.
This allows commands to be disabled."
  (when (stringp command)
    (let ((default-directory root))
      (split-string (shell-command-to-string command) "\0" t))))

(defun projectile-adjust-files (project vcs files)
  "First remove ignored files from FILES, then add back unignored files."
  (projectile-add-unignored project vcs (projectile-remove-ignored files)))

(defun projectile-remove-ignored (files)
  "Remove ignored files and folders from FILES.

If ignored directory prefixed with '*', then ignore all
directories/subdirectories with matching filename,
otherwise operates relative to project root."
  (let ((ignored-files (projectile-ignored-files-rel))
        (ignored-dirs (projectile-ignored-directories-rel)))
    (cl-remove-if
     (lambda (file)
       (or (cl-some
            (lambda (f)
              (string= f (file-name-nondirectory file)))
            ignored-files)
           (cl-some
            (lambda (dir)
              ;; if the directory is prefixed with '*' then ignore all directories matching that name
              (if (string-prefix-p "*" dir)
                  ;; remove '*' and trailing slash from ignored directory name
                  (let ((d (substring dir 1 (if (equal (substring dir -1) "/") -1 nil))))
                    (cl-some
                     (lambda (p)
                       (string= d p))
                     ;; split path by '/', remove empty strings, and check if any subdirs match name 'd'
                     (delete "" (split-string (or (file-name-directory file) "") "/"))))
                (string-prefix-p dir file)))
            ignored-dirs)
           (cl-some
            (lambda (suf)
              (string-suffix-p suf file t))
            projectile-globally-ignored-file-suffixes)))
     files)))

(defun projectile-keep-ignored-files (project vcs files)
  "Filter FILES to retain only those that are ignored."
  (when files
    (cl-remove-if-not
     (lambda (file)
       (cl-some (lambda (f) (string-prefix-p f file)) files))
     (projectile-get-repo-ignored-files project vcs))))

(defun projectile-keep-ignored-directories (project vcs directories)
  "Get ignored files within each of DIRECTORIES."
  (when directories
    (let (result)
      (dolist (dir directories result)
        (setq result (append result
                             (projectile-get-repo-ignored-directory project vcs dir))))
      result)))

(defun projectile-add-unignored (project vcs files)
  "This adds unignored files to FILES.

Useful because the VCS may not return ignored files at all.  In
this case unignored files will be absent from FILES."
  (let ((unignored-files (projectile-keep-ignored-files
                          project
                          vcs
                          (projectile-unignored-files-rel)))
        (unignored-paths (projectile-remove-ignored
                          (projectile-keep-ignored-directories
                           project
                           vcs
                           (projectile-unignored-directories-rel)))))
    (append files unignored-files unignored-paths)))

(defun projectile-buffers-with-file (buffers)
  "Return only those BUFFERS backed by files."
  (cl-remove-if-not (lambda (b) (buffer-file-name b)) buffers))

(defun projectile-buffers-with-file-or-process (buffers)
  "Return only those BUFFERS backed by files or processes."
  (cl-remove-if-not (lambda (b) (or (buffer-file-name b)
                                    (get-buffer-process b))) buffers))

(defun projectile-project-buffers (&optional project)
  "Get a list of a project's buffers.
If PROJECT is not specified the command acts on the current project."
  (let* ((project-root (or project (projectile-project-root)))
         (all-buffers (cl-remove-if-not
                       (lambda (buffer)
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

(defun projectile-project-buffer-files (&optional project)
  "Get a list of a project's buffer files.
If PROJECT is not specified the command acts on the current project."
  (let ((project-root (or project (projectile-project-root))))
    (mapcar
     (lambda (buffer)
       (file-relative-name
        (buffer-file-name buffer)
        project-root))
     (projectile-buffers-with-file
      (projectile-project-buffers project)))))

(defun projectile-project-buffer-p (buffer project-root)
  "Check if BUFFER is under PROJECT-ROOT."
  (with-current-buffer buffer
    (and (not (string-prefix-p " " (buffer-name buffer)))
         (not (projectile-ignored-buffer-p buffer))
         default-directory
         (string-equal (file-remote-p default-directory)
                       (file-remote-p project-root))
         (not (string-match-p "^http\\(s\\)?://" default-directory))
         (string-prefix-p project-root (file-truename default-directory) (eq system-type 'windows-nt)))))

(defun projectile-ignored-buffer-p (buffer)
  "Check if BUFFER should be ignored.

Regular expressions can be use."
  (or
   (with-current-buffer buffer
     (cl-some
      (lambda (name)
        (string-match-p name (buffer-name)))
      projectile-globally-ignored-buffers))
   (with-current-buffer buffer
     (cl-some
      (lambda (mode)
        (string-match-p (concat "^" mode "$")
                        (symbol-name major-mode)))
      projectile-globally-ignored-modes))))

(defun projectile-recently-active-files ()
  "Get list of recently active files.

Files are ordered by recently active buffers, and then recently
opened through use of recentf."
  (let ((project-buffer-files (projectile-project-buffer-files)))
    (append project-buffer-files
            (projectile-difference
             (projectile-recentf-files)
             project-buffer-files))))

(defun projectile-project-buffer-names ()
  "Get a list of project buffer names."
  (mapcar #'buffer-name (projectile-project-buffers)))

(defun projectile-prepend-project-name (string)
  "Prepend the current project's name to STRING."
  (format "[%s] %s" (projectile-project-name) string))

(defun projectile-read-buffer-to-switch (prompt)
  "Read the name of a buffer to switch to, prompting with PROMPT.

This function excludes the current buffer from the offered
choices."
  (projectile-completing-read
   prompt
   (delete (buffer-name (current-buffer))
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
(defun projectile-switch-to-buffer-other-frame ()
  "Switch to a project buffer and show it in another window."
  (interactive)
  (switch-to-buffer-other-frame
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
  (cl-remove-if-not
   (lambda (buffer)
     (not (get-buffer-window buffer 'visible)))
   (projectile-project-buffers)))

;;;###autoload
(defun projectile-multi-occur (&optional nlines)
  "Do a `multi-occur' in the project's buffers.
With a prefix argument, show NLINES of context."
  (interactive "P")
  (let ((project (projectile-ensure-project (projectile-project-root))))
    (multi-occur (projectile-project-buffers project)
                 (car (occur-read-primary-args))
                 nlines)))

(defun projectile-normalise-paths (patterns)
  "Remove leading `/' from the elements of PATTERNS."
  (delq nil (mapcar (lambda (pat) (and (string-prefix-p "/" pat)
                                       ;; remove the leading /
                                       (substring pat 1)))
                    patterns)))

(defun projectile-expand-paths (paths)
  "Expand the elements of PATHS.

Elements containing wildcards are expanded and spliced into the
resulting paths.  The returned PATHS are absolute, based on the
projectile project root."
  (let ((default-directory (projectile-project-root)))
    (projectile-flatten (mapcar
                         (lambda (pattern)
                           (or (file-expand-wildcards pattern t)
                               (projectile-expand-root pattern)))
                         paths))))

(defun projectile-normalise-patterns (patterns)
  "Remove paths from PATTERNS."
  (cl-remove-if (lambda (pat) (string-prefix-p "/" pat)) patterns))

(defun projectile-make-relative-to-root (files)
  "Make FILES relative to the project root."
  (let ((project-root (projectile-project-root)))
    (mapcar (lambda (f) (file-relative-name f project-root)) files)))

(defun projectile-ignored-directory-p (directory)
  "Check if DIRECTORY should be ignored.

Regular expressions can be used."
  (cl-some
   (lambda (name)
     (string-match-p name directory))
   (projectile-ignored-directories)))

(defun projectile-ignored-file-p (file)
  "Check if FILE should be ignored.

Regular expressions can be used."
  (cl-some
   (lambda (name)
     (string-match-p name file))
   (projectile-ignored-files)))

(defun projectile-check-pattern-p (file pattern)
  "Check if FILE meets PATTERN."
  (or (string-suffix-p (directory-file-name pattern)
                      (directory-file-name file))
     (member file (file-expand-wildcards pattern t))))

(defun projectile-ignored-rel-p (file directory patterns)
  "Check if FILE should be ignored relative to DIRECTORY
according to PATTERNS: (ignored . unignored)"
  (let ((default-directory directory))
    (and (cl-some
          (lambda (pat) (projectile-check-pattern-p file pat))
          (car patterns))
         (cl-notany
          (lambda (pat) (projectile-check-pattern-p file pat))
          (cdr patterns)))))

(defun projectile-ignored-files ()
  "Return list of ignored files."
  (projectile-difference
   (mapcar
    #'projectile-expand-root
    (append
     projectile-globally-ignored-files
     (projectile-project-ignored-files)))
   (projectile-unignored-files)))

(defun projectile-ignored-directories ()
  "Return list of ignored directories."
  (projectile-difference
   (mapcar
    #'file-name-as-directory
    (mapcar
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
  "Return list of project ignored files.
Unignored files are not included."
  (cl-remove-if 'file-directory-p (projectile-project-ignored)))

(defun projectile-project-ignored-directories ()
  "Return list of project ignored directories.
Unignored directories are not included."
  (cl-remove-if-not 'file-directory-p (projectile-project-ignored)))

(defun projectile-paths-to-ignore ()
  "Return a list of ignored project paths."
  (projectile-normalise-paths (nth 1 (projectile-parse-dirconfig-file))))

(defun projectile-patterns-to-ignore ()
  "Return a list of relative file patterns."
  (projectile-normalise-patterns (nth 1 (projectile-parse-dirconfig-file))))

(defun projectile-project-ignored ()
  "Return list of project ignored files/directories.
Unignored files/directories are not included."
  (let ((paths (projectile-paths-to-ignore)))
    (projectile-expand-paths paths)))

(defun projectile-unignored-files ()
  "Return list of unignored files."
  (mapcar
   #'projectile-expand-root
   (append
    projectile-globally-unignored-files
    (projectile-project-unignored-files))))

(defun projectile-unignored-directories ()
  "Return list of unignored directories."
  (mapcar
   #'file-name-as-directory
   (mapcar
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
  (cl-remove-if 'file-directory-p (projectile-project-unignored)))

(defun projectile-project-unignored-directories ()
  "Return list of project unignored directories."
  (cl-remove-if-not 'file-directory-p (projectile-project-unignored)))

(defun projectile-paths-to-ensure ()
  "Return a list of unignored project paths."
  (projectile-normalise-paths (nth 2 (projectile-parse-dirconfig-file))))

(defun projectile-files-to-ensure ()
  (projectile-flatten (mapcar (lambda (pat) (file-expand-wildcards pat t))
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
            (_ (push (buffer-substring (point) (line-end-position)) ignore)))
          (forward-line)))
      (list (mapcar (lambda (f) (file-name-as-directory (string-trim f)))
                    (delete "" (reverse keep)))
            (mapcar #'string-trim
                    (delete "" (reverse ignore)))
            (mapcar #'string-trim
                    (delete "" (reverse ensure)))))))

(defun projectile-expand-root (name)
  "Expand NAME to project root.

Never use on many files since it's going to recalculate the
project-root for every file."
  (expand-file-name name (projectile-project-root)))

(cl-defun projectile-completing-read (prompt choices &key initial-input action)
  "Present a project tailored PROMPT with CHOICES."
  (let ((prompt (projectile-prepend-project-name prompt))
        res)
    (setq res
          (cond
           ((eq projectile-completion-system 'ido)
            (ido-completing-read prompt choices nil nil initial-input))
           ((eq projectile-completion-system 'default)
            (completing-read prompt choices nil nil initial-input))
           ((eq projectile-completion-system 'helm)
            (if (and (fboundp 'helm)
                     (fboundp 'helm-make-source))
                (helm :sources
                      (helm-make-source "Projectile" 'helm-source-sync
                        :candidates choices
                        :action (if action
                                    (prog1 action
                                      (setq action nil))
                                  #'identity))
                      :prompt prompt
                      :input initial-input
                      :buffer "*helm-projectile*")
              (user-error "Please install helm from \
https://github.com/emacs-helm/helm")))
           ((eq projectile-completion-system 'ivy)
            (if (fboundp 'ivy-read)
                (ivy-read prompt choices
                          :initial-input initial-input
                          :action (prog1 action
                                    (setq action nil))
                          :caller 'projectile-completing-read)
              (user-error "Please install ivy from \
https://github.com/abo-abo/swiper")))
           (t (funcall projectile-completion-system prompt choices))))
    (if action
        (funcall action res)
      res)))

(defun projectile-project-files (project-root)
  "Return a list of files for the PROJECT-ROOT."
  (let (files)
    ;; If the cache is too stale, don't use it.
    (when projectile-files-cache-expire
      (let ((cache-time
             (gethash project-root projectile-projects-cache-time)))
        (when (or (null cache-time)
                  (< (+ cache-time projectile-files-cache-expire)
                     (projectile-time-seconds)))
          (remhash project-root projectile-projects-cache)
          (remhash project-root projectile-projects-cache-time))))

    ;; Use the cache, if requested and available.
    (when projectile-enable-caching
      (setq files (gethash project-root projectile-projects-cache)))

    ;; Calculate the list of files.
    (when (null files)
      (when projectile-enable-caching
        (message "Projectile is initializing cache..."))
      (setq files
            (if (eq projectile-indexing-method 'alien)
                ;; In alien mode we can just skip reading
                ;; .projectile and find all files in the root dir.
                (projectile-dir-files-alien project-root)
              ;; If a project is defined as a list of subfolders
              ;; then we'll have the files returned for each subfolder,
              ;; so they are relative to the project root.
              ;;
              ;; TODO: That's pretty slow and we need to improve it.
              ;; One options would be to pass explicitly the subdirs
              ;; to commands like `git ls-files` which would return
              ;; files paths relative to the project root.
              (cl-mapcan
               (lambda (dir)
                 (mapcar (lambda (f)
                           (file-relative-name (concat dir f)
                                               project-root))
                         (projectile-dir-files dir)))
               (projectile-get-project-directories project-root))))

      ;; Save the cached list.
      (when projectile-enable-caching
        (projectile-cache-project project-root files)))

    ;;; Sorting
    ;;
    ;; Files can't be cached in sorted order as some sorting schemes
    ;; require dynamic data.  Sorting is ignored completely when in
    ;; alien mode.
    (if (eq projectile-indexing-method 'alien)
        files
      (projectile-sort-files files))))

(defun projectile-current-project-files ()
  "Return a list of the files in the current project."
  (projectile-project-files (projectile-project-root)))

(defun projectile-process-current-project-files (action)
  "Process the current project's files using ACTION."
  (let ((project-files (projectile-current-project-files))
        (default-directory (projectile-project-root)))
    (dolist (filename project-files)
      (funcall action filename))))

(defun projectile-project-dirs (project)
  "Return a list of dirs for PROJECT."
  (delete-dups
   (delq nil
         (mapcar #'file-name-directory
                 (projectile-project-files project)))))

(defun projectile-current-project-dirs ()
  "Return a list of dirs for the current project."
  (projectile-project-dirs (projectile-ensure-project (projectile-project-root))))

;;; Interactive commands

(defun projectile--find-other-file (&optional flex-matching ff-variant)
  "Switch between files with the same name but different extensions.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable
`projectile-other-file-alist'.  With FF-VARIANT set to a defun, use that
instead of `find-file'.   A typical example of such a defun would be
`find-file-other-window' or `find-file-other-frame'"
  (let ((ff (or ff-variant #'find-file))
        (other-files (projectile-get-other-files
                      (buffer-file-name)
                      (projectile-current-project-files)
                      flex-matching)))
    (if other-files
        (let ((file-name (if (= (length other-files) 1)
                             (car other-files)
                           (projectile-completing-read "Switch to: "
                                                       other-files))))
          (funcall ff (expand-file-name file-name
                                        (projectile-project-root))))
      (error "No other file found"))))

;;;###autoload
(defun projectile-find-other-file (&optional flex-matching)
  "Switch between files with the same name but different extensions.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'."
  (interactive "P")
  (projectile--find-other-file flex-matching))

;;;###autoload
(defun projectile-find-other-file-other-window (&optional flex-matching)
  "Switch between files with the same name but different extensions in other window.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'."
  (interactive "P")
  (projectile--find-other-file flex-matching
                               #'find-file-other-window))

;;;###autoload
(defun projectile-find-other-file-other-frame (&optional flex-matching)
  "Switch between files with the same name but different extensions in other window.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'."
  (interactive "P")
  (projectile--find-other-file flex-matching
                               #'find-file-other-frame))

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
  (let (extensions-start)
    (substring file-name
               (if (setq extensions-start (string-match "\\..*" file-name 1))
                   (1+ extensions-start)
                 (length file-name)))))

(defun projectile-associated-file-name-extensions (file-name)
  "Return projectile-other-file-extensions associated to FILE-NAME's extensions.
If no associated other-file-extensions for the complete (nested) extension are found, remove subextensions from FILENAME's extensions until a match is found."
  (let ((current-extensions (projectile--file-name-extensions (file-name-nondirectory file-name)))
        associated-extensions)
    (catch 'break
      (while (not (string= "" current-extensions))
        (if (setq associated-extensions (cdr (assoc current-extensions projectile-other-file-alist)))
            (throw 'break associated-extensions))
        (setq current-extensions (projectile--file-name-extensions current-extensions))))))

(defun projectile-get-other-files (current-file project-file-list &optional flex-matching)
  "Narrow to files with the same names but different extensions.
Returns a list of possible files for users to choose.

With FLEX-MATCHING, match any file that contains the base name of current file"
  (let* ((file-ext-list (projectile-associated-file-name-extensions current-file))
         (fulldirname (if (file-name-directory current-file)
                          (file-name-directory current-file) "./"))
         (dirname (file-name-nondirectory (directory-file-name fulldirname)))
         (filename (regexp-quote (projectile--file-name-sans-extensions current-file)))
         (file-list (mapcar (lambda (ext)
                              (if flex-matching
                                  (concat ".*" filename ".*" "\." ext "\\'")
                                (concat "^" filename
                                        (unless (equal ext "")
                                          (concat "\." ext))
                                        "\\'")))
                            file-ext-list))
         (candidates (cl-remove-if-not
                      (lambda (project-file)
                        (string-match filename project-file))
                      project-file-list))
         (candidates
          (projectile-flatten (mapcar
                               (lambda (file)
                                 (cl-remove-if-not
                                  (lambda (project-file)
                                    (string-match file
                                                  (concat (file-name-base project-file)
                                                          (unless (equal (file-name-extension project-file) nil)
                                                            (concat "\." (file-name-extension project-file))))))
                                  candidates))
                               file-list)))
         (candidates
          (cl-remove-if-not (lambda (file) (not (backup-file-name-p file))) candidates))
         (candidates
          (cl-sort (copy-sequence candidates)
                   (lambda (file _)
                     (let ((candidate-dirname (file-name-nondirectory (directory-file-name (file-name-directory file)))))
                       (unless (equal fulldirname (file-name-directory file))
                         (equal dirname candidate-dirname)))))))
    candidates))

(defun projectile-select-files (project-files &optional invalidate-cache)
  "Select a list of files based on filename at point.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (projectile-maybe-invalidate-cache invalidate-cache)
  (let* ((file (if (region-active-p)
                   (buffer-substring (region-beginning) (region-end))
                 (or (thing-at-point 'filename) "")))
         (file (if (string-match "\\.?\\./" file)
                   (file-relative-name (file-truename file) (projectile-project-root))
                 file))
         (files (if file
                    (cl-remove-if-not
                     (lambda (project-file)
                       (string-match file project-file))
                     project-files)
                  nil)))
    files))

(defun projectile--find-file-dwim (invalidate-cache &optional ff-variant)
  "Jump to a project's files using completion based on context.

With a INVALIDATE-CACHE invalidates the cache first.

With FF-VARIANT set to a defun, use that instead of `find-file'.
A typical example of such a defun would be `find-file-other-window' or
`find-file-other-frame'

Subroutine for `projectile-find-file-dwim' and
`projectile-find-file-dwim-other-window'"
  (let* ((project-root (projectile-project-root))
         (project-files (projectile-project-files project-root))
         (files (projectile-select-files project-files invalidate-cache))
         (file (cond ((= (length files) 1)
                      (car files))
                     ((> (length files) 1)
                      (projectile-completing-read "Switch to: " files))
                     (t
                      (projectile-completing-read "Switch to: " project-files))))
         (ff (or ff-variant #'find-file)))
    (funcall ff (expand-file-name file project-root))
    (run-hooks 'projectile-find-file-hook)))

;;;###autoload
(defun projectile-find-file-dwim (&optional invalidate-cache)
  "Jump to a project's files using completion based on context.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim' still switches to \"projectile/projectile.el\" immediately
 because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file-dwim' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename like
 \"projectile/a\", a list of files with character 'a' in that directory is presented.

- If it finds nothing, display a list of all files in project for selecting."
  (interactive "P")
  (projectile--find-file-dwim invalidate-cache))

;;;###autoload
(defun projectile-find-file-dwim-other-window (&optional invalidate-cache)
  "Jump to a project's files using completion based on context in other window.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-window' still switches to \"projectile/projectile.el\"
immediately because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file-dwim-other-window' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename
like \"projectile/a\", a list of files with character 'a' in that directory
is presented.

- If it finds nothing, display a list of all files in project for selecting."
  (interactive "P")
  (projectile--find-file-dwim invalidate-cache #'find-file-other-window))

;;;###autoload
(defun projectile-find-file-dwim-other-frame (&optional invalidate-cache)
  "Jump to a project's files using completion based on context in other frame.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-frame' still switches to \"projectile/projectile.el\"
immediately because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file-dwim-other-frame' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename
like \"projectile/a\", a list of files with character 'a' in that directory
is presented.

- If it finds nothing, display a list of all files in project for selecting."
  (interactive "P")
  (projectile--find-file-dwim invalidate-cache #'find-file-other-frame))

(defun projectile--find-file (invalidate-cache &optional ff-variant)
  "Jump to a project's file using completion.
With INVALIDATE-CACHE invalidates the cache first.  With FF-VARIANT set to a
defun, use that instead of `find-file'.   A typical example of such a defun
would be `find-file-other-window' or `find-file-other-frame'"
  (interactive "P")
  (projectile-maybe-invalidate-cache invalidate-cache)
  (let* ((project-root (projectile-ensure-project (projectile-project-root)))
         (file (projectile-completing-read "Find file: "
                                          (projectile-project-files project-root)))
         (ff (or ff-variant #'find-file)))
    (when file
      (funcall ff (expand-file-name file project-root))
      (run-hooks 'projectile-find-file-hook))))

;;;###autoload
(defun projectile-find-file (&optional invalidate-cache)
  "Jump to a project's file using completion.
With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile--find-file invalidate-cache))

;;;###autoload
(defun projectile-find-file-other-window (&optional invalidate-cache)
  "Jump to a project's file using completion and show it in another window.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile--find-file invalidate-cache #'find-file-other-window))

;;;###autoload
(defun projectile-find-file-other-frame (&optional invalidate-cache)
  "Jump to a project's file using completion and show it in another frame.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile--find-file invalidate-cache #'find-file-other-frame))

;;;###autoload
(defun projectile-toggle-project-read-only ()
  "Toggle project read only."
  (interactive)
  (let ((inhibit-read-only t)
        (val (not buffer-read-only))
        (default-directory (projectile-ensure-project (projectile-project-root))))
    (add-dir-local-variable nil 'buffer-read-only val)
    (save-buffer)
    (kill-buffer)
    (when buffer-file-name
      (read-only-mode (if val +1 -1))
      (message "[%s] read-only-mode is %s" (projectile-project-name) (if val "on" "off")))))


;;;; Sorting project files
(defun projectile-sort-files (files)
  "Sort FILES according to `projectile-sort-order'."
  (cl-case projectile-sort-order
    (default files)
    (recentf (projectile-sort-by-recentf-first files))
    (recently-active (projectile-sort-by-recently-active-first files))
    (modification-time (projectile-sort-by-modification-time files))
    (access-time (projectile-sort-by-access-time files))))

(defun projectile-sort-by-recentf-first (files)
  "Sort FILES by a recent first scheme."
  (let ((project-recentf-files (projectile-recentf-files)))
    (append project-recentf-files
            (projectile-difference files project-recentf-files))))

(defun projectile-sort-by-recently-active-first (files)
  "Sort FILES by most recently active buffers or opened files."
  (let ((project-recently-active-files (projectile-recently-active-files)))
    (append project-recently-active-files
            (projectile-difference files project-recently-active-files))))

(defun projectile-sort-by-modification-time (files)
  "Sort FILES by modification time."
  (let ((default-directory (projectile-project-root)))
    (cl-sort
     (copy-sequence files)
     (lambda (file1 file2)
       (let ((file1-mtime (nth 5 (file-attributes file1)))
             (file2-mtime (nth 5 (file-attributes file2))))
         (not (time-less-p file1-mtime file2-mtime)))))))

(defun projectile-sort-by-access-time (files)
  "Sort FILES by access time."
  (let ((default-directory (projectile-project-root)))
    (cl-sort
     (copy-sequence files)
     (lambda (file1 file2)
       (let ((file1-atime (nth 4 (file-attributes file1)))
             (file2-atime (nth 4 (file-attributes file2))))
         (not (time-less-p file1-atime file2-atime)))))))


;;;; Find directory in project functionality
(defun projectile--find-dir (invalidate-cache &optional dired-variant)
  "Jump to a project's directory using completion.

With INVALIDATE-CACHE invalidates the cache first.  With DIRED-VARIANT set to a
defun, use that instead of `dired'.  A typical example of such a defun would be
`dired-other-window' or `dired-other-frame'"
  (projectile-maybe-invalidate-cache invalidate-cache)
  (let* ((project (projectile-ensure-project (projectile-project-root)))
         (dir (projectile-complete-dir project))
         (dired-v (or dired-variant #'dired)))
    (funcall dired-v (expand-file-name dir project))
    (run-hooks 'projectile-find-dir-hook)))

;;;###autoload
(defun projectile-find-dir (&optional invalidate-cache)
  "Jump to a project's directory using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile--find-dir invalidate-cache))

;;;###autoload
(defun projectile-find-dir-other-window (&optional invalidate-cache)
  "Jump to a project's directory in other window using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile--find-dir invalidate-cache #'dired-other-window))

;;;###autoload
(defun projectile-find-dir-other-frame (&optional invalidate-cache)
  "Jump to a project's directory in other window using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile--find-dir invalidate-cache #'dired-other-frame))

(defun projectile-complete-dir (project)
  (let ((project-dirs (projectile-project-dirs project)))
    (projectile-completing-read
    "Find dir: "
    (if projectile-find-dir-includes-top-level
        (append '("./") project-dirs)
      project-dirs))))

;;;###autoload
(defun projectile-find-test-file (&optional invalidate-cache)
  "Jump to a project's test file using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache invalidate-cache)
  (let ((file (projectile-completing-read "Find test file: "
                                          (projectile-current-project-test-files))))
    (find-file (expand-file-name file (projectile-project-root)))))

(defun projectile-test-files (files)
  "Return only the test FILES."
  (cl-remove-if-not 'projectile-test-file-p files))

(defun projectile-test-file-p (file)
  "Check if FILE is a test file."
  (or (cl-some (lambda (pat) (string-prefix-p pat (file-name-nondirectory file)))
               (delq nil (list (funcall projectile-test-prefix-function (projectile-project-type)))))
      (cl-some (lambda (pat) (string-suffix-p pat (file-name-sans-extension (file-name-nondirectory file))))
               (delq nil (list (funcall projectile-test-suffix-function (projectile-project-type)))))))

(defun projectile-current-project-test-files ()
  "Return a list of test files for the current project."
  (projectile-test-files (projectile-current-project-files)))

(defvar projectile-project-types nil
  "An alist holding all project types that are known to Projectile.
The project types are symbols and they are linked to plists holding
the properties of the various project types.")

(cl-defun projectile-register-project-type
    (project-type marker-files &key compilation-dir configure compile test run test-suffix test-prefix src-dir test-dir)
  "Register a project type with projectile.

A project type is defined by PROJECT-TYPE, a set of MARKER-FILES,
and optional keyword arguments:
COMPILATION-DIR the directory to run the tests- and compilations in,
CONFIGURE which specifies a command that configures the project
          `%s' in the command will be substituted with (projectile-project-root)
          before the command is run,
COMPILE which specifies a command that builds the project,
TEST which specified a command that tests the project,
RUN which specifies a command that runs the project,
TEST-SUFFIX which specifies test file suffix, and
TEST-PREFIX which specifies test file prefix.
SRC-DIR which specifies the path to the source relative to the project root.
TEST-DIR which specifies the path to the tests relative to the project root."
  (let ((project-plist (list 'marker-files marker-files
                             'compilation-dir compilation-dir
                             'configure-command configure
                             'compile-command compile
                             'test-command test
                             'run-command run)))
    ;; There is no way for the function to distinguish between an
    ;; explicit argument of nil and an omitted argument. However, the
    ;; body of the function is free to consider nil an abbreviation
    ;; for some other meaningful value
    (when test-suffix
      (plist-put project-plist 'test-suffix test-suffix))
    (when test-prefix
      (plist-put project-plist 'test-prefix test-prefix))
    (when src-dir
      (plist-put project-plist 'src-dir src-dir))
    (when test-dir
      (plist-put project-plist 'test-dir test-dir))
    (setq projectile-project-types
          (cons `(,project-type . ,project-plist)
                projectile-project-types))))

(defun projectile-cabal-project-p ()
  "Check if a project contains *.cabal files but no stack.yaml file."
  (and (projectile-verify-file-wildcard "*.cabal")
       (not (projectile-verify-file "stack.yaml"))))

(defun projectile-go-project-p ()
  "Check if a project contains Go source files."
  (projectile-verify-file-wildcard "*.go"))

(define-obsolete-variable-alias 'projectile-go-function 'projectile-go-project-test-function "1.0.0")
(defcustom projectile-go-project-test-function #'projectile-go-project-p
  "Function to determine if project's type is go."
  :group 'projectile
  :type 'function)

;;; Project type registration
;;
;; Project type detection happens in a reverse order with respect to
;; project type registration (invocations of `projectile-register-project-type').
;;
;; As function-based project type detection is pretty slow, so it
;; should be tried at the end if everything else failed (meaning here
;; it should be listed first).
;;
;; Ideally common project types should be checked earlier than exotic ones.

;; Function-based detection project type
(projectile-register-project-type 'haskell-cabal #'projectile-cabal-project-p
                                  :compile "cabal build"
                                  :test "cabal test"
                                  :test-suffix "Spec")
(projectile-register-project-type 'go projectile-go-project-test-function
                                  :compile "go build ./..."
                                  :test "go test ./..."
                                  :test-suffix "_test")
;; File-based detection project types

;; Universal
(projectile-register-project-type 'scons '("SConstruct")
                                  :compile "scons"
                                  :test "scons test"
                                  :test-suffix "test")
(projectile-register-project-type 'meson '("meson.build")
                                  :compilation-dir "build"
                                  :configure "meson %s"
                                  :compile "ninja"
                                  :test "ninja test")
(projectile-register-project-type 'nix '("default.nix")
                                  :compile "nix-build"
                                  :test "nix-build")
;; Make & CMake
(projectile-register-project-type 'make '("Makefile")
                                  :compile "make"
                                  :test "make test")
(projectile-register-project-type 'cmake '("CMakeLists.txt")
                                  :configure "cmake %s"
                                  :compile "cmake --build ."
                                  :test "ctest")
;; PHP
(projectile-register-project-type 'php-symfony '("composer.json" "app" "src" "vendor")
                                  :compile "app/console server:run"
                                  :test "phpunit -c app "
                                  :test-suffix "Test")
;; Erlang & Elixir
(projectile-register-project-type 'rebar '("rebar.config")
                                  :compile "rebar"
                                  :test "rebar eunit"
                                  :test-suffix "_SUITE")
(projectile-register-project-type 'elixir '("mix.exs")
                                  :compile "mix compile"
                                  :src-dir "lib/"
                                  :test "mix test"
                                  :test-suffix "_test")
;; JavaScript
(projectile-register-project-type 'grunt '("Gruntfile.js")
                                  :compile "grunt"
                                  :test "grunt test")
(projectile-register-project-type 'gulp '("gulpfile.js")
                                  :compile "gulp"
                                  :test "gulp test")
(projectile-register-project-type 'npm '("package.json")
                                  :compile "npm install"
                                  :test "npm test"
                                  :test-suffix ".test")
;; Angular
(projectile-register-project-type 'angular '("angular.json" ".angular-cli.json")
                                  :compile "ng build"
                                  :run "ng serve"
                                  :test "ng test")
;; Python
(projectile-register-project-type 'django '("manage.py")
                                  :compile "python manage.py runserver"
                                  :test "python manage.py test"
                                  :test-prefix "test_"
                                  :test-suffix"_test")
(projectile-register-project-type 'python-pip '("requirements.txt")
                                  :compile "python setup.by build"
                                  :test "python -m unittest discover"
                                  :test-prefix "test_"
                                  :test-suffix"_test")
(projectile-register-project-type 'python-pkg '("setup.py")
                                  :compile "python setup.py build"
                                  :test "python -m unittest discover"
                                  :test-prefix "test_"
                                  :test-suffix"_test")
(projectile-register-project-type 'python-tox '("tox.ini")
                                  :compile "tox -r --notest"
                                  :test "tox"
                                  :test-prefix "test_"
                                  :test-suffix"_test")
(projectile-register-project-type 'python-pipenv '("Pipfile")
                                  :compile "pipenv run build"
                                  :test "pipenv run test"
                                  :test-prefix "test_"
                                  :test-suffix "_test")
;; Java & friends
(projectile-register-project-type 'maven '("pom.xml")
                                  :compile "mvn clean install"
                                  :test "mvn test"
                                  :test-suffix "Test"
                                  :src-dir "main/src/"
                                  :test-dir "main/test/")
(projectile-register-project-type 'gradle '("build.gradle")
                                  :compile "gradle build"
                                  :test "gradle test"
                                  :test-suffix "Spec")
(projectile-register-project-type 'gradlew '("gradlew")
                                  :compile "./gradlew build"
                                  :test "./gradlew test"
                                  :test-suffix "Spec")
(projectile-register-project-type 'grails '("application.properties" "grails-app")
                                  :compile "grails package"
                                  :test "grails test-app"
                                  :test-suffix "Spec")
(projectile-register-project-type 'sbt '("build.sbt")
                                  :compile "sbt compile"
                                  :test "sbt test"
                                  :test-suffix "Spec")
(projectile-register-project-type 'lein-test '("project.clj")
                                  :compile "lein compile"
                                  :test "lein test"
                                  :test-suffix "_test")
(projectile-register-project-type 'lein-midje '("project.clj" ".midje.clj")
                                  :compile "lein compile"
                                  :test "lein midje"
                                  :test-prefix "t_")
(projectile-register-project-type 'boot-clj '("build.boot")
                                  :compile "boot aot"
                                  :test "boot test"
                                  :test-suffix "_test")
(projectile-register-project-type 'clojure-cli '("deps.edn")
                                  :test-suffix "_test")
;; Ruby
(projectile-register-project-type 'ruby-rspec '("Gemfile" "lib" "spec")
                                  :compile "bundle exec rake"
                                  :src-dir "lib/"
                                  :test "bundle exec rspec"
                                  :test-dir "spec/"
                                  :test-suffix "_spec")
(projectile-register-project-type 'ruby-test '("Gemfile" "lib" "test")
                                  :compile"bundle exec rake"
                                  :src-dir "lib/"
                                  :test "bundle exec rake test"
                                  :test-suffix "_test")
;; Rails needs to be registered after npm, otherwise `package.json` makes it `npm`.
;; https://github.com/bbatsov/projectile/pull/1191
(projectile-register-project-type 'rails-test '("Gemfile" "app" "lib" "db" "config" "test")
                                  :compile "bundle exec rails server"
                                  :src-dir "lib/"
                                  :test "bundle exec rake test"
                                  :test-suffix "_test")
(projectile-register-project-type 'rails-rspec '("Gemfile" "app" "lib" "db" "config" "spec")
                                  :compile "bundle exec rails server"
                                  :src-dir "lib/"
                                  :test "bundle exec rspec"
                                  :test-dir "spec/"
                                  :test-suffix "_spec")
;; Crystal
(projectile-register-project-type 'crystal-spec '("shard.yml")
                                  :src-dir "src/"
                                  :test "crystal spec"
                                  :test-dir "spec/"
                                  :test-suffix "_spec")

;; Emacs
(projectile-register-project-type 'emacs-cask '("Cask")
                                  :compile "cask install"
                                  :test-prefix "test-"
                                  :test-suffix "-test")

;; R
(projectile-register-project-type 'r '("DESCRIPTION")
                                  :compile "R CMD INSTALL --with-keep.source ."
                                  :test (concat "R CMD check -o " temporary-file-directory " ."))

;; Haskell
(projectile-register-project-type 'haskell-stack '("stack.yaml")
                                  :compile "stack build"
                                  :test "stack build --test"
                                  :test-suffix "Spec")

;; Rust
(projectile-register-project-type 'rust-cargo '("Cargo.toml")
                                  :compile "cargo build"
                                  :test "cargo test")

;; Racket
(projectile-register-project-type 'racket '("info.rkt")
                                  :test "raco test .")


(defvar-local projectile-project-type nil
  "Buffer local var for overriding the auto-detected project type.
Normally you'd set this from .dir-locals.el.")
(put 'projectile-project-type 'safe-local-variable #'symbolp)

(defun projectile-detect-project-type ()
  "Detect the type of the current project.
Fallsback to a generic project type when the type can't be determined."
  (let ((project-type
         (or (car (cl-find-if
                   (lambda (project-type-record)
                     (let ((project-type (car project-type-record))
                           (marker (plist-get (cdr project-type-record) 'marker-files)))
                       (if (listp marker)
                           (and (projectile-verify-files marker) project-type)
                         (and (funcall marker) project-type))))
                   projectile-project-types))
             'generic)))
    (puthash (projectile-project-root) project-type projectile-project-type-cache)
    project-type))

(defun projectile-project-type (&optional dir)
  "Determine a project's type based on its structure.
When DIR is specified it checks it, otherwise it acts
on the current project.

The project type is cached for improved performance."
  (if projectile-project-type
      projectile-project-type
    (let* ((dir (or dir default-directory))
           (project-root (projectile-project-root dir)))
      (if project-root
          (or (gethash project-root projectile-project-type-cache)
              (projectile-detect-project-type))
        ;; if we're not in a project we just return nil
        nil))))

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
  (cl-every #'projectile-verify-file files))

(defun projectile-verify-file (file)
  "Check whether FILE exists in the current project."
  (file-exists-p (projectile-expand-root file)))

(defun projectile-verify-file-wildcard (file)
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
   ((projectile-file-exists-p (expand-file-name ".fslckout" project-root)) 'fossil)
   ((projectile-file-exists-p (expand-file-name "_FOSSIL_" project-root)) 'fossil)
   ((projectile-file-exists-p (expand-file-name ".bzr" project-root)) 'bzr)
   ((projectile-file-exists-p (expand-file-name "_darcs" project-root)) 'darcs)
   ((projectile-file-exists-p (expand-file-name ".svn" project-root)) 'svn)
   ((projectile-locate-dominating-file project-root ".git") 'git)
   ((projectile-locate-dominating-file project-root ".hg") 'hg)
   ((projectile-locate-dominating-file project-root ".fslckout") 'fossil)
   ((projectile-locate-dominating-file project-root "_FOSSIL_") 'fossil)
   ((projectile-locate-dominating-file project-root ".bzr") 'bzr)
   ((projectile-locate-dominating-file project-root "_darcs") 'darcs)
   ((projectile-locate-dominating-file project-root ".svn") 'svn)
   (t 'none)))

(defun projectile--test-name-for-impl-name (impl-file-path)
  "Determine the name of the test file for IMPL-FILE-PATH."
  (let* ((project-type (projectile-project-type))
         (impl-file-name (file-name-sans-extension (file-name-nondirectory impl-file-path)))
         (impl-file-ext (file-name-extension impl-file-path))
         (test-prefix (funcall projectile-test-prefix-function project-type))
         (test-suffix (funcall projectile-test-suffix-function project-type)))
    (cond
     (test-prefix (concat test-prefix impl-file-name "." impl-file-ext))
     (test-suffix (concat impl-file-name test-suffix "." impl-file-ext))
     (t (error "Project type `%s' not supported!" project-type)))))

(defun projectile-create-test-file-for (impl-file-path)
  "Create a test file for IMPL-FILE-PATH."
  (let* ((test-file (projectile--test-name-for-impl-name impl-file-path))
         (project-root (projectile-project-root))
         (relative-dir (file-name-directory (file-relative-name impl-file-path project-root)))
         (src-dir-name (projectile-src-directory (projectile-project-type)))
         (test-dir-name (projectile-test-directory (projectile-project-type)))
         (test-dir (expand-file-name (replace-regexp-in-string src-dir-name test-dir-name relative-dir) project-root))
         (test-path (expand-file-name test-file test-dir)))
    (unless (file-exists-p test-path)
      (progn (unless (file-exists-p test-dir)
               (make-directory test-dir :create-parents))
             test-path))))

(defun projectile-find-implementation-or-test (file-name)
  "Given a FILE-NAME return the matching implementation or test filename.

If `projectile-create-missing-test-files' is non-nil, create the missing
test file."
  (unless file-name (error "The current buffer is not visiting a file"))
  (if (projectile-test-file-p file-name)
      ;; find the matching impl file
      (let ((impl-file (projectile-find-matching-file file-name)))
        (if impl-file
            (projectile-expand-root impl-file)
          (error
           "No matching source file found for project type `%s'"
           (projectile-project-type))))
    ;; find the matching test file
    (let ((test-file (projectile-find-matching-test file-name)))
      (if test-file
          (projectile-expand-root test-file)
        (if projectile-create-missing-test-files
            (projectile-create-test-file-for file-name)
          (error "No matching test file found for project type `%s'"
                 (projectile-project-type)))))))

;;;###autoload
(defun projectile-find-implementation-or-test-other-window ()
  "Open matching implementation or test file in other window."
  (interactive)
  (find-file-other-window
   (projectile-find-implementation-or-test (buffer-file-name))))

;;;###autoload
(defun projectile-find-implementation-or-test-other-frame ()
  "Open matching implementation or test file in other frame."
  (interactive)
  (find-file-other-frame
   (projectile-find-implementation-or-test (buffer-file-name))))

;;;###autoload
(defun projectile-toggle-between-implementation-and-test ()
  "Toggle between an implementation file and its test file."
  (interactive)
  (find-file
   (projectile-find-implementation-or-test (buffer-file-name))))


(defun projectile-project-type-attribute (project-type key &optional default-value)
  "Return the value of some PROJECT-TYPE attribute identified by KEY.
Fallback to DEFAULT-VALUE for missing attributes."
  (let ((project (alist-get project-type projectile-project-types)))
    (if (and project (plist-member project key))
        (plist-get project key)
      default-value)))

(defun projectile-test-prefix (project-type)
  "Find default test files prefix based on PROJECT-TYPE."
  (projectile-project-type-attribute project-type 'test-prefix))

(defun projectile-test-suffix (project-type)
  "Find default test files suffix based on PROJECT-TYPE."
  (projectile-project-type-attribute project-type 'test-suffix))

(defun projectile-src-directory (project-type)
  "Find default src directory based on PROJECT-TYPE."
  (projectile-project-type-attribute project-type 'src-dir "src/"))

(defun projectile-test-directory (project-type)
  "Find default test directory based on PROJECT-TYPE."
  (projectile-project-type-attribute project-type 'test-dir "test/"))

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
  (cl-sort (copy-sequence
            (let (value result)
              (while (setq value (pop candidates))
                (let* ((key (projectile-dirname-matching-count file value))
                       (kv (assoc key result)))
                  (if kv
                      (setcdr kv (cons value (cdr kv)))
                    (push (list key value) result))))
              (mapcar (lambda (x)
                        (cons (car x) (nreverse (cdr x))))
                      (nreverse result))))
           (lambda (a b) (> (car a) (car b)))))

(defun projectile-find-matching-test (file)
  "Compute the name of the test matching FILE."
  (let* ((basename (file-name-nondirectory (file-name-sans-extension file)))
         (test-prefix (funcall projectile-test-prefix-function (projectile-project-type)))
         (test-suffix (funcall projectile-test-suffix-function (projectile-project-type)))
         (candidates
          (cl-remove-if-not
           (lambda (current-file)
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
              (car (last (car grouped-candidates)))
            (projectile-completing-read
             "Switch to: "
             (apply 'append (mapcar 'cdr grouped-candidates)))))))))

(defun projectile-find-matching-file (test-file)
  "Compute the name of a file matching TEST-FILE."
  (let* ((basename (file-name-nondirectory (file-name-sans-extension test-file)))
         (test-prefix (funcall projectile-test-prefix-function (projectile-project-type)))
         (test-suffix (funcall projectile-test-suffix-function (projectile-project-type)))
         (candidates
          (cl-remove-if-not
           (lambda (current-file)
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
              (car (last (car grouped-candidates)))
            (projectile-completing-read
             "Switch to: "
             (apply 'append (mapcar 'cdr grouped-candidates)))))))))

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
  (mapcar (lambda (pat) (concat "*" pat)) projectile-globally-ignored-file-suffixes))

(defun projectile--read-search-string-with-default (prefix-label)
  (let* ((prefix-label (projectile-prepend-project-name prefix-label))
         (default-value (projectile-symbol-or-selection-at-point))
         (default-label (if (or (not default-value)
                                (string= default-value ""))
                            ""
                          (format " (default %s)" default-value))))
    (read-string (format "%s%s: " prefix-label default-label) nil nil default-value)))

;;;###autoload
(defun projectile-grep (&optional regexp arg)
  "Perform rgrep in the project.

With a prefix ARG asks for files (globbing-aware) which to grep in.
With prefix ARG of `-' (such as `M--'), default the files (without prompt),
to `projectile-grep-default-files'.

With REGEXP given, don't query the user for a regexp."
  (interactive "i\nP")
  (require 'grep) ;; for `rgrep'
  (let* ((roots (projectile-get-project-directories (projectile-project-root)))
         (search-regexp (or regexp
                            (projectile--read-search-string-with-default "Grep for")))
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
               (cl-union (mapcar (lambda (f) (directory-file-name (file-relative-name f root-dir)))
                                 (projectile-ignored-directories))
                         grep-find-ignored-directories))
              (grep-find-ignored-files
               (cl-union (append (mapcar (lambda (file)
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
   (list (projectile--read-search-string-with-default
          (format "Ag %ssearch for" (if current-prefix-arg "regexp " "")))
         current-prefix-arg))
  (if (require 'ag nil 'noerror)
      (let ((ag-command (if arg 'ag-regexp 'ag))
            (ag-ignore-list (delq nil
                                  (delete-dups
                                   (append
                                    ag-ignore-list
                                    (projectile--globally-ignored-file-suffixes-glob)
                                    ;; ag supports git ignore files directly
                                    (unless (eq (projectile-project-vcs) 'git)
                                      (append (projectile-ignored-files-rel)
                                              (projectile-ignored-directories-rel)
                                              grep-find-ignored-files
                                              grep-find-ignored-directories
                                              '()))))))
            ;; reset the prefix arg, otherwise it will affect the ag-command
            (current-prefix-arg nil))
        (funcall ag-command search-term (projectile-project-root)))
    (error "Package 'ag' is not available")))

;;;###autoload
(defun projectile-ripgrep (search-term)
  "Run a Ripgrep search with `SEARCH-TERM' at current project root.

SEARCH-TERM is a regexp."
  (interactive (list (projectile--read-search-string-with-default
                      "Ripgrep search for")))
  (if (require 'ripgrep nil 'noerror)
      (let ((args (mapcar (lambda (val) (concat "--glob !" val))
                          (append projectile-globally-ignored-files
                                  projectile-globally-ignored-directories))))
        (ripgrep-regexp search-term
                        (projectile-project-root)
                        (if current-prefix-arg
                            args
                          (cons "--fixed-strings" args))))
    (error "Package `ripgrep' is not available")))

(defun projectile-tags-exclude-patterns ()
  "Return a string with exclude patterns for ctags."
  (mapconcat (lambda (pattern) (format "--exclude=\"%s\""
                                       (directory-file-name pattern)))
             (projectile-ignored-directories-rel) " "))

;;;###autoload
(defun projectile-regenerate-tags ()
  "Regenerate the project's [e|g]tags."
  (interactive)
  (if (and (boundp 'ggtags-mode)
           (memq projectile-tags-backend '(auto ggtags)))
      (progn
        (let* ((ggtags-project-root (projectile-project-root))
               (default-directory ggtags-project-root))
          (ggtags-ensure-project)
          (ggtags-update-tags t)))
    (let* ((project-root (projectile-project-root))
           (tags-exclude (projectile-tags-exclude-patterns))
           (default-directory project-root)
           (tags-file (expand-file-name projectile-tags-file-name))
           (command (format projectile-tags-command tags-file tags-exclude default-directory))
           shell-output exit-code)
      (with-temp-buffer
        (setq exit-code
              (call-process-shell-command command nil (current-buffer))
              shell-output (string-trim
                            (buffer-substring (point-min) (point-max)))))
      (unless (zerop exit-code)
        (error shell-output))
      (visit-tags-table tags-file)
      (message "Regenerated %s" tags-file))))

(defun projectile-visit-project-tags-table ()
  "Visit the current project's tags table."
  (when (projectile-project-p)
    (let ((tags-file (projectile-expand-root projectile-tags-file-name)))
      (when (file-exists-p tags-file)
        (with-demoted-errors "Error loading tags-file: %s"
          (visit-tags-table tags-file t))))))

(defun projectile-determine-find-tag-fn ()
  "Determine which function to use for a call to `projectile-find-tag'."
  (or
   (cond
    ((eq projectile-tags-backend 'auto)
     (cond
      ((fboundp 'ggtags-find-tag-dwim)
       'ggtags-find-tag-dwim)
      ((fboundp 'xref-find-definitions)
       'xref-find-definitions)
      ((fboundp 'etags-select-find-tag)
       'etags-select-find-tag)))
    ((eq projectile-tags-backend 'xref)
     (when (fboundp 'xref-find-definitions)
       'xref-find-definitions))
    ((eq projectile-tags-backend 'ggtags)
     (when (fboundp 'ggtags-find-tag-dwim)
       'ggtags-find-tag-dwim))
    ((eq projectile-tags-backend 'etags-select)
     (when (fboundp 'etags-select-find-tag)
       'etags-select-find-tag)))
   'find-tag))

;;;###autoload
(defun projectile-find-tag ()
  "Find tag in project."
  (interactive)
  (projectile-visit-project-tags-table)
  ;; Auto-discover the user's preference for tags
  (let ((find-tag-fn (projectile-determine-find-tag-fn)))
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
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
    (call-interactively 'execute-extended-command)))

;;;###autoload
(defun projectile-run-shell-command-in-root ()
  "Invoke `shell-command' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
    (call-interactively 'shell-command)))

;;;###autoload
(defun projectile-run-async-shell-command-in-root ()
  "Invoke `async-shell-command' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
    (call-interactively 'async-shell-command)))

;;;###autoload
(defun projectile-run-shell ()
  "Invoke `shell' in the project's root.

Switch to the project specific shell buffer if it already exists."
  (interactive)
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
    (shell (concat "*shell " (projectile-project-name) "*"))))

;;;###autoload
(defun projectile-run-eshell ()
  "Invoke `eshell' in the project's root.

Switch to the project specific eshell buffer if it already exists."
  (interactive)
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
    (let ((eshell-buffer-name (concat "*eshell " (projectile-project-name) "*")))
      (eshell))))

;;;###autoload
(defun projectile-run-ielm ()
  "Invoke `ielm' in the project's root.

Switch to the project specific ielm buffer if it already exists."
  (interactive)
  (let* ((project (projectile-ensure-project (projectile-project-root)))
         (ielm-buffer-name (format "*ielm %s*" (projectile-project-name project))))
    (if (get-buffer ielm-buffer-name)
        (switch-to-buffer ielm-buffer-name)
      (projectile-with-default-dir project
        (ielm))
      ;; ielm's buffer name is hardcoded, so we have to rename it after creation
      (rename-buffer ielm-buffer-name))))

;;;###autoload
(defun projectile-run-term (program)
  "Invoke `term' in the project's root.

Switch to the project specific term buffer if it already exists."
  (interactive (list nil))
  (let* ((project (projectile-ensure-project (projectile-project-root)))
         (term (concat "term " (projectile-project-name project)))
         (buffer (concat "*" term "*")))
    (unless (get-buffer buffer)
      (require 'term)
      (let ((program (or program
                         (read-from-minibuffer "Run program: "
                                               (or explicit-shell-file-name
                                                   (getenv "ESHELL")
                                                   (getenv "SHELL")
                                                   "/bin/sh")))))
        (projectile-with-default-dir project
          (set-buffer (make-term term program))
          (term-mode)
          (term-char-mode))))
    (switch-to-buffer buffer)))

(defun projectile-files-in-project-directory (directory)
  "Return a list of files in DIRECTORY."
  (let* ((project (projectile-ensure-project (projectile-project-root)))
         (dir (file-relative-name (expand-file-name directory)
                                  project)))
    (cl-remove-if-not
     (lambda (f) (string-prefix-p dir f))
     (projectile-project-files project))))

(defun projectile-files-from-cmd (cmd directory)
  "Use a grep-like CMD to search for files within DIRECTORY.

CMD should include the necessary search params and should output
equivalently to grep -HlI (only unique matching filenames).
Returns a list of expanded filenames."
  (let ((default-directory directory))
    (mapcar (lambda (str)
              (concat directory
                      (if (string-prefix-p "./" str)
                          (substring str 2)
                        str)))
            (split-string
             (string-trim (shell-command-to-string cmd))
             "\n+"
             t))))

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
    (cl-remove-if
     #'file-directory-p
     (mapcar #'projectile-expand-root (projectile-dir-files directory)))))

;;;###autoload
(defun projectile-replace (&optional arg)
  "Replace literal string in project using non-regexp `tags-query-replace'.

With a prefix argument ARG prompts you for a directory on which
to run the replacement."
  (interactive "P")
  (let* ((directory (if arg
                        (file-name-as-directory
                         (read-directory-name "Replace in directory: "))
                      (projectile-ensure-project (projectile-project-root))))
         (old-text (read-string
                    (projectile-prepend-project-name "Replace: ")
                    (projectile-symbol-or-selection-at-point)))
         (new-text (read-string
                    (projectile-prepend-project-name
                     (format "Replace %s with: " old-text))))
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
  (let* ((directory (if arg
                        (file-name-as-directory
                         (read-directory-name "Replace regexp in directory: "))
                      (projectile-ensure-project (projectile-project-root))))
         (old-text (read-string
                    (projectile-prepend-project-name "Replace regexp: ")
                    (projectile-symbol-or-selection-at-point)))
         (new-text (read-string
                    (projectile-prepend-project-name
                     (format "Replace regexp %s with: " old-text))))
         (files
          ;; We have to reject directories as a workaround to work with git submodules.
          ;;
          ;; We can't narrow the list of files with
          ;; `projectile-files-with-string' because those regexp tools
          ;; don't support Emacs regular expressions.
          (cl-remove-if
           #'file-directory-p
           (mapcar #'projectile-expand-root (projectile-dir-files directory)))))
    (tags-query-replace old-text new-text nil (cons 'list files))))

;;;###autoload
(defun projectile-kill-buffers ()
  "Kill project buffers.

The buffer are killed according to the value of
`projectile-kill-buffers-filter'."
  (interactive)
  (let* ((project (projectile-ensure-project (projectile-project-root)))
         (project-name (projectile-project-name project))
         (buffers (projectile-project-buffers project)))
    (when (yes-or-no-p
           (format "Are you sure you want to kill %s buffers for '%s'? "
                   (length buffers) project-name))
      (dolist (buffer buffers)
        (when (and
               ;; we take care not to kill indirect buffers directly
               ;; as we might encounter them after their base buffers are killed
               (not (buffer-base-buffer buffer))
               (if (functionp projectile-kill-buffers-filter)
                   (funcall projectile-kill-buffers-filter buffer)
                 (pcase projectile-kill-buffers-filter
                   ('kill-all t)
                   ('kill-only-files (buffer-file-name buffer))
                   (_ (user-error "Invalid projectile-kill-buffers-filter value: %S" projectile-kill-buffers-filter)))))
          (kill-buffer buffer))))))

;;;###autoload
(defun projectile-save-project-buffers ()
  "Save all project buffers."
  (interactive)
  (let* ((project (projectile-ensure-project (projectile-project-root)))
         (project-name (projectile-project-name project))
         (modified-buffers (cl-remove-if-not (lambda (buf)
                                               (and (buffer-file-name buf)
                                                    (buffer-modified-p buf)))
                                             (projectile-project-buffers project))))
    (if (null modified-buffers)
        (message "[%s] No buffers need saving" project-name)
      (dolist (buf modified-buffers)
        (with-current-buffer buf
          (save-buffer)))
      (message "[%s] Saved %d buffers" project-name (length modified-buffers)))))

;;;###autoload
(defun projectile-dired ()
  "Open `dired' at the root of the project."
  (interactive)
  (dired (projectile-ensure-project (projectile-project-root))))

;;;###autoload
(defun projectile-dired-other-window ()
  "Open `dired'  at the root of the project in another window."
  (interactive)
  (dired-other-window (projectile-ensure-project (projectile-project-root))))

;;;###autoload
(defun projectile-dired-other-frame ()
  "Open `dired' at the root of the project in another frame."
  (interactive)
  (dired-other-frame (projectile-ensure-project (projectile-project-root))))

;;;###autoload
(defun projectile-vc (&optional project-root)
  "Open `vc-dir' at the root of the project.

For git projects `magit-status-internal' is used if available.
For hg projects `monky-status' is used if available.

If PROJECT-ROOT is given, it is opened instead of the project
root directory of the current buffer file.  If interactively
called with a prefix argument, the user is prompted for a project
directory to open."
  (interactive (and current-prefix-arg
                    (list
                     (projectile-completing-read
                      "Open project VC in: "
                      projectile-known-projects))))
  (or project-root (setq project-root (projectile-project-root)))
  (let ((vcs (projectile-project-vcs project-root)))
    (cl-case vcs
      (git
       (cond ((fboundp 'magit-status-internal)
              (magit-status-internal project-root))
             ((fboundp 'magit-status)
              (with-no-warnings (magit-status project-root)))
             (t
              (vc-dir project-root))))
      (hg
       (if (fboundp 'monky-status)
           (monky-status project-root)
         (vc-dir project-root)))
      (t (vc-dir project-root)))))

;;;###autoload
(defun projectile-recentf ()
  "Show a list of recently visited files in a project."
  (interactive)
  (if (boundp 'recentf-list)
      (find-file (projectile-expand-root
                  (projectile-completing-read
                   "Recently visited files: "
                   (projectile-recentf-files))))
    (message "recentf is not enabled")))

(defun projectile-recentf-files ()
  "Return a list of recently visited files in a project."
  (and (boundp 'recentf-list)
       (let ((project-root (projectile-ensure-project (projectile-project-root))))
         (mapcar
          (lambda (f) (file-relative-name f project-root))
          (cl-remove-if-not
           (lambda (f) (string-prefix-p project-root f))
           recentf-list)))))

(defun projectile-serialize-cache ()
  "Serializes the memory cache to the hard drive."
  (projectile-serialize projectile-projects-cache projectile-cache-file))

(defvar projectile-configure-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last configure command used on them.")

(defvar projectile-compilation-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last compilation command used on them.")

(defvar projectile-test-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last test command used on them.")

(defvar projectile-run-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last run command used on them.")

(defvar projectile-project-configure-cmd nil
  "The command to use with `projectile-configure-project'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el.")

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

(defun projectile-default-generic-command (project-type command-type)
  "Generic retrieval of COMMAND-TYPEs default cmd-value for PROJECT-TYPE.

If found, checks if value is symbol or string.  In case of symbol
resolves to function `funcall's.  Return value of function MUST
be string to be executed as command."
  (let ((command (plist-get (alist-get project-type projectile-project-types) command-type)))
    (cond
     ((stringp command) command)
     ((functionp command)
      (if (fboundp command)
          (funcall (symbol-function command))))
     ((and (not command) (eq command-type 'compilation-dir))
      ;; `compilation-dir' is special in that it is used as a fallback for the root
      nil)
     (t
      (user-error "The value for: %s in project-type: %s was neither a function nor a string." command-type project-type)))))

(defun projectile-default-configure-command (project-type)
  "Retrieve default configure command for PROJECT-TYPE."
  (projectile-default-generic-command project-type 'configure-command))

(defun projectile-default-compilation-command (project-type)
  "Retrieve default compilation command for PROJECT-TYPE."
  (projectile-default-generic-command project-type 'compile-command))

(defun projectile-default-compilation-dir (project-type)
  "Retrieve default compilation directory for PROJECT-TYPE."
  (projectile-default-generic-command project-type 'compilation-dir))

(defun projectile-default-test-command (project-type)
  "Retrieve default test command for PROJECT-TYPE."
  (projectile-default-generic-command project-type 'test-command))

(defun projectile-default-run-command (project-type)
  "Retrieve default run command for PROJECT-TYPE."
  (projectile-default-generic-command project-type 'run-command))

(defun projectile-configure-command (compile-dir)
  "Retrieve the configure command for COMPILE-DIR.

The command is determined like this:

- first we check `projectile-configure-cmd-map' for the last
configure command that was invoked on the project

- then we check for `projectile-project-configure-cmd' supplied
via .dir-locals.el

- finally we check for the default configure command for a
project of that type"
  (or (gethash compile-dir projectile-configure-cmd-map)
      projectile-project-configure-cmd
      (let ((cmd-format-string (projectile-default-configure-command (projectile-project-type))))
        (when cmd-format-string
          (format cmd-format-string (projectile-project-root))))))

(defun projectile-compilation-command (compile-dir)
  "Retrieve the compilation command for COMPILE-DIR.

The command is determined like this:

- first we check `projectile-compilation-cmd-map' for the last
compile command that was invoked on the project

- then we check for `projectile-project-compilation-cmd' supplied
via .dir-locals.el

- finally we check for the default compilation command for a
project of that type"
  (or (gethash compile-dir projectile-compilation-cmd-map)
      projectile-project-compilation-cmd
      (projectile-default-compilation-command (projectile-project-type))))

(defun projectile-test-command (compile-dir)
  "Retrieve the test command for COMPILE-DIR.

The command is determined like this:

- first we check `projectile-test-cmd-map' for the last
test command that was invoked on the project

- then we check for `projectile-project-test-cmd' supplied
via .dir-locals.el

- finally we check for the default test command for a
project of that type"
  (or (gethash compile-dir projectile-test-cmd-map)
      projectile-project-test-cmd
      (projectile-default-test-command (projectile-project-type))))

(defun projectile-run-command (compile-dir)
  "Retrieve the run command for COMPILE-DIR.

The command is determined like this:

- first we check `projectile-run-cmd-map' for the last
run command that was invoked on the project

- then we check for `projectile-project-run-cmd' supplied
via .dir-locals.el

- finally we check for the default run command for a
project of that type"
  (or (gethash compile-dir projectile-run-cmd-map)
      projectile-project-run-cmd
      (projectile-default-run-command (projectile-project-type))))

(defun projectile-read-command (prompt command)
  "Adapted from `compilation-read-command'."
  (read-shell-command prompt command
                      (if (equal (car compile-history) command)
                          '(compile-history . 1)
                        'compile-history)))

(defun projectile-compilation-dir ()
  "Retrieve the compilation directory for this project."
  (let* ((type (projectile-project-type))
         (directory (or projectile-project-compilation-dir
                        (projectile-default-compilation-dir type))))
    (if directory
        (file-truename
         (concat (file-name-as-directory (projectile-project-root))
                 (file-name-as-directory directory)))
      (projectile-project-root))))

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

(defvar projectile-project-command-history (make-hash-table :test 'equal)
  "The history of last executed project commands, per project.

Projects are indexed by their project-root value.")

(defun projectile--get-command-history (project-root)
  (or (gethash project-root projectile-project-command-history)
      (puthash project-root
               (make-ring 16)
               projectile-project-command-history)))

(cl-defun projectile--run-project-cmd
    (command command-map &key show-prompt prompt-prefix save-buffers)
  "Run a project COMMAND, typically a test- or compile command.

Cache the COMMAND for later use inside the hash-table COMMAND-MAP.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
by setting SHOW-PROMPT.  The prompt will be prefixed with PROMPT-PREFIX.

If SAVE-BUFFERS is non-nil save all projectile buffers before
running the command.

The command actually run is returned."
  (let* ((project-root (projectile-project-root))
         (default-directory (projectile-compilation-dir))
         (command (projectile-maybe-read-command show-prompt
                                                 command
                                                 prompt-prefix)))
    (when command-map
      (puthash default-directory command command-map)
      (ring-insert (projectile--get-command-history project-root) command))
    (when save-buffers
      (save-some-buffers (not compilation-ask-about-save)
                         (lambda ()
                           (projectile-project-buffer-p (current-buffer)
                                                        project-root))))
    (unless (file-directory-p default-directory)
      (mkdir default-directory))
    (projectile-run-compilation command)
    command))

;;;###autoload
(defun projectile-configure-project (arg)
  "Run project configure command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let ((command (projectile-configure-command (projectile-compilation-dir))))
    (projectile--run-project-cmd command projectile-configure-cmd-map
                                 :show-prompt arg
                                 :prompt-prefix "Configure command: "
                                 :save-buffers t)))

;;;###autoload
(defun projectile-compile-project (arg)
  "Run project compilation command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let ((command (projectile-compilation-command (projectile-compilation-dir))))
    (projectile--run-project-cmd command projectile-compilation-cmd-map
                                 :show-prompt arg
                                 :prompt-prefix "Compile command: "
                                 :save-buffers t)))

;;;###autoload
(defun projectile-test-project (arg)
  "Run project test command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let ((command (projectile-test-command (projectile-compilation-dir))))
    (projectile--run-project-cmd command projectile-test-cmd-map
                                 :show-prompt arg
                                 :prompt-prefix "Test command: "
                                 :save-buffers t)))

;;;###autoload
(defun projectile-run-project (arg)
  "Run project run command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let ((command (projectile-run-command (projectile-compilation-dir))))
    (projectile--run-project-cmd command projectile-run-cmd-map
                                 :show-prompt arg
                                 :prompt-prefix "Run command: ")))

;;;###autoload
(defun projectile-repeat-last-command (show-prompt)
  "Run last projectile external command.

External commands are: `projectile-configure-project',
`projectile-compile-project', `projectile-test-project' and
`projectile-run-project'.

If the prefix argument SHOW_PROMPT is non nil, the command can be edited."
  (interactive "P")
  (let* ((project-root
          (projectile-ensure-project (projectile-project-root)))
         (command-history (projectile--get-command-history project-root))
         (command (car-safe (ring-elements command-history)))
         (compilation-read-command show-prompt)
         executed-command)
    (unless command
      (user-error "No command has been run yet for this project"))
    (setq executed-command
          (projectile--run-project-cmd command
                                       nil
                                       :save-buffers t
                                       :prompt-prefix "Execute command: "))
    (unless (string= command executed-command)
      (ring-insert command-history executed-command))))

(defadvice compilation-find-file (around projectile-compilation-find-file)
  "Try to find a buffer for FILENAME, if we cannot find it,
fallback to the original function."
  (let ((filename (ad-get-arg 1))
        full-filename)
    (ad-set-arg 1
                (or
                 (if (file-exists-p (expand-file-name filename))
                     filename)
                 ;; Try to find the filename using projectile
                 (and (projectile-project-p)
                      (let ((root (projectile-project-root))
                            (dirs (cons "" (projectile-current-project-dirs))))
                        (when (setq full-filename
                                    (car (cl-remove-if-not
                                          #'file-exists-p
                                          (mapcar
                                           (lambda (f)
                                             (expand-file-name
                                              filename
                                              (expand-file-name f root)))
                                           dirs))))
                          full-filename)))
                 ;; Fall back to the old argument
                 filename))
    ad-do-it))

(defun projectile-open-projects ()
  "Return a list of all open projects.
An open project is a project with any open buffers."
  (delete-dups
   (delq nil
         (mapcar (lambda (buffer)
                   (with-current-buffer buffer
                     (when (projectile-project-p)
                       (abbreviate-file-name (projectile-project-root)))))
                 (buffer-list)))))

(defun projectile--remove-current-project (projects)
  "Remove the current project (if any) from the list of PROJECTS."
  (if-let ((project (projectile-project-root)))
      (projectile-difference projects
                             (list (abbreviate-file-name project)))
    projects))

(defun projectile--move-current-project-to-end (projects)
  "Move current project (if any) to the end of list in the list of PROJECTS."
  (if-let ((project (projectile-project-root)))
      (append
       (projectile--remove-current-project projects)
       (list (abbreviate-file-name project)))
    projects))

(defun projectile-relevant-known-projects ()
  "Return a list of known projects."
  (pcase projectile-current-project-on-switch
   ('remove (projectile--remove-current-project projectile-known-projects))
   ('move-to-end (projectile--move-current-project-to-end projectile-known-projects))
   ('keep projectile-known-projects)))

(defun projectile-relevant-open-projects ()
  "Return a list of open projects."
  (let ((open-projects (projectile-open-projects)))
    (pcase projectile-current-project-on-switch
     ('remove (projectile--remove-current-project open-projects))
     ('move-to-end (projectile--move-current-project-to-end open-projects))
     ('keep open-projects))))

;;;###autoload
(defun projectile-switch-project (&optional arg)
  "Switch to a project we have visited before.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'"
  (interactive "P")
  (let ((projects (projectile-relevant-known-projects)))
    (if projects
        (projectile-completing-read
         "Switch to project: " projects
         :action (lambda (project)
                   (projectile-switch-project-by-name project arg)))
      (user-error "There are no known projects"))))

;;;###autoload
(defun projectile-switch-open-project (&optional arg)
  "Switch to a project we have currently opened.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'"
  (interactive "P")
  (let ((projects (projectile-relevant-open-projects)))
    (if projects
        (projectile-completing-read
         "Switch to open project: " projects
         :action (lambda (project)
                   (projectile-switch-project-by-name project arg)))
      (user-error "There are no open projects"))))

(defun projectile-switch-project-by-name (project-to-switch &optional arg)
  "Switch to project by project name PROJECT-TO-SWITCH.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'"
  (unless (projectile-project-p project-to-switch)
    (projectile-remove-known-project project-to-switch)
    (error "Directory %s is not a project" project-to-switch))
  (let ((switch-project-action (if arg
                                   'projectile-commander
                                 projectile-switch-project-action)))
    (run-hooks 'projectile-before-switch-project-hook)
    (let ((default-directory project-to-switch))
      ;; use a temporary buffer to load PROJECT-TO-SWITCH's dir-locals before calling SWITCH-PROJECT-ACTION
      (with-temp-buffer
        (hack-dir-local-variables-non-file-buffer))
      ;; Normally the project name is determined from the current
      ;; buffer. However, when we're switching projects, we want to
      ;; show the name of the project being switched to, rather than
      ;; the current project, in the minibuffer. This is a simple hack
      ;; to tell the `projectile-project-name' function to ignore the
      ;; current buffer and the caching mechanism, and just return the
      ;; value of the `projectile-project-name' variable.
      (let ((projectile-project-name (funcall projectile-project-name-function
                                              project-to-switch)))
        (funcall switch-project-action)))
    (run-hooks 'projectile-after-switch-project-hook)))

;;;###autoload
(defun projectile-find-file-in-directory (&optional directory)
  "Jump to a file in a (maybe regular) DIRECTORY.

This command will first prompt for the directory the file is in."
  (interactive "DFind file in directory: ")
  (unless (projectile--directory-p directory)
    (user-error "Directory %S does not exist" directory))
  (let ((default-directory directory))
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
  (cl-mapcan
   (lambda (project)
     (when (file-exists-p project)
       (mapcar (lambda (file)
                 (expand-file-name file project))
               (projectile-project-files project))))
   projectile-known-projects))

;;;###autoload
(defun projectile-find-file-in-known-projects ()
  "Jump to a file in any of the known projects."
  (interactive)
  (find-file (projectile-completing-read "Find file in projects: " (projectile-all-project-files))))

(defun projectile-keep-project-p (project)
  "Determine whether we should cleanup (remove) PROJECT or not.

It handles the case of remote projects as well.
See `projectile--cleanup-known-projects'."
  ;; Taken from from `recentf-keep-default-predicate'
  (cond
   ((file-remote-p project nil t) (file-readable-p project))
   ((file-remote-p project))
   ((file-readable-p project))))

(defun projectile--cleanup-known-projects ()
  "Remove known projects that don't exist anymore and return a list of projects removed."
  (projectile-merge-known-projects)
  (let ((projects-kept (cl-remove-if-not #'projectile-keep-project-p projectile-known-projects))
        (projects-removed (cl-remove-if #'projectile-keep-project-p projectile-known-projects)))
    (setq projectile-known-projects projects-kept)
    (projectile-merge-known-projects)
    projects-removed))

;;;###autoload
(defun projectile-cleanup-known-projects ()
  "Remove known projects that don't exist anymore."
  (interactive)
  (if-let ((projects-removed (projectile--cleanup-known-projects)))
      (message "Projects removed: %s"
               (mapconcat #'identity projects-removed ", "))
    (message "No projects needed to be removed.")))

;;;###autoload
(defun projectile-clear-known-projects ()
  "Clear both `projectile-known-projects' and `projectile-known-projects-file'."
  (interactive)
  (setq projectile-known-projects nil)
  (projectile-save-known-projects))

;;;###autoload
(defun projectile-remove-known-project (&optional project)
  "Remove PROJECT from the list of known projects."
  (interactive (list (projectile-completing-read
                      "Remove from known projects: " projectile-known-projects
                      :action 'projectile-remove-known-project)))
  (unless (called-interactively-p 'any)
    (setq projectile-known-projects
          (cl-remove-if
           (lambda (proj) (string= project proj))
           projectile-known-projects))
    (projectile-merge-known-projects)
    (when projectile-verbose
      (message "Project %s removed from the list of known projects." project))))

;;;###autoload
(defun projectile-remove-current-project-from-known-projects ()
  "Remove the current project from the list of known projects."
  (interactive)
  (projectile-remove-known-project (abbreviate-file-name (projectile-project-root))))

(defun projectile-ignored-projects ()
  "A list of projects that should not be save in `projectile-known-projects'."
  (mapcar #'file-truename projectile-ignored-projects))

(defun projectile-ignored-project-p (project-root)
  "Return t if PROJECT-ROOT should not be added to `projectile-known-projects'."
  (or (member project-root (projectile-ignored-projects))
      (and (functionp projectile-ignored-project-function)
           (funcall projectile-ignored-project-function project-root))))

(defun projectile-add-known-project (project-root)
  "Add PROJECT-ROOT to the list of known projects."
  (interactive (list (read-directory-name "Add to known projects: ")))
  (unless (projectile-ignored-project-p project-root)
    (setq projectile-known-projects
          (delete-dups
           (cons (file-name-as-directory (abbreviate-file-name project-root))
                 projectile-known-projects)))
    (projectile-merge-known-projects)))

(defun projectile-load-known-projects ()
  "Load saved projects from `projectile-known-projects-file'.
Also set `projectile-known-projects'."
  (setq projectile-known-projects
        (projectile-unserialize projectile-known-projects-file))
  (setq projectile-known-projects-on-file
        (and (sequencep projectile-known-projects)
             (copy-sequence projectile-known-projects))))

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
         (removed-after-sync (projectile-difference known-on-last-sync known-now))
         (removed-in-other-process
          (projectile-difference known-on-last-sync known-on-file))
         (result (delete-dups
                  (projectile-difference
                   (append known-now known-on-file)
                   (append removed-after-sync removed-in-other-process)))))
    (setq projectile-known-projects result)
    (projectile-save-known-projects)))


;;; IBuffer integration
(define-ibuffer-filter projectile-files
    "Show Ibuffer with all buffers in the current project."
  (:reader (read-directory-name "Project root: " (projectile-project-root))
           :description nil)
  (with-current-buffer buf
    (equal (file-name-as-directory (expand-file-name qualifier))
           (projectile-project-root))))

(defun projectile-ibuffer-by-project (project-root)
  "Open an IBuffer window showing all buffers in PROJECT-ROOT."
  (let ((project-name (funcall projectile-project-name-function project-root)))
    (ibuffer nil (format "*%s Buffers*" project-name)
             (list (cons 'projectile-files project-root)))))

;;;###autoload
(defun projectile-ibuffer (prompt-for-project)
  "Open an IBuffer window showing all buffers in the current project.

Let user choose another project when PROMPT-FOR-PROJECT is supplied."
  (interactive "P")
  (let ((project-root (if prompt-for-project
                          (projectile-completing-read
                           "Project name: "
                           (projectile-relevant-known-projects))
                        (projectile-project-root))))

    (projectile-ibuffer-by-project project-root)))


;;;; projectile-commander

(defconst projectile-commander-help-buffer "*Projectile Commander Help*")

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
  (let* ((choices (mapcar #'car projectile-commander-methods))
         (prompt (concat "Select Projectile command [" choices "]: "))
         (ch (read-char-choice prompt choices))
         (fn (nth 2 (assq ch projectile-commander-methods))))
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
           (cl-sort (copy-sequence
                     (cons (list ,key ,description ,method)
                           (assq-delete-all ,key projectile-commander-methods)))
                    (lambda (a b) (< (car a) (car b)))))))

(def-projectile-commander-method ?? "Commander help buffer."
  (ignore-errors (kill-buffer projectile-commander-help-buffer))
  (with-current-buffer (get-buffer-create projectile-commander-help-buffer)
    (insert "Projectile Commander Methods:\n\n")
    (dolist (met projectile-commander-methods)
      (insert (format "%c:\t%s\n" (car met) (cadr met))))
    (goto-char (point-min))
    (help-mode)
    (display-buffer (current-buffer) t))
  (projectile-commander))

(defun projectile-commander-bindings ()
  "Setup the keybindings for the Projectile Commander."
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

  (def-projectile-commander-method ?V
    "Browse dirty projects"
    (projectile-browse-dirty-projects))

  (def-projectile-commander-method ?r
    "Replace a string in the project."
    (projectile-replace))

  (def-projectile-commander-method ?R
    "Regenerate the project's [e|g]tags."
    (projectile-regenerate-tags))

  (def-projectile-commander-method ?g
    "Run grep on project."
    (projectile-grep))

  (def-projectile-commander-method ?a
    "Run ag on project."
    (call-interactively 'projectile-ag))

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


;;; Dirty (modified) project check related functionality
(defun projectile-check-vcs-status (&optional project-path)
  "Check the status of the current project.
If PROJECT-PATH is a project, check this one instead."
  (let ((project-path (or project-path (projectile-project-root)))
        (project-status nil))
    (save-excursion
      (vc-dir project-path)
      ;; wait until vc-dir is done
      (while (vc-dir-busy) (sleep-for 0 100))
      ;; check for status
      (save-excursion
        (save-match-data
          (dolist (check projectile-vcs-dirty-state)
            (goto-char (point-min))
            (when (search-forward check nil t)
              (setq project-status (cons check project-status))))))
      (kill-buffer)
      project-status)))

(defvar projectile-cached-dirty-projects-status nil
  "Cache of the last dirty projects check.")

(defun projectile-check-vcs-status-of-known-projects ()
  "Return the list of dirty projects.
The list is composed of sublists~: (project-path, project-status).
Raise an error if their is no dirty project."
  (save-window-excursion
    (message "Checking for modifications in known projects...")
    (let ((projects projectile-known-projects)
          (status ()))
      (dolist (project projects)
        (when (and (projectile-keep-project-p project) (not (string= 'none (projectile-project-vcs project))))
          (let ((tmp-status (projectile-check-vcs-status project)))
            (when tmp-status
              (setq status (cons (list project tmp-status) status))))))
      (when (= (length status) 0)
        (message "No dirty projects have been found"))
      (setq projectile-cached-dirty-projects-status status)
      status)))

;;;###autoload
(defun projectile-browse-dirty-projects (&optional cached)
  "Browse dirty version controlled projects.

With a prefix argument, or if CACHED is non-nil, try to use the cached
dirty project list."
  (interactive "P")
  (let ((status (if (and cached projectile-cached-dirty-projects-status)
                    projectile-cached-dirty-projects-status
                  (projectile-check-vcs-status-of-known-projects)))
        (mod-proj nil))
    (while (not (= (length status) 0))
      (setq mod-proj (cons (car (pop status)) mod-proj)))
    (projectile-completing-read "Select project: " mod-proj
                                :action 'projectile-vc)))


;;; Find next/previous project buffer
(defun projectile--repeat-until-project-buffer (orig-fun &rest args)
  "Repeat ORIG-FUN with ARGS until the current buffer is a project buffer."
  (if (projectile-project-root)
      (let* ((other-project-buffers (make-hash-table :test 'eq))
             (projectile-project-buffers (projectile-project-buffers))
             (max-iterations (length (buffer-list)))
             (counter 0))
        (dolist (buffer projectile-project-buffers)
          (unless (eq buffer (current-buffer))
            (puthash buffer t other-project-buffers)))
        (when (cdr-safe projectile-project-buffers)
          (while (and (< counter max-iterations)
                      (not (gethash (current-buffer) other-project-buffers)))
            (apply orig-fun args)
            (cl-incf counter))))
    (apply orig-fun args)))

(defun projectile-next-project-buffer ()
  "In selected window switch to the next project buffer.

If the current buffer does not belong to a project, call `next-buffer'."
  (interactive)
  (projectile--repeat-until-project-buffer #'next-buffer))

(defun projectile-previous-project-buffer ()
  "In selected window switch to the previous project buffer.

If the current buffer does not belong to a project, call `previous-buffer'."
  (interactive)
  (projectile--repeat-until-project-buffer #'previous-buffer))


;;; Editing a project's .dir-locals
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


;;; Projectile Minor mode
(define-obsolete-variable-alias 'projectile-mode-line-lighter 'projectile-mode-line-prefix)
(defcustom projectile-mode-line-prefix
  " Projectile"
  "Mode line lighter prefix for Projectile.
It's used by `projectile-default-mode-line'
when using dynamic mode line lighter and is the only
thing shown in the mode line otherwise."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "0.12.0"))

(defvar-local projectile--mode-line projectile-mode-line-prefix)

(defun projectile-default-mode-line ()
  "Report project name and type in the modeline."
  (let ((project-name (projectile-project-name))
        (project-type (projectile-project-type)))
    (format "%s[%s%s]"
            projectile-mode-line-prefix
            (or project-name "-")
            (if project-type
                (format ":%s" project-type)
              ""))))

(defun projectile-update-mode-line ()
  "Update the Projectile mode-line."
  (let ((mode-line (funcall projectile-mode-line-function)))
    (setq projectile--mode-line mode-line))
  (force-mode-line-update))

(defvar projectile-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "4 a") #'projectile-find-other-file-other-window)
    (define-key map (kbd "4 b") #'projectile-switch-to-buffer-other-window)
    (define-key map (kbd "4 C-o") #'projectile-display-buffer)
    (define-key map (kbd "4 d") #'projectile-find-dir-other-window)
    (define-key map (kbd "4 D") #'projectile-dired-other-window)
    (define-key map (kbd "4 f") #'projectile-find-file-other-window)
    (define-key map (kbd "4 g") #'projectile-find-file-dwim-other-window)
    (define-key map (kbd "4 t") #'projectile-find-implementation-or-test-other-window)
    (define-key map (kbd "5 a") #'projectile-find-other-file-other-frame)
    (define-key map (kbd "5 b") #'projectile-switch-to-buffer-other-frame)
    (define-key map (kbd "5 d") #'projectile-find-dir-other-frame)
    (define-key map (kbd "5 D") #'projectile-dired-other-frame)
    (define-key map (kbd "5 f") #'projectile-find-file-other-frame)
    (define-key map (kbd "5 g") #'projectile-find-file-dwim-other-frame)
    (define-key map (kbd "5 t") #'projectile-find-implementation-or-test-other-frame)
    (define-key map (kbd "!") #'projectile-run-shell-command-in-root)
    (define-key map (kbd "&") #'projectile-run-async-shell-command-in-root)
    (define-key map (kbd "a") #'projectile-find-other-file)
    (define-key map (kbd "b") #'projectile-switch-to-buffer)
    (define-key map (kbd "C") #'projectile-configure-project)
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
    (define-key map (kbd "s r") #'projectile-ripgrep)
    (define-key map (kbd "s s") #'projectile-ag)
    (define-key map (kbd "S") #'projectile-save-project-buffers)
    (define-key map (kbd "t") #'projectile-toggle-between-implementation-and-test)
    (define-key map (kbd "T") #'projectile-find-test-file)
    (define-key map (kbd "u") #'projectile-run-project)
    (define-key map (kbd "v") #'projectile-vc)
    (define-key map (kbd "V") #'projectile-browse-dirty-projects)
    (define-key map (kbd "x e") #'projectile-run-eshell)
    (define-key map (kbd "x i") #'projectile-run-ielm)
    (define-key map (kbd "x t") #'projectile-run-term)
    (define-key map (kbd "x s") #'projectile-run-shell)
    (define-key map (kbd "z") #'projectile-cache-current-file)
    (define-key map (kbd "<left>") #'projectile-previous-project-buffer)
    (define-key map (kbd "<right>") #'projectile-next-project-buffer)
    (define-key map (kbd "ESC") #'projectile-project-buffers-other-buffer)
    map)
  "Keymap for Projectile commands after `projectile-keymap-prefix'.")
(fset 'projectile-command-map projectile-command-map)

(defvar projectile-mode-map
  (let ((map (make-sparse-keymap)))
    (when projectile-keymap-prefix
      (define-key map projectile-keymap-prefix 'projectile-command-map))
    (easy-menu-define projectile-mode-menu map
      "Menu for Projectile"
      '("Projectile"
        ["Find file" projectile-find-file]
        ["Find file in known projects" projectile-find-file-in-known-projects]
        ["Find test file" projectile-find-test-file]
        ["Find directory" projectile-find-dir]
        ["Find file in directory" projectile-find-file-in-directory]
        ["Find other file" projectile-find-other-file]
        ["Switch to buffer" projectile-switch-to-buffer]
        ["Jump between implementation file and test file" projectile-toggle-between-implementation-and-test]
        ["Kill project buffers" projectile-kill-buffers]
        ["Save project buffers" projectile-save-project-buffers]
        ["Recent files" projectile-recentf]
        ["Previous buffer" projectile-previous-project-buffer]
        ["Next buffer" projectile-next-project-buffer]
        "--"
        ["Toggle project wide read-only" projectile-toggle-project-read-only]
        ["Edit .dir-locals.el" projectile-edit-dir-locals]
        "--"
        ["Switch to project" projectile-switch-project]
        ["Switch to open project" projectile-switch-open-project]
        ["Discover projects in directory" projectile-discover-projects-in-directory]
        ["Browse dirty projects" projectile-browse-dirty-projects]
        ["Open project in dired" projectile-dired]
        "--"
        ["Search in project (grep)" projectile-grep]
        ["Search in project (ag)" projectile-ag]
        ["Replace in project" projectile-replace]
        ["Multi-occur in project" projectile-multi-occur]
        "--"
        ["Run shell" projectile-run-shell]
        ["Run eshell" projectile-run-eshell]
        ["Run ielm" projectile-run-ielm]
        ["Run term" projectile-run-term]
        "--"
        ["Cache current file" projectile-cache-current-file]
        ["Invalidate cache" projectile-invalidate-cache]
        ["Regenerate [e|g]tags" projectile-regenerate-tags]
        "--"
        ["Configure project" projectile-configure-project]
        ["Compile project" projectile-compile-project]
        ["Test project" projectile-test-project]
        ["Run project" projectile-run-project]
        ["Repeat last external command" projectile-repeat-last-command]
        "--"
        ["Project info" projectile-project-info]
        ["About" projectile-version]))
    map)
  "Keymap for Projectile mode.")

(defun projectile-find-file-hook-function ()
  "Called by `find-file-hook' when `projectile-mode' is on.

The function does pretty much nothing when triggered on remote files
as all the operations it normally performs are extremely slow over
tramp."
  (unless (file-remote-p default-directory)
    (when projectile-dynamic-mode-line
      (projectile-update-mode-line))
    (projectile-cache-files-find-file-hook)
    (projectile-track-known-projects-find-file-hook)
    (projectile-visit-project-tags-table)))

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
  :lighter projectile--mode-line
  :keymap projectile-mode-map
  :group 'projectile
  :require 'projectile
  :global t
  (cond
   (projectile-mode
    ;; setup the commander bindings
    (projectile-commander-bindings)
    ;; initialize the projects cache if needed
    (unless projectile-projects-cache
      (setq projectile-projects-cache
            (or (projectile-unserialize projectile-cache-file)
                (make-hash-table :test 'equal))))
    (unless projectile-projects-cache-time
      (setq projectile-projects-cache-time
            (make-hash-table :test 'equal)))
    ;; load the known projects
    (projectile-load-known-projects)
    ;; update the list of known projects
    (projectile--cleanup-known-projects)
    (projectile-discover-projects-in-search-path)
    (add-hook 'find-file-hook 'projectile-find-file-hook-function)
    (add-hook 'projectile-find-dir-hook #'projectile-track-known-projects-find-file-hook t)
    (add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook t t)
    (ad-activate 'compilation-find-file)
    (ad-activate 'delete-file))
   (t
    (remove-hook 'find-file-hook #'projectile-find-file-hook-function)
    (remove-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook t)
    (ad-deactivate 'compilation-find-file)
    (ad-deactivate 'delete-file))))

;;;###autoload
(define-obsolete-function-alias 'projectile-global-mode 'projectile-mode "1.0")

(provide 'projectile)

;;; projectile.el ends here
