;;; projectile.el --- Manage and navigate projects in Emacs easily -*- lexical-binding: t -*-

;; Copyright © 2011-2026 Bozhidar Batsov <bozhidar@batsov.dev>

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/bbatsov/projectile
;; Keywords: project, convenience
;; Version: 3.3.0-snapshot
;; Package-Requires: ((emacs "28.1") (compat "30"))

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
;; Projectile is a project interaction library for Emacs.
;; It provides a powerful set of features operating at the project
;; level, as well as simple heuristics to identify projects.
;;
;; See the README and https://docs.projectile.mx for more details.
;;
;;; Code:

(require 'cl-lib)
(require 'compat)
(require 'seq)
(require 'thingatpt)
(require 'ibuffer)
(require 'ibuf-ext)
(require 'compile)
(require 'grep)
(require 'fileloop)
(require 'filenotify)
(eval-when-compile
  ;; `transient' is bundled with Emacs 28.1+ (Projectile's minimum), but
  ;; it's only needed once `projectile-dispatch' is invoked, so it's
  ;; loaded lazily at run time (see `projectile-dispatch') and required
  ;; here only for macro expansion during byte-compilation.
  (require 'transient)
  (require 'find-dired)
  (require 'subr-x))

;; All calls run after the lazy `(require 'transient)' in
;; `projectile-dispatch' (or are otherwise guarded).
(declare-function transient-args "transient" (prefix))
(declare-function transient-setup "transient" (&optional name layout edit &rest params))
(declare-function transient-prefix "transient")
(declare-function transient--default-infix-command "transient")
(declare-function transient--suffix-only "transient")
;; Newer transient versions emit different internals from the
;; `transient-define-prefix' expansion; declare them as they appear so
;; byte-compiling against Emacs snapshots stays warning-free.
(declare-function transient--set-layout "transient")

;;; Declarations
;;
;; A bunch of variable and function declarations
;; needed to appease the byte-compiler.
(defvar ag-ignore-list)
(defvar eshell-buffer-name)
(defvar explicit-shell-file-name)
(defvar grep-files-aliases)
(defvar grep-find-ignored-directories)
(defvar grep-find-ignored-files)
(defvar eat-buffer-name)
(defvar ghostel-buffer-name)

(declare-function make-term "term")
(declare-function term-mode "term")
(declare-function term-char-mode "term")
(declare-function term-ansi-make-term "term")
(declare-function eshell-search-path "esh-ext")
(declare-function vc-dir "vc-dir")
(declare-function vc-dir-busy "vc-dir")
(declare-function vc-git-grep "vc-git")
(declare-function tramp-archive-file-name-p "tramp-archive")
(declare-function tramp-archive-file-name-archive "tramp-archive")
(declare-function helm-grep-get-file-extensions "helm-grep")

(declare-function ripgrep-regexp "ext:ripgrep")
(declare-function rg-run "ext:rg")
(declare-function vterm "ext:vterm")
(declare-function vterm-other-window "ext:vterm")
(declare-function vterm-send-return "ext:vterm")
(declare-function vterm-send-string "ext:vterm")
(declare-function eat "ext:eat")
(declare-function eat-other-window "ext:eat")
(declare-function ghostel "ext:ghostel")
(declare-function xref-show-xrefs "xref")
(declare-function xref-matches-in-directory "xref")

;; Only available on Emacs 29+ built with tree-sitter support; every call
;; site is guarded at runtime (see `projectile-run-test-at-point').
(declare-function treesit-available-p "treesit.c")
(declare-function treesit-parser-list "treesit.c")
(declare-function treesit-node-at "treesit")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-text "treesit")


;;; Customization
(defgroup projectile nil
  "Manage and navigate projects easily."
  :group 'tools
  :group 'convenience
  :link '(url-link :tag "GitHub" "https://github.com/bbatsov/projectile")
  :link '(url-link :tag "Online Manual" "https://docs.projectile.mx/")
  :link '(emacs-commentary-link :tag "Commentary" "projectile"))

(defcustom projectile-indexing-method
  (if (eq system-type 'windows-nt) 'native 'alien)
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
In this sense that approach is a hybrid between native indexing
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
  :safe (lambda (x) (memq x '(native hybrid alien)))
  :group 'projectile
  :type '(radio
          (const :tag "Native" native)
          (const :tag "Hybrid" hybrid)
          (const :tag "Alien" alien))
  :package-version '(projectile . "2.0.0"))

(defcustom projectile-enable-caching (eq projectile-indexing-method 'native)
  "When t enables project files caching.

Normally the cache lasts for the duration of your Emacs session.
If you want to cache to persist between Emacs sessions you
should set this option to `'persistent'.

Project caching is automatically enabled by default if you're
using the native indexing method."
  :group 'projectile
  :type '(radio
          (const :tag "Disabled" nil)
          (const :tag "Transient" t)
          (const :tag "Persistent" persistent))
  :package-version '(projectile . "2.9.0"))

(defcustom projectile-async-indexing t
  "Whether to index projects without freezing Emacs.

When non-nil, the external-command indexing methods (`alien' and
`hybrid') run their indexing command asynchronously and wait for it in a
way that keeps Emacs responsive to redisplay and `keyboard-quit' (\\[keyboard-quit]),
instead of blocking until the command finishes.  This matters most on
large projects and on remote (TRAMP) hosts, where a cold
`projectile-find-file' could otherwise freeze Emacs for seconds.

The resulting file list is identical to the synchronous path; only the
responsiveness during indexing differs.  Has no effect under `native'
indexing (the Emacs Lisp directory walk cannot run off the main thread),
in batch mode, or while a keyboard macro is executing - those fall back
to synchronous indexing."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "3.0.0"))

(defcustom projectile-kill-buffers-filter 'kill-all
  "Determine which buffers are killed by `projectile-kill-buffers'.

When the kill-all option is selected, kills each buffer.

When the kill-only-files option is selected, kill only the buffer
associated to a file.

It can also be a list of conditions, in which case a buffer is killed
when it satisfies any of them.  This is a composable DSL modeled on
project.el's `project-kill-buffer-conditions'.  Each condition is either:
- a regular expression, matched against the buffer name,
- a predicate function that takes the buffer as its argument and returns
  non-nil if it should be killed,
- a cons cell whose car says how to interpret the cdr:
  * `major-mode' - kill if the buffer's major mode is `eq' to the cdr,
  * `derived-mode' - kill if the buffer's major mode is derived from it,
  * `not' - the cdr is a single negated condition,
  * `and' - the cdr is a list of conditions that must all match,
  * `or' - the cdr is a list of conditions, any of which match.

An empty list (or nil) matches no buffers, so nothing is killed.

For example, to kill only file-visiting buffers and dired buffers:

  (setq projectile-kill-buffers-filter
        \\='(buffer-file-name (derived-mode . dired-mode)))

Otherwise, it should be a predicate that takes one argument: the buffer to
be killed."
  :group 'projectile
  :type '(choice
          (const :tag "All project buffers" kill-all)
          (const :tag "Project file buffers" kill-only-files)
          (function :tag "Predicate")
          (repeat :tag "Conditions"
                  (choice regexp function symbol
                          (cons :tag "Major mode" (const major-mode) symbol)
                          (cons :tag "Derived mode" (const derived-mode) symbol)
                          (cons :tag "Negation" (const not) sexp)
                          (cons :tag "Conjunction" (const and) sexp)
                          (cons :tag "Disjunction" (const or) sexp))))
  :package-version '(projectile . "2.0.0"))

(defcustom projectile-file-exists-local-cache-expire nil
  "Number of seconds before the local file existence cache expires.
Local refers to a file on a local file system.

A value of nil disables this cache.
See `projectile-file-exists-p' for details."
  :group 'projectile
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :package-version '(projectile . "0.11.0"))

(defcustom projectile-file-exists-remote-cache-expire (* 5 60)
  "Number of seconds before the remote file existence cache expires.
Remote refers to a file on a remote file system such as tramp.

A value of nil disables this cache.
See `projectile-file-exists-p' for details."
  :group 'projectile
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :package-version '(projectile . "0.11.0"))

(defcustom projectile-files-cache-expire nil
  "Number of seconds before project files list cache expires.

A value of nil means the cache never expires."
  :group 'projectile
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :package-version '(projectile . "1.0.0"))

(defcustom projectile-auto-discover t
  "Whether to discover projects under `projectile-project-search-path'.
When non-nil, the projects under the search path are discovered and
remembered the first time a project-switching command runs in an Emacs
session (see `projectile-discover-projects-in-search-path').

This has no effect unless `projectile-project-search-path' is set, so
the default is harmless out of the box; point the search path at your
projects directory and they'll be picked up automatically.

See also `projectile-project-search-path'."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "3.1.0"))

(defcustom projectile-auto-cleanup-known-projects nil
  "Whether to cleanup projects when project switching commands are invoked.

See also `projectile-cleanup-known-projects'."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.9.0"))

(defcustom projectile-auto-update-cache t
  "Whether cache is automatically updated when files are opened or deleted."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.0.0"))

(defcustom projectile-auto-update-cache-with-watches nil
  "When non-nil, watch project directories to keep the files cache fresh.

Experimental.  When enabled (and `projectile-enable-caching' is
non-nil), Projectile registers filesystem notification watches (via
`file-notify-add-watch') for the directories of a project whenever the
project's file list is cached.  Files created, deleted or renamed
outside Emacs then update the cached file list automatically, largely
removing the need for manual `projectile-invalidate-cache' calls.

Emacs file notifications are not recursive, so this costs one watch per
directory; projects spanning more directories than
`projectile-watch-directory-limit' are not watched.  Remote (TRAMP)
projects are never watched.  When an event cannot be applied
incrementally the project's cache is invalidated instead and rebuilt
lazily on the next file listing, which also re-arms the watches.

Change this via Customize (or `setopt'): disabling it then drops all
active watches immediately, and enabling it arms watches for the
projects that are already cached.  With plain `setq' the new value only
takes effect the next time a project's file list is cached."
  :group 'projectile
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         ;; The watch machinery is defined further down in this file, so
         ;; guard against the initial `defcustom' evaluation at load time.
         (if value
             ;; Only arm watches when the mode is on; otherwise they'd have no
             ;; mode-disable teardown to fire and would linger until Emacs exits.
             (when (and (bound-and-true-p projectile-mode)
                        (fboundp 'projectile--watch-all-cached-projects))
               (projectile--watch-all-cached-projects))
           (when (fboundp 'projectile--teardown-all-watches)
             (projectile--teardown-all-watches))))
  :package-version '(projectile . "3.1.0"))

(defcustom projectile-watch-directory-limit 512
  "Maximum number of file-notify watches to register per project.

File notifications are not recursive, so watching a project costs one
watch (and, on most backends, one file descriptor) per directory.
Projects whose cached file list spans more directories than this are
not watched at all; when `projectile-verbose' is non-nil a message is
emitted once per project.  Directories created inside a watched project
count against the same limit.

Only relevant when `projectile-auto-update-cache-with-watches' is
enabled."
  :group 'projectile
  :type 'integer
  :package-version '(projectile . "3.1.0"))

(defcustom projectile-require-project-root 'prompt
  "Require the presence of a project root to operate when true.
When set to `prompt' Projectile will ask you to select a project
directory if you're not in a project.

When nil Projectile will consider the current directory the project root."
  :group 'projectile
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (const :tag "Prompt for project" prompt))
  :package-version '(projectile . "2.0.0"))

(defcustom projectile-completion-system 'default
  "The completion system to be used by Projectile.
Either `default' (Emacs's built-in `completing-read', which works with
Vertico, Consult, Fido, `ido-completing-read+', etc.) or a custom
function accepting a prompt and a list of choices.

Note: the dedicated `ido', `helm' and `ivy' options were removed - those
frameworks are used through `completing-read' (or their own Projectile
integration packages, `helm-projectile' / `counsel-projectile'), so any
of those legacy values now behaves like `default'."
  :group 'projectile
  :type '(choice (const :tag "Default (completing-read)" default)
                 (function :tag "Custom function"))
  :package-version '(projectile . "3.0.0"))

(defcustom projectile-keymap-prefix nil
  "Projectile keymap prefix."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "0.7"))

(defcustom projectile-cache-file  ".projectile-cache.eld"
  "The name of Projectile's cache.
It's relative to the project root."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "2.9.0"))

(defcustom projectile-tags-file-name "TAGS"
  "The name of the tags file Projectile excludes from indexing.
Listed in `projectile-globally-ignored-files' so a generated tags
file doesn't show up among the project files."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "0.12.0"))

(defcustom projectile-sort-order 'default
  "The sort order used for a project's files.

It can also be set to a function that takes the list of project
files (as relative paths) and returns them in the desired order.

Note that files aren't sorted if `projectile-indexing-method'
is set to `alien'."
  :group 'projectile
  :type '(choice
          (radio
           (const :tag "Default (no sorting)" default)
           (const :tag "Recently opened files" recentf)
           (const :tag "Recently active buffers, then recently opened files" recently-active)
           (const :tag "Access time (atime)" access-time)
           (const :tag "Modification time (mtime)" modification-time))
          (function :tag "Custom sort function"))
  :package-version '(projectile . "3.1.0"))

(defcustom projectile-verbose t
  "Echo messages that are not errors."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "0.12.0"))

(defcustom projectile-buffers-filter-function nil
  "A function used to filter the buffers in `projectile-project-buffers'.

The function should accept and return a list of Emacs buffers.
Two example filter functions are shipped by default -
`projectile-buffers-with-file' and
`projectile-buffers-with-file-or-process'."
  :group 'projectile
  :type 'function
  :package-version '(projectile . "0.11.0"))

(defcustom projectile-project-name nil
  "If this value is non-nil, it will be used as project name.

It has precedence over function `projectile-project-name-function'."
  :group 'projectile
  :type 'string
  :safe (lambda (v) (or (null v)
                        (and (stringp v)
                             (not (string-blank-p v)))))
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-project-name-function 'projectile-default-project-name
  "A function that receives the project-root and returns the project name.

If variable `projectile-project-name' is non-nil, this function will not be
used."
  :group 'projectile
  :type 'function
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-project-root-files
  '(
    "GTAGS"              ; GNU Global tags
    "TAGS"               ; etags/ctags are usually in the root of project
    "configure.ac"       ; autoconf new style
    "configure.in"       ; autoconf old style
    "cscope.out"         ; cscope
    )
  "A list of files considered to mark the root of a project.
The topmost match has precedence.
See `projectile-register-project-type'."
  :group 'projectile
  :type '(repeat string)
  :package-version '(projectile . "0.10.0"))

(defcustom projectile-project-root-files-bottom-up
  '(".git"        ; Git VCS root dir
    ".hg"         ; Mercurial VCS root dir
    ".fslckout"   ; Fossil VCS root dir
    "_FOSSIL_"    ; Fossil VCS root DB on Windows
    ".bzr"        ; Bazaar VCS root dir
    "_darcs"      ; Darcs VCS root dir
    ".pijul"      ; Pijul VCS root dir
    ".sl"         ; Sapling VCS root dir
    ".jj"         ; Jujutsu VCS root dir
    )
  "A list of files considered to mark the root of a project.
The bottommost (parentmost) match has precedence.

This list holds only VCS markers (plus whatever you add yourself).
Per-language project manifests are deliberately *not* included, so an
enclosing VC root wins over a manifest sitting in a subdirectory - the
behavior most users and IDEs expect (the git repository is the
project).  In a polyglot or monorepo layout where you want a deeper
subproject to win instead, drop a `.projectile' file in it;
`projectile-root-marked' runs before `projectile-root-bottom-up', so
the marked subproject takes precedence over the outer VC root."
  :group 'projectile
  :type '(repeat string)
  :package-version '(projectile . "0.11.0"))

(defcustom projectile-project-root-files-top-down-recurring
  '(".svn" ; Svn VCS root dir
    "CVS"  ; CVS VCS root dir
    ".osc" ; osc (openSUSE Build Service) checkout dir
    "Makefile")
  "A list of files considered to mark the root of a project.
The search starts at the top and descends down till a directory
that contains a match file but its parent does not.  Thus, it's a
bottommost match in the topmost sequence of directories
containing a root file."
  :group 'projectile
  :type '(repeat string)
  :package-version '(projectile . "3.1.0"))

(defcustom projectile-project-root-functions
  '(projectile-root-local
    projectile-root-marked
    projectile-root-bottom-up
    projectile-root-top-down
    projectile-root-top-down-recurring)
  "A list of functions for finding project root folders.
The functions will be run until one of them returns a project folder.
Reordering the default functions will alter the project discovery
algorithm."
  :group 'projectile
  :type '(repeat function)
  :package-version '(projectile . "2.4.0"))

(defcustom projectile-dirconfig-file
  ".projectile"
  "The file which serves both as a project marker and configuration file.

The mere presence of this file in a directory marks that directory
as a Projectile project root, even when the file is empty.  When
the file has content, it is parsed by `projectile-parse-dirconfig-file'
to drive `+' keep / `-' ignore / `!' ensure rules; see the manual
for the full format.

This should _not_ be set via .dir-locals.el."
  :group 'projectile
  :type 'file
  :package-version '(projectile . "2.7.0"))

(defcustom projectile-dirconfig-comment-prefix
  nil
  "`projectile-dirconfig-file` comment start marker.
If specified, starting a line in a project's .projectile file with this
character marks that line as a comment instead of a pattern.
Similar to '#' in .gitignore files."
  :group 'projectile
  :type 'character
  :package-version '(projectile . "2.2.0"))

(defcustom projectile-warn-when-dirconfig-is-ignored t
  "Whether to warn when a non-empty .projectile is bypassed by alien indexing.
Under the `alien' indexing method, Projectile does not consult the
project's dirconfig file at indexing time.  When this option is
non-nil, a one-time warning is shown for each project where a
non-empty dirconfig is present alongside alien indexing, since the
silent bypass is a frequent source of confusion."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "3.0.0"))

(defcustom projectile-warn-on-prefixless-dirconfig-lines t
  "Whether to warn about deprecated prefix-less ignore entries.
Lines in `.projectile' that start with no `+'/`-'/`!' prefix are
still accepted as ignore patterns for backward compatibility, but
the implicit form is being phased out.  When this option is
non-nil, a one-time warning is shown per project that uses any
such line, listing the offending entries."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "3.0.0"))

(defcustom projectile-globally-ignored-files
  (list projectile-tags-file-name projectile-cache-file)
  "A list of files globally ignored by projectile.
Note that files aren't filtered if `projectile-indexing-method'
is set to `alien'."
  :safe (lambda (x) (not (remq t (mapcar #'stringp x))))
  :group 'projectile
  :type '(repeat string)
  :package-version '(projectile . "3.0.0"))

(defcustom projectile-globally-unignored-files nil
  "A list of files globally unignored by projectile.

Note that files aren't filtered if `projectile-indexing-method'
is set to `alien'."
  :group 'projectile
  :type '(repeat string)
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-globally-ignored-file-suffixes
  nil
  "A list of file suffixes globally ignored by projectile.
Note that files aren't filtered if `projectile-indexing-method'
is set to `alien'."
  :group 'projectile
  :type '(repeat string)
  :package-version '(projectile . "0.12.0"))

(defcustom projectile-globally-ignored-directories
  '(".idea"
    ".vscode"
    ".ensime_cache"
    ".eunit"
    ".git"
    ".hg"
    ".fslckout"
    "_FOSSIL_"
    ".bzr"
    "_darcs"
    ".pijul"
    ".tox"
    ".svn"
    ".stack-work"
    ".ccls-cache"
    ".cache"
    ".clangd"
    ".sl"
    ".jj"
    "*.osc")
  "A list of directories globally ignored by projectile.

Strings that don't start with * are only ignored at the top level
of the project.  Strings that start with * are ignored everywhere
in the project, as if there was no *.  So note that * when used as
a prefix is not a wildcard; it is an indicator that the directory
should be ignored at all levels, not just root.

Examples: \"tmp\" ignores only ./tmp at the top level of the
project, but not ./src/tmp.  \"*tmp\" will ignore both ./tmp and
./src/tmp, but not ./not-a-tmp or ./src/not-a-tmp.

Note that files aren't filtered if `projectile-indexing-method'
is set to `alien'.

See also `projectile-global-ignore-file-patterns'."
  :safe (lambda (x) (not (remq t (mapcar #'stringp x))))
  :group 'projectile
  :type '(repeat string)
  :package-version '(projectile . "3.1.0"))

(defcustom projectile-globally-unignored-directories nil
  "A list of directories globally unignored by projectile.
Note that files aren't filtered if `projectile-indexing-method'
is set to `alien'."
  :group 'projectile
  :type '(repeat string)
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-global-ignore-file-patterns
  nil
  "A list of file regexp patterns ignored by Projectile.

It complements `projectile-globally-ignored-files' and
`projectile-globally-ignored-directories'.  See also
`projectile-ignored-file-p' and `projectile-ignored-directory-p'.

Note that files aren't filtered if `projectile-indexing-method'
is set to `alien'."
  :safe (lambda (x) (not (remq t (mapcar #'stringp x))))
  :group 'projectile
  :type '(repeat string)
  :package-version '(projectile . "2.9.0"))

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
  :type '(repeat string)
  :package-version '(projectile . "0.10.0"))

(defcustom projectile-globally-ignored-buffers
  '("*scratch*"
    "*lsp-log*")
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
  :type 'hook
  :package-version '(projectile . "0.10.0"))

(defcustom projectile-find-dir-hook nil
  "Hooks run when a directory is opened with `projectile-find-dir'."
  :group 'projectile
  :type 'hook
  :package-version '(projectile . "0.10.0"))

(defcustom projectile-switch-project-action 'projectile-find-file
  "Action invoked after switching projects with `projectile-switch-project'.

Any function that does not take arguments will do."
  :group 'projectile
  :type 'function
  :package-version '(projectile . "0.10.0"))

(defcustom projectile-switch-project-other-window-action 'projectile-find-file-other-window
  "Action run by `projectile-switch-project-other-window' after switching.
Like `projectile-switch-project-action', but for the other-window variant.

Any function that does not take arguments will do."
  :group 'projectile
  :type 'function
  :package-version '(projectile . "3.0.0"))

(defcustom projectile-switch-project-other-frame-action 'projectile-find-file-other-frame
  "Action run by `projectile-switch-project-other-frame' after switching.
Like `projectile-switch-project-action', but for the other-frame variant.

Any function that does not take arguments will do."
  :group 'projectile
  :type 'function
  :package-version '(projectile . "3.0.0"))

(defcustom projectile-find-dir-includes-top-level nil
  "If true, add top-level dir to options offered by `projectile-find-dir'."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "0.10.0"))

(defcustom projectile-use-git-grep nil
  "If true, use `vc-git-grep' in git projects."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "0.11.0"))

(defcustom projectile-search-backend 'auto
  "The backend `projectile-search' uses to search the project.
Either a backend name registered in `projectile-search-backends'
\(`grep', `ripgrep', `ag', or one you registered yourself with
`projectile-register-search-backend'), `auto' to pick the first
available backend (favouring ripgrep, then grep), or `prompt' to be
asked which backend to use each time."
  :group 'projectile
  :type '(choice (const :tag "Automatic" auto)
                 (const :tag "Prompt each time" prompt)
                 (const :tag "grep" grep)
                 (const :tag "ripgrep" ripgrep)
                 (const :tag "ag" ag)
                 (symbol :tag "Other registered backend"))
  :package-version '(projectile . "3.0.0"))

(defcustom projectile-shell-backend 'eshell
  "The backend `projectile-run' uses to open a shell/REPL/terminal.
Either a backend name registered in `projectile-shell-backends'
\(`shell', `eshell', `ielm', `term', `vterm', `eat', `ghostel', or one
you registered yourself with `projectile-register-shell-backend'), `auto'
to pick the first available backend, or `prompt' to be asked each time."
  :group 'projectile
  :type '(choice (const :tag "shell" shell)
                 (const :tag "eshell" eshell)
                 (const :tag "ielm" ielm)
                 (const :tag "term" term)
                 (const :tag "vterm" vterm)
                 (const :tag "eat" eat)
                 (const :tag "ghostel" ghostel)
                 (const :tag "Automatic" auto)
                 (const :tag "Prompt each time" prompt)
                 (symbol :tag "Other registered backend"))
  :package-version '(projectile . "3.0.0"))

(defcustom projectile-grep-finished-hook nil
  "Hooks run when `projectile-grep' finishes."
  :group 'projectile
  :type 'hook
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-test-prefix-function 'projectile-test-prefix
  "Function to find test files prefix based on PROJECT-TYPE."
  :group 'projectile
  :type 'function
  :package-version '(projectile . "0.11.0"))

(defcustom projectile-test-suffix-function 'projectile-test-suffix
  "Function to find test files suffix based on PROJECT-TYPE."
  :group 'projectile
  :type 'function
  :package-version '(projectile . "0.11.0"))

(defcustom projectile-related-files-fn-function 'projectile-related-files-fn
  "Function to find related files based on PROJECT-TYPE."
  :group 'projectile
  :type 'function
  :package-version '(projectile . "2.1.0"))

(defcustom projectile-dynamic-mode-line t
  "If true, update the mode-line dynamically.
The mode-line is updated when files are opened via `find-file-hook'
and when the window configuration changes.

Change the value via Customize or `setopt' so it takes effect
immediately; a plain `setq' only takes effect before
`projectile-mode' is enabled.

See also `projectile-mode-line-function' and `projectile-update-mode-line'."
  :group 'projectile
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (bound-and-true-p projectile-mode)
           (if value
               (add-hook 'window-configuration-change-hook #'projectile-update-mode-line-on-window-change)
             (remove-hook 'window-configuration-change-hook #'projectile-update-mode-line-on-window-change))))
  :package-version '(projectile . "2.0.0"))

(defcustom projectile-mode-line-function 'projectile-default-mode-line
  "The function to use to generate project-specific mode-line.
The default function adds the project name and type to the mode-line.
See also `projectile-update-mode-line'."
  :group 'projectile
  :type 'function
  :package-version '(projectile . "2.0.0"))

(defcustom projectile-default-src-directory "src/"
  "The default value of a project's src-dir property.

It's used as a fallback in the case the property is not set for a project
type when `projectile-toggle-between-implementation-and-test' is used."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "2.6.0"))

(defcustom projectile-default-test-directory "test/"
  "The default value of a project's test-dir property.

It's used as a fallback in the case the property is not set for a project
type when `projectile-toggle-between-implementation-and-test' is used."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "2.6.0"))

;;; Per-project cache registry
;;
;; Projectile keeps a growing set of per-project caches.  Every one of
;; them has to be dropped when a project is invalidated and isolated in
;; the test sandbox; forgetting either wiring is a silent bug.  Caches
;; defined through `projectile-define-project-cache' get both for free,
;; and cleanups that aren't a simple table entry (killing a process,
;; deleting a file) can be attached with
;; `projectile--register-project-cache-cleanup'.

(defvar projectile--project-cache-vars nil
  "Hash-table variables holding per-project caches.
Collected by `projectile-define-project-cache'.  The test sandbox
rebinds each of these to a fresh table for isolation.")

(defvar projectile--project-cache-cleanups nil
  "Alist of NAME to cleanup function, run when a project is invalidated.
Each function is called with the project root being invalidated.
Keyed by name so that reloading projectile doesn't accumulate
duplicate cleanups.")

(defun projectile--register-project-cache-cleanup (name function)
  "Register FUNCTION under NAME to run when a project is invalidated.
FUNCTION is called with the project root.  Registering under an
existing NAME replaces the previous cleanup."
  (setf (alist-get name projectile--project-cache-cleanups) function))

(defmacro projectile-define-project-cache (name docstring &rest props)
  "Define NAME as a per-project cache table documented by DOCSTRING.
The table is keyed by project root (with test `equal') and is wired
into `projectile--invalidate-project-cache' and the test sandbox
automatically.  PROPS may contain `:prefix-keyed t' for tables whose
keys are directories under a project rather than the root itself;
invalidation then drops every entry under the invalidated root."
  (declare (indent 1) (doc-string 2))
  `(progn
     (defvar ,name (make-hash-table :test 'equal) ,docstring)
     (add-to-list 'projectile--project-cache-vars ',name)
     (projectile--register-project-cache-cleanup
      ',name
      ,(if (plist-get props :prefix-keyed)
           `(lambda (project-root)
              (dolist (key (hash-table-keys ,name))
                (when (string-prefix-p project-root key)
                  (remhash key ,name))))
         `(lambda (project-root) (remhash project-root ,name))))
     ',name))

(projectile-define-project-cache projectile-projects-cache
  "A hashmap used to cache project file names to speed up related operations.")

(projectile-define-project-cache projectile-projects-cache-time
  "A hashmap used to record when we populated `projectile-projects-cache'.")

(defvar projectile--async-index-processes (make-hash-table :test 'equal)
  "Map of project root -> in-flight async indexing process.
Used to avoid running more than one background index for the same
project at a time, and to discard a stale background result whose
project cache was invalidated while it was still running.")

;; Cancel any in-flight background index for an invalidated project so
;; its now-stale result can't repopulate the cache we just cleared.
(projectile--register-project-cache-cleanup
 'projectile--async-index-processes
 (lambda (project-root)
   (when-let* ((proc (gethash project-root projectile--async-index-processes)))
     (when (process-live-p proc)
       (delete-process proc))
     (remhash project-root projectile--async-index-processes))))

(defvar projectile-project-root-cache (make-hash-table :test 'equal)
  "Cached value of function `projectile-project-root`.")

;;; Project path spelling helpers
;;
;; A "project root" string is spelled in exactly two canonical ways across
;; the code base, and mixing them up is a recurring source of subtle cache
;; bugs (a root looked up in one spelling never matching a key stored in the
;; other).  The two spellings are:
;;
;; - The *cache-key* spelling: what `projectile-project-root' returns, i.e.
;;   an absolute, symlink-resolved (its search starts from `file-truename'),
;;   directory name ending in a slash.  This is what every per-project cache
;;   table (`projectile-projects-cache', the frecency table, the watch
;;   registry, ...) is keyed by.  Never re-abbreviate or re-expand such a
;;   value before using it as a key - it is already canonical.
;;
;; - The *known-projects* spelling: the abbreviated form persisted in
;;   `projectile-known-projects' and shown to the user.  Produce it only via
;;   `projectile--known-project-root' so every entry is spelled identically
;;   (abbreviated, trailing slash), which is what removal and membership
;;   checks rely on.
;;
;; `projectile--project-relative-name' owns the third boundary: turning an
;; absolute path into a root-relative one.

(defun projectile--project-relative-name (path root)
  "Return PATH spelled relative to project ROOT.
ROOT must be spelled as `projectile-project-root' returns it (absolute,
symlink-resolved, trailing slash) and PATH must share that spelling
\(e.g. already run through `file-truename' / `expand-file-name' the same
way).  When the two spellings diverge - PATH reached through a symlink
or abbreviation the root wasn't - the result gains leading `../'
segments; callers that cache the result should reject a value starting
with \"..\" rather than store a bogus entry.

This is a thin wrapper around `file-relative-name' whose sole purpose is
to give that boundary a single, documented owner."
  (file-relative-name path root))

(defun projectile--known-project-root (root)
  "Return ROOT in the canonical spelling used inside `projectile-known-projects'.
Entries are stored abbreviated (via `abbreviate-file-name') and with a
trailing slash so that membership and removal checks compare equal
regardless of how the root was originally obtained."
  (file-name-as-directory (abbreviate-file-name root)))

(projectile-define-project-cache projectile-project-type-cache
  "A hashmap used to cache project type to speed up related operations.")

(projectile-define-project-cache projectile-project-vcs-cache
  "Cache of `projectile-project-vcs' results keyed by directory.
Cleared by `projectile-invalidate-cache' and
`projectile-discard-root-cache'.  Entries are VCS symbols (or `none'
for projects with no detected VCS).")

(projectile-define-project-cache projectile--dirconfig-cache
  "Cache for parsed dirconfig files, keyed by project root.
Each value is a list of (DIRCONFIG-PATH MTIME PARSED-RESULT); a
cache hit requires both DIRCONFIG-PATH and MTIME to match the
current file, so changing `projectile-dirconfig-file' mid-session
naturally invalidates the entry.")

(projectile-define-project-cache projectile--git-submodules-cache
  "Cache of raw git submodule listings, keyed by directory.
Each value is a list of (GITMODULES-PATH MTIME COMMAND SUBMODULES); a
cache hit requires the `.gitmodules' path, its modification time and
`projectile-git-submodule-command' to all match, so editing
`.gitmodules' (or changing the command mid-session) naturally
invalidates the entry.  Alien/hybrid indexing lists submodules on
every file listing and the `git submodule foreach' shell-out dominates
its runtime (see issue #1953); a stat of `.gitmodules' is practically
free in comparison."
  :prefix-keyed t)

(defvar projectile--pending-cache-flush-timers (make-hash-table :test 'equal)
  "Map of project root to a pending idle-timer that will serialize its cache.
Used by `projectile-cache-current-file' to coalesce rapid file additions
into a single delayed disk write per project.")

;; Cancel a pending flush for an invalidated project - if it fired
;; after invalidation it would serialize the now-empty in-memory state,
;; recreating the cache file we just deleted with nil contents.
(projectile--register-project-cache-cleanup
 'projectile--pending-cache-flush-timers
 (lambda (project-root)
   (when-let* ((timer (gethash project-root
                               projectile--pending-cache-flush-timers)))
     (cancel-timer timer)
     (remhash project-root projectile--pending-cache-flush-timers))))

;; Deliberately a plain defvar rather than a `projectile-define-project-cache':
;; the descriptors in this table are live OS resources, so the test sandbox
;; must not rebind it to a fresh table (that would orphan active watches).
;; Invalidation goes through `projectile--unwatch-project' instead, which
;; removes the watches before dropping the registry entries.
(defvar projectile--project-watches (make-hash-table :test 'equal)
  "Map of project root to its registered file-notify watches.
Each value is a list of (DESCRIPTOR . DIRECTORY) conses, DIRECTORY
being the watched directory as an absolute name with a trailing slash.
Only populated when `projectile-auto-update-cache-with-watches' is
enabled; see `projectile--watch-project'.")

;; Drop an invalidated project's file-notify watches along with their
;; queued events; they re-arm the next time the cache is filled (see
;; `projectile-cache-project').
(projectile--register-project-cache-cleanup
 'projectile--project-watches
 (lambda (project-root)
   (projectile--unwatch-project project-root)))

(defvar projectile--watch-pending-events (make-hash-table :test 'equal)
  "Map of project root to its queued (not yet processed) file-notify events.
Events are pushed by `projectile--handle-watch-event' (so the list is in
reverse arrival order) and drained by `projectile--process-watch-events'
once the debounce timer fires.")

(defvar projectile--watch-debounce-timers (make-hash-table :test 'equal)
  "Map of project root to the pending debounce timer for its watch events.")

(defvar projectile--watch-debounce-delay 0.5
  "Seconds to wait after a file-notify event before processing the batch.
Coalesces event bursts (e.g. a `git checkout' touching many files) into
a single pass over the project's cached file list.")

(defvar projectile--watch-skipped-projects (make-hash-table :test 'equal)
  "Set of project roots already reported as too big (or unable) to watch.
Used to emit the `projectile-watch-directory-limit' message only once
per project and session.")

(defvar projectile--alien-dirconfig-warned-projects (make-hash-table :test 'equal)
  "Set of project roots already warned about alien indexing skipping the dirconfig.")

(defvar projectile--prefixless-dirconfig-warned-projects (make-hash-table :test 'equal)
  "Set of project roots already warned about prefix-less dirconfig entries.")

(defvar projectile--glob-keep-warned-projects (make-hash-table :test 'equal)
  "Set of project roots already warned about glob patterns in + keep entries.")

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
  :type 'string
  :package-version '(projectile . "0.9.0"))

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
the variable `projectile-ignored-projects'.

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

When set to nil you'll always have to add projects explicitly with
`projectile-add-known-project'."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "1.0.0"))

(defcustom projectile-project-search-path nil
  "List of folders where projectile is automatically going to look for projects.
You can think of something like $PATH, but for projects instead of executables.
Examples of such paths might be ~/projects, ~/work, (~/github . 1) etc.

For elements of form (DIRECTORY . DEPTH), DIRECTORY has to be a
directory and DEPTH an integer that specifies the depth at which to
look for projects.  A DEPTH of 0 means check DIRECTORY.  A depth of 1
means check all the subdirectories of DIRECTORY.  Etc."
  :group 'projectile
  :type '(repeat (choice directory (cons directory (integer :tag "Depth"))))
  :package-version '(projectile . "1.0.0"))

(defcustom projectile-fd-executable
  (cond
   ((executable-find "fdfind") "fdfind")
   ((executable-find "fd") "fd"))
  "Path or name of fd executable used by Projectile if enabled.
Nil means fd is not installed or should not be used.

Note: this variable holds the locally-detected executable.  For
projects on a TRAMP host fd is detected separately on the remote (and
cached per host), since whatever is on the local box may not exist on
the remote.  See `projectile-fd-executable-for'."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "2.8.0"))

(defvar projectile--remote-fd-executable-cache (make-hash-table :test 'equal)
  "Per-host cache of fd availability on remote (TRAMP) hosts.
Keys are the prefixes returned by `file-remote-p' (e.g.
`/ssh:user@host:'); values are either the executable name (a string)
or nil for hosts where neither `fd' nor `fdfind' is available.  The
sentinel `unset' distinguishes \"never looked up\" from \"looked up
and unavailable\".")

(defun projectile--remote-fd-executable (remote)
  "Return the fd executable available on REMOTE, or nil.
REMOTE is a TRAMP file name prefix as returned by `file-remote-p'.
The lookup is performed once per host and cached in
`projectile--remote-fd-executable-cache' to avoid the round-trip on
every indexing call."
  (let ((cached (gethash remote projectile--remote-fd-executable-cache 'unset)))
    (if (not (eq cached 'unset))
        cached
      (let* ((default-directory remote)
             (found (or (executable-find "fdfind" t)
                        (executable-find "fd" t)))
             (program (and found (file-name-nondirectory found))))
        (puthash remote program projectile--remote-fd-executable-cache)
        program))))

(defun projectile-fd-executable-for (directory)
  "Return the fd executable to use for DIRECTORY.
For local directories returns `projectile-fd-executable'.  For remote
directories looks up `fd'/`fdfind' on the remote (cached per host) and
returns the bare program name, or nil when fd is not available there."
  (if-let* ((remote (file-remote-p directory)))
      (projectile--remote-fd-executable remote)
    projectile-fd-executable))

(defcustom projectile-git-use-fd (when projectile-fd-executable t)
  "Non-nil means use fd to implement git ls-files.
This may change Projectile's performance in large Git repositories
depending on your system, but it will also work around the Git behavior
that causes deleted files to still be shown in Projectile listings until
their deletions are staged."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.8.0"))

(defcustom projectile-git-command "git ls-files -zco --exclude-standard"
  "Command used by projectile to get the files in a git project."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "0.9.0"))

(defcustom projectile-git-fd-args "-H -0 -E .git -tf --strip-cwd-prefix -c never"
  "Arguments to fd used to re-implement `git ls-files'.
This is used with `projectile-fd-executable' when `projectile-git-use-fd'
is non-nil."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "2.8.0"))

(defconst projectile--default-git-submodule-command
  "git submodule --quiet foreach 'echo $displaypath' | tr '\\n' '\\0'"
  "Default value of `projectile-git-submodule-command'.
When the variable still has this value the command is never actually
run; the submodules are listed by invoking git directly instead (see
`projectile--git-submodule-paths').")

(defcustom projectile-git-submodule-command projectile--default-git-submodule-command
  "Command used by projectile to list submodules of a given git repository.
Set to nil to disable listing submodules contents.

The default listing no longer shells out: when this variable has its
default value the submodules are listed by running git directly, with
no shell involved (see issue #1600).  Customizing the variable switches
back to running it as a shell command."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "0.12.0"))

(defcustom projectile-git-ignored-command "git ls-files -zcoi --exclude-standard"
  "Command used by projectile to get the ignored files in a git project."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-hg-command "hg locate -f -0 -I ."
  "Command used by projectile to get the files in a hg project."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "0.9.0"))

(defcustom projectile-hg-ignored-command "hg status -in0 ."
  "Command used by projectile to get the ignored files in a hg project."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "3.0.0"))

(defcustom projectile-jj-command "jj file list -T 'path ++ \"\\0\"' --no-pager ."
  "Command used by projectile to get the files in a Jujutsu project."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "2.9.0"))

(defcustom projectile-sapling-command "sl locate -0 -I ."
  "Command used by projectile to get the files in a Sapling project."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "2.9.0"))

(defcustom projectile-fossil-command (concat "fossil ls | "
                                             (when (eq system-type 'windows-nt)
                                               "dos2unix | ")
                                             "tr '\\n' '\\0'")
  "Command used by projectile to get the files in a fossil project."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "0.9.2"))

(defcustom projectile-bzr-command "bzr ls -R --versioned -0"
  "Command used by projectile to get the files in a bazaar project."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "0.9.0"))

(defcustom projectile-darcs-command "darcs show files -0 . "
  "Command used by projectile to get the files in a darcs project."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "0.9.0"))

(defcustom projectile-pijul-command "pijul list | tr '\\n' '\\0'"
   "Command used by projectile to get the files in a pijul project."
   :group 'projectile
   :type 'string
  :package-version '(projectile . "2.6.0"))

(defcustom projectile-svn-command "svn list -R . | grep -v '$/' | tr '\\n' '\\0'"
  "Command used by projectile to get the files in a svn project.

The command runs non-interactively (its output is piped), so `svn'
can't prompt for credentials.  For projects on an authenticated remote
you need to have your credentials cached first (e.g. by running `svn'
once interactively and letting it store them), otherwise the command
fails with an authentication error.  See URL
`https://github.com/bbatsov/projectile/issues/1638'."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "0.9.0"))

(defcustom projectile-svn-ignored-command "svn status --no-ignore | grep '^I' | cut -c9- | tr '\\n' '\\0'"
  "Command used by projectile to get the ignored files in a svn project."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "3.0.0"))

(defcustom projectile-generic-command
  (cond
   ;; we prefer fd over find
   ;; note that --strip-cwd-prefix is only available in version 8.3.0+
   (projectile-fd-executable
    (format "%s . -0 --type f --color=never --strip-cwd-prefix" projectile-fd-executable))
   ;; with find we have to be careful to strip the ./ from the paths
   ;; see https://stackoverflow.com/questions/2596462/how-to-strip-leading-in-unix-find
   (t "find . -type f | cut -c3- | tr '\\n' '\\0'"))
  "Command used by projectile to get the files in a generic project."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "2.8.0"))

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

    ;; OCaml extensions
    ("ml" . ("mli"))
    ("mli" . ("ml" "mll" "mly"))
    ("mll" . ("mli"))
    ("mly" . ("mli"))
    ("eliomi" . ("eliom"))
    ("eliom" . ("eliomi"))

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
  :group 'projectile
  :type 'alist
  :package-version '(projectile . "0.12.0"))

(defcustom projectile-create-missing-test-files nil
  "During toggling, if non-nil enables creating test files if not found.

When not-nil, every call to projectile-find-implementation-or-test-*
creates test files if not found on the file system.  Defaults to nil.
It assumes the test/ folder is at the same level as src/."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "0.13.0"))

(defcustom projectile-per-project-compilation-buffer nil
  "When non-nil, the compilation command makes the per-project compilation buffer."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.6.0"))

(defcustom projectile-after-switch-project-hook nil
  "Hooks run right after project is switched."
  :group 'projectile
  :type 'hook
  :package-version '(projectile . "0.13.0"))

(defcustom projectile-before-switch-project-hook nil
  "Hooks run right before project is switched."
  :group 'projectile
  :type 'hook
  :package-version '(projectile . "0.13.0"))

(defcustom projectile-project-changed-functions nil
  "Functions to run when the current project changes.

Each function is called with two arguments - the new project root and
the project root it changed from (nil when there was none).  Unlike
`projectile-after-switch-project-hook', which only runs on
`projectile-switch-project', these functions also run when the project
changes implicitly, e.g. by visiting a file or directory of another
project.  Moving to a buffer outside any project is not a change;
the functions run again only when another project is entered."
  :group 'projectile
  :type 'hook
  :package-version '(projectile . "3.1.0"))

(defcustom projectile-current-project-on-switch 'remove
  "Determines whether to display current project when switching projects.

When set to `remove' current project is not included, `move-to-end'
will display current project and the end of the list of known
projects, `keep' will leave the current project at the default
position."
  :group 'projectile
  :type '(radio
          (const :tag "Remove" remove)
          (const :tag "Move to end" move-to-end)
          (const :tag "Keep" keep))
  :package-version '(projectile . "2.0.0"))

(defcustom projectile-max-file-buffer-count nil
  "Maximum number of file buffers per project that are kept open.

If the value is nil, there is no limit to the opened buffers count."
  :group 'projectile
  :type 'integer
  :package-version '(projectile . "2.2.0"))

(defcustom projectile-cmd-hist-ignoredups t
  "Controls when inputs are added to projectile's command history.

A value of t means consecutive duplicates are ignored.
A value of `erase' means only the last duplicate is kept.
A value of nil means nothing is ignored."
  :group 'projectile
  :type '(choice (const :tag "Don't ignore anything" nil)
                 (const :tag "Ignore consecutive duplicates" t)
                 (const :tag "Only keep last duplicate" erase))
  :package-version '(projectile . "2.9.0"))

(defvar projectile-project-test-suffix nil
  "Use this variable to override the current project's test-suffix property.
It takes precedence over the test-suffix for the project type when set.
Should be set via .dir-locals.el.")
(put 'projectile-project-test-suffix 'safe-local-variable #'stringp)

(defvar projectile-project-test-prefix nil
  "Use this variable to override the current project's test-prefix property.
It takes precedence over the test-prefix for the project type when set.
Should be set via .dir-locals.el.")
(put 'projectile-project-test-prefix 'safe-local-variable #'stringp)

(defvar projectile-project-related-files-fn nil
  "Use this variable to override the current project's related-files-fn property.
It takes precedence over the related-files-fn attribute for the project type
when set.  Should be set via .dir-locals.el.")

(defvar projectile-project-src-dir nil
  "Use this variable to override the current project's src-dir property.
It takes precedence over the src-dir for the project type when set.
Should be set via .dir-locals.el.")
(put 'projectile-project-src-dir 'safe-local-variable #'stringp)

(defvar projectile-project-test-dir nil
  "Use this variable to override the current project's test-dir property.
It takes precedence over the test-dir for the project type when set.
Should be set via .dir-locals.el.")
(put 'projectile-project-test-dir 'safe-local-variable #'stringp)


;;; Version information

(defconst projectile-version "3.3.0-snapshot"
  "The current version of Projectile.")

(defun projectile--pkg-version ()
  "Extract Projectile's package version from its package metadata."
  ;; Use `cond' below to avoid a compiler unused return value warning
  ;; when `package-get-version' returns nil. See #3181.
  (cond ((package-get-version))))

;;;###autoload
(defun projectile-version (&optional show-version)
  "Get the Projectile version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both are present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (let ((version (or (projectile--pkg-version) projectile-version)))
   (if show-version
       (message "Projectile %s" version)
     version)))

;;; Misc utility functions

(defun projectile-unixy-system-p ()
  "Check to see if unixy text utilities are installed."
  (seq-every-p
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

(defun projectile-generate-process-name (process make-new &optional project)
  "Infer the buffer name for PROCESS or generate a new one if MAKE-NEW is true.
The function operates on the current project by default, but you can also
specify a project explicitly via the optional PROJECT param."
  (let* ((project (or project (projectile-acquire-root)))
         (name (projectile-project-name project))
         (base-name (format "*%s %s*" process name))
         ;; When a buffer with the same name already exists but belongs to a
         ;; different project root, disambiguate using the project path.
         (base-name (if (and (not make-new)
                             (let ((buf (get-buffer base-name)))
                               (and buf
                                    (not (string= (with-current-buffer buf default-directory)
                                                  project)))))
                        (format "*%s %s*" process (abbreviate-file-name project))
                      base-name)))
    (if make-new
        (generate-new-buffer-name base-name)
      base-name)))


;;; Serialization
(defun projectile-serialize (data filename)
  "Serialize DATA to FILENAME.

The saved data can be restored with `projectile-unserialize'."
  (if (file-writable-p filename)
    (with-temp-file filename
      (insert (let (print-length) (prin1-to-string data))))
    (display-warning 'projectile (format "Cache file '%s' is not writable" filename) :warning)))

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
  "Timer for scheduling `projectile-file-exists-cache-cleanup'.")

(defun projectile-file-exists-cache-cleanup ()
  "Remove timed out cache entries.
Also reschedule or remove the timer if no more items are in the cache."
  (let ((now (current-time)))
    (maphash (lambda (key value)
               (if (time-less-p (cdr value) now)
                   (remhash key projectile-file-exists-cache)))
             projectile-file-exists-cache)
    (setq projectile-file-exists-cache-timer
          (if (> (hash-table-count projectile-file-exists-cache) 0)
              (run-with-timer 10 nil 'projectile-file-exists-cache-cleanup)))))

(defun projectile-file-exists-p (filename)
  "Return t if file FILENAME exists.
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

(defsubst projectile-persistent-cache-p ()
  (eq projectile-enable-caching 'persistent))

;; Delete the invalidated project's on-disk cache file, when
;; persistent caching is enabled.
(projectile--register-project-cache-cleanup
 'projectile-project-cache-file
 (lambda (project-root)
   (when (projectile-persistent-cache-p)
     (let ((cache-file (projectile-project-cache-file project-root)))
       (when (file-exists-p cache-file)
         (delete-file cache-file))))))

(defun projectile--invalidate-project-cache (project-root)
  "Drop all of Projectile's per-project caches for PROJECT-ROOT.

Runs every cleanup in `projectile--project-cache-cleanups': the
tables defined via `projectile-define-project-cache' plus the ad-hoc
cleanups (cancelling in-flight background indexing and pending cache
flushes, removing file-notify watches, deleting the on-disk cache
file)."
  (dolist (entry projectile--project-cache-cleanups)
    (funcall (cdr entry) project-root)))

;;;###autoload
(defun projectile-invalidate-cache (prompt)
  "Remove the current project's files from `projectile-projects-cache'.

With a prefix argument PROMPT prompts for the name of the project whose cache
to invalidate.

The global (project-independent) cache for checking which project a file
belongs to, is also cleared. Therefore this function is still useful even
when not operating on a specific project, and as such only the global cache
is cleared when there is no current project (unless you give a prefix
argument)."
  (interactive "P")
  (setq projectile-project-root-cache (make-hash-table :test 'equal))
  ;; Drop the file-existence cache too - otherwise, after the user
  ;; creates a project marker (`.projectile', `.git', etc.) over TRAMP,
  ;; the negative entries cached during earlier root-walks would keep
  ;; reporting "not found" for up to `projectile-file-exists-remote-cache-expire'
  ;; seconds even though the project root walk has been invalidated.
  (clrhash projectile-file-exists-cache)
  (when-let* ((project-root
              (if prompt
                  (completing-read "Remove cache for: "
                                   (hash-table-keys projectile-projects-cache))
                (projectile-project-root))))
    (projectile--invalidate-project-cache project-root)
    (when projectile-verbose
      (message "Invalidated Projectile cache for %s."
               (propertize project-root 'face 'font-lock-keyword-face))))
  (when (fboundp 'recentf-cleanup)
    (recentf-cleanup)))

;;;###autoload
(defun projectile-invalidate-cache-all ()
  "Invalidate the caches of every known project.

Runs the same per-project invalidation as `projectile-invalidate-cache'
over all projects in `projectile-known-projects' (plus any project that
only has an entry in `projectile-projects-cache'), and also clears the
global project root and file-existence caches.  When persistent caching
is enabled the projects' on-disk cache files are deleted too.

Remote (TRAMP) projects are skipped, as touching each one could mean a
slow connection round-trip per project; invalidate those individually
with `projectile-invalidate-cache'."
  (interactive)
  (setq projectile-project-root-cache (make-hash-table :test 'equal))
  (clrhash projectile-file-exists-cache)
  (let ((roots (seq-remove #'file-remote-p
                           (delete-dups
                            (append (projectile-known-projects)
                                    (hash-table-keys projectile-projects-cache))))))
    (dolist (project-root roots)
      (projectile--invalidate-project-cache project-root))
    (when projectile-verbose
      (message "Invalidated the Projectile caches of %d project(s)."
               (length roots))))
  (when (fboundp 'recentf-cleanup)
    (recentf-cleanup)))

;;;###autoload
(defun projectile-discard-root-cache ()
  "Clear `projectile-project-root-cache' without touching other caches.
Useful after creating, removing, or moving a project marker (e.g.
`.projectile' or `.git') - Projectile would otherwise keep returning
its previously cached answer for that directory.

See also `projectile-invalidate-cache', which does this and also drops
the per-project file list and project-type caches."
  (interactive)
  (setq projectile-project-root-cache (make-hash-table :test 'equal))
  ;; The file-existence cache holds the negative answers gathered
  ;; while walking up looking for project markers; without clearing it
  ;; here, a freshly-created `.projectile' over TRAMP wouldn't be
  ;; visible until the entries time out (see
  ;; `projectile-file-exists-remote-cache-expire').
  (clrhash projectile-file-exists-cache)
  (when projectile-verbose
    (message "Cleared Projectile project root cache.")))

(defun projectile-time-seconds ()
  "Return the number of seconds since the unix epoch."
  (time-convert nil 'integer))

(defun projectile-cache-project (project files)
  "Cache PROJECTs FILES.
The cache is created both in memory and on the hard drive."
  (puthash project files projectile-projects-cache)
  (puthash project (projectile-time-seconds) projectile-projects-cache-time)
  (when (projectile-persistent-cache-p)
    (projectile-serialize files (projectile-project-cache-file project)))
  (projectile--maybe-watch-project project files))

(defun projectile-load-project-cache (project-root)
  "Load the cache file for PROJECT-ROOT in memory."
  (when-let* ((cache-file (projectile-project-cache-file project-root)))
    (when (file-exists-p cache-file)
      (when-let* ((data (projectile-unserialize cache-file)))
        (puthash project-root data projectile-projects-cache)
        ;; Seed the cache time from the file's mtime so the TTL check in
        ;; `projectile-project-files' can decide whether the loaded data is
        ;; already stale, and so the in-memory entry isn't evicted on the
        ;; next call just because no time was recorded.
        (puthash project-root
                 (time-convert
                  (file-attribute-modification-time
                   (file-attributes cache-file))
                  'integer)
                 projectile-projects-cache-time)
        ;; Loading the persistent cache fills the in-memory file list just
        ;; like a fresh index does, so arm the watches here too.
        (projectile--maybe-watch-project project-root data)
        data))))

;;;###autoload
(defun projectile-purge-file-from-cache (file)
  "Purge FILE from the cache of the current project."
  (interactive
   (list (projectile-completing-read
          "Remove file from cache: "
          (projectile-current-project-files)
          :caller 'projectile-read-file)))
  (let* ((project-root (projectile-project-root))
         (project-cache (gethash project-root projectile-projects-cache)))
    (if (projectile-file-cached-p file project-root)
        (let ((new-cache (remove file project-cache)))
          (puthash project-root new-cache projectile-projects-cache)
          (when (projectile-persistent-cache-p)
            (projectile-serialize new-cache (projectile-project-cache-file project-root)))
          ;; Re-derive watches from the shrunken cache, otherwise the purged
          ;; file's directory keeps being watched and a later create-event
          ;; there re-adds the entries we just removed.
          (projectile--maybe-watch-project project-root new-cache)
          (when projectile-verbose
            (message "%s removed from cache" file)))
      (user-error "%s is not in the cache" file))))

;;;###autoload
(defun projectile-purge-dir-from-cache (dir)
  "Purge DIR from the cache of the current project."
  (interactive
   (list (projectile-completing-read
          "Remove directory from cache: "
          (projectile-current-project-dirs)
          :caller 'projectile-read-directory)))
  (let* ((project-root (projectile-project-root))
         (project-cache (gethash project-root projectile-projects-cache))
         (new-cache (seq-remove (lambda (str) (string-prefix-p dir str))
                                project-cache)))
    (puthash project-root new-cache projectile-projects-cache)
    (when (projectile-persistent-cache-p)
      (projectile-serialize new-cache (projectile-project-cache-file project-root)))
    ;; Re-derive watches so the purged directory is no longer watched (and its
    ;; files don't get re-added by a later create-event).
    (projectile--maybe-watch-project project-root new-cache)))

(defun projectile-file-cached-p (file project)
  "Check if FILE is already in PROJECT cache."
  (member file (gethash project projectile-projects-cache)))

(defun projectile--schedule-cache-flush (project)
  "Arrange for PROJECT's in-memory cache to be serialized after Emacs is idle.
A pending flush for the same PROJECT is cancelled and rescheduled, so that
adding several files in quick succession only results in a single disk write,
and the write always uses the latest in-memory contents."
  (when-let* ((existing (gethash project projectile--pending-cache-flush-timers)))
    (cancel-timer existing))
  (puthash project
           (run-with-idle-timer
            30 nil
            (lambda ()
              (remhash project projectile--pending-cache-flush-timers)
              (projectile-serialize
               (gethash project projectile-projects-cache)
               (projectile-project-cache-file project))))
           projectile--pending-cache-flush-timers))


;;; Automatic cache updates via file-notify watches
;;
;; Opt-in machinery (see `projectile-auto-update-cache-with-watches') that
;; keeps `projectile-projects-cache' in sync with the filesystem.  Emacs
;; file notifications are not recursive, so a watched project gets one
;; watch per directory, derived from the cached file list and bounded by
;; `projectile-watch-directory-limit'.  Events are debounced per project
;; and applied incrementally; anything that can't be applied safely falls
;; back to invalidating the project's cache, which rebuilds lazily and
;; re-arms the watches on the next cache fill.

(defun projectile--maybe-watch-project (project files)
  "Arm file-notify watches for PROJECT, whose cached file list is FILES.
No-op unless both `projectile-auto-update-cache-with-watches' and
`projectile-enable-caching' are non-nil.  Remote (TRAMP) projects are
never watched: registering one watch per directory would mean a remote
round-trip each, and most remote handlers don't support file
notifications anyway."
  (when (and projectile-auto-update-cache-with-watches
             projectile-enable-caching
             (not (file-remote-p project)))
    (projectile--watch-project project files)))

(defun projectile--watch-directories (project files)
  "Return the directories of PROJECT to watch, derived from cached FILES.
The result contains the project root and the directory chain of every
cached file, as absolute names with trailing slashes.  Directories with
no cached files below them (e.g. empty directories) are not included,
so files that later appear in them go unnoticed until the next full
re-index."
  (let ((dirs (make-hash-table :test 'equal)))
    (puthash (file-name-as-directory (expand-file-name project)) t dirs)
    (dolist (file files)
      (dolist (dir (projectile--directory-ancestors file))
        (puthash (expand-file-name dir project) t dirs)))
    (hash-table-keys dirs)))

(defun projectile--watch-make-callback (project)
  "Return a file-notify callback that queues events for PROJECT."
  (lambda (event) (projectile--handle-watch-event project event)))

(defun projectile--watch-skipped-once (project format-string &rest args)
  "Report (via FORMAT-STRING and ARGS) that PROJECT won't be watched.
The message is only emitted when `projectile-verbose' is non-nil, and
only once per project and session."
  (unless (gethash project projectile--watch-skipped-projects)
    (puthash project t projectile--watch-skipped-projects)
    (when projectile-verbose
      (apply #'message format-string args))))

(defun projectile--watch-project (project files)
  "Register file-notify watches for PROJECT, whose cached files are FILES.
One watch per directory, into `projectile--project-watches'.  Any
watches already registered for PROJECT are replaced.  Does nothing
beyond a one-time message when the project spans more directories than
`projectile-watch-directory-limit' or when the platform provides no
usable file notification backend."
  (projectile--unwatch-project project)
  (let ((dirs (projectile--watch-directories project files)))
    (if (> (length dirs) projectile-watch-directory-limit)
        (projectile--watch-skipped-once
         project
         "Projectile: not watching %s: %d directories exceed `projectile-watch-directory-limit' (%d)"
         project (length dirs) projectile-watch-directory-limit)
      (let ((callback (projectile--watch-make-callback project))
            (failed nil)
            watches)
        (dolist (dir dirs)
          (unless failed
            (condition-case nil
                (when (file-directory-p dir)
                  (push (cons (file-notify-add-watch dir '(change) callback)
                              dir)
                        watches))
              (error (setq failed t)))))
        (if (not failed)
            (puthash project watches projectile--project-watches)
          ;; No usable backend, or the OS ran out of watches: roll back the
          ;; partial registration and don't retry noisily on every cache fill.
          (dolist (entry watches)
            (ignore-errors (file-notify-rm-watch (car entry))))
          (projectile--watch-skipped-once
           project
           "Projectile: cannot watch %s (no file notification backend, or watch registration failed)"
           project))))))

(defun projectile--unwatch-project (project)
  "Remove all file-notify watches registered for PROJECT.
Queued events and the pending debounce timer are discarded too."
  (dolist (entry (gethash project projectile--project-watches))
    (ignore-errors (file-notify-rm-watch (car entry))))
  ;; Unconditional: a nil registry value (no watches left) must still be
  ;; removed, or it would survive `projectile--teardown-all-watches'.
  (remhash project projectile--project-watches)
  (when-let* ((timer (gethash project projectile--watch-debounce-timers)))
    (cancel-timer timer)
    (remhash project projectile--watch-debounce-timers))
  (remhash project projectile--watch-pending-events))

(defun projectile--teardown-all-watches ()
  "Drop the file-notify watches of every watched project.
Runs when `projectile-mode' is disabled, when
`projectile-auto-update-cache-with-watches' is customized to nil, and
on `kill-emacs'."
  (dolist (project (hash-table-keys projectile--project-watches))
    (projectile--unwatch-project project)))

(defun projectile--watch-all-cached-projects ()
  "Arm watches for every project that already has a cached file list.
Used when `projectile-auto-update-cache-with-watches' is enabled
mid-session, so already-cached projects don't have to wait for their
next cache fill."
  (maphash #'projectile--maybe-watch-project projectile-projects-cache))

(defun projectile--handle-watch-event (project event)
  "Queue file-notify EVENT for PROJECT and start the debounce timer.
Events from descriptors no longer in `projectile--project-watches' are
dropped; in particular the `stopped' event that `file-notify-rm-watch'
itself generates on some backends can't re-trigger processing after the
project was unwatched."
  (when (assoc (car event) (gethash project projectile--project-watches))
    (push event (gethash project projectile--watch-pending-events))
    (unless (gethash project projectile--watch-debounce-timers)
      (puthash project
               (run-with-timer projectile--watch-debounce-delay nil
                               #'projectile--process-watch-events project)
               projectile--watch-debounce-timers))))

(defun projectile--process-watch-events (project)
  "Apply PROJECT's queued file-notify events to its cached file list.
Runs from the debounce timer.  If any event can't be applied
incrementally the whole batch falls back to
`projectile--invalidate-project-cache' - correctness beats cleverness;
the cache rebuilds lazily on the next file listing, re-arming the
watches.  After successful mutations the persistent cache flush is
scheduled via `projectile--schedule-cache-flush'."
  (remhash project projectile--watch-debounce-timers)
  (let ((events (nreverse (gethash project projectile--watch-pending-events))))
    (remhash project projectile--watch-pending-events)
    (when (and events (gethash project projectile--project-watches))
      ;; The cache entry can vanish without an invalidation (e.g. the
      ;; `projectile-files-cache-expire' TTL drops it directly), leaving the
      ;; watches orphaned.  Mutating an absent cache would fabricate a bogus
      ;; one-file project, so just drop the watches; they re-arm when the
      ;; cache is next filled.  A present-but-empty file list is different:
      ;; that project is still watched and its events still apply.
      (if (eq (gethash project projectile-projects-cache 'projectile--none)
              'projectile--none)
          (projectile--unwatch-project project)
        (let* ((mutated nil)
               (fallback
                (catch 'projectile--watch-fallback
                  (dolist (event events)
                    (when (projectile--watch-apply-event project event)
                      (setq mutated t)))
                  nil)))
          (cond
           (fallback
            (when projectile-verbose
              (message "Projectile: invalidating the cache of %s (%s)"
                       project fallback))
            ;; Also drops the watches (and any events queued meanwhile).
            (projectile--invalidate-project-cache project))
           ((and mutated (projectile-persistent-cache-p))
            (projectile--schedule-cache-flush project))))))))

(defun projectile--watch-apply-event (project event)
  "Apply one file-notify EVENT to PROJECT's cached file list.
Returns non-nil when the cached list was mutated.  Throws
`projectile--watch-fallback' (with a reason string) when the event
cannot be applied incrementally."
  (pcase-let ((`(,descriptor ,action ,file . ,rest) event))
    (pcase action
      ('created (projectile--watch-handle-created project file))
      ('deleted (projectile--watch-handle-deleted project file))
      ;; A rename is a deletion at the old name plus a creation at the new
      ;; one.  `or' would short-circuit the second handler, so evaluate both.
      ('renamed
       (let ((removed (projectile--watch-handle-deleted project file))
             (added (and (car rest)
                         (projectile--watch-handle-created project (car rest)))))
         (or removed added)))
      ('stopped (projectile--watch-handle-stopped project descriptor))
      ;; `changed' / `attribute-changed' don't affect the file list.
      (_ nil))))

(defun projectile--watch-transient-file-p (file)
  "Return non-nil when FILE is an editor artifact that shouldn't be cached.
Matches lockfiles (.#foo), auto-save files (#foo#), backup files (foo~)
and the project's own persistent cache file - all of them appear and
vanish as a side effect of editing and would otherwise churn the cache
(the cache file would even schedule a flush that touches itself)."
  (let ((name (file-name-nondirectory (directory-file-name file))))
    (or (string-prefix-p ".#" name)
        (and (string-prefix-p "#" name) (string-suffix-p "#" name))
        (string-suffix-p "~" name)
        (string= name projectile-cache-file))))

(defun projectile--watch-keep-file-p (project file)
  "Return non-nil when FILE (relative to PROJECT) belongs in the file list.
Runs FILE through the same dirconfig and globally-ignored filtering the
native and hybrid indexers use, and respects dirconfig `+' keep
entries.  VCS-level ignores (e.g. `.gitignore') are not consulted, so
under alien indexing a watched project can temporarily gain entries the
VCS would have excluded, until the next full re-index."
  (let ((default-directory project))
    (and (projectile-remove-ignored (list file))
         (let ((dirs (projectile-get-project-directories project)))
           (or (member project dirs)
               (let ((absolute (expand-file-name file project)))
                 (seq-some (lambda (dir) (string-prefix-p dir absolute))
                           dirs)))))))

(defun projectile--watch-handle-created (project file)
  "Handle the creation of FILE (absolute) inside PROJECT.
Regular files go through the ignore filter into the cached file list;
directories are adopted via `projectile--watch-adopt-directory'.
Returns non-nil when the cached list was mutated."
  (cond
   ;; A `renamed' event can carry a destination outside the project (the
   ;; file was moved away, not renamed in place); caching it would insert
   ;; a bogus ../ entry into the file list.
   ((not (string-prefix-p (file-name-as-directory (expand-file-name project))
                          (expand-file-name file)))
    nil)
   ((projectile--watch-transient-file-p file) nil)
   ((file-directory-p file)
    (projectile--watch-adopt-directory project file))
   ((file-regular-p file)
    (let ((relative (file-relative-name file project)))
      (when (and (not (member relative
                              (gethash project projectile-projects-cache)))
                 (projectile--watch-keep-file-p project relative))
        (puthash project
                 (cons relative (gethash project projectile-projects-cache))
                 projectile-projects-cache)
        t)))
   ;; The path is already gone (created and deleted within one debounce
   ;; window), or something exotic like a socket: nothing to cache.
   (t nil)))

(defun projectile--watch-adopt-directory (project dir)
  "Watch DIR, a directory newly created inside PROJECT, and cache its files.
DIR may already have contents - e.g. a populated directory moved into
the project generates a single `created' event - so its entries are
enumerated: regular files are handed to
`projectile--watch-handle-created' and subdirectories are adopted
recursively.  Ignored directories are skipped entirely.  Returns
non-nil when the cached file list was mutated.  Throws
`projectile--watch-fallback' when DIR can't be watched (the watch limit
was reached, or the backend refused)."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (relative (file-relative-name dir project))
         (watches (gethash project projectile--project-watches)))
    (cond
     ;; Already watched (duplicate or overlapping events): nothing to do.
     ((rassoc dir watches) nil)
     ;; Don't descend into ignored directories.
     ((let ((default-directory project))
        (null (projectile-remove-ignored (list relative))))
      nil)
     ((>= (length watches) projectile-watch-directory-limit)
      (throw 'projectile--watch-fallback
             (format "new directory %s would exceed `projectile-watch-directory-limit'"
                     relative)))
     (t
      (let ((descriptor
             (condition-case err
                 (file-notify-add-watch
                  dir '(change) (projectile--watch-make-callback project))
               (error (throw 'projectile--watch-fallback
                             (error-message-string err))))))
        (puthash project (cons (cons descriptor dir) watches)
                 projectile--project-watches))
      (let ((mutated nil))
        ;; Enumerating the new directory races against whatever is still
        ;; populating (or already removing) it; an IO error here must not
        ;; leave the batch half-applied, so convert it into the fallback.
        ;; A nested `projectile--watch-fallback' throw is not an error
        ;; condition and passes through untouched.
        (condition-case err
            (dolist (entry (directory-files
                            dir t directory-files-no-dot-files-regexp t))
              (when (projectile--watch-handle-created project entry)
                (setq mutated t)))
          (error (throw 'projectile--watch-fallback
                        (error-message-string err))))
        mutated)))))

(defun projectile--watch-handle-deleted (project file)
  "Handle the deletion of FILE (absolute) inside PROJECT.
FILE no longer exists, so whether it was a file or a directory can't be
queried; cached entries at the path and below it are removed, and so
are the watches of any directories below it.  Returns non-nil when the
cached list was mutated.  Throws `projectile--watch-fallback' when FILE
is the project root itself - there is nothing left to watch, so the
whole cache is invalidated instead."
  (let* ((relative (file-relative-name file project))
         (dir-absolute (file-name-as-directory (expand-file-name file)))
         (dir-relative (file-name-as-directory relative))
         (files (gethash project projectile-projects-cache)))
    (when (string= dir-absolute
                   (file-name-as-directory (expand-file-name project)))
      (throw 'projectile--watch-fallback "the project root itself was deleted"))
    ;; Drop the watches under the deleted path.  Their backends may emit a
    ;; `stopped' event on removal; by then the descriptors are no longer
    ;; registered, so `projectile--handle-watch-event' discards it.
    (let ((watches (gethash project projectile--project-watches))
          (remaining nil))
      (dolist (entry watches)
        (if (string-prefix-p dir-absolute (cdr entry))
            (ignore-errors (file-notify-rm-watch (car entry)))
          (push entry remaining)))
      ;; `remaining' always contains at least the root watch here (only a
      ;; root deletion prunes everything, and that threw above), but never
      ;; store nil: a nil registry value would read as "not watched".
      (if remaining
          (puthash project (nreverse remaining) projectile--project-watches)
        (remhash project projectile--project-watches)))
    (let ((new-files (seq-remove
                      (lambda (f)
                        (or (string= f relative)
                            (string-prefix-p dir-relative f)))
                      files)))
      (unless (= (length new-files) (length files))
        (puthash project new-files projectile-projects-cache)
        t))))

(defun projectile--watch-handle-stopped (project descriptor)
  "Handle DESCRIPTOR's watch stopping in PROJECT.
When the watched directory is gone this is just the tail end of a
deletion that the parent directory's watch already reported, so only
the bookkeeping entry is dropped.  When the directory still exists the
watch died under us (backend hiccup, event queue overflow) and the
cache can no longer be trusted, so throw to trigger invalidation.
Always returns nil - the cached file list itself is not touched."
  (let* ((watches (gethash project projectile--project-watches))
         (entry (assoc descriptor watches)))
    (when entry
      (let ((remaining (delq entry watches)))
        (if remaining
            (puthash project remaining projectile--project-watches)
          ;; Never store nil - it would read as "not watched".
          (remhash project projectile--project-watches)))
      ;; The root has no watched parent to report its deletion, so a stopped
      ;; root watch always invalidates, whether the directory survived or not.
      (when (or (file-directory-p (cdr entry))
                (string= (cdr entry)
                         (file-name-as-directory (expand-file-name project))))
        (throw 'projectile--watch-fallback
               (format "the watch on %s stopped unexpectedly" (cdr entry))))))
  nil)

;;;###autoload
(defun projectile-cache-current-file (&optional project-root)
  "Add the currently visited file to the cache.
PROJECT-ROOT defaults to the current project."
  (interactive)
  (let ((current-project (or project-root (projectile-project-root))))
    (when (and (buffer-file-name)
               (file-exists-p (buffer-file-name))
               (gethash current-project projectile-projects-cache))
      (let* ((abs-current-file (file-truename (buffer-file-name)))
             (current-file (file-relative-name abs-current-file current-project)))
        (unless (or (projectile-file-cached-p current-file current-project)
                    (projectile-ignored-directory-p (file-name-directory abs-current-file))
                    (projectile-ignored-file-p abs-current-file))
          (let ((project-files (cons current-file (gethash current-project projectile-projects-cache))))
            (puthash current-project project-files projectile-projects-cache)
            ;; Defer the disk write until Emacs is idle to avoid freezing the
            ;; UI immediately after the new file was created.
            (when (projectile-persistent-cache-p)
              (projectile--schedule-cache-flush current-project)))
          (message "File %s added to project %s cache."
                   (propertize current-file 'face 'font-lock-keyword-face)
                   (propertize current-project 'face 'font-lock-keyword-face)))))))

;; cache opened files automatically to reduce the need for cache invalidation
(defun projectile-cache-files-find-file-hook (&optional project-root)
  "Function for caching files with `find-file-hook'.
PROJECT-ROOT defaults to the current project."
  (let ((project-root (or project-root (projectile-project-p))))
    (when (and projectile-enable-caching
               project-root
               (not (projectile-ignored-project-p project-root)))
      (projectile-cache-current-file project-root))))

(defun projectile-track-known-projects-find-file-hook (&optional project-root)
  "Function for caching projects with `find-file-hook'.
PROJECT-ROOT defaults to the current project."
  (when projectile-track-known-projects-automatically
    (when-let* ((project-root (or project-root (projectile-project-p))))
      (projectile-add-known-project project-root))))

(defvar projectile--current-project nil
  "The project root `projectile-project-changed-functions' last saw.")

(defun projectile--maybe-run-project-changed-functions (&optional project-root)
  "Run `projectile-project-changed-functions' when the project changed.
PROJECT-ROOT defaults to the current project.  The last seen project
is tracked in `projectile--current-project'."
  (when projectile-project-changed-functions
    (when-let* ((project-root (or project-root (projectile-project-p))))
      (unless (equal project-root projectile--current-project)
        (let ((previous projectile--current-project))
          (setq projectile--current-project project-root)
          (run-hook-with-args 'projectile-project-changed-functions
                              project-root previous))))))

(defun projectile-maybe-invalidate-cache (force)
  "Invalidate if FORCE or project's dirconfig newer than cache."
  (when (or force (file-newer-than-file-p (projectile-dirconfig-file)
                                          (projectile-project-cache-file)))
    (projectile-invalidate-cache nil)))


;;; File frecency
;;
;; Tracks which project files are visited and how often, so that
;; `projectile-find-file' (and friends) can rank the files you actually
;; work with first.  The ranking is applied through completion metadata
;; (`display-sort-function'), so it works with any completion UI that
;; honors it (the default completion UI, Vertico, Icomplete, ...) and
;; under every indexing method, including `alien'.

(defcustom projectile-enable-frecency t
  "When non-nil, rank project files by frecency in completion.
Projectile records file visits per project and sorts completion
candidates by a combination of visit frequency and recency, so the
files you work with the most show up first in `projectile-find-file'
and related commands.

The history is persisted in `projectile-frecency-file'."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "3.1.0"))

(defcustom projectile-frecency-file
  (expand-file-name "projectile-frecency.eld" user-emacs-directory)
  "File where Projectile persists the per-project file visit history."
  :group 'projectile
  :type 'file
  :package-version '(projectile . "3.1.0"))

(defcustom projectile-frecency-max-files 200
  "Maximum number of files tracked per project.
When the limit is exceeded, the lowest-ranking entries are dropped."
  :group 'projectile
  :type 'natnum
  :package-version '(projectile . "3.1.0"))

(defcustom projectile-frecency-max-projects 100
  "Maximum number of projects whose frecency history is kept.
When the store holds more roots than this, the least recently active
ones are dropped on save, so the history can't grow without bound as
projects come and go."
  :group 'projectile
  :type 'natnum
  :package-version '(projectile . "3.2.0"))

(defvar projectile--frecency-table nil
  "Hash of project root to a hash of relative file name to (COUNT . TIME).
TIME is the last visit in seconds since the epoch.  nil until loaded
from `projectile-frecency-file' by `projectile--frecency-data'.")

(defvar projectile--frecency-dirty nil
  "Non-nil when the frecency data changed since it was last saved.")

(defun projectile--frecency-data ()
  "Return the frecency table, loading it from disk on first use.
A malformed history file yields an empty table with a warning; it
must never break `find-file' (the recording hook would re-signal on
every file visit otherwise)."
  (or projectile--frecency-table
      (setq projectile--frecency-table
            (condition-case err
                (let ((table (make-hash-table :test 'equal)))
                  (dolist (project (projectile-unserialize
                                    projectile-frecency-file))
                    (let ((files (make-hash-table :test 'equal)))
                      (dolist (entry (cdr project))
                        (pcase-let ((`(,file ,count ,time) entry))
                          (puthash file (cons count time) files)))
                      (puthash (car project) files table)))
                  table)
              (error
               (display-warning
                'projectile
                (format "Malformed frecency file '%s' ignored (%s)"
                        projectile-frecency-file (error-message-string err))
                :warning)
               (make-hash-table :test 'equal))))))

(defun projectile--frecency-score (entry now)
  "Compute the frecency score of ENTRY (COUNT . TIME) at time NOW.
The visit count decays with a half-life of two weeks, so a file
visited often long ago eventually ranks below one visited recently."
  (let ((age-days (/ (max 0 (- now (cdr entry))) 86400.0)))
    (* (car entry) (expt 0.5 (/ age-days 14.0)))))

(defun projectile--frecency-prune (files)
  "Drop the lowest-scoring entries of FILES down to the configured limit."
  (let ((now (projectile-time-seconds))
        (entries nil))
    (maphash (lambda (file entry)
               (push (cons file (projectile--frecency-score entry now)) entries))
             files)
    (dolist (victim (nthcdr projectile-frecency-max-files
                            (seq-sort-by #'cdr #'> entries)))
      (remhash (car victim) files))))

(defun projectile--frecency-record (project-root)
  "Record a visit to the current buffer's file under PROJECT-ROOT.
Remote projects are not tracked, to keep the visit hook free of
TRAMP round-trips."
  (when (and projectile-enable-frecency
             project-root
             buffer-file-name
             (not (file-remote-p project-root)))
    ;; `projectile-project-root' is symlink-resolved, so resolve the
    ;; visited file the same way; otherwise a project reached through a
    ;; symlink produces a `../'-relative name and tracking is dropped for
    ;; the whole project.  The recorded name then also matches the
    ;; completion candidates, which are relative to the resolved root.
    (let ((file (projectile--project-relative-name
                 (file-truename buffer-file-name) project-root)))
      ;; Still skip anything genuinely outside the project.
      (unless (string-prefix-p ".." file)
        (let* ((table (projectile--frecency-data))
               (files (or (gethash project-root table)
                          (puthash project-root
                                   (make-hash-table :test 'equal) table)))
               (entry (gethash file files)))
          (puthash file (cons (1+ (or (car entry) 0))
                              (projectile-time-seconds))
                   files)
          (setq projectile--frecency-dirty t)
          (when (> (hash-table-count files) (* 2 projectile-frecency-max-files))
            (projectile--frecency-prune files)))))))

(defun projectile--frecency-sort-function (project-root)
  "Return a completion sort function ranking PROJECT-ROOT's files, or nil.
The returned function puts tracked files first, ordered by
`projectile--frecency-score', and preserves the order of the rest.
Return nil when frecency is disabled or nothing is tracked yet."
  (when (and projectile-enable-frecency project-root)
    (when-let* ((files (gethash project-root (projectile--frecency-data))))
      (when (> (hash-table-count files) 0)
        (let ((scores (make-hash-table :test 'equal
                                       :size (hash-table-count files)))
              (now (projectile-time-seconds)))
          (maphash (lambda (file entry)
                     (puthash file (projectile--frecency-score entry now)
                              scores))
                   files)
          (lambda (candidates)
            (let (frecent rest)
              (dolist (cand candidates)
                (if (gethash cand scores)
                    (push cand frecent)
                  (push cand rest)))
              (nconc (sort (nreverse frecent)
                           (lambda (a b)
                             (> (gethash a scores) (gethash b scores))))
                     (nreverse rest)))))))))

(defun projectile--frecency-merge-from-disk ()
  "Merge newer on-disk frecency entries into the in-memory table.
Another Emacs session may have saved since we loaded, so a plain
overwrite would discard its data (the same reason
`projectile-merge-known-projects' exists).  For each file present in
both, the higher visit count and the later timestamp win."
  (let ((disk-table (let ((projectile--frecency-table nil))
                      (projectile--frecency-data)))
        (table projectile--frecency-table))
    (maphash
     (lambda (root disk-files)
       (let ((files (or (gethash root table)
                        (puthash root (make-hash-table :test 'equal) table))))
         (maphash
          (lambda (file disk-entry)
            (let ((entry (gethash file files)))
              (puthash file
                       (if entry
                           (cons (max (car entry) (car disk-entry))
                                 (max (cdr entry) (cdr disk-entry)))
                         disk-entry)
                       files)))
          disk-files)))
     disk-table)))

(defun projectile--frecency-cap-projects (data)
  "Return DATA capped at `projectile-frecency-max-projects' roots.
DATA is an alist of (ROOT . FILE-ENTRIES), each FILE-ENTRY a
\(FILE COUNT TIME) list.  The most recently active roots (highest file
timestamp) are kept and the rest dropped, so the store can't accumulate
dead roots without bound."
  (if (<= (length data) projectile-frecency-max-projects)
      data
    (let ((ranked
           (sort (copy-sequence data)
                 (lambda (a b)
                   (> (apply #'max 0 (mapcar (lambda (fe) (or (nth 2 fe) 0)) (cdr a)))
                      (apply #'max 0 (mapcar (lambda (fe) (or (nth 2 fe) 0)) (cdr b))))))))
      (seq-take ranked projectile-frecency-max-projects))))

(defun projectile--frecency-save ()
  "Persist the frecency data to `projectile-frecency-file'.
Merges with the data on disk first, so concurrent Emacs sessions
don't wipe out each other's history.  The dirty flag is kept when
the file isn't writable, so a later save can retry."
  (when (and projectile--frecency-dirty projectile--frecency-table)
    (if (not (file-writable-p projectile-frecency-file))
        (display-warning
         'projectile
         (format "Frecency file '%s' is not writable" projectile-frecency-file)
         :warning)
      (projectile--frecency-merge-from-disk)
      (let (data)
        (maphash
         (lambda (root files)
           (projectile--frecency-prune files)
           (when (> (hash-table-count files) 0)
             (let (file-entries)
               (maphash (lambda (file entry)
                          (push (list file (car entry) (cdr entry))
                                file-entries))
                        files)
               (push (cons root file-entries) data))))
         projectile--frecency-table)
        (projectile-serialize (projectile--frecency-cap-projects data)
                              projectile-frecency-file))
      (setq projectile--frecency-dirty nil))))

;;;###autoload
(defun projectile-discover-projects-in-directory (directory &optional depth)
  "Discover any projects in DIRECTORY and add them to the projectile cache.

If DEPTH is non-nil recursively descend exactly DEPTH levels below DIRECTORY and
discover projects there."
  (interactive
   (list (read-directory-name "Starting directory: ")))

  ;; set a default value for depth
  (setq depth (or depth 1))

  (if (file-directory-p directory)
      (if (and (numberp depth) (> depth 0))
          ;; Ignore errors when listing files in the directory, because
          ;; sometimes that directory is an unreadable one at the root of a
          ;; volume. This is the case, for example, on macOS with the
          ;; .Spotlight-V100 directory.
          (let ((progress-reporter
                 (make-progress-reporter
                  (format "Projectile is discovering projects in %s..."
                          (propertize directory 'face 'font-lock-keyword-face)))))
            (progress-reporter-update progress-reporter)
            (dolist (dir (ignore-errors
                           (directory-files directory t
                                            directory-files-no-dot-files-regexp)))
              (when (and (file-directory-p dir)
                         ;; Don't walk into remote trees during discovery -
                         ;; that would issue a TRAMP round-trip per directory.
                         (not (file-remote-p dir)))
                (projectile-discover-projects-in-directory dir (1- depth))))
            (progress-reporter-done progress-reporter))
        (when (projectile-project-p directory)
          (let ((dir (projectile--known-project-root (projectile-project-root directory))))
            (unless (member dir projectile-known-projects)
              (projectile-add-known-project dir)))))
    (message "Project search path directory %s doesn't exist" directory)))

(defvar projectile--search-path-discovered nil
  "Non-nil once `projectile-project-search-path' has been auto-discovered.
Used to run automatic discovery once per session instead of on every
project-switch command.")

(defun projectile-discover-projects-in-search-path ()
  "Discover projects in `projectile-project-search-path'.
When called interactively, always re-scans; the automatic scan (see
`projectile-auto-discover') runs this once per session."
  (interactive)
  (setq projectile--search-path-discovered t)
  (dolist (path projectile-project-search-path)
    ;; Skip remote entries: discovery would walk them over TRAMP.
    (unless (file-remote-p (if (consp path) (car path) path))
      (if (consp path)
          (projectile-discover-projects-in-directory (car path) (cdr path))
        (projectile-discover-projects-in-directory path 1)))))


(defun delete-file-projectile-remove-from-cache (filename &optional _trash)
  (if (and projectile-enable-caching projectile-auto-update-cache (projectile-project-p))
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

(defun projectile--directory-entry-set (directory)
  "Return a hash set of the immediate entry names of DIRECTORY, or nil.
A single `directory-files' call replaces one `file-exists-p' per
candidate name - over TRAMP that turns N sequential remote round-trips
into one.  Returns nil when DIRECTORY can't be listed (missing or
permission denied) or contains no entries.

Note: membership is by directory entry, not `file-exists-p', so a broken
symlink named like a marker counts as present here (the old per-candidate
`file-exists-p' followed the link and returned nil).  This is harmless in
practice - a project marker that is a dangling symlink is a pathological
setup."
  (when-let* ((entries (ignore-errors
                         (directory-files
                          directory nil directory-files-no-dot-files-regexp t))))
    (let ((set (make-hash-table :test 'equal :size (length entries))))
      (dolist (entry entries) (puthash entry t set))
      set)))

(defun projectile--locate-dominating-file (file name first-match-only)
  "Walk up from FILE looking for NAME and return the matching directory.
NAME is either a filename (matched via `projectile-file-exists-p' in
each candidate directory) or a predicate of one argument (the candidate
directory).

When FIRST-MATCH-ONLY is non-nil, return the bottommost (closest to
FILE) match; otherwise keep walking and return the topmost match.
Returns nil when no match is found."
  ;; The walk skeleton was originally copied from files.el; the bottom-up
  ;; / top-down split was previously two near-identical functions.
  (setq file (abbreviate-file-name file))
  (let ((root nil)
        try)
    (while (and file
                (not (string-match locate-dominating-stop-dir-regexp file))
                (not (and first-match-only root)))
      (setq try (if (stringp name)
                    (projectile-file-exists-p
                     (projectile-expand-file-name-wildcard name file))
                  (funcall name file)))
      (when try (setq root file))
      (let ((parent (file-name-directory (directory-file-name file))))
        (setq file (and (not (equal file parent)) parent))))
    (and root (expand-file-name (file-name-as-directory root)))))

(defun projectile-locate-dominating-file (file name)
  "Look up the directory hierarchy from FILE for a directory containing NAME.
Stop at the first parent directory containing a file NAME,
and return the directory.  Return nil if not found.
Instead of a string, NAME can also be a predicate taking one argument
\(a directory) and returning a non-nil value if that directory is the one for
which we're looking."
  (projectile--locate-dominating-file file name t))

(defun projectile-locate-dominating-file-top-down (file name)
  "Look up the directory hierarchy from FILE for a directory containing NAME.
Unlike `projectile-locate-dominating-file' which returns the first (bottommost)
match, this returns the topmost match.  Return nil if not found.
Instead of a string, NAME can also be a predicate taking one argument
\(a directory) and returning a non-nil value if that directory is the one for
which we're looking."
  (projectile--locate-dominating-file file name nil))

(defvar-local projectile-project-root nil
  "Defines a custom Projectile project root.
This is intended to be used as a file local variable.")

(defun projectile-root-local (_dir)
  "A simple wrapper around the variable `projectile-project-root'."
  projectile-project-root)

(defun projectile-root-top-down (dir &optional list)
  "Identify a project root in DIR by top-down search for files in LIST.
If LIST is nil, use `projectile-project-root-files' instead.
Return the first (topmost) matched directory or nil if not found."
  (projectile-locate-dominating-file-top-down
   dir
   (lambda (dir)
     (seq-find (lambda (f)
                    (let ((expanded (projectile-expand-file-name-wildcard f dir)))
                      (and (projectile-file-exists-p expanded)
                           (not (file-directory-p expanded)))))
                 (or list projectile-project-root-files)))))

(defun projectile-root-marked (dir)
  "Identify a project root in DIR by search for `projectile-dirconfig-file`."
  (projectile-root-bottom-up dir (list projectile-dirconfig-file)))

(defun projectile-root-bottom-up (dir &optional list)
  "Identify a project root in DIR by bottom-up search for files in LIST.
If LIST is nil, use `projectile-project-root-files-bottom-up' instead.
Return the first (bottommost) matched directory or nil if not found."
  (let ((markers (or list projectile-project-root-files-bottom-up)))
    (projectile-locate-dominating-file
     dir
     (if (and (null (cdr markers))
              (stringp (car markers))
              (not (string-match-p "/" (car markers))))
         ;; With a single marker a directory listing can't beat one stat
         ;; per level, so probe it directly.  `projectile-root-marked'
         ;; (which runs on every root resolution) is this case.
         (lambda (directory)
           (projectile-file-exists-p (expand-file-name (car markers) directory)))
       (lambda (directory)
         ;; Probe each level with a single `directory-files' listing rather
         ;; than one `file-exists-p' per marker.  The default markers are all
         ;; plain names sitting directly in the directory, so membership in
         ;; the listing is equivalent; markers carrying a path separator
         ;; (a user customization) can't be answered from the basename listing
         ;; and fall back to `projectile-file-exists-p'.  The listing is
         ;; fetched lazily, once, and only if a plain-name marker is reached.
         (let ((entries nil) (listed nil))
           (seq-some
            (lambda (marker)
              (cond
               ((not marker) nil)
               ((string-match-p "/" marker)
                (projectile-file-exists-p (expand-file-name marker directory)))
               (t (unless listed
                    (setq entries (projectile--directory-entry-set directory)
                          listed t))
                  (and entries (gethash marker entries)))))
            markers)))))))

(defun projectile-root-top-down-recurring (dir &optional list)
  "Identify a project root in DIR by recurring top-down search for files in LIST.
If LIST is nil, use `projectile-project-root-files-top-down-recurring'
instead.  Return the last (bottommost) matched directory in the
topmost sequence of matched directories.  Nil otherwise."
  (seq-some
   (lambda (f)
     (projectile-locate-dominating-file
      dir
      (lambda (dir)
        (and (projectile-file-exists-p (projectile-expand-file-name-wildcard f dir))
             (or (string-match locate-dominating-stop-dir-regexp (projectile-parent dir))
                 (not (projectile-file-exists-p (projectile-expand-file-name-wildcard f (projectile-parent dir)))))))))
   (or list projectile-project-root-files-top-down-recurring)))

(defun projectile-project-root (&optional dir)
  "Return the root directory of the project containing DIR, or nil.
If DIR is not supplied it defaults to `default-directory'.

Each function in `projectile-project-root-functions' is tried in order;
the first non-nil result wins.  Results - including failures - are
memoized in `projectile-project-root-cache' (see the Project root cache
section in the manual).  Use `projectile-invalidate-cache' to reset.

Special cases:

- Tramp archive paths (e.g. inside a `.zip') are unwrapped to the
  directory that contains the archive before searching.
- Remote files reached via TRAMP whose host is not currently connected
  return nil without caching, so reconnecting works without manual cache
  invalidation."
  ;; `default-directory' can be nil in some buffers; short-circuit to nil so
  ;; callers get "no project" instead of a `(wrong-type-argument stringp nil)'
  ;; from `file-remote-p' and friends below (#1829).
  (when-let* ((dir (or dir default-directory)))
    ;; Back out of any archives, the project will live on the outside and
    ;; searching them is slow.
    (when (and (fboundp 'tramp-archive-file-name-p)
               (tramp-archive-file-name-p dir))
      (setq dir (file-name-directory (tramp-archive-file-name-archive dir))))
    ;; The cached value is 'none when no project root was found (so we don't
    ;; reevaluate every time when not inside a project); we map that back to
    ;; nil for callers.  Cache keys are conses: (FUNC . DIR) for per-function
    ;; results, ('none . DIR) for the overall failure marker.
    (let ((result (or
       ;; if we've already failed to find a project dir for this
       ;; dir, and cached that failure, don't recompute
       (gethash (cons 'none dir) projectile-project-root-cache)
       ;; if the file isn't local, and we're not connected, don't try to
       ;; find a root now, but don't cache failure, as we might
       ;; re-connect.  The `is-local' and `is-connected' variables are
       ;; used to fix the behavior where Emacs hangs because of
       ;; Projectile when you open a file over TRAMP. It basically
       ;; prevents Projectile from trying to find information about
       ;; files for which it's not possible to get that information
       ;; right now.
       (let ((is-local (not (file-remote-p dir)))      ;; `true' if the file is local
             (is-connected (file-remote-p dir nil t))) ;; `true' if the file is remote AND we are connected to the remote
         (unless (or is-local is-connected)
           'none))
       ;; if the file is local or we're connected to it via TRAMP, run
       ;; through the project root functions until we find a project dir.
       ;; `projectile-root-local' reads a buffer-local variable rather
       ;; than inspecting DIR, so its result must not be cached - two
       ;; buffers in the same directory can legitimately disagree.
       ;; For other functions, both successes and per-function failures
       ;; (stored as the 'none sentinel) are memoized, so functions
       ;; earlier in the list that returned nil aren't re-walked on
       ;; every call.
       ;;
       ;; `true-dir-cell' lazily memoizes `(file-truename dir)' across
       ;; the loop so we pay the (potentially remote) symlink resolution
       ;; at most once per `projectile-project-root' call instead of
       ;; once per project-root-function on cache miss.
       (let ((true-dir-cell (list nil)))
         (seq-some
          (lambda (func)
            (if (eq func 'projectile-root-local)
                (funcall func dir)
              (let* ((cache-key (cons func dir))
                     (cache-value (gethash cache-key projectile-project-root-cache)))
                (cond
                 ((eq cache-value 'none) nil)
                 ;; Use `projectile-file-exists-p' so the remote
                 ;; stat is cached (per `projectile-file-exists-remote-cache-expire')
                 ;; instead of round-tripping on every call.
                 ((and cache-value (projectile-file-exists-p cache-value)) cache-value)
                 (t (let ((value (funcall
                                  func
                                  (or (car true-dir-cell)
                                      (setcar true-dir-cell (file-truename dir))))))
                      (puthash cache-key (or value 'none) projectile-project-root-cache)
                      value))))))
          projectile-project-root-functions))
       ;; if we get here, we have failed to find a root by all
       ;; conventional means, and we assume the failure isn't transient
       ;; / network related, so cache the failure
       (puthash (cons 'none dir) 'none projectile-project-root-cache))))
      (unless (eq result 'none) result))))

(defun projectile-ensure-project (dir)
  "Ensure that DIR is non-nil.
Useful for commands that expect the presence of a project.
Controlled by `projectile-require-project-root'.

See also `projectile-acquire-root'."
  (if dir
      dir
    (cond
     ((eq projectile-require-project-root 'prompt) (projectile-completing-read
                                                    "Switch to project: " projectile-known-projects
                                                    :category 'file
                                                    :caller 'projectile-read-project))
     (projectile-require-project-root (user-error "Projectile cannot find a project definition in %s" default-directory))
     (t default-directory))))

(defun projectile-acquire-root (&optional dir)
  "Find the current project root, and prompts the user for it if that fails.
Provides the common idiom (projectile-ensure-project (projectile-project-root)).
Starts the search for the project with DIR."
  (projectile-ensure-project (projectile-project-root dir)))

(defun projectile-project-p (&optional dir)
  "Check if DIR is a project.
Defaults to the current directory if not provided
explicitly."
  (projectile-project-root (or dir default-directory)))

(defun projectile-default-project-name (project-root)
  "Default function used to create the project name.
The project name is based on the value of PROJECT-ROOT."
  (file-name-nondirectory (directory-file-name project-root)))

(defun projectile-project-name (&optional project)
  "Return project name.
If PROJECT is not specified acts on the current project."
  (or projectile-project-name
      (let ((project-root (or project (projectile-project-root))))
        (if project-root
            (funcall projectile-project-name-function project-root)
          "-"))))


(defun projectile-uniquify-dirname-transform (dirname)
  "Project-aware transform for `uniquify-dirname-transform'.
When DIRNAME is inside a project, return a path with the project name
spliced in, so buffers visiting same-named files in different projects
get distinct, project-qualified names.  Outside a project DIRNAME is
returned unchanged.

To enable, set `uniquify-dirname-transform' to this function:

    (setq uniquify-dirname-transform #\\='projectile-uniquify-dirname-transform)"
  (if-let* ((root (projectile-project-root dirname)))
      (expand-file-name
       (file-name-concat
        (file-name-directory root)
        (projectile-project-name root)
        (file-relative-name dirname root)))
    dirname))


;;; Project indexing
(defun projectile-get-project-directories (project-dir)
  "Get the list of PROJECT-DIR directories that are of interest to the user.
When the dirconfig file has no `+' keep entries, return a single-
element list with PROJECT-DIR itself."
  (let* ((cfg (projectile-parse-dirconfig-file))
         (keep (and cfg (projectile-dirconfig-keep cfg))))
    (if keep
        (mapcar (lambda (subdir) (concat project-dir subdir)) keep)
      (list project-dir))))

(defun projectile--directory-p (directory)
  "Checks if DIRECTORY is a string designating a valid directory."
  (and (stringp directory) (file-directory-p directory)))

(defun projectile-dir-files (directory)
  "List the files in DIRECTORY and in its sub-directories.
Files are returned as relative paths to DIRECTORY."
  (unless (projectile--directory-p directory)
    (user-error "Directory %S does not exist" directory))
  ;; check for a cache hit first if caching is enabled
  (let ((files-list (and projectile-enable-caching
                         (gethash directory projectile-projects-cache))))
    ;; cache disabled or cache miss
    (or files-list
        (pcase projectile-indexing-method
          ('native (projectile-dir-files-native directory))
          ;; use external tools to get the project files
          ('hybrid (let ((vcs (projectile-project-vcs directory)))
                     (projectile-adjust-files directory vcs
                                              (projectile-dir-files-alien directory vcs))))
          ('alien (projectile-dir-files-alien directory))
          (_ (user-error "Unsupported indexing method `%S'" projectile-indexing-method))))))

;;; Native Project Indexing
;;
;; This corresponds to `projectile-indexing-method' being set to native.
(defun projectile-dir-files-native (directory)
  "Get the files under DIRECTORY using just Emacs Lisp."
  (let ((progress-reporter
         (make-progress-reporter
          (format "Projectile is indexing %s"
                  (propertize directory 'face 'font-lock-keyword-face))))
        ;; The walker returns absolute paths that all share DIRECTORY as a
        ;; literal prefix - `directory-files-and-attributes' expands each
        ;; entry against the expanded directory.  Stripping that prefix with
        ;; a single `substring' is equivalent to `file-relative-name' here
        ;; but avoids its per-file `expand-file-name'/`abbreviate-file-name'
        ;; cost, which otherwise dominates the post-walk step on large trees.
        (prefix-len (length (file-name-as-directory (expand-file-name directory)))))
    ;; we need the files with paths relative to the project root
    (mapcar (lambda (file) (substring file prefix-len))
            (projectile-index-directory directory (projectile-filtering-patterns)
                                        progress-reporter))))

(defun projectile--list->set (list)
  "Return a hash table whose keys are the elements of LIST.
Values are all `t'.  Tests with `equal'."
  (let ((set (make-hash-table :test 'equal :size (max 1 (length list)))))
    (dolist (elt list set)
      (puthash elt t set))))

(defun projectile--make-walk-rules (ignored-files ignored-directories globally-ignored-directories)
  "Build a plist of pre-computed rule sets used by `projectile-index-directory'.
IGNORED-FILES and IGNORED-DIRECTORIES are the absolute paths to
ignore; GLOBALLY-IGNORED-DIRECTORIES is the list of directory
basenames to ignore anywhere in the tree."
  (list :ignored-files-set (projectile--list->set ignored-files)
        :ignored-dirs-set (projectile--list->set ignored-directories)
        :globally-ignored-dir-names-set
        (projectile--list->set globally-ignored-directories)))

(defun projectile--ignored-file-fast-p (file rules)
  "Like `projectile-ignored-file-p' but consulting pre-built RULES.
Used on the hot indexing path to avoid O(N*M) `member' scans."
  (or (gethash file (plist-get rules :ignored-files-set))
      (seq-some (lambda (re) (string-match-p re file))
                projectile-global-ignore-file-patterns)
      (seq-some (lambda (suf) (string-suffix-p suf file t))
                projectile-globally-ignored-file-suffixes)))

(defun projectile--ignored-directory-fast-p (directory local-name rules)
  "Like `projectile-ignored-directory-p' but consulting pre-built RULES.
LOCAL-NAME is the basename of DIRECTORY."
  (or (gethash directory (plist-get rules :ignored-dirs-set))
      (seq-some (lambda (re) (string-match-p re directory))
                projectile-global-ignore-file-patterns)
      (gethash local-name (plist-get rules :globally-ignored-dir-names-set))))

(defun projectile--glob-to-regexp (glob)
  "Translate the dirconfig GLOB into a regexp fragment.
`*' matches within a path segment, `**' spans segments, `?' matches
a single non-slash character and `[...]'/`[!...]' character classes
pass through (with `!' translated to `^')."
  (let ((i 0) (n (length glob)) (fragments nil))
    (while (< i n)
      (let ((c (aref glob i)))
        (cond
         ((eq c ?*)
          (if (and (< (1+ i) n) (eq (aref glob (1+ i)) ?*))
              (progn (push ".*" fragments) (setq i (1+ i)))
            (push "[^/]*" fragments)))
         ((eq c ??) (push "[^/]" fragments))
         ((eq c ?\[)
          ;; Copy a character class through, translating glob's [!...]
          ;; negation; an unterminated class is treated literally.
          (if-let* ((end (string-search "]" glob (+ i 2))))
              (progn
                (push (if (and (< (1+ i) n) (eq (aref glob (1+ i)) ?!))
                          (concat "[^" (substring glob (+ i 2) (1+ end)))
                        (substring glob i (1+ end)))
                      fragments)
                (setq i end))
            (push "\\[" fragments)))
         (t (push (regexp-quote (char-to-string c)) fragments))))
      (setq i (1+ i)))
    (apply #'concat (nreverse fragments))))

(defun projectile--dirconfig-pattern-to-regexp (pattern)
  "Translate a dirconfig ignore/ensure PATTERN into a regexp.
The regexp matches root-relative paths using gitignore-like rules:
a pattern without a slash matches the file name or any directory
segment anywhere in the tree, while a pattern containing a slash is
anchored at the project root.  A trailing slash restricts the match
to directories (and thus everything below them).  Directories must
be matched with a trailing slash appended."
  (let* ((dir-only (string-suffix-p "/" pattern))
         (pattern (string-remove-suffix "/" pattern))
         (floating (string-prefix-p "**/" pattern))
         (pattern (if floating (substring pattern 3) pattern))
         (anchored (string-search "/" pattern)))
    (concat (cond (floating "\\`\\(?:.*/\\)?")
                  (anchored "\\`")
                  (t "\\(?:\\`\\|/\\)"))
            (projectile--glob-to-regexp pattern)
            (if dir-only "/" "\\(?:/\\|\\'\\)"))))

(defun projectile--compile-dirconfig-patterns (patterns)
  "Compile dirconfig PATTERNS into a single regexp.
Return nil when PATTERNS is empty.  The regexp matches a
root-relative path when any of PATTERNS does; pass directory paths
with a trailing slash so directory-only patterns can match them."
  (when patterns
    (mapconcat #'projectile--dirconfig-pattern-to-regexp patterns "\\|")))

(defun projectile-index-directory (directory patterns progress-reporter &optional ignored-files ignored-directories globally-ignored-directories)
  "Index DIRECTORY taking into account PATTERNS.

The function dispatches to an internal walker that uses pre-built
hash sets, so the per-file membership checks stay O(1) on large
projects.  The PROGRESS-REPORTER is updated while the function is
executing.  Lists of IGNORED-FILES, IGNORED-DIRECTORIES, and
GLOBALLY-IGNORED-DIRECTORIES may optionally be provided to share
state across calls."
  (let* ((ignored-files (or ignored-files (projectile-ignored-files)))
         (ignored-directories (or ignored-directories (projectile-ignored-directories)))
         (globally-ignored-directories (or globally-ignored-directories
                                           (projectile-globally-ignored-directory-names)))
         ;; Dirconfig patterns match root-relative paths, so when DIRECTORY
         ;; is a subdirectory of the project (a dirconfig `+' keep entry)
         ;; the paths matched must stay relative to the project root, not
         ;; to the walked directory.  Fall back to DIRECTORY when it isn't
         ;; under the current project.
         (walk-base (file-name-as-directory (expand-file-name directory)))
         (project-root (projectile-project-p directory))
         (match-base (if (and project-root
                              (string-prefix-p (file-name-as-directory
                                                (expand-file-name project-root))
                                               walk-base))
                         (file-name-as-directory (expand-file-name project-root))
                       walk-base))
         (rules (append (projectile--make-walk-rules ignored-files ignored-directories
                                                     globally-ignored-directories)
                        (list :dirconfig-ignore-re
                              (projectile--compile-dirconfig-patterns (car patterns))
                              :dirconfig-ensure-re
                              (projectile--compile-dirconfig-patterns (cdr patterns))
                              :match-base-len (length match-base))))
         ;; A 1-element list whose car is the accumulator.  Using a
         ;; mutable cell lets the recursive walker push results onto a
         ;; single shared list (O(N) total) instead of `apply append'-ing
         ;; per-level results (O(N*depth)).
         (acc-cell (list nil)))
    (projectile--index-directory-walk directory progress-reporter rules acc-cell)
    (nreverse (car acc-cell))))

(defun projectile--index-directory-walk (directory progress-reporter rules acc-cell)
  "Recursive walker for `projectile-index-directory'.
DIRECTORY, PROGRESS-REPORTER and RULES carry the same state as the
public entry point.  ACC-CELL is a 1-element list whose car
accumulates discovered file paths in reverse order."
  ;; Use ignore-errors to skip unreadable directories (e.g.
  ;; .Spotlight-V100 on macOS) instead of aborting the entire indexing
  ;; operation.
  ;; `directory-files-no-dot-files-regexp' filters out . and .. at the
  ;; C level so we don't have to do it again in the loop.
  ;; `directory-files-and-attributes' (rather than plain `directory-files')
  ;; gives us each entry's type in the same listing call, so we can tell
  ;; files from directories without a `file-directory-p' stat per entry -
  ;; that stat is a separate filesystem round-trip each, which dominates the
  ;; walk on large or remote (TRAMP) trees.
  (let ((entries (ignore-errors
                   (directory-files-and-attributes
                    directory t directory-files-no-dot-files-regexp nil 'integer)))
        (ignore-re (plist-get rules :dirconfig-ignore-re))
        (ensure-re (plist-get rules :dirconfig-ensure-re))
        (match-base-len (plist-get rules :match-base-len)))
    (dolist (entry entries)
      (let* ((f (car entry))
             ;; The type field is t for a directory, a string (the link
             ;; target) for a symlink, and nil for a regular file.  For a
             ;; symlink we still defer to `file-directory-p' so that a link
             ;; pointing at a directory is traversed, matching the previous
             ;; follow-symlink behaviour; that extra stat only happens for
             ;; the rare symlink entry, not for every file.
             (type (file-attribute-type (cdr entry)))
             (local-f (file-name-nondirectory (directory-file-name f)))
             (directory-p (if (stringp type) (file-directory-p f) (eq type t)))
             ;; Dirconfig patterns match against the root-relative path,
             ;; with a trailing slash appended for directories so that
             ;; directory-only patterns (trailing `/') can match.
             (match-name (and ignore-re
                              (concat (substring f match-base-len)
                                      (and directory-p "/")))))
        (unless (and match-name
                     (string-match-p ignore-re match-name)
                     (not (and ensure-re (string-match-p ensure-re match-name))))
          (progress-reporter-update progress-reporter)
          (cond
           (directory-p
            (unless (projectile--ignored-directory-fast-p
                     (file-name-as-directory f) local-f rules)
              (projectile--index-directory-walk f progress-reporter
                                                rules acc-cell)))
           (t
            (unless (projectile--ignored-file-fast-p f rules)
              (setcar acc-cell (cons f (car acc-cell)))))))))))

;;; Alien Project Indexing
;;
;; This corresponds to `projectile-indexing-method' being set to hybrid or alien.
;; The only difference between the two methods is that alien doesn't do
;; any post-processing of the files obtained via the external command.
(defun projectile-dir-files-alien (directory &optional vcs subdirs)
  "Get the files for DIRECTORY using external tools.
VCS, when supplied, must be the project's VCS as returned by
`projectile-project-vcs'.  It is computed from DIRECTORY when
omitted; callers that already resolved the VCS can pass it in to
avoid the redundant work.

SUBDIRS, when non-nil, is a list of subdirectory paths (relative
to DIRECTORY) restricting the listing.  The external command
receives them as positional arguments and submodule files are
filtered to those falling under one of the subdirectories.  This
is how dirconfig `+' keep entries are honoured by hybrid indexing
without shelling out per kept directory."
  (let ((vcs (or vcs (projectile-project-vcs directory))))
    (cond
     ((eq vcs 'git)
      (let* ((fd (and projectile-git-use-fd
                      (projectile-fd-executable-for directory)))
             (files (nconc (projectile-files-via-ext-command
                            directory (projectile-get-ext-command vcs directory)
                            subdirs)
                           (projectile--restricted-sub-projects-files directory vcs subdirs)))
             ;; When using git ls-files (not fd), deleted but unstaged
             ;; files are still reported.  Remove them.  Note that the
             ;; fd-availability check is per-DIRECTORY: a project may be
             ;; on a remote host where fd isn't installed even though it
             ;; is locally.
             (deleted (unless fd
                        (projectile-git-deleted-files directory))))
        (if deleted
            (let ((deleted-set (make-hash-table :test 'equal :size (length deleted))))
              (dolist (f deleted) (puthash f t deleted-set))
              (seq-remove (lambda (f) (gethash f deleted-set)) files))
          files)))
     (t (projectile-files-via-ext-command directory (projectile-get-ext-command vcs directory) subdirs)))))

(defun projectile--restricted-sub-projects-files (project-root vcs subdirs)
  "Return git submodule files under PROJECT-ROOT, optionally restricted to SUBDIRS.
SUBDIRS is a list of paths relative to PROJECT-ROOT; when non-nil
only files whose project-relative path starts with one of those
subdirectories are returned.  When nil, behaves exactly like
`projectile-get-sub-projects-files'."
  (let ((files (projectile-get-sub-projects-files project-root vcs)))
    (if subdirs
        (let ((normalized (mapcar #'file-name-as-directory subdirs)))
          (seq-filter
           (lambda (f)
             (seq-some (lambda (sd) (string-prefix-p sd f)) normalized))
           files))
      files)))

(defun projectile-git-deleted-files (directory)
  "Get a list of deleted but unstaged files in DIRECTORY."
  (projectile-files-via-ext-command directory "git ls-files -zd"))

(defun projectile-get-ext-command (vcs &optional directory)
  "Determine which external command to invoke based on the project's VCS.
Fallback to a generic command when not in a VCS-controlled project.

DIRECTORY, when supplied, is used to pick the right fd executable for
the git case: for remote projects the local `projectile-fd-executable'
may not exist on the remote host, so fd is detected per-host (see
`projectile-fd-executable-for').  When DIRECTORY is omitted the
current `default-directory' is used, preserving backward compatibility
for callers that don't yet thread it through."
  (let* ((directory (or directory default-directory))
         (fd (and projectile-git-use-fd
                  (projectile-fd-executable-for directory))))
    (pcase vcs
      ('git (if fd
                (concat fd " " projectile-git-fd-args)
              projectile-git-command))
      ('hg projectile-hg-command)
      ('fossil projectile-fossil-command)
      ('bzr projectile-bzr-command)
      ('darcs projectile-darcs-command)
      ('pijul projectile-pijul-command)
      ('svn projectile-svn-command)
      ('sapling projectile-sapling-command)
      ('jj projectile-jj-command)
      (_ projectile-generic-command))))

(defun projectile-get-sub-projects-command (vcs)
  "Get the sub-projects command for VCS.
Currently that's supported just for Git (sub-projects being Git
sub-modules there)."
  (pcase vcs
    ('git projectile-git-submodule-command)
    (_ nil)))

(defun projectile-get-ext-ignored-command (vcs)
  "Determine which external command to invoke based on the project's VCS."
  (pcase vcs
    ('git projectile-git-ignored-command)
    ('hg projectile-hg-ignored-command)
    ('svn projectile-svn-ignored-command)
    (_ nil)))


(defun projectile-get-all-sub-projects (project)
  "Get all sub-projects for a given project.

PROJECT is base directory to start search recursively."
  (let ((submodules (projectile-get-immediate-sub-projects project)))
    (cond
     ((null submodules)
      nil)
     (t
      (append submodules (flatten-tree
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
         (listing (if (eq vcs 'git)
                      (projectile--git-submodules path)
                    (projectile-files-via-ext-command
                     path (projectile-get-sub-projects-command vcs))))
         (submodules (mapcar
                      (lambda (s)
                        (file-name-as-directory (expand-file-name s path)))
                      listing))
         (project-child-folder-regex
          (concat "\\`" (regexp-quote path))))
    ;; If project root is inside of an VCS folder, but not
    ;; actually an VCS root itself, submodules external to the
    ;; project will be included in the VCS get sub-projects
    ;; result.  Let's remove them.
    (seq-filter
     (lambda (submodule)
       (string-match-p project-child-folder-regex submodule))
     submodules)))

(defun projectile--git-submodule-paths (gitmodules-dir)
  "List the populated submodule paths of the Git repo at GITMODULES-DIR.
Reads `.gitmodules' with `git config' run via `process-file' - no shell
is involved, so the listing works regardless of the local shell (the
old shell-out relied on Unix single quotes and `tr', which broke on
Windows - see issue #1600) and still goes through TRAMP for remote
projects.  Submodules that are registered but not checked out (no
`.git' in their directory) are omitted, matching what `git submodule
foreach' used to report.  The returned paths are relative to
GITMODULES-DIR."
  (let ((default-directory gitmodules-dir)
        paths)
    (with-temp-buffer
      (process-file "git" nil '(t nil) nil
                    "config" "-z" "--file" ".gitmodules"
                    "--get-regexp" "\\.path$")
      ;; Each NUL-separated record is "submodule.<name>.path\n<value>";
      ;; git has already unquoted the value for us.
      (dolist (record (split-string (buffer-string) "\0" t))
        (when-let* ((separator (string-search "\n" record)))
          (let ((path (substring record (1+ separator))))
            (when (file-exists-p
                   (expand-file-name ".git"
                                     (expand-file-name path gitmodules-dir)))
              (push path paths))))))
    (nreverse paths)))

(defun projectile--git-submodules (path)
  "Return the raw submodule listing for the Git repo containing PATH.
The result is a list of submodule paths relative to PATH.

With `projectile-git-submodule-command' at its default value the
listing is produced without a shell by `projectile--git-submodule-paths'
\(see issue #1600); when the variable is customized it is honored as a
shell command, and when nil submodules are disabled.

For Git projects without a `.gitmodules' file there are no submodules
to find, so the listing is skipped altogether.  PATH may be inside a
Git repo without being its toplevel \(e.g. a subproject of an outer
repo) so `.gitmodules' is looked up at the toplevel of the repo
containing PATH - the nearest parent with a `.git' entry - which is
where git itself resolves it.  Stopping at the repo boundary also
means a populated submodule doesn't pick up its superproject's
`.gitmodules'.

Alien/hybrid indexing calls this on every file listing, so the result
is cached in `projectile--git-submodules-cache' and recomputed only
when `.gitmodules' changes on disk - a stat is far cheaper than the
listing (see issue #1953).  `projectile-invalidate-cache' also
drops the cached listing."
  (when-let* ((gitmodules-dir (locate-dominating-file path ".git"))
              (gitmodules (expand-file-name ".gitmodules" gitmodules-dir))
              ;; A plain `_' binding trips "variable `_' not left unused"
              ;; in the Emacs 28/29 byte-compilers.
              (gitmodules-exists (file-exists-p gitmodules)))
    (let* ((mtime (file-attribute-modification-time
                   (file-attributes gitmodules)))
           (command (projectile-get-sub-projects-command 'git))
           (cached (gethash path projectile--git-submodules-cache)))
      (pcase-let ((`(,cached-gitmodules ,cached-mtime ,cached-command ,cached-result)
                   cached))
        (if (and cached
                 (equal cached-gitmodules gitmodules)
                 (equal cached-mtime mtime)
                 (equal cached-command command))
            cached-result
          (let ((submodules
                 (cond
                  ;; nil disables submodule listing altogether.
                  ((null command) nil)
                  ;; The stock command is never actually run: list the
                  ;; submodules shell-free instead (issue #1600).
                  ((equal command projectile--default-git-submodule-command)
                   (let ((dir (file-name-as-directory
                               (expand-file-name gitmodules-dir)))
                         (paths (projectile--git-submodule-paths gitmodules-dir)))
                     (if (equal dir (file-name-as-directory (expand-file-name path)))
                         paths
                       ;; PATH is below the `.gitmodules' dir: rebase the
                       ;; listing so it stays relative to PATH.
                       (mapcar (lambda (submodule)
                                 (file-relative-name
                                  (expand-file-name submodule dir) path))
                               paths))))
                  ;; A customized command is still run through the shell.
                  (t (projectile-files-via-ext-command path command)))))
            (puthash path (list gitmodules mtime command submodules)
                     projectile--git-submodules-cache)
            submodules))))))

(defun projectile-get-sub-projects-files (project-root vcs)
  "Get files from sub-projects for PROJECT-ROOT recursively.
VCS is the version control system of the project."
  (flatten-tree
   (mapcar (lambda (sub-project)
             (let ((project-relative-path
                    (file-name-as-directory (file-relative-name
                                             sub-project project-root))))
               (mapcar (lambda (file)
                         (concat project-relative-path file))
                       (projectile-files-via-ext-command
                        sub-project
                        (projectile-get-ext-command vcs sub-project)))))
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

(defun projectile--ext-command-line (command pathspecs)
  "Return COMMAND with PATHSPECS appended as shell-quoted positional arguments.
PATHSPECS may be nil, in which case COMMAND is returned unchanged.
Shared by the synchronous and asynchronous indexing-command runners.

Most indexing tools (`git ls-files', `find', `hg locate', ...) accept
trailing path arguments to restrict the listing.  `fd' is the exception:
its positional grammar is `[pattern] [path...]', so a trailing path
would be taken as the search pattern, and `fd' 9+ additionally rejects
`--strip-cwd-prefix' whenever an explicit path is given (see #2005).  So
for `fd' commands we drop `--strip-cwd-prefix' (Projectile strips the
`./' prefix from the output anyway) and pass the directories via
`--search-path', which is unambiguous regardless of whether the command
already carries a search pattern.  `fd' commands are recognised by the
`--strip-cwd-prefix' flag Projectile puts in its default `fd' recipes."
  (if (not pathspecs)
      command
    (if (string-match-p "--strip-cwd-prefix\\b" command)
        (concat (projectile--strip-fd-cwd-prefix-flag command) " "
                (mapconcat (lambda (path)
                             (concat "--search-path " (shell-quote-argument path)))
                           pathspecs " "))
      (concat command " "
              (mapconcat #'shell-quote-argument pathspecs " ")))))

(defun projectile--strip-fd-cwd-prefix-flag (command)
  "Remove fd's `--strip-cwd-prefix' flag (with any `=<when>' value) from COMMAND.
Also drops the space that preceded it, so the remaining command stays
tidy."
  (replace-regexp-in-string
   " ?--strip-cwd-prefix\\(=[^ ]*\\)?"
   ""
   command t t))

(defun projectile--ext-command-output-files ()
  "Parse an indexing command's stdout in the current buffer into a file list.
Splits the output on NUL, drops empty records, and strips a leading
\"./\" from each path.  Shared by `projectile-files-via-ext-command' and
its asynchronous counterpart so both produce identical results."
  (let ((shell-output (buffer-substring (point-min) (point-max))))
    (mapcar (lambda (f)
              (string-remove-prefix "./" f))
            (split-string (string-trim shell-output) "\0" t))))

(defun projectile--surface-ext-command-errors (errors-file)
  "Copy ERRORS-FILE's contents into the `*projectile-files-errors*' buffer.
Return non-nil when ERRORS-FILE held any text.  Shared by the synchronous
and asynchronous indexing-command runners so a failing command's stderr is
available for inspection."
  (with-current-buffer (get-buffer-create "*projectile-files-errors*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (ignore-errors (insert-file-contents errors-file))
      (> (buffer-size) 0))))

(defun projectile-files-via-ext-command (root command &optional pathspecs)
  "Get a list of relative file names in the project ROOT by executing COMMAND.

PATHSPECS, when non-nil, is a list of subdirectories (relative to
ROOT) appended to COMMAND as positional arguments.  Each entry is
shell-quoted before being appended.  All of the indexing commands
shipped with Projectile (`git ls-files', `fd', `find', `hg locate'
etc.) accept additional path arguments at the end of the command
line; users with heavily customised commands that don't should
either not rely on `+' keep entries in `.projectile' or arrange
their command to accept positional paths.

If `command' is nil or an empty string, return nil.
This allows commands to be disabled.

When COMMAND exits non-zero but still produced output, that output is
used: external listers such as `fd' routinely exit non-zero on benign
conditions (e.g. an unreadable directory encountered mid-traversal)
while having listed everything else.  A `user-error' is signalled only
when a non-zero exit produced no output at all, so a genuinely broken
command (most commonly a binary like `fd' or `git' missing on a remote
host) surfaces immediately instead of being mistaken for an empty
project.  Either way COMMAND's stderr is captured into the
`*projectile-files-errors*' buffer.

Only text sent to standard output is taken into account."
  (when (and (stringp command) (not (string-empty-p command)))
    (let ((default-directory root)
          (full-command (projectile--ext-command-line command pathspecs))
          (errors-file (make-temp-file "projectile-files-errors")))
      (unwind-protect
          (with-temp-buffer
            ;; `process-file-shell-command' goes through TRAMP for remote
            ;; roots and returns a reliable exit code.  Stderr is collected
            ;; into a temp file and copied into `*projectile-files-errors*'
            ;; only when the command fails, so we don't pollute the buffer
            ;; on the happy path.
            (let ((exit-code
                   (process-file-shell-command full-command nil
                                               (list t errors-file)))
                  (files (projectile--ext-command-output-files)))
              (when (and (numberp exit-code) (not (zerop exit-code)))
                (let ((had-stderr (projectile--surface-ext-command-errors errors-file)))
                  (cond
                   ;; Non-zero exit but we still got a listing: trust it.  Only
                   ;; mention it (quietly) when there was stderr worth seeing.
                   (files
                    (when had-stderr
                      (message "Projectile: `%s' exited with code %d but produced output; using it (see *projectile-files-errors*)"
                               full-command exit-code)))
                   ;; Non-zero exit and nothing on stdout: a real failure.
                   (t
                    (user-error
                     "Projectile indexing command failed with exit code %d: %s\n\
See the *projectile-files-errors* buffer for details"
                     exit-code full-command)))))
              files))
        (when (file-exists-p errors-file)
          (delete-file errors-file))))))

(defun projectile-files-via-ext-command-async (root command callback &optional pathspecs)
  "Asynchronously list relative file names in project ROOT by running COMMAND.

Like `projectile-files-via-ext-command', but spawns COMMAND with
`make-process' so Emacs is not blocked while it runs.  The output is
parsed with the very same logic, so the result is identical to the
synchronous command.

CALLBACK is funcalled with two arguments when COMMAND finishes: the list
of files (nil on failure) and an error description string (nil on
success).  As in `projectile-files-via-ext-command', a non-zero exit
that still produced output is treated as success (the output is passed
to CALLBACK); only a non-zero exit with no output is reported as an
error.  Either way the command's stderr is copied into the
`*projectile-files-errors*' buffer.

PATHSPECS is handled exactly as in `projectile-files-via-ext-command'.

Returns the process object, or nil when COMMAND is nil or empty (CALLBACK
is then invoked with an empty list and no error, so callers don't have to
special-case disabled commands) or when a remote file-name handler
declines to start the process (CALLBACK is invoked with an error).

Remote ROOTs are handled via TRAMP (`make-process' is given a non-nil
`:file-handler')."
  (if (not (and (stringp command) (not (string-empty-p command))))
      (progn (funcall callback nil nil) nil)
    (let* ((default-directory root)
           ;; Capture stderr in a temp file on the *same host* as the
           ;; command (local file locally, remote file over TRAMP) and
           ;; redirect the whole command group into it, mirroring the
           ;; synchronous runner's stderr handling without relying on
           ;; `make-process' :stderr support over TRAMP.
           (errors-file (make-nearby-temp-file "projectile-files-errors"))
           (errors-localname (or (file-remote-p errors-file 'localname) errors-file))
           (full-command (concat "{ " (projectile--ext-command-line command pathspecs)
                                 "; } 2>" (shell-quote-argument errors-localname)))
           (stdout-buffer (generate-new-buffer " *projectile-async-index*"))
           (proc
            (make-process
             :name "projectile-index"
             :buffer stdout-buffer
             ;; Run under a POSIX shell rather than the user's interactive
             ;; `shell-file-name': the `{ ...; } 2>file' wrapper (and the
             ;; `shell-quote-argument' quoting above) is POSIX-sh syntax, which
             ;; breaks under csh/tcsh/fish (see #2042).  The indexing command
             ;; itself is a plain POSIX command, so `/bin/sh' is the right
             ;; interpreter regardless of the user's login shell.
             :command (list "/bin/sh" "-c" full-command)
             :connection-type 'pipe
             :noquery t
             :file-handler t
             :sentinel
             (lambda (proc _event)
               (when (memq (process-status proc) '(exit signal))
                 (unwind-protect
                     ;; A consumer that gives up on the wait (e.g. a C-g during
                     ;; `projectile--dir-files-alien-await') marks the process
                     ;; aborted and kills it.  Killing fires this sentinel with a
                     ;; `signal' status, but we must not then report a bogus
                     ;; failure or clobber the errors buffer - just clean up.
                     (unless (process-get proc 'projectile-aborted)
                       (let ((exit-code (process-exit-status proc))
                             (files (with-current-buffer stdout-buffer
                                      (projectile--ext-command-output-files))))
                         (cond
                          ;; Clean exit: pass the listing through.
                          ((and (numberp exit-code) (zerop exit-code))
                           (funcall callback files nil))
                          ;; Non-zero exit but we still got a listing: trust it,
                          ;; mirroring the synchronous runner.  Surface stderr
                          ;; and mention it quietly when there's anything to see.
                          (files
                           (when (projectile--surface-ext-command-errors errors-file)
                             (message "Projectile: `%s' exited with code %s but produced output; using it (see *projectile-files-errors*)"
                                      command exit-code))
                           (funcall callback files nil))
                          ;; Non-zero exit and nothing on stdout: a real failure.
                          (t
                           (projectile--surface-ext-command-errors errors-file)
                           (funcall callback
                                    nil
                                    (format "exit code %s: %s"
                                            exit-code command))))))
                   (when (buffer-live-p stdout-buffer) (kill-buffer stdout-buffer))
                   (when (file-exists-p errors-file)
                     (ignore-errors (delete-file errors-file)))))))))
      ;; A file-name handler may decline to create a process (e.g. a remote
      ;; host that doesn't support `make-process'), returning nil and never
      ;; firing the sentinel.  Honour the callback contract and clean up.
      (unless proc
        (when (buffer-live-p stdout-buffer) (kill-buffer stdout-buffer))
        (when (file-exists-p errors-file) (ignore-errors (delete-file errors-file)))
        (funcall callback nil "could not start the indexing process"))
      proc)))

(defun projectile-dir-files-alien-async (directory callback &optional vcs subdirs)
  "Asynchronous counterpart of `projectile-dir-files-alien'.
Runs the project's main indexing command for DIRECTORY without blocking
and funcalls CALLBACK with (FILES ERROR) - the same convention as
`projectile-files-via-ext-command-async'.

VCS and SUBDIRS are interpreted exactly as in
`projectile-dir-files-alien'.  For git projects the cheap auxiliary
steps (collecting submodule files and removing deleted-but-unstaged
files) run synchronously once the main command finishes, inside the
sentinel - off the caller's critical path - so the assembled result
matches the synchronous function.  Returns the main command's process."
  (let* ((vcs (or vcs (projectile-project-vcs directory)))
         (command (projectile-get-ext-command vcs directory)))
    (if (eq vcs 'git)
        (let ((fd (and projectile-git-use-fd
                       (projectile-fd-executable-for directory))))
          (projectile-files-via-ext-command-async
           directory command
           (lambda (files err)
             (if err
                 (funcall callback nil err)
               (let* ((all (nconc files
                                  (projectile--restricted-sub-projects-files
                                   directory vcs subdirs)))
                      (deleted (unless fd (projectile-git-deleted-files directory))))
                 (funcall callback
                          (if deleted
                              (let ((deleted-set (make-hash-table :test 'equal :size (length deleted))))
                                (dolist (f deleted) (puthash f t deleted-set))
                                (seq-remove (lambda (f) (gethash f deleted-set)) all))
                            all)
                          nil))))
           subdirs))
      (projectile-files-via-ext-command-async directory command callback subdirs))))

;;;###autoload
(defun projectile-index-project-async (&optional project-root)
  "Index PROJECT-ROOT in the background and populate the files cache.

This warms `projectile-projects-cache' without blocking Emacs, so a
later `projectile-find-file' (or any command that lists project files)
finds the cache already populated instead of indexing synchronously.

Only the external-command indexing methods (`alien' and `hybrid') can be
warmed this way; under `native' indexing this is a no-op with a message,
since the Elisp directory walk cannot run off the main thread.  Warming
also requires caching to be enabled.

When PROJECT-ROOT is omitted the current project is used.  Returns the
indexing process, or nil when nothing was started."
  (interactive)
  (let ((root (or project-root (projectile-acquire-root))))
    (cond
     ((eq projectile-indexing-method 'native)
      (message "Projectile: async indexing needs the `alien'/`hybrid' method; `native' cannot be warmed")
      nil)
     ((not projectile-enable-caching)
      (message "Projectile: async indexing has no effect while caching is disabled")
      nil)
     ((let ((proc (gethash root projectile--async-index-processes)))
        (and proc (process-live-p proc)))
      (message "Projectile: already indexing %s" root)
      nil)
     (t
      (message "Projectile: indexing %s in the background..." root)
      ;; The callback needs to know which process it belongs to so it can
      ;; tell whether it is still the active index for ROOT when it
      ;; finishes (a re-trigger or `projectile-invalidate-cache' replaces
      ;; or clears the registry entry).  But the process object is the
      ;; return value of the very call the callback is passed to, so we
      ;; thread it through a mutable cell that is filled in below, before
      ;; the (asynchronous) sentinel can ever run.
      (let* ((proc-cell (list nil))
             (proc
              (projectile-dir-files-alien-async
               root
               (lambda (files err)
                 ;; Ignore a stale result whose registry entry was
                 ;; replaced (re-trigger) or removed (invalidation) while
                 ;; we were running, so it can't resurrect a cache that
                 ;; has since moved on.
                 (when (eq (gethash root projectile--async-index-processes)
                           (car proc-cell))
                   (remhash root projectile--async-index-processes)
                   (if err
                       (message "Projectile: background indexing of %s failed: %s" root err)
                     (projectile-cache-project root files)
                     (message "Projectile: finished indexing %s (%d files)"
                              root (length files))))))))
        (setcar proc-cell proc)
        (when (processp proc)
          (puthash root proc projectile--async-index-processes))
        proc)))))

(defun projectile--dir-files-alien-await (directory &optional vcs subdirs)
  "Like `projectile-dir-files-alien' for DIRECTORY but without freezing Emacs.
Runs the asynchronous indexer and waits for it with `accept-process-output'
so redisplay keeps happening and `keyboard-quit' (\\[keyboard-quit]) stays live; on a
quit the indexing process is killed and the quit is re-signalled.  The
returned list is the same one `projectile-dir-files-alien' would produce.

VCS and SUBDIRS are interpreted exactly as in `projectile-dir-files-alien'.
Falls back to the synchronous function when the asynchronous process
can't be started (e.g. a remote host whose handler doesn't support
`make-process').

Note that waiting pumps the event loop, so process filters, sentinels
and timers (but not interactive commands) may run while we wait."
  (let* (done files err
         (proc (projectile-dir-files-alien-async
                directory
                (lambda (fs e) (setq done t files fs err e))
                vcs subdirs)))
    (cond
     ;; The async indexer didn't spawn a process.  Either the command was
     ;; empty - in which case the callback already ran synchronously and we
     ;; just return its (empty) result - or a remote handler declined
     ;; make-process, in which case we fall back to the synchronous,
     ;; TRAMP-safe path.
     ((null proc)
      (if (and done (null err))
          files
        (projectile-dir-files-alien directory vcs subdirs)))
     ;; A genuine process is running: wait for it without freezing.
     (t
      (let ((reporter (make-progress-reporter
                       (format "Projectile is indexing %s"
                               (propertize (abbreviate-file-name directory)
                                           'face 'font-lock-keyword-face)))))
        (unwind-protect
            (while (not done)
              (accept-process-output proc 0.1)
              (progress-reporter-update reporter))
          ;; Runs on normal completion and on `keyboard-quit': make sure we
          ;; never leave the indexing process running behind us.  Mark it
          ;; aborted first so its sentinel skips the failure path (killing
          ;; it fires the sentinel with a `signal' status) and just cleans
          ;; up - a quit must stay a clean quit.
          (when (process-live-p proc)
            (process-put proc 'projectile-aborted t)
            (delete-process proc)))
        (progress-reporter-done reporter))
      (if err
          (user-error "Projectile indexing failed: %s.  \
See the *projectile-files-errors* buffer for details" err)
        files)))))

(defun projectile--dir-files-alien-maybe-async (directory &optional vcs subdirs)
  "Return alien-indexed files for DIRECTORY, without freezing when possible.
Dispatches to the responsive asynchronous indexer
\(`projectile--dir-files-alien-await') when `projectile-async-indexing' is
enabled and we're in an interactive context, and to the synchronous
`projectile-dir-files-alien' otherwise (batch mode, keyboard macros, or
when the option is disabled).  Both return the same list."
  (if (and projectile-async-indexing
           (not noninteractive)
           (not executing-kbd-macro))
      (projectile--dir-files-alien-await directory vcs subdirs)
    (projectile-dir-files-alien directory vcs subdirs)))

(defun projectile-project-files-producer (&optional project-root)
  "Describe how to list PROJECT-ROOT's files with an external command.

Return a plist exposing the pieces an external file finder (for example
an asynchronous, streaming one built on `consult' or `affe') needs to
run Projectile's own indexing command itself:

  :directory  the directory the command should run in (the project root)
  :vcs        the detected version-control system, a symbol (or `none')
  :command    the shell command that lists the files, NUL-separated, or
              nil when external-command indexing is disabled
  :separator  the string that separates records in the command's output

The command's output is exactly what `projectile-files-via-ext-command'
parses.  Note that for git projects Projectile additionally folds in
submodule files and drops deleted-but-unstaged ones (see
`projectile-dir-files-alien'); a finder that wants byte-for-byte the same
set as `projectile-find-file' should drive `projectile-dir-files-alien-async'
rather than running :command directly.

PROJECT-ROOT defaults to the current project."
  (let* ((root (or project-root (projectile-acquire-root)))
         (vcs (projectile-project-vcs root)))
    (list :directory root
          :vcs vcs
          :command (projectile-get-ext-command vcs root)
          :separator "\0")))

(defun projectile-adjust-files (project vcs files)
  "First remove ignored files from FILES, then add back unignored files."
  (projectile-add-unignored project vcs (projectile-remove-ignored files)))

(defun projectile-remove-ignored (files)
  "Remove ignored files and folders from FILES.

If ignored directory prefixed with `*', then ignore all
directories/subdirectories with matching filename,
otherwise operates relative to project root.

Dirconfig ignore patterns (the non-slash-prefixed `-' entries of the
`.projectile' file) are also applied, compiled via
`projectile--compile-dirconfig-patterns'; `!' ensure patterns rescue
files from them."
  (let* ((filtering-patterns (projectile-filtering-patterns))
         (dirconfig-ignore-re (projectile--compile-dirconfig-patterns
                               (car filtering-patterns)))
         (dirconfig-ensure-re (projectile--compile-dirconfig-patterns
                               (cdr filtering-patterns)))
         (ignored-files (projectile-ignored-files-rel))
         (ignored-dirs (projectile-ignored-directories-rel))
         ;; Hash basenames of ignored files for O(1) lookup per project
         ;; file (the original `seq-some'/`string=' over the list was
         ;; O(M) per file).
         (ignored-files-set (projectile--list->set ignored-files))
         ;; Split ignored dirs into the two matching modes used below:
         ;; entries prefixed with `*' are matched as a path *segment*
         ;; (basename anywhere in the file's directory chain), the rest
         ;; are matched as a literal path *prefix*.  Hash the segment
         ;; entries so the per-file segment loop becomes O(segments).
         (any-segment-dir-names nil)
         (prefix-dirs nil))
    (dolist (dir ignored-dirs)
      (if (string-prefix-p "*" dir)
          (push (string-remove-suffix "/" (substring dir 1))
                any-segment-dir-names)
        (push dir prefix-dirs)))
    (let ((any-segment-dir-set (projectile--list->set any-segment-dir-names))
          (suffixes projectile-globally-ignored-file-suffixes))
      (seq-remove
       (lambda (file)
         (or (gethash (file-name-nondirectory file) ignored-files-set)
             (and any-segment-dir-names
                  (seq-some
                   (lambda (segment) (gethash segment any-segment-dir-set))
                   (delete "" (split-string
                               (or (file-name-directory file) "") "/"))))
             (seq-some (lambda (dir) (string-prefix-p dir file)) prefix-dirs)
             (seq-some (lambda (suf) (string-suffix-p suf file t)) suffixes)
             (and dirconfig-ignore-re
                  (string-match-p dirconfig-ignore-re file)
                  (not (and dirconfig-ensure-re
                            (string-match-p dirconfig-ensure-re file))))))
       files))))

(defun projectile-keep-ignored-files (project vcs files)
  "Filter FILES to retain only those that are ignored."
  (when files
    (seq-filter
     (lambda (file)
       (seq-some (lambda (f) (string-prefix-p f file)) files))
     (projectile-get-repo-ignored-files project vcs))))

(defun projectile-keep-ignored-directories (project vcs directories)
  "Get ignored files within each of DIRECTORIES."
  (when directories
    (let (result)
      (dolist (dir directories result)
        (setq result (append result
                             (projectile-get-repo-ignored-directory project dir vcs))))
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
  (seq-filter (lambda (b) (buffer-file-name b)) buffers))

(defun projectile-buffers-with-file-or-process (buffers)
  "Return only those BUFFERS backed by files or processes."
  (seq-filter (lambda (b) (or (buffer-file-name b)
                                    (get-buffer-process b))) buffers))

(defun projectile-project-buffers (&optional project)
  "Get a list of a project's buffers.
If PROJECT is not specified the command acts on the current project."
  (let* ((project-root (or project (projectile-acquire-root)))
         (truename-cache (make-hash-table :test 'equal))
         (all-buffers (seq-filter
                       (lambda (buffer)
                         (projectile-project-buffer-p buffer project-root truename-cache))
                       (buffer-list))))
    (if projectile-buffers-filter-function
        (funcall projectile-buffers-filter-function all-buffers)
      all-buffers)))

(defun projectile-process-current-project-buffers (action)
  "Process the current project's buffers using ACTION."
  (let ((project-buffers (projectile-project-buffers)))
    (dolist (buffer project-buffers)
      (funcall action buffer))))

(defun projectile-process-current-project-buffers-current (action)
  "Invoke ACTION on every project buffer with that buffer current.
ACTION is called without arguments."
  (let ((project-buffers (projectile-project-buffers)))
    (dolist (buffer project-buffers)
      (with-current-buffer buffer
        (funcall action)))))

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

(defun projectile-project-buffer-p (buffer project-root &optional truename-cache)
  "Check if BUFFER is under PROJECT-ROOT.
Optional TRUENAME-CACHE is a hash table used to memoize `file-truename'
calls when checking multiple buffers against the same project root.

For buffers visiting remote (TRAMP) files we skip the `file-truename'
call: each such call is a remote round-trip, and resolving symlinks
across a TRAMP boundary is rarely what users want.  The downside is
that a remote project reached via two different symlinked paths won't
be matched - we trade that edge case for not stalling
`projectile-project-buffers' on networks with high latency."
  (with-current-buffer buffer
    (let ((directory (if buffer-file-name
                         (file-name-directory buffer-file-name)
                       default-directory)))
      (and (not (string-prefix-p " " (buffer-name buffer)))
           (not (projectile-ignored-buffer-p buffer))
           directory
           (string-equal (file-remote-p directory)
                         (file-remote-p project-root))
           (not (string-match-p "^http\\(s\\)?://" directory))
           (let ((compare-dir
                  (cond
                   ((file-remote-p directory) directory)
                   (truename-cache
                    (or (gethash directory truename-cache)
                        (puthash directory (file-truename directory) truename-cache)))
                   (t (file-truename directory)))))
             (string-prefix-p project-root compare-dir (eq system-type 'windows-nt)))))))

(defun projectile-ignored-buffer-p (buffer)
  "Check if BUFFER should be ignored."
  (or
   (with-current-buffer buffer
     (seq-some
      (lambda (name)
        (string-match-p name (buffer-name)))
      projectile-globally-ignored-buffers))
   (with-current-buffer buffer
     (seq-some
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
            (seq-difference
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
           (projectile-project-buffer-names))
   :category 'buffer
   :caller 'projectile-read-buffer))

;;; Other window/frame display variants
;;
;; Most Projectile commands that display a buffer come with -other-window
;; and -other-frame variants (bound under the `4' and `5' prefixes in
;; `projectile-command-map').  They are all generated by the macro below;
;; the bodies only differ in the display function they hand to the shared
;; subroutine.  See also `projectile-other-window-command' and
;; `projectile-other-frame-command', which provide the same functionality
;; for *any* command without needing a dedicated wrapper.

(eval-and-compile
  (defun projectile--substitute-in-symbols (form from to)
    "Return a copy of FORM with FROM replaced by TO in every symbol name."
    (cond
     ((consp form)
      (cons (projectile--substitute-in-symbols (car form) from to)
            (projectile--substitute-in-symbols (cdr form) from to)))
     ((and form (symbolp form)
           (string-match-p (regexp-quote from) (symbol-name form)))
      (intern (string-replace from to (symbol-name form))))
     (t form))))

(defmacro projectile--define-display-variants (base arglist docstring &rest body)
  "Define other-window/other-frame variants of the Projectile command BASE.

Defines the commands `BASE-other-window' and `BASE-other-frame'.  Each
takes ARGLIST as its argument list; when ARGLIST is non-empty the
commands read the raw prefix argument (all such variants mirror a base
command with an (interactive \"P\") spec).  DOCSTRING is a format
string; every `%s' in it is filled in with \"window\" or \"frame\".

BODY is the body of the other-window variant.  The other-frame variant
is derived from it by replacing \"other-window\" with \"other-frame\"
inside every symbol, so bodies reference display functions like
`find-file-other-window' and get the frame flavor for free.

If BODY starts with the keyword :places followed by a list, only the
variants for the listed places are defined, e.g. (window) for commands
that have no other-frame counterpart."
  (declare (indent 2) (doc-string 3))
  (let ((places '(window frame)))
    (when (eq (car body) :places)
      (setq places (cadr body)
            body (cddr body)))
    `(progn
       ,@(mapcar
          (lambda (place)
            (let ((name (intern (format "%s-other-%s" base place)))
                  (body (if (eq place 'window)
                            body
                          (projectile--substitute-in-symbols
                           body "other-window" "other-frame")))
                  (%-count (1- (length (split-string docstring "%s")))))
              `(defun ,name ,arglist
                 ,(apply #'format docstring (make-list %-count place))
                 (interactive ,@(when arglist '("P")))
                 ,@body)))
          places))))

(defun projectile--switch-to-buffer (switch-fn)
  "Read a project buffer and display it with SWITCH-FN.
SWITCH-FN is a `switch-to-buffer'-like command; passing
`switch-to-buffer-other-window' or `switch-to-buffer-other-frame'
yields the other-window/-frame variants."
  (funcall switch-fn (projectile-read-buffer-to-switch "Switch to buffer: ")))

;;;###autoload
(defun projectile-switch-to-buffer ()
  "Switch to a project buffer."
  (interactive)
  (projectile--switch-to-buffer #'switch-to-buffer))

;;;###autoload (autoload 'projectile-switch-to-buffer-other-window "projectile" nil t)
;;;###autoload (autoload 'projectile-switch-to-buffer-other-frame "projectile" nil t)
(projectile--define-display-variants projectile-switch-to-buffer ()
  "Switch to a project buffer and show it in another %s."
  (projectile--switch-to-buffer #'switch-to-buffer-other-window))

;;;###autoload
(defun projectile-display-buffer ()
  "Display a project buffer in another window without selecting it."
  (interactive)
  (display-buffer
   (projectile-completing-read
    "Display buffer: "
    (projectile-project-buffer-names)
    :category 'buffer
    :caller 'projectile-read-buffer)))

;;;###autoload
(defun projectile-project-buffers-other-buffer ()
  "Switch to the most recently selected buffer project buffer.
Only buffers not visible in windows are returned."
  (interactive)
  (switch-to-buffer (car (projectile-project-buffers-non-visible)) nil t))

(defun projectile-project-buffers-non-visible ()
  "Get a list of non visible project buffers."
  (seq-filter
   (lambda (buffer)
     (not (get-buffer-window buffer 'visible)))
   (projectile-project-buffers)))

;;;###autoload
(defun projectile-multi-occur (&optional nlines)
  "Do a `multi-occur' in the project's buffers.
With a prefix argument, show NLINES of context."
  (interactive "P")
  (let ((project (projectile-acquire-root)))
    (multi-occur (projectile-project-buffers project)
                 (car (occur-read-primary-args))
                 nlines)))

(defun projectile-normalise-paths (patterns)
  "Remove leading `/' from the elements of PATTERNS."
  ;; TODO: Replace delq+mapcar with seq-keep when Emacs 29.1 is the minimum version
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
    (flatten-tree (mapcar
                         (lambda (pattern)
                           (or (file-expand-wildcards pattern t)
                               (projectile-expand-root pattern)))
                         paths))))

(defun projectile-normalise-patterns (patterns)
  "Remove paths from PATTERNS."
  (seq-remove (lambda (pat) (string-prefix-p "/" pat)) patterns))

(defun projectile-make-relative-to-root (files)
  "Make FILES relative to the project root."
  (let ((project-root (projectile-project-root)))
    (mapcar (lambda (f) (projectile--project-relative-name f project-root)) files)))

(defun projectile-ignored-directory-p
    (directory &optional ignored-directories local-directory globally-ignored-directories)
  "Check if DIRECTORY should be ignored.

Pre-computed lists of IGNORED-DIRECTORIES and GLOBALLY-IGNORED-DIRECTORIES
and the LOCAL-DIRECTORY name may optionally be provided."
  (let ((ignored-directories (or ignored-directories (projectile-ignored-directories)))
        (globally-ignored-directories (or globally-ignored-directories (projectile-globally-ignored-directory-names)))
        (local-directory (or local-directory (file-name-nondirectory (directory-file-name directory)))))
    (or (member directory ignored-directories)
        (seq-some
         (lambda (name)
           (string-match-p name directory))
         projectile-global-ignore-file-patterns)
        (member local-directory globally-ignored-directories))))

(defun projectile-ignored-file-p (file &optional ignored-files)
  "Check if FILE should be ignored.

A pre-computed list of IGNORED-FILES may optionally be provided."
  (or
   (member file (or ignored-files (projectile-ignored-files)))
   (seq-some
    (lambda (name)
      (string-match-p name file))
    projectile-global-ignore-file-patterns)
   (seq-some
    (lambda (suffix)
      (string-suffix-p suffix file t))
    projectile-globally-ignored-file-suffixes)))

(defun projectile-ignored-files ()
  "Return list of ignored files.

That's a combination of the globally ignored files and
files ignored in a project's dirconfig."
  (seq-difference
   (mapcar
    #'projectile-expand-root
    (append
     projectile-globally-ignored-files
     (projectile-project-ignored-files)))
   (projectile-unignored-files)))

(defun projectile-globally-ignored-directory-names ()
  "Return list of ignored directory names."
  (seq-difference
   projectile-globally-ignored-directories
   projectile-globally-unignored-directories))

(defun projectile-ignored-directories ()
  "Return list of ignored directories."
  (seq-difference
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
  (seq-remove 'file-directory-p (projectile-project-ignored)))

(defun projectile-project-ignored-directories ()
  "Return list of project ignored directories.
Unignored directories are not included."
  (seq-filter 'file-directory-p (projectile-project-ignored)))

(defun projectile--dirconfig-ignore ()
  "Return the IGNORE entries from the project's dirconfig, or nil."
  (when-let* ((cfg (projectile-parse-dirconfig-file)))
    (projectile-dirconfig-ignore cfg)))

(defun projectile--dirconfig-ensure ()
  "Return the ENSURE entries from the project's dirconfig, or nil."
  (when-let* ((cfg (projectile-parse-dirconfig-file)))
    (projectile-dirconfig-ensure cfg)))

(defun projectile-paths-to-ignore ()
  "Return a list of ignored project paths."
  (projectile-normalise-paths (projectile--dirconfig-ignore)))

(defun projectile-patterns-to-ignore ()
  "Return a list of relative file patterns."
  (projectile-normalise-patterns (projectile--dirconfig-ignore)))

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
  (seq-remove 'file-directory-p (projectile-project-unignored)))

(defun projectile-project-unignored-directories ()
  "Return list of project unignored directories."
  (seq-filter 'file-directory-p (projectile-project-unignored)))

(defun projectile-paths-to-ensure ()
  "Return a list of unignored project paths."
  (projectile-normalise-paths (projectile--dirconfig-ensure)))

(defun projectile-files-to-ensure ()
  (let ((default-directory (projectile-project-root)))
    (flatten-tree (mapcar #'file-expand-wildcards
                          (projectile-patterns-to-ensure)))))

(defun projectile-patterns-to-ensure ()
  "Return a list of relative file patterns."
  (projectile-normalise-patterns (projectile--dirconfig-ensure)))

(defun projectile-filtering-patterns ()
  (cons (projectile-patterns-to-ignore)
        (projectile-patterns-to-ensure)))

(defun projectile-project-unignored ()
  "Return list of project ignored files/directories."
  (seq-uniq (append (projectile-expand-paths (projectile-paths-to-ensure))
                    (projectile-expand-paths (projectile-files-to-ensure)))))


(defun projectile-dirconfig-file ()
  "Return the absolute path to the project's dirconfig file."
  (expand-file-name projectile-dirconfig-file (projectile-project-root)))

(cl-defstruct projectile-dirconfig
  "Parsed contents of a project's dirconfig file.
KEEP is the list of subdirectories to restrict the project to (as
returned with a trailing slash).  IGNORE and ENSURE are the lists
of files or directories to ignore and to forcibly include,
respectively.  PREFIXLESS-IGNORE is the subset of IGNORE entries
that arrived without a leading `+'/`-'/`!'/comment marker; they
are accepted for backward compatibility but recorded separately so
callers can flag the deprecated syntax.  All slots default to nil."
  (keep nil) (ignore nil) (ensure nil) (prefixless-ignore nil))

(defun projectile--maybe-warn-glob-keep-entries (project-root cfg)
  "Warn once per session about glob patterns in + keep entries.
PROJECT-ROOT identifies the warned-projects set; CFG is the parsed
`projectile-dirconfig' struct.  The `+' prefix is for subdirectories
only; the parser silently coerces each entry to a directory, so a
glob pattern would never match."
  (when (and cfg
             (not (gethash project-root projectile--glob-keep-warned-projects)))
    (when-let* ((globbed (seq-filter
                          (lambda (entry) (string-match-p "[][*?]" entry))
                          (projectile-dirconfig-keep cfg))))
      (puthash project-root t projectile--glob-keep-warned-projects)
      (display-warning
       'projectile
       (format "%s contains `+' entries with glob metacharacters: %s.  \
The `+' prefix is for subdirectory paths only; globs are not expanded \
and the entries are silently coerced to directory names.  Use a plain \
directory or move the pattern to a `-'/`!' rule."
               (expand-file-name projectile-dirconfig-file project-root)
               (mapconcat (lambda (s) (format "`%s'" s)) globbed ", "))
       :warning))))

(defun projectile--dirconfig-classify-line (line)
  "Classify LINE from a dirconfig file.
Return a cons (BUCKET . VALUE) where BUCKET is one of `:keep',
`:ignore', `:ensure', `:legacy-ignore', or `:comment'.  Return nil
for a blank line.  Leading whitespace is skipped before dispatch
so an accidental space or tab before the prefix does not change
classification.  `:legacy-ignore' is reserved for prefix-less
lines, which are still treated as ignore patterns for backward
compatibility but are tracked separately so callers can warn."
  (let* ((trimmed (string-trim-left line))
         (first-char (and (> (length trimmed) 0) (aref trimmed 0))))
    (cond
     ((null first-char) nil)
     ((and projectile-dirconfig-comment-prefix
           (eql first-char projectile-dirconfig-comment-prefix))
      (cons :comment nil))
     ((eql first-char ?+) (cons :keep          (string-trim (substring trimmed 1))))
     ((eql first-char ?-) (cons :ignore        (string-trim (substring trimmed 1))))
     ((eql first-char ?!) (cons :ensure        (string-trim (substring trimmed 1))))
     (t                   (cons :legacy-ignore (string-trim trimmed))))))

(defun projectile--parse-dirconfig-string (text)
  "Parse TEXT (a dirconfig file's contents) into a `projectile-dirconfig'."
  (let (keep ignore ensure prefixless)
    (dolist (line (split-string text "\n"))
      (pcase (projectile--dirconfig-classify-line line)
        (`(:keep . ,v)          (unless (string-empty-p v) (push v keep)))
        (`(:ignore . ,v)        (unless (string-empty-p v) (push v ignore)))
        (`(:ensure . ,v)        (unless (string-empty-p v) (push v ensure)))
        (`(:legacy-ignore . ,v) (unless (string-empty-p v)
                                  (push v ignore)
                                  (push v prefixless)))))
    (make-projectile-dirconfig
     :keep              (mapcar #'file-name-as-directory (nreverse keep))
     :ignore            (nreverse ignore)
     :ensure            (nreverse ensure)
     :prefixless-ignore (nreverse prefixless))))

(defun projectile--parse-dirconfig-file-uncached ()
  "Parse the dirconfig file without caching.
Return a `projectile-dirconfig' or nil if the file doesn't exist."
  (let ((dirconfig (projectile-dirconfig-file)))
    (when (projectile-file-exists-p dirconfig)
      (projectile--parse-dirconfig-string
       (with-temp-buffer
         (insert-file-contents dirconfig)
         (buffer-string))))))

(defun projectile--maybe-warn-prefixless-entries (project-root cfg)
  "Warn once per session about prefix-less ignore entries in CFG for PROJECT-ROOT.
CFG is a `projectile-dirconfig' struct."
  (when (and projectile-warn-on-prefixless-dirconfig-lines
             cfg
             (projectile-dirconfig-prefixless-ignore cfg)
             (not (gethash project-root
                           projectile--prefixless-dirconfig-warned-projects)))
    (puthash project-root t projectile--prefixless-dirconfig-warned-projects)
    (display-warning
     'projectile
     (format "%s contains entries without a `+'/`-'/`!' prefix: %s.  \
The implicit form is treated as an ignore rule for backward \
compatibility but is being phased out — please prefix the lines \
explicitly.  Set `projectile-warn-on-prefixless-dirconfig-lines' \
to nil to silence this warning."
             (expand-file-name projectile-dirconfig-file project-root)
             (mapconcat (lambda (s) (format "`%s'" s))
                        (projectile-dirconfig-prefixless-ignore cfg)
                        ", "))
     :warning)))

(defun projectile-parse-dirconfig-file ()
  "Parse project ignore file and return its rules.

The return value is a `projectile-dirconfig' struct with three
slots: KEEP (subdirectories to restrict the project to), IGNORE
(files or directories to skip), and ENSURE (files or directories
to forcibly include even when otherwise ignored).  When the file
does not exist, the return value is nil.

Lines are dispatched on their first non-whitespace character:

  +  add to the keep list
  -  add to the ignore list
  !  add to the ensure list

Without a prefix, the line is assumed to be an ignore pattern, for
backward compatibility.  When `projectile-dirconfig-comment-prefix'
is non-nil, lines whose first non-whitespace character matches it
are treated as comments.

Results are cached per project root and invalidated when the
dirconfig file's modification time changes."
  (let* ((dirconfig (projectile-dirconfig-file))
         (project-root (projectile-project-root))
         (cached (gethash project-root projectile--dirconfig-cache))
         (attrs (file-attributes dirconfig))
         (mtime (when attrs (file-attribute-modification-time attrs)))
         (result (pcase-let ((`(,cached-dirconfig ,cached-mtime ,cached-result) cached))
                   (if (and cached mtime
                            (equal cached-dirconfig dirconfig)
                            (equal cached-mtime mtime))
                       cached-result
                     (let ((parsed (projectile--parse-dirconfig-file-uncached)))
                       (when mtime
                         (puthash project-root
                                  (list dirconfig mtime parsed)
                                  projectile--dirconfig-cache))
                       parsed)))))
    (projectile--maybe-warn-prefixless-entries project-root result)
    (projectile--maybe-warn-glob-keep-entries project-root result)
    result))

(defun projectile-expand-root (name &optional dir)
  "Expand NAME to project root.
When DIR is specified it uses DIR's project, otherwise it acts
on the current project.

Never use on many files since it's going to recalculate the
project-root for every file."
  (expand-file-name name (projectile-project-root dir)))

(cl-defun projectile-completing-read (prompt choices &key initial-input action caller sort-function (category 'project-file))
  "Present a project tailored PROMPT with CHOICES.

Reads with `completing-read', unless `projectile-completion-system' is a
function, in which case that function is called with PROMPT and CHOICES.

INITIAL-INPUT is passed to `completing-read'.  ACTION, when non-nil, is
called on the selected candidate and its result returned.  SORT-FUNCTION,
when non-nil, is exposed as the completion metadata's
`display-sort-function' and `cycle-sort-function', so completion UIs
that honor metadata present the candidates in that order.

CATEGORY is the completion metadata category advertised to UIs like
marginalia and embark so they annotate and act on the candidates
appropriately; it defaults to `project-file' (the candidates are project
files) and should be overridden when they are not - e.g. `buffer' for a
buffer switch or `file' for a directory.  A nil CATEGORY omits it.

CALLER is accepted for backward compatibility but no longer used."
  (ignore caller)
  (let* ((prompt (projectile-prepend-project-name prompt))
         (res (if (functionp projectile-completion-system)
                  (funcall projectile-completion-system prompt choices)
                (completing-read
                 prompt
                 (lambda (string pred action)
                   ;; The completion category lets packages like marginalia
                   ;; and embark enhance how candidates are presented.
                   (if (eq action 'metadata)
                       `(metadata ,@(when category `((category . ,category)))
                                  ,@(when sort-function
                                      `((display-sort-function . ,sort-function)
                                        (cycle-sort-function . ,sort-function))))
                     (complete-with-action action choices string pred)))
                 nil nil initial-input))))
    (if action (funcall action res) res)))

(defun projectile--dirconfig-non-empty-p ()
  "Return non-nil if the current project's dirconfig file has any content."
  (let* ((dirconfig (projectile-dirconfig-file))
         (attrs (and (projectile-file-exists-p dirconfig)
                     (file-attributes dirconfig))))
    (and attrs (> (file-attribute-size attrs) 0))))

(defun projectile--maybe-warn-dirconfig-ignored (project-root)
  "Warn once per session that PROJECT-ROOT's dirconfig is bypassed by alien mode."
  (when (and projectile-warn-when-dirconfig-is-ignored
             (eq projectile-indexing-method 'alien)
             (not (gethash project-root
                           projectile--alien-dirconfig-warned-projects))
             (projectile--dirconfig-non-empty-p))
    (puthash project-root t projectile--alien-dirconfig-warned-projects)
    (display-warning
     'projectile
     (format "Project %s has a non-empty %s but `projectile-indexing-method' \
is `alien', which bypasses dirconfig filtering.  Switch to `hybrid' or \
`native' if you need those rules to apply, or set \
`projectile-warn-when-dirconfig-is-ignored' to nil to silence this warning."
             project-root projectile-dirconfig-file)
     :warning)))

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
      (setq files (or (gethash project-root projectile-projects-cache)
                      ;; load the cache from disk only if persistent cache is
                      ;; enabled
                      (and (eq projectile-enable-caching 'persistent)
                           (projectile-load-project-cache project-root)))))

    ;; Calculate the list of files.
    (when (null files)
      (when projectile-enable-caching
        (message "Projectile is initializing cache for %s ..." project-root))
      (setq files
            (if (eq projectile-indexing-method 'alien)
                ;; In alien mode we can just skip reading
                ;; .projectile and find all files in the root dir.
                (progn
                  (projectile--maybe-warn-dirconfig-ignored project-root)
                  (projectile--dir-files-alien-maybe-async project-root))
              (let ((dirs (projectile-get-project-directories project-root)))
                (cond
                 ((and (eq projectile-indexing-method 'hybrid) (cdr dirs))
                  ;; Hybrid + dirconfig `+' keep entries: batch the
                  ;; external command into a single invocation with
                  ;; the kept subdirectories as pathspecs, then run
                  ;; projectile-adjust-files once over the combined
                  ;; result.  Avoids one shell-out per kept directory.
                  (let* ((vcs (projectile-project-vcs project-root))
                         (subdirs (mapcar
                                   (lambda (d) (file-relative-name d project-root))
                                   dirs)))
                    (projectile-adjust-files
                     project-root vcs
                     (projectile--dir-files-alien-maybe-async project-root vcs subdirs))))
                 (t
                  ;; Native, or hybrid without keep entries: walk each
                  ;; project directory.  For native this is the only
                  ;; implementation; for hybrid+single-dir it's
                  ;; equivalent to the batched call above.
                  (mapcan
                   (lambda (dir)
                     (let ((files (projectile-dir-files dir)))
                       ;; `projectile-dir-files' already returns paths
                       ;; relative to DIR, so when DIR is the project root
                       ;; itself (the single-directory case - native, or
                       ;; hybrid without keep entries) re-relativising every
                       ;; path against PROJECT-ROOT is a no-op.  Skip it
                       ;; rather than pay a `file-relative-name' per file.
                       (if (string= dir project-root)
                           files
                         (mapcar (lambda (f)
                                   (file-relative-name (concat dir f)
                                                       project-root))
                                 files))))
                   dirs))))))

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
  (projectile-project-files (projectile-acquire-root)))

(defun projectile-process-current-project-files (action)
  "Process the current project's files using ACTION."
  (let ((project-files (projectile-current-project-files))
        (default-directory (projectile-project-root)))
    (dolist (filename project-files)
      (funcall action filename))))

(defun projectile-project-dirs (project)
  "Return a list of dirs for PROJECT."
  (seq-uniq
   (delq nil
         (mapcan #'projectile--directory-ancestors
                    (projectile-project-files project)))))

(defun projectile--directory-ancestors (path)
  "Return a list of the directory of PATH and all its ancestor directories.
For example, \"src/foo/bar.el\" returns (\"src/\" \"src/foo/\")."
  (let ((dir (file-name-directory path))
        result)
    (while (and dir (not (equal dir "")))
      (push dir result)
      (let ((parent (file-name-directory (directory-file-name dir))))
        (setq dir (unless (equal parent dir) parent))))
    result))

(defun projectile-current-project-dirs ()
  "Return a list of dirs for the current project."
  (projectile-project-dirs (projectile-acquire-root)))

(defun projectile-get-other-files (file-name &optional flex-matching)
  "Return a list of other files for FILE-NAME.
The list depends on `:related-files-fn' project option and
`projectile-other-file-alist'.  For the latter, FLEX-MATCHING can be used
to match any basename."
  (if-let* ((plist (projectile--related-files-plist-by-kind  file-name :other)))
      (projectile--related-files-from-plist plist)
    (projectile--other-extension-files file-name
                                       (projectile-current-project-files)
                                       flex-matching)))

(defun projectile--find-other-file (&optional flex-matching ff-variant)
  "Switch between files with the same name but different extensions.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable
`projectile-other-file-alist'.  With FF-VARIANT set to a defun, use that
instead of `find-file'.   A typical example of such a defun would be
`find-file-other-window' or `find-file-other-frame'"
  (let ((ff (or ff-variant #'find-file))
        (other-files (projectile-get-other-files (buffer-file-name) flex-matching)))
    (if other-files
        (let ((file-name (projectile--choose-from-candidates other-files :caller 'projectile-read-file)))
          (funcall ff (expand-file-name file-name
                                        (projectile-project-root))))
      (user-error "No other file found"))))


;;; Interactive commands
;;;###autoload
(defun projectile-find-other-file (&optional flex-matching)
  "Switch between files with the same name but different extensions.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable
`projectile-other-file-alist'."
  (interactive "P")
  (projectile--find-other-file flex-matching))

;;;###autoload (autoload 'projectile-find-other-file-other-window "projectile" nil t)
;;;###autoload (autoload 'projectile-find-other-file-other-frame "projectile" nil t)
(projectile--define-display-variants projectile-find-other-file (&optional flex-matching)
  "Switch between files with different extensions in other %s.
Switch between files with the same name but different extensions in
another %s.  With FLEX-MATCHING, match any file that contains the base
name of current file.  Other file extensions can be customized with the
variable `projectile-other-file-alist'."
  (projectile--find-other-file flex-matching #'find-file-other-window))

(defun projectile--file-name-sans-extensions (file-name)
  "Return FILE-NAME sans any extensions.
The extensions, in a filename, are what follows the first '.', with the
exception of a leading '.'"
  (setq file-name (file-name-nondirectory file-name))
  (substring file-name 0 (string-match "\\..*" file-name 1)))

(defun projectile--file-name-extensions (file-name)
  "Return FILE-NAME's extensions.
The extensions, in a filename, are what follows the first '.', with the
exception of a leading '.'"
  ;;would it make sense to return nil instead of an empty string if no extensions are found?
  (setq file-name (file-name-nondirectory file-name))
  (let (extensions-start)
    (substring file-name
               (if (setq extensions-start (string-match "\\..*" file-name 1))
                   (1+ extensions-start)
                 (length file-name)))))

(defun projectile-associated-file-name-extensions (file-name)
  "Return projectile-other-file-extensions associated to FILE-NAME's extensions.
If no associated other-file-extensions for the complete (nested) extension
are found, remove subextensions from FILENAME's extensions until a match is
found."
  (let ((current-extensions (projectile--file-name-extensions (file-name-nondirectory file-name)))
        associated-extensions)
    (catch 'break
      (while (not (string= "" current-extensions))
        (if (setq associated-extensions (alist-get current-extensions projectile-other-file-alist nil nil #'equal))
            (throw 'break associated-extensions))
        (setq current-extensions (projectile--file-name-extensions current-extensions))))))

(defun projectile--other-extension-files (current-file project-file-list &optional flex-matching)
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
                                  (concat ".*" filename ".*" "\\." ext "\\'")
                                (concat "^" filename
                                        (unless (equal ext "")
                                          (concat "\\." ext))
                                        "\\'")))
                            file-ext-list))
         (candidates (seq-filter
                      (lambda (project-file)
                        (string-match filename project-file))
                      project-file-list))
         (candidates
          (flatten-tree (mapcar
                               (lambda (file)
                                 (seq-filter
                                  (lambda (project-file)
                                    (string-match file
                                                  (concat (file-name-base project-file)
                                                          (when (file-name-extension project-file)
                                                            (concat "." (file-name-extension project-file))))))
                                  candidates))
                               file-list)))
         (candidates
          (seq-filter (lambda (file) (not (backup-file-name-p file))) candidates))
         (sibling-dir-p (lambda (file)
                          (let ((candidate-dirname (file-name-nondirectory
                                                    (directory-file-name
                                                     (or (file-name-directory file) "./")))))
                            (and (not (equal fulldirname (file-name-directory file)))
                                 (equal dirname candidate-dirname)))))
         (candidates (append (seq-filter sibling-dir-p candidates)
                             (seq-remove sibling-dir-p candidates))))
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
                    (seq-filter
                     (lambda (project-file)
                       (string-search file project-file))
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
  (let* ((project-root (projectile-acquire-root))
         (project-files (projectile-project-files project-root))
         (files (projectile-select-files project-files invalidate-cache))
         (sort-function (projectile--frecency-sort-function project-root))
         (file (cond ((= (length files) 1)
                      (car files))
                     ((length> files 1)
                      (projectile-completing-read "Switch to: " files
                                                  :caller 'projectile-read-file
                                                  :sort-function sort-function))
                     (t
                      (projectile-completing-read "Switch to: " project-files
                                                  :caller 'projectile-read-file
                                                  :sort-function sort-function))))
         (ff (or ff-variant #'find-file)))
    (funcall ff (expand-file-name file project-root))
    (run-hooks 'projectile-find-file-hook)))

;;;###autoload
(defun projectile-find-file-dwim (&optional invalidate-cache)
  "Jump to a project's files using completion based on context.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works
even if the filename is incomplete, but there's only a single file in the
current project that matches the filename at point.  For example, if
there's only a single file named \"projectile/projectile.el\" but the
current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim' still switches to \"projectile/projectile.el\"
immediately because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list
of files is displayed when a filename appears more than one in the project
or the filename at point is a prefix of more than two files in a project.
For example, if `projectile-find-file-dwim' is executed on a filepath like
\"projectile/\", it lists the content of that directory.  If it is executed
on a partial filename like \"projectile/a\", a list of files with character
\"a\" in that directory is presented.

- If it finds nothing, display a list of all files in project for selecting."
  (interactive "P")
  (projectile--find-file-dwim invalidate-cache))

;;;###autoload (autoload 'projectile-find-file-dwim-other-window "projectile" nil t)
;;;###autoload (autoload 'projectile-find-file-dwim-other-frame "projectile" nil t)
(projectile--define-display-variants projectile-find-file-dwim (&optional invalidate-cache)
  "Jump to a project's files based on context, opening them in another %s.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

See `projectile-find-file-dwim' for the details of how the file at
point is used to narrow down the candidates."
  (projectile--find-file-dwim invalidate-cache #'find-file-other-window))

(defun projectile--find-file (invalidate-cache &optional ff-variant)
  "Jump to a project's file using completion.
With INVALIDATE-CACHE invalidates the cache first.  With FF-VARIANT set to a
defun, use that instead of `find-file'.   A typical example of such a defun
would be `find-file-other-window' or `find-file-other-frame'"
  (interactive "P")
  (projectile-maybe-invalidate-cache invalidate-cache)
  (let* ((project-root (projectile-acquire-root))
         (file (projectile-completing-read "Find file: "
                                           (projectile-project-files project-root)
                                           :caller 'projectile-read-file
                                           :sort-function
                                           (projectile--frecency-sort-function project-root)))
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

;;;###autoload (autoload 'projectile-find-file-other-window "projectile" nil t)
;;;###autoload (autoload 'projectile-find-file-other-frame "projectile" nil t)
(projectile--define-display-variants projectile-find-file (&optional invalidate-cache)
  "Jump to a project's file using completion and show it in another %s.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (projectile--find-file invalidate-cache #'find-file-other-window))

;;;###autoload
(defun projectile-find-file-all ()
  "Jump to any file in the project, ignoring VCS and projectile ignore rules.
This lists all files under the project root using a generic file listing
command (fd or find), bypassing `.gitignore', `.projectile', and other
ignore mechanisms."
  (interactive)
  (let* ((project-root (projectile-acquire-root))
         (all-files (projectile-files-via-ext-command project-root projectile-generic-command))
         (file (projectile-completing-read "Find file (all): " all-files
                                           :caller 'projectile-read-file)))
    (when file
      (find-file (expand-file-name file project-root))
      (run-hooks 'projectile-find-file-hook))))

;;;###autoload
(defun projectile-toggle-project-read-only ()
  "Toggle project read only."
  (interactive)
  (let ((inhibit-read-only t)
        (val (not buffer-read-only))
        (default-directory (projectile-acquire-root)))
    (save-selected-window
      (add-dir-local-variable nil 'buffer-read-only val)
      (save-buffer)
      (kill-buffer))
    (when buffer-file-name
      (read-only-mode (if val +1 -1))
      (message "[%s] read-only-mode is %s" (projectile-project-name) (if val "on" "off")))))

;;;###autoload
(defun projectile-add-dir-local-variable (mode variable value)
  "Run `add-dir-local-variable' with .dir-locals.el in root of project.

Parameters MODE VARIABLE VALUE are passed directly to `add-dir-local-variable'."
  (let ((inhibit-read-only t)
        (default-directory (projectile-acquire-root)))
    (save-selected-window
      (add-dir-local-variable mode variable value)
      (save-buffer)
      (kill-buffer))))

;;;###autoload
(defun projectile-delete-dir-local-variable (mode variable)
  "Run `delete-dir-local-variable' with .dir-locals.el in root of project.

Parameters MODE VARIABLE VALUE are passed directly to
`delete-dir-local-variable'."
  (let ((inhibit-read-only t)
        (default-directory (projectile-acquire-root)))
    (save-selected-window
      (delete-dir-local-variable mode variable)
      (save-buffer)
      (kill-buffer))))


;;;; Sorting project files
(defun projectile-sort-files (files)
  "Sort FILES according to `projectile-sort-order'."
  (pcase projectile-sort-order
    ('default files)
    ('recentf (projectile-sort-by-recentf-first files))
    ('recently-active (projectile-sort-by-recently-active-first files))
    ('modification-time (projectile-sort-by-modification-time files))
    ('access-time (projectile-sort-by-access-time files))
    ((pred functionp) (funcall projectile-sort-order files))
    ;; An unrecognized value must not return nil - that would present
    ;; the project as empty.
    (_ files)))

(defun projectile--sort-prioritized-first (prioritized files)
  "Return FILES with the members of PRIORITIZED first, in order.
Membership is tracked in a hash set, so the cost stays linear in
the length of FILES."
  (let ((seen (make-hash-table :test 'equal :size (length prioritized))))
    (dolist (file prioritized)
      (puthash file t seen))
    (append prioritized
            (seq-remove (lambda (file) (gethash file seen)) files))))

(defun projectile-sort-by-recentf-first (files)
  "Sort FILES by a recent first scheme."
  (projectile--sort-prioritized-first (projectile-recentf-files) files))

(defun projectile-sort-by-recently-active-first (files)
  "Sort FILES by most recently active buffers or opened files."
  (projectile--sort-prioritized-first (projectile-recently-active-files) files))

(defun projectile-sort-by-modification-time (files)
  "Sort FILES by modification time."
  (let ((default-directory (projectile-project-root))
        (mtimes (make-hash-table :test 'equal :size (length files))))
    (dolist (file files)
      (let ((attrs (file-attributes file)))
        (puthash file (if attrs (file-attribute-modification-time attrs) 0) mtimes)))
    (seq-sort (lambda (file1 file2)
                (not (time-less-p (gethash file1 mtimes)
                                  (gethash file2 mtimes))))
              files)))

(defun projectile-sort-by-access-time (files)
  "Sort FILES by access time."
  (let ((default-directory (projectile-project-root))
        (atimes (make-hash-table :test 'equal :size (length files))))
    (dolist (file files)
      (let ((attrs (file-attributes file)))
        (puthash file (if attrs (file-attribute-access-time attrs) 0) atimes)))
    (seq-sort (lambda (file1 file2)
                (not (time-less-p (gethash file1 atimes)
                                  (gethash file2 atimes))))
              files)))


;;;; Find directory in project functionality
(defun projectile--find-dir (invalidate-cache &optional dired-variant)
  "Jump to a project's directory using completion.

With INVALIDATE-CACHE invalidates the cache first.  With DIRED-VARIANT set to a
defun, use that instead of `dired'.  A typical example of such a defun would be
`dired-other-window' or `dired-other-frame'"
  (projectile-maybe-invalidate-cache invalidate-cache)
  (let* ((project (projectile-acquire-root))
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

;;;###autoload (autoload 'projectile-find-dir-other-window "projectile" nil t)
;;;###autoload (autoload 'projectile-find-dir-other-frame "projectile" nil t)
(projectile--define-display-variants projectile-find-dir (&optional invalidate-cache)
  "Jump to a project's directory in other %s using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (projectile--find-dir invalidate-cache #'dired-other-window))

(defun projectile-complete-dir (project)
  (let ((project-dirs (projectile-project-dirs project)))
    (projectile-completing-read
     "Find dir: "
     (if projectile-find-dir-includes-top-level
         (append '("./") project-dirs)
       project-dirs)
     :caller 'projectile-read-directory)))

;;;###autoload
(defun projectile-find-test-file (&optional invalidate-cache)
  "Jump to a project's test file using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache invalidate-cache)
  (let ((file (projectile-completing-read "Find test file: "
                                          (projectile-current-project-test-files)
                                          :caller 'projectile-read-file)))
    (find-file (expand-file-name file (projectile-project-root)))))

(defun projectile-test-files (files)
  "Return only the test FILES."
  (seq-filter 'projectile-test-file-p files))

(defun projectile--merge-related-files-fns (related-files-fns)
  "Merge multiple RELATED-FILES-FNS into one function."
  (lambda (path)
    (let (merged-plist)
      (dolist (fn related-files-fns merged-plist)
        (let ((plist (funcall fn path)))
          (cl-loop for (key value) on plist by #'cddr
                   do (let ((values (if (consp value) value (list value))))
                        (setq merged-plist
                              (plist-put merged-plist key
                                         (append (plist-get merged-plist key) values))))))))))

(defun projectile--related-files-plist (project-root file)
  "Return a plist containing all related files information for FILE.
PROJECT-ROOT is the project root."
  (if-let* ((rel-path (if (file-name-absolute-p file)
                         (file-relative-name file project-root)
                       file))
           (custom-function (funcall projectile-related-files-fn-function (projectile-project-type))))
      (funcall (cond ((functionp custom-function)
                      custom-function)
                     ((consp custom-function)
                      (projectile--merge-related-files-fns custom-function))
                     (t
                      (error "Unsupported value type of :related-files-fn")))
               rel-path)))

(defun projectile--related-files-plist-by-kind (file kind)
  "Return a plist containing :paths and/or :predicate of KIND for FILE."
  (if-let* ((project-root (projectile-project-root))
           (plist (projectile--related-files-plist project-root file))
           (has-kind? (plist-member plist kind)))
      (let* ((kind-value (plist-get plist kind))
             (values (if (or (stringp kind-value) (functionp kind-value))
                         (list kind-value)
                       kind-value))
             (paths (seq-uniq (seq-filter 'stringp values)))
             (predicates (seq-uniq (seq-filter 'functionp values))))
        (append
         ;; Make sure that :paths exists even with nil if there is no predicates
         (when (or paths (null predicates))
           (list :paths (seq-filter
                         (lambda (f)
                           (projectile-file-exists-p (projectile-expand-file-name-wildcard f project-root)))
                         paths)))
         (when predicates
           (list :predicate (if (= 1 (length predicates))
                                (car predicates)
                              (lambda (other-file)
                                (seq-some (lambda (predicate)
                                           (funcall predicate other-file))
                                         predicates)))))))))

(defun projectile--related-files-from-plist (plist)
  "Return a list of files matching to PLIST from current project files."
  (let* ((predicate (plist-get plist :predicate))
         (paths (plist-get plist :paths)))
    (seq-uniq (append
               paths
               (when predicate
                 (seq-filter predicate (projectile-current-project-files)))))))

(defun projectile--related-files-kinds(file)
  "Return a list of keywords meaning available related kinds for FILE."
  (if-let* ((project-root (projectile-project-root))
           (plist (projectile--related-files-plist project-root file)))
      (cl-loop for key in plist by #'cddr
               collect key)))

(defun projectile--related-files (file kind)
  "Return a list of related files of KIND for FILE."
  (projectile--related-files-from-plist (projectile--related-files-plist-by-kind file kind)))

(defun projectile--find-related-file (file &optional kind)
  "Choose a file from files related to FILE as KIND.
If KIND is not provided, a list of possible kinds can be chosen."
  (unless kind
    (if-let* ((available-kinds (projectile--related-files-kinds file)))
        (setq kind (if (= (length available-kinds) 1)
                       (car available-kinds)
                     (intern (projectile-completing-read "Kind :" available-kinds
                                                         :caller 'projectile-read-file))))
      (user-error "No related files found")))

  (if-let* ((candidates (projectile--related-files file kind)))
      (projectile-expand-root (projectile--choose-from-candidates candidates :caller 'projectile-read-file))
    (error
     "No matching related file as `%s' found for project type `%s'"
     kind (projectile-project-type))))

;;;###autoload (autoload 'projectile-find-related-file-other-window "projectile" nil t)
;;;###autoload (autoload 'projectile-find-related-file-other-frame "projectile" nil t)
(projectile--define-display-variants projectile-find-related-file ()
  "Open related file in other %s."
  (find-file-other-window
   (projectile--find-related-file (buffer-file-name))))

;;;###autoload
(defun projectile-find-related-file()
  "Open related file."
  (interactive)
  (find-file
   (projectile--find-related-file (buffer-file-name))))

;;;###autoload
(defun projectile-related-files-fn-groups(kind groups)
  "Generate a related-files-fn which relates as KIND for files in each of GROUPS."
  (lambda (path)
    (if-let* ((group-found (seq-find (lambda (group)
                                        (member path group))
                                      groups)))
        (list kind (remove path group-found)))))

;;;###autoload
(defun projectile-related-files-fn-extensions(kind extensions)
  "Generate a related-files-fn which relates as KIND for files having EXTENSIONS."
  (lambda (path)
    (let* ((ext (file-name-extension path))
           (basename (file-name-base path))
           (basename-regexp (regexp-quote basename)))
      (when (member ext extensions)
        (list kind (lambda (other-path)
                     (and (string-match-p basename-regexp other-path)
                          (equal basename (file-name-base other-path))
                          (let ((other-ext (file-name-extension other-path)))
                            (and (member other-ext extensions)
                                 (not (equal other-ext ext)))))))))))

;;;###autoload
(defun projectile-related-files-fn-test-with-prefix(extension test-prefix)
  "Generate a related-files-fn which relates tests and impl.
Use files with EXTENSION based on TEST-PREFIX."
  (lambda (path)
    (when (equal (file-name-extension path) extension)
      (let* ((file-name (file-name-nondirectory path))
             (find-impl? (string-prefix-p test-prefix file-name))
             (file-name-to-find (if find-impl?
                                    (substring file-name (length test-prefix))
                                  (concat test-prefix file-name))))
        (list (if find-impl? :impl :test)
              (lambda (other-path)
                (and (string-suffix-p file-name-to-find other-path)
                     (equal (file-name-nondirectory other-path) file-name-to-find))))))))

;;;###autoload
(defun projectile-related-files-fn-test-with-suffix(extension test-suffix)
  "Generate a related-files-fn which relates tests and impl.
Use files with EXTENSION based on TEST-SUFFIX."
  (lambda (path)
    (when (equal (file-name-extension path) extension)
      (let* ((file-name (file-name-nondirectory path))
             (dot-ext (concat "." extension))
             (suffix-ext (concat test-suffix dot-ext))
             (find-impl? (string-suffix-p suffix-ext file-name))
             (file-name-to-find (if find-impl?
                                    (concat (substring file-name 0 (- (length suffix-ext)))
                                            dot-ext)
                                  (concat (substring file-name 0 (- (length dot-ext)))
                                          suffix-ext))))
        (list (if find-impl? :impl :test)
              (lambda (other-path)
                (and (string-suffix-p file-name-to-find other-path)
                     (equal (file-name-nondirectory other-path) file-name-to-find))))))))


;;; File kinds
;;
;; A "file kind" is a declarative description of one category of files a
;; project type has - Rails models, controllers and views; Django models,
;; views and urls; and so on.  Kinds are declared with the `:file-kinds'
;; keyword of `projectile-register-project-type' as an alist of (KIND-NAME
;; . SPEC), where KIND-NAME is a keyword (e.g. `:model') and SPEC is a plist
;; describing which files belong to the kind and how to derive a shared
;; "resource key" from a file's path.  Two files of different kinds are
;; *related* when their keys are equal, which is what powers
;; `projectile-toggle-related-file'; `projectile-find-file-of-kind' uses just
;; the membership test.  The whole thing compiles down to an ordinary
;; related-files-fn (see `projectile--file-kinds-related-files-fn'), so it
;; rides on the existing kind-agnostic related-files machinery.

(defun projectile--singularize (word)
  "Naively singularize the English noun WORD.

Only the regular cases are handled: a trailing -ies becomes -y, a
trailing -ses/-xes/-zes/-ches/-shes drops the -es, and any other
trailing -s (but not -ss) is dropped.  Irregular nouns (person/people,
child/children, ...) and uncountables are returned unchanged, so this is
adequate for deriving resource keys from Rails-style file names but is
not a general-purpose inflector."
  (cond
   ((string-suffix-p "ies" word)
    (concat (substring word 0 -3) "y"))
   ((string-match-p "\\(s\\|x\\|z\\|ch\\|sh\\)es\\'" word)
    (substring word 0 -2))
   ((and (string-suffix-p "s" word)
         (not (string-suffix-p "ss" word)))
    (substring word 0 -1))
   (t word)))

(defun projectile--pluralize (word)
  "Naively pluralize the English noun WORD.

The mirror of `projectile--singularize', handling only the regular
cases: a consonant followed by -y becomes -ies, an -s/-x/-z/-ch/-sh
ending gains -es, and everything else gains -s.  Irregular nouns are not
handled."
  (cond
   ((string-match-p "[^aeiou]y\\'" word)
    (concat (substring word 0 -1) "ies"))
   ((string-match-p "\\(s\\|x\\|z\\|ch\\|sh\\)\\'" word)
    (concat word "es"))
   (t (concat word "s"))))

;;;; Reference file-kinds tables (Rails, Django)

(defun projectile--rails-resource-key (rel-path prefix suffix)
  "Return the namespaced singular Rails resource key of REL-PATH.
PREFIX is the resource root (e.g. \"app/controllers/\") and SUFFIX the
file suffix (e.g. \"_controller.rb\").  Rails names controllers, helpers
and views after the *plural* resource (e.g. \"users_controller.rb\"),
while models use the singular (\"user.rb\"); singularizing the stripped
name makes them share the model's key.  Any namespace directories under
PREFIX are preserved, so \"app/controllers/admin/users_controller.rb\"
keys as \"admin/user\" and does not collide with a top-level
\"users_controller.rb\".  The singularizer is naive (see
`projectile--singularize'), so irregular resource names will not relate
correctly."
  (when (and (string-prefix-p prefix rel-path)
             (string-suffix-p suffix (file-name-nondirectory rel-path)))
    (let* ((subpath (substring rel-path (length prefix)))
           (dir (or (file-name-directory subpath) ""))
           (base (file-name-nondirectory subpath))
           (resource (substring base 0 (- (length suffix)))))
      (concat dir (projectile--singularize resource)))))

(defun projectile--rails-controller-key (rel-path)
  "Return the namespaced singular Rails resource key of the controller REL-PATH."
  (projectile--rails-resource-key rel-path "app/controllers/" "_controller.rb"))

(defun projectile--rails-helper-key (rel-path)
  "Return the namespaced singular Rails resource key of the helper REL-PATH."
  (projectile--rails-resource-key rel-path "app/helpers/" "_helper.rb"))

(defun projectile--rails-view-key (rel-path)
  "Return the namespaced singular Rails resource key of the view file REL-PATH.
Rails views live in a per-resource directory (e.g. \"app/views/users/\"),
so the key is that directory's path with its final segment singularized;
\"app/views/admin/users/index.html.erb\" keys as \"admin/user\"."
  (when (string-prefix-p "app/views/" rel-path)
    (let ((rest (substring rel-path (length "app/views/"))))
      (when-let* (((string-match-p "/" rest))
                  (resource-dir (directory-file-name (file-name-directory rest))))
        (concat (or (file-name-directory resource-dir) "")
                (projectile--singularize
                 (file-name-nondirectory resource-dir)))))))

(defun projectile--django-app-key (rel-path)
  "Return the Django app directory of REL-PATH.
This is REL-PATH's parent directory (without a trailing slash), so
\"polls/models.py\" keys as \"polls\" and a nested \"apps/polls/models.py\"
keys as \"apps/polls\" without colliding with a different \"polls\" app."
  (when-let* ((dir (file-name-directory rel-path)))
    (directory-file-name dir)))

(defvar projectile--rails-file-kinds
  '((:model      . (:path "app/models/"))
    (:controller . (:path "app/controllers/"
                    :suffix "_controller.rb"
                    :key-fn projectile--rails-controller-key))
    (:view       . (:path "app/views/"
                    :key-fn projectile--rails-view-key))
    (:helper     . (:path "app/helpers/"
                    :suffix "_helper.rb"
                    :key-fn projectile--rails-helper-key)))
  "Reference `:file-kinds' table for Rails project types.
Relates a resource's model, controller, views and helper by a shared
singular key (e.g. app/models/user.rb, app/controllers/users_controller.rb,
app/views/users/*, app/helpers/users_helper.rb).  Plural-to-singular
mapping is naive; see `projectile--singularize'.")

(defvar projectile--django-file-kinds
  '((:model . (:suffix "models.py" :key-fn projectile--django-app-key))
    (:view  . (:suffix "views.py"  :key-fn projectile--django-app-key))
    (:urls  . (:suffix "urls.py"   :key-fn projectile--django-app-key))
    (:admin . (:suffix "admin.py"  :key-fn projectile--django-app-key))
    (:tests . (:suffix "tests.py"  :key-fn projectile--django-app-key)))
  "Reference `:file-kinds' table for the Django project type.
Relates the per-app files of a Django application by the app directory
name (e.g. polls/models.py, polls/views.py, polls/urls.py); no
inflection is needed.")

(defun projectile--file-kind-member-p (rel-path spec)
  "Return non-nil when REL-PATH belongs to the file kind described by SPEC.

REL-PATH is a path relative to the project root.  Membership holds when
REL-PATH lives under SPEC's `:path' prefix (if any) and its basename
matches SPEC's `:prefix' and `:suffix' (if any)."
  (let ((path (plist-get spec :path))
        (prefix (plist-get spec :prefix))
        (suffix (plist-get spec :suffix))
        (name (file-name-nondirectory rel-path)))
    ;; `:path' is matched as a directory prefix, so \"app/models/\" does
    ;; not also match \"app/models_archive/x.rb\".
    (and (or (null path)
             (string-prefix-p (file-name-as-directory path) rel-path))
         (or (null prefix) (string-prefix-p prefix name))
         (or (null suffix) (string-suffix-p suffix name)))))

(defun projectile--file-kind-default-key (rel-path spec)
  "Derive the default resource key of REL-PATH for the kind SPEC.

The key is REL-PATH taken relative to SPEC's `:path', with the final
component's `:suffix' (or, lacking that, its file extension) and
`:prefix' stripped.  Any namespace directories under `:path' are kept,
so files that differ only by subdirectory get distinct keys.  Return nil
for an empty result."
  (let* ((path (plist-get spec :path))
         (subpath (if (and path
                           (string-prefix-p (file-name-as-directory path)
                                            rel-path))
                      (substring rel-path (length (file-name-as-directory path)))
                    rel-path))
         (dir (or (file-name-directory subpath) ""))
         (name (file-name-nondirectory subpath))
         (prefix (plist-get spec :prefix))
         (suffix (plist-get spec :suffix)))
    (when (and suffix (string-suffix-p suffix name))
      (setq name (substring name 0 (- (length suffix)))))
    (unless suffix
      (setq name (file-name-sans-extension name)))
    (when (and prefix (string-prefix-p prefix name))
      (setq name (substring name (length prefix))))
    (unless (string-empty-p name)
      (concat dir name))))

(defun projectile--file-kind-key (rel-path spec)
  "Return REL-PATH's resource key for the kind SPEC, or nil.

Uses SPEC's `:key-fn' when set, otherwise
`projectile--file-kind-default-key'.  A `:key-fn' that signals an error
is treated as no match (nil), so one misbehaving user-supplied key-fn
can't break related-file navigation for the whole project type."
  (if-let* ((key-fn (plist-get spec :key-fn)))
      (condition-case nil
          (funcall key-fn rel-path)
        (error nil))
    (projectile--file-kind-default-key rel-path spec)))

(defun projectile--file-kind-match (rel-path spec)
  "Return REL-PATH's resource key for the kind SPEC, or nil.

REL-PATH matches the kind only when it is a member (see
`projectile--file-kind-member-p') and its derived key is a non-empty
string (so a `:key-fn' returning nil, an empty string or a non-string
value simply doesn't match, rather than relating unrelated files)."
  (when (projectile--file-kind-member-p rel-path spec)
    (let ((key (projectile--file-kind-key rel-path spec)))
      (and (stringp key) (not (string-empty-p key)) key))))

(defun projectile--file-kinds-related-files-fn (file-kinds)
  "Compile FILE-KINDS into a related-files-fn.

FILE-KINDS is an alist of (KIND . SPEC).  KIND is a keyword and SPEC is
a plist with the following optional properties:

  :path    a root-relative directory prefix (e.g. \"app/models/\").
  :prefix  a basename prefix the file must have.
  :suffix  a basename suffix the file must have (e.g. \"_controller.rb\").
  :key-fn  a function of the file's relative path returning its resource
           key string, or nil when the file is not of this kind.  When
           omitted the key is derived by `projectile--file-kind-default-key'.

The returned function, given a relative path, finds the first kind the
path belongs to and emits, for every *other* kind, an entry
\(:KIND PREDICATE) whose PREDICATE matches files of that kind sharing the
same resource key."
  (lambda (rel-path)
    (let (this-kind this-key)
      (cl-dolist (entry file-kinds)
        (when-let* ((key (projectile--file-kind-match rel-path (cdr entry))))
          (setq this-kind (car entry)
                this-key key)
          (cl-return)))
      (when this-kind
        (let (result)
          (dolist (entry file-kinds)
            (let ((kind (car entry))
                  (spec (cdr entry)))
              (unless (eq kind this-kind)
                (setq result
                      (plist-put result kind
                                 (lambda (other-path)
                                   (equal this-key
                                          (projectile--file-kind-match other-path spec))))))))
          result)))))

(defun projectile--file-kinds ()
  "Return the current project type's `:file-kinds' alist."
  (projectile-project-type-attribute (projectile-project-type) 'file-kinds))

(defun projectile--related-file-candidates (rel-path &optional file-kinds project-files)
  "Return an ordered alist of (KIND . FILE) related to REL-PATH.

Each entry is a project file of a *different* kind than REL-PATH's own
whose resource key equals REL-PATH's, listed in FILE-KINDS table order.
FILE-KINDS defaults to the current project type's kinds and PROJECT-FILES
to `projectile-current-project-files'.  Return nil when REL-PATH is not
of any known kind or has no related files."
  (let* ((file-kinds (or file-kinds (projectile--file-kinds)))
         (project-files (or project-files (projectile-current-project-files)))
         this-kind this-key candidates)
    (cl-dolist (entry file-kinds)
      (when-let* ((key (projectile--file-kind-match rel-path (cdr entry))))
        (setq this-kind (car entry)
              this-key key)
        (cl-return)))
    (when this-kind
      (dolist (entry file-kinds (nreverse candidates))
        (let ((kind (car entry))
              (spec (cdr entry)))
          (unless (eq kind this-kind)
            (when-let* ((match (seq-find
                                (lambda (f)
                                  (and (not (equal f rel-path))
                                       (equal this-key
                                              (projectile--file-kind-match f spec))))
                                project-files)))
              (push (cons kind match) candidates))))))))

(defun projectile--related-file-ring (rel-path &optional file-kinds project-files)
  "Return the stable ring of files sharing REL-PATH's resource key.

The ring holds one file per kind that has a match, in FILE-KINDS table
order, with REL-PATH itself standing in for its own kind.  Its order does
not depend on which file in the ring REL-PATH is, so advancing from
REL-PATH's position cycles through the related kinds deterministically.
FILE-KINDS defaults to the current project type's kinds and PROJECT-FILES
to `projectile-current-project-files'.  Return nil when REL-PATH is not of
any known kind."
  (let* ((file-kinds (or file-kinds (projectile--file-kinds)))
         (project-files (or project-files (projectile-current-project-files)))
         this-kind this-key ring)
    (cl-dolist (entry file-kinds)
      (when-let* ((key (projectile--file-kind-match rel-path (cdr entry))))
        (setq this-kind (car entry)
              this-key key)
        (cl-return)))
    (when this-kind
      (dolist (entry file-kinds (nreverse ring))
        (let ((kind (car entry))
              (spec (cdr entry)))
          (if (eq kind this-kind)
              (push rel-path ring)
            (when-let* ((match (seq-find
                                (lambda (f)
                                  (and (not (equal f rel-path))
                                       (equal this-key
                                              (projectile--file-kind-match f spec))))
                                project-files)))
              (push match ring))))))))

(defun projectile--file-kind-name (kind)
  "Return the human-readable name of the file KIND keyword."
  (substring (symbol-name kind) 1))

(defun projectile--read-file-kind (prompt)
  "Read one of the current project type's file kinds using PROMPT.
Return the chosen (KIND . SPEC) entry."
  (let ((file-kinds (projectile--file-kinds)))
    (unless file-kinds
      (user-error "Project type `%s' defines no file kinds"
                  (projectile-project-type)))
    (let* ((names (mapcar (lambda (entry)
                            (projectile--file-kind-name (car entry)))
                          file-kinds))
           (choice (projectile-completing-read prompt names
                                               :caller 'projectile-find-file-of-kind)))
      (assq (intern (concat ":" choice)) file-kinds))))

(defun projectile--find-file-of-kind (kind-entry &optional ff-variant)
  "Complete over project files of KIND-ENTRY and open the chosen one.
KIND-ENTRY is a (KIND . SPEC) pair.  With FF-VARIANT set to a defun, use
that instead of `find-file' (e.g. `find-file-other-window')."
  (let* ((project-root (projectile-acquire-root))
         (spec (cdr kind-entry))
         (files (seq-filter (lambda (f)
                              (projectile--file-kind-member-p f spec))
                            (projectile-project-files project-root)))
         (file (projectile-completing-read
                (format "Find %s: " (projectile--file-kind-name (car kind-entry)))
                files
                :caller 'projectile-find-file-of-kind
                :sort-function (projectile--frecency-sort-function project-root)))
         (ff (or ff-variant #'find-file)))
    (when file
      (funcall ff (expand-file-name file project-root))
      (run-hooks 'projectile-find-file-hook))))

;;;###autoload
(defun projectile-find-file-of-kind (&optional invalidate-cache)
  "Jump to a project file of a chosen file kind using completion.

Prompt for one of the current project type's file kinds (see the
`:file-kinds' keyword of `projectile-register-project-type') and then
complete over all project files belonging to that kind.  With a prefix
arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache invalidate-cache)
  (projectile--find-file-of-kind
   (projectile--read-file-kind "Find file of kind: ")))

;;;###autoload (autoload 'projectile-find-file-of-kind-other-window "projectile" nil t)
;;;###autoload (autoload 'projectile-find-file-of-kind-other-frame "projectile" nil t)
(projectile--define-display-variants projectile-find-file-of-kind ()
  "Jump to a project file of a chosen file kind and show it in another %s."
  (projectile--find-file-of-kind
   (projectile--read-file-kind "Find file of kind: ")
   #'find-file-other-window))

(defun projectile--read-related-file-target (rel-path)
  "Prompt for one of REL-PATH's related file kinds and return its file."
  (let* ((candidates (projectile--related-file-candidates rel-path))
         (names (mapcar (lambda (c) (projectile--file-kind-name (car c)))
                        candidates))
         (choice (projectile-completing-read
                  "Related file kind: " names
                  :caller 'projectile-toggle-related-file)))
    (alist-get (intern (concat ":" choice)) candidates)))

;;;###autoload
(defun projectile-toggle-related-file ()
  "Jump between the current file and its related files of other kinds.

This is the file-kinds generalization of
`projectile-toggle-between-implementation-and-test'.  The current file's
kind and resource key are detected from the project type's `:file-kinds'
declaration and its related files (files of other kinds sharing the same
key) are collected in table order.  When exactly one related kind exists
it is opened immediately; when several exist the first invocation prompts
for the kind, and repeated invocations cycle through them in table order."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "The current buffer is not visiting a file"))
    (let* ((project-root (projectile-acquire-root))
           ;; Spell the file the same way as the root (symlink-resolved), so a
           ;; project reached through a symlinked root doesn't yield a bogus
           ;; `../'-prefixed relative name and a spurious "no related files".
           (rel-path (projectile--project-relative-name (file-truename file) project-root))
           (ring (projectile--related-file-ring rel-path))
           (others (remove rel-path ring)))
      (unless others
        (user-error "No related files found for `%s'"
                    (file-name-nondirectory file)))
      (let ((target
             (cond
              ;; A single related kind: jump straight to it.
              ((= (length others) 1) (car others))
              ;; Repeated invocation: cycle to the next kind in table order.
              ((eq last-command this-command)
               (let ((pos (or (seq-position ring rel-path) 0)))
                 (nth (mod (1+ pos) (length ring)) ring)))
              ;; Several kinds, first invocation: let the user choose.
              (t (projectile--read-related-file-target rel-path)))))
        (find-file (expand-file-name target project-root))))))

(defun projectile-test-file-p (file)
  "Check if FILE is a test file."
  (let ((kinds (projectile--related-files-kinds file)))
    (cond ((member :impl kinds) t)
          ((member :test kinds) nil)
          (t (or (seq-some (lambda (pat) (string-prefix-p pat (file-name-nondirectory file)))
                          (delq nil (list (funcall projectile-test-prefix-function (projectile-project-type)))))
                 (seq-some (lambda (pat) (string-suffix-p pat (file-name-sans-extension (file-name-nondirectory file))))
                          (delq nil (list (funcall projectile-test-suffix-function (projectile-project-type))))))))))

(defun projectile-current-project-test-files ()
  "Return a list of test files for the current project."
  (projectile-test-files (projectile-current-project-files)))

(defvar projectile-project-types nil
  "An alist holding all project types that are known to Projectile.
The project types are symbols and they are linked to plists holding
the properties of the various project types.")

(defun projectile--combine-plists (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting properties
from the other lists.  Settings in the last list are the most significant
ones and overrule settings in the other lists."
  (let ((rtn (copy-sequence (pop plists)))
        p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
        (setq p (pop ls) v (pop ls))
        (setq rtn (plist-put rtn p v))))
    rtn))

(cl-defun projectile--build-project-plist
    (marker-files &key project-file compilation-dir configure compile install package test run test-suffix test-prefix src-dir test-dir related-files-fn file-kinds tasks)
  "Return a project type plist with the provided arguments.

A project type is defined by PROJECT-TYPE, a set of MARKER-FILES,
and optional keyword arguments.

MARKER-FILES is either a list of files or a predicate function.  When it
is a list, ALL of the listed files must be present in the project root for
the type to match (logical AND) - so a single-file marker like `(\"Foo\")'
is the common case.  To match when ANY one of several files is present,
don't pass a list; use a predicate function instead.  The predicate is
called with the project root as its single argument and should return
non-nil when the project is of this type.

The optional keyword arguments are:
PROJECT-FILE the main project file in the root project directory.  It may be a
             single file or a list of possible files.  When omitted it
             defaults to the first marker file.  Pass the symbol `none'
             to opt out, so the type is detected but contributes no
             project-root marker (e.g. when its marker also appears
             outside real projects).
COMPILATION-DIR the directory to run the tests- and compilations in,
CONFIGURE which specifies a command that configures the project
          `%s' in the command will be substituted with (projectile-project-root)
          before the command is run,
COMPILE which specifies a command that builds the project,
INSTALL which specifies a command to install the project.
PACKAGE which specifies a command to package the project.
TEST which specifies a command that tests the project,
RUN which specifies a command that runs the project,
TEST-SUFFIX which specifies test file suffix, and
TEST-PREFIX which specifies test file prefix.
SRC-DIR which specifies the path to the source relative to the project root.
TEST-DIR which specifies the path to the tests relative to the project root.
RELATED-FILES-FN which specifies a custom function to find the related
files such as test/impl/other files as below:
    CUSTOM-FUNCTION accepts FILE as relative path from the project root and
    returns a plist containing :test, :impl or :other as key and the
    relative path/paths or predicate as value.  PREDICATE accepts a
    relative path as the input.
TASKS an alist of named tasks of the form (TASK-NAME . COMMAND); see
    `projectile-tasks' for the exact shape."
  ;; When PROJECT-FILE isn't given explicitly, derive it from the first
  ;; marker file - that's the project's primary manifest in every
  ;; file-based registration, so callers needn't repeat it.  Function
  ;; markers (a symbol or lambda) have no list to derive from.  Passing
  ;; the symbol `none' opts out of both the derivation and the root-file
  ;; seeding below, for types (e.g. bloop) whose only marker also shows
  ;; up outside real projects and so must not anchor a project root.
  (let* ((project-file (cond ((eq project-file 'none) nil)
                             (project-file project-file)
                             ((and (consp marker-files)
                                   (stringp (car marker-files)))
                              (car marker-files))))
         (project-plist (list 'marker-files marker-files
                              'project-file project-file
                              'compilation-dir compilation-dir
                              'configure-command configure
                              'compile-command compile
                              'test-command test
                              'install-command install
                              'package-command package
                              'run-command run))
         (project-files (if (listp project-file)
                            project-file
                          (list project-file))))
    (dolist (project-file project-files)
      (when (and project-file (not (member project-file projectile-project-root-files)))
        (add-to-list 'projectile-project-root-files project-file)))
    (when test-suffix
      (plist-put project-plist 'test-suffix test-suffix))
    (when test-prefix
      (plist-put project-plist 'test-prefix test-prefix))
    (when src-dir
      (plist-put project-plist 'src-dir src-dir))
    (when test-dir
      (plist-put project-plist 'test-dir test-dir))
    (when related-files-fn
      (plist-put project-plist 'related-files-fn related-files-fn))
    (when file-kinds
      (plist-put project-plist 'file-kinds file-kinds))
    (when tasks
      (plist-put project-plist 'tasks tasks))
    project-plist))

(cl-defun projectile-register-project-type
    (project-type marker-files &key project-file compilation-dir configure compile install package test run test-suffix test-prefix src-dir test-dir related-files-fn file-kinds tasks)
  "Register a project type with projectile.

A project type is defined by PROJECT-TYPE, a set of MARKER-FILES,
and optional keyword arguments.

MARKER-FILES is either a list of files or a predicate function.  When it
is a list, ALL of the listed files must be present in the project root for
the type to match (logical AND) - so a single-file marker like `(\"Foo\")'
is the common case.  To match when ANY one of several files is present,
don't pass a list; use a predicate function instead.  The predicate is
called with the project root as its single argument and should return
non-nil when the project is of this type.

The optional keyword arguments are:
PROJECT-FILE the main project file in the root project directory.  It may be a
             single file or a list of possible files.  When omitted it
             defaults to the first marker file.  Pass the symbol `none'
             to opt out, so the type is detected but contributes no
             project-root marker (e.g. when its marker also appears
             outside real projects).
COMPILATION-DIR the directory to run the tests- and compilations in,
CONFIGURE which specifies a command that configures the project
          `%s' in the command will be substituted with (projectile-project-root)
          before the command is run,
COMPILE which specifies a command that builds the project,
INSTALL which specifies a command to install the project.
PACKAGE which specifies a command to package the project.
TEST which specifies a command that tests the project,
RUN which specifies a command that runs the project,
TEST-SUFFIX which specifies test file suffix, and
TEST-PREFIX which specifies test file prefix.
SRC-DIR which specifies the path to the source relative to the project root.
TEST-DIR which specifies the path to the tests relative to the project root.
RELATED-FILES-FN which specifies a custom function to find the related
files such as test/impl/other files as below:
    CUSTOM-FUNCTION accepts FILE as relative path from the project root and
    returns a plist containing :test, :impl or :other as key and the
    relative path/paths or predicate as value.  PREDICATE accepts a
    relative path as the input.
FILE-KINDS an alist of (KIND . SPEC) declaratively describing the kinds of
    files the project type has (e.g. Rails models, controllers and views),
    used by `projectile-find-file-of-kind' and
    `projectile-toggle-related-file'.  KIND is a keyword and SPEC is a plist;
    see `projectile--file-kinds-related-files-fn' for the supported
    properties.  When both FILE-KINDS and RELATED-FILES-FN are set they are
    combined, so declarative and hand-written relations coexist.
TASKS an alist of named tasks of the form (TASK-NAME . COMMAND) run via
    `projectile-run-task'; see `projectile-tasks' for the exact shape.

All command strings (CONFIGURE, COMPILE, INSTALL, PACKAGE, TEST, RUN,
and TASKS commands) support `%p' as a placeholder that will be replaced
with the project name at execution time."
  (setq projectile-project-types
        (cons `(,project-type .
                              ,(projectile--build-project-plist
                                marker-files
                                :project-file project-file
                                :compilation-dir compilation-dir
                                :configure configure
                                :compile compile
                                :install install
                                :package package
                                :test test
                                :run run
                                :test-suffix test-suffix
                                :test-prefix test-prefix
                                :src-dir src-dir
                                :test-dir test-dir
                                :related-files-fn related-files-fn
                                :file-kinds file-kinds
                                :tasks tasks))
              projectile-project-types)))

(cl-defun projectile-update-project-type
    (project-type
     &key precedence
     (marker-files nil marker-files-specified)
     (project-file nil project-file-specified)
     (compilation-dir nil compilation-dir-specified)
     (configure nil configure-specified)
     (compile nil compile-specified)
     (install nil install-specified)
     (package nil package-specified)
     (test nil test-specified)
     (run nil run-specified)
     (test-suffix nil test-suffix-specified)
     (test-prefix nil test-prefix-specified)
     (src-dir nil src-dir-specified)
     (test-dir nil test-dir-specified)
     (related-files-fn nil related-files-fn-specified)
     (file-kinds nil file-kinds-specified)
     (tasks nil tasks-specified))
    "Update an existing projectile project type.

Passed items will override existing values for the project type given
by PROJECT-TYPE.  nil can be used to remove a project type attribute.
Raise an error if PROJECT-TYPE is not already registered with
projectile.  This function may also take the keyword argument
PRECEDENCE which when set to `high' will make projectile prioritise
this project type over other clashing project types, and a value of
`low' will make projectile prefer (all) other project types by
default.

The remaining arguments - MARKER-FILES and the optional keyword
arguments - have the same meaning as for
`projectile-register-project-type', which see."
    (let* ((existing-project-plist
            (or (seq-find
                 (lambda (p) (eq project-type (car p))) projectile-project-types)
                (error "No existing project found for: %s" project-type)))
           (new-plist
            (append
             (when marker-files-specified `(marker-files ,marker-files))
             (when project-file-specified `(project-file ,project-file))
             (when compilation-dir-specified `(compilation-dir ,compilation-dir))
             (when configure-specified `(configure-command ,configure))
             (when compile-specified `(compile-command ,compile))
             (when test-specified `(test-command ,test))
             (when install-specified `(install-command ,install))
             (when package-specified `(package-command ,package))
             (when run-specified `(run-command ,run))
             (when test-suffix-specified `(test-suffix ,test-suffix))
             (when test-prefix-specified `(test-prefix ,test-prefix))
             (when src-dir-specified `(src-dir ,src-dir))
             (when test-dir-specified `(test-dir ,test-dir))
             (when related-files-fn-specified
               `(related-files-fn ,related-files-fn))
             (when file-kinds-specified `(file-kinds ,file-kinds))
             (when tasks-specified `(tasks ,tasks))))
           (merged-plist
            (projectile--combine-plists
             (cdr existing-project-plist) new-plist))
           (project-type-elt (cons project-type merged-plist)))
      (cl-flet* ((project-filter (p) (eq project-type (car p)))
                 (project-map (p) (if (project-filter p) project-type-elt p)))
        (setq projectile-project-types
              (if precedence
                  (let ((filtered-types
                       (seq-remove #'project-filter projectile-project-types)))
                    (setq projectile-project-type-cache (make-hash-table :test 'equal))
                    (cond ((eq precedence 'high)
                           (cons project-type-elt filtered-types))
                          ((eq precedence 'low)
                           (append filtered-types (list project-type-elt)))
                          (t (error "Precedence must be one of '(high low)"))))
                (mapcar #'project-map projectile-project-types))))))

(defun projectile-remove-project-type (project-type)
  "Remove PROJECT-TYPE from the list of registered project types.

This is the supported way to stop Projectile from auto-detecting a
project type.  Clearing the type's marker files instead does not work:
an empty marker set is vacuously satisfied, so the type would match
every project rather than none.

Raise an error if PROJECT-TYPE is not currently registered.  The project
type cache is reset so the change takes effect immediately."
  (unless (seq-find (lambda (p) (eq project-type (car p)))
                    projectile-project-types)
    (error "No existing project found for: %s" project-type))
  (setq projectile-project-types
        (seq-remove (lambda (p) (eq project-type (car p)))
                    projectile-project-types))
  (setq projectile-project-type-cache (make-hash-table :test 'equal)))

(defun projectile-eldev-project-p (&optional dir)
  "Check if a project contains eldev files.
When DIR is specified it checks DIR's project, otherwise
it acts on the current project."
  (or (projectile-verify-file "Eldev" dir)
      (projectile-verify-file "Eldev-local" dir)))

(defun projectile-expand-file-name-wildcard (name-pattern dir)
  "Expand the maybe-wildcard-containing NAME-PATTERN in DIR.
If there are results expanding a wildcard, get the first result,
otherwise expand NAME-PATTERN in DIR ignoring wildcards."
  (let ((expanded (expand-file-name name-pattern dir)))
    (or (if (string-match-p "[[*?]" name-pattern)
            (car
             (ignore-errors (file-expand-wildcards expanded))))
        expanded)))

(defun projectile-cabal-project-p (&optional dir)
  "Check if a project contains *.cabal files but no stack.yaml file.
When DIR is specified it checks DIR's project, otherwise
it acts on the current project."
  (and (projectile-verify-file-wildcard "?*.cabal" dir)
       (not (projectile-verify-file "stack.yaml" dir))))

(defun projectile-dotnet-project-p (&optional dir)
  "Check if a project contains a .NET project marker.
When DIR is specified it checks DIR's project, otherwise
it acts on the current project."
  (or (projectile-verify-file-wildcard "?*.csproj" dir)
      (projectile-verify-file-wildcard "?*.fsproj" dir)))

(defun projectile-dotnet-sln-project-p (&optional dir)
  "Check if a project contains a .NET solution project marker.
When DIR is specified it checks DIR's project, otherwise
it acts on the current project."
  (or (projectile-verify-file-wildcard "?*.sln" dir)
      (projectile-verify-file-wildcard "?*.slnx" dir)))

(defun projectile-go-project-p (&optional dir)
  "Check if a project contains Go source files.
When DIR is specified it checks DIR's project, otherwise
it acts on the current project."
  (or (projectile-verify-file "go.mod" dir)
      (projectile-verify-file-wildcard "*.go" dir)))

(defun projectile-make-project-p (&optional dir)
  "Check if a project contains a Makefile.
Both `Makefile' and the lowercase `makefile' GNU make also reads are
recognized.  When DIR is specified it checks DIR's project, otherwise
it acts on the current project."
  (or (projectile-verify-file "Makefile" dir)
      (projectile-verify-file "makefile" dir)))

(defun projectile-mill-project-p (&optional dir)
  "Check if a project contains a mill build file.
When DIR is specified it checks DIR's project, otherwise
it acts on the current project."
  (or (projectile-verify-file "build.mill" dir)
      (projectile-verify-file "build.sc" dir)))

(defcustom projectile-go-project-test-function #'projectile-go-project-p
  "Function to determine if project's type is go."
  :group 'projectile
  :type 'function
  :package-version '(projectile . "1.0.0"))

(defun projectile-nimble-project-p (&optional dir)
  "Check if a project contains a Nimble project marker.
Nim projects that use Nimble contain a <projectname>.nimble file.
When DIR is specified it checks DIR's project, otherwise
it acts on the current project."
  (projectile-verify-file-wildcard "?*.nimble" dir))

;;;; Constant signifying opting out of CMake preset commands.
(defconst projectile--cmake-no-preset "*no preset*")

(defun projectile--cmake-version ()
  "Compute CMake version."
  (let* ((string (shell-command-to-string "cmake --version"))
         (match (string-match "^cmake version \\([0-9]+\\.[0-9]+\\.[0-9]+\\).*$" string)))
    (when match
      (version-to-list (match-string 1 string)))))

(defun projectile--cmake-check-version (version)
  "Check if CMake version is at least VERSION."
  (and
   (version-list-<= version (projectile--cmake-version))))

(defconst projectile--cmake-command-presets-minimum-version-alist
  '((:configure-command . (3 19))
    (:compile-command . (3 20))
    (:test-command . (3 20))
    (:package-command . (3 19))
    (:install-command . (3 20))))

(defun projectile--cmake-command-presets-supported (command-type)
  "Check if CMake supports presets for COMMAND-TYPE."
  (let ((minimum-version
         (alist-get command-type projectile--cmake-command-presets-minimum-version-alist)))
    (projectile--cmake-check-version minimum-version)))

(defun projectile--cmake-read-preset (filename)
  "Read CMake preset from FILENAME."
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (json-parse-buffer :array-type 'list))))

(defconst projectile--cmake-command-preset-array-id-alist
  '((:configure-command . "configurePresets")
    (:compile-command . "buildPresets")
    (:test-command . "testPresets")
    (:package-command . "packagePresets")
    (:install-command . "buildPresets")))

(defun projectile--cmake-command-preset-array-id (command-type)
  "Map from COMMAND-TYPE to id of command preset array in CMake preset."
  (alist-get command-type projectile--cmake-command-preset-array-id-alist))

(defun projectile--cmake-command-presets-shallow (filename command-type)
  "Get CMake COMMAND-TYPE presets from FILENAME."
  (when-let* ((preset (projectile--cmake-read-preset (projectile-expand-root filename))))
    (seq-remove
     (lambda (preset) (equal (gethash "hidden" preset) t))
     (gethash (projectile--cmake-command-preset-array-id command-type) preset))))

(defun projectile--cmake-command-presets (filename command-type)
  "Get CMake COMMAND-TYPE presets from FILENAME.  Follows included files."
  ;; Anchor FILENAME to the project root before taking its directory:
  ;; the top-level call passes a relative name, whose `file-name-directory'
  ;; is nil, and included files must resolve relative to the file that
  ;; includes them - not to `default-directory'.
  (let ((filename (projectile-expand-root filename)))
    (when-let* ((preset (projectile--cmake-read-preset filename)))
      (append
       (projectile--cmake-command-presets-shallow filename command-type)
       (mapcan
        (lambda (included-file) (projectile--cmake-command-presets
                                 (expand-file-name included-file (file-name-directory filename))
                                 command-type))
        (gethash "include" preset))))))

(defun projectile--cmake-all-command-presets (command-type)
  "Get CMake user and system COMMAND-TYPE presets."
  (flatten-tree
   (mapcar (lambda (filename) (projectile--cmake-command-presets filename command-type))
           '("CMakeUserPresets.json" "CMakePresets.json"))))

(defun projectile--cmake-command-preset-names (command-type)
  "Get names of CMake user and system COMMAND-TYPE presets."
  (mapcar (lambda (preset)
            (gethash "name" preset))
          (projectile--cmake-all-command-presets command-type)))

(defcustom projectile-enable-cmake-presets nil
  "Enables configuration with CMake presets.

When `projectile-enable-cmake-presets' is non-nil, CMake projects can
be configured, built and tested using presets."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.4.0"))

(defun projectile--cmake-use-command-presets (command-type)
  "Test whether or not to use command presets for COMMAND-TYPE.

Presets are used if `projectile-enable-cmake-presets' is non-nil, and CMake
supports presets for COMMAND-TYPE, and `json-parse-buffer' is available."
  (and projectile-enable-cmake-presets
       (projectile--cmake-command-presets-supported command-type)
       (functionp 'json-parse-buffer)))

(defun projectile--cmake-select-command (command-type)
  "Select a CMake command preset or a manual CMake command.

The selection is done like this:

- If `projectile--cmake-use-commands-presets' for COMMAND-TYPE returns true, and
there is at least one preset available for COMMAND-TYPE, the user is prompted to
select a name of a command preset, or opt a manual command by selecting
`projectile--cmake-no-preset'.

- Else `projectile--cmake-no-preset' is used."
  (if-let* ((use-presets (projectile--cmake-use-command-presets command-type))
           (preset-names (projectile--cmake-command-preset-names command-type)))
      (projectile-completing-read
       "Use preset: "
       (append preset-names `(,projectile--cmake-no-preset))
       :caller nil)
    projectile--cmake-no-preset))

(defconst projectile--cmake-manual-command-alist
  '((:configure-command . "cmake -S . -B build")
    (:compile-command . "cmake --build build")
    (:test-command . "cmake --build build --target test")
    (:package-command . "cmake --build build --target package")
    (:install-command . "cmake --build build --target install")))

(defun projectile--cmake-manual-command (command-type)
  "Create manual CMake COMMAND-TYPE command."
  (alist-get command-type projectile--cmake-manual-command-alist))

(defconst projectile--cmake-preset-command-alist
  '((:configure-command . "cmake . --preset %s")
    (:compile-command . "cmake --build --preset %s")
    (:test-command . "ctest --preset %s")
    (:package-command . "cpack --preset %s")
    (:install-command . "cmake --build --preset %s --target install")))

(defun projectile--cmake-preset-command (command-type preset)
  "Create CMake COMMAND-TYPE command using PRESET."
  (format (alist-get command-type projectile--cmake-preset-command-alist) preset))

(defun projectile--cmake-command (command-type)
  "Create a CMake COMMAND-TYPE command.

The command is created like this:

- If `projectile--cmake-select-command' returns `projectile--cmake-no-preset'
a manual COMMAND-TYPE command is created with
`projectile--cmake-manual-command'.

- Else a preset COMMAND-TYPE command using the selected preset is created with
`projectile--cmake-preset-command'."
  (let ((maybe-preset (projectile--cmake-select-command command-type)))
    (if (equal maybe-preset projectile--cmake-no-preset)
        (projectile--cmake-manual-command command-type)
      (projectile--cmake-preset-command command-type maybe-preset))))

(defun projectile--cmake-configure-command ()
  "CMake configure command."
  (projectile--cmake-command :configure-command))

(defun projectile--cmake-compile-command ()
  "CMake compile command."
  (projectile--cmake-command :compile-command))

(defun projectile--cmake-test-command ()
  "CMake test command."
  (projectile--cmake-command :test-command))

(defun projectile--cmake-install-command ()
  "CMake install command."
  (projectile--cmake-command :install-command))

(defun projectile--cmake-package-command ()
  "CMake package command."
  (projectile--cmake-command :package-command))

;;; Project type registration
;;
;; Project type detection happens in a reverse order with respect to
;; project type registration (invocations of `projectile-register-project-type').
;;
;; As function-based project type detection is pretty slow, it
;; should be tried at the end if everything else failed (meaning here
;; it should be listed first).
;;
;; Ideally common project types should be checked earlier than exotic ones.

;; Function-based detection project type
(projectile-register-project-type 'haskell-cabal #'projectile-cabal-project-p
                                  :compile "cabal build"
                                  :test "cabal test"
                                  :run "cabal run"
                                  :test-suffix "Spec")
(projectile-register-project-type 'dotnet #'projectile-dotnet-project-p
                                  :project-file '("?*.csproj" "?*.fsproj")
                                  :compile "dotnet build"
                                  :run "dotnet run"
                                  :test "dotnet test")
(projectile-register-project-type 'dotnet-sln #'projectile-dotnet-sln-project-p
                                  :project-file '("?*.sln" "?*.slnx")
                                  :compile "dotnet build"
                                  :run "dotnet run"
                                  :test "dotnet test")
(projectile-register-project-type 'nim-nimble #'projectile-nimble-project-p
                                  :project-file "?*.nimble"
                                  :compile "nimble --noColor build --colors:off"
                                  :install "nimble --noColor install --colors:off"
                                  :test "nimble --noColor test -d:nimUnittestColor:off --colors:off"
                                  :run "nimble --noColor run --colors:off"
                                  :src-dir "src"
                                  :test-dir "tests")
;; File-based detection project types

;; Universal
(projectile-register-project-type 'xmake '("xmake.lua")
                                  :compile "xmake build"
                                  :test "xmake test"
                                  :run "xmake run"
                                  :install "xmake install")
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
(projectile-register-project-type 'nix-flake '("flake.nix")
                                  :compile "nix build"
                                  :test "nix flake check"
                                  :run "nix run")
(projectile-register-project-type 'bazel '("WORKSPACE")
                                  :compile "bazel build"
                                  :test "bazel test"
                                  :run "bazel run")
(projectile-register-project-type 'debian '("debian/control")
                                  :compile "debuild -uc -us")

;; Make & CMake
;; Marker lists mean AND, so the Makefile/makefile alternatives go
;; through a predicate.  The explicit :project-file keeps "Makefile"
;; registered as a root file, as the marker list used to do.
(projectile-register-project-type 'make #'projectile-make-project-p
                                  :project-file "Makefile"
                                  :compile "make"
                                  :test "make test"
                                  :install "make install")
(projectile-register-project-type 'gnumake '("GNUmakefile")
                                  :compile "make"
                                  :test "make test"
                                  :install "make install")
(projectile-register-project-type 'cmake '("CMakeLists.txt")
                                  :configure #'projectile--cmake-configure-command
                                  :compile #'projectile--cmake-compile-command
                                  :test #'projectile--cmake-test-command
                                  :install #'projectile--cmake-install-command
                                  :package #'projectile--cmake-package-command)
;; go-task/task
(projectile-register-project-type 'go-task '("Taskfile.yml")
                                  :compile "task build"
                                  :test "task test"
                                  :install "task install")
;; Go should take higher precedence than Make because Go projects often have a Makefile.
(projectile-register-project-type 'go projectile-go-project-test-function
                                  :compile "go build"
                                  :test "go test ./..."
                                  :test-suffix "_test")
;; PHP
(projectile-register-project-type 'php-symfony '("composer.json" "app" "src" "vendor")
                                  :compile "app/console server:run"
                                  :test "phpunit -c app "
                                  :test-suffix "Test")
;; Erlang & Elixir
(projectile-register-project-type 'rebar '("rebar.config")
                                  :compile "rebar3 compile"
                                  :test "rebar3 do eunit,ct"
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
(projectile-register-project-type 'npm '("package.json" "package-lock.json")
                                  :compile "npm install && npm run build"
                                  :test "npm test"
                                  :test-suffix ".test")
(projectile-register-project-type 'yarn '("package.json" "yarn.lock")
                                  :compile "yarn && yarn build"
                                  :test "yarn test"
                                  :test-suffix ".test")
(projectile-register-project-type 'pnpm '("package.json" "pnpm-lock.yaml")
                                  :compile "pnpm install && pnpm build"
                                  :test "pnpm test"
                                  :test-suffix ".test")
;; Angular
(projectile-register-project-type 'angular '("angular.json" ".angular-cli.json")
                                  :compile "ng build"
                                  :run "ng serve"
                                  :test "ng test"
                                  :test-suffix ".spec")
;; Python
(projectile-register-project-type 'django '("manage.py")
                                  :compile "python manage.py runserver"
                                  :test "python manage.py test"
                                  :test-prefix "test_"
                                  :test-suffix "_test"
                                  :file-kinds projectile--django-file-kinds)
(projectile-register-project-type 'python-pip '("requirements.txt")
                                  :compile "python setup.py build"
                                  :test "python -m unittest discover"
                                  :test-prefix "test_"
                                  :test-suffix "_test")
(projectile-register-project-type 'python-pkg '("setup.py")
                                  :compile "python setup.py build"
                                  :test "python -m unittest discover"
                                  :test-prefix "test_"
                                  :test-suffix "_test")
(projectile-register-project-type 'python-tox '("tox.ini")
                                  :compile "tox -r --notest"
                                  :test "tox"
                                  :test-prefix "test_"
                                  :test-suffix "_test")
(projectile-register-project-type 'python-pipenv '("Pipfile")
                                  :compile "pipenv run build"
                                  :test "pipenv run test"
                                  :test-prefix "test_"
                                  :test-suffix "_test")
(projectile-register-project-type 'python-poetry '("poetry.lock")
                                  :compile "poetry build"
                                  :test "poetry run python -m unittest discover"
                                  :test-prefix "test_"
                                  :test-suffix "_test")
(projectile-register-project-type 'python-toml '("pyproject.toml")
                                  :compile "python -m build"
                                  :test "python -m unittest discover"
                                  :test-prefix "test_"
                                  :test-suffix "_test")
;; Java & friends
(projectile-register-project-type 'maven '("pom.xml")
                                  :compile "mvn -B clean install"
                                  :test "mvn -B test"
                                  :test-suffix "Test"
                                  :src-dir "src/main/"
                                  :test-dir "src/test/")
(projectile-register-project-type 'gradle '("build.gradle")
                                  :compile "gradle build"
                                  :test "gradle test"
                                  :test-suffix "Spec")
(projectile-register-project-type 'gradlew '("gradlew")
                                  :compile "./gradlew build"
                                  :test "./gradlew test"
                                  :test-suffix "Spec")
(projectile-register-project-type 'grails '("application.yml" "grails-app")
                                  :compile "grails package"
                                  :test "grails test-app"
                                  :test-suffix "Spec")
;; Scala
(projectile-register-project-type 'sbt '("build.sbt")
                                  :src-dir "main"
                                  :test-dir "test"
                                  :compile "sbt compile"
                                  :test "sbt test"
                                  :test-suffix "Spec")

(projectile-register-project-type 'mill #'projectile-mill-project-p
                                  :project-file '("build.sc" "build.mill")
                                  :src-dir "src/"
                                  :test-dir "test/src/"
                                  :compile "mill __.compile"
                                  :test "mill __.test"
                                  :test-suffix "Test")

;; Bloop drops a `.bloop/bloop.settings.json' in the project, but its
;; server also keeps one in `$HOME', so the marker must not anchor a
;; project root (see #1901) - only drive type detection.
(projectile-register-project-type 'bloop '(".bloop/bloop.settings.json")
                                  :project-file 'none
                                  :compile "bloop compile root"
                                  :test "bloop test --propagate --reporter scalac root"
                                  :src-dir "src/main/"
                                  :test-dir "src/test/"
                                  :test-suffix "Spec")

;; Clojure
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
                                  :compile "bundle exec rake"
                                  :src-dir "lib/"
                                  :test "bundle exec rake test"
                                  :test-suffix "_test")
;; Rails needs to be registered after npm, otherwise `package.json` makes it `npm`.
;; https://github.com/bbatsov/projectile/pull/1191
(projectile-register-project-type 'rails-test '("Gemfile" "app" "lib" "db" "config" "test")
                                  :compile "bundle exec rails server"
                                  :src-dir "app/"
                                  :test "bundle exec rake test"
                                  :test-suffix "_test"
                                  :file-kinds projectile--rails-file-kinds)
(projectile-register-project-type 'rails-rspec '("Gemfile" "app" "lib" "db" "config" "spec")
                                  :compile "bundle exec rails server"
                                  :src-dir "app/"
                                  :test "bundle exec rspec"
                                  :test-dir "spec/"
                                  :test-suffix "_spec"
                                  :file-kinds projectile--rails-file-kinds)
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

(projectile-register-project-type 'emacs-eask '("Eask")
                                  :compile "eask install"
                                  :test "eask test"
                                  :test-prefix "test-"
                                  :test-suffix "-test")

(projectile-register-project-type 'emacs-eldev #'projectile-eldev-project-p
                                  :project-file "Eldev"
                                  :compile "eldev compile"
                                  :test "eldev test"
                                  :run "eldev emacs"
                                  :package "eldev package")

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
                                  :test "cargo test"
                                  :run "cargo run")

;; Racket
(projectile-register-project-type 'racket '("info.rkt")
                                  :test "raco test ."
                                  :install "raco pkg install"
                                  :package "raco pkg create --source $(pwd)")

;; Dart
(projectile-register-project-type 'dart '("pubspec.yaml")
                                  :compile "pub get"
                                  :test "pub run test"
                                  :run "dart"
                                  :test-suffix "_test.dart")

;; Elm
(projectile-register-project-type 'elm '("elm.json")
                                  :compile "elm make")

;; Julia
(projectile-register-project-type 'julia '("Project.toml")
                                  :compile "julia --project=@. -e 'import Pkg; Pkg.precompile(); Pkg.build()'"
                                  :test "julia --project=@. -e 'import Pkg; Pkg.test()' --check-bounds=yes"
                                  :src-dir "src"
                                  :test-dir "test")

;; OCaml
(projectile-register-project-type 'ocaml-dune '("dune-project")
                                  :compile "dune build"
                                  :test "dune runtest")

;; Zig
(projectile-register-project-type 'zig '("build.zig.zon")
                                  :compile "zig build"
                                  :test "zig build test"
                                  :run "zig build run")

;; Swift
(projectile-register-project-type 'swift-spm '("Package.swift")
                                  :compile "swift build"
                                  :test "swift test"
                                  :run "swift run")

(defvar-local projectile-project-type nil
  "Buffer local var for overriding the auto-detected project type.
Normally you'd set this from .dir-locals.el.")
(put 'projectile-project-type 'safe-local-variable #'symbolp)

(defun projectile-detect-project-type (&optional dir project-root)
  "Detect the type of the project.
When DIR is specified it detects its project type, otherwise it acts
on the current project.  PROJECT-ROOT, if provided, is used for caching
instead of re-resolving via `projectile-project-root'.

Fallback to a generic project type when the type can't be determined."
  ;; Resolve the root up front so function markers receive it (they used to
  ;; get DIR, which is nil when detecting the current project's type - see
  ;; #1909) and so the cache key below reuses the same value.
  (let* ((project-root (or project-root (projectile-project-root dir)))
         ;; List the root once and answer plain-name markers from the
         ;; listing.  Detection walks every registered type (50+), and the
         ;; vast majority use plain-name markers in the root, so this turns
         ;; "one stat per marker per type" into a single `directory-files'
         ;; call - a big win on the first detection of a remote project.
         (entry-set (and project-root
                         (projectile--directory-entry-set project-root)))
         (project-type
          (or (car (seq-find
                    (lambda (project-type-record)
                      (let ((project-type (car project-type-record))
                            (marker (plist-get (cdr project-type-record) 'marker-files)))
                        (if (functionp marker)
                            (and (funcall marker project-root) project-type)
                          ;; An empty marker set is vacuously satisfied by
                          ;; `projectile-verify-files' (`seq-every-p' over nil
                          ;; is t), which would make the type match every
                          ;; project.  Guard against it so clearing a type's
                          ;; markers disables detection instead of inverting it.
                          (and marker (projectile-verify-files marker dir entry-set) project-type))))
                    projectile-project-types))
              'generic)))
    (puthash project-root project-type projectile-project-type-cache)
    project-type))

(defun projectile-project-type (&optional dir)
  "Determine a project's type based on its structure.
When DIR is specified it checks it, otherwise it acts
on the current project.

The project type is cached for improved performance."
  (or (and (not dir) projectile-project-type)
      (if-let* ((project-root (projectile-project-root dir)))
          (or (gethash project-root projectile-project-type-cache)
              (projectile-detect-project-type dir project-root)))))

;;;###autoload
(defun projectile-project-info ()
  "Display info for current project."
  (interactive)
  (message "Project dir: %s ## Project VCS: %s ## Project type: %s"
           (projectile-acquire-root)
           (projectile-project-vcs)
           (projectile-project-type)))

(defun projectile-verify-files (files &optional dir entry-set)
  "Check whether all FILES exist in the project.
When DIR is specified it checks DIR's project, otherwise
it acts on the current project.  ENTRY-SET, when non-nil, is a hash set
of the project root's immediate entries (see
`projectile--directory-entry-set') used to answer plain-name FILES
without a filesystem round-trip each."
  (seq-every-p (lambda (file) (projectile-verify-file file dir entry-set)) files))

(defun projectile-verify-file (file &optional dir entry-set)
  "Check whether FILE exists in the current project.
When DIR is specified it checks DIR's project, otherwise
it acts on the current project.

ENTRY-SET, when non-nil, is a hash set of the project root's immediate
entries.  A plain-name FILE (one sitting directly in the root) is then
answered by membership in ENTRY-SET instead of a filesystem stat; a FILE
carrying a path separator can't be and falls back to
`projectile-file-exists-p'."
  (if (and entry-set (not (string-match-p "/" file)))
      (and (gethash file entry-set) t)
    (projectile-file-exists-p (projectile-expand-root file dir))))

(defun projectile-verify-file-wildcard (file &optional dir)
  "Check whether FILE exists in the current project.
When DIR is specified it checks DIR's project, otherwise
it acts on the current project.
Expands wildcards using `file-expand-wildcards' before checking."
  (file-expand-wildcards (projectile-expand-root file dir)))

(define-obsolete-variable-alias 'projectile--vcs-markers
  'projectile-vcs-markers "3.1.0")

(defcustom projectile-vcs-markers
  '((".git" . git)
    (".hg" . hg)
    (".fslckout" . fossil)
    ("_FOSSIL_" . fossil)
    (".bzr" . bzr)
    ("_darcs" . darcs)
    (".pijul" . pijul)
    (".svn" . svn)
    (".sl" . sapling)
    (".jj" . jj)
    (".osc" . osc))
  "Alist of (MARKER . VCS) pairs probed by `projectile-project-vcs'.

The order only breaks ties between markers in the same directory -
during the upward walk the nearest marker directory always wins.
The main reason to customize this is colocated repositories: a
repository created with `jj git init' contains both `.jj' and
`.git', and moving `.jj' first makes such projects detect as `jj'.

Unknown VCS symbols are fine - file listing falls back to the
generic command and `projectile-vc' to `vc-dir' - so new markers
can be added here without any other configuration.

VCS detection is cached per project; run `projectile-invalidate-cache'
after changing this for it to affect already-visited projects."
  :group 'projectile
  :type '(alist :key-type (string :tag "Marker") :value-type (symbol :tag "VCS"))
  :package-version '(projectile . "3.1.0"))

(defun projectile--vcs-from-directory-listing (directory)
  "Return the VCS symbol matching a marker directly inside DIRECTORY.
Issues a single `directory-files' call rather than one
`file-exists-p' per marker - over TRAMP that turns 10 sequential
remote round-trips into one."
  (when-let* ((entry-set (projectile--directory-entry-set directory)))
    (cl-some (lambda (cell)
               (and (gethash (car cell) entry-set) (cdr cell)))
             projectile-vcs-markers)))

(defun projectile-project-vcs (&optional project-root)
  "Determine the VCS used by the project if any.
PROJECT-ROOT is the targeted directory.  If nil, use
the variable `projectile-project-root'.

Results are cached in `projectile-project-vcs-cache' (cleared by
`projectile-invalidate-cache')."
  (or project-root (setq project-root (projectile-acquire-root)))
  (let ((cached (gethash project-root projectile-project-vcs-cache 'unset)))
    (if (not (eq cached 'unset))
        cached
      (let ((vcs (or
                  ;; first we check for a VCS marker in the project root itself
                  (projectile--vcs-from-directory-listing project-root)
                  ;; then we check if there's a VCS marker up the directory tree
                  ;; that covers the case when a project is part of a
                  ;; multi-project repository - in those cases you can still
                  ;; use the VCS to get a list of files for the project in
                  ;; question.  All markers are checked together via a single
                  ;; predicate so each ancestor directory is listed at most
                  ;; once instead of up to 10 times.
                  (let ((found nil))
                    (projectile-locate-dominating-file
                     project-root
                     (lambda (dir)
                       (setq found (projectile--vcs-from-directory-listing dir))))
                    found)
                  'none)))
        (puthash project-root vcs projectile-project-vcs-cache)
        vcs))))

(defun projectile--test-name-for-impl-name (impl-file-path)
  "Determine the name of the test file for IMPL-FILE-PATH.

IMPL-FILE-PATH may be an absolute path, relative path or a file name."
  (let* ((project-type (projectile-project-type))
         (impl-file-name (file-name-sans-extension (file-name-nondirectory impl-file-path)))
         (impl-file-ext (file-name-extension impl-file-path))
         (test-prefix (funcall projectile-test-prefix-function project-type))
         (test-suffix (funcall projectile-test-suffix-function project-type)))
    (cond
     (test-prefix (concat test-prefix impl-file-name "." impl-file-ext))
     (test-suffix (concat impl-file-name test-suffix "." impl-file-ext))
     (t (user-error "Cannot determine a test file name, one of \"test-suffix\" or \"test-prefix\" must be set for project type `%s'" project-type)))))

(defun projectile--impl-name-for-test-name (test-file-path)
  "Determine the name of the implementation file for TEST-FILE-PATH.

TEST-FILE-PATH may be an absolute path, relative path or a file name."
  (let* ((project-type (projectile-project-type))
         (test-file-name (file-name-sans-extension (file-name-nondirectory test-file-path)))
         (test-file-ext (file-name-extension test-file-path))
         (test-prefix (funcall projectile-test-prefix-function project-type))
         (test-suffix (funcall projectile-test-suffix-function project-type)))
    (cond
     (test-prefix
      (concat (string-remove-prefix test-prefix test-file-name) "." test-file-ext))
     (test-suffix
      (concat (string-remove-suffix test-suffix test-file-name) "." test-file-ext))
     (t (user-error "Cannot determine an implementation file name, one of \"test-suffix\" or \"test-prefix\" must be set for project type `%s'" project-type)))))

(defun projectile--test-to-impl-dir (test-dir-path)
  "Return the directory path of an impl file with test file in TEST-DIR-PATH.

Occurrences of the current project type's test-dir property (which should be a
string) are replaced with the current project type's src-dir property
 (which should be a string) to obtain the new directory.

Nil is returned if either the src-dir or test-dir properties are not strings."
  (let* ((project-type (projectile-project-type))
         (test-dir (projectile-test-directory project-type))
         (impl-dir (projectile-src-directory project-type)))
    (when (and (stringp test-dir) (stringp impl-dir))
      (if (not (string-match-p test-dir (file-name-directory test-dir-path)))
          (user-error "Attempted to find a implementation file by switching this project type's (%s) test-dir property \"%s\" with this project type's src-dir property \"%s\", but %s does not contain \"%s\""
                 project-type test-dir impl-dir test-dir-path test-dir)
        (projectile-complementary-dir test-dir-path test-dir impl-dir)))))

(defun projectile--impl-to-test-dir-fallback (impl-dir-path)
  "Return the test file for IMPL-DIR-PATH by guessing a test directory.

Occurrences of the `projectile-default-src-directory' in the directory of
IMPL-DIR-PATH are replaced with `projectile-default-test-directory'.  Nil is
returned if `projectile-default-src-directory' is not a substring of
IMPL-DIR-PATH."
  (when-let* ((file (projectile--complementary-file
                    impl-dir-path
                    (lambda (f)
                      (when (string-match-p projectile-default-src-directory f)
                        (projectile-complementary-dir
                         impl-dir-path
                         projectile-default-src-directory
                         projectile-default-test-directory)))
                    #'projectile--test-name-for-impl-name)))
    (projectile--project-relative-name file (projectile-project-root))))

(defun projectile--test-to-impl-dir-fallback (test-dir-path)
  "Return the impl file for TEST-DIR-PATH by guessing a source directory.

Occurrences of `projectile-default-test-directory' in the directory of
TEST-DIR-PATH are replaced with `projectile-default-src-directory'.  Nil is
returned if `projectile-default-test-directory' is not a substring of
TEST-DIR-PATH."
  (when-let* ((file (projectile--complementary-file
                    test-dir-path
                    (lambda (f)
                      (when (string-match-p projectile-default-test-directory f)
                        (projectile-complementary-dir
                         test-dir-path
                         projectile-default-test-directory
                         projectile-default-src-directory)))
                    #'projectile--impl-name-for-test-name)))
    (projectile--project-relative-name file (projectile-project-root))))

(defun projectile--impl-to-test-dir (impl-dir-path)
  "Return the directory path of a test whose impl file resides in IMPL-DIR-PATH.

Occurrences of the current project type's src-dir property (which should be a
string) are replaced with the current project type's test-dir property
 (which should be a string) to obtain the new directory.

If the src-dir property is set and IMPL-DIR-PATH does not contain (as a
substring) the src-dir property of the current project type, an error is
signalled.

Nil is returned if either the src-dir or test-dir properties are not strings."
  (let* ((project-type (projectile-project-type))
         (test-dir (projectile-test-directory project-type))
         (impl-dir (projectile-src-directory project-type)))
    (when (and (stringp test-dir) (stringp impl-dir))
      (if (not (string-match-p impl-dir (file-name-directory impl-dir-path)))
          (user-error "Attempted to find a test file by switching this project type's (%s) src-dir property \"%s\" with this project type's test-dir property \"%s\", but %s does not contain \"%s\""
                 project-type impl-dir test-dir impl-dir-path impl-dir)
        (projectile-complementary-dir impl-dir-path impl-dir test-dir)))))

(defun projectile-complementary-dir (dir-path string replacement)
  "Return the \"complementary\" directory of DIR-PATH.
Replace STRING in DIR-PATH with REPLACEMENT."
  (let* ((project-root (projectile-project-root))
         (relative-dir (file-name-directory (projectile--project-relative-name dir-path project-root))))
    (projectile-expand-root
     (string-replace string replacement relative-dir))))

(defun projectile--create-directories-for (path)
  "Create directories necessary for PATH."
  (unless (file-exists-p path)
    (make-directory (if (file-directory-p path)
                        path
                      (file-name-directory path))
                    :create-parents)))

(defun projectile-find-implementation-or-test (file-name)
  "Given a FILE-NAME return the matching implementation or test filename.

If `projectile-create-missing-test-files' is non-nil, create the missing
test file."
  (unless file-name (user-error "The current buffer is not visiting a file"))
  (unless (projectile-project-type) (projectile-ensure-project nil))
  (if (projectile-test-file-p file-name)
      ;; find the matching impl file
      (let ((impl-file (projectile-find-matching-file file-name)))
        (if impl-file
            (projectile-expand-root impl-file)
          (error
           "No matching source file found for project type `%s'"
           (projectile-project-type))))
    ;; find the matching test file
    (let* ((error-msg (format
                       "No matching test file found for project type `%s'"
                       (projectile-project-type)))
           (test-file (or (projectile-find-matching-test file-name)
                          (error error-msg)))
           (expanded-test-file (projectile-expand-root test-file)))
      (cond ((file-exists-p expanded-test-file) expanded-test-file)
            (projectile-create-missing-test-files
             (projectile--create-directories-for expanded-test-file)
             expanded-test-file)
            (t (user-error "Determined test file to be \"%s\", which does not exist.  Set `projectile-create-missing-test-files' to allow `projectile-find-implementation-or-test' to create new files" test-file))))))

(defun projectile--find-implementation-or-test-in (ff-variant)
  "Open the matching implementation or test file using FF-VARIANT.
FF-VARIANT is a `find-file'-like command; passing
`find-file-other-window' or `find-file-other-frame' yields the
corresponding display variants."
  (funcall ff-variant (projectile-find-implementation-or-test (buffer-file-name))))

;;;###autoload (autoload 'projectile-find-implementation-or-test-other-window "projectile" nil t)
;;;###autoload (autoload 'projectile-find-implementation-or-test-other-frame "projectile" nil t)
(projectile--define-display-variants projectile-find-implementation-or-test ()
  "Open matching implementation or test file in other %s.

See the documentation of `projectile--find-matching-file' and
`projectile--find-matching-test' for how implementation and test files
are determined."
  (projectile--find-implementation-or-test-in #'find-file-other-window))

;;;###autoload
(defun projectile-toggle-between-implementation-and-test ()
  "Toggle between an implementation file and its test file.


See the documentation of `projectile--find-matching-file' and
`projectile--find-matching-test' for how implementation and test files
are determined."
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
  (or projectile-project-test-prefix
      (projectile-project-type-attribute project-type 'test-prefix)))

(defun projectile-test-suffix (project-type)
  "Find default test files suffix based on PROJECT-TYPE."
  (or projectile-project-test-suffix
      (projectile-project-type-attribute project-type 'test-suffix)))

(defun projectile-related-files-fn (project-type)
  "Find relative file based on PROJECT-TYPE.

Combines the type's hand-written `related-files-fn' (or the
`projectile-project-related-files-fn' override) with a related-files-fn
compiled from its `file-kinds' declaration, if any.  When both are
present they are merged into a list of functions so declarative and
hand-written relations coexist."
  (let ((custom (or projectile-project-related-files-fn
                    (projectile-project-type-attribute project-type 'related-files-fn)))
        (file-kinds (projectile-project-type-attribute project-type 'file-kinds)))
    (if (null file-kinds)
        custom
      (let ((kinds-fn (projectile--file-kinds-related-files-fn file-kinds)))
        (cond
         ((null custom) kinds-fn)
         ((functionp custom) (list custom kinds-fn))
         ((consp custom) (append custom (list kinds-fn)))
         (t (error "Unsupported value type of :related-files-fn")))))))

(defun projectile-src-directory (project-type)
  "Find default src directory based on PROJECT-TYPE."
  (or projectile-project-src-dir
      (projectile-project-type-attribute project-type 'src-dir)))

(defun projectile-test-directory (project-type)
  "Find default test directory based on PROJECT-TYPE."
  (or projectile-project-test-dir
      (projectile-project-type-attribute project-type 'test-dir)))

(defun projectile-dirname-matching-count (a b)
  "Count matching dirnames ascending file paths in A and B."
  (setq a (reverse (split-string (or (file-name-directory a) "") "/" t))
        b (reverse (split-string (or (file-name-directory b) "") "/" t)))
  (let ((common 0))
    (while (and a b (string-equal (pop a) (pop b)))
      (setq common (1+ common)))
    common))

(defun projectile-group-file-candidates (file candidates)
  "Group file candidates by dirname matching count."
  (seq-sort (lambda (a b) (> (car a) (car b)))
            (let (value result)
              (while (setq value (pop candidates))
                (let* ((key (projectile-dirname-matching-count file value))
                       (kv (assoc key result)))
                  (if kv
                      (setcdr kv (cons value (cdr kv)))
                    (push (list key value) result))))
              (mapcar (lambda (x)
                        (cons (car x) (nreverse (cdr x))))
                      (nreverse result)))))

(defun projectile--best-or-all-candidates-based-on-parents-dirs (file candidates)
  "Return a list of the best one for FILE from CANDIDATES or all CANDIDATES."
  (let ((grouped-candidates (projectile-group-file-candidates file candidates)))
    (if (= (length (car grouped-candidates)) 2)
        (list (car (last (car grouped-candidates))))
      (apply #'append (mapcar #'cdr grouped-candidates)))))

(defun projectile--impl-to-test-predicate (impl-file)
  "Return a predicate, which returns t for any test files for IMPL-FILE."
  (let* ((basename (file-name-sans-extension (file-name-nondirectory impl-file)))
         (test-prefix (funcall projectile-test-prefix-function (projectile-project-type)))
         (test-suffix (funcall projectile-test-suffix-function (projectile-project-type)))
         (prefix-name (when test-prefix (concat test-prefix basename)))
         (suffix-name (when test-suffix (concat basename test-suffix))))
    (lambda (current-file)
      (let ((name (file-name-sans-extension (file-name-nondirectory current-file))))
        (or (string-equal prefix-name name)
            (string-equal suffix-name name))))))

(defun projectile--complementary-file (file-path dir-fn filename-fn)
  "Apply DIR-FN and FILENAME-FN to the directory and name of FILE-PATH.

More specifically, return DIR-FN applied to the directory of FILE-PATH
concatenated with FILENAME-FN applied to the file name of FILE-PATH.

If either function returns nil, return nil."
  (let ((filename (file-name-nondirectory file-path)))
    (when-let* ((complementary-filename (funcall filename-fn filename))
               (dir (funcall dir-fn (file-name-directory file-path))))
     (concat (file-name-as-directory dir) complementary-filename))))

(defun projectile--impl-file-from-src-dir-str (file-name)
  "Get the relative path of the implementation file FILE-NAME.
Return a path relative to the project root for the impl file of FILE-NAME
using the src-dir and test-dir properties of the current project type which
should be strings, nil returned if this is not the case."
  (when-let* ((complementary-file (projectile--complementary-file
                                  file-name
                                  #'projectile--test-to-impl-dir
                                  #'projectile--impl-name-for-test-name)))
    (projectile--project-relative-name complementary-file (projectile-project-root))))

(defun projectile--test-file-from-test-dir-str (file-name)
  "Get the relative path of the test file FILE-NAME.
Return a path relative to the project root for the test file of FILE-NAME
using the src-dir and test-dir properties of the current project type which
should be strings, nil returned if this is not the case."
  (when-let* ((complementary-file (projectile--complementary-file
                                   file-name
                                   #'projectile--impl-to-test-dir
                                   #'projectile--test-name-for-impl-name)))
    (projectile--project-relative-name complementary-file (projectile-project-root))))

(defun projectile--impl-file-from-src-dir-fn (test-file)
  "Get the relative path to the implementation file corresponding to TEST-FILE.
Return the implementation file path for the absolute path TEST-FILE
relative to the project root in the case the current project type's src-dir
has been set to a custom function, return nil if this is not the case or
the path points to a file that does not exist."
  (when-let* ((src-dir (projectile-src-directory (projectile-project-type))))
    (when (functionp src-dir)
      (let ((impl-file (projectile--complementary-file
                        test-file
                        src-dir
                        #'projectile--impl-name-for-test-name)))
        (when (file-exists-p impl-file)
          (file-relative-name impl-file (projectile-project-root)))))))

(defun projectile--test-file-from-test-dir-fn (impl-file)
  "Get the relative path to the test file corresponding to IMPL-FILE.
Return the test file path for the absolute path IMPL-FILE relative to the
project root, in the case the current project type's test-dir has been set
to a custom function, else return nil."
  (when-let* ((test-dir (projectile-test-directory (projectile-project-type))))
    (when (functionp test-dir)
      (file-relative-name
       (projectile--complementary-file
        impl-file
        test-dir
        #'projectile--test-name-for-impl-name)
       (projectile-project-root)))))

(defmacro projectile--acond (&rest clauses)
  "Like `cond', but the result of each condition is bound to `it'.

The variable `it' is available within the remainder of each of CLAUSES.

CLAUSES are otherwise as documented for `cond'.  This is copied from
anaphora.el."
  (declare (debug cond))
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
          (sym (gensym)))
      `(let ((,sym ,(car cl1)))
         (if ,sym
             (if (null ',(cdr cl1))
                 ,sym
               (let ((it ,sym)) ,@(cdr cl1)))
           (projectile--acond ,@(cdr clauses)))))))

(defun projectile--find-matching-test (impl-file)
  "Return a list of test files for IMPL-FILE.

The precedence for determining test files to return is:

1. Use the project type's test-dir property if it's set to a function
2. Use the project type's related-files-fn property if set
3. Use the project type's test-dir property if it's set to a string
4. Attempt to find a file by matching all project files against
   `projectile--impl-to-test-predicate'
5. Fallback to swapping \"src\" for \"test\" in IMPL-FILE if \"src\"
   is a substring of IMPL-FILE."
  (projectile--acond
   ((projectile--test-file-from-test-dir-fn impl-file) (list it))
   ((projectile--related-files-plist-by-kind impl-file :test)
    (projectile--related-files-from-plist it))
   ((projectile--test-file-from-test-dir-str impl-file) (list it))
   ((projectile--best-or-all-candidates-based-on-parents-dirs
     impl-file (seq-filter
                (projectile--impl-to-test-predicate impl-file)
                (projectile-current-project-files))) it)
   ((projectile--impl-to-test-dir-fallback impl-file)
    (list it))))

(defun projectile--test-to-impl-predicate (test-file)
  "Return a predicate, which returns t for any impl files for TEST-FILE."
  (let* ((basename (file-name-sans-extension (file-name-nondirectory test-file)))
         (test-prefix (funcall projectile-test-prefix-function (projectile-project-type)))
         (test-suffix (funcall projectile-test-suffix-function (projectile-project-type))))
    (lambda (current-file)
      (let ((name (file-name-nondirectory (file-name-sans-extension current-file))))
        (or (when test-prefix (string-equal (concat test-prefix name) basename))
            (when test-suffix (string-equal (concat name test-suffix) basename)))))))

(defun projectile--find-matching-file (test-file)
  "Return a list of impl files tested by TEST-FILE.

The precedence for determining implementation files to return is:

1. Use the project type's src-dir property if it's set to a function
2. Use the project type's related-files-fn property if set
3. Use the project type's src-dir property if it's set to a string
4. Default to a fallback which matches all project files against
   `projectile--test-to-impl-predicate'
5. Fallback to swapping \"test\" for \"src\" in TEST-FILE if \"test\"
   is a substring of TEST-FILE."
  (projectile--acond
   ((projectile--impl-file-from-src-dir-fn test-file) (list it))
   ((projectile--related-files-plist-by-kind test-file :impl)
    (projectile--related-files-from-plist it))
   ((projectile--impl-file-from-src-dir-str test-file) (list it))
   ((projectile--best-or-all-candidates-based-on-parents-dirs
     test-file (seq-filter
                (projectile--test-to-impl-predicate test-file)
                (projectile-current-project-files))) it)
   ((projectile--test-to-impl-dir-fallback test-file) (list it))))

(defun projectile--choose-from-candidates (candidates &key caller)
  "Choose one item from CANDIDATES."
  (if (= (length candidates) 1)
      (car candidates)
    (projectile-completing-read "Switch to: " candidates :caller caller)))

(defun projectile-find-matching-test (impl-file)
  "Compute the name of the test matching IMPL-FILE."
  (when-let* ((candidates (projectile--find-matching-test impl-file)))
    (projectile--choose-from-candidates candidates :caller 'projectile-read-file)))

(defun projectile-find-matching-file (test-file)
  "Compute the name of a file matching TEST-FILE."
  (when-let* ((candidates (projectile--find-matching-file test-file)))
    (projectile--choose-from-candidates candidates :caller 'projectile-read-file)))

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

(defface projectile-search-prompt-tool
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for the tool/backend highlighted in Projectile's search prompts."
  :group 'projectile
  :package-version '(projectile . "3.2.0"))

(defface projectile-search-prompt-default
  '((t :inherit font-lock-constant-face))
  "Face for the default value highlighted in Projectile's search prompts."
  :group 'projectile
  :package-version '(projectile . "3.2.0"))

(defun projectile--search-tool-tag (tool)
  "Return a faced `[TOOL]' tag for a search prompt.
TOOL is the backend, given as a symbol or a string: `projectile-search'
passes the backend name symbol while the reviewable search passes a
\"ripgrep\"/\"elisp\" string.  Used where the backend varies so the
prompt makes clear which tool will run."
  (format "[%s]" (propertize (format "%s" tool)
                             'face 'projectile-search-prompt-tool)))

(defun projectile--read-search-string-with-default (prompt-label)
  "Read a search string, defaulting to the symbol or region at point.
PROMPT-LABEL is the action shown in the prompt (already carrying any
faced tool tag its caller wants).  The default value, when there is one,
is appended and faced as `(default VALUE)'."
  (let* ((prompt-label (projectile-prepend-project-name prompt-label))
         (default-value (projectile-symbol-or-selection-at-point))
         (default-label (if (or (not default-value)
                                (string= default-value ""))
                            ""
                          (format " (default: %s)"
                                  (propertize default-value
                                              'face 'projectile-search-prompt-default)))))
    (read-string (format "%s%s: " prompt-label default-label) nil nil default-value)))

(defvar projectile-grep-find-ignored-paths)
(defvar projectile-grep-find-unignored-paths)
(defvar projectile-grep-find-ignored-patterns)
(defvar projectile-grep-find-unignored-patterns)

(defun projectile-rgrep-default-command (regexp files dir)
  "Compute the command for \\[rgrep] to use by default.

Extension of the Emacs 25.1 implementation of `rgrep-default-command', with
which it shares its arglist."
  (require 'find-dired)      ; for `find-name-arg'
  (grep-expand-template
   grep-find-template
   regexp
   (concat (shell-quote-argument "(")
           " " find-name-arg " "
           (mapconcat
            #'shell-quote-argument
            (split-string files)
            (concat " -o " find-name-arg " "))
           " "
           (shell-quote-argument ")"))
   dir
   (concat
    (and grep-find-ignored-directories
         (concat "-type d "
                 (shell-quote-argument "(")
                 ;; we should use shell-quote-argument here
                 " -path "
                 (mapconcat
                  #'identity
                  ;; TODO: Replace delq+mapcar with seq-keep when Emacs 29.1 is the minimum version
                  (delq nil (mapcar (lambda (ignore)
                                      (cond ((stringp ignore)
                                             (shell-quote-argument
                                              (concat "*/" ignore)))
                                            ((consp ignore)
                                             (and (funcall (car ignore) dir)
                                                  (shell-quote-argument
                                                   (concat "*/"
                                                           (cdr ignore)))))))
                                    grep-find-ignored-directories))
                  " -o -path ")
                 " "
                 (shell-quote-argument ")")
                 " -prune -o "))
    (and grep-find-ignored-files
         (concat (shell-quote-argument "!") " -type d "
                 (shell-quote-argument "(")
                 ;; we should use shell-quote-argument here
                 " -name "
                 (mapconcat
                  #'(lambda (ignore)
                      (cond ((stringp ignore)
                             (shell-quote-argument ignore))
                            ((consp ignore)
                             (and (funcall (car ignore) dir)
                                  (shell-quote-argument
                                   (cdr ignore))))))
                  grep-find-ignored-files
                  " -o -name ")
                 " "
                 (shell-quote-argument ")")
                 " -prune -o "))
    (and projectile-grep-find-ignored-paths
         (concat (shell-quote-argument "(")
                 " -path "
                 (mapconcat
                  (lambda (ignore) (shell-quote-argument
                                    (concat "./" ignore)))
                  projectile-grep-find-ignored-paths
                  " -o -path ")
                 " "
                 (shell-quote-argument ")")
                 " -prune -o "))
    (and projectile-grep-find-ignored-patterns
         (concat (shell-quote-argument "(")
                 (and (or projectile-grep-find-unignored-paths
                          projectile-grep-find-unignored-patterns)
                      (concat " "
                              (shell-quote-argument "(")))
                 " -path "
                 (mapconcat
                  (lambda (ignore)
                    (shell-quote-argument
                     (if (string-prefix-p "*" ignore) ignore
                       (concat "*/" ignore))))
                  projectile-grep-find-ignored-patterns
                  " -o -path ")
                 (and (or projectile-grep-find-unignored-paths
                          projectile-grep-find-unignored-patterns)
                      (concat " "
                              (shell-quote-argument ")")
                              " -a "
                              (shell-quote-argument "!")
                              " "
                              (shell-quote-argument "(")
                              (and projectile-grep-find-unignored-paths
                                   (concat " -path "
                                           (mapconcat
                                            (lambda (ignore) (shell-quote-argument
                                                              (concat "./" ignore)))
                                            projectile-grep-find-unignored-paths
                                            " -o -path ")))
                              (and projectile-grep-find-unignored-paths
                                   projectile-grep-find-unignored-patterns
                                   " -o")
                              (and projectile-grep-find-unignored-patterns
                                   (concat " -path "
                                           (mapconcat
                                            (lambda (ignore)
                                              (shell-quote-argument
                                               (if (string-prefix-p "*" ignore) ignore
                                                 (concat "*/" ignore))))
                                            projectile-grep-find-unignored-patterns
                                            " -o -path ")))
                              " "
                              (shell-quote-argument ")")))
                 " "
                 (shell-quote-argument ")")
                 " -prune -o ")))))

;;; Project search
;;
;; `projectile-search' runs a text search over the project using a pluggable
;; backend.  Backends live in `projectile-search-backends' and are selected via
;; `projectile-search-backend'; register your own (deadgrep, consult-ripgrep,
;; ...) with `projectile-register-search-backend'.  The registry helpers below
;; are deliberately family-agnostic so the same mechanism can drive other
;; command families later on.

;;;; Generic backend registry

(defun projectile-register-backend (registry-symbol name &rest plist)
  "Register backend NAME (a symbol) into REGISTRY-SYMBOL's alist.
REGISTRY-SYMBOL names a variable holding an alist of (NAME . PLIST)
descriptors; an existing entry for NAME is replaced.  PLIST properties
are family-specific, but `:description' (a string) and `:available' (a
zero-argument predicate, or nil for \"always available\") are understood
by the resolution helpers below."
  (set registry-symbol
       (cons (cons name plist)
             (assq-delete-all name (symbol-value registry-symbol)))))

(defun projectile--backend-available-p (backend)
  "Return non-nil when BACKEND, a (NAME . PLIST) descriptor, is usable."
  (let ((predicate (plist-get (cdr backend) :available)))
    (or (null predicate) (funcall predicate))))

(defun projectile--resolve-backend (backends preference family)
  "Return a usable backend from BACKENDS, honouring PREFERENCE.
BACKENDS is an alist of (NAME . PLIST).  PREFERENCE is a backend name, or
`auto' to pick the first available backend, or `prompt' to ask.  FAMILY
is a noun used in prompts and errors, e.g. \"search\".  Signals a
`user-error' when no suitable backend is available."
  (let ((available (seq-filter #'projectile--backend-available-p backends)))
    (cond
     ((null available)
      (user-error "No %s backend is available" family))
     ((eq preference 'auto)
      (car available))
     ((eq preference 'prompt)
      (assq (intern (completing-read
                     (format "%s backend: " (capitalize family))
                     (mapcar (lambda (b) (symbol-name (car b))) available)
                     nil t))
            backends))
     (t
      (let ((backend (assq preference backends)))
        (cond
         ((null backend)
          (user-error "Unknown %s backend: %s" family preference))
         ((projectile--backend-available-p backend)
          backend)
         (t
          (user-error "The %s backend `%s' is not available" family preference))))))))

;;;; Search engines

(defun projectile--grep (search-regexp &optional files)
  "Run rgrep (or `vc-git-grep') for SEARCH-REGEXP across the project.
FILES, when non-nil, is an rgrep files specification restricting the
search.  Honours Projectile's ignore configuration and runs
`projectile-grep-finished-hook' when done."
  (require 'grep) ;; for `rgrep'
  (let ((roots (projectile-get-project-directories (projectile-acquire-root))))
    (dolist (root-dir roots)
      (require 'vc-git) ;; for `vc-git-grep'
      ;; in git projects users have the option to use `vc-git-grep' instead of `rgrep'
      (if (and (eq (projectile-project-vcs) 'git)
               projectile-use-git-grep)
          (vc-git-grep search-regexp (or files "") root-dir)
        ;; paths for find-grep should relative and without trailing /
        (let ((grep-find-ignored-files
               (seq-union (projectile--globally-ignored-file-suffixes-glob)
                          grep-find-ignored-files))
              (projectile-grep-find-ignored-paths
               (append (mapcar (lambda (f) (directory-file-name (file-relative-name f root-dir)))
                               (projectile-ignored-directories))
                       (mapcar (lambda (file)
                                 (file-relative-name file root-dir))
                               (projectile-ignored-files))))
              (projectile-grep-find-unignored-paths
               (append (mapcar (lambda (f) (directory-file-name (file-relative-name f root-dir)))
                               (projectile-unignored-directories))
                       (mapcar (lambda (file)
                                 (file-relative-name file root-dir))
                               (projectile-unignored-files))))
              (projectile-grep-find-ignored-patterns (projectile-patterns-to-ignore))
              (projectile-grep-find-unignored-patterns (projectile-patterns-to-ensure)))
          (grep-compute-defaults)
          (cl-letf (((symbol-function 'rgrep-default-command) #'projectile-rgrep-default-command))
            (rgrep search-regexp (or files "* .*") root-dir)
            (when (get-buffer "*grep*")
              ;; When grep is using a global *grep* buffer rename it to be
              ;; scoped to the current root to allow multiple concurrent grep
              ;; operations, one per root
              (with-current-buffer "*grep*"
                (rename-buffer (concat "*grep <" root-dir ">*") t)))))))
    (run-hooks 'projectile-grep-finished-hook)))

(defun projectile--ag (search-term &optional regexp)
  "Run an `ag' search for SEARCH-TERM in the project.
When REGEXP is non-nil, SEARCH-TERM is treated as a regular expression.
Requires the `ag' Emacs package."
  (unless (require 'ag nil 'noerror)
    (user-error "Package `ag' is not available"))
  (let ((ag-command (if regexp 'ag-regexp 'ag))
        (ag-ignore-list (delq nil
                              (seq-uniq
                               (append
                                ag-ignore-list
                                (projectile-ignored-files-rel)
                                (projectile-ignored-directories-rel)
                                (projectile--globally-ignored-file-suffixes-glob)
                                ;; ag supports git ignore files directly
                                (unless (eq (projectile-project-vcs) 'git)
                                  (append grep-find-ignored-files
                                          grep-find-ignored-directories
                                          '()))))))
        ;; reset the prefix arg, otherwise it will affect the ag-command
        (current-prefix-arg nil))
    (funcall ag-command search-term (projectile-acquire-root))))

(defun projectile--ripgrep-ignore-globs ()
  "Return ripgrep `--glob' exclusions for the globally ignored files and dirs.

Uses the `--glob=!PATTERN' form rather than `--glob \\='!PATTERN\\='', whose
surrounding single quotes are only stripped by POSIX shells - on Windows
`cmd' they become part of the pattern and the exclusion silently fails
\(see #1946)."
  (mapcar (lambda (val) (concat "--glob=!" val))
          (append projectile-globally-ignored-files
                  projectile-globally-ignored-directories)))

(defun projectile--ripgrep (search-term &optional regexp)
  "Run a ripgrep (rg) search for SEARCH-TERM in the project.
When REGEXP is non-nil, SEARCH-TERM is treated as a regular expression.
Requires the `ripgrep' or `rg' Emacs package."
  (let ((args (projectile--ripgrep-ignore-globs)))
    ;; we rely on the external packages ripgrep and rg for the actual search
    (cond ((require 'ripgrep nil 'noerror)
           (ripgrep-regexp search-term
                           (projectile-acquire-root)
                           (if regexp
                               args
                             (cons "--fixed-strings --hidden" args))))
          ((require 'rg nil 'noerror)
           (rg-run search-term
                   "*"                       ;; all files
                   (projectile-acquire-root)
                   (not regexp)              ;; literal search?
                   nil                       ;; no need to confirm
                   args))
          (t (user-error "Packages `ripgrep' and `rg' are not available")))))

;;;; Search backends registry

(defvar projectile-search-backends nil
  "Alist of registered `projectile-search' backends.
Each entry is (NAME . PLIST); see `projectile-register-search-backend'.")

(defun projectile-register-search-backend (name &rest plist)
  "Register NAME as a `projectile-search' backend with PLIST properties.
Recognised PLIST keys:
  :description  a human-readable string shown in prompts;
  :available    a zero-argument predicate returning non-nil when the
                backend can be used (omit for an always-available one);
  :search       a function called as (SEARCH-TERM REGEXP) to run the
                search, REGEXP being non-nil when the term is a regular
                expression.
Use this to plug in your own search tool, e.g.:

  (projectile-register-search-backend \\='deadgrep
    :description \"deadgrep\"
    :available (lambda () (require \\='deadgrep nil t))
    :search (lambda (term _regexp) (deadgrep term (projectile-acquire-root))))"
  (apply #'projectile-register-backend 'projectile-search-backends name plist))

;; Built-ins.  Registered so the `auto' preference favours ripgrep, then grep
;; (which is always available); ag is only used when explicitly selected.
(projectile-register-search-backend 'ag
  :description "the Silver Searcher (ag)"
  :available (lambda () (require 'ag nil 'noerror))
  :search #'projectile--ag)
(projectile-register-search-backend 'grep
  :description "grep (rgrep / git-grep)"
  :search (lambda (term _regexp) (projectile--grep term)))
(projectile-register-search-backend 'ripgrep
  :description "ripgrep (rg)"
  :available (lambda () (or (require 'ripgrep nil 'noerror)
                            (require 'rg nil 'noerror)))
  :search #'projectile--ripgrep)

;;;###autoload
(defun projectile-search (&optional search-term regexp)
  "Search the project for SEARCH-TERM using `projectile-search-backend'.

With a prefix argument treat SEARCH-TERM as a regular expression (for the
backends that distinguish literal from regexp searches).

The backend is chosen from `projectile-search-backends' according to
`projectile-search-backend'; register new ones with
`projectile-register-search-backend'."
  (interactive (list nil current-prefix-arg))
  ;; Fail fast (with a friendly error) before prompting when not in a project.
  (projectile-acquire-root)
  (let* ((backend (projectile--resolve-backend projectile-search-backends
                                                projectile-search-backend
                                                "search"))
         (term (or search-term
                   (projectile--read-search-string-with-default
                    (format "Search %s%s for"
                            (projectile--search-tool-tag (car backend))
                            (if regexp " regexp" ""))))))
    (funcall (plist-get (cdr backend) :search) term regexp)))

;;;###autoload
(defun projectile-grep (&optional regexp arg)
  "Perform rgrep in the project (the grep `projectile-search' backend).

With a prefix ARG asks for files (globbing-aware) which to grep in.
With prefix ARG of `-' (such as `M--'), default the files (without prompt),
to `projectile-grep-default-files'.

With REGEXP given, don't query the user for a regexp."
  (interactive "i\nP")
  ;; Fail fast (with a friendly error) before prompting when not in a project.
  (projectile-acquire-root)
  (let ((search-regexp (or regexp
                           (projectile--read-search-string-with-default
                            (format "Search %s for"
                                    (projectile--search-tool-tag "grep")))))
        (files (and arg (or (and (equal current-prefix-arg '-)
                                 (projectile-grep-default-files))
                            (read-string (projectile-prepend-project-name "Grep in: ")
                                         (projectile-grep-default-files))))))
    (projectile--grep search-regexp files)))

;;;###autoload
(defun projectile-ag (search-term &optional arg)
  "Run an ag search with SEARCH-TERM in the project.
This is `projectile-search' with the ag backend.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression."
  (interactive
   (list (projectile--read-search-string-with-default
          (format "Search %s%s for"
                  (projectile--search-tool-tag "ag")
                  (if current-prefix-arg " regexp" "")))
         current-prefix-arg))
  (let ((projectile-search-backend 'ag))
    (projectile-search search-term arg)))

;;;###autoload
(defun projectile-ripgrep (search-term &optional arg)
  "Run a ripgrep (rg) search with SEARCH-TERM in the project.
This is `projectile-search' with the ripgrep backend.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression.

This command depends on the Emacs packages ripgrep or rg being
installed to work."
  (interactive
   (list (projectile--read-search-string-with-default
          (format "Search %s%s for"
                  (projectile--search-tool-tag "ripgrep")
                  (if current-prefix-arg " regexp" "")))
         current-prefix-arg))
  (let ((projectile-search-backend 'ripgrep))
    (projectile-search search-term arg)))

(defun projectile--project-ignore-globs (root)
  "Return ROOT's ignore patterns as globs in `project-ignores' format.
Derived from Projectile's ignore configuration: globally ignored
directory names and file suffixes match at any depth, while the
files and directories ignored via the project's dirconfig
\(`.projectile') are rooted at ROOT with a leading `./'."
  (let ((default-directory root))
    (append
     ;; globally ignored directory names, matched at any depth
     (mapcar (lambda (name)
               (concat (directory-file-name name) "/"))
             (projectile-globally-ignored-directory-names))
     ;; globally ignored files, matched at any depth
     (copy-sequence projectile-globally-ignored-files)
     ;; globally ignored file suffixes as globs, e.g. "*.elc"
     (projectile--globally-ignored-file-suffixes-glob)
     ;; dirconfig patterns, matched by base name at any depth
     (projectile-patterns-to-ignore)
     ;; dirconfig ignored directories, rooted at the project root
     (mapcar (lambda (dir)
               (concat "./" (file-relative-name (directory-file-name dir)) "/"))
             (projectile-project-ignored-directories))
     ;; dirconfig ignored files, rooted at the project root
     (mapcar (lambda (file)
               (concat "./" (file-relative-name file)))
             (projectile-project-ignored-files)))))

(defun projectile-find-references (&optional symbol)
  "Find textual references to SYMBOL across the current project.

SYMBOL defaults to the active region or the symbol at point.  The search
is scoped to the project root and honours Projectile's ignore
configuration (`.projectile' and the globally-ignored files and
directories), like Projectile's other search commands.

This is a backend-agnostic textual search (it greps the project for
SYMBOL).  For semantic references from a language server or tags table,
use the built-in `xref-find-references', which is scoped to the
Projectile project too when `projectile-mode' is enabled."
  (interactive)
  (require 'xref)
  (let* ((project-root (projectile-acquire-root))
         (symbol (or symbol
                     (read-string
                      (projectile-prepend-project-name "Find references to: ")
                      (projectile-symbol-or-selection-at-point))))
         (ignores (projectile--project-ignore-globs project-root))
         ;; `xref-matches-in-directory' greps for the pattern (honouring
         ;; IGNORES) and re-matches it in-buffer to pin down columns, so a
         ;; plain quoted symbol is the portable choice: symbol/word-boundary
         ;; constructs (`\\_<', `\\b') don't survive the translation to the
         ;; platform grep.
         (regexp (regexp-quote symbol))
         (fetcher (lambda ()
                    (xref-matches-in-directory regexp "*" project-root ignores))))
    (xref-show-xrefs fetcher nil)))

(defmacro projectile-with-default-dir (dir &rest body)
  "Invoke in DIR the BODY."
  (declare (debug t) (indent 1))
  `(let ((default-directory ,dir))
     ,@body))

;;;###autoload
(defun projectile-run-command-in-root ()
  "Invoke `execute-extended-command' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-acquire-root)
    (call-interactively #'execute-extended-command)))

;;;###autoload
(defun projectile-run-shell-command-in-root (command &optional output-buffer error-buffer)
  "Invoke `shell-command' in the project's root."
  (interactive (list (read-shell-command "Shell command: ")))
  (projectile-with-default-dir (projectile-acquire-root)
    (shell-command command output-buffer error-buffer)))

;;;###autoload
(defun projectile-run-async-shell-command-in-root (command &optional output-buffer error-buffer)
  "Invoke `async-shell-command' in the project's root."
  (interactive (list (read-shell-command "Async shell command: ")))
  (projectile-with-default-dir (projectile-acquire-root)
    (async-shell-command command output-buffer error-buffer)))

;;;###autoload
(defun projectile-run-gdb ()
  "Invoke `gdb' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-acquire-root)
    (call-interactively 'gdb)))

;;; Shells, REPLs and terminals
;;
;; `projectile-run' launches a shell, REPL or terminal in the project root
;; using a pluggable backend (built on the generic registry defined for
;; `projectile-search').  Register your own with
;; `projectile-register-shell-backend'.

;;;; Engines for the built-in shells (always available)

(defun projectile--run-shell (new-process &optional _other-window)
  "Invoke `shell' in the project's root.
NEW-PROCESS forces creation of a new process instead of reusing an
existing buffer."
  (let ((project (projectile-acquire-root)))
    (projectile-with-default-dir project
      (shell (projectile-generate-process-name "shell" new-process project)))))

(defun projectile--run-eshell (new-process &optional _other-window)
  "Invoke `eshell' in the project's root.
NEW-PROCESS forces creation of a new process instead of reusing an
existing buffer."
  (let ((project (projectile-acquire-root)))
    (projectile-with-default-dir project
      (let ((eshell-buffer-name (projectile-generate-process-name "eshell" new-process project)))
        (eshell)))))

(defun projectile--run-ielm (new-process &optional _other-window)
  "Invoke `ielm' in the project's root.
NEW-PROCESS forces creation of a new process instead of reusing an
existing buffer."
  (let* ((project (projectile-acquire-root))
         (ielm-buffer-name (projectile-generate-process-name "ielm" new-process project)))
    (if (get-buffer ielm-buffer-name)
        (switch-to-buffer ielm-buffer-name)
      (projectile-with-default-dir project
        (ielm))
      ;; ielm's buffer name is hardcoded, so we have to rename it after creation
      (rename-buffer ielm-buffer-name))))

(defun projectile--run-term (new-process &optional _other-window)
  "Invoke `term' in the project's root.
NEW-PROCESS forces creation of a new process instead of reusing an
existing buffer."
  (let* ((project (projectile-acquire-root))
         (buffer-name (projectile-generate-process-name "term" new-process project))
         (default-program (or explicit-shell-file-name
                              (getenv "ESHELL")
                              (getenv "SHELL")
                              "/bin/sh")))
    (unless (get-buffer buffer-name)
      (require 'term)
      (let ((program (read-from-minibuffer "Run program: " default-program)))
        (projectile-with-default-dir project
          (set-buffer (term-ansi-make-term buffer-name program))
          (term-mode)
          (term-char-mode))))
    (switch-to-buffer buffer-name)))

;;;; Engines for the package-backed terminals

(defun projectile--vterm (&optional new-process other-window)
  "Invoke `vterm' in the project's root.

Use argument NEW-PROCESS to indicate creation of a new process instead.
Use argument OTHER-WINDOW to indicate whether the buffer should
be displayed in a different window.

Switch to the project specific term buffer if it already exists."
  (let* ((project (projectile-acquire-root))
         (buffer (projectile-generate-process-name "vterm" new-process project)))
    (unless (require 'vterm nil 'noerror)
      (user-error "Package 'vterm' is not available"))
    (if (buffer-live-p (get-buffer buffer))
        (if other-window
            (switch-to-buffer-other-window buffer)
          (switch-to-buffer buffer))
      (projectile-with-default-dir project
        (if other-window
            (vterm-other-window buffer)
          (vterm buffer))))))

(defun projectile--eat (&optional new-process other-window)
  "Invoke `eat' in the project's root.

Use argument NEW-PROCESS to indicate creation of a new process instead.
Use argument OTHER-WINDOW to indicate whether the buffer should
be displayed in a different window.

Switch to the project specific eat buffer if it already exists."
  (let* ((project (projectile-acquire-root))
         (eat-buffer-name (projectile-generate-process-name "eat" new-process project)))
    (unless (require 'eat nil 'noerror)
      (user-error "Package 'eat' is not available"))
    (projectile-with-default-dir project
      (if other-window
          (eat-other-window nil new-process)
        (eat nil new-process)))))

(defun projectile--ghostel (&optional new-process other-window)
  "Invoke `ghostel' in the project's root.

Use argument NEW-PROCESS to indicate creation of a new process instead.
Use argument OTHER-WINDOW to indicate whether the buffer should
be displayed in a different window.

Switch to the project specific ghostel buffer if it already exists."
  (unless (require 'ghostel nil 'noerror)
    (user-error "Package 'ghostel' is not available"))
  (let* ((project (projectile-acquire-root))
         (ghostel-buffer-name
          (projectile-generate-process-name "ghostel" new-process project))
         (display-buffer-overriding-action
          (and other-window '((display-buffer-pop-up-window)))))
    (projectile-with-default-dir project
      (ghostel))))

;;;; Shell backend registry

(defvar projectile-shell-backends nil
  "Alist of registered `projectile-run' shell/REPL/terminal backends.
Each entry is (NAME . PLIST); see `projectile-register-shell-backend'.")

(defun projectile-register-shell-backend (name &rest plist)
  "Register NAME as a `projectile-run' backend with PLIST properties.
Recognised PLIST keys:
  :description  a human-readable string shown in prompts;
  :available    a zero-argument predicate returning non-nil when the
                backend can be used (omit for an always-available one);
  :run          a function called as (NEW-PROCESS OTHER-WINDOW) that
                launches the shell/REPL/terminal in the project root.
NEW-PROCESS is the command's prefix argument (start a fresh process);
OTHER-WINDOW requests display in another window (honoured by the
terminals that support it).

Use this to plug in your own terminal, e.g.:

  (projectile-register-shell-backend \\='mistty
    :description \"mistty\"
    :available (lambda () (require \\='mistty nil t))
    :run (lambda (_new-process _other-window)
           (mistty-in-project)))"
  (apply #'projectile-register-backend 'projectile-shell-backends name plist))

(projectile-register-shell-backend 'shell
  :description "shell" :run #'projectile--run-shell)
(projectile-register-shell-backend 'eshell
  :description "eshell" :run #'projectile--run-eshell)
(projectile-register-shell-backend 'ielm
  :description "ielm (Emacs Lisp REPL)" :run #'projectile--run-ielm)
(projectile-register-shell-backend 'term
  :description "term" :run #'projectile--run-term)
(projectile-register-shell-backend 'vterm
  :description "vterm"
  :available (lambda () (require 'vterm nil 'noerror))
  :run #'projectile--vterm)
(projectile-register-shell-backend 'eat
  :description "eat"
  :available (lambda () (require 'eat nil 'noerror))
  :run #'projectile--eat)
(projectile-register-shell-backend 'ghostel
  :description "ghostel"
  :available (lambda () (require 'ghostel nil 'noerror))
  :run #'projectile--ghostel)

(defun projectile--run (preference new-process other-window)
  "Launch the shell backend PREFERENCE, passing NEW-PROCESS and OTHER-WINDOW.
PREFERENCE is resolved against `projectile-shell-backends' the same way
`projectile-search-backend' is (a name, `auto', or `prompt')."
  (funcall (plist-get (cdr (projectile--resolve-backend
                            projectile-shell-backends preference "shell"))
                      :run)
           new-process other-window))

;;;###autoload
(defun projectile-run (&optional arg)
  "Run a shell, REPL or terminal in the project root.

The backend is chosen from `projectile-shell-backends' according to
`projectile-shell-backend'; register new ones with
`projectile-register-shell-backend'.

With a prefix ARG, start a fresh process instead of reusing an existing
one."
  (interactive "P")
  (projectile--run projectile-shell-backend arg nil))

;;;###autoload
(defun projectile-run-shell (&optional arg)
  "Invoke `shell' in the project's root (the shell `projectile-run' backend).
Use a prefix argument ARG to indicate creation of a new process instead."
  (interactive "P")
  (projectile--run 'shell arg nil))

;;;###autoload
(defun projectile-run-eshell (&optional arg)
  "Invoke `eshell' in the project's root (the eshell `projectile-run' backend).
Use a prefix argument ARG to indicate creation of a new process instead."
  (interactive "P")
  (projectile--run 'eshell arg nil))

;;;###autoload
(defun projectile-run-ielm (&optional arg)
  "Invoke `ielm' in the project's root (the ielm `projectile-run' backend).
Use a prefix argument ARG to indicate creation of a new process instead."
  (interactive "P")
  (projectile--run 'ielm arg nil))

;;;###autoload
(defun projectile-run-term (&optional arg)
  "Invoke `term' in the project's root (the term `projectile-run' backend).
Use a prefix argument ARG to indicate creation of a new process instead."
  (interactive "P")
  (projectile--run 'term arg nil))

;;;###autoload
(defun projectile-run-vterm (&optional arg)
  "Invoke `vterm' in the project's root (the vterm `projectile-run' backend).
Use a prefix argument ARG to indicate creation of a new process instead."
  (interactive "P")
  (projectile--run 'vterm arg nil))

;;;###autoload (autoload 'projectile-run-vterm-other-window "projectile" nil t)
(projectile--define-display-variants projectile-run-vterm (&optional arg)
  "Invoke `vterm' in the project's root, displayed in another %s.
Use a prefix argument ARG to indicate creation of a new process instead."
  :places (window)
  (projectile--run 'vterm arg t))

;;;###autoload
(defun projectile-run-eat (&optional arg)
  "Invoke `eat' in the project's root (the eat `projectile-run' backend).
Use a prefix argument ARG to indicate creation of a new process instead."
  (interactive "P")
  (projectile--run 'eat arg nil))

;;;###autoload (autoload 'projectile-run-eat-other-window "projectile" nil t)
(projectile--define-display-variants projectile-run-eat (&optional arg)
  "Invoke `eat' in the project's root, displayed in another %s.
Use a prefix argument ARG to indicate creation of a new process instead."
  :places (window)
  (projectile--run 'eat arg t))

;;;###autoload
(defun projectile-run-ghostel (&optional arg)
  "Invoke `ghostel' in the project's root (the ghostel `projectile-run' backend).
Use a prefix argument ARG to indicate creation of a new process instead."
  (interactive "P")
  (projectile--run 'ghostel arg nil))

;;;###autoload (autoload 'projectile-run-ghostel-other-window "projectile" nil t)
(projectile--define-display-variants projectile-run-ghostel (&optional arg)
  "Invoke `ghostel' in the project's root, displayed in another %s.
Use a prefix argument ARG to indicate creation of a new process instead."
  :places (window)
  (projectile--run 'ghostel arg t))

(defun projectile-files-from-cmd (cmd directory)
  "Use a grep-like CMD to search for files within DIRECTORY.

CMD should include the necessary search params and should output
equivalently to grep -HlI (only unique matching filenames).
Returns a list of expanded filenames."
  (let ((default-directory directory))
    (mapcar (lambda (str)
              ;; `expand-file-name' (rather than `concat') so the results
              ;; are in canonical form even when DIRECTORY is abbreviated
              ;; (e.g. "~/project/"), and thus comparable with other
              ;; expanded file names (#1115).
              (expand-file-name str directory))
            (split-string
             (string-trim (shell-command-to-string cmd))
             "\n+"
             t))))

;; The listing is case-insensitive on purpose: it's only used to narrow
;; down the files to visit, and the actual (case-sensitive or smart-case)
;; matching happens in Emacs afterwards.  A case-sensitive listing would
;; silently drop files that a case-insensitive replacement should have
;; touched (#1115).
;;
;; This alist is the customizable source of each tool's *base* command.
;; How the optional file-extension filter is layered on top lives in
;; `projectile--search-tool-descriptors', and both are combined by
;; `projectile--construct-files-with-string-command'.
(defvar projectile-files-with-string-commands
  '((rg . "rg -liF --no-heading --color never ")
    (ag . "ag --literal --ignore-case --nocolor --noheading -l ")
    (ack . "ack --literal --ignore-case --nocolor -l ")
    (git . "git grep -HlIiF ")
    ;; -r: recursive
    ;; -H: show filename for each match
    ;; -l: show only file names with matches
    ;; -I: no binary files
    ;; -i: ignore case
    ;; -F: interpret pattern as fixed string, not regexp
    (grep . "grep -rHlIiF %s .")))

;; One descriptor per search tool, capturing how the (optional) file
;; extension filter is expressed.  Combined with the tool's base command
;; from `projectile-files-with-string-commands', this drives
;; `projectile--construct-files-with-string-command' so the five
;; per-tool constructors need not repeat the same skeleton.  Keys:
;;
;;   :kind        how the filter attaches to the base command --
;;                `prefix' inserts it between the base and the search term
;;                (rg, ag); `suffix' appends it after the whole command
;;                (git, grep); `pipe' feeds a separate file listing into
;;                the base command (ack).
;;   :ext-regexp  when non-nil, the extension glob is turned into an
;;                anchored regexp via `projectile--search-glob-to-regexp'
;;                (ag, ack) rather than passed through verbatim.
;;   :ext-open    text emitted just before the extension.
;;   :ext-close   text emitted just after the extension.
;;   :term-format when non-nil, the base command is a format string whose
;;                %s is the search term (grep) rather than a prefix the
;;                term is concatenated onto.
(defvar projectile--search-tool-descriptors
  '((rg   . (:kind prefix :ext-open "-g '" :ext-close "' "))
    (ag   . (:kind prefix :ext-regexp t :ext-open "-G " :ext-close "$ "))
    (ack  . (:kind pipe :ext-regexp t))
    (git  . (:kind suffix :ext-open "  -- '" :ext-close "'"))
    (grep . (:kind suffix :term-format t :ext-open " --include '" :ext-close "'"))))

(defun projectile--search-glob-to-regexp (file-ext)
  "Turn extension glob FILE-EXT into the regexp body used by ag/ack.
Dots are escaped and \"*\" wildcards dropped, e.g. \"*.el\" becomes
\"\\.el\"; the caller anchors it with a trailing \"$\"."
  (replace-regexp-in-string
   "\\*" ""
   (replace-regexp-in-string "\\." "\\\\." file-ext)))

(defun projectile--construct-files-with-string-command (tool search-term &optional file-ext)
  "Build TOOL's files-with-string command for SEARCH-TERM.
The base command comes from `projectile-files-with-string-commands'
and, when FILE-EXT is a string, the extension filter described by
`projectile--search-tool-descriptors' is layered on top."
  (let* ((base (alist-get tool projectile-files-with-string-commands))
         (desc (alist-get tool projectile--search-tool-descriptors))
         (term-format (plist-get desc :term-format))
         ;; the plain, no-extension command, shared by every :kind
         (core (if term-format (format base search-term)
                 (concat base search-term))))
    (if (not (stringp file-ext))
        core
      (let ((ext (if (plist-get desc :ext-regexp)
                     (projectile--search-glob-to-regexp file-ext)
                   file-ext))
            (open (plist-get desc :ext-open))
            (close (plist-get desc :ext-close)))
        (pcase (plist-get desc :kind)
          ('prefix (concat base open ext close search-term))
          ('suffix (concat core open ext close))
          ('pipe (concat "ack -g '" ext "$' | " base "-x " search-term)))))))

(defun projectile--rg-construct-command (search-term &optional file-ext)
  "Construct Rg option to search files by the extension FILE-EXT."
  (projectile--construct-files-with-string-command 'rg search-term file-ext))

(defun projectile--ag-construct-command (search-term &optional file-ext)
  "Construct Ag option to search files by the extension FILE-EXT."
  (projectile--construct-files-with-string-command 'ag search-term file-ext))

(defun projectile--ack-construct-command (search-term &optional file-ext)
  "Construct Ack option to search files by the extension FILE-EXT."
  (projectile--construct-files-with-string-command 'ack search-term file-ext))

(defun projectile--git-grep-construct-command (search-term &optional file-ext)
  "Construct Grep option to search files by the extension FILE-EXT."
  (projectile--construct-files-with-string-command 'git search-term file-ext))

(defun projectile--grep-construct-command (search-term &optional file-ext)
  "Construct Grep option to search files by the extension FILE-EXT."
  (projectile--construct-files-with-string-command 'grep search-term file-ext))

(defun projectile-files-with-string (string directory &optional file-ext)
  "Return a list of all files containing STRING in DIRECTORY.

Tries to use rg, ag, ack, git-grep, and grep in that order.  If those
are impossible (for instance on Windows), returns a list of all
files in the project."
  (if (projectile-unixy-system-p)
      (let* ((search-term (shell-quote-argument string))
             (cmd (cond ((executable-find "rg")
                         (projectile--rg-construct-command search-term file-ext))
                        ((executable-find "ag")
                         (projectile--ag-construct-command search-term file-ext))
                        ((executable-find "ack")
                         (projectile--ack-construct-command search-term file-ext))
                        ((and (executable-find "git")
                              (eq (projectile-project-vcs) 'git))
                         (projectile--git-grep-construct-command search-term file-ext))
                        (t
                         (projectile--grep-construct-command search-term file-ext)))))
        (projectile-files-from-cmd cmd directory))
    ;; we have to reject directories as a workaround to work with git submodules
    (seq-remove
     #'file-directory-p
     (mapcar #'(lambda (file) (expand-file-name file directory))
             (projectile-dir-files directory)))))

(defun projectile--replace-in-files (from to files)
  "Query-replace matches of the regexp FROM with TO in FILES.

Buffers that are already visiting one of FILES are scanned from the
beginning of the buffer; older Emacsen (< 28.1) would otherwise resume
the scan from point in such buffers and silently skip any matches
before it (#1677)."
  (dolist (file files)
    (when-let* ((buffer (get-file-buffer file)))
      (with-current-buffer buffer
        (goto-char (point-min)))))
  (fileloop-initialize-replace from to files 'default)
  (fileloop-continue))

;;;###autoload
(defun projectile-replace (&optional arg)
  "Replace a literal string in the project's files.

With a prefix argument ARG prompts you for a directory and file name patterns
on which to run the replacement."
  (interactive "P")
  (let* ((directory (if arg
                        (file-name-as-directory
                         (read-directory-name "Replace in directory: "))
                      (projectile-acquire-root)))
         (file-ext (if arg
                       (if (fboundp #'helm-grep-get-file-extensions)
                           (car (helm-grep-get-file-extensions (list directory)))
                         (read-string
                          (projectile-prepend-project-name
                           "With file extension (empty string means all files): ")))
                     nil))
         (old-text (read-string
                    (projectile-prepend-project-name "Replace: ")
                    (projectile-symbol-or-selection-at-point)))
         (new-text (read-string
                    (projectile-prepend-project-name
                     (format "Replace %s with: " old-text))))
         (files (projectile-files-with-string old-text directory file-ext))
         ;; Filter results through the project's file list so that files
         ;; ignored via .projectile or other ignore rules are excluded.
         (project-files (mapcar (lambda (file) (expand-file-name file directory))
                                (projectile-dir-files directory)))
         (filtered-files (seq-filter (lambda (f) (member f project-files)) files)))
    (projectile--replace-in-files (regexp-quote old-text) new-text filtered-files)))

;;;###autoload
(defun projectile-replace-regexp (&optional arg)
  "Replace a regexp in the project's files.

With a prefix argument ARG prompts you for a directory on which
to run the replacement."
  (interactive "P")
  (let* ((directory (if arg
                        (file-name-as-directory
                         (read-directory-name "Replace regexp in directory: "))
                      (projectile-acquire-root)))
         (old-text (read-string
                    (projectile-prepend-project-name "Replace regexp: ")
                    (projectile-symbol-or-selection-at-point)))
         (new-text (read-string
                    (projectile-prepend-project-name
                     (format "Replace regexp %s with: " old-text))))
         (files
          ;; We have to reject directories as a workaround to work with git submodules.
          ;; We also reject nonexistent files to avoid errors during replacement.
          ;;
          ;; We can't narrow the list of files with
          ;; `projectile-files-with-string' because those regexp tools
          ;; don't support Emacs regular expressions.
          (seq-remove
           (lambda (f) (or (file-directory-p f) (not (file-exists-p f))))
           (mapcar #'(lambda (file) (expand-file-name file directory))
                   (projectile-dir-files directory)))))
    (projectile--replace-in-files old-text new-text files)))

;;; Reviewable project-wide find-and-replace
;;
;; `projectile-replace' and `projectile-replace-regexp' above drive a
;; blocking, sequential query-replace walk with no preview.  The commands
;; below add a results-buffer flow instead: gather every match up front,
;; render them in a read-only buffer where each match can be toggled on or
;; off, and apply only the enabled ones (in any order).  Answers #1924.
;;
;; Matches are gathered in pure Emacs Lisp (not via grep) so that Emacs
;; regexp semantics are preserved for the regexp command and so the preview
;; reflects the exact text that will be edited, including unsaved changes in
;; already-open buffers.
;;
;; The write-back and buffer-refresh structure borrows from the GPL-3
;; packages wgrep (`wgrep-commit-file') and color-rg (`color-rg-apply-changed'):
;; edits are applied from the highest buffer position downwards so earlier
;; edits don't invalidate later match offsets, live buffers are edited in
;; place under a single `atomic-change-group', and buffers modified since the
;; scan are skipped rather than corrupted.

(defcustom projectile-replace-max-matches 5000
  "Upper bound on how many matches `projectile-replace-review' collects.
When a search would exceed this, only the first that many matches are
shown and a note is displayed."
  :group 'projectile
  :type 'natnum
  :package-version '(projectile . "3.2.0"))

(defcustom projectile-search-whole-word nil
  "Whether the reviewable search/replace start in whole-word mode.
Seeds `projectile-replace--word' for a fresh `projectile-search-review'
or `projectile-replace-review'; you can still toggle it per search with
`w' in the results buffer, and the `projectile-dispatch' `--word' switch
binds it for a single invocation."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "3.2.0"))

(defcustom projectile-replace-async t
  "Whether the reviewable search/replace commands scan asynchronously.
When non-nil (the default) `projectile-replace-review',
`projectile-search-review' and their in-buffer re-scan commands (`g',
`c', `x') scan candidate files in timer-yielded chunks: the results
buffer is shown right away, matches stream in as they are found, Emacs
stays responsive, and the scan can be canceled (`q', \\`C-g', or by
killing the buffer).  While a scan is still running, applying (`!') and
exporting (`e') refuse until it finishes so the write-back never runs
against a partial match set.  When nil the scan runs synchronously in one
blocking pass instead.

Regardless of this setting, in batch mode (`noninteractive') the scan is
always synchronous so scripted runs stay deterministic.  The final match
set is identical whether scanning runs asynchronously or synchronously;
async only changes when and how matches are delivered."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "3.2.0"))

(defcustom projectile-search-use-ripgrep t
  "Whether `projectile-search-review' accelerates literal search with ripgrep.
When non-nil (the default) a literal `projectile-search-review' scan
uses ripgrep (`rg') to find matches when the `rg' executable is
available, which returns near-instantly even on a large project;
otherwise, and always for the regexp search command and for every part
of the replace reviewer, the portable pure-Emacs-Lisp scan is used.

The ripgrep fast-path's result set follows ripgrep's own ignore rules
\(`.gitignore', `.ignore', hidden-file handling, and so on) plus
Projectile's ignore globs (`.projectile' and the globally-ignored files
and directories, passed to `rg' via `--glob'), and skips matches in files
that aren't valid UTF-8, so it can differ slightly from the pure-elisp
path's `projectile-dir-files' set (for example in how
hidden files or symlinks are treated).  This is an accepted trade-off for
speed; set this to nil to force the elisp scan, whose result set matches
Projectile's ignore configuration exactly.

The regexp search command always uses the elisp scan (ripgrep's regex
syntax is not Emacs regexp syntax), and the whole replace reviewer always
uses the elisp scan (its write-back needs the exact buffer positions the
elisp scan records).  In batch mode (`noninteractive') the elisp scan is
used so scripted runs stay deterministic."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "3.2.0"))

(defcustom projectile-replace-scan-chunk-size 24
  "Number of candidate files scanned per async chunk before yielding.
Each chunk scans this many files, delivers the matches into the results
buffer and re-renders, then yields to redisplay via a zero-delay timer
before the next chunk.  Larger values scan faster but redisplay less
often; smaller values keep Emacs more responsive."
  :group 'projectile
  :type 'natnum
  :package-version '(projectile . "3.2.0"))

(defface projectile-replace-file
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for the per-file header lines in the replace results buffer."
  :group 'projectile
  :package-version '(projectile . "3.2.0"))

(defface projectile-replace-match
  '((t :inherit match))
  "Face for a matched span shown without a pending replacement."
  :group 'projectile
  :package-version '(projectile . "3.2.0"))

(defface projectile-replace-old
  '((t :inherit diff-removed :strike-through t))
  "Face for the old text of a match with a pending replacement."
  :group 'projectile
  :package-version '(projectile . "3.2.0"))

(defface projectile-replace-new
  '((t :inherit diff-added))
  "Face for the new text of a match with a pending replacement."
  :group 'projectile
  :package-version '(projectile . "3.2.0"))

(defface projectile-replace-line-number
  '((t :inherit shadow))
  "Face for the LINE:COL locator of each match."
  :group 'projectile
  :package-version '(projectile . "3.2.0"))

(defface projectile-replace-header
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the status header line of the replace results buffer."
  :group 'projectile
  :package-version '(projectile . "3.2.0"))

(defvar projectile-replace-buffer-name "*projectile-replace*"
  "Name of the buffer used by `projectile-replace-review'.")

(cl-defstruct (projectile-replace--match
               (:constructor projectile-replace--match-create)
               (:copier nil))
  "A single match collected for the reviewable replace UI."
  file        ; absolute file name
  buffer      ; live buffer visiting FILE, or nil when scanned from disk
  tick        ; `buffer-chars-modified-tick' of BUFFER at scan time (or nil)
  line        ; 1-based line number of the match
  column      ; 0-based character offset of the match within its line
  beg         ; buffer position of the match start (in BUFFER or a disk re-read)
  end         ; buffer position of the match end
  string      ; the matched text
  match-data  ; copy of `(match-data t)' for the match (positions only)
  groups      ; list of matched-group strings, index 0 = whole match
  context     ; the whole line the match sits on
  enabled)    ; non-nil when the match will be applied

;; Buffer-local state of a `*projectile-replace*' buffer.
(defvar-local projectile-replace--root nil
  "Project root the current results buffer was gathered from.")
(defvar-local projectile-replace--term nil
  "Raw search term the current results buffer was gathered with.")
(defvar-local projectile-replace--search nil
  "Emacs regexp actually searched for (the term, `regexp-quote'd if literal).")
(defvar-local projectile-replace--replacement nil
  "Pending replacement string for the current results buffer.")
(defvar-local projectile-replace--literal nil
  "Non-nil when the current results buffer is a literal (not regexp) replace.")
(defvar-local projectile-replace--case-fold nil
  "Non-nil when the current search ignores case.
Initialized from `case-fold-search' at the first search and bound
around every (re-)gather so it drives which matches are collected.")
(defvar-local projectile-replace--word nil
  "Non-nil when the current search only matches whole words.
Initialized from `projectile-search-whole-word' at the first search; the
scan regexp is fenced with word boundaries (and ripgrep gets
`--word-regexp') while it holds.")
(defvar-local projectile-replace--matches nil
  "List of `projectile-replace--match' structs shown in the results buffer.")
(defvar-local projectile-replace--truncated nil
  "Non-nil when the match list was capped at `projectile-replace-max-matches'.")
(defvar-local projectile-replace--filtered nil
  "Non-nil when the shown match list was pruned by a filter command.
Re-searching (\\<projectile-replace-mode-map>\\[projectile-replace--refresh]) gathers from scratch and clears this.")
(defvar-local projectile-replace--scanning nil
  "Non-nil while an asynchronous scan is still filling this results buffer.
While set, matches are streaming in, the header shows a progress note,
and applying and exporting refuse (the write-back must never run against
a partial match set).  Cleared when the scan finishes or is canceled.")
(defvar-local projectile-replace--scan-timer nil
  "The in-flight async scan timer for this results buffer, or nil.
Held so a re-scan, a quit, or killing the buffer can cancel a scan that
is still running.")
(defvar-local projectile-replace--scan-process nil
  "The in-flight ripgrep subprocess filling this results buffer, or nil.
Only the read-only search reviewer's ripgrep fast-path uses this; the
pure-elisp async scan uses `projectile-replace--scan-timer' instead.
Held so a re-scan, a quit, or killing the buffer can kill a scan that is
still running, leaving no orphaned process.")
(defvar-local projectile-replace--scan-generation 0
  "Monotonic token identifying the current async scan of this buffer.
Every (re-)scan and every cancel bumps it; a chunk timer carries the
generation it was scheduled under and no-ops when it no longer matches.
This way a timer that already fired and is waiting to run can't append
to a newer scan's match list if it is superseded in the meantime.")
(defvar-local projectile-replace--render-function #'projectile-replace--render
  "Function that redraws the current results buffer from its state.
The scanning, navigation, filter and toggle machinery is shared
between the replace reviewer and the read-only search reviewer
(`projectile-search-mode'); this buffer-local seam lets those shared
commands redraw with the current mode's renderer.  It defaults to the
replace renderer and search mode rebinds it to its own.")

(defun projectile-replace--expand (replacement groups literal)
  "Expand REPLACEMENT for a match whose group strings are GROUPS.
GROUPS is a list of matched substrings, index 0 being the whole match.
When LITERAL is non-nil REPLACEMENT is returned verbatim; otherwise
\\N and \\& references are expanded from GROUPS (\\\\ yields a
backslash).  The same expansion drives both the preview and the actual
write-back so the two can never diverge."
  (if literal
      replacement
    (let ((result "")
          (i 0)
          (len (length replacement)))
      (while (< i len)
        (let ((ch (aref replacement i)))
          (if (and (eq ch ?\\) (< (1+ i) len))
              (let ((next (aref replacement (1+ i))))
                (cond
                 ((eq next ?&)
                  (setq result (concat result (or (nth 0 groups) ""))))
                 ((and (>= next ?0) (<= next ?9))
                  (setq result (concat result (or (nth (- next ?0) groups) ""))))
                 ((eq next ?\\)
                  (setq result (concat result "\\")))
                 (t (setq result (concat result (char-to-string next)))))
                (setq i (+ i 2)))
            (setq result (concat result (char-to-string ch)))
            (setq i (1+ i)))))
      result)))

(defun projectile-replace--capture-groups ()
  "Return the matched-group strings for the last search in this buffer.
Index 0 is the whole match; unmatched optional groups are nil."
  (let ((n (/ (length (match-data)) 2))
        (groups nil))
    (dotimes (i n)
      (push (match-string i) groups))
    (nreverse groups)))

(defun projectile-replace--binary-p ()
  "Return non-nil when the current buffer looks like binary content.
Only the leading portion is inspected, which is enough to skip files
whose NUL bytes would make an in-buffer replacement meaningless."
  (save-excursion
    (goto-char (point-min))
    (search-forward "\0" (min (point-max) (+ (point-min) 8000)) t)))

(defun projectile-replace--scan-region (file buffer regexp budget)
  "Collect up to BUDGET matches of REGEXP in the current buffer.
Each match is recorded as a `projectile-replace--match' tagged with
FILE and BUFFER (nil when scanning a disk re-read)."
  (let ((matches nil)
        (count 0)
        (done nil)
        (tick (and buffer (buffer-chars-modified-tick buffer))))
    (save-excursion
      (goto-char (point-min))
      (while (and (not done)
                  (< count budget)
                  (re-search-forward regexp nil t))
        (if (= (match-beginning 0) (match-end 0))
            ;; A regexp that can match the empty string (e.g. `^', `a*')
            ;; leaves point put; advance past it, but at end-of-buffer there
            ;; is nowhere to advance, so stop rather than spin forever.
            (if (eobp)
                (setq done t)
              (forward-char 1))
          (let ((beg (match-beginning 0))
                (end (match-end 0))
                (md (match-data t))
                (groups (projectile-replace--capture-groups))
                line lstart col context)
            (save-excursion
              (goto-char beg)
              (setq line (line-number-at-pos)
                    lstart (line-beginning-position)
                    col (- beg lstart)
                    context (buffer-substring-no-properties
                             lstart (line-end-position))))
            (push (projectile-replace--match-create
                   :file file :buffer buffer :tick tick
                   :line line :column col :beg beg :end end
                   :string (buffer-substring-no-properties beg end)
                   :match-data md :groups groups
                   :context context :enabled t)
                  matches)
            (cl-incf count)))))
    (nreverse matches)))

(defun projectile-replace--scan-file (file regexp budget)
  "Return up to BUDGET matches of REGEXP in FILE.
A file visited in a live buffer is scanned from that buffer's current
text (so the preview matches what will be edited, even when the buffer
has unsaved changes); otherwise it is read from disk into a temp buffer.
Binary-looking and unreadable files are skipped."
  ;; Capture the intended `case-fold-search' (bound dynamically around the
  ;; gather) before entering a live buffer, then re-establish it there: some
  ;; major modes make `case-fold-search' buffer-local, which would otherwise
  ;; shadow the dynamic binding and make the case toggle a no-op for that file.
  (let ((buffer (get-file-buffer file))
        (fold case-fold-search))
    (if buffer
        (with-current-buffer buffer
          (let ((case-fold-search fold))
            (projectile-replace--scan-region file buffer regexp budget)))
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (unless (projectile-replace--binary-p)
              (let ((case-fold-search fold))
                (projectile-replace--scan-region file nil regexp budget))))
        (error nil)))))

(defun projectile-replace--gather (candidates regexp)
  "Scan CANDIDATES for REGEXP, capped at `projectile-replace-max-matches'.
Return a plist with `:matches' (the collected structs, in file order)
and `:truncated' (non-nil when the cap was hit)."
  (let ((all nil)
        (budget projectile-replace-max-matches)
        (truncated nil))
    (dolist (file candidates)
      (if (> budget 0)
          (let ((ms (projectile-replace--scan-file file regexp budget)))
            (setq all (append all ms)
                  budget (- budget (length ms))))
        ;; a candidate was left unscanned because the cap was already hit
        (setq truncated t)))
    (list :matches all :truncated truncated)))

;;; Asynchronous, cancelable scanning
;;
;; The synchronous `projectile-replace--gather' above stays the primitive that
;; batch runs and the tests drive.  The async driver below reuses the very same
;; per-file `projectile-replace--scan-file', so the structs it produces are
;; identical to what `--gather' would produce over the same candidate list and
;; regexp -- only the DELIVERY changes: files are scanned in timer-yielded
;; chunks, matches stream into the results buffer as they are found, and the
;; scan can be canceled.  The `case-fold-search' that `--scan-file' reads is
;; re-established from the buffer's `projectile-replace--case-fold' inside each
;; chunk (the dynamic binding used by the sync path is gone once we run from a
;; timer).

(defun projectile-replace--async-p ()
  "Return non-nil when scanning should run asynchronously.
True when `projectile-replace-async' is set and we are interactive;
batch (`noninteractive') always scans synchronously so scripted runs stay
deterministic."
  (and projectile-replace-async (not noninteractive)))

(defun projectile-replace--cancel-scan ()
  "Cancel any in-flight async scan in the current results buffer.
Kills the pending chunk timer (if any) and clears the scanning flag, so
no timer is left dangling.  Safe to call when nothing is scanning; used
on re-scan, on quit, and from `kill-buffer-hook'."
  (when projectile-replace--scan-timer
    (cancel-timer projectile-replace--scan-timer))
  (when (process-live-p projectile-replace--scan-process)
    ;; drop our sentinel first so killing it doesn't run the finish handler
    (set-process-sentinel projectile-replace--scan-process #'ignore)
    (delete-process projectile-replace--scan-process))
  ;; Bump the generation so a chunk timer that already fired and is queued
  ;; behind us no-ops rather than resurrecting this scan.
  (cl-incf projectile-replace--scan-generation)
  (setq projectile-replace--scan-timer nil
        projectile-replace--scan-process nil
        projectile-replace--scanning nil))

(defun projectile-replace--gather-async (candidates regexp buffer on-done)
  "Scan CANDIDATES for REGEXP into BUFFER incrementally, then call ON-DONE.
Resets BUFFER's match list and scanning state, then processes CANDIDATES
in `projectile-replace-scan-chunk-size' batches, each batch delivering
its matches and re-rendering before yielding to redisplay via a
zero-delay timer.  Matches accumulate in file order, so the final list is
identical to `projectile-replace--gather' over the same CANDIDATES and
REGEXP; `projectile-replace-max-matches' and the `:truncated' note are
honored the same way.  ON-DONE (or nil) is called in BUFFER once the scan
finishes.  A scan already running in BUFFER should be canceled first (see
`projectile-replace--cancel-scan')."
  (let ((generation
         (with-current-buffer buffer
           (setq projectile-replace--matches nil
                 projectile-replace--truncated nil
                 projectile-replace--filtered nil
                 projectile-replace--scanning t
                 projectile-replace--scan-timer nil)
           (cl-incf projectile-replace--scan-generation))))
    (projectile-replace--scan-step
     buffer candidates regexp projectile-replace-max-matches on-done generation)))

(defun projectile-replace--scan-step (buffer remaining regexp budget on-done generation)
  "Scan one chunk of REMAINING candidates for REGEXP into BUFFER.
BUDGET is the remaining match allowance.  GENERATION is the scan token
this chunk belongs to; the step no-ops when it no longer matches BUFFER's
current `projectile-replace--scan-generation' (i.e. the scan was
superseded or canceled).  Delivers this chunk's matches into BUFFER and
re-renders; while candidates and budget remain it schedules itself for
the next chunk via a zero-delay timer, otherwise it finishes: clears the
scanning state, does a final render and calls ON-DONE.  Guarded against a
killed BUFFER (leaving no work behind) and against \\`C-g' during a chunk
\(which cancels the scan cleanly)."
  (when (and (buffer-live-p buffer)
             (= generation
                (buffer-local-value 'projectile-replace--scan-generation buffer)))
    (condition-case nil
        (let ((fold (buffer-local-value 'projectile-replace--case-fold buffer))
              (count 0)
              (new nil)
              (truncated nil)
              (stop nil))
          ;; scan up to a chunk of files, mirroring the sync `--gather' loop:
          ;; a file is scanned while budget remains; the first file reached
          ;; with the budget exhausted marks the list truncated and stops.
          (while (and remaining (not stop)
                      (< count projectile-replace-scan-chunk-size))
            (if (> budget 0)
                (let ((ms (let ((case-fold-search fold))
                            (projectile-replace--scan-file
                             (car remaining) regexp budget))))
                  (setq new (append new ms)
                        budget (- budget (length ms))
                        remaining (cdr remaining)))
              (setq truncated t stop t))
            (cl-incf count))
          (with-current-buffer buffer
            (when new
              (setq projectile-replace--matches
                    (append projectile-replace--matches new)))
            (when truncated
              (setq projectile-replace--truncated t)))
          (if (and remaining (not stop))
              ;; more to do: show progress and yield to redisplay
              (with-current-buffer buffer
                ;; schedule the next chunk BEFORE rendering, so a render error
                ;; can't strand the scan with the flag set and no pending timer
                (setq projectile-replace--scan-timer
                      (run-with-timer 0 nil
                                      #'projectile-replace--scan-step
                                      buffer remaining regexp budget on-done generation))
                (funcall projectile-replace--render-function))
            ;; finished (or budget-truncated): settle and hand off
            (with-current-buffer buffer
              (setq projectile-replace--scanning nil
                    projectile-replace--scan-timer nil)
              (funcall projectile-replace--render-function)
              (when on-done (funcall on-done buffer)))))
      (quit
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (projectile-replace--cancel-scan)
           (funcall projectile-replace--render-function)))))))

(defun projectile-replace--ensure-not-scanning ()
  "Refuse with a `user-error' when the results buffer is still scanning.
The write-back and export must never run against a partial match set."
  (when projectile-replace--scanning
    (user-error "Still searching; wait for the scan to finish")))

(defun projectile-replace--candidates (term literal case-fold directory)
  "Return the files under DIRECTORY worth scanning for TERM.
For a case-sensitive LITERAL search this narrows to files that contain
TERM, via `projectile-files-with-string' - which matches
case-insensitively, so the narrowed set is a safe superset that the scan
then filters exactly - intersected with the project's ignore-aware file
list.  For a regexp search, or a case-insensitive literal search, it
returns the whole ignore-aware file list, since those can't be narrowed
by an external grep this way."
  (let ((project-files (mapcar (lambda (f) (expand-file-name f directory))
                               (projectile-dir-files directory))))
    (if (and literal (not case-fold))
        (seq-filter (lambda (f) (member f project-files))
                    (projectile-files-with-string term directory))
      (seq-remove (lambda (f) (or (file-directory-p f) (not (file-exists-p f))))
                  project-files))))

;;; Results buffer rendering

(defun projectile-replace--file-header (file root count)
  "Return a propertized header line for FILE relative to ROOT with COUNT matches."
  (propertize
   (concat (propertize (file-relative-name file root)
                       'face 'projectile-replace-file)
           (format " (%d)\n" count))
   'projectile-replace-file file))

(defun projectile-replace--render-line (m replacement literal)
  "Return the propertized results line for match M.
When REPLACEMENT is non-empty and M is enabled the matched text is
shown struck out followed by the expanded replacement; LITERAL selects
verbatim vs. capture-group expansion.  The whole line carries the match
struct under the `projectile-replace-match' text property."
  (let* ((enabled (projectile-replace--match-enabled m))
         (col (projectile-replace--match-column m))
         (ctx (projectile-replace--match-context m))
         (mstr (projectile-replace--match-string m))
         (before (substring ctx 0 (min col (length ctx))))
         (after (if (<= (+ col (length mstr)) (length ctx))
                    (substring ctx (+ col (length mstr)))
                  ""))
         (has-repl (and replacement (not (string-empty-p replacement))))
         (highlight
          (if (and enabled has-repl)
              (concat (propertize mstr 'face 'projectile-replace-old)
                      (propertize (projectile-replace--expand
                                   replacement
                                   (projectile-replace--match-groups m)
                                   literal)
                                  'face 'projectile-replace-new))
            (propertize mstr 'face 'projectile-replace-match)))
         (indicator (if enabled "  [X] " "  [ ] "))
         (locator (propertize (format "%d:%d: "
                                      (projectile-replace--match-line m)
                                      (1+ col))
                              'face 'projectile-replace-line-number))
         (line (concat indicator locator before highlight after "\n")))
    (propertize line 'projectile-replace-match m)))

(defun projectile-replace--header-string ()
  "Return the propertized status header for the results buffer.
Shows the term, the replacement (or \"(none)\"), the match and file
counts, the mode flags (regexp/literal and case), and a note when the
list has been filtered.  The header carries no `projectile-replace-match'
property, so match navigation skips it."
  (let* ((matches projectile-replace--matches)
         (nmatches (length matches))
         (seen (make-hash-table :test 'equal))
         (repl (if (and projectile-replace--replacement
                        (not (string-empty-p projectile-replace--replacement)))
                   (format "%S" projectile-replace--replacement)
                 "(none)"))
         nfiles flags)
    (dolist (m matches)
      (puthash (projectile-replace--match-file m) t seen))
    (setq nfiles (hash-table-count seen)
          flags (concat
                 (if projectile-replace--literal "[literal]" "[regexp]")
                 " "
                 (if projectile-replace--case-fold
                     "[ignore-case]" "[case-sensitive]")
                 (if projectile-replace--word " [word]" "")
                 (if projectile-replace--filtered "  filtered" "")
                 (projectile-replace--scanning-note nmatches)))
    (propertize
     (format "Replace %S with %s\n%d match%s in %d file%s  %s\n"
             projectile-replace--term repl
             nmatches (if (= nmatches 1) "" "es")
             nfiles (if (= nfiles 1) "" "s")
             flags)
     'face 'projectile-replace-header)))

(defun projectile-replace--scanning-note (nmatches)
  "Return a progress note for a results buffer still scanning, else \"\".
NMATCHES is the count found so far.  Shown in the status header while an
async scan streams matches in."
  (if projectile-replace--scanning
      (format "  Searching... %d match%s so far"
              nmatches (if (= nmatches 1) "" "es"))
    ""))

(defun projectile-replace--render ()
  "Redraw the current results buffer from its buffer-local state."
  (let ((inhibit-read-only t)
        (root projectile-replace--root)
        (replacement projectile-replace--replacement)
        (literal projectile-replace--literal)
        (matches projectile-replace--matches)
        (counts (make-hash-table :test 'equal))
        (prev-file nil))
    (dolist (m matches)
      (cl-incf (gethash (projectile-replace--match-file m) counts 0)))
    (erase-buffer)
    (insert (projectile-replace--header-string))
    (insert (substitute-command-keys
             (concat "\\<projectile-replace-mode-map>"
                     "\\[projectile-replace--toggle] toggle  "
                     "\\[projectile-replace--toggle-file] toggle file  "
                     "\\[projectile-replace--set-replacement] set replacement  "
                     "\\[projectile-replace--apply] apply  "
                     "\\[projectile-replace--export] export  "
                     "\\[projectile-replace--refresh] re-search  "
                     "\\[projectile-replace--visit] visit  "
                     "\\[projectile-replace--quit] quit\n"
                     "\\[projectile-replace--toggle-case] case  "
                     "\\[projectile-replace--toggle-regexp] regexp/literal  "
                     "\\[projectile-replace--toggle-word] word  "
                     "\\[projectile-replace--keep-matches]/\\[projectile-replace--flush-matches] keep/flush line  "
                     "\\[projectile-replace--keep-files]/\\[projectile-replace--flush-files] keep/flush file\n\n")))
    (if (null matches)
        (insert "No matches.\n")
      (dolist (m matches)
        (let ((file (projectile-replace--match-file m)))
          (unless (equal prev-file file)
            (when prev-file (insert "\n"))
            (setq prev-file file)
            (insert (projectile-replace--file-header
                     file root (gethash file counts))))
          (insert (projectile-replace--render-line m replacement literal)))))
    (when projectile-replace--truncated
      (insert (format "\n(showing the first %d matches)\n"
                      projectile-replace-max-matches)))
    (goto-char (point-min))))

(defun projectile-replace--render-preserve ()
  "Redraw the results buffer, keeping point on the same line."
  (let ((line (line-number-at-pos)))
    (projectile-replace--render)
    (goto-char (point-min))
    (forward-line (1- line))))

;;; Results buffer commands

(defun projectile-replace--match-at-point ()
  "Return the match struct on the current line, or nil."
  (get-text-property (line-beginning-position) 'projectile-replace-match))

(defun projectile-replace--goto-next-match ()
  "Move point to the next match line."
  (interactive)
  (let ((start (point)))
    (forward-line 1)
    (while (and (not (eobp)) (not (projectile-replace--match-at-point)))
      (forward-line 1))
    (if (projectile-replace--match-at-point)
        (beginning-of-line)
      (goto-char start)
      (message "No more matches"))))

(defun projectile-replace--goto-prev-match ()
  "Move point to the previous match line."
  (interactive)
  (let ((start (point)))
    (forward-line -1)
    (while (and (not (bobp)) (not (projectile-replace--match-at-point)))
      (forward-line -1))
    (if (projectile-replace--match-at-point)
        (beginning-of-line)
      (goto-char start)
      (message "No previous matches"))))

(defun projectile-replace--goto-next-file ()
  "Move point to the next file header."
  (interactive)
  (let ((start (point)))
    (forward-line 1)
    (while (and (not (eobp))
                (not (get-text-property (line-beginning-position)
                                        'projectile-replace-file)))
      (forward-line 1))
    (if (get-text-property (line-beginning-position) 'projectile-replace-file)
        (beginning-of-line)
      (goto-char start)
      (message "No more files"))))

(defun projectile-replace--goto-prev-file ()
  "Move point to the previous file header."
  (interactive)
  (let ((start (point)))
    (forward-line -1)
    (while (and (not (bobp))
                (not (get-text-property (line-beginning-position)
                                        'projectile-replace-file)))
      (forward-line -1))
    (if (get-text-property (line-beginning-position) 'projectile-replace-file)
        (beginning-of-line)
      (goto-char start)
      (message "No previous files"))))

(defun projectile-replace--visit ()
  "Visit the match on the current line in another window."
  (interactive)
  (let ((m (projectile-replace--match-at-point)))
    (unless m (user-error "No match on this line"))
    (let ((buf (or (projectile-replace--match-buffer m)
                   (find-file-noselect (projectile-replace--match-file m))))
          (line (projectile-replace--match-line m))
          (col (projectile-replace--match-column m)))
      (switch-to-buffer-other-window buf)
      (goto-char (point-min))
      (forward-line (1- line))
      (forward-char (min col (- (line-end-position) (point)))))))

(defun projectile-replace--toggle ()
  "Toggle whether the match on the current line will be applied."
  (interactive)
  (let ((m (projectile-replace--match-at-point)))
    (unless m (user-error "No match on this line"))
    (setf (projectile-replace--match-enabled m)
          (not (projectile-replace--match-enabled m)))
    (projectile-replace--render-preserve)))

(defun projectile-replace--toggle-file ()
  "Toggle all matches in the file of the match on the current line.
If any of the file's matches are enabled they are all disabled;
otherwise they are all enabled."
  (interactive)
  (let ((m (projectile-replace--match-at-point)))
    (unless m (user-error "No match on this line"))
    (let* ((file (projectile-replace--match-file m))
           (fmatches (cl-remove-if-not
                      (lambda (x) (equal (projectile-replace--match-file x) file))
                      projectile-replace--matches))
           (target (not (cl-every #'projectile-replace--match-enabled fmatches))))
      (dolist (x fmatches)
        (setf (projectile-replace--match-enabled x) target)))
    (projectile-replace--render-preserve)))

(defun projectile-replace--set-replacement ()
  "Re-read the replacement string and re-render the previews."
  (interactive)
  (setq projectile-replace--replacement
        (read-string (format "Replace %s with: " projectile-replace--term)
                     projectile-replace--replacement))
  (projectile-replace--render-preserve))

(defun projectile-replace--regather ()
  "Re-run the search from scratch into the buffer-local match list.
Cancels any in-flight scan first, then re-scans (asynchronously when
`projectile-replace--async-p' holds, else synchronously) with
`case-fold-search' honoring `projectile-replace--case-fold', clearing any
active filter because the list is rebuilt from every match in the
project.  Because the match list is rebuilt, every match comes back
enabled: re-scanning (via `g', the case toggle, or the regexp toggle)
resets any per-match include or exclude toggles you had set.  Use the
filter commands instead to prune the list while preserving the survivors'
state.  The re-render happens from the (streaming) scan, so callers need
not render again."
  (let ((candidates (projectile-replace--candidates
                     projectile-replace--term projectile-replace--literal
                     projectile-replace--case-fold projectile-replace--root)))
    (setq projectile-replace--filtered nil)
    (projectile-replace--start (current-buffer) candidates
                               projectile-replace--search nil)))

(defun projectile-replace--refresh ()
  "Re-run the search and redraw the results buffer.
This gathers from scratch, so any filtering is undone and matches
removed by a filter command reappear."
  (interactive)
  (projectile-replace--regather))

(defun projectile-replace--toggle-case ()
  "Toggle case sensitivity of the search, re-scan, and re-render."
  (interactive)
  (setq projectile-replace--case-fold (not projectile-replace--case-fold))
  (projectile-replace--regather)
  (message "%s"
           (projectile-prepend-project-name
            (if projectile-replace--case-fold
                "search now ignores case" "search now case-sensitive"))))

(defun projectile-replace--valid-regexp-p (regexp)
  "Return non-nil when REGEXP is a valid Emacs regexp."
  (condition-case nil
      (progn (string-match-p regexp "") t)
    (error nil)))

(defun projectile-replace--toggle-regexp ()
  "Toggle between literal and regexp search, re-scan, and re-render.
Switching to regexp mode with a term that is not a valid regexp is
refused with a message so the buffer stays usable rather than erroring."
  (interactive)
  (let ((new-literal (not projectile-replace--literal)))
    (if (and (not new-literal)
             (not (projectile-replace--valid-regexp-p projectile-replace--term)))
        (message "%s"
                 (projectile-prepend-project-name
                  (format "%S is not a valid regexp; staying literal"
                          projectile-replace--term)))
      (setq projectile-replace--literal new-literal
            projectile-replace--search (if new-literal
                                           (regexp-quote projectile-replace--term)
                                         projectile-replace--term))
      (projectile-replace--regather)
      (message "%s"
               (projectile-prepend-project-name
                (if new-literal "literal search" "regexp search"))))))

(defun projectile-replace--toggle-word ()
  "Toggle whole-word matching of the search, re-scan, and re-render."
  (interactive)
  (setq projectile-replace--word (not projectile-replace--word))
  (projectile-replace--regather)
  (message "%s"
           (projectile-prepend-project-name
            (if projectile-replace--word
                "search now matches whole words only"
              "search now matches anywhere"))))

(defun projectile-replace--filter-by (predicate)
  "Keep only matches satisfying PREDICATE, mark the list filtered, re-render.
The removed matches are recoverable by re-searching (\\<projectile-replace-mode-map>\\[projectile-replace--refresh]), which
gathers from scratch.  Refused while a scan is still streaming in, since a
later chunk would append past the filter and leave an incoherent list."
  (projectile-replace--ensure-not-scanning)
  (setq projectile-replace--matches
        (cl-remove-if-not predicate projectile-replace--matches)
        projectile-replace--filtered t)
  (funcall projectile-replace--render-function))

(defun projectile-replace--line-matches-p (m regexp)
  "Return non-nil when match M's context line matches REGEXP.
Honors the buffer's case setting."
  (let ((case-fold-search projectile-replace--case-fold))
    (string-match-p regexp (projectile-replace--match-context m))))

(defun projectile-replace--file-matches-p (m regexp)
  "Return non-nil when match M's project-relative file matches REGEXP.
Honors the buffer's case setting."
  (let ((case-fold-search projectile-replace--case-fold))
    (string-match-p regexp
                    (file-relative-name (projectile-replace--match-file m)
                                        projectile-replace--root))))

(defun projectile-replace--keep-matches (regexp)
  "Keep only matches whose context line matches REGEXP."
  (interactive (list (read-regexp "Keep matches whose line matches regexp")))
  (projectile-replace--filter-by
   (lambda (m) (projectile-replace--line-matches-p m regexp))))

(defun projectile-replace--flush-matches (regexp)
  "Remove matches whose context line matches REGEXP."
  (interactive (list (read-regexp "Flush matches whose line matches regexp")))
  (projectile-replace--filter-by
   (lambda (m) (not (projectile-replace--line-matches-p m regexp)))))

(defun projectile-replace--keep-files (regexp)
  "Keep only matches whose project-relative file matches REGEXP."
  (interactive (list (read-regexp "Keep matches whose file matches regexp")))
  (projectile-replace--filter-by
   (lambda (m) (projectile-replace--file-matches-p m regexp))))

(defun projectile-replace--flush-files (regexp)
  "Remove matches whose project-relative file matches REGEXP."
  (interactive (list (read-regexp "Flush matches whose file matches regexp")))
  (projectile-replace--filter-by
   (lambda (m) (not (projectile-replace--file-matches-p m regexp)))))

;;; Applying the enabled matches

(defun projectile-replace--do-one (m replacement literal)
  "Replace match M in the current buffer with the expansion of REPLACEMENT.
LITERAL selects verbatim vs. capture-group expansion.  M's stored
`match-data' must still be valid for the current buffer."
  (set-match-data (projectile-replace--match-match-data m))
  (replace-match (projectile-replace--expand
                  replacement (projectile-replace--match-groups m) literal)
                 t t))

(defun projectile-replace--positions-valid-p (matches)
  "Return non-nil when every match in MATCHES still spans its recorded text.
Checked against the current buffer.  This is the authoritative guard
against stale positions: if the file changed on disk, or the live buffer
was edited, since the scan, the recorded spans no longer hold the matched
text and applying them would corrupt unrelated bytes."
  (cl-every (lambda (m)
              (let ((beg (projectile-replace--match-beg m))
                    (end (projectile-replace--match-end m)))
                (and (integerp beg) (integerp end)
                     (<= (point-min) beg end (point-max))
                     (string= (buffer-substring-no-properties beg end)
                              (projectile-replace--match-string m)))))
            matches))

(defun projectile-replace--skip (name reason)
  "Warn that NAME is skipped for REASON and return the symbol `skipped'."
  (message "%s"
           (projectile-prepend-project-name
            (format "skipping %s (%s)" name reason)))
  'skipped)

(defun projectile-replace--apply-file (file matches replacement literal)
  "Apply MATCHES in FILE and return the count, or the symbol `skipped'.
Edits run from the highest buffer position downwards so earlier edits
don't shift later matches.  The live buffer visiting FILE (if any) is
re-resolved now rather than trusted from scan time, so a file opened
since the scan is edited in its buffer instead of being clobbered on
disk, and a scan-time buffer that has since been killed is handled.  In
either case the recorded positions are verified to still span the matched
text; if not (the file or buffer changed since the scan) the file is
skipped rather than corrupted.  A clean buffer is saved; a buffer with
unsaved changes is edited but left for the user to save."
  (let* ((descending (sort (copy-sequence matches)
                           (lambda (a b)
                             (> (projectile-replace--match-beg a)
                                (projectile-replace--match-beg b)))))
         (buffer (get-file-buffer file)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (if (not (projectile-replace--positions-valid-p descending))
              (projectile-replace--skip (buffer-name buffer) "changed since scan")
            (let ((was-modified (buffer-modified-p)))
              (save-excursion
                (save-restriction
                  (widen)
                  (atomic-change-group
                    (dolist (m descending)
                      (projectile-replace--do-one m replacement literal)))))
              ;; don't silently save a buffer that already had unsaved edits
              (unless was-modified
                (let ((require-final-newline nil))
                  (save-buffer)))
              (length matches))))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((coding last-coding-system-used))
          (if (not (projectile-replace--positions-valid-p descending))
              (projectile-replace--skip (file-name-nondirectory file)
                                        "changed on disk since scan")
            (dolist (m descending)
              (projectile-replace--do-one m replacement literal))
            (let ((coding-system-for-write coding))
              (write-region (point-min) (point-max) file nil 'no-message))
            (length matches)))))))

(defun projectile-replace--apply ()
  "Apply every enabled match, grouped by file, then re-run the search."
  (interactive)
  (projectile-replace--ensure-not-scanning)
  (let ((enabled (cl-remove-if-not #'projectile-replace--match-enabled
                                   projectile-replace--matches))
        (replacement projectile-replace--replacement)
        (literal projectile-replace--literal)
        (groups (make-hash-table :test 'equal))
        (order nil)
        (nfiles 0)
        (nrepl 0)
        (skipped 0))
    (when (null enabled)
      (user-error "No matches are enabled"))
    ;; group the enabled matches by file, preserving first-seen order
    (dolist (m enabled)
      (let ((file (projectile-replace--match-file m)))
        (unless (gethash file groups)
          (push file order))
        (push m (gethash file groups))))
    (dolist (file (nreverse order))
      ;; isolate each file: a read-only file or a write error must not abort
      ;; the batch (leaving earlier files edited and no summary shown)
      (let ((result (condition-case err
                        (projectile-replace--apply-file
                         file (gethash file groups) replacement literal)
                      (error
                       (projectile-replace--skip
                        (file-name-nondirectory file)
                        (error-message-string err))))))
        (if (eq result 'skipped)
            (cl-incf skipped)
          (cl-incf nfiles)
          (cl-incf nrepl result))))
    (message "%s"
             (projectile-prepend-project-name
              (format "Replaced %d occurrence%s in %d file%s%s"
                      nrepl (if (= nrepl 1) "" "s")
                      nfiles (if (= nfiles 1) "" "s")
                      (if (> skipped 0)
                          (format " (skipped %d file%s)"
                                  skipped (if (= skipped 1) "" "s"))
                        ""))))
    (projectile-replace--refresh)))

;;; Exporting to a grep-mode buffer for wgrep / grep-edit-mode

(defvar projectile--grep-export-buffer-name "*projectile-grep*"
  "Name of the `grep-mode' buffer produced by `projectile-replace--export'.
Shared by the replace and search reviewers, hence the neutral name.")

(defun projectile-replace--grep-line (m root)
  "Format match M as a RELPATH:LINE:CONTEXT grep hit relative to ROOT."
  (format "%s:%d:%s"
          (file-relative-name (projectile-replace--match-file m) root)
          (projectile-replace--match-line m)
          (projectile-replace--match-context m)))

(defun projectile-replace--export-guidance ()
  "Message how to make the exported grep buffer editable.
The wording adapts to what's installed: wgrep, Emacs 31's
`grep-edit-mode', or neither.  Returns the message string."
  (let ((msg (projectile-prepend-project-name
              (cond
               ((fboundp 'wgrep-change-to-wgrep-mode)
                "exported to grep buffer; press C-c C-p to edit with wgrep, then C-c C-c to write back")
               ((fboundp 'grep-edit-mode)
                "exported to grep buffer; run M-x grep-edit-mode to edit, then C-c C-c to write back")
               (t
                "exported to a read-only grep buffer for navigation; install wgrep from MELPA to edit and write back")))))
    (message "%s" msg)
    msg))

(defun projectile-replace--export ()
  "Export the enabled matches to a `grep-mode' buffer for editing with wgrep.
Renders the matches Projectile's own apply command would act on (the
enabled matches from the reviewed and filtered list; ones toggled off are
excluded, just as they are by apply) as standard RELPATH:LINE:CONTEXT grep
hits in a `*projectile-grep*' buffer whose `default-directory' is
the project root, so the relative paths resolve.  The buffer is a real
`grep-mode' buffer navigable with `next-error' and RET, so wgrep
(`wgrep-change-to-wgrep-mode', bound to \\`C-c C-p') or Emacs 31's
`grep-edit-mode' can turn it editable and write your edits back to the
files.  This is the bridge for people who prefer the grep/wgrep workflow;
Projectile's own apply command
(\\<projectile-replace-mode-map>\\[projectile-replace--apply]) is the no-dependency path and needs no external package."
  (interactive)
  (projectile-replace--ensure-not-scanning)
  (require 'grep)
  (let ((matches (cl-remove-if-not #'projectile-replace--match-enabled
                                   projectile-replace--matches))
        (root projectile-replace--root)
        ;; label the export by which reviewer it came from (read here, before
        ;; switching to the grep buffer, so `major-mode' is the source buffer's)
        (kind (if (derived-mode-p 'projectile-search-mode) "search" "replace"))
        (buf (get-buffer-create projectile--grep-export-buffer-name)))
    (when (null matches)
      (user-error "No enabled matches to export"))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq default-directory root)
        (insert (format "-*- mode: grep; default-directory: %S -*-\n\n" root))
        ;; No colon before a value here: the search term could be `10:30' and
        ;; would otherwise parse as a phantom `file:line:' grep hit.
        (insert (format "Projectile %s  (%d match%s)\n\n"
                        kind (length matches)
                        (if (= (length matches) 1) "" "es")))
        (dolist (m matches)
          (insert (projectile-replace--grep-line m root) "\n"))
        (insert (format "\nProjectile %s export finished\n" kind)))
      (grep-mode)
      ;; keep the root as default-directory so the relative hits resolve
      (setq default-directory root)
      ;; let wgrep hook up its keys if the user has it; strictly optional
      (when (fboundp 'wgrep-setup) (wgrep-setup))
      (goto-char (point-min)))
    (pop-to-buffer buf)
    (projectile-replace--export-guidance)
    buf))

(defvar projectile-replace-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'projectile-replace--visit)
    (define-key map (kbd "n") #'projectile-replace--goto-next-match)
    (define-key map (kbd "p") #'projectile-replace--goto-prev-match)
    (define-key map (kbd "M-n") #'projectile-replace--goto-next-file)
    (define-key map (kbd "M-p") #'projectile-replace--goto-prev-file)
    (define-key map (kbd "t") #'projectile-replace--toggle)
    (define-key map (kbd "SPC") #'projectile-replace--toggle)
    (define-key map (kbd "f") #'projectile-replace--toggle-file)
    (define-key map (kbd "r") #'projectile-replace--set-replacement)
    (define-key map (kbd "c") #'projectile-replace--toggle-case)
    (define-key map (kbd "x") #'projectile-replace--toggle-regexp)
    (define-key map (kbd "w") #'projectile-replace--toggle-word)
    (define-key map (kbd "k") #'projectile-replace--keep-matches)
    (define-key map (kbd "d") #'projectile-replace--flush-matches)
    (define-key map (kbd "K") #'projectile-replace--keep-files)
    (define-key map (kbd "D") #'projectile-replace--flush-files)
    (define-key map (kbd "e") #'projectile-replace--export)
    (define-key map (kbd "!") #'projectile-replace--apply)
    (define-key map (kbd "C-c C-c") #'projectile-replace--apply)
    (define-key map (kbd "g") #'projectile-replace--refresh)
    (define-key map (kbd "q") #'projectile-replace--quit)
    map)
  "Keymap for `projectile-replace-mode'.")

(define-derived-mode projectile-replace-mode special-mode "Projectile-Replace"
  "Major mode for reviewing and applying a project-wide replacement.

Each match starts enabled and can be toggled on or off; only the
enabled matches are applied.  Besides toggling and applying, the search
itself can be reshaped without leaving the buffer: toggle case
sensitivity (\\<projectile-replace-mode-map>\\[projectile-replace--toggle-case]),
literal/regexp matching (\\[projectile-replace--toggle-regexp]) or whole-word
matching (\\[projectile-replace--toggle-word]) to re-scan, and
narrow the shown matches by keeping or flushing them against a regexp
matched on the context line (\\[projectile-replace--keep-matches] / \\[projectile-replace--flush-matches]) or the file name
(\\[projectile-replace--keep-files] / \\[projectile-replace--flush-files]).  Re-searching (\\[projectile-replace--refresh]) rebuilds the list from scratch,
undoing any filtering.

Applying with \\[projectile-replace--apply] needs no external package.  If you prefer the
grep/wgrep workflow, \\[projectile-replace--export] exports the shown matches to a `grep-mode'
buffer that wgrep or Emacs 31's `grep-edit-mode' can turn editable and
write back to the files.

\\{projectile-replace-mode-map}"
  (setq-local truncate-lines t)
  ;; killing the buffer mid-scan must not leave a dangling chunk timer
  (add-hook 'kill-buffer-hook #'projectile-replace--cancel-scan nil t)
  (buffer-disable-undo))

(defun projectile-replace--seed (buf mode root term regexp replacement
                                     literal case-fold &optional word)
  "Put BUF in MODE and seed its results-buffer state, with no matches yet.
ROOT, TERM, REGEXP, REPLACEMENT, LITERAL, CASE-FOLD and WORD seed the
search parameters; the match list starts empty, ready for a (sync or
async) scan to fill it."
  (with-current-buffer buf
    (funcall mode)
    (setq projectile-replace--root root
          projectile-replace--term term
          projectile-replace--search regexp
          projectile-replace--replacement replacement
          projectile-replace--literal literal
          projectile-replace--case-fold case-fold
          projectile-replace--word word
          projectile-replace--matches nil
          projectile-replace--truncated nil
          projectile-replace--filtered nil
          projectile-replace--scanning nil
          projectile-replace--scan-timer nil
          projectile-replace--scan-process nil)))

;;; Ripgrep fast-path for the read-only literal search reviewer
;;
;; An optional accelerator for `projectile-search-review': when the search is
;; literal, `rg' is installed and `projectile-search-use-ripgrep' is on, the
;; candidate scan runs `rg --json' as a subprocess and parses its NDJSON match
;; stream into the very same `projectile-replace--match' structs the elisp scan
;; produces, streaming them into the results buffer.  Only the fields the
;; search reviewer renders are filled (file, line, character column, matched
;; string, context line); the write-back-only fields (`beg'/`end'/`match-data'/
;; `groups') stay nil because the search->replace bridge re-gathers via elisp.
;; This path is deliberately narrow: the regexp search command keeps the elisp
;; scan (rg's Rust regex is not Emacs regexp) and the whole replace reviewer
;; keeps the elisp scan (its apply needs exact buffer positions).  The elisp
;; scan stays the default and the fallback; rg is purely additive.

(defun projectile-search--rg-executable ()
  "Return the ripgrep executable name if available, else nil."
  (and (executable-find "rg") "rg"))

(defun projectile-search--rg-fastpath-p (literal)
  "Return non-nil when the search reviewer should use the ripgrep fast-path.
True for a LITERAL search when `projectile-search-use-ripgrep' is set,
`rg' is available, and we are interactive; batch (`noninteractive') keeps
the deterministic elisp scan."
  (and projectile-search-use-ripgrep
       literal
       (not noninteractive)
       (projectile-search--rg-executable)
       t))

(defun projectile-search--rg-command (term case-fold word globs)
  "Build the `rg --json' command line searching for literal TERM.
CASE-FOLD selects case-insensitive matching; WORD restricts matches to
whole words (`--word-regexp'); GLOBS is a list of ignore patterns (from
`projectile--project-ignore-globs') passed as `--glob' exclusions so
Projectile's ignores narrow ripgrep's own ignore rules.  The search path
is the relative `./', so the caller must run the process with
`default-directory' bound to the project root."
  (append
   (list (projectile-search--rg-executable)
         "--json" "--fixed-strings" "--line-number" "--column"
         "--color" "never"
         (if case-fold "--ignore-case" "--case-sensitive"))
   (when word (list "--word-regexp"))
   ;; Projectile's `project-ignores' globs mark a root-anchored pattern with a
   ;; leading `./' (e.g. a `.projectile' `-/vendor' line); ripgrep spells a
   ;; root-anchored glob with a leading `/', so translate `./PAT' to `/PAT'.
   ;; rg honors that only when searching a path relative to the root, which is
   ;; why the search path below is `./' and the caller binds default-directory.
   (mapcan (lambda (g)
             (list "--glob"
                   (concat "!" (if (string-prefix-p "./" g)
                                   (substring g 1)
                                 g))))
           globs)
   ;; `--' terminates options so a TERM starting with `-' is not misread.
   (list "--" term "./")))

(defun projectile-search--rg-json-get (obj &rest keys)
  "Walk KEYS through nested hash-table OBJ, returning the leaf or nil.
OBJ is a `json-parse-string' object (a hash-table with string keys)."
  (dolist (k keys obj)
    (setq obj (and (hash-table-p obj) (gethash k obj)))))

(defun projectile-search--rg-byte->char-column (line byte)
  "Return the 0-based character column for BYTE offset into LINE.
BYTE is a UTF-8 byte offset into the line text (as ripgrep reports
submatch offsets); LINE is the already-decoded line string.  The prefix
is re-encoded to UTF-8 and its BYTE-long head decoded back, so multibyte
characters before the match count as one column each, not one per byte."
  (if (or (null byte) (<= byte 0))
      0
    (let* ((bytes (encode-coding-string line 'utf-8))
           (n (min byte (length bytes))))
      (length (decode-coding-string (substring bytes 0 n) 'utf-8)))))

(defun projectile-search--rg-parse-line (line root)
  "Parse one ripgrep NDJSON LINE into a list of match structs under ROOT.
Returns nil for non-\"match\" records (begin/end/summary/context) and for
records without decodable path or line text.  A line with several
submatches yields one struct per submatch, in order."
  (condition-case nil
      (let ((obj (json-parse-string line)))
        (when (equal (gethash "type" obj) "match")
          (let* ((data (gethash "data" obj))
                 (path (projectile-search--rg-json-get data "path" "text"))
                 (line-no (projectile-search--rg-json-get data "line_number"))
                 (text (projectile-search--rg-json-get data "lines" "text"))
                 (subs (gethash "submatches" data))
                 (context (and (stringp text)
                               (replace-regexp-in-string "\r?\n\\'" "" text)))
                 (result nil))
            (when (and (stringp path) (integerp line-no) (stringp text) subs)
              (let ((file (expand-file-name path root)))
                (dotimes (i (length subs))
                  (let* ((sub (aref subs i))
                         (start (gethash "start" sub))
                         (mstr (projectile-search--rg-json-get
                                sub "match" "text")))
                    (when (stringp mstr)
                      (push (projectile-replace--match-create
                             :file file :buffer nil :tick nil
                             :line line-no
                             :column (projectile-search--rg-byte->char-column
                                      text start)
                             :beg nil :end nil
                             :string mstr :match-data nil :groups nil
                             :context context :enabled t)
                            result))))))
            (nreverse result))))
    (error nil)))

(defun projectile-search--rg-ingest (buffer lines root)
  "Parse rg NDJSON LINES into BUFFER's match list, render, honor the cap.
Appends up to `projectile-replace-max-matches' matches in arrival order;
when the cap is reached the surplus is dropped and the truncated flag is
set.  Returns non-nil when the cap has been reached, so the caller can
finish the scan."
  (with-current-buffer buffer
    (if projectile-replace--truncated
        t
      (let ((new nil))
        (dolist (l lines)
          (unless (string-empty-p l)
            (setq new (nconc new (projectile-search--rg-parse-line l root)))))
        (let ((room (- projectile-replace-max-matches
                       (length projectile-replace--matches))))
          (when (> (length new) room)
            (setq new (take (max 0 room) new)
                  projectile-replace--truncated t))
          (when new
            (setq projectile-replace--matches
                  (append projectile-replace--matches new)))
          (funcall projectile-replace--render-function)
          projectile-replace--truncated)))))

(defun projectile-search--rg-finish (buffer on-done)
  "Settle BUFFER after its ripgrep scan ends and call ON-DONE.
Kills the scan process if still live (dropping its sentinel first),
clears the scanning state, does a final render and calls ON-DONE.  Safe
against a killed BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (process-live-p projectile-replace--scan-process)
        (set-process-sentinel projectile-replace--scan-process #'ignore)
        (delete-process projectile-replace--scan-process))
      (setq projectile-replace--scanning nil
            projectile-replace--scan-process nil
            projectile-replace--scan-timer nil)
      (funcall projectile-replace--render-function)
      (when on-done (funcall on-done buffer)))))

(defun projectile-search--gather-rg (buffer term on-done)
  "Scan for literal TERM into BUFFER via `rg --json', then call ON-DONE.
Resets BUFFER's match list and scanning state, launches `rg' as a
subprocess reading ROOT, CASE-FOLD and the ignore globs from BUFFER's
buffer-locals, and streams parsed matches into the buffer as ripgrep
emits them, re-rendering per output chunk.  `projectile-replace-max-matches'
is honored (the process is killed when the cap is hit) and the scan is
cancelable and kill-safe exactly like the elisp async engine: the process
is registered in `projectile-replace--scan-process' so
`projectile-replace--cancel-scan' (re-search, quit, kill-buffer) can kill
it, leaving no orphan.  ON-DONE (or nil) is called in BUFFER when the scan
finishes."
  (let* ((root (buffer-local-value 'projectile-replace--root buffer))
         (case-fold (buffer-local-value 'projectile-replace--case-fold buffer))
         (word (buffer-local-value 'projectile-replace--word buffer))
         (globs (projectile--project-ignore-globs root))
         (command (projectile-search--rg-command term case-fold word globs))
         (pending ""))
    (with-current-buffer buffer
      (setq projectile-replace--matches nil
            projectile-replace--truncated nil
            projectile-replace--filtered nil
            projectile-replace--scanning t
            projectile-replace--scan-timer nil
            projectile-replace--scan-process nil))
    (let* (;; run rg IN the project root so the relative `./' search path and
           ;; the `/'-anchored ignore globs resolve against it
           (default-directory root)
           ;; keep rg's stderr out of the JSON stream, so a diagnostic line
           ;; (e.g. an unreadable directory) can't split a match across filter
           ;; chunks and get silently dropped
           (stderr-buffer (get-buffer-create " *projectile-search-rg-stderr*"))
           (_ (with-current-buffer stderr-buffer (erase-buffer)))
           (proc
           (make-process
            :name "projectile-search-rg"
            :buffer nil
            :command command
            :connection-type 'pipe
            :noquery t
            :stderr stderr-buffer
            :coding 'utf-8-unix
            :filter
            (lambda (_proc output)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (unless projectile-replace--truncated
                    (setq pending (concat pending output))
                    (let ((parts (split-string pending "\n")))
                      ;; the last element is the (possibly empty) partial line
                      (setq pending (car (last parts)))
                      (when (projectile-search--rg-ingest
                             buffer (butlast parts) root)
                        (projectile-search--rg-finish buffer on-done)))))))
            :sentinel
            (lambda (proc _event)
              (when (and (buffer-live-p buffer)
                         (memq (process-status proc) '(exit signal)))
                (with-current-buffer buffer
                  (when (eq proc projectile-replace--scan-process)
                    (unless projectile-replace--truncated
                      (projectile-search--rg-ingest buffer (list pending) root))
                    (projectile-search--rg-finish buffer on-done))))))))
      (with-current-buffer buffer
        (setq projectile-replace--scan-process proc))
      proc)))

(defun projectile-replace--word-boundary-regexp (regexp)
  "Fence REGEXP with word boundaries so it only matches whole words.
The pattern is shy-grouped and wrapped in `\\<'/`\\>', mirroring what
ripgrep's `--word-regexp' does, so the elisp scan and the ripgrep
fast-path agree on what counts as a whole-word match."
  (concat "\\<\\(?:" regexp "\\)\\>"))

(defun projectile-replace--effective-regexp (regexp)
  "Return REGEXP as the current results buffer will actually match it.
Fences it with word boundaries when `projectile-replace--word' is set,
otherwise returns REGEXP unchanged."
  (if projectile-replace--word
      (projectile-replace--word-boundary-regexp regexp)
    regexp))

(defun projectile-replace--start (buffer candidates regexp on-done)
  "Fill BUFFER's match list by scanning CANDIDATES for REGEXP.
Cancels any in-flight scan in BUFFER first, then scans with the async
chunked driver when `projectile-replace--async-p' holds and otherwise
synchronously (always in batch), so the final match list is identical
either way -- only delivery differs.  BUFFER is re-rendered when done and
ON-DONE (or nil) is called in it.

As an optional accelerator, a read-only search-reviewer BUFFER doing a
literal search takes the ripgrep fast-path (`projectile-search--gather-rg')
when `projectile-search--rg-fastpath-p' holds; the replace reviewer and
the regexp search always take the elisp path below."
  (with-current-buffer buffer
    (projectile-replace--cancel-scan))
  (cond
   ((with-current-buffer buffer
      (and (derived-mode-p 'projectile-search-mode)
           (projectile-search--rg-fastpath-p projectile-replace--literal)))
    (projectile-search--gather-rg
     buffer (buffer-local-value 'projectile-replace--term buffer) on-done))
   ((projectile-replace--async-p)
    (projectile-replace--gather-async
     candidates
     (with-current-buffer buffer (projectile-replace--effective-regexp regexp))
     buffer on-done))
   (t
    (with-current-buffer buffer
      (let* ((case-fold-search projectile-replace--case-fold)
             (result (projectile-replace--gather
                      candidates (projectile-replace--effective-regexp regexp))))
        (setq projectile-replace--matches (plist-get result :matches)
              projectile-replace--truncated (plist-get result :truncated)
              projectile-replace--scanning nil
              projectile-replace--scan-timer nil))
      (funcall projectile-replace--render-function)
      (when on-done (funcall on-done buffer))))))

(defun projectile-replace--open-finish (buffer)
  "Announce truncation once the scan filling BUFFER has finished."
  (with-current-buffer buffer
    (when projectile-replace--truncated
      (message "%s"
               (projectile-prepend-project-name
                (format "showing the first %d matches"
                        projectile-replace-max-matches))))))

(defun projectile-replace--open (mode buf-name root term regexp replacement
                                      literal case-fold candidates no-match-msg
                                      &optional word)
  "Open BUF-NAME in MODE and scan CANDIDATES for REGEXP into it.
When scanning is asynchronous the buffer is shown immediately and matches
stream in; when synchronous (always in batch) the scan completes first
and, to preserve the pre-async behavior, no buffer is shown when nothing
matched -- NO-MATCH-MSG is issued instead.  Returns the results buffer,
or nil on the synchronous no-match path.  ROOT, TERM, REGEXP,
REPLACEMENT, LITERAL, CASE-FOLD and WORD seed the buffer state."
  ;; re-running the command must not orphan a scan still filling an earlier
  ;; instance of the buffer (re-seeding resets its buffer-locals)
  (when-let* ((existing (get-buffer buf-name)))
    (with-current-buffer existing (projectile-replace--cancel-scan)))
  (if (or (projectile-replace--async-p)
          ;; the read-only search reviewer's ripgrep fast-path is inherently
          ;; async, so it opens the streaming buffer even when the elisp async
          ;; engine is off (`projectile-replace-async' nil); `--start' then
          ;; dispatches it to ripgrep
          (and (eq mode #'projectile-search-mode)
               (projectile-search--rg-fastpath-p literal)))
      (let ((buf (get-buffer-create buf-name)))
        (projectile-replace--seed buf mode root term regexp replacement
                                  literal case-fold word)
        (with-current-buffer buf
          (setq projectile-replace--scanning t)
          (funcall projectile-replace--render-function))
        (pop-to-buffer buf)
        (projectile-replace--start buf candidates regexp
                                   #'projectile-replace--open-finish)
        buf)
    (let* ((case-fold-search case-fold)
           (scan-regexp (if word
                            (projectile-replace--word-boundary-regexp regexp)
                          regexp))
           (result (projectile-replace--gather candidates scan-regexp))
           (matches (plist-get result :matches))
           (truncated (plist-get result :truncated)))
      (if (null matches)
          (progn (when no-match-msg (message "%s" no-match-msg)) nil)
        (let ((buf (get-buffer-create buf-name)))
          (projectile-replace--seed buf mode root term regexp replacement
                                    literal case-fold word)
          (with-current-buffer buf
            (setq projectile-replace--matches matches
                  projectile-replace--truncated truncated)
            (funcall projectile-replace--render-function))
          (when truncated
            (message "%s"
                     (projectile-prepend-project-name
                      (format "showing the first %d matches"
                              projectile-replace-max-matches))))
          (pop-to-buffer buf)
          buf)))))

(defun projectile-replace--quit ()
  "Cancel any in-flight scan and quit the results window."
  (interactive)
  (projectile-replace--cancel-scan)
  (quit-window))

(defun projectile-replace--review (literal)
  "Gather matches for a project-wide replacement and pop the results buffer.
LITERAL non-nil runs a literal replace; otherwise the search term is an
Emacs regexp and the replacement may reference capture groups."
  (let* ((root (projectile-acquire-root))
         (term (read-string
                (projectile-prepend-project-name
                 (if literal "Replace: " "Replace regexp: "))
                (projectile-symbol-or-selection-at-point)))
         (replacement (read-string
                       (projectile-prepend-project-name
                        (format "Replace %s with: " term))))
         (regexp (if literal (regexp-quote term) term))
         (case-fold case-fold-search)
         (word projectile-search-whole-word)
         (candidates (projectile-replace--candidates term literal case-fold root)))
    (projectile-replace--open
     #'projectile-replace-mode projectile-replace-buffer-name
     root term regexp replacement literal case-fold candidates
     (projectile-prepend-project-name (format "No matches for %s" term))
     word)))

;;;###autoload
(defun projectile-replace-review ()
  "Review and apply a literal project-wide replacement.

Prompts for a literal search string and a replacement, gathers every
match across the project into a `*projectile-replace*' buffer, and lets
you toggle which matches to apply before committing them.  This is a
non-blocking, previewable alternative to `projectile-replace'."
  (interactive)
  (projectile-replace--review t))

;;;###autoload
(defun projectile-replace-regexp-review ()
  "Review and apply a project-wide regexp replacement.

Like `projectile-replace-review', but the search term is an Emacs
regexp and the replacement may reference capture groups (\\1, \\&).
This is a non-blocking, previewable alternative to
`projectile-replace-regexp'."
  (interactive)
  (projectile-replace--review nil))

;;; Reviewable read-only project-content search
;;
;; A search-only sibling of the reviewable replace UI above.  It reuses the
;; same pure "find matches" machinery (`projectile-replace--candidates',
;; `--gather', `--scan-file', the match struct and its accessors) and the
;; same navigation, filter, case/regexp-toggle, visit and grep-export
;; commands, but renders the matches into a read-only `*projectile-search*'
;; buffer with no before->after preview, no per-match enable/disable toggle,
;; and no apply.  It is a distinct major mode (`projectile-search-mode', a
;; sibling of `projectile-replace-mode', not a shared base) so its keymap can
;; simply omit every write-back key rather than disable it; the shared code
;; lives under the `projectile-replace--' prefix (where it already was) and
;; the two modes share the results-buffer buffer-locals.  A `replace these'
;; bridge hands the current search off to the replace reviewer.

(defvar projectile-search-buffer-name "*projectile-search*"
  "Name of the buffer used by `projectile-search-review'.")

(defun projectile-search--header-string ()
  "Return the propertized status header for the search results buffer.
Shows the term, the match and file counts, the mode flags
\(regexp/literal and case), and a note when the list has been filtered.
Carries no `projectile-replace-match' property, so match navigation
skips it."
  (let* ((matches projectile-replace--matches)
         (nmatches (length matches))
         (seen (make-hash-table :test 'equal))
         nfiles flags)
    (dolist (m matches)
      (puthash (projectile-replace--match-file m) t seen))
    (setq nfiles (hash-table-count seen)
          flags (concat
                 (if projectile-replace--literal "[literal]" "[regexp]")
                 " "
                 (if projectile-replace--case-fold
                     "[ignore-case]" "[case-sensitive]")
                 (if projectile-replace--word " [word]" "")
                 (if projectile-replace--filtered "  filtered" "")
                 (projectile-replace--scanning-note nmatches)))
    (propertize
     (format "Search %S\n%d match%s in %d file%s  %s\n"
             projectile-replace--term
             nmatches (if (= nmatches 1) "" "es")
             nfiles (if (= nfiles 1) "" "s")
             flags)
     'face 'projectile-replace-header)))

(defun projectile-search--render-line (m)
  "Return the propertized results line for match M.
The line is `LINE:COL: CONTEXT' with the matched span highlighted; there
is no replacement preview and no enable/disable indicator.  The whole
line carries the match struct under the `projectile-replace-match' text
property so the shared navigation, visit and filter commands find it."
  (let* ((col (projectile-replace--match-column m))
         (ctx (projectile-replace--match-context m))
         (mstr (projectile-replace--match-string m))
         (before (substring ctx 0 (min col (length ctx))))
         (after (if (<= (+ col (length mstr)) (length ctx))
                    (substring ctx (+ col (length mstr)))
                  ""))
         (highlight (propertize mstr 'face 'projectile-replace-match))
         (locator (propertize (format "%d:%d: "
                                      (projectile-replace--match-line m)
                                      (1+ col))
                              'face 'projectile-replace-line-number))
         (line (concat locator before highlight after "\n")))
    (propertize line 'projectile-replace-match m)))

(defun projectile-search--render ()
  "Redraw the current search results buffer from its buffer-local state."
  (let ((inhibit-read-only t)
        (root projectile-replace--root)
        (matches projectile-replace--matches)
        (counts (make-hash-table :test 'equal))
        (prev-file nil))
    (dolist (m matches)
      (cl-incf (gethash (projectile-replace--match-file m) counts 0)))
    (erase-buffer)
    (insert (projectile-search--header-string))
    (insert (substitute-command-keys
             (concat "\\<projectile-search-mode-map>"
                     "\\[projectile-replace--visit] visit  "
                     "\\[projectile-replace--refresh] re-search  "
                     "\\[projectile-search--to-replace] replace these  "
                     "\\[projectile-replace--export] export  "
                     "\\[projectile-replace--quit] quit\n"
                     "\\[projectile-replace--toggle-case] case  "
                     "\\[projectile-replace--toggle-regexp] regexp/literal  "
                     "\\[projectile-replace--toggle-word] word  "
                     "\\[projectile-replace--keep-matches]/\\[projectile-replace--flush-matches] keep/flush line  "
                     "\\[projectile-replace--keep-files]/\\[projectile-replace--flush-files] keep/flush file\n\n")))
    (if (null matches)
        (insert "No matches.\n")
      (dolist (m matches)
        (let ((file (projectile-replace--match-file m)))
          (unless (equal prev-file file)
            (when prev-file (insert "\n"))
            (setq prev-file file)
            (insert (projectile-replace--file-header
                     file root (gethash file counts))))
          (insert (projectile-search--render-line m)))))
    (when projectile-replace--truncated
      (insert (format "\n(showing the first %d matches)\n"
                      projectile-replace-max-matches)))
    (goto-char (point-min))))

(defun projectile-search--to-replace ()
  "Hand the current search to the reviewable replace UI.
Carries over the same term, literal-ness and case setting and prompts
only for the replacement.  The project is re-scanned from scratch (so
any filtering is undone and every match comes back enabled), mirroring
`projectile-replace-review'."
  (interactive)
  (let* ((root projectile-replace--root)
         (term projectile-replace--term)
         (literal projectile-replace--literal)
         (case-fold projectile-replace--case-fold)
         (word projectile-replace--word)
         (regexp projectile-replace--search)
         (replacement (read-string
                       (projectile-prepend-project-name
                        (format "Replace %s with: " term))))
         (candidates (projectile-replace--candidates term literal case-fold root)))
    (projectile-replace--open
     #'projectile-replace-mode projectile-replace-buffer-name
     root term regexp replacement literal case-fold candidates
     (projectile-prepend-project-name (format "No matches for %s" term))
     word)))

(defvar projectile-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'projectile-replace--visit)
    (define-key map (kbd "n") #'projectile-replace--goto-next-match)
    (define-key map (kbd "p") #'projectile-replace--goto-prev-match)
    (define-key map (kbd "M-n") #'projectile-replace--goto-next-file)
    (define-key map (kbd "M-p") #'projectile-replace--goto-prev-file)
    (define-key map (kbd "c") #'projectile-replace--toggle-case)
    (define-key map (kbd "x") #'projectile-replace--toggle-regexp)
    (define-key map (kbd "w") #'projectile-replace--toggle-word)
    (define-key map (kbd "k") #'projectile-replace--keep-matches)
    (define-key map (kbd "d") #'projectile-replace--flush-matches)
    (define-key map (kbd "K") #'projectile-replace--keep-files)
    (define-key map (kbd "D") #'projectile-replace--flush-files)
    (define-key map (kbd "e") #'projectile-replace--export)
    (define-key map (kbd "r") #'projectile-search--to-replace)
    (define-key map (kbd "g") #'projectile-replace--refresh)
    (define-key map (kbd "q") #'projectile-replace--quit)
    map)
  "Keymap for `projectile-search-mode'.")

(define-derived-mode projectile-search-mode special-mode "Projectile-Search"
  "Major mode for reviewing project-wide search matches, read-only.

A search-only sibling of `projectile-replace-mode': the buffer is a
read-only listing of every match, grouped by file, with no replacement
preview and no way to edit the files from here.  Navigate with
\\<projectile-search-mode-map>\\[projectile-replace--goto-next-match] / \\[projectile-replace--goto-prev-match] (match) and \\[projectile-replace--goto-next-file] / \\[projectile-replace--goto-prev-file] (file), and \\[projectile-replace--visit]
visits the match under point.  The search can be reshaped in place:
\\[projectile-replace--toggle-case] toggles case sensitivity,
\\[projectile-replace--toggle-regexp] toggles literal/regexp matching and
\\[projectile-replace--toggle-word] toggles whole-word matching,
each re-scanning; \\[projectile-replace--keep-matches] / \\[projectile-replace--flush-matches] keep or flush matches by line and
\\[projectile-replace--keep-files] / \\[projectile-replace--flush-files] by file; \\[projectile-replace--refresh] re-runs the search, undoing any filtering.

\\[projectile-search--to-replace] hands the current search off to the reviewable replace UI
\(prompting only for the replacement), and \\[projectile-replace--export] exports the shown
matches to a `grep-mode' buffer for wgrep or Emacs 31's `grep-edit-mode'.

\\{projectile-search-mode-map}"
  (setq-local truncate-lines t)
  (setq-local projectile-replace--render-function #'projectile-search--render)
  ;; killing the buffer mid-scan must not leave a dangling chunk timer
  (add-hook 'kill-buffer-hook #'projectile-replace--cancel-scan nil t)
  (buffer-disable-undo))

(defun projectile-search--review (literal)
  "Gather matches for a project-wide search and pop the read-only results buffer.
LITERAL non-nil searches for a literal string; otherwise the term is an
Emacs regexp.  There is no replacement prompt."
  (let* ((root (projectile-acquire-root))
         (term (projectile--read-search-string-with-default
                (format "Search %s%s for"
                        (projectile--search-tool-tag
                         (if (and literal (projectile-search--rg-fastpath-p t))
                             "ripgrep" "elisp"))
                        (if literal "" " regexp"))))
         (regexp (if literal (regexp-quote term) term))
         (case-fold case-fold-search)
         (word projectile-search-whole-word)
         (candidates (projectile-replace--candidates term literal case-fold root)))
    ;; SEAM: everything above computes the candidate file list; the shared
    ;; opener below seeds the buffer and scans -- synchronously in batch,
    ;; asynchronously (streaming) when interactive -- then renders whatever
    ;; matches come back.
    (projectile-replace--open
     #'projectile-search-mode projectile-search-buffer-name
     root term regexp nil literal case-fold candidates
     (projectile-prepend-project-name (format "No matches for %s" term))
     word)))

;;;###autoload
(defun projectile-search-review ()
  "Search the project for a literal string and review the matches read-only.

Prompts for a literal search string (defaulting to the symbol or region
at point), gathers every match across the project into a read-only
`*projectile-search*' buffer grouped by file, and lets you navigate,
filter and reshape the search.  Use \\<projectile-search-mode-map>\\[projectile-search--to-replace] to turn it into a
reviewable replacement.  This is the read-only sibling of
`projectile-replace-review'."
  (interactive)
  (projectile-search--review t))

;;;###autoload
(defun projectile-search-regexp-review ()
  "Search the project for an Emacs regexp and review the matches read-only.

Like `projectile-search-review', but the search term is an Emacs regexp,
so full Emacs regexp syntax (e.g. symbol boundaries like `\\_<foo\\_>')
is honored."
  (interactive)
  (projectile-search--review nil))

(defun projectile--buffer-matches-conditions (buffer conditions)
  "Return non-nil if BUFFER satisfies any condition in CONDITIONS.

CONDITIONS is a list using the DSL documented in
`projectile-kill-buffers-filter'.  Modeled on project.el's
`project--buffer-check'."
  (catch 'match
    (dolist (c conditions)
      (when (cond
             ((stringp c)
              (string-match-p c (buffer-name buffer)))
             ((functionp c)
              (funcall c buffer))
             ((eq (car-safe c) 'major-mode)
              (eq (buffer-local-value 'major-mode buffer) (cdr c)))
             ((eq (car-safe c) 'derived-mode)
              (provided-mode-derived-p
               (buffer-local-value 'major-mode buffer) (cdr c)))
             ((eq (car-safe c) 'not)
              (not (projectile--buffer-matches-conditions buffer (cdr c))))
             ((eq (car-safe c) 'or)
              (projectile--buffer-matches-conditions buffer (cdr c)))
             ((eq (car-safe c) 'and)
              (seq-every-p
               (apply-partially #'projectile--buffer-matches-conditions buffer)
               (mapcar #'list (cdr c)))))
        (throw 'match t)))))

(defun projectile-buffer-killed-p (buffer)
  "Return non-nil if BUFFER should be killed by `projectile-kill-buffers'.
The decision follows `projectile-kill-buffers-filter'."
  (cond
   ((functionp projectile-kill-buffers-filter)
    (funcall projectile-kill-buffers-filter buffer))
   ((eq projectile-kill-buffers-filter 'kill-all) t)
   ((eq projectile-kill-buffers-filter 'kill-only-files)
    (buffer-file-name buffer))
   ((listp projectile-kill-buffers-filter)
    (projectile--buffer-matches-conditions buffer projectile-kill-buffers-filter))
   (t (user-error "Invalid projectile-kill-buffers-filter value: %S"
                  projectile-kill-buffers-filter))))

;;;###autoload
(defun projectile-kill-buffers ()
  "Kill project buffers.

The buffers are killed according to the value of
`projectile-kill-buffers-filter'."
  (interactive)
  (let* ((project (projectile-acquire-root))
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
               (projectile-buffer-killed-p buffer))
          (kill-buffer buffer))))))

;;;###autoload
(defun projectile-save-project-buffers ()
  "Save all project buffers."
  (interactive)
  (let* ((project (projectile-acquire-root))
         (project-name (projectile-project-name project))
         (modified-buffers (seq-filter (lambda (buf)
                                               (and (buffer-file-name buf)
                                                    (buffer-modified-p buf)))
                                             (projectile-project-buffers project))))
    (if (null modified-buffers)
        (message "[%s] No buffers need saving" project-name)
      (dolist (buf modified-buffers)
        (with-current-buffer buf
          (save-buffer)))
      (message "[%s] Saved %d buffers" project-name (length modified-buffers)))))

(defun projectile--dired (dired-fn &optional arg)
  "Open the project root in dired using DIRED-FN.
DIRED-FN is a `dired'-like command; passing `dired-other-window' or
`dired-other-frame' yields the other-window/-frame variants.  With ARG,
prompt for a known project to open instead of the current one."
  (funcall dired-fn
           (if arg
               (projectile-completing-read
                "Dired in project: " (projectile-relevant-known-projects)
                :category 'file
                :caller 'projectile-read-project)
             (projectile-acquire-root))))

;;;###autoload
(defun projectile-dired (&optional arg)
  "Open `dired' at the root of the project.
With a prefix argument ARG, prompt for a known project to open in dired."
  (interactive "P")
  (projectile--dired #'dired arg))

;;;###autoload (autoload 'projectile-dired-other-window "projectile" nil t)
;;;###autoload (autoload 'projectile-dired-other-frame "projectile" nil t)
(projectile--define-display-variants projectile-dired (&optional arg)
  "Open `dired' at the root of the project in another %s.
With a prefix argument ARG, prompt for a known project to open in dired."
  (projectile--dired #'dired-other-window arg))

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
                      projectile-known-projects
                      :category 'file
                      :caller 'projectile-read-project))))
  (unless project-root
    (setq project-root (projectile-acquire-root)))
  (let ((vcs (projectile-project-vcs project-root)))
    (pcase vcs
      ('git
       (cond ((fboundp 'magit-status-internal)
              (magit-status-internal project-root))
             ((fboundp 'magit-status)
              (with-no-warnings (magit-status project-root)))
             (t
              (vc-dir project-root))))
      ('hg
       (if (fboundp 'monky-status)
           (monky-status project-root)
         (vc-dir project-root)))
      (_ (vc-dir project-root)))))

;;;###autoload
(defun projectile-recentf ()
  "Show a list of recently visited files in a project."
  (interactive)
  (if (boundp 'recentf-list)
      (find-file (projectile-expand-root
                  (projectile-completing-read
                   "Recently visited files: "
                   (projectile-recentf-files)
                   :caller 'projectile-read-file)))
    (message "recentf is not enabled")))

(defun projectile-recentf-files ()
  "Return a list of recently visited files in a project."
  (and (boundp 'recentf-list)
       (let ((project-root (file-truename (projectile-acquire-root))))
         (mapcar
          (lambda (f) (file-relative-name f project-root))
          (seq-filter
           (lambda (f) (string-prefix-p project-root (expand-file-name f)))
           recentf-list)))))

(defun projectile-project-cache-file (&optional project-root)
  "The path to a project's cache file for PROJECT-ROOT.
Acts on the current project if not specified explicitly."
  (if project-root
      (expand-file-name projectile-cache-file project-root)
    (projectile-expand-root projectile-cache-file)))

(defvar projectile-configure-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last configure command used on them.")

(defvar projectile-compilation-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last compilation command used on them.")

(defvar projectile-install-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last install command used on them.")

(defvar projectile-package-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last package command used on them.")

(defvar projectile-test-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last test command used on them.")

(defvar projectile-run-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last run command used on them.")

(defconst projectile--lifecycle-phases
  '((:name configure :prompt "Configure command: " :save-buffers t
     :cmd-map projectile-configure-cmd-map
     :dir-local-var projectile-project-configure-cmd
     :default-fn projectile--expand-configure-command
     :command-fn projectile-configure-command
     :use-comint-var projectile-configure-use-comint-mode)
    (:name compile :prompt "Compile command: " :save-buffers t
     :cmd-map projectile-compilation-cmd-map
     :dir-local-var projectile-project-compilation-cmd
     :default-fn projectile-default-compilation-command
     :command-fn projectile-compilation-command
     :use-comint-var projectile-compile-use-comint-mode)
    (:name test :prompt "Test command: " :save-buffers t
     :cmd-map projectile-test-cmd-map
     :dir-local-var projectile-project-test-cmd
     :default-fn projectile-default-test-command
     :command-fn projectile-test-command
     :use-comint-var projectile-test-use-comint-mode)
    (:name install :prompt "Install command: " :save-buffers t
     :cmd-map projectile-install-cmd-map
     :dir-local-var projectile-project-install-cmd
     :default-fn projectile-default-install-command
     :command-fn projectile-install-command
     :use-comint-var projectile-install-use-comint-mode)
    (:name package :prompt "Package command: " :save-buffers t
     :cmd-map projectile-package-cmd-map
     :dir-local-var projectile-project-package-cmd
     :default-fn projectile-default-package-command
     :command-fn projectile-package-command
     :use-comint-var projectile-package-use-comint-mode)
    (:name run :prompt "Run command: " :save-buffers nil
     :cmd-map projectile-run-cmd-map
     :dir-local-var projectile-project-run-cmd
     :default-fn projectile-default-run-command
     :command-fn projectile-run-command
     :use-comint-var projectile-run-use-comint-mode))
  "Descriptors for the project lifecycle phases.

Each entry is a plist with the phase symbol (`:name', also used as the
command type for the per-type command history), the variable caching
the last command per project (`:cmd-map'), the .dir-locals.el override
variable (`:dir-local-var'), a function of the project type returning
the default command (`:default-fn'), the public command resolver
\(`:command-fn'), the option making the output buffer interactive
\(`:use-comint-var'), the prompt prefix (`:prompt') and whether to save
the project's buffers before running the command (`:save-buffers').")

(defun projectile--phase-descriptor (phase)
  "Return the lifecycle descriptor for PHASE (a symbol like `compile')."
  (or (seq-find (lambda (descriptor) (eq (plist-get descriptor :name) phase))
                projectile--lifecycle-phases)
      (error "Unknown project lifecycle phase `%s'" phase)))

;;;###autoload
(defun projectile-discard-command-cache ()
  "Discard the cached lifecycle commands for the current project.

Projectile caches the last command used for each of the configure,
compile, test, install, package, and run actions and prefers it over the
value from `.dir-locals.el' or the project type's default.  After
editing those, run this command so the next invocation re-reads them.
Handy on `after-save-hook' for `.dir-locals.el' buffers.

This only clears the cached commands, not the command history offered at
the prompt.  See also `projectile-discard-root-cache'."
  (interactive)
  (let ((root (projectile-acquire-root)))
    (dolist (descriptor projectile--lifecycle-phases)
      (let ((command-map (symbol-value (plist-get descriptor :cmd-map))))
        (dolist (dir (hash-table-keys command-map))
          (when (string-prefix-p root dir)
            (remhash dir command-map)))))
    ;; Give feedback when invoked interactively; stay quiet when used
    ;; programmatically (e.g. from `after-save-hook') unless verbose.
    (when (or projectile-verbose (called-interactively-p 'interactive))
      (message "Discarded Projectile command cache for %s" root))))

(defvar projectile-project-enable-cmd-caching t
  "Enables command caching for the project.  Set to nil to disable.
Should be set via .dir-locals.el.")
(put 'projectile-project-enable-cmd-caching 'safe-local-variable #'booleanp)

(defun projectile--cache-project-commands-p ()
  "Whether to cache project commands.
The variable `projectile-project-enable-cmd-caching' is typically
set via .dir-locals.el, which applies it buffer-locally in file
buffers."
  projectile-project-enable-cmd-caching)

(defvar projectile-project-configure-cmd nil
  "The command to use with `projectile-configure-project'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el.")
(put 'projectile-project-configure-cmd 'safe-local-variable #'stringp)

(defvar projectile-project-compilation-cmd nil
  "The command to use with `projectile-compile-project'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el.")
(put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)

(defvar projectile-project-compilation-dir nil
  "The directory to use with `projectile-compile-project'.
The directory path is relative to the project root.
Should be set via .dir-locals.el.")
(put 'projectile-project-compilation-dir 'safe-local-variable #'stringp)

(defvar projectile-project-test-cmd nil
  "The command to use with `projectile-test-project'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el.")
(put 'projectile-project-test-cmd 'safe-local-variable #'stringp)

(defvar projectile-project-install-cmd nil
  "The command to use with `projectile-install-project'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el.")
(put 'projectile-project-install-cmd 'safe-local-variable #'stringp)

(defvar projectile-project-package-cmd nil
  "The command to use with `projectile-package-project'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el.")
(put 'projectile-project-package-cmd 'safe-local-variable #'stringp)

(defvar projectile-project-run-cmd nil
  "The command to use with `projectile-run-project'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el.")
(put 'projectile-project-run-cmd 'safe-local-variable #'stringp)

(defun projectile-tasks-safe-p (value)
  "Return non-nil if VALUE is a safe directory-local `projectile-tasks'.
Only an alist mapping task-name strings to command strings is
considered safe.  Function commands are rejected: a .dir-locals.el
travels with the project, so a function value could execute arbitrary
code the moment a task is run."
  (let ((safe t))
    (while (and safe (consp value))
      (let ((entry (car value)))
        (setq safe (and (consp entry)
                        (stringp (car entry))
                        (stringp (cdr entry)))))
      (setq value (cdr value)))
    (and safe (null value))))

(defcustom projectile-tasks nil
  "An alist of named tasks that can be run with `projectile-run-task'.

Each entry has the form (TASK-NAME . COMMAND), where TASK-NAME is a
string and COMMAND is either a shell command string or a function
called with no arguments (and `default-directory' set to the project
root) that returns such a string.

Command strings support the same `%p' placeholder as the lifecycle
commands; it's replaced with the project name at execution time.

The variable can be set globally, per project type (see the `:tasks'
keyword of `projectile-register-project-type'), or per project via
.dir-locals.el, which makes the tasks shareable through your VCS.
Entries here override same-named tasks defined by the project type
\(see `projectile-project-tasks').

Note that only string commands are safe as a directory-local value;
tasks with function commands have to be defined globally or via a
project type."
  :group 'projectile
  :type '(alist :key-type (string :tag "Task name")
                :value-type (choice (string :tag "Shell command")
                                    (function :tag "Function returning a shell command")))
  :safe #'projectile-tasks-safe-p
  :package-version '(projectile . "3.1.0"))

(defun projectile-project-tasks (&optional project-type)
  "Return the effective tasks alist for the current project.

That's the PROJECT-TYPE's `:tasks' table (PROJECT-TYPE defaults to the
current project's type) merged with `projectile-tasks', whose entries -
set globally or per project via .dir-locals.el - override same-named
project-type tasks."
  (let ((type-tasks (projectile-project-type-attribute
                     (or project-type (projectile-project-type)) 'tasks)))
    (append projectile-tasks
            (seq-remove (lambda (task) (assoc (car task) projectile-tasks))
                        type-tasks))))

(defun projectile-default-generic-command (project-type command-type)
  "Generic retrieval of COMMAND-TYPEs default cmd-value for PROJECT-TYPE.

If found, checks if value is symbol or string.  In case of symbol
resolves to function `funcall's.  Return value of function MUST
be string to be executed as command."
  (let ((command (plist-get (alist-get project-type projectile-project-types) command-type)))
    (cond
     ((not command) nil)
     ((stringp command) command)
     ((functionp command)
      (funcall command))
     (t
      (error "The value for: %s in project-type: %s was neither a function nor a string" command-type project-type)))))

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

(defun projectile-default-install-command (project-type)
  "Retrieve default install command for PROJECT-TYPE."
  (projectile-default-generic-command project-type 'install-command))

(defun projectile-default-package-command (project-type)
  "Retrieve default package command for PROJECT-TYPE."
  (projectile-default-generic-command project-type 'package-command))

(defun projectile-default-run-command (project-type)
  "Retrieve default run command for PROJECT-TYPE."
  (projectile-default-generic-command project-type 'run-command))

(defun projectile--expand-configure-command (project-type)
  "Default configure command for PROJECT-TYPE with the project root filled in.
The command may contain a `%s' placeholder which is replaced with
the project root."
  (when-let* ((cmd-format-string (projectile-default-configure-command project-type)))
    (format cmd-format-string (projectile-project-root))))

(defun projectile--phase-command (phase compile-dir)
  "Resolve the command to run for lifecycle PHASE in COMPILE-DIR.
Checks the phase's command cache first, then its .dir-locals.el
override variable and finally the default command for the current
project type."
  (let ((descriptor (projectile--phase-descriptor phase)))
    (or (gethash compile-dir (symbol-value (plist-get descriptor :cmd-map)))
        (symbol-value (plist-get descriptor :dir-local-var))
        (funcall (plist-get descriptor :default-fn) (projectile-project-type)))))

(defun projectile-configure-command (compile-dir)
  "Retrieve the configure command for COMPILE-DIR.
Checks `projectile-configure-cmd-map' for the last configure command
that was invoked on the project, then `projectile-project-configure-cmd'
supplied via .dir-locals.el and finally the default configure command
for a project of that type."
  (projectile--phase-command 'configure compile-dir))

(defun projectile-compilation-buffer-name (compilation-mode)
  "Meant to be used for `compilation-buffer-name-function`.
Argument COMPILATION-MODE is the name of the major mode used for the
compilation buffer."
  (concat "*" (downcase compilation-mode) "*"
          (if (projectile-project-p) (concat "<" (projectile-project-name) ">") "")))

(defun projectile-current-project-buffer-p ()
  "Meant to be used for `compilation-save-buffers-predicate`.
This indicates whether the current buffer is in the same project as the current
window (including returning true if neither is in a project)."
  (let ((root (with-current-buffer (window-buffer) (projectile-project-root))))
    (or (not root)
        (projectile-project-buffer-p (current-buffer) root))))

(defun projectile-compilation-command (compile-dir)
  "Retrieve the compilation command for COMPILE-DIR.
Checks `projectile-compilation-cmd-map' for the last compile command
that was invoked on the project, then `projectile-project-compilation-cmd'
supplied via .dir-locals.el and finally the default compilation command
for a project of that type."
  (projectile--phase-command 'compile compile-dir))

(defun projectile-test-command (compile-dir)
  "Retrieve the test command for COMPILE-DIR.
Checks `projectile-test-cmd-map' for the last test command that was
invoked on the project, then `projectile-project-test-cmd' supplied
via .dir-locals.el and finally the default test command for a project
of that type."
  (projectile--phase-command 'test compile-dir))

(defun projectile-install-command (compile-dir)
  "Retrieve the install command for COMPILE-DIR.
Checks `projectile-install-cmd-map' for the last install command that
was invoked on the project, then `projectile-project-install-cmd'
supplied via .dir-locals.el and finally the default install command
for a project of that type."
  (projectile--phase-command 'install compile-dir))

(defun projectile-package-command (compile-dir)
  "Retrieve the package command for COMPILE-DIR.
Checks `projectile-package-cmd-map' for the last package command that
was invoked on the project, then `projectile-project-package-cmd'
supplied via .dir-locals.el and finally the default package command
for a project of that type."
  (projectile--phase-command 'package compile-dir))

(defun projectile-run-command (compile-dir)
  "Retrieve the run command for COMPILE-DIR.
Checks `projectile-run-cmd-map' for the last run command that was
invoked on the project, then `projectile-project-run-cmd' supplied
via .dir-locals.el and finally the default run command for a project
of that type."
  (projectile--phase-command 'run compile-dir))

(defun projectile-read-command (prompt command &optional command-type)
  "Adapted from the function `compilation-read-command'.

COMMAND-TYPE, when non-nil, selects the per-type command history
\(see `projectile--get-command-history') as the minibuffer history."
  (let ((compile-history
         ;; fetch the command history for the current project
         (ring-elements (projectile--get-command-history (projectile-acquire-root)
                                                         command-type))))
    (read-shell-command prompt command
                        (if (equal (car compile-history) command)
                            '(compile-history . 1)
                          'compile-history))))

(defun projectile-subproject-root ()
  "Find the root of the nearest subproject containing the current file.
Walk up from `default-directory' looking for the project type's
`:project-file' marker, stopping at the project root.  Returns the
directory containing the nearest marker, or signals an error if no
subproject is found between the current directory and the project root."
  (let* ((project-root (projectile-acquire-root))
         (type (projectile-project-type project-root))
         (project-file (projectile-project-type-attribute type 'project-file))
         (markers (if (listp project-file) project-file (list project-file)))
         (dir (file-name-directory (or (buffer-file-name) default-directory)))
         (result nil))
    (unless markers
      (user-error "Project type `%s' has no project-file defined" type))
    ;; Walk up from current directory, stop at (but include) project root.
    (while (and (not result)
                dir
                (string-prefix-p project-root dir))
      (when (seq-some (lambda (m)
                        (file-exists-p (expand-file-name m dir)))
                      markers)
        (setq result dir))
      (let ((parent (file-name-directory (directory-file-name dir))))
        (setq dir (unless (string= parent dir) parent))))
    (or result
        (user-error "No subproject found between current directory and project root"))))

(defun projectile-compilation-dir ()
  "Retrieve the compilation directory for this project."
  (let* ((project-root (projectile-acquire-root))
         (type (projectile-project-type project-root))
         (comp-dir (or projectile-project-compilation-dir
                        (projectile-default-compilation-dir type))))
    (if comp-dir
        (expand-file-name (file-name-as-directory comp-dir) project-root)
      project-root)))

(defun projectile-maybe-read-command (arg default-cmd prompt &optional command-type)
  "Prompt user for command unless DEFAULT-CMD is an Elisp function.
COMMAND-TYPE is forwarded to `projectile-read-command' to pick the
per-type command history."
  (if (and (or (stringp default-cmd) (null default-cmd))
           (or compilation-read-command arg))
      (projectile-read-command prompt default-cmd command-type)
    default-cmd))

(defun projectile-run-compilation (cmd &optional use-comint-mode)
  "Run external or Elisp compilation command CMD."
  (if (functionp cmd)
      (funcall cmd)
    (compile cmd use-comint-mode)))

(defvar projectile-project-command-history (make-hash-table :test 'equal)
  "The history of last executed project commands, per project.

Projects are indexed by their project-root value.")

(defun projectile--get-command-history (project-root &optional command-type)
  "Return the command history ring for PROJECT-ROOT.

With COMMAND-TYPE non-nil (one of the lifecycle command type
symbols, e.g. `compile' or `test', or a (task . TASK-NAME) cons
for named tasks) return the history specific to that command
type, so histories of different types don't bleed into each
other's prompts.  With COMMAND-TYPE nil return the combined
per-project history, which is what `projectile-repeat-last-command'
reads."
  (let ((key (if command-type (cons project-root command-type) project-root)))
    (or (gethash key projectile-project-command-history)
        (puthash key
                 (make-ring 16)
                 projectile-project-command-history))))

(defun projectile--command-history-insert (history command)
  "Insert COMMAND into the ring HISTORY.
Duplicates are handled according to `projectile-cmd-hist-ignoredups'."
  (cond
   ((eq projectile-cmd-hist-ignoredups t)
    (unless (string= (car-safe (ring-elements history)) command)
      (ring-insert history command)))
   ((eq projectile-cmd-hist-ignoredups 'erase)
    (let ((idx (ring-member history command)))
      (while idx
        (ring-remove history idx)
        (setq idx (ring-member history command))))
    (ring-insert history command))
   (t (ring-insert history command))))

(cl-defun projectile--run-project-cmd
    (command command-map &key command-type show-prompt prompt-prefix save-buffers use-comint-mode buffer-name-function no-cache)
  "Run a project COMMAND, typically a test- or compile command.

Cache the COMMAND for later use inside the hash-table COMMAND-MAP.
With NO-CACHE non-nil the command is not stored in COMMAND-MAP (though
its result still enters the command history); this is for
function-derived commands that must be re-resolved on every run.

COMMAND-TYPE, when non-nil, is the lifecycle command type symbol
\(e.g. `compile' or `test') and is used to keep a per-type command
history for the prompt, in addition to the combined per-project one.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
by setting SHOW-PROMPT.  The prompt will be prefixed with PROMPT-PREFIX.

If SAVE-BUFFERS is non-nil save all projectile buffers before
running the command.

BUFFER-NAME-FUNCTION, when non-nil, is used as the
`compilation-buffer-name-function' for the compilation, taking
precedence over the `projectile-per-project-compilation-buffer'
naming.

The placeholder `%p' in COMMAND is replaced with the project name.

The command actually run is returned."
  (let* ((project-root (projectile-acquire-root))
         (default-directory (projectile-compilation-dir))
         (command (projectile-maybe-read-command show-prompt
                                                 command
                                                 prompt-prefix
                                                 command-type))
         (compilation-buffer-name-function compilation-buffer-name-function)
         (compilation-save-buffers-predicate compilation-save-buffers-predicate))
    (when command-map
      ;; A function-derived command (NO-CACHE) is re-resolved on every run so
      ;; it can prompt again (e.g. a CMake preset picker); don't freeze its
      ;; result in the cache, only feed it to the history below.
      (unless no-cache
        (puthash default-directory command command-map))
      ;; Record into the combined per-project history (read by
      ;; `projectile-repeat-last-command') and, when known, into the
      ;; per-type history used for this command's prompt.
      (projectile--command-history-insert
       (projectile--get-command-history project-root) command)
      (when command-type
        (projectile--command-history-insert
         (projectile--get-command-history project-root command-type) command)))
    (when save-buffers
      (save-some-buffers (not compilation-ask-about-save)
                         (lambda ()
                           (projectile-project-buffer-p (current-buffer)
                                                        project-root))))
    (when projectile-per-project-compilation-buffer
      (setq compilation-buffer-name-function #'projectile-compilation-buffer-name)
      (setq compilation-save-buffers-predicate #'projectile-current-project-buffer-p))
    (when buffer-name-function
      (setq compilation-buffer-name-function buffer-name-function))
    (unless command
      (user-error "No %scommand configured for project type `%s'"
                  (or prompt-prefix "") (projectile-project-type)))
    (unless (file-directory-p default-directory)
      (mkdir default-directory))
    ;; Substitute placeholders: %p -> project name
    (when (string-match-p "%p" command)
      (setq command (string-replace "%p" (projectile-project-name project-root) command)))
    (projectile-run-compilation command use-comint-mode)
    command))

(defcustom projectile-configure-use-comint-mode nil
  "Make the output buffer of `projectile-configure-project' interactive."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.5.0"))

(defcustom projectile-compile-use-comint-mode nil
  "Make the output buffer of `projectile-compile-project' interactive."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.5.0"))

(defcustom projectile-test-use-comint-mode nil
  "Make the output buffer of `projectile-test-project' interactive."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.5.0"))

(defcustom projectile-install-use-comint-mode nil
  "Make the output buffer of `projectile-install-project' interactive."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.5.0"))

(defcustom projectile-package-use-comint-mode nil
  "Make the output buffer of `projectile-package-project' interactive."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.5.0"))

(defcustom projectile-run-use-comint-mode nil
  "Make the output buffer of `projectile-run-project' interactive."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.5.0"))

(defun projectile--phase-command-dynamic-p (phase)
  "Non-nil when PHASE's command comes from a function for the current project.
A project type can register a lifecycle command as a function (e.g. the
CMake preset pickers, or a user's own `:test'/`:run' function).  Such a
command is meant to be re-invoked - and may prompt - on every run, so its
result must not be frozen in the command cache after the first run.  A
`.dir-locals.el' override always wins and is a plain string, so it is
never treated as dynamic."
  (let ((descriptor (projectile--phase-descriptor phase)))
    (and (not (symbol-value (plist-get descriptor :dir-local-var)))
         (functionp
          (plist-get (alist-get (projectile-project-type) projectile-project-types)
                     (intern (format "%s-command" phase)))))))

(defun projectile--run-lifecycle-phase (phase show-prompt)
  "Run the current project's command for lifecycle PHASE.
PHASE is a symbol naming an entry of `projectile--lifecycle-phases'.
With SHOW-PROMPT non-nil force prompting for the command, as in
`projectile--run-project-cmd'."
  (let* ((descriptor (projectile--phase-descriptor phase))
         (command (funcall (plist-get descriptor :command-fn)
                           (projectile-compilation-dir)))
         (command-map (if (projectile--cache-project-commands-p)
                          (symbol-value (plist-get descriptor :cmd-map)))))
    (projectile--run-project-cmd command command-map
                                 :command-type phase
                                 :show-prompt show-prompt
                                 :prompt-prefix (plist-get descriptor :prompt)
                                 :save-buffers (plist-get descriptor :save-buffers)
                                 :no-cache (projectile--phase-command-dynamic-p phase)
                                 :use-comint-mode (symbol-value (plist-get descriptor :use-comint-var)))))

;;;###autoload
(defun projectile-configure-project (arg)
  "Run project configure command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (projectile--run-lifecycle-phase 'configure arg))

;;;###autoload
(defun projectile-compile-project (arg)
  "Run project compilation command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.  Per project default command can be set through
`projectile-project-compilation-cmd'."
  (interactive "P")
  (projectile--run-lifecycle-phase 'compile arg))

;;;###autoload
(defun projectile-test-project (arg)
  "Run project test command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (projectile--run-lifecycle-phase 'test arg))

;;;###autoload
(defun projectile-compile-subproject (arg)
  "Run compilation in the nearest subproject.
Find the closest build file (e.g. pom.xml, build.gradle) between the
current directory and the project root, then run the project's compile
command there.  This is useful for multi-module projects where building
a single module is faster than building the entire project.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let* ((subproject-root (projectile-subproject-root))
         (project-root (projectile-acquire-root))
         (projectile-project-compilation-dir
          (file-relative-name subproject-root project-root))
         (command (projectile-compilation-command (projectile-compilation-dir)))
         (command-map (if (projectile--cache-project-commands-p) projectile-compilation-cmd-map)))
    (projectile--run-project-cmd command command-map
                                 :show-prompt arg
                                 :prompt-prefix "Compile subproject command: "
                                 :save-buffers t
                                 :use-comint-mode projectile-compile-use-comint-mode)))

;;;###autoload
(defun projectile-test-subproject (arg)
  "Run tests in the nearest subproject.
Find the closest build file (e.g. pom.xml, build.gradle) between the
current directory and the project root, then run the project's test
command there.  This is useful for multi-module projects where testing
a single module is faster than testing the entire project.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let* ((subproject-root (projectile-subproject-root))
         (project-root (projectile-acquire-root))
         (projectile-project-compilation-dir
          (file-relative-name subproject-root project-root))
         (command (projectile-test-command (projectile-compilation-dir)))
         (command-map (if (projectile--cache-project-commands-p) projectile-test-cmd-map)))
    (projectile--run-project-cmd command command-map
                                 :show-prompt arg
                                 :prompt-prefix "Test subproject command: "
                                 :save-buffers t
                                 :use-comint-mode projectile-test-use-comint-mode)))

(defun projectile-test-at-point-python-name (node)
  "Return the test name for the Python `function_definition' NODE.
Return nil unless the function's name starts with \"test_\"."
  (when-let* ((name-node (treesit-node-child-by-field-name node "name"))
              (name (treesit-node-text name-node t)))
    (when (string-prefix-p "test_" name)
      name)))

(defun projectile-test-at-point-python-command (test-name file-name)
  "Return a pytest command running TEST-NAME in FILE-NAME.
TEST-NAME and FILE-NAME are shell-quoted: a test name comes from the
buffer's own source and a file name from the repository, so an
unquoted interpolation would let a hostile project inject shell code."
  (format "python -m pytest %s::%s"
          (shell-quote-argument file-name)
          (shell-quote-argument test-name)))

(defun projectile-test-at-point-go-name (node)
  "Return the test name for the Go `function_declaration' NODE.
Return nil unless the function's name is one `go test' would run:
\"Test\" followed by a non-lowercase character (or nothing)."
  (when-let* ((name-node (treesit-node-child-by-field-name node "name"))
              (name (treesit-node-text name-node t)))
    (let ((case-fold-search nil))
      (when (string-match-p "\\`Test\\([^[:lower:]]\\|\\'\\)" name)
        name))))

(defun projectile-test-at-point-go-command (test-name file-name)
  "Return a `go test' command running TEST-NAME from FILE-NAME's package.
The command targets exactly the package containing FILE-NAME (e.g.
`./pkg/foo' for `pkg/foo/foo_test.go'); the recursive `./pkg/foo/...'
form would also build every nested package and run any same-named
tests they contain."
  (let ((dir (file-name-directory file-name)))
    ;; TEST-NAME and the package directory are shell-quoted to keep a
    ;; hostile repository from injecting shell code.  The `^...$'
    ;; regexp anchors stay inside the quoting so `-run' still gets one
    ;; anchored pattern.
    (format "go test -run %s %s"
            (shell-quote-argument (concat "^" test-name "$"))
            (shell-quote-argument
             (if dir (concat "./" (directory-file-name dir)) ".")))))

(defun projectile-test-at-point-jest-name (node)
  "Return the test name for the JS/TS `call_expression' NODE.
Matches `it'/`test'/`describe' calls (including member calls like
`it.only' or `test.each') whose first argument is a string literal,
returning that string without the surrounding quotes.  Return nil
otherwise."
  (let* ((fn (treesit-node-child-by-field-name node "function"))
         (fn-name (when fn
                    (pcase (treesit-node-type fn)
                      ("identifier" (treesit-node-text fn t))
                      ("member_expression"
                       (when-let* ((object (treesit-node-child-by-field-name
                                            fn "object")))
                         (when (equal (treesit-node-type object) "identifier")
                           (treesit-node-text object t))))))))
    (when (member fn-name '("it" "test" "describe"))
      (when-let* ((args (treesit-node-child-by-field-name node "arguments"))
                  (arg (treesit-node-child args 0 t)))
        (when (member (treesit-node-type arg) '("string" "template_string"))
          ;; Strip the surrounding quotes/backticks.
          (substring (treesit-node-text arg t) 1 -1))))))

(defun projectile-test-at-point-jest-command (test-name file-name)
  "Return a jest command running TEST-NAME in FILE-NAME.
TEST-NAME and FILE-NAME are shell-quoted: the test name is arbitrary
source text (JS strings can contain any character), so an unquoted
interpolation would let a hostile project inject shell code."
  (format "npx jest %s -t %s"
          (shell-quote-argument file-name)
          (shell-quote-argument test-name)))

(defcustom projectile-test-at-point-rules
  (let ((jest-rule '(:node-types ("call_expression")
                     :name-fn projectile-test-at-point-jest-name
                     :command-fn projectile-test-at-point-jest-command)))
    `((python-ts-mode
       :node-types ("function_definition")
       :name-fn projectile-test-at-point-python-name
       :command-fn projectile-test-at-point-python-command)
      (go-ts-mode
       :node-types ("function_declaration")
       :name-fn projectile-test-at-point-go-name
       :command-fn projectile-test-at-point-go-command)
      (js-ts-mode ,@jest-rule)
      (typescript-ts-mode ,@jest-rule)
      (tsx-ts-mode ,@jest-rule)))
  "Rules telling `projectile-run-test-at-point' how to run a single test.

An alist keyed by major mode symbol.  The current buffer's mode is
matched against the keys with `derived-mode-p', so a rule keyed on a
mode also applies to modes derived from it.  Each value is a plist
with the following keys:

`:node-types' - a list of tree-sitter node type strings; walking up
the parse tree from point, only nodes of these types are considered.

`:name-fn' - a function called with a matching tree-sitter node that
returns the test name string, or nil if the node isn't a test (in
which case the walk continues upward).

`:command-fn' - a function called with the test name and the file
name (relative to the directory the command runs in, normally the
project root) that returns the shell command to run."
  :group 'projectile
  :type '(alist :key-type (symbol :tag "Major mode")
                :value-type (plist :tag "Rule"))
  :package-version '(projectile . "3.1.0"))

(defun projectile--test-at-point-rule ()
  "Return the `projectile-test-at-point-rules' rule for the current buffer.
The buffer's major mode is matched against the rule keys with
`derived-mode-p'.  Return nil when no rule matches."
  (cdr (seq-find (lambda (rule) (derived-mode-p (car rule)))
                 projectile-test-at-point-rules)))

(defun projectile--test-at-point-name (rule)
  "Return the name of the test around point according to RULE, or nil.
Walk up the tree-sitter parse tree from the node at point; for every
enclosing node whose type is in RULE's `:node-types', call the rule's
`:name-fn' with the node and return its first non-nil result."
  (let ((node-types (plist-get rule :node-types))
        (name-fn (plist-get rule :name-fn))
        (node (treesit-node-at (point)))
        name)
    (while (and node (null name))
      (when (member (treesit-node-type node) node-types)
        (setq name (funcall name-fn node)))
      (setq node (treesit-node-parent node)))
    name))

;;;###autoload
(defun projectile-run-test-at-point (arg)
  "Run the test around point, if any.

The test is located by walking up the buffer's tree-sitter parse
tree according to the rule for the buffer's major mode in
`projectile-test-at-point-rules', which also determines the command
used to run it.  Requires Emacs 29+ built with tree-sitter support
and a tree-sitter major mode (e.g. `python-ts-mode').

The command runs like `projectile-test-project' does (same working
directory and buffer-saving behavior), but it is not recorded as the
project's test command and doesn't touch the command history.  With
a prefix ARG you can edit the command before it's run."
  (interactive "P")
  (unless (and (fboundp 'treesit-available-p) (treesit-available-p))
    (user-error "This command requires Emacs 29+ built with tree-sitter support"))
  (unless (treesit-parser-list)
    (user-error "No tree-sitter parser in this buffer; use a tree-sitter major mode (e.g. `python-ts-mode')"))
  (unless buffer-file-name
    (user-error "The current buffer is not visiting a file"))
  (let ((rule (projectile--test-at-point-rule)))
    (unless rule
      (user-error "No test-at-point rule for `%s'; see `projectile-test-at-point-rules'"
                  major-mode))
    (let ((test-name (projectile--test-at-point-name rule)))
      (unless test-name
        (user-error "No test found at point"))
      ;; The command runs in the compilation directory, so the file
      ;; name is made relative to it as spelled - not through
      ;; `file-truename', which would escape the project for a file
      ;; under a symlinked subdirectory and yield a useless `../..' path.
      (let ((command (funcall (plist-get rule :command-fn)
                              test-name
                              (file-relative-name
                               buffer-file-name
                               (projectile-compilation-dir))))
            ;; The command was derived from the test at point, so the
            ;; usual `compilation-read-command' prompt doesn't apply;
            ;; prompt only when explicitly asked to with a prefix arg.
            (compilation-read-command nil))
        ;; A nil command-map keeps the project's cached test command and
        ;; command history untouched.
        (projectile--run-project-cmd command nil
                                     :show-prompt arg
                                     :prompt-prefix "Test at point command: "
                                     :save-buffers t
                                     :use-comint-mode projectile-test-use-comint-mode)))))

;;;###autoload
(defun projectile-install-project (arg)
  "Run project install command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (projectile--run-lifecycle-phase 'install arg))

;;;###autoload
(defun projectile-package-project (arg)
  "Run project package command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (projectile--run-lifecycle-phase 'package arg))

;;;###autoload
(defun projectile-run-project (arg)
  "Run project run command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (projectile--run-lifecycle-phase 'run arg))

;;;###autoload
(defun projectile-repeat-last-command (show-prompt)
  "Run last projectile external command.

External commands are: `projectile-configure-project',
`projectile-compile-project', `projectile-test-project',
`projectile-install-project', `projectile-package-project',
`projectile-run-project' and the named tasks run via
`projectile-run-task' (which also feed the combined history this
command reads).

If the prefix argument SHOW-PROMPT is non nil, the command can be edited."
  (interactive "P")
  (let* ((project-root (projectile-acquire-root))
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
    ;; `command-map' is nil above, so `projectile--run-project-cmd' doesn't
    ;; record anything; we record here instead.  This command is
    ;; type-agnostic (it repeats the last command of any type), so it only
    ;; updates the combined history, not the per-type ones.
    (unless (string= command executed-command)
      (ring-insert command-history executed-command))))

(defvar projectile-last-task-map (make-hash-table :test 'equal)
  "The last task run per project, indexed by project root.
Each value is a cons of the task name and the command that was run,
which is what `projectile-repeat-last-task' re-runs.")

(defun projectile--run-task (task-name command show-prompt &optional confirmed)
  "Run TASK-NAME's COMMAND for the current project.

COMMAND is a shell command string, or a function returning one, called
with `default-directory' set to the project root.  With SHOW-PROMPT
non-nil the command can always be edited before it's run; otherwise
the command is offered for confirmation when `compilation-read-command'
is non-nil (the default), like the other lifecycle commands, unless
CONFIRMED says the user already confirmed this exact command (the
repeat case).

Task commands can come from a checked-out project's `.dir-locals.el',
whose `projectile-tasks' entries are accepted without the
risky-local-variable prompt - that is only acceptable because this
run-time confirmation is guaranteed, the same trade-off Emacs's
`compile-command' makes.

The command is executed like the other lifecycle commands (see
`projectile--run-project-cmd'), except that the output goes to a
per-task compilation buffer.  Return the command that was run, with
the `%p' placeholder still intact."
  (let* ((project-root (projectile-acquire-root))
         (command (if (functionp command)
                      (let ((default-directory project-root))
                        (funcall command))
                    command))
         ;; keyed like the per-type lifecycle histories, but per task
         (history-key (cons 'task task-name)))
    (unless (stringp command)
      (user-error "The command of task `%s' must be a string or a function returning one"
                  task-name))
    (when (or show-prompt
              (and compilation-read-command (not confirmed)))
      (setq command (projectile-read-command
                     (format "Task [%s] command: " task-name)
                     command
                     history-key)))
    ;; Any prompting already happened above, so bind
    ;; `compilation-read-command' to nil to stop
    ;; `projectile--run-project-cmd' from prompting a second time.
    (let ((compilation-read-command nil)
          (buffer-name (concat "*projectile-task: " task-name "*"
                               (when projectile-per-project-compilation-buffer
                                 (concat "<" (projectile-project-name project-root) ">")))))
      (projectile--run-project-cmd command nil
                                   :save-buffers t
                                   :buffer-name-function (lambda (_mode) buffer-name)))
    ;; `command-map' is nil above, so `projectile--run-project-cmd' records
    ;; nothing; record the command - before `%p' expansion, like the other
    ;; lifecycle commands - into the combined per-project history (which
    ;; `projectile-repeat-last-command' reads) and the per-task one here.
    (projectile--command-history-insert
     (projectile--get-command-history project-root) command)
    (projectile--command-history-insert
     (projectile--get-command-history project-root history-key) command)
    (puthash project-root (cons task-name command) projectile-last-task-map)
    command))

;;;###autoload
(defun projectile-run-task (arg)
  "Run one of the current project's named tasks.

The task is picked with completion among the tasks of the project's
type and those in `projectile-tasks', which win for same-named tasks
\(see `projectile-project-tasks').  With a prefix ARG the task's
command can be edited before it's run, e.g. to pass it ad-hoc
arguments.

The command runs through the same machinery as
`projectile-compile-project' - in `projectile-compilation-dir', with
`%p' expanded to the project name - but its output goes to a per-task
compilation buffer named after the task."
  (interactive "P")
  ;; Establish we're in a project before prompting, so the task menu
  ;; doesn't pop up (with globally-defined tasks) outside one.
  (projectile-acquire-root)
  (let ((tasks (projectile-project-tasks)))
    (unless tasks
      (user-error "No tasks defined for the current project"))
    (let* ((task-name (projectile-completing-read "Run task: " (mapcar #'car tasks)))
           (task (assoc task-name tasks)))
      (unless task
        (user-error "No task named `%s' in the current project" task-name))
      (projectile--run-task task-name (cdr task) arg))))

;;;###autoload
(defun projectile-repeat-last-task (arg)
  "Re-run the last task executed in the current project.

This re-runs the exact command the task ran last time, including any
ad-hoc edits made then.  With a prefix ARG the command can be edited
again before it's run."
  (interactive "P")
  (let* ((project-root (projectile-acquire-root))
         (last-task (gethash project-root projectile-last-task-map)))
    (unless last-task
      (user-error "No task has been run yet for this project"))
    ;; The stored command was confirmed when it first ran, so re-running
    ;; doesn't prompt again (like `projectile-repeat-last-command').
    (projectile--run-task (car last-task) (cdr last-task) arg 'confirmed)))

(defun compilation-find-file-projectile-find-compilation-buffer (orig-fun marker filename directory &rest formats)
  "Advice around compilation-find-file.
We enhance its functionality by appending the current project's directories
to its search path.  This way when filenames in compilation buffers can't be
found by compilation's normal logic they are searched for in project
directories."
  ;; If the file already exists, don't bother running the extra logic as the
  ;; project directories might be massive (i.e. Unreal-sized).
  (if (file-exists-p filename)
      (apply orig-fun `(,marker ,filename ,directory ,@formats))

    (let* ((root (projectile-project-root))
           (compilation-search-path
            (if (projectile-project-p)
                (let ((dirs (append compilation-search-path (list root)
                                    (mapcar (lambda (f) (expand-file-name f root))
                                            (projectile-current-project-dirs)))))
                  ;; If the file can be found relative to the project root,
                  ;; add its parent directory to the search path.  This
                  ;; handles directories that contain only subdirectories
                  ;; and no files directly.
                  (let ((candidate (expand-file-name filename root)))
                    (when (file-exists-p candidate)
                      (push (file-name-directory candidate) dirs)))
                  dirs)
              compilation-search-path)))
      (apply orig-fun `(,marker ,filename ,directory ,@formats)))))

(defun projectile-open-projects ()
  "Return a list of all open projects.
An open project is a project with any open buffers."
  (let ((truename-cache (make-hash-table :test 'equal)))
    (seq-uniq
     ;; TODO: Replace delq+mapcar with seq-keep when Emacs 29.1 is the minimum version
     (delq nil
           (mapcar (lambda (buffer)
                     (with-current-buffer buffer
                       (when-let* ((project-root (projectile-project-root)))
                         (when (projectile-project-buffer-p buffer project-root truename-cache)
                           (abbreviate-file-name project-root)))))
                   (buffer-list))))))

(defun projectile--remove-current-project (projects)
  "Remove the current project (if any) from the list of PROJECTS."
  (if-let* ((project (projectile-project-root)))
      (seq-difference projects
                             (list (abbreviate-file-name project)))
    projects))

(defun projectile--move-current-project-to-end (projects)
  "Move current project (if any) to the end of the list of PROJECTS."
  (if-let* ((project (projectile-project-root)))
      (append
       (projectile--remove-current-project projects)
       (list (abbreviate-file-name project)))
    projects))

(defun projectile-known-projects ()
  "Initialize the known projects.

This might potentially clean up redundant projects and discover new ones if
`projectile-auto-cleanup-known-projects' or `projectile-auto-discover' are
enabled."
  ;; load the known projects
  (unless projectile-known-projects
    (projectile-load-known-projects))
  (when projectile-auto-cleanup-known-projects
    (projectile--cleanup-known-projects))
  (when (and projectile-auto-discover
             projectile-project-search-path
             (not projectile--search-path-discovered))
    (projectile-discover-projects-in-search-path))
  ;; return the list of known projects
  projectile-known-projects)

(defun projectile-relevant-known-projects ()
  "Return a list of known projects.

Projects matched by `projectile-ignored-projects' or
`projectile-ignored-project-function' are excluded, even if they were
added to the known projects before being ignored (see #1663).

It factors the value of `projectile-current-project-on-switch'."
  (let ((known-projects (projectile-known-projects)))
    ;; Only filter when there's actually some ignore configuration, so the
    ;; common case doesn't pay for a `file-truename' per known project.
    (when (or projectile-ignored-projects projectile-ignored-project-function)
      (setq known-projects (seq-remove #'projectile-ignored-project-p known-projects)))
    (pcase projectile-current-project-on-switch
      ('remove (projectile--remove-current-project known-projects))
      ('move-to-end (projectile--move-current-project-to-end known-projects))
      ('keep known-projects))))

(defun projectile-relevant-open-projects ()
  "Return a list of open projects."
  (let ((open-projects (projectile-open-projects)))
    (pcase projectile-current-project-on-switch
      ('remove (projectile--remove-current-project open-projects))
      ('move-to-end (projectile--move-current-project-to-end open-projects))
      ('keep open-projects))))

(defvar projectile-most-recent-project nil
  "Root of the project that was current before the most recent project switch.

Updated by `projectile-switch-project-by-name', so it only tracks
switches made through Projectile's switch-project commands (not project
changes that happen merely by visiting a file or buffer in another
project).  Use `projectile-switch-to-most-recent-project' to jump to it.")

;;;###autoload
(defun projectile-switch-project (&optional arg)
  "Switch to a project we have visited before.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-dispatch' instead
of `projectile-switch-project-action'."
  (interactive "P")
  (let ((projects (projectile-relevant-known-projects)))
    (if projects
        (projectile-completing-read
         "Switch to project: " projects
         :action (lambda (project)
                   (projectile-switch-project-by-name project arg))
         :category 'file
         :caller 'projectile-read-project)
      (user-error "There are no known projects"))))

;;;###autoload
(defun projectile-switch-open-project (&optional arg)
  "Switch to a project we have currently opened.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-dispatch' instead
of `projectile-switch-project-action'."
  (interactive "P")
  (let ((projects (projectile-relevant-open-projects)))
    (if projects
        (projectile-completing-read
         "Switch to open project: " projects
         :action (lambda (project)
                   (projectile-switch-project-by-name project arg))
         :category 'file
         :caller 'projectile-read-project)
      (user-error "There are no open projects"))))

;; The other-window/-frame switch commands reuse `projectile-switch-project'
;; wholesale (so the dir-locals dance, prefix-arg dispatch, and most-recent
;; tracking all stay in one place) and just rebind the post-switch action
;; around it.  This relies on the completion framework invoking its action
;; synchronously, which the supported ones (default, vertico, ivy, helm) do.
;;;###autoload (autoload 'projectile-switch-project-other-window "projectile" nil t)
;;;###autoload (autoload 'projectile-switch-project-other-frame "projectile" nil t)
(projectile--define-display-variants projectile-switch-project (&optional arg)
  "Switch to a project we have visited before; display it in another %s.

Like `projectile-switch-project', but runs
`projectile-switch-project-other-%s-action' (by default
`projectile-find-file-other-%s') after switching, so the project is
shown in another %s.  With a prefix ARG invokes `projectile-dispatch'
instead."
  (let ((projectile-switch-project-action projectile-switch-project-other-window-action))
    (projectile-switch-project arg)))

;;;###autoload
(defun projectile-switch-to-most-recent-project (&optional arg)
  "Switch to the project recorded in `projectile-most-recent-project'.
That's the project that was current before the most recent project
switch, so calling this from a buffer in the switched-to project takes
you back where you came from.  With a prefix ARG invokes
`projectile-dispatch' instead of
`projectile-switch-project-action'."
  (interactive "P")
  (if projectile-most-recent-project
      (projectile-switch-project-by-name projectile-most-recent-project arg)
    (user-error "No most recent project recorded yet")))

(defun projectile--transient-command-p (command)
  "Return non-nil if COMMAND is a transient prefix.
Such commands (e.g. `projectile-dispatch') pop a menu and run the chosen
suffix command asynchronously, after the caller has already returned.
Detected via the `transient--prefix' symbol property that
`transient-define-prefix' sets, which is also how `transient' itself
recognises its prefixes."
  (and (symbolp command)
       (fboundp command)
       (get command 'transient--prefix)))

(defun projectile--dispatch-in-directory (directory action)
  "Run transient ACTION with DIRECTORY as the project context.
ACTION (e.g. `projectile-dispatch') is a transient prefix, so its suffix
commands run after this function returns; a dynamic `default-directory'
binding (or a temporary buffer) would be unwound by then.  Instead set
the current buffer's `default-directory' to DIRECTORY (the buffer that
is current now is the one the suffix commands run in) for the lifetime
of the transient, restoring it once the menu exits.  This makes commands
picked from the menu - like `projectile-find-file' - target the
switched-to project."
  (let ((buffer (current-buffer))
        (original-directory default-directory))
    (setq default-directory directory)
    (letrec ((restore
              (lambda ()
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (setq default-directory original-directory)))
                (remove-hook 'transient-exit-hook restore))))
      (add-hook 'transient-exit-hook restore))
    ;; A transient prefix is an interactive-only command, so invoke it via
    ;; `call-interactively'.
    (call-interactively action)))

(defun projectile-switch-project-by-name (project-to-switch &optional arg)
  "Switch to project by project name PROJECT-TO-SWITCH.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-dispatch' instead
of `projectile-switch-project-action'."
  ;; let's make sure that the target directory exists and is actually a project
  ;; we ignore remote folders, as the check breaks for TRAMP unless already connected
  (unless (or (file-remote-p project-to-switch) (projectile-project-p project-to-switch))
    (projectile-remove-known-project project-to-switch)
    (user-error "Directory %s is not a project" project-to-switch))
  ;; Record the project we're leaving so `projectile-most-recent-project'
  ;; points at it after the switch (captured before `default-directory' is
  ;; rebound below).
  (let ((previous-project (projectile-project-root))
        (action (if arg
                    'projectile-dispatch
                  projectile-switch-project-action)))
    (run-hooks 'projectile-before-switch-project-hook)
    (if (projectile--transient-command-p action)
        ;; A transient action (e.g. `projectile-dispatch', whether reached
        ;; via the prefix argument or set as `projectile-switch-project-action'
        ;; directly) runs its suffix commands *after* this function returns,
        ;; so a dynamic `default-directory' binding (or the temporary buffer
        ;; below) would be gone by the time the chosen command runs.  Hand off
        ;; to a helper that keeps PROJECT-TO-SWITCH current for the lifetime of
        ;; the menu instead.
        (projectile--dispatch-in-directory project-to-switch action)
      (let* ((default-directory project-to-switch)
             (switched-buffer
              ;; use a temporary buffer to load PROJECT-TO-SWITCH's dir-locals
              ;; before calling the switch-project action
              (with-temp-buffer
                (hack-dir-local-variables-non-file-buffer)
                ;; Normally the project name is determined from the current
                ;; buffer. However, when we're switching projects, we want to
                ;; show the name of the project being switched to, rather than
                ;; the current project, in the minibuffer. This is a simple hack
                ;; to tell the `projectile-project-name' function to ignore the
                ;; current buffer and the caching mechanism, and just return the
                ;; value of the `projectile-project-name' variable.
                (let ((projectile-project-name (funcall projectile-project-name-function
                                                        project-to-switch)))
                  (funcall action)
                  (current-buffer)))))
        ;; If the action switched buffers then with-temp-buffer will
        ;; have lost that change, so switch back to the correct buffer.
        (when (buffer-live-p switched-buffer)
          (switch-to-buffer switched-buffer))))
    ;; Don't record the project we just came from if it's the same one we
    ;; switched to.  Compare with `file-equal-p' for local paths (handles
    ;; symlinks/abbreviation), but fall back to a plain string compare when
    ;; either side is remote, so we don't trigger a TRAMP round-trip (and
    ;; possible hang) for an unconnected remote project - the very thing the
    ;; remote skip above guards against.
    (when (and previous-project
               (not (if (or (file-remote-p previous-project)
                            (file-remote-p project-to-switch))
                        (string-equal (file-name-as-directory previous-project)
                                      (file-name-as-directory project-to-switch))
                      (file-equal-p previous-project project-to-switch))))
      (setq projectile-most-recent-project previous-project))
    ;; The switch action usually visits a file or directory, which already
    ;; runs the project-changed functions; this covers actions that don't.
    ;; Resolving the root of an unconnected remote project would trigger a
    ;; TRAMP connection, so leave remote detection to the next file visit.
    (unless (file-remote-p project-to-switch)
      (projectile--maybe-run-project-changed-functions
       (projectile-project-root project-to-switch)))
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
                                                (projectile-dir-files directory)
                                                :caller 'projectile-read-file)))
          (find-file (expand-file-name file directory))
          (run-hooks 'projectile-find-file-hook))
      ;; target directory is not in a project
      (projectile-find-file))))

(defun projectile-all-project-files ()
  "Get a list of all files in all projects."
  (mapcan
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
  (find-file (projectile-completing-read "Find file in projects: " (projectile-all-project-files)
                                         :caller 'projectile-read-file)))

(defun projectile-keep-project-p (project)
  "Determine whether we should cleanup (remove) PROJECT or not.

It handles the case of remote projects as well.
See `projectile--cleanup-known-projects'.

Remote projects are always kept regardless of connection state.
Previously a remote project that *was* connected was tested with
`file-readable-p', which is a remote round-trip per project - and
since `projectile--cleanup-known-projects' may be called every time
the user invokes a switch-project command (when
`projectile-auto-cleanup-known-projects' is on), that turned project
switching into a sequence of network stats.  The user can still
explicitly drop dead remote projects via `projectile-remove-known-project'."
  ;; Taken from `recentf-keep-default-predicate'
  (cond
   ((file-remote-p project))
   ((file-readable-p project))))

(defun projectile--cleanup-known-projects ()
  "Remove known projects that don't exist anymore.
Return a list of projects removed."
  (projectile-merge-known-projects)
  (let ((projects-kept (seq-filter #'projectile-keep-project-p projectile-known-projects))
        (projects-removed (seq-remove #'projectile-keep-project-p projectile-known-projects)))
    (setq projectile-known-projects projects-kept)
    (projectile-merge-known-projects)
    projects-removed))

;;;###autoload
(defun projectile-cleanup-known-projects ()
  "Remove known projects that don't exist anymore."
  (interactive)
  (if-let* ((projects-removed (projectile--cleanup-known-projects)))
      (message "Projects removed: %s"
               (mapconcat #'identity projects-removed ", "))
    (message "No projects needed to be removed.")))

;;;###autoload
(defalias 'projectile-forget-zombie-projects #'projectile-cleanup-known-projects
  "Forget known projects that don't exist any more.
An alias for `projectile-cleanup-known-projects', provided for
discoverability and parity with project.el's
`project-forget-zombie-projects'.")

;;;###autoload
(defun projectile-forget-projects-under (directory &optional recursive)
  "Remove known projects located under DIRECTORY.

Interactively, prompt for DIRECTORY.  With optional argument
RECURSIVE non-nil (interactively, the prefix argument), remove
projects nested at any depth under DIRECTORY; otherwise only remove
projects that are immediate children of DIRECTORY.

Matching is lexical (after `file-truename' expansion for local paths,
which is skipped for remote ones to avoid a round-trip), so projects
are removed even when DIRECTORY has already been deleted.  Mirrors
project.el's `project-forget-projects-under'.  Return the number of
projects removed."
  (interactive "DForget projects under directory: \nP")
  (let* ((expand (lambda (path)
                   (file-name-as-directory
                    (if (file-remote-p path) path (file-truename path)))))
         (directory (funcall expand directory))
         (projects-removed
          (seq-filter
           (lambda (project)
             (let ((project (funcall expand project)))
               (if recursive
                   (string-prefix-p directory project)
                 (string= (file-name-directory (directory-file-name project))
                          directory))))
           projectile-known-projects)))
    (setq projectile-known-projects
          (seq-difference projectile-known-projects projects-removed))
    (projectile-merge-known-projects)
    (if projects-removed
        (message "Projects removed: %s"
                 (mapconcat #'identity projects-removed ", "))
      (message "No projects found under %s." (abbreviate-file-name directory)))
    (length projects-removed)))

;;;###autoload
(defun projectile-clear-known-projects ()
  "Clear both `projectile-known-projects' and `projectile-known-projects-file'."
  (interactive)
  (setq projectile-known-projects nil)
  (projectile-save-known-projects))

;;;###autoload
(defun projectile-reset-known-projects ()
  "Clear known projects and rediscover."
  (interactive)
  (projectile-clear-known-projects)
  (projectile-discover-projects-in-search-path))

;;;###autoload
(defun projectile-remove-known-project (&optional project)
  "Remove PROJECT from the list of known projects."
  (interactive (list (projectile-completing-read
                      "Remove from known projects: " projectile-known-projects
                      :action 'projectile-remove-known-project
                      :category 'file
                      :caller 'projectile-read-project)))
  (unless (called-interactively-p 'any)
    (setq projectile-known-projects
          (seq-remove
           (lambda (proj) (string= project proj))
           projectile-known-projects))
    ;; Known projects are stored abbreviated while the watch registry is
    ;; keyed by the cache key (usually expanded), so try both forms.
    (when project
      (projectile--unwatch-project project)
      (projectile--unwatch-project (expand-file-name project)))
    (projectile-merge-known-projects)
    (when projectile-verbose
      (message "Project %s removed from the list of known projects." project))))

;;;###autoload
(defun projectile-remove-current-project-from-known-projects ()
  "Remove the current project from the list of known projects."
  (interactive)
  (projectile-remove-known-project (projectile--known-project-root (projectile-acquire-root))))

(defun projectile-ignored-projects ()
  "A list of projects that should not be saved in `projectile-known-projects'.
Local entries are canonicalized via `file-truename'; remote entries
are returned as-is to avoid a remote round-trip per entry on every
lookup (see `projectile-ignored-project-p')."
  (mapcar (lambda (project)
            (if (file-remote-p project) project (file-truename project)))
          projectile-ignored-projects))

(defun projectile-ignored-project-p (project-root)
  "Return t if PROJECT-ROOT should not be added to `projectile-known-projects'.

For remote (TRAMP) paths the symlink-resolution step is skipped:
`file-truename' would round-trip to the remote, and matching against
`projectile-ignored-projects' for a remote project is uncommon enough
that requiring exact paths is acceptable.  Local behavior is unchanged."
  (let ((project-root (if (file-remote-p project-root)
                          project-root
                        (file-truename project-root))))
    (or (member project-root (projectile-ignored-projects))
        (and (functionp projectile-ignored-project-function)
             (funcall projectile-ignored-project-function project-root)))))

;;;###autoload
(defun projectile-add-known-project (project-root)
  "Add PROJECT-ROOT to the list of known projects."
  (interactive (list (read-directory-name "Add to known projects: ")))
  (unless (projectile-ignored-project-p project-root)
    (push (projectile--known-project-root project-root) projectile-known-projects)
    (setq projectile-known-projects (seq-uniq projectile-known-projects))
    (projectile-merge-known-projects)))

;;;###autoload
(defun projectile-add-and-switch-project (project-root)
  "Add PROJECT-ROOT to the list of known projects and switch to it.
This combines `projectile-add-known-project' and
`projectile-switch-project-by-name' into a single command."
  (interactive (list (read-directory-name "Add and switch to project: ")))
  (projectile-add-known-project project-root)
  (projectile-switch-project-by-name (file-name-as-directory project-root)))

(defun projectile-load-known-projects ()
  "Load saved projects from `projectile-known-projects-file'.
Also set `projectile-known-projects'."
  (let ((data (projectile-unserialize projectile-known-projects-file)))
    (setq projectile-known-projects
          (if (proper-list-p data) data nil))
    (unless (equal data projectile-known-projects)
      (message "Warning: Projectile known projects file was corrupted, ignoring saved data"))
    (setq projectile-known-projects-on-file
          (and (sequencep projectile-known-projects)
               (copy-sequence projectile-known-projects)))))

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
          (let ((data (projectile-unserialize projectile-known-projects-file)))
            (if (proper-list-p data) data nil)))
         (removed-after-sync (seq-difference known-on-last-sync known-now))
         (removed-in-other-process
          (seq-difference known-on-last-sync known-on-file))
         (result (seq-uniq
                  (seq-difference
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
    (let ((directory (file-name-as-directory (expand-file-name qualifier))))
      (and (projectile-project-buffer-p buf directory)
           (equal directory
                  (projectile-project-root))))))

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
                           (projectile-relevant-known-projects)
                           :category 'file
                           :caller 'projectile-read-project)
                        (projectile-acquire-root))))
    (projectile-ibuffer-by-project project-root)))


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
            (setq counter (1+ counter)))))
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
  "Prompt for a variable and return its name as a string.
Return nil on empty input, which ends the variable-entry loop of
`projectile-skel-dir-locals' while keeping the entries made so far."
  (let ((var (completing-read "Variable (RET when done): "
                              obarray
                              (lambda (v)
                                (and (boundp v) (not (keywordp v))))
                              t)))
    (unless (string-empty-p var) var)))

(define-skeleton projectile-skel-variable-cons
  "Insert a variable-name and a value in a cons-cell."
  (projectile-read-variable)
  "(" str " . " (skeleton-read "Value: " nil t) ")")

(define-skeleton projectile-skel-dir-locals
  "Insert a .dir-locals.el template.
The variable-entry loop ends when an empty variable name is
entered, keeping the entries made so far."
  nil
  "((nil . ("
  ((projectile-read-variable) "(" str " . " (skeleton-read "Value: " nil t) ")" \n)
  resume:
  ")))")

;;;###autoload
(defun projectile-edit-dir-locals ()
  "Edit or create a .dir-locals.el file of the project."
  (interactive)
  (let ((file (expand-file-name ".dir-locals.el" (projectile-acquire-root))))
    (find-file file)
    (when (not (file-exists-p file))
      (projectile-skel-dir-locals)
      (save-buffer))))


;;; Projectile Minor mode

(defcustom projectile-mode-line-prefix
  " Projectile"
  "Mode line lighter prefix for Projectile.
It's used by `projectile-default-mode-line'
when using dynamic mode line lighter and is the only
thing shown in the mode line otherwise."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "0.12.0"))

(defcustom projectile-show-menu t
  "Controls whether to display Projectile's menu."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.6.0"))

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

(defun projectile-update-mode-line-on-window-change ()
  "Update the mode-line when the window configuration changes.
This ensures the mode-line is correct in non-file buffers like
Magit that don't trigger `find-file-hook'."
  (when projectile-dynamic-mode-line
    (unless (file-remote-p default-directory)
      (projectile-update-mode-line))))

(defvar projectile-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "4 4") #'projectile-other-window-command)
    (define-key map (kbd "4 a") #'projectile-find-other-file-other-window)
    (define-key map (kbd "4 b") #'projectile-switch-to-buffer-other-window)
    (define-key map (kbd "4 C-o") #'projectile-display-buffer)
    (define-key map (kbd "4 d") #'projectile-find-dir-other-window)
    (define-key map (kbd "4 D") #'projectile-dired-other-window)
    (define-key map (kbd "4 f") #'projectile-find-file-other-window)
    (define-key map (kbd "4 g") #'projectile-find-file-dwim-other-window)
    (define-key map (kbd "4 j") #'projectile-find-file-of-kind-other-window)
    (define-key map (kbd "4 p") #'projectile-switch-project-other-window)
    (define-key map (kbd "4 t") #'projectile-find-implementation-or-test-other-window)
    (define-key map (kbd "5 5") #'projectile-other-frame-command)
    (define-key map (kbd "5 a") #'projectile-find-other-file-other-frame)
    (define-key map (kbd "5 b") #'projectile-switch-to-buffer-other-frame)
    (define-key map (kbd "5 d") #'projectile-find-dir-other-frame)
    (define-key map (kbd "5 D") #'projectile-dired-other-frame)
    (define-key map (kbd "5 f") #'projectile-find-file-other-frame)
    (define-key map (kbd "5 g") #'projectile-find-file-dwim-other-frame)
    (define-key map (kbd "5 j") #'projectile-find-file-of-kind-other-frame)
    (define-key map (kbd "5 p") #'projectile-switch-project-other-frame)
    (define-key map (kbd "5 t") #'projectile-find-implementation-or-test-other-frame)
    (define-key map (kbd "!") #'projectile-run-shell-command-in-root)
    (define-key map (kbd "&") #'projectile-run-async-shell-command-in-root)
    (define-key map (kbd "?") #'projectile-find-references)
    (define-key map (kbd "a") #'projectile-find-other-file)
    (define-key map (kbd "A") #'projectile-add-known-project)
    (define-key map (kbd "b") #'projectile-switch-to-buffer)
    (define-key map (kbd "d") #'projectile-find-dir)
    (define-key map (kbd "D") #'projectile-dired)
    (define-key map (kbd "e") #'projectile-recentf)
    (define-key map (kbd "E") #'projectile-edit-dir-locals)
    (define-key map (kbd "f") #'projectile-find-file)
    (define-key map (kbd "g") #'projectile-find-file-dwim)
    (define-key map (kbd "F") #'projectile-find-file-in-known-projects)
    ;; the h key is reserved for helm-projectile
    ;; the binding below will be added when helm-projectile is enabled
    ;; (define-key projectile-command-map (kbd "h") #'helm-projectile)
    (define-key map (kbd "i") #'projectile-invalidate-cache)
    (define-key map (kbd "I") #'projectile-ibuffer)
    (define-key map (kbd "j") #'projectile-find-file-of-kind)
    (define-key map (kbd "J") #'projectile-toggle-related-file)
    (define-key map (kbd "k") #'projectile-kill-buffers)
    (define-key map (kbd "l") #'projectile-find-file-in-directory)
    (define-key map (kbd "m") #'projectile-dispatch)
    (define-key map (kbd "o") #'projectile-multi-occur)
    (define-key map (kbd "p") #'projectile-switch-project)
    (define-key map (kbd "q") #'projectile-switch-open-project)
    (define-key map (kbd "r") #'projectile-replace)
    (define-key map (kbd "R") #'projectile-replace-review)
    (define-key map (kbd "s s") #'projectile-search)
    (define-key map (kbd "s g") #'projectile-grep)
    (define-key map (kbd "s r") #'projectile-ripgrep)
    (define-key map (kbd "s a") #'projectile-ag)
    (define-key map (kbd "s x") #'projectile-find-references)
    (define-key map (kbd "s R") #'projectile-search-review)
    (define-key map (kbd "s X") #'projectile-search-regexp-review)
    (define-key map (kbd "S") #'projectile-save-project-buffers)
    (define-key map (kbd "t") #'projectile-toggle-between-implementation-and-test)
    (define-key map (kbd "T") #'projectile-find-test-file)
    (define-key map (kbd "v") #'projectile-vc)
    ;; per-project sessions (see `projectile-session-mode')
    (define-key map (kbd "w s") #'projectile-session-save)
    (define-key map (kbd "w S") #'projectile-session-save-all)
    (define-key map (kbd "w r") #'projectile-session-restore)
    (define-key map (kbd "w R") #'projectile-session-restore-all)
    (define-key map (kbd "w f") #'projectile-session-forget)
    (define-key map (kbd "w b") #'projectile-session-switch-to-buffer)
    ;; project lifecycle external commands
    (define-key map (kbd "c o") #'projectile-configure-project)
    (define-key map (kbd "c c") #'projectile-compile-project)
    (define-key map (kbd "c p") #'projectile-package-project)
    (define-key map (kbd "c i") #'projectile-install-project)
    (define-key map (kbd "c t") #'projectile-test-project)
    (define-key map (kbd "c .") #'projectile-run-test-at-point)
    (define-key map (kbd "c r") #'projectile-run-project)
    (define-key map (kbd "c m c") #'projectile-compile-subproject)
    (define-key map (kbd "c m t") #'projectile-test-subproject)
    (define-key map (kbd "c x") #'projectile-run-task)
    (define-key map (kbd "c X") #'projectile-repeat-last-task)
    ;; integration with utilities
    (define-key map (kbd "x r") #'projectile-run)
    (define-key map (kbd "x e") #'projectile-run-eshell)
    (define-key map (kbd "x i") #'projectile-run-ielm)
    (define-key map (kbd "x t") #'projectile-run-term)
    (define-key map (kbd "x s") #'projectile-run-shell)
    (define-key map (kbd "x g") #'projectile-run-gdb)
    (define-key map (kbd "x v") #'projectile-run-vterm)
    (define-key map (kbd "x 4 v") #'projectile-run-vterm-other-window)
    (define-key map (kbd "x x") #'projectile-run-eat)
    (define-key map (kbd "x 4 x") #'projectile-run-eat-other-window)
    (define-key map (kbd "x G") #'projectile-run-ghostel)
    (define-key map (kbd "x 4 G") #'projectile-run-ghostel-other-window)
    ;; misc
    (define-key map (kbd "z") #'projectile-cache-current-file)
    (define-key map (kbd "<left>") #'projectile-previous-project-buffer)
    (define-key map (kbd "<right>") #'projectile-next-project-buffer)
    (define-key map (kbd "ESC") #'projectile-project-buffers-other-buffer)
    map)
  "Keymap for Projectile commands after `projectile-keymap-prefix'.")
(fset 'projectile-command-map projectile-command-map)

;;; Prefix commands for other-window/-frame display
;;
;; These are Projectile's take on the Emacs 28 `other-window-prefix' and
;; `other-frame-prefix' commands (C-x 4 4 and C-x 5 5): they arrange for
;; the buffer displayed by the *next* command to go to another window or
;; frame, and additionally keep `projectile-command-map' active for the
;; next key sequence.  So `4 4 f' (after the Projectile prefix) opens a
;; project file in another window, and the same trick works for commands
;; that never had a dedicated -other-window variant, e.g. `4 4 x s' runs
;; a project shell in another window.

(defun projectile--obey-display-actions-for-next-command ()
  "Cover `switch-to-buffer'-based commands in the next-command override.
Emacs 29 made `display-buffer-override-next-command' temporarily enable
`switch-to-buffer-obey-display-actions', so that the display override
armed by `other-window-prefix'/`other-frame-prefix' also applies to
commands that display buffers with `switch-to-buffer'.  Emacs 28 lacks
that, so arrange for it here: enable the option and restore the user's
setting once the next command is done (mirroring the teardown conditions
of window.el's own post-command function)."
  (when (< emacs-major-version 29)
    (let* ((obey-display switch-to-buffer-obey-display-actions)
           (command this-command)
           (depth (minibuffer-depth))
           (restore (make-symbol "projectile--restore-obey-display-actions")))
      (setq switch-to-buffer-obey-display-actions t)
      (fset restore
            (lambda ()
              ;; Stay armed while reading from the minibuffer and while
              ;; the command that armed us is still in progress.
              (unless (or (> (minibuffer-depth) depth)
                          (eq this-command command))
                (setq switch-to-buffer-obey-display-actions obey-display)
                (remove-hook 'post-command-hook restore))))
      (add-hook 'post-command-hook restore))))

;;;###autoload
(defun projectile-other-window-command ()
  "Show the buffer of the next Projectile command in another window.

Set up the next command's buffer to be displayed in a new window (via
`other-window-prefix') and keep `projectile-command-map' active for the
next key sequence, so any Projectile key typed right after this command
gets the other-window treatment without re-typing the Projectile
prefix.  Any non-Projectile command works as well; commands that use
`switch-to-buffer' are covered too, by temporarily enabling
`switch-to-buffer-obey-display-actions'."
  (interactive)
  (other-window-prefix)
  (projectile--obey-display-actions-for-next-command)
  (set-transient-map projectile-command-map)
  (message "Display buffer of next (Projectile) command in a new window..."))

;;;###autoload
(defun projectile-other-frame-command ()
  "Show the buffer of the next Projectile command in another frame.

Set up the next command's buffer to be displayed in a new frame (via
`other-frame-prefix') and keep `projectile-command-map' active for the
next key sequence, so any Projectile key typed right after this command
gets the other-frame treatment without re-typing the Projectile
prefix.  Any non-Projectile command works as well; commands that use
`switch-to-buffer' are covered too, by temporarily enabling
`switch-to-buffer-obey-display-actions'."
  (interactive)
  (other-frame-prefix)
  (projectile--obey-display-actions-for-next-command)
  (set-transient-map projectile-command-map)
  (message "Display buffer of next (Projectile) command in a new frame..."))

;;; projectile-dispatch modifiers
;;
;; `projectile-dispatch' (below) exposes a handful of command modifiers as
;; transient switches: `--invalidate-cache', `--regexp', `--new-process' and a
;; `--display' target (this window / other window / other frame).  Several
;; Projectile commands already honour these via a prefix argument, and the
;; other-window/-frame behaviour is provided by dedicated command variants.
;; The switches are wired to the commands through the thin wrapper commands
;; generated below: each reads the active switches and either dispatches to the
;; right display variant or sets `current-prefix-arg' accordingly.
;;
;; These wrappers are plain commands (usable without `transient' loaded, they
;; simply see no active switches then), so they're defined unconditionally,
;; unlike the transient prefix itself.

(defun projectile-dispatch--args ()
  "Return the active `projectile-dispatch' switches, or nil.
Only returns switches while a transient suffix is executing; a wrapper
invoked outside the menu sees none."
  (and (bound-and-true-p transient-current-command)
       (transient-args transient-current-command)))

(defmacro projectile-dispatch--define (name command &rest props)
  "Define command NAME as a `projectile-dispatch' wrapper around COMMAND.
PROPS is a plist of:
  :other-window CMD, :other-frame CMD  command variants selected by the
                                       `--display' switch;
  :prefix-arg SWITCH                   set `current-prefix-arg' to a plain
                                       prefix when SWITCH is active."
  (declare (indent 2))
  (let ((ow (plist-get props :other-window))
        (of (plist-get props :other-frame))
        (switch (plist-get props :prefix-arg)))
    `(defun ,name ()
       ,(format "A `projectile-dispatch' wrapper honouring its modifier switches.\nRuns `%s'." command)
       (interactive)
       (let* ((projectile-dispatch--switches (projectile-dispatch--args))
              (current-prefix-arg
               ,(if switch
                    `(if (member ,switch projectile-dispatch--switches)
                         '(4)
                       current-prefix-arg)
                  'current-prefix-arg))
              (command
               ,(if ow
                    `(cond
                      ((member "--display=frame" projectile-dispatch--switches) #',of)
                      ((member "--display=window" projectile-dispatch--switches) #',ow)
                      (t #',command))
                  `#',command)))
         (call-interactively command)))))

;; Display + cache
(projectile-dispatch--define projectile-dispatch-find-file projectile-find-file
  :other-window projectile-find-file-other-window
  :other-frame projectile-find-file-other-frame
  :prefix-arg "--invalidate-cache")
(projectile-dispatch--define projectile-dispatch-find-file-dwim projectile-find-file-dwim
  :other-window projectile-find-file-dwim-other-window
  :other-frame projectile-find-file-dwim-other-frame
  :prefix-arg "--invalidate-cache")
(projectile-dispatch--define projectile-dispatch-find-dir projectile-find-dir
  :other-window projectile-find-dir-other-window
  :other-frame projectile-find-dir-other-frame
  :prefix-arg "--invalidate-cache")
;; Display only
(projectile-dispatch--define projectile-dispatch-find-other-file projectile-find-other-file
  :other-window projectile-find-other-file-other-window
  :other-frame projectile-find-other-file-other-frame)
(projectile-dispatch--define projectile-dispatch-find-file-of-kind projectile-find-file-of-kind
  :other-window projectile-find-file-of-kind-other-window
  :other-frame projectile-find-file-of-kind-other-frame)
(projectile-dispatch--define projectile-dispatch-dired projectile-dired
  :other-window projectile-dired-other-window
  :other-frame projectile-dired-other-frame)
(projectile-dispatch--define projectile-dispatch-switch-to-buffer projectile-switch-to-buffer
  :other-window projectile-switch-to-buffer-other-window
  :other-frame projectile-switch-to-buffer-other-frame)
(projectile-dispatch--define projectile-dispatch-switch-project projectile-switch-project
  :other-window projectile-switch-project-other-window
  :other-frame projectile-switch-project-other-frame)
(projectile-dispatch--define projectile-dispatch-impl-or-test
    projectile-toggle-between-implementation-and-test
  :other-window projectile-find-implementation-or-test-other-window
  :other-frame projectile-find-implementation-or-test-other-frame)
;; Cache only
(projectile-dispatch--define projectile-dispatch-find-test-file projectile-find-test-file
  :prefix-arg "--invalidate-cache")
;; Regexp search
(projectile-dispatch--define projectile-dispatch-search projectile-search
  :prefix-arg "--regexp")
(projectile-dispatch--define projectile-dispatch-ag projectile-ag
  :prefix-arg "--regexp")
(projectile-dispatch--define projectile-dispatch-ripgrep projectile-ripgrep
  :prefix-arg "--regexp")

(defun projectile-dispatch-search-review ()
  "Reviewable search honouring the `--regexp' and `--case-sensitive' switches.
`--regexp' runs the Emacs-regexp reviewer, `--case-sensitive' seeds the
search case-sensitive; all can still be flipped (`x'/`c'/`w') in the results
buffer."
  (interactive)
  (let ((switches (projectile-dispatch--args)))
    (let ((case-fold-search (if (member "--case-sensitive" switches)
                                nil case-fold-search))
          (projectile-search-whole-word
           (if (member "--word" switches) t projectile-search-whole-word)))
      (call-interactively (if (member "--regexp" switches)
                              #'projectile-search-regexp-review
                            #'projectile-search-review)))))

(defun projectile-dispatch-replace-review ()
  "Reviewable replace honouring the `--regexp' and `--case-sensitive' switches.
Like `projectile-dispatch-search-review', but for the replace reviewer."
  (interactive)
  (let ((switches (projectile-dispatch--args)))
    (let ((case-fold-search (if (member "--case-sensitive" switches)
                                nil case-fold-search))
          (projectile-search-whole-word
           (if (member "--word" switches) t projectile-search-whole-word)))
      (call-interactively (if (member "--regexp" switches)
                              #'projectile-replace-regexp-review
                            #'projectile-replace-review)))))
;; New process
(projectile-dispatch--define projectile-dispatch-run projectile-run
  :prefix-arg "--new-process")
(projectile-dispatch--define projectile-dispatch-run-eshell projectile-run-eshell
  :prefix-arg "--new-process")
(projectile-dispatch--define projectile-dispatch-run-shell projectile-run-shell
  :prefix-arg "--new-process")
(projectile-dispatch--define projectile-dispatch-run-ielm projectile-run-ielm
  :prefix-arg "--new-process")
(projectile-dispatch--define projectile-dispatch-run-term projectile-run-term
  :prefix-arg "--new-process")
(projectile-dispatch--define projectile-dispatch-run-vterm projectile-run-vterm
  :prefix-arg "--new-process")
(projectile-dispatch--define projectile-dispatch-run-eat projectile-run-eat
  :prefix-arg "--new-process")
(projectile-dispatch--define projectile-dispatch-run-ghostel projectile-run-ghostel
  :prefix-arg "--new-process")

;; `projectile-dispatch' is a transient menu mirroring `projectile-command-map'.
;; The menu keys deliberately match the `projectile-command-map' bindings.
;; The transient prefix is defined lazily: loading `transient' costs a few
;; milliseconds and some memory for every session, while the menu is only
;; needed once invoked.  `projectile-dispatch' below is a stub that loads
;; `transient', evaluates the real definition (replacing itself), and
;; re-invokes it; `transient' is required at compile time (see the top of
;; the file) so the macro still expands during byte-compilation.
(defun projectile--dispatch-define ()
  "Define the `projectile-dispatch' transient prefix, replacing the stub."
  (transient-define-prefix projectile-dispatch ()
    "Dispatch menu for Projectile commands.

The switches in the Modifiers group tweak how the commands below run:
`--invalidate-cache' rebuilds the file cache first (file/dir commands),
`--regexp' searches for a regexp (the ag/ripgrep search and the
reviewable search/replace), `--case-sensitive' seeds the reviewable
search/replace case-sensitive, `--word' makes it match whole words,
`--new-process' starts a fresh process
(shells), and `--display' opens the result in another window or frame
(file/buffer/project commands)."
    ["Modifiers"
     ("-i" "invalidate cache" "--invalidate-cache")
     ("-r" "regexp search" "--regexp")
     ("-c" "case-sensitive search" "--case-sensitive")
     ("-w" "whole-word search" "--word")
     ("-n" "new process" "--new-process")
     ("-d" "display in" "--display="
      :class transient-switches
      :argument-format "--display=%s"
      :argument-regexp "\\(--display=\\(window\\|frame\\)\\)"
      :choices ("window" "frame"))]
    [["Find"
      ("f" "file" projectile-dispatch-find-file)
      ("g" "file dwim" projectile-dispatch-find-file-dwim)
      ("a" "other file" projectile-dispatch-find-other-file)
      ("l" "file in dir" projectile-find-file-in-directory)
      ("F" "file in known projects" projectile-find-file-in-known-projects)
      ("d" "dir" projectile-dispatch-find-dir)
      ("D" "dired" projectile-dispatch-dired)
      ("e" "recentf" projectile-recentf)
      ("E" "edit .dir-locals" projectile-edit-dir-locals)
      ("T" "test file" projectile-dispatch-find-test-file)
      ("t" "toggle impl/test" projectile-dispatch-impl-or-test)
      ("j" "file of kind" projectile-dispatch-find-file-of-kind)
      ("J" "toggle related" projectile-toggle-related-file)]
     ["Buffers"
      ("b" "switch buffer" projectile-dispatch-switch-to-buffer)
      ("B" "display buffer" projectile-display-buffer)
      ("I" "ibuffer" projectile-ibuffer)
      ("k" "kill buffers" projectile-kill-buffers)
      ("S" "save buffers" projectile-save-project-buffers)]
     ["Search / Replace"
      ("ss" "search" projectile-dispatch-search)
      ("sg" "grep" projectile-grep)
      ("sr" "ripgrep" projectile-dispatch-ripgrep)
      ("sa" "ag" projectile-dispatch-ag)
      ("sx" "references" projectile-find-references)
      ("sR" "search (review)" projectile-dispatch-search-review)
      ("o" "multi-occur" projectile-multi-occur)
      ("r" "replace" projectile-replace)
      ("R" "replace (review)" projectile-dispatch-replace-review)]]
    [["Project"
      ("p" "switch project" projectile-dispatch-switch-project)
      ("q" "switch open project" projectile-switch-open-project)
      ("A" "add known project" projectile-add-known-project)
      ("v" "vc" projectile-vc)]
     ["Lifecycle"
      ("cc" "compile" projectile-compile-project)
      ("ct" "test" projectile-test-project)
      ("c." "test at point" projectile-run-test-at-point)
      ("cr" "run" projectile-run-project)
      ("co" "configure" projectile-configure-project)
      ("ci" "install" projectile-install-project)
      ("cp" "package" projectile-package-project)
      ("cx" "run task" projectile-run-task)
      ("cX" "repeat last task" projectile-repeat-last-task)]
     ["Shells / Run"
      ("xr" "run" projectile-dispatch-run)
      ("xe" "eshell" projectile-dispatch-run-eshell)
      ("xs" "shell" projectile-dispatch-run-shell)
      ("xt" "term" projectile-dispatch-run-term)
      ("xi" "ielm" projectile-dispatch-run-ielm)
      ("xg" "gdb" projectile-run-gdb)
      ("xv" "vterm" projectile-dispatch-run-vterm)
      ("xx" "eat" projectile-dispatch-run-eat)
      ("xG" "ghostel" projectile-dispatch-run-ghostel)
      ("!" "shell command" projectile-run-shell-command-in-root)
      ("&" "async shell command" projectile-run-async-shell-command-in-root)]
     ["Session"
      ("ws" "save session" projectile-session-save)
      ("wS" "save all sessions" projectile-session-save-all)
      ("wr" "restore session" projectile-session-restore)
      ("wR" "restore all sessions" projectile-session-restore-all)
      ("wf" "forget session" projectile-session-forget)
      ("wb" "switch project buffer" projectile-session-switch-to-buffer)]
     ["Cache"
      ("i" "invalidate cache" projectile-invalidate-cache)
      ("z" "cache current file" projectile-cache-current-file)]]))

(defun projectile-dispatch ()
  "Dispatch menu for Projectile commands.

The switches in the Modifiers group tweak how the commands below run:
`--invalidate-cache' rebuilds the file cache first (file/dir commands),
`--regexp' searches for a regexp (the ag/ripgrep search and the
reviewable search/replace), `--case-sensitive' seeds the reviewable
search/replace case-sensitive, `--word' makes it match whole words,
`--new-process' starts a fresh process
(shells), and `--display' opens the result in another window or frame
(file/buffer/project commands)."
  (interactive)
  ;; Loading `transient' is deferred until the menu is first used; this
  ;; stub is replaced by the real transient prefix on that first call.
  (require 'transient)
  (projectile--dispatch-define)
  (call-interactively 'projectile-dispatch))

;; Mark the stub as a transient prefix so `projectile--transient-command-p'
;; recognizes it before the first invocation replaces the stub (and this
;; property) with the real definition.
(put 'projectile-dispatch 'transient--prefix t)

(defvar projectile-mode-map
  (let ((map (make-sparse-keymap)))
    (when projectile-keymap-prefix
      (define-key map projectile-keymap-prefix 'projectile-command-map))
    (easy-menu-define projectile-mode-menu map
      "Menu for Projectile"
      '("Projectile" :visible projectile-show-menu
        ("Find..."
         ["Find file" projectile-find-file]
         ["Find file (all, ignoring rules)" projectile-find-file-all]
         ["Find file in known projects" projectile-find-file-in-known-projects]
         ["Find test file" projectile-find-test-file]
         ["Find directory" projectile-find-dir]
         ["Find file in directory" projectile-find-file-in-directory]
         ["Find other file" projectile-find-other-file]
         ["Find file of kind" projectile-find-file-of-kind]
         ["Jump between implementation file and test file" projectile-toggle-between-implementation-and-test]
         ["Toggle between related files" projectile-toggle-related-file])
        ("Buffers"
         ["Switch to buffer" projectile-switch-to-buffer]
         ["Kill project buffers" projectile-kill-buffers]
         ["Save project buffers" projectile-save-project-buffers]
         ["Recent files" projectile-recentf]
         ["Previous buffer" projectile-previous-project-buffer]
         ["Next buffer" projectile-next-project-buffer])
        ("Projects"
         ["Add known project" projectile-add-known-project]
         ["Add and switch to project" projectile-add-and-switch-project]
         "--"
         ["Switch to project" projectile-switch-project]
         ["Switch to open project" projectile-switch-open-project]
         "--"
         ["Discover projects in directory" projectile-discover-projects-in-directory]
         ["Discover projects in search path" projectile-discover-projects-in-search-path]
         ["Clear known projects" projectile-clear-known-projects]
         ["Reset known projects" projectile-reset-known-projects]
         "--"
         ["Open project in dired" projectile-dired]
         "--"
         "--"
         ["Cache current file" projectile-cache-current-file]
         ["Invalidate cache" projectile-invalidate-cache]
         ["Invalidate all project caches" projectile-invalidate-cache-all]
         ["Discard project root cache" projectile-discard-root-cache]
         "--"
         ["Toggle project wide read-only" projectile-toggle-project-read-only]
         ["Edit .dir-locals.el" projectile-edit-dir-locals]
         ["Project info" projectile-project-info])
        ("Search"
         ["Search (default backend)" projectile-search]
         ["Search with grep" projectile-grep]
         ["Search with ripgrep" projectile-ripgrep]
         ["Search with ag" projectile-ag]
         ["Replace in project" projectile-replace]
         ["Replace in project (review)" projectile-replace-review]
         ["Replace regexp in project (review)" projectile-replace-regexp-review]
         ["Multi-occur in project" projectile-multi-occur]
         ["Find references in project" projectile-find-references])
        ("Run..."
         ["Run (default backend)" projectile-run]
         "--"
         ["Run shell" projectile-run-shell]
         ["Run eshell" projectile-run-eshell]
         ["Run ielm" projectile-run-ielm]
         ["Run term" projectile-run-term]
         ["Run vterm" projectile-run-vterm]
         ["Run eat" projectile-run-eat]
         ["Run ghostel" projectile-run-ghostel]
         "--"
         ["Run GDB" projectile-run-gdb])
        ("Build"
         ["Configure project" projectile-configure-project]
         ["Compile project" projectile-compile-project]
         ["Test project" projectile-test-project]
         ["Run test at point" projectile-run-test-at-point]
         ["Install project" projectile-install-project]
         ["Package project" projectile-package-project]
         ["Run project" projectile-run-project]
         "--"
         ["Run task" projectile-run-task]
         ["Repeat last task" projectile-repeat-last-task]
         "--"
         ["Repeat last build command" projectile-repeat-last-command])
        ("Session"
         ["Save session" projectile-session-save]
         ["Save all sessions" projectile-session-save-all]
         ["Restore session" projectile-session-restore]
         ["Restore all sessions" projectile-session-restore-all]
         ["Forget session" projectile-session-forget]
         "--"
         ["Switch to project buffer" projectile-session-switch-to-buffer])
        "--"
        ["About" projectile-version]))
    map)
  "Keymap for Projectile mode.")

(defun projectile-find-file-hook-function ()
  "Called by `find-file-hook' when `projectile-mode' is on.

For remote (TRAMP) buffers the slow operations are skipped: the
mode-line update probes many project-type markers on cold cache.  The
cheap operations - caching the visited file, registering the project as
a known project, and the open-buffer-count cap - run regardless of
remoteness; they were previously skipped only because the original
blanket guard was overly broad."
  (let ((remote (file-remote-p default-directory))
        ;; Resolve the project root once and thread it through the
        ;; sub-hooks, so a single `find-file' doesn't repeat the lookup.
        (project-root (projectile-project-p)))
    (projectile-maybe-limit-project-file-buffers project-root)
    (when projectile-auto-update-cache
      (projectile-cache-files-find-file-hook project-root))
    (projectile-track-known-projects-find-file-hook project-root)
    (projectile--maybe-run-project-changed-functions project-root)
    (projectile--frecency-record project-root)
    (unless remote
      (when projectile-dynamic-mode-line
        (projectile-update-mode-line)))))

(defun projectile-maybe-limit-project-file-buffers (&optional project-root)
  "Limit the opened file buffers for a project.

The function simply kills the last buffer, as it's normally called
when opening new files.  PROJECT-ROOT defaults to the current project."
  (when projectile-max-file-buffer-count
    (let ((project-buffers (projectile-project-buffer-files project-root)))
      (when (length> project-buffers projectile-max-file-buffer-count)
        (kill-buffer (car (last project-buffers)))))))

;;;; project.el integration
;;
;; Projectile will become the default provider for
;; project.el project and project files lookup when
;; projectile-mode is enabled.
;;
;; The integration can also be manually enabled like this:
;;
;; (add-hook 'project-find-functions #'project-projectile)
;;
;; See https://github.com/bbatsov/projectile/issues/1591 for
;; more details.

;; it's safe to require this directly, as it was added in Emacs 25.1
(require 'project)

;; Only define an override for project-root if the method exists.  For versions
;; before emacs 28, project.el provided project-roots instead of project-root.
(if (fboundp 'project-root)
    (cl-defmethod project-root ((project (head projectile)))
      (cdr project)))

(cl-defmethod project-files ((project (head projectile)) &optional _dirs)
  (let ((root (project-root project)))
    ;; Make paths absolute and ignore the optional dirs argument,
    ;; see https://github.com/bbatsov/projectile/issues/1591#issuecomment-896423965
    ;; That's needed because Projectile uses relative paths for project files
    ;; and project.el expects them to be absolute.
    ;; FIXME: That's probably going to be very slow in large projects.
    (mapcar (lambda (f)
              (concat root f))
            (projectile-project-files root))))

(cl-defmethod project-name ((project (head projectile)))
  (projectile-project-name (cdr project)))

(cl-defmethod project-buffers ((project (head projectile)))
  (projectile-project-buffers (cdr project)))

(cl-defmethod project-ignores ((project (head projectile)) _dir)
  "Return a list of glob patterns to ignore in PROJECT.

The patterns are derived from Projectile's ignore configuration and
returned in the format expected by project.el (see `project-ignores'):
globally ignored directory names and file suffixes are matched at any
depth, while the files and directories ignored via the project's
dirconfig (`.projectile') are rooted at the project root with a
leading `./'."
  ;; PROJECT is Projectile's own `(projectile . root)' representation, so read
  ;; the root straight from the cdr rather than going through `project-root'.
  (projectile--project-ignore-globs (cdr project)))

;;;###autoload
(defun project-projectile (dir)
  "Return Projectile project of form ('projectile . root-dir) for DIR."
  (let ((root (projectile-project-root dir)))
    (when root
      (cons 'projectile root))))

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
    (add-hook 'project-find-functions #'project-projectile)
    (add-hook 'find-file-hook 'projectile-find-file-hook-function)
    (add-hook 'projectile-find-dir-hook #'projectile-track-known-projects-find-file-hook t)
    (add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook t)
    (add-hook 'dired-before-readin-hook #'projectile--maybe-run-project-changed-functions t)
    (add-hook 'kill-emacs-hook #'projectile--frecency-save)
    (when projectile-dynamic-mode-line
      (add-hook 'window-configuration-change-hook #'projectile-update-mode-line-on-window-change))
    (add-hook 'kill-emacs-hook #'projectile--teardown-all-watches)
    ;; Disabling the mode tears the watches down, so re-enabling it has to
    ;; re-arm them - a warm cache would otherwise never trigger a cache fill.
    (projectile--watch-all-cached-projects)
    (advice-add 'compilation-find-file :around #'compilation-find-file-projectile-find-compilation-buffer)
    (advice-add 'delete-file :before #'delete-file-projectile-remove-from-cache))
   (t
    (remove-hook 'project-find-functions #'project-projectile)
    (remove-hook 'find-file-hook #'projectile-find-file-hook-function)
    (remove-hook 'projectile-find-dir-hook #'projectile-track-known-projects-find-file-hook)
    (remove-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook)
    (remove-hook 'dired-before-readin-hook #'projectile--maybe-run-project-changed-functions)
    (projectile--frecency-save)
    (remove-hook 'kill-emacs-hook #'projectile--frecency-save)
    (remove-hook 'window-configuration-change-hook #'projectile-update-mode-line-on-window-change)
    (remove-hook 'kill-emacs-hook #'projectile--teardown-all-watches)
    (projectile--teardown-all-watches)
    ;; Forget the last-seen project so re-enabling the mode fires
    ;; `projectile-project-changed-functions' on the first re-entry.
    (setq projectile--current-project nil)
    (advice-remove 'compilation-find-file #'compilation-find-file-projectile-find-compilation-buffer)
    (advice-remove 'delete-file #'delete-file-projectile-remove-from-cache))))

;;; savehist-mode - When `savehist-mode' is t, projectile-project-command-history will be saved.
;; See https://github.com/bbatsov/projectile/issues/1637 for more details
(defvar savehist-additional-variables nil)

(defun projectile--register-savehist-variables ()
  "Add Projectile's persistable history variables to savehist."
  (add-to-list 'savehist-additional-variables 'projectile-project-command-history)
  ;; So `projectile-repeat-last-task' survives restarts, like
  ;; `projectile-repeat-last-command' does via the command history.
  (add-to-list 'savehist-additional-variables 'projectile-last-task-map))

(if (bound-and-true-p savehist-loaded)
    (projectile--register-savehist-variables)
  (add-hook 'savehist-mode-hook #'projectile--register-savehist-variables))

;;; Per-project sessions
;;
;; `projectile-session-mode' gives every project its own `tab-bar' tab.
;; Each project tab is a native tab-bar tab, so it keeps its own window
;; layout for free, and is bound to a project by stamping the project
;; root onto a tab parameter (`projectile-root').  Switching to a project
;; selects its existing tab (restoring that project's layout) when one is
;; open, or otherwise opens a fresh, project-named tab and populates it.
;;
;; This milestone only deals with live, in-session tabs; persisting the
;; tabs to disk and restoring them across restarts is planned for a
;; follow-up, hence the deliberately storage-neutral naming.

;; `tab-bar' is built in since Emacs 27.1, comfortably below Projectile's
;; floor, so it's always available.
(require 'tab-bar)

(defcustom projectile-session-default-action 'projectile-find-file
  "Action used to populate a project's freshly created tab.
Called with no arguments by `projectile-session-switch-project-action'
when a project is switched to for the first time (and thus gets a new
tab).  Any command that takes no arguments will do."
  :group 'projectile
  :type 'function
  :package-version '(projectile . "3.2.0"))

(defcustom projectile-session-tab-name-function 'projectile-session-default-tab-name
  "Function computing the tab name for a project.
It is called with the project root and must return a string.  The
default, `projectile-session-default-tab-name', names the tab after the
project and disambiguates same-named projects with a parent-directory
component."
  :group 'projectile
  :type 'function
  :package-version '(projectile . "3.2.0"))

(defcustom projectile-session-directory
  (expand-file-name "projectile-sessions/" user-emacs-directory)
  "Directory under which per-project session files are stored.
Each project's saved layout and buffers live in a single file here, named
after the project (see `projectile-session--file')."
  :group 'projectile
  :type 'directory
  :package-version '(projectile . "3.2.0"))

(defcustom projectile-session-restore-on-switch t
  "Whether switching to a project restores its saved session.
When non-nil and the project being switched to has no open tab but does
have a session saved on disk, `projectile-session-switch-project-action'
restores that session (recreating its buffers and layout) instead of
running `projectile-session-default-action'."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "3.2.0"))

(defcustom projectile-session-restore-on-startup nil
  "Whether to reopen every saved project session when Emacs starts.
When non-nil and `projectile-session-mode' is enabled, a handler added to
`emacs-startup-hook' runs `projectile-session-restore-all' once, reopening
each saved project into its own tab after your init files have finished
loading.  Because that hook is installed on mode enable and
`emacs-startup-hook' fires only once, right after startup, enabling the
mode *after* Emacs has finished starting never triggers a restore."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "3.2.0"))

(defcustom projectile-session-autosave nil
  "Whether `projectile-session-mode' saves sessions automatically.
When non-nil, the outgoing project's session is saved when you switch
away from it, and every open project's session is saved when Emacs exits.
Degenerate layouts with no serializable buffer are skipped."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "3.2.0"))

(defcustom projectile-session-buffer-serializers
  '((dired-mode
     . (projectile-session--serialize-dired
        . projectile-session--deserialize-dired))
    (t
     . (projectile-session--serialize-file
        . projectile-session--deserialize-file)))
  "How buffers are turned into readable records and back.

An alist whose entries have the shape (KEY SERIALIZE . DESERIALIZE),
i.e. KEY mapped to a (SERIALIZE . DESERIALIZE) pair:

KEY selects which buffers an entry handles.  It is one of:
- a major-mode symbol - matches buffers whose mode is (derived from) it;
- the symbol t - matches any buffer visiting a file (keep it last, so
  mode-specific handlers win);
- a predicate function of one argument (the buffer).

SERIALIZE is called with the buffer current and returns a readable record
\(any `read'-able sexp), or nil to decline the buffer.  DESERIALIZE is
called with such a record and must recreate and return the live buffer,
or nil when it cannot (e.g. the file is gone), in which case the buffer's
window is dropped on restore rather than erroring.

The first entry whose KEY matches and whose SERIALIZE returns non-nil wins.
Buffers no entry handles are skipped, not saved.

Handlers keyed by a major-mode symbol or by t round-trip cleanly: restore
dispatches on that key.  A record produced by a predicate-keyed handler is
stored under the buffer's major mode and, on restore, is handled by the
first predicate-keyed entry that has a DESERIALIZE; so if you register
several predicate handlers, give them distinct major-mode keys instead when
they must restore differently.  To persist e.g. Magit or eshell buffers,
add an entry keyed by their major mode, for instance:

  (add-to-list \\='projectile-session-buffer-serializers
               \\='(magit-status-mode
                 . (my-serialize-magit . my-deserialize-magit)))"
  :group 'projectile
  :type '(alist :key-type sexp :value-type sexp)
  :package-version '(projectile . "3.2.0"))

(defconst projectile-session--format-version 1
  "Format version stamped into session files written on disk.
Session files whose version does not match are ignored on restore.")

(defvar projectile-session--saved-switch-action nil
  "The `projectile-switch-project-action' saved when the mode was enabled.
Restored when `projectile-session-mode' is disabled.")

(defvar projectile-session--closing-tab nil
  "A project tab currently being closed, or nil.
Bound while re-simplifying survivor names from
`projectile-session--on-tab-close' (the pre-close hook, where the closing
tab is still present in the tab list) so `projectile-session--project-tabs'
omits it and its name no longer counts as a clash.")

(declare-function dired-noselect "dired")

(defun projectile-session--current-tab ()
  "Return the current tab of the selected frame."
  (assq 'current-tab (tab-bar-tabs)))

(defun projectile-session--tab-root (tab)
  "Return the project root stamped on TAB, or nil when it holds no project."
  (alist-get 'projectile-root (cdr tab)))

(defun projectile-session--set-tab-root (tab root)
  "Stamp TAB with project ROOT."
  (setf (alist-get 'projectile-root (cdr tab)) root))

(defun projectile-session--set-tab-name (tab name)
  "Give TAB the explicit NAME.
`explicit-name' is set so `tab-bar' doesn't overwrite NAME with its own
automatic naming.  NAME is also recorded in the `projectile-auto-name'
tab parameter so `projectile-session--refresh-tab-names' can tell a name
Projectile assigned from one the user set with `tab-bar-rename-tab'."
  (setf (alist-get 'name (cdr tab)) name)
  (setf (alist-get 'explicit-name (cdr tab)) t)
  (setf (alist-get 'projectile-auto-name (cdr tab)) name))

(defun projectile-session--same-root-p (a b)
  "Return non-nil when project roots A and B denote the same directory.
Identical paths match directly.  Otherwise local paths are compared with
`file-equal-p' (so symlinks and abbreviations still match); remote paths
never reach `file-equal-p', so an unconnected host doesn't trigger a
TRAMP round-trip."
  (and a b
       (let ((a (file-name-as-directory a))
             (b (file-name-as-directory b)))
         (or (string-equal a b)
             (and (not (or (file-remote-p a) (file-remote-p b)))
                  (file-equal-p a b))))))

(defun projectile-session--project-tabs ()
  "Return the open tabs that are bound to a project.
The tab held in `projectile-session--closing-tab' (one being closed) is
omitted, so survivor names recomputed from the pre-close hook don't still
treat the closing tab as an open clash."
  (seq-filter (lambda (tab)
                (and (not (eq tab projectile-session--closing-tab))
                     (projectile-session--tab-root tab)))
              (tab-bar-tabs)))

(defun projectile-session--project-tab (root)
  "Return the open tab bound to project ROOT, or nil when there is none."
  (seq-find (lambda (tab)
              (projectile-session--same-root-p
               (projectile-session--tab-root tab) root))
            (tab-bar-tabs)))

(defun projectile-session--project-name (root)
  "Return the project name for ROOT.
Unlike `projectile-project-name', the name is always derived from ROOT
via `projectile-project-name-function', so it stays correct while the
dynamic `projectile-project-name' is bound during a project switch."
  (funcall projectile-project-name-function root))

(defun projectile-session--parent-components (root)
  "Return ROOT's ancestor directory names, nearest parent first."
  (let ((components (split-string (directory-file-name (expand-file-name root))
                                  "/" t)))
    ;; drop ROOT's own final component; reverse so the nearest parent leads
    (reverse (butlast components))))

(defun projectile-session--name-with-parents (root name depth)
  "Return NAME prefixed with ROOT's DEPTH nearest parent directories.
With DEPTH 0 the plain NAME is returned; with DEPTH 1 the immediate
parent is prepended (e.g. \"shared/foo\"), and so on, in path order."
  (let ((prefix (reverse (seq-take (projectile-session--parent-components root)
                                   depth))))
    (if prefix
        (concat (string-join prefix "/") "/" name)
      name)))

(defun projectile-session-default-tab-name (root)
  "Return the tab name for the project rooted at ROOT.
Use the project's name, prepending as many parent-directory components as
it takes to stay distinct from every other open project tab that shares
the name, so same-named checkouts (even ones whose immediate parent also
matches) remain distinguishable."
  (let* ((name (projectile-session--project-name root))
         ;; roots of the other open project tabs that share this name;
         ;; `delq'/`mapcar' rather than `seq-keep', which is Emacs 29.1+
         (clashers (delq nil
                         (mapcar
                          (lambda (tab)
                            (let ((other (projectile-session--tab-root tab)))
                              (and (not (projectile-session--same-root-p other root))
                                   (equal (projectile-session--project-name other) name)
                                   other)))
                          (projectile-session--project-tabs)))))
    (if (null clashers)
        name
      (let ((max-depth (length (projectile-session--parent-components root)))
            (depth 1))
        (while (and (< depth max-depth)
                    (let ((candidate (projectile-session--name-with-parents
                                      root name depth)))
                      (seq-some
                       (lambda (other)
                         (equal candidate (projectile-session--name-with-parents
                                           other name depth)))
                       clashers)))
          (setq depth (1+ depth)))
        (projectile-session--name-with-parents root name depth)))))

(defun projectile-session--refresh-tab-names ()
  "Recompute and apply names for every open project tab.
Naming every project tab (not just the newest) lets same-named projects
become disambiguated the moment a clash appears.  Tabs the user renamed
by hand are left alone: a tab is only renamed while its current name
still matches the one Projectile last assigned it (its `projectile-auto-name'
parameter), which a manual `tab-bar-rename-tab' breaks."
  (dolist (tab (projectile-session--project-tabs))
    (let ((auto (alist-get 'projectile-auto-name (cdr tab)))
          (current (alist-get 'name (cdr tab))))
      (when (or (null auto) (equal auto current))
        (projectile-session--set-tab-name
         tab (funcall projectile-session-tab-name-function
                      (projectile-session--tab-root tab))))))
  (force-mode-line-update t))

(defun projectile-session--on-tab-close (tab &optional _last)
  "Re-simplify survivor tab names after project TAB is closed.
Wired onto `tab-bar-tab-pre-close-functions', so a project tab that was
disambiguated only because of TAB (e.g. \"work/foo\" beside TAB's
\"home/foo\") reverts to its plain name once TAB goes away.

That hook fires while TAB is still in the tab list, so TAB is bound as
`projectile-session--closing-tab' to exclude it from the recomputation.
It is used rather than a post-close hook because Emacs has no post-close
tab hook at Projectile's 28.1 floor (`tab-bar-tab-pre-close-functions'
dates to 27.1; `tab-bar-tab-post-close-functions' does not exist), which
keeps this 28.1-safe."
  (when (projectile-session--tab-root tab)
    (let ((projectile-session--closing-tab tab))
      ;; never let a naming error (e.g. a custom `projectile-session-tab-name-function'
      ;; that signals) escape this pre-close hook and abort the tab close
      (ignore-errors
        (projectile-session--refresh-tab-names)))))

(defun projectile-session--make-project-tab (root)
  "Create and select a fresh tab bound to project ROOT.
The new tab is stamped with ROOT and named; populating it is left to the
caller."
  (tab-bar-new-tab)
  (projectile-session--set-tab-root (projectile-session--current-tab) root)
  (projectile-session--refresh-tab-names))

(defun projectile-session--current-tab-index ()
  "Return the 1-based index of the selected frame's current tab."
  (1+ (or (cl-position 'current-tab (tab-bar-tabs) :key #'car :test #'eq) 0)))

(defun projectile-session--select-tab-by-root (root)
  "Select the open project tab bound to ROOT, if any.
Returns non-nil when a tab was selected.  Resolves the tab to a 1-based
index in a single pass and selects by index, so it is robust to
`tab-bar-select-tab' rebuilding tab cons cells as it switches away from
the current tab (a captured cons would go stale mid-loop)."
  (let ((index 0) (target nil))
    (dolist (tab (tab-bar-tabs))
      (setq index (1+ index))
      (when (and (not target)
                 (projectile-session--same-root-p
                  (projectile-session--tab-root tab) root))
        (setq target index)))
    (when target
      (tab-bar-select-tab target)
      t)))

(defun projectile-session--select-tab (tab)
  "Select TAB, restoring the project layout it holds.
Selection goes through the tab's project root, so it stays correct even
after other tabs' cons cells have been rebuilt."
  (let ((root (projectile-session--tab-root tab)))
    (when root
      (projectile-session--select-tab-by-root root))))

(defun projectile-session--adopt-current-tab ()
  "Bind the current tab to the current project, when there is one.
Called on mode enable so the tab you're already sitting on is adopted
rather than left unowned."
  (when-let* ((root (projectile-project-root))
              (tab (projectile-session--current-tab)))
    (unless (projectile-session--tab-root tab)
      (projectile-session--set-tab-root tab root)
      (projectile-session--refresh-tab-names))))

(defun projectile-session-switch-project-action ()
  "Tab-aware switch action installed by `projectile-session-mode'.
When the target project already has a tab, select it and restore its
live window layout instead of re-running the switch action.  Otherwise
create a new tab for the project and either restore its saved session (see
`projectile-session-restore-on-switch') or populate it by calling
`projectile-session-default-action'."
  (let* ((root (projectile-project-root))
         (tab (and root (projectile-session--project-tab root))))
    (cond
     (tab (projectile-session--select-tab tab))
     (root
      (projectile-session--make-project-tab root)
      ;; Bind `default-directory' so the populate action lists the new
      ;; project regardless of what buffer `tab-bar-new-tab' left current
      ;; (e.g. a non-default `tab-bar-new-tab-choice').
      (let ((default-directory root))
        (unless (and projectile-session-restore-on-switch
                     (projectile-session--saved-p root)
                     (projectile-session-restore root))
          (funcall projectile-session-default-action))))
     (t (funcall projectile-session-default-action)))))

;;;###autoload
(defun projectile-session-switch-to-buffer ()
  "Switch to a buffer belonging to the current tab's project.
Complete over just the buffers of the project bound to the current tab.
When the current tab holds no project, fall back to the plain
`switch-to-buffer'."
  (interactive)
  (let ((root (projectile-session--tab-root (projectile-session--current-tab))))
    (if root
        (switch-to-buffer
         (projectile-completing-read
          "Switch to project buffer: "
          (mapcar #'buffer-name (projectile-project-buffers root))))
      (call-interactively #'switch-to-buffer))))

;;; Session persistence
;;
;; A project's live layout is a native tab, but tabs don't survive an Emacs
;; restart.  To persist one we write a small readable sexp per project: the
;; window layout as `window-state-get' with the WRITABLE flag (so it stays a
;; plain, re-readable sexp) plus a manifest of the buffers it shows, each
;; turned into a record by `projectile-session-buffer-serializers'.  Restoring
;; recreates the buffers first (window-state only references them by name and
;; won't recreate them) and then puts the layout back, replacing any buffer it
;; couldn't recreate with a placeholder so the restore never errors.

(defun projectile-session--buffer-matches-p (key buffer)
  "Return non-nil when serializer KEY applies to BUFFER.
KEY is a major-mode symbol, the symbol t (any file-visiting buffer), or a
predicate function of one argument."
  (cond
   ((eq key t) (and (buffer-file-name buffer) t))
   ((symbolp key) (with-current-buffer buffer (derived-mode-p key)))
   (t (funcall key buffer))))

(defun projectile-session--buffer-kind (key buffer)
  "Return the symbol under which BUFFER's record is stored for serializer KEY.
A symbol KEY (a mode or t) is its own kind and restore dispatches on it; a
predicate KEY falls back to BUFFER's major mode."
  (if (symbolp key)
      key
    (buffer-local-value 'major-mode buffer)))

(defun projectile-session--readable-p (object)
  "Return non-nil when OBJECT survives a `prin1'/`read' round-trip.
Guards against a custom serializer returning a record that embeds a live
object (buffer, marker, window) which can't be read back.  Binds
`print-circle' so shared or circular structure prints, and reads, finitely
rather than hanging."
  (ignore-errors
    (let ((print-circle t))
      (read (prin1-to-string object))
      t)))

(defun projectile-session--serialize-buffer (buffer)
  "Serialize BUFFER via the first matching entry of the serializer registry.
Return a cons (KIND . RECORD), or nil when no entry handles BUFFER.
Each registry entry is tried in isolation: a matcher or serializer that
errors, or that yields a non-readable record, is skipped rather than
aborting the whole session save."
  (catch 'done
    (dolist (entry projectile-session-buffer-serializers)
      (ignore-errors
        (let ((key (car entry))
              (serialize (cadr entry)))
          (when (projectile-session--buffer-matches-p key buffer)
            (let ((record (with-current-buffer buffer (funcall serialize buffer))))
              (when (and record (projectile-session--readable-p record))
                (throw 'done
                       (cons (projectile-session--buffer-kind key buffer)
                             record))))))))
    nil))

(defun projectile-session--deserializer (kind)
  "Return the deserialize function able to restore a record of KIND.
Prefer an entry whose key is exactly KIND (a mode symbol or t).  Records
produced by a predicate-keyed serializer are stored under the buffer's
major mode, which no `assq' can match, so fall back to the first
predicate-keyed entry that carries a deserializer."
  (let ((exact (assq kind projectile-session-buffer-serializers)))
    (if exact
        (cddr exact)
      (catch 'found
        (dolist (entry projectile-session-buffer-serializers)
          (when (and (functionp (car entry)) (cddr entry))
            (throw 'found (cddr entry))))
        nil))))

(defun projectile-session--recreate-buffer (saved)
  "Recreate the buffer described by SAVED, a (KIND . RECORD) cons.
Return the live buffer, or nil when no deserializer handles KIND or the
deserializer declines (e.g. the underlying file is gone)."
  (let ((deserialize (projectile-session--deserializer (car saved))))
    (when deserialize
      (funcall deserialize (cdr saved)))))

(defun projectile-session--serialize-file (buffer)
  "Serialize file-visiting BUFFER as a (:file PATH :point N) record."
  (when (buffer-file-name buffer)
    (list :file (buffer-file-name buffer) :point (point))))

(defun projectile-session--deserialize-file (record)
  "Recreate the file buffer described by RECORD, or nil when the file is gone.
The file is visited non-interactively - large-file warnings and unsafe
file-local-variable prompts are suppressed - so restoring a session (in
particular on startup) never blocks Emacs on a `yes-or-no-p'."
  (let ((file (plist-get record :file)))
    (when (and file (file-exists-p file))
      (let* ((large-file-warning-threshold nil)
             (enable-local-variables :safe)
             (buffer (find-file-noselect file)))
        (when (buffer-live-p buffer)
          (let ((point (plist-get record :point)))
            (when (integerp point)
              (with-current-buffer buffer
                (goto-char (min point (point-max))))))
          buffer)))))

(defun projectile-session--serialize-dired (buffer)
  "Serialize dired BUFFER as a (:dir DIRECTORY) record."
  (with-current-buffer buffer
    (when (derived-mode-p 'dired-mode)
      (list :dir (expand-file-name default-directory)))))

(defun projectile-session--deserialize-dired (record)
  "Recreate the dired buffer described by RECORD, or nil when it's gone."
  (let ((dir (plist-get record :dir)))
    (when (and dir (file-directory-p dir))
      (dired-noselect dir))))

(defun projectile-session--placeholder-buffer ()
  "Return a live placeholder buffer for windows whose buffer is missing."
  (get-buffer-create " *projectile-session-placeholder*"))

(defun projectile-session--sanitize-window-state (state)
  "Return a copy of window STATE with missing buffers replaced.
Any window referencing a buffer that isn't live is pointed at a
placeholder buffer instead, so `window-state-put' can't error out.  This
is the Emacs 28 substitute for `window-restore-killed-buffer-windows'
\(added in Emacs 30)."
  (cond
   ((and (consp state)
         (eq (car state) 'buffer)
         (stringp (cadr state))
         (not (get-buffer (cadr state))))
    (cons 'buffer
          (cons (buffer-name (projectile-session--placeholder-buffer))
                (cddr state))))
   ((consp state)
    (cons (projectile-session--sanitize-window-state (car state))
          (projectile-session--sanitize-window-state (cdr state))))
   (t state)))

(defun projectile-session--file (root)
  "Return the absolute session file name for project ROOT.
The name pairs a readable, filesystem-safe project name with a hash of
ROOT's canonical path, so distinct roots never collide and the same root
maps to the same file across restarts."
  (let* ((canonical (directory-file-name
                     ;; Canonicalize with `file-truename' so a symlinked root
                     ;; and its target map to the same file, matching M1's
                     ;; `projectile-session--same-root-p'.  Skip it for remote
                     ;; roots to avoid a TRAMP round-trip.
                     (if (file-remote-p root)
                         (expand-file-name root)
                       (file-truename root))))
         (name (projectile-session--project-name root))
         (safe (replace-regexp-in-string "[^A-Za-z0-9_.-]" "_" (or name "project")))
         (hash (md5 canonical)))
    (expand-file-name (concat safe "-" hash ".eld")
                      projectile-session-directory)))

(defun projectile-session--saved-p (root)
  "Return non-nil when project ROOT has a session saved on disk."
  (file-exists-p (projectile-session--file root)))

(defun projectile-session--write (root data)
  "Write session DATA for project ROOT, creating the session directory.
Return non-nil only when the file was actually written, so callers don't
report success for an unwritable session directory.  Binds `print-circle'
so shared or circular structure in a record can't hang the write."
  (let ((file (projectile-session--file root)))
    (ignore-errors (make-directory (file-name-directory file) t))
    (and (file-writable-p file)
         (progn
           (let ((print-circle t))
             (projectile-serialize data file))
           (file-exists-p file)))))

(defun projectile-session--read-file (file)
  "Read and return the session data stored in FILE, or nil.
Data that isn't a well-formed session plist of the current version is
ignored, so an unreadable or stale file is skipped rather than erroring.
A real session file written by an incompatible format version is skipped
with a message, so the user learns why nothing was restored."
  (let ((data (projectile-unserialize file)))
    (cond
     ((and (consp data)
           (equal (plist-get data :projectile-session-version)
                  projectile-session--format-version)
           data))
     ((and (consp data) (plist-member data :projectile-session-version))
      (message "Ignoring session file %s: format version %s (expected %s)"
               file (plist-get data :projectile-session-version)
               projectile-session--format-version)
      nil))))

(defun projectile-session--read (root)
  "Read and return project ROOT's session data, or nil.
Data whose format version doesn't match is ignored."
  (projectile-session--read-file (projectile-session--file root)))

(defun projectile-session--saved-roots ()
  "Return the roots of every project with a session saved on disk.
Scan `projectile-session-directory' for session files, read each (skipping
any that is unreadable or of a mismatched version, via
`projectile-session--read-file'), and collect the `:root' it stores.  The
result is sorted so `projectile-session-restore-all' reopens tabs in a
stable order across calls and restarts."
  (let ((dir projectile-session-directory)
        (roots '()))
    (when (file-directory-p dir)
      (dolist (file (directory-files dir t "\\.eld\\'"))
        (let ((data (ignore-errors (projectile-session--read-file file))))
          ;; require a string root: a corrupt/hand-edited file with a
          ;; non-string `:root' would otherwise crash the `sort' below and
          ;; (via the startup handler's guard) silently disable all restore
          (when-let* ((root (plist-get data :root))
                      ((stringp root)))
            (push root roots)))))
    (sort roots #'string-lessp)))

(defun projectile-session--frame-buffers ()
  "Return the distinct buffers shown in the selected frame's windows."
  (delete-dups (mapcar #'window-buffer (window-list nil 'nomini))))

;;;###autoload
(defun projectile-session-save (&optional project)
  "Save the current window layout and buffers as PROJECT's session.
PROJECT defaults to the current project's root.  The layout is captured
from the selected frame, so this saves whichever project's tab is
current.  Buffers are recorded via `projectile-session-buffer-serializers';
a layout with no serializable buffer is not saved.  Return non-nil on a
successful save."
  (interactive)
  (let ((root (or project (projectile-project-root))))
    (unless root
      (user-error "Not in a project"))
    (let* ((buffers (projectile-session--frame-buffers))
           (records (delq nil (mapcar #'projectile-session--serialize-buffer
                                      buffers))))
      (cond
       ((null records)
        (when (called-interactively-p 'any)
          (message "No serializable buffers to save for %s" root))
        nil)
       ((projectile-session--write
         root
         (list :projectile-session-version projectile-session--format-version
               :root root
               :buffers records
               :window-state (window-state-get (frame-root-window) t)))
        (when (called-interactively-p 'any)
          (message "Saved session for %s" root))
        t)
       (t
        (when (called-interactively-p 'any)
          (message "Could not write session for %s" root))
        nil)))))

;;;###autoload
(defun projectile-session-restore (&optional project)
  "Restore PROJECT's saved session into the selected frame.
PROJECT defaults to the current project's root.  Buffers are recreated
first, then the saved window layout is put back; windows whose buffer
can't be recreated fall back to a placeholder.  Return non-nil when a
session was restored."
  (interactive)
  (let* ((root (or project (projectile-project-root)))
         (data (and root (projectile-session--read root))))
    (cond
     (data
      (let ((recreated nil))
        (dolist (saved (plist-get data :buffers))
          (when (ignore-errors (buffer-live-p (projectile-session--recreate-buffer saved)))
            (setq recreated t)))
        (cond
         (recreated
          (let ((state (plist-get data :window-state)))
            (when state
              ;; A hand-edited or corrupt :window-state that still passes the
              ;; version check shouldn't abort the whole restore - fall through
              ;; to the recreated buffers instead, matching the buffer-recreation
              ;; guard above and the file-read robustness elsewhere here.
              (condition-case err
                  (window-state-put (projectile-session--sanitize-window-state state)
                                    (frame-root-window) 'safe)
                (error
                 (message "projectile-session: could not restore window layout: %S" err)))))
          t)
         ;; Nothing could be recreated (every saved file is gone, say);
         ;; return nil so `restore-on-switch' falls back to populating the
         ;; tab instead of leaving the user in an all-placeholder frame.
         (t
          (when (called-interactively-p 'any)
            (message "No buffers could be restored for %s" (or root "current project")))
          nil))))
     ((called-interactively-p 'any)
      (user-error "No saved session for %s" (or root "current project"))))))

;;;###autoload
(defun projectile-session-forget (&optional project)
  "Delete PROJECT's saved session file.
PROJECT defaults to the current project's root."
  (interactive)
  (let ((root (or project (projectile-project-root))))
    (unless root
      (user-error "Not in a project"))
    (let ((file (projectile-session--file root)))
      (when (file-exists-p file)
        (delete-file file)
        (when (called-interactively-p 'any)
          (message "Forgot session for %s" root))))))

(defun projectile-session--maybe-autosave ()
  "Save the current project's session when autosave is enabled.
Wired onto `projectile-before-switch-project-hook', where the current
project is still the one being switched away from."
  (when projectile-session-autosave
    (ignore-errors (projectile-session-save))))

(defun projectile-session--save-all-tabs ()
  "Save the session of every open project tab, selecting each in turn.
Each project tab is selected so its own window layout is what gets saved,
then the originally-selected tab is restored.  Tabs are re-resolved by
root on each iteration rather than by a cons captured up front, because
`tab-bar-select-tab' rebuilds cons cells as it switches tabs (a stale cons
would save the wrong layout under a project's root).  The per-tab body is
guarded so a tab that can't be selected or saved (its root gone, say)
neither abandons the remaining projects nor lets an error escape (this
also matters on `kill-emacs-hook').  Return the number of tabs whose
session was actually written."
  (let ((origin (projectile-session--current-tab-index))
        (roots (delq nil (mapcar #'projectile-session--tab-root
                                 (projectile-session--project-tabs))))
        (saved 0))
    (dolist (root roots)
      (ignore-errors
        (when (and (projectile-session--select-tab-by-root root)
                   (projectile-session-save root))
          (setq saved (1+ saved)))))
    (ignore-errors (tab-bar-select-tab origin))
    saved))

;;;###autoload
(defun projectile-session-save-all ()
  "Save the session of every open project in one go.
Every open project tab's window layout and buffers are saved (see
`projectile-session-save'); a tab whose layout has no serializable buffer
is skipped.  Unlike autosave, this runs regardless of
`projectile-session-autosave'.  When called interactively, report how many
sessions were saved."
  (interactive)
  ;; `--save-all-tabs' selects each project tab in turn and restores the
  ;; originally-selected one afterwards.
  (let ((saved (projectile-session--save-all-tabs)))
    (when (called-interactively-p 'any)
      (message "Saved %d project session%s" saved (if (= saved 1) "" "s")))
    saved))

;;;###autoload
(defun projectile-session-restore-all ()
  "Reopen every saved project's session into its own tab.
For each project with a session saved on disk (see
`projectile-session--saved-roots'): when a tab for it is already open,
leave it be rather than duplicating it; otherwise create a fresh project
tab and restore the saved session into it.  A session whose files are all
gone recreates nothing, so its just-created empty tab is closed again and
it is not counted, keeping restore-all from littering the frame with empty
tabs.  Tabs are re-resolved by root, never by a cons captured before a
selection, since `tab-bar-select-tab' rebuilds cons cells as it switches.
End on the first successfully restored project's tab (falling back to the
starting tab when nothing was restored) rather than wherever the iteration
left off.  When called interactively, report how many sessions were
restored.  Return that count."
  (interactive)
  (let ((origin (projectile-session--current-tab-index))
        (first-root nil)
        (restored 0))
    (dolist (root (projectile-session--saved-roots))
      (unless (projectile-session--project-tab root)
        (projectile-session--make-project-tab root)
        (if (ignore-errors (projectile-session-restore root))
            (progn
              (setq restored (1+ restored))
              (unless first-root (setq first-root root)))
          ;; nothing recreated (the project's files are gone): drop the
          ;; empty tab `--make-project-tab' just created and selected
          (ignore-errors (tab-bar-close-tab)))))
    ;; Land the user somewhere deterministic.  When we restored something,
    ;; go to the first restored project; otherwise return to where we
    ;; started (any tabs we made for stale sessions were closed again, so
    ;; the starting index still points at the same tab).
    (if first-root
        (projectile-session--select-tab-by-root first-root)
      (ignore-errors (tab-bar-select-tab origin)))
    (when (called-interactively-p 'any)
      (message "Restored %d project session%s" restored
               (if (= restored 1) "" "s")))
    restored))

(defun projectile-session--autosave-on-kill ()
  "Save every open project's session when autosave is enabled.
Wired onto `kill-emacs-hook'; the frame is going away, so the tab left
selected by `projectile-session--save-all-tabs' does not matter."
  (when projectile-session-autosave
    (projectile-session--save-all-tabs)))

(defun projectile-session--maybe-restore-on-startup ()
  "Reopen all saved sessions at startup when configured to.
Wired onto `emacs-startup-hook' while `projectile-session-mode' is on; the
restore is gated on `projectile-session-restore-on-startup'.  Errors are
swallowed so a restore problem can't abort the rest of Emacs startup."
  (when projectile-session-restore-on-startup
    (ignore-errors (projectile-session-restore-all))))

;;;###autoload
(define-minor-mode projectile-session-mode
  "Global minor mode giving each project its own `tab-bar' tab.

When enabled, `tab-bar-mode' is turned on and project switching becomes
tab-aware: switching to a project selects its existing tab, restoring
that project's live window layout, when one is open; otherwise it opens
a fresh tab dedicated to the project and populates it via
`projectile-session-default-action'.  Each project tab is stamped with
its root and named after the project (see
`projectile-session-tab-name-function').

If the mode is enabled from inside a project, the tab you're already on
is adopted so it isn't left unowned.

Disabling the mode restores `projectile-switch-project-action' to the
value it had when the mode was enabled; it leaves `tab-bar-mode' and any
existing tabs untouched."
  :group 'projectile
  :global t
  :require 'projectile
  (cond
   (projectile-session-mode
    (unless (bound-and-true-p tab-bar-mode)
      (tab-bar-mode 1))
    ;; Guard the save so re-enabling an already-on mode (config re-eval, a
    ;; startup hook, `custom-set' plus code) can't capture our own action as
    ;; the "saved" value and lose the user's real one on the next disable.
    (unless (eq projectile-switch-project-action
                #'projectile-session-switch-project-action)
      (setq projectile-session--saved-switch-action projectile-switch-project-action))
    (setq projectile-switch-project-action #'projectile-session-switch-project-action)
    ;; Autosave wiring.  `add-hook' is idempotent for an `eq' function, so a
    ;; double enable can't stack duplicates; the actual saving is gated on
    ;; `projectile-session-autosave' inside the hook functions.
    (add-hook 'projectile-before-switch-project-hook
              #'projectile-session--maybe-autosave)
    (add-hook 'kill-emacs-hook #'projectile-session--autosave-on-kill)
    ;; Restore-on-startup wiring.  The handler is gated on
    ;; `projectile-session-restore-on-startup' and `emacs-startup-hook' fires
    ;; once, right after init, so setting the mode plus the defcustom in init
    ;; restores after startup; enabling the mode later simply never fires it.
    ;; `add-hook' is idempotent for an `eq' function, so a double enable can't
    ;; stack duplicates.
    (add-hook 'emacs-startup-hook #'projectile-session--maybe-restore-on-startup)
    ;; Re-simplify a survivor's name when its same-named sibling tab is
    ;; closed.  `add-hook' is idempotent for an `eq' function, so a double
    ;; enable can't stack duplicates.
    (add-hook 'tab-bar-tab-pre-close-functions
              #'projectile-session--on-tab-close)
    (projectile-session--adopt-current-tab))
   (t
    (when (eq projectile-switch-project-action
              #'projectile-session-switch-project-action)
      (setq projectile-switch-project-action
            projectile-session--saved-switch-action))
    (setq projectile-session--saved-switch-action nil)
    (remove-hook 'projectile-before-switch-project-hook
                 #'projectile-session--maybe-autosave)
    (remove-hook 'kill-emacs-hook #'projectile-session--autosave-on-kill)
    (remove-hook 'emacs-startup-hook
                 #'projectile-session--maybe-restore-on-startup)
    (remove-hook 'tab-bar-tab-pre-close-functions
                 #'projectile-session--on-tab-close))))

(provide 'projectile)

;;; projectile.el ends here
