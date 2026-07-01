;;; projectile.el --- Manage and navigate projects in Emacs easily -*- lexical-binding: t -*-

;; Copyright © 2011-2026 Bozhidar Batsov <bozhidar@batsov.dev>

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/bbatsov/projectile
;; Keywords: project, convenience
;; Version: 2.10.0-snapshot
;; Package-Requires: ((emacs "27.1") (compat "30"))

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
(eval-when-compile
  (require 'find-dired)
  (require 'subr-x))

;;; Declarations
;;
;; A bunch of variable and function declarations
;; needed to appease the byte-compiler.
(defvar ag-ignore-list)
;; Defined by the optional `transient' dependency (see `projectile-dispatch').
(defvar transient-exit-hook)
(defvar transient-current-command)
(declare-function transient-args "transient")
(defvar eshell-buffer-name)
(defvar explicit-shell-file-name)
(defvar grep-files-aliases)
(defvar grep-find-ignored-directories)
(defvar grep-find-ignored-files)
(defvar eat-buffer-name)
(defvar ghostel-buffer-name)

;; `projectile-dispatch' is defined later in this file, but only when the
;; optional `transient' dependency is available, so forward-declare it.
(declare-function projectile-dispatch "projectile")
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
          (const :tag "Alien" alien)))

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
          (const :tag "Persistent" persistent)))

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
  :package-version '(projectile . "2.10.0"))

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
                          (cons :tag "Disjunction" (const or) sexp)))))

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

(defcustom projectile-auto-discover nil
  "Whether to discover projects when project switching commands are invoked.

See also `projectile-project-search-path'."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.3.0"))

(defcustom projectile-auto-cleanup-known-projects nil
  "Whether to cleanup projects when project switching commands are invoked.

See also `projectile-cleanup-known-projects'."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.9.0"))

(defcustom projectile-auto-update-cache t
  "Whether cache is automatically updated when files are opened or deleted."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-require-project-root 'prompt
  "Require the presence of a project root to operate when true.
When set to `prompt' Projectile will ask you to select a project
directory if you're not in a project.

When nil Projectile will consider the current directory the project root."
  :group 'projectile
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (const :tag "Prompt for project" prompt)))

(defcustom projectile-completion-system 'default
  "The completion system to be used by Projectile.
Either `default' (Emacs's built-in `completing-read', which works with
Vertico, Consult, Fido, Ido's `ido-ubiquitous', etc.) or a custom
function accepting a prompt and a list of choices.

Note: the dedicated `ido', `helm' and `ivy' options were removed - those
frameworks are used through `completing-read' (or their own Projectile
integration packages, `helm-projectile' / `counsel-projectile'), so any
of those legacy values now behaves like `default'."
  :group 'projectile
  :type '(choice (const :tag "Default (completing-read)" default)
                 (function :tag "Custom function"))
  :package-version '(projectile . "2.10.0"))

(defcustom projectile-keymap-prefix nil
  "Projectile keymap prefix."
  :group 'projectile
  :type 'string)

(defcustom projectile-cache-file  ".projectile-cache.eld"
  "The name of Projectile's cache.
It's relative to the project root."
  :group 'projectile
  :type 'string)

(defcustom projectile-tags-file-name "TAGS"
  "The name of the tags file Projectile excludes from indexing.
Listed in `projectile-globally-ignored-files' so a generated tags
file doesn't show up among the project files."
  :group 'projectile
  :type 'string)

(defcustom projectile-sort-order 'default
  "The sort order used for a project's files.

Note that files aren't sorted if `projectile-indexing-method'
is set to `alien'."
  :group 'projectile
  :type '(radio
          (const :tag "Default (no sorting)" default)
          (const :tag "Recently opened files" recentf)
          (const :tag "Recently active buffers, then recently opened files" recently-active)
          (const :tag "Access time (atime)" access-time)
          (const :tag "Modification time (mtime)" modification-time)))

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
  :type '(repeat string))

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
  :type '(repeat string))

(defcustom projectile-project-root-files-top-down-recurring
  '(".svn" ; Svn VCS root dir
    "CVS"  ; CVS VCS root dir
    "Makefile")
  "A list of files considered to mark the root of a project.
The search starts at the top and descends down till a directory
that contains a match file but its parent does not.  Thus, it's a
bottommost match in the topmost sequence of directories
containing a root file."
  :group 'projectile
  :type '(repeat string))

(define-obsolete-variable-alias 'projectile-project-root-files-functions 'projectile-project-root-functions "2.4")

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
  :type '(repeat function))

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
  :package-version '(projectile . "2.10.0"))

(defcustom projectile-warn-on-prefixless-dirconfig-lines t
  "Whether to warn about deprecated prefix-less ignore entries.
Lines in `.projectile' that start with no `+'/`-'/`!' prefix are
still accepted as ignore patterns for backward compatibility, but
the implicit form is being phased out.  When this option is
non-nil, a one-time warning is shown per project that uses any
such line, listing the offending entries."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.10.0"))

(defcustom projectile-globally-ignored-files
  (list projectile-tags-file-name projectile-cache-file)
  "A list of files globally ignored by projectile.
Note that files aren't filtered if `projectile-indexing-method'
is set to `alien'."
  :safe (lambda (x) (not (remq t (mapcar #'stringp x))))
  :group 'projectile
  :type '(repeat string))

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
  :type '(repeat string))

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
    ".jj")
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
  :type '(repeat string))

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
  :type '(repeat string))

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

(defcustom projectile-switch-project-other-window-action 'projectile-find-file-other-window
  "Action run by `projectile-switch-project-other-window' after switching.
Like `projectile-switch-project-action', but for the other-window variant.

Any function that does not take arguments will do."
  :group 'projectile
  :type 'function
  :package-version '(projectile . "2.10.0"))

(defcustom projectile-switch-project-other-frame-action 'projectile-find-file-other-frame
  "Action run by `projectile-switch-project-other-frame' after switching.
Like `projectile-switch-project-action', but for the other-frame variant.

Any function that does not take arguments will do."
  :group 'projectile
  :type 'function
  :package-version '(projectile . "2.10.0"))

(defcustom projectile-find-dir-includes-top-level nil
  "If true, add top-level dir to options offered by `projectile-find-dir'."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-use-git-grep nil
  "If true, use `vc-git-grep' in git projects."
  :group 'projectile
  :type 'boolean)

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
  :package-version '(projectile . "2.10.0"))

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
  :package-version '(projectile . "2.10.0"))

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

(defcustom projectile-related-files-fn-function 'projectile-related-files-fn
  "Function to find related files based on PROJECT-TYPE."
  :group 'projectile
  :type 'function)

(defcustom projectile-dynamic-mode-line t
  "If true, update the mode-line dynamically.
The mode-line is updated when files are opened via `find-file-hook'
and when the window configuration changes.

See also `projectile-mode-line-function' and `projectile-update-mode-line'."
  :group 'projectile
  :type 'boolean
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
  :type 'string)

(defcustom projectile-default-test-directory "test/"
  "The default value of a project's test-dir property.

It's used as a fallback in the case the property is not set for a project
type when `projectile-toggle-between-implementation-and-test' is used."
  :group 'projectile
  :type 'string)

(defvar projectile-projects-cache (make-hash-table :test 'equal)
  "A hashmap used to cache project file names to speed up related operations.")

(defvar projectile-projects-cache-time (make-hash-table :test 'equal)
  "A hashmap used to record when we populated `projectile-projects-cache'.")

(defvar projectile--async-index-processes (make-hash-table :test 'equal)
  "Map of project root -> in-flight async indexing process.
Used to avoid running more than one background index for the same
project at a time, and to discard a stale background result whose
project cache was invalidated while it was still running.")

(defvar projectile-project-root-cache (make-hash-table :test 'equal)
  "Cached value of function `projectile-project-root`.")

(defvar projectile-project-type-cache (make-hash-table :test 'equal)
  "A hashmap used to cache project type to speed up related operations.")

(defvar projectile-project-vcs-cache (make-hash-table :test 'equal)
  "Cache of `projectile-project-vcs' results keyed by directory.
Cleared by `projectile-invalidate-cache' and
`projectile-discard-root-cache'.  Entries are VCS symbols (or `none'
for projects with no detected VCS).")

(defvar projectile--dirconfig-cache (make-hash-table :test 'equal)
  "Cache for parsed dirconfig files, keyed by project root.
Each value is a list of (DIRCONFIG-PATH MTIME PARSED-RESULT); a
cache hit requires both DIRCONFIG-PATH and MTIME to match the
current file, so changing `projectile-dirconfig-file' mid-session
naturally invalidates the entry.")

(defvar projectile--pending-cache-flush-timers (make-hash-table :test 'equal)
  "Map of project root to a pending idle-timer that will serialize its cache.
Used by `projectile-cache-current-file' to coalesce rapid file additions
into a single delayed disk write per project.")

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
  :type 'boolean
  :package-version '(projectile . "2.8.0"))

(defcustom projectile-git-command "git ls-files -zco --exclude-standard"
  "Command used by projectile to get the files in a git project."
  :group 'projectile
  :type 'string)

(defcustom projectile-git-fd-args "-H -0 -E .git -tf --strip-cwd-prefix -c never"
  "Arguments to fd used to re-implement `git ls-files'.
This is used with `projectile-fd-executable' when `projectile-git-use-fd'
is non-nil."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "2.8.0"))

(defcustom projectile-git-submodule-command "git submodule --quiet foreach 'echo $displaypath' | tr '\\n' '\\0'"
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

(defcustom projectile-hg-ignored-command "hg status -in0 ."
  "Command used by projectile to get the ignored files in a hg project."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "2.10.0"))

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
  :type 'string)

(defcustom projectile-bzr-command "bzr ls -R --versioned -0"
  "Command used by projectile to get the files in a bazaar project."
  :group 'projectile
  :type 'string)

(defcustom projectile-darcs-command "darcs show files -0 . "
  "Command used by projectile to get the files in a darcs project."
  :group 'projectile
  :type 'string)

(defcustom projectile-pijul-command "pijul list | tr '\\n' '\\0'"
   "Command used by projectile to get the files in a pijul project."
   :group 'projectile
   :type 'string)

(defcustom projectile-svn-command "svn list -R . | grep -v '$/' | tr '\\n' '\\0'"
  "Command used by projectile to get the files in a svn project.

The command runs non-interactively (its output is piped), so `svn'
can't prompt for credentials.  For projects on an authenticated remote
you need to have your credentials cached first (e.g. by running `svn'
once interactively and letting it store them), otherwise the command
fails with an authentication error.  See URL
`https://github.com/bbatsov/projectile/issues/1638'."
  :group 'projectile
  :type 'string)

(defcustom projectile-svn-ignored-command "svn status --no-ignore | grep '^I' | cut -c9- | tr '\\n' '\\0'"
  "Command used by projectile to get the ignored files in a svn project."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "2.10.0"))

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
  :type 'string)

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
  :type 'alist)

(defcustom projectile-create-missing-test-files nil
  "During toggling, if non-nil enables creating test files if not found.

When not-nil, every call to projectile-find-implementation-or-test-*
creates test files if not found on the file system.  Defaults to nil.
It assumes the test/ folder is at the same level as src/."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-per-project-compilation-buffer nil
  "When non-nil, the compilation command makes the per-project compilation buffer."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "2.6.0"))

(defcustom projectile-after-switch-project-hook nil
  "Hooks run right after project is switched."
  :group 'projectile
  :type 'hook)

(defcustom projectile-before-switch-project-hook nil
  "Hooks run right before project is switched."
  :group 'projectile
  :type 'hook)

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
          (const :tag "Keep" keep)))

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

(defconst projectile-version "2.10.0-snapshot"
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
    ;; reset the in-memory cache
    (remhash project-root projectile-project-type-cache)
    (remhash project-root projectile-project-vcs-cache)
    (remhash project-root projectile-projects-cache)
    (remhash project-root projectile-projects-cache-time)
    (remhash project-root projectile--dirconfig-cache)
    ;; Cancel any in-flight background index for this project so its
    ;; now-stale result can't repopulate the cache we just cleared.
    (when-let* ((proc (gethash project-root projectile--async-index-processes)))
      (when (process-live-p proc)
        (delete-process proc))
      (remhash project-root projectile--async-index-processes))
    ;; reset the project's cache file
    (when (projectile-persistent-cache-p)
      (let ((cache-file (projectile-project-cache-file project-root)))
        (when (file-exists-p cache-file)
          (delete-file cache-file))))
    (when projectile-verbose
      (message "Invalidated Projectile cache for %s."
               (propertize project-root 'face 'font-lock-keyword-face))))
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
    (projectile-serialize files (projectile-project-cache-file project))))

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
          (when projectile-verbose
            (message "%s removed from cache" file)))
      (error "%s is not in the cache" file))))

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
      (projectile-serialize new-cache (projectile-project-cache-file project-root)))))

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

;;;###autoload
(defun projectile-cache-current-file ()
  "Add the currently visited file to the cache."
  (interactive)
  (let ((current-project (projectile-project-root)))
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
                                          (projectile-project-cache-file)))
    (projectile-invalidate-cache nil)))

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
              (when (file-directory-p dir)
                (projectile-discover-projects-in-directory dir (1- depth))))
            (progress-reporter-done progress-reporter))
        (when (projectile-project-p directory)
          (let ((dir (abbreviate-file-name (projectile-project-root directory))))
            (unless (member dir projectile-known-projects)
              (projectile-add-known-project dir)))))
    (message "Project search path directory %s doesn't exist" directory)))

;;;###autoload
(defun projectile-discover-projects-in-search-path ()
  "Discover projects in `projectile-project-search-path'.
Invoked automatically when `projectile-mode' is enabled."
  (interactive)
  (dolist (path projectile-project-search-path)
    (if (consp path)
        (projectile-discover-projects-in-directory (car path) (cdr path))
      (projectile-discover-projects-in-directory path 1))))


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
  (projectile-locate-dominating-file
   dir
   (lambda (directory)
     (let ((files (mapcar (lambda (file) (expand-file-name file directory))
                          (or list projectile-project-root-files-bottom-up))))
       (seq-some (lambda (file) (and file (file-exists-p file))) files)))))

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
                                                    :caller 'projectile-read-project))
     (projectile-require-project-root (error "Projectile cannot find a project definition in %s" default-directory))
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
    (error "Directory %S does not exist" directory))
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

(defun projectile--expand-glob-set (patterns)
  "Expand glob PATTERNS in the current `default-directory'.
Return a hash-set of absolute file paths matched by any pattern.
Patterns that don't contain wildcard metacharacters expand to the
file itself if it exists, so a single hash lookup later catches
both glob and literal matches without revisiting the filesystem."
  (let ((set (make-hash-table :test 'equal)))
    (dolist (p patterns set)
      (dolist (f (file-expand-wildcards p t))
        (puthash f t set)))))

(defun projectile--matches-pattern-set-p (file pats glob-set)
  "Return non-nil when FILE matches any of PATS or is in GLOB-SET.
PATS is the original pattern list (for the literal-suffix fallback
that `projectile-check-pattern-p' uses); GLOB-SET is a hash-set
returned by `projectile--expand-glob-set' for the same patterns,
or nil when PATS is empty."
  (or (and glob-set (gethash file glob-set))
      (seq-some (lambda (p)
                  (string-suffix-p (directory-file-name p)
                                   (directory-file-name file)))
                pats)))

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
         (rules (projectile--make-walk-rules ignored-files ignored-directories
                                             globally-ignored-directories))
         ;; A 1-element list whose car is the accumulator.  Using a
         ;; mutable cell lets the recursive walker push results onto a
         ;; single shared list (O(N) total) instead of `apply append'-ing
         ;; per-level results (O(N*depth)).
         (acc-cell (list nil)))
    (projectile--index-directory-walk directory patterns progress-reporter
                                      rules acc-cell)
    (nreverse (car acc-cell))))

(defun projectile--index-directory-walk (directory patterns progress-reporter rules acc-cell)
  "Recursive walker for `projectile-index-directory'.
DIRECTORY, PATTERNS, PROGRESS-REPORTER and RULES carry the same
state as the public entry point.  ACC-CELL is a 1-element list
whose car accumulates discovered file paths in reverse order."
  ;; Resolve the directory listing first.  We rebind `default-directory'
  ;; below for `file-expand-wildcards', so the relative `directory'
  ;; argument has to be resolved against the caller's `default-directory'
  ;; before that rebind takes effect.  Use ignore-errors to skip
  ;; unreadable directories (e.g.  .Spotlight-V100 on macOS) instead of
  ;; aborting the entire indexing operation.
  ;; `directory-files-no-dot-files-regexp' filters out . and .. at the
  ;; C level so we don't have to do it again in the loop.
  ;; `directory-files-and-attributes' (rather than plain `directory-files')
  ;; gives us each entry's type in the same listing call, so we can tell
  ;; files from directories without a `file-directory-p' stat per entry -
  ;; that stat is a separate filesystem round-trip each, which dominates the
  ;; walk on large or remote (TRAMP) trees.
  (let ((entries (ignore-errors
                   (directory-files-and-attributes
                    directory t directory-files-no-dot-files-regexp nil 'integer))))
    (let* ((default-directory (file-name-as-directory directory))
           (ignore-pats (car patterns))
           (ensure-pats (cdr patterns))
           ;; Glob expansion is sensitive to `default-directory' so it
           ;; has to happen at every recursion level - but only once
           ;; per level rather than once per (file, pattern) pair as
           ;; it used to.
           (ignore-glob-set (and ignore-pats (projectile--expand-glob-set ignore-pats)))
           (ensure-glob-set (and ensure-pats (projectile--expand-glob-set ensure-pats))))
      (dolist (entry entries)
        (let* ((f (car entry))
               ;; The type field is t for a directory, a string (the link
               ;; target) for a symlink, and nil for a regular file.  For a
               ;; symlink we still defer to `file-directory-p' so that a link
               ;; pointing at a directory is traversed, matching the previous
               ;; follow-symlink behaviour; that extra stat only happens for
               ;; the rare symlink entry, not for every file.
               (type (file-attribute-type (cdr entry)))
               (local-f (file-name-nondirectory (directory-file-name f))))
          (unless (and patterns
                       (projectile--matches-pattern-set-p f ignore-pats ignore-glob-set)
                       (not (projectile--matches-pattern-set-p f ensure-pats ensure-glob-set)))
            (progress-reporter-update progress-reporter)
            (cond
             ((if (stringp type) (file-directory-p f) (eq type t))
              (unless (projectile--ignored-directory-fast-p
                       (file-name-as-directory f) local-f rules)
                (projectile--index-directory-walk f patterns progress-reporter
                                                  rules acc-cell)))
             (t
              (unless (projectile--ignored-file-fast-p f rules)
                (setcar acc-cell (cons f (car acc-cell))))))))))))

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
  (let ((vcs (projectile-project-vcs path)))
    ;; For Git projects without a `.gitmodules' file there is nothing
    ;; for `git submodule foreach' to find, so we can skip the
    ;; shell-out altogether.  PATH may be inside a Git repo without
    ;; being its toplevel (e.g. a subproject of an outer repo) so look
    ;; for `.gitmodules' along the parent chain rather than just at
    ;; PATH itself.  This is hot for monorepos that index the project
    ;; root often.
    (unless (and (eq vcs 'git)
                 (not (locate-dominating-file path ".gitmodules")))
      (let* ((submodules (mapcar
                          (lambda (s)
                            (file-name-as-directory (expand-file-name s path)))
                          (projectile-files-via-ext-command
                           path (projectile-get-sub-projects-command vcs))))
             (project-child-folder-regex
              (concat "\\`" (regexp-quote path))))
        ;; If project root is inside of an VCS folder, but not
        ;; actually an VCS root itself, submodules external to the
        ;; project will be included in the VCS get sub-projects
        ;; result.  Let's remove them.
        (seq-filter
         (lambda (submodule)
           (string-match-p project-child-folder-regex submodule))
         submodules)))))

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
`:file-handler', which requires Emacs 27.1+)."
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
otherwise operates relative to project root."
  (let* ((ignored-files (projectile-ignored-files-rel))
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
             (seq-some (lambda (suf) (string-suffix-p suf file t)) suffixes)))
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
   :caller 'projectile-read-buffer))

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

;;;###autoload
(defun projectile-switch-to-buffer-other-window ()
  "Switch to a project buffer and show it in another window."
  (interactive)
  (projectile--switch-to-buffer #'switch-to-buffer-other-window))

;;;###autoload
(defun projectile-switch-to-buffer-other-frame ()
  "Switch to a project buffer and show it in another frame."
  (interactive)
  (projectile--switch-to-buffer #'switch-to-buffer-other-frame))

;;;###autoload
(defun projectile-display-buffer ()
  "Display a project buffer in another window without selecting it."
  (interactive)
  (display-buffer
   (projectile-completing-read
    "Display buffer: "
    (projectile-project-buffer-names)
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
    (mapcar (lambda (f) (file-relative-name f project-root)) files)))

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

(defun projectile-check-pattern-p (file pattern)
  "Check if FILE matches globbing PATTERN."
  (or (string-suffix-p (directory-file-name pattern)
                       (directory-file-name file))
      (member file (file-expand-wildcards pattern t))))

(defun projectile-ignored-rel-p (file directory patterns)
  "Check if FILE should be ignored relative to DIRECTORY.
PATTERNS should have the form: (ignored . unignored)"
  (let ((default-directory directory))
    (and (seq-some
          (lambda (pat) (projectile-check-pattern-p file pat))
          (car patterns))
         (not (seq-some
               (lambda (pat) (projectile-check-pattern-p file pat))
               (cdr patterns))))))

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
         (result (if (and cached mtime
                          (equal (nth 0 cached) dirconfig)
                          (equal (nth 1 cached) mtime))
                     (nth 2 cached)
                   (let ((parsed (projectile--parse-dirconfig-file-uncached)))
                     (when mtime
                       (puthash project-root
                                (list dirconfig mtime parsed)
                                projectile--dirconfig-cache))
                     parsed))))
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

(cl-defun projectile-completing-read (prompt choices &key initial-input action caller)
  "Present a project tailored PROMPT with CHOICES.

Reads with `completing-read', unless `projectile-completion-system' is a
function, in which case that function is called with PROMPT and CHOICES.

INITIAL-INPUT is passed to `completing-read'.  ACTION, when non-nil, is
called on the selected candidate and its result returned.  CALLER is
accepted for backward compatibility but no longer used."
  (ignore caller)
  (let* ((prompt (projectile-prepend-project-name prompt))
         (res (if (functionp projectile-completion-system)
                  (funcall projectile-completion-system prompt choices)
                (completing-read
                 prompt
                 (lambda (string pred action)
                   ;; The `project-file' category lets packages like
                   ;; marginalia and embark enhance how candidates are
                   ;; presented.
                   (if (eq action 'metadata)
                       '(metadata . ((category . project-file)))
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
      (error "No other file found"))))


;;; Interactive commands
;;;###autoload
(defun projectile-find-other-file (&optional flex-matching)
  "Switch between files with the same name but different extensions.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable
`projectile-other-file-alist'."
  (interactive "P")
  (projectile--find-other-file flex-matching))

;;;###autoload
(defun projectile-find-other-file-other-window (&optional flex-matching)
  "Switch between files with different extensions in other window.
Switch between files with the same name but different extensions in other
window.  With FLEX-MATCHING, match any file that contains the base name of
current file.  Other file extensions can be customized with the variable
`projectile-other-file-alist'."
  (interactive "P")
  (projectile--find-other-file flex-matching
                               #'find-file-other-window))

;;;###autoload
(defun projectile-find-other-file-other-frame (&optional flex-matching)
  "Switch between files with different extensions in other frame.
Switch between files with the same name but different extensions in other frame.
With FLEX-MATCHING, match any file that contains the base name of current
file.  Other file extensions can be customized with the variable
`projectile-other-file-alist'."
  (interactive "P")
  (projectile--find-other-file flex-matching
                               #'find-file-other-frame))

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
                                  (concat ".*" filename ".*" "\." ext "\\'")
                                (concat "^" filename
                                        (unless (equal ext "")
                                          (concat "\." ext))
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
                                                            (concat "\." (file-name-extension project-file))))))
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
         (file (cond ((= (length files) 1)
                      (car files))
                     ((length> files 1)
                      (projectile-completing-read "Switch to: " files
                                                  :caller 'projectile-read-file))
                     (t
                      (projectile-completing-read "Switch to: " project-files
                                                  :caller 'projectile-read-file))))
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

;;;###autoload
(defun projectile-find-file-dwim-other-window (&optional invalidate-cache)
  "Jump to a project's files using completion based on context in other window.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works
even if the filename is incomplete, but there's only a single file in the
current project that matches the filename at point.  For example, if
there's only a single file named \"projectile/projectile.el\" but the
current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-window' still switches to
\"projectile/projectile.el\" immediately because this is the only filename
that matches.

- If it finds a list of files, the list is displayed for selecting.  A list
of files is displayed when a filename appears more than one in the project
or the filename at point is a prefix of more than two files in a project.
For example, if `projectile-find-file-dwim-other-window' is executed on a
filepath like \"projectile/\", it lists the content of that directory.  If
it is executed on a partial filename like \"projectile/a\", a list of files
with character \"a\" in that directory is presented.

- If it finds nothing, display a list of all files in project for selecting."
  (interactive "P")
  (projectile--find-file-dwim invalidate-cache #'find-file-other-window))

;;;###autoload
(defun projectile-find-file-dwim-other-frame (&optional invalidate-cache)
  "Jump to a project's files using completion based on context in other frame.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works
even if the filename is incomplete, but there's only a single file in the
current project that matches the filename at point.  For example, if
there's only a single file named \"projectile/projectile.el\" but the
current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-frame' still switches to
\"projectile/projectile.el\" immediately because this is the only filename
that matches.

- If it finds a list of files, the list is displayed for selecting.  A list
of files is displayed when a filename appears more than one in the project
or the filename at point is a prefix of more than two files in a project.
For example, if `projectile-find-file-dwim-other-frame' is executed on a
filepath like \"projectile/\", it lists the content of that directory.  If
it is executed on a partial filename like \"projectile/a\", a list of files
with character \"a\" in that directory is presented.

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
  (let* ((project-root (projectile-acquire-root))
         (file (projectile-completing-read "Find file: "
                                           (projectile-project-files project-root)
                                           :caller 'projectile-read-file))
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
    ('access-time (projectile-sort-by-access-time files))))

(defun projectile-sort-by-recentf-first (files)
  "Sort FILES by a recent first scheme."
  (let ((project-recentf-files (projectile-recentf-files)))
    (append project-recentf-files
            (seq-difference files project-recentf-files))))

(defun projectile-sort-by-recently-active-first (files)
  "Sort FILES by most recently active buffers or opened files."
  (let ((project-recently-active-files (projectile-recently-active-files)))
    (append project-recently-active-files
            (seq-difference files project-recently-active-files))))

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

;;;###autoload
(defun projectile-find-dir-other-window (&optional invalidate-cache)
  "Jump to a project's directory in other window using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile--find-dir invalidate-cache #'dired-other-window))

;;;###autoload
(defun projectile-find-dir-other-frame (&optional invalidate-cache)
  "Jump to a project's directory in other frame using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile--find-dir invalidate-cache #'dired-other-frame))

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
      (error "No related files found")))

  (if-let* ((candidates (projectile--related-files file kind)))
      (projectile-expand-root (projectile--choose-from-candidates candidates :caller 'projectile-read-file))
    (error
     "No matching related file as `%s' found for project type `%s'"
     kind (projectile-project-type))))

;;;###autoload
(defun projectile-find-related-file-other-window ()
  "Open related file in other window."
  (interactive)
  (find-file-other-window
   (projectile--find-related-file (buffer-file-name))))

;;;###autoload
(defun projectile-find-related-file-other-frame ()
  "Open related file in other frame."
  (interactive)
  (find-file-other-frame
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
    (marker-files &key project-file compilation-dir configure compile install package test run test-suffix test-prefix src-dir test-dir related-files-fn)
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
    relative path as the input."
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
    project-plist))

(cl-defun projectile-register-project-type
    (project-type marker-files &key project-file compilation-dir configure compile install package test run test-suffix test-prefix src-dir test-dir related-files-fn)
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
    relative path as the input."
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
                                :related-files-fn related-files-fn))
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
     (related-files-fn nil related-files-fn-specified))
    "Update an existing projectile project type.

Passed items will override existing values for the project type given
by PROJECT-TYPE.  nil can be used to remove a project type attribute.  Raise
an error if PROJECT-TYPE is not already registered with projectile.  This
function may also take the keyword argument PRECEDENCE which when set to ‘high’
will make projectile prioritise this project type over other clashing project
types, and a value of ‘low’ will make projectile prefer (all) other project
types by default.  Otherwise, the arguments to this function are as for
`projectile-register-project-type':

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
MARKER-FILES a set of indicator files for PROJECT-TYPE.
PROJECT-FILE the main project file in the root project directory.
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
    relative path as the input."
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
               `(related-files-fn ,related-files-fn))))
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
  (when-let* ((preset (projectile--cmake-read-preset (projectile-expand-root filename))))
    (append
     (projectile--cmake-command-presets-shallow filename command-type)
     (mapcan
      (lambda (included-file) (projectile--cmake-command-presets
                               (expand-file-name included-file (file-name-directory filename))
                               command-type))
      (gethash "include" preset)))))

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
(projectile-register-project-type 'make '("Makefile")
                                  :compile "make"
                                  :test "make test"
                                  :install "make install")
(projectile-register-project-type 'gnumake '("GNUMakefile")
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
                                  :test-suffix "_test")
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
                                  :test-suffix "_test")
(projectile-register-project-type 'rails-rspec '("Gemfile" "app" "lib" "db" "config" "spec")
                                  :compile "bundle exec rails server"
                                  :src-dir "app/"
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
                          (and marker (projectile-verify-files marker dir) project-type))))
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

(defun projectile-verify-files (files &optional dir)
  "Check whether all FILES exist in the project.
When DIR is specified it checks DIR's project, otherwise
it acts on the current project."
  (seq-every-p #'(lambda (file) (projectile-verify-file file dir)) files))

(defun projectile-verify-file (file &optional dir)
  "Check whether FILE exists in the current project.
When DIR is specified it checks DIR's project, otherwise
it acts on the current project."
  (file-exists-p (projectile-expand-root file dir)))

(defun projectile-verify-file-wildcard (file &optional dir)
  "Check whether FILE exists in the current project.
When DIR is specified it checks DIR's project, otherwise
it acts on the current project.
Expands wildcards using `file-expand-wildcards' before checking."
  (file-expand-wildcards (projectile-expand-root file dir)))

(defconst projectile--vcs-markers
  '((".git" . git)
    (".hg" . hg)
    (".fslckout" . fossil)
    ("_FOSSIL_" . fossil)
    (".bzr" . bzr)
    ("_darcs" . darcs)
    (".pijul" . pijul)
    (".svn" . svn)
    (".sl" . sapling)
    (".jj" . jj))
  "Alist of (MARKER . VCS) pairs probed by `projectile-project-vcs'.
Order matches the historic `cond' chain: the first matching marker
wins, so changes here are visible to all VCS detection.")

(defun projectile--vcs-from-directory-listing (directory)
  "Return the VCS symbol matching a marker directly inside DIRECTORY.
Issues a single `directory-files' call rather than one
`file-exists-p' per marker - over TRAMP that turns 10 sequential
remote round-trips into one."
  (when-let* ((entries (ignore-errors
                         (directory-files directory nil nil t))))
    (let ((entry-set (make-hash-table :test 'equal :size (length entries))))
      (dolist (e entries) (puthash e t entry-set))
      (cl-some (lambda (cell)
                 (and (gethash (car cell) entry-set) (cdr cell)))
               projectile--vcs-markers))))

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
     (t (error "Cannot determine a test file name, one of \"test-suffix\" or \"test-prefix\" must be set for project type `%s'" project-type)))))

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
     (t (error "Cannot determine an implementation file name, one of \"test-suffix\" or \"test-prefix\" must be set for project type `%s'" project-type)))))

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
          (error "Attempted to find a implementation file by switching this project type's (%s) test-dir property \"%s\" with this project type's src-dir property \"%s\", but %s does not contain \"%s\""
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
    (file-relative-name file (projectile-project-root))))

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
    (file-relative-name file (projectile-project-root))))

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
          (error "Attempted to find a test file by switching this project type's (%s) src-dir property \"%s\" with this project type's test-dir property \"%s\", but %s does not contain \"%s\""
                 project-type impl-dir test-dir impl-dir-path impl-dir)
        (projectile-complementary-dir impl-dir-path impl-dir test-dir)))))

(defun projectile-complementary-dir (dir-path string replacement)
  "Return the \"complementary\" directory of DIR-PATH.
Replace STRING in DIR-PATH with REPLACEMENT."
  (let* ((project-root (projectile-project-root))
         (relative-dir (file-name-directory (file-relative-name dir-path project-root))))
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
  (unless file-name (error "The current buffer is not visiting a file"))
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
            (t (error "Determined test file to be \"%s\", which does not exist.  Set `projectile-create-missing-test-files' to allow `projectile-find-implementation-or-test' to create new files" test-file))))))

;;;###autoload
(defun projectile--find-implementation-or-test-in (ff-variant)
  "Open the matching implementation or test file using FF-VARIANT.
FF-VARIANT is a `find-file'-like command; passing
`find-file-other-window' or `find-file-other-frame' yields the
corresponding display variants."
  (funcall ff-variant (projectile-find-implementation-or-test (buffer-file-name))))

(defun projectile-find-implementation-or-test-other-window ()
  "Open matching implementation or test file in other window.

See the documentation of `projectile--find-matching-file' and
`projectile--find-matching-test' for how implementation and test files
are determined."
  (interactive)
  (projectile--find-implementation-or-test-in #'find-file-other-window))

;;;###autoload
(defun projectile-find-implementation-or-test-other-frame ()
  "Open matching implementation or test file in other frame.

See the documentation of `projectile--find-matching-file' and
`projectile--find-matching-test' for how implementation and test files
are determined."
  (interactive)
  (projectile--find-implementation-or-test-in #'find-file-other-frame))

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
  "Find relative file based on PROJECT-TYPE."
  (or projectile-project-related-files-fn
      (projectile-project-type-attribute project-type 'related-files-fn)))

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
    (file-relative-name complementary-file (projectile-project-root))))

(defun projectile--test-file-from-test-dir-str (file-name)
  "Get the relative path of the test file FILE-NAME.
Return a path relative to the project root for the test file of FILE-NAME
using the src-dir and test-dir properties of the current project type which
should be strings, nil returned if this is not the case."
  (when-let* ((complementary-file (projectile--complementary-file
                                   file-name
                                   #'projectile--impl-to-test-dir
                                   #'projectile--test-name-for-impl-name)))
    (file-relative-name complementary-file (projectile-project-root))))

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

(defun projectile--read-search-string-with-default (prefix-label)
  (let* ((prefix-label (projectile-prepend-project-name prefix-label))
         (default-value (projectile-symbol-or-selection-at-point))
         (default-label (if (or (not default-value)
                                (string= default-value ""))
                            ""
                          (format " (default %s)" default-value))))
    (read-string (format "%s%s: " prefix-label default-label) nil nil default-value)))

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
        ;; TODO: Replace seq-uniq+append with seq-union when Emacs 28.1 is the minimum version
        (let ((grep-find-ignored-files
               (seq-uniq (append (projectile--globally-ignored-file-suffixes-glob)
                                 grep-find-ignored-files)))
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
    (error "Package `ag' is not available"))
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
          (t (error "Packages `ripgrep' and `rg' are not available")))))

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
                    (format "Search [%s]%s for"
                            (car backend) (if regexp " regexp" ""))))))
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
                           (projectile--read-search-string-with-default "Grep for")))
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
          (format "Ag %ssearch for" (if current-prefix-arg "regexp " "")))
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
          (format "Ripgrep %ssearch for" (if current-prefix-arg "regexp " "")))
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
      (error "Package 'vterm' is not available"))
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
      (error "Package 'eat' is not available"))
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
    (error "Package 'ghostel' is not available"))
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

;;;###autoload
(defun projectile-run-vterm-other-window (&optional arg)
  "Invoke `vterm' in the project's root, displayed in another window.
Use a prefix argument ARG to indicate creation of a new process instead."
  (interactive "P")
  (projectile--run 'vterm arg t))

;;;###autoload
(defun projectile-run-eat (&optional arg)
  "Invoke `eat' in the project's root (the eat `projectile-run' backend).
Use a prefix argument ARG to indicate creation of a new process instead."
  (interactive "P")
  (projectile--run 'eat arg nil))

;;;###autoload
(defun projectile-run-eat-other-window (&optional arg)
  "Invoke `eat' in the project's root, displayed in another window.
Use a prefix argument ARG to indicate creation of a new process instead."
  (interactive "P")
  (projectile--run 'eat arg t))

;;;###autoload
(defun projectile-run-ghostel (&optional arg)
  "Invoke `ghostel' in the project's root (the ghostel `projectile-run' backend).
Use a prefix argument ARG to indicate creation of a new process instead."
  (interactive "P")
  (projectile--run 'ghostel arg nil))

;;;###autoload
(defun projectile-run-ghostel-other-window (&optional arg)
  "Invoke `ghostel' in the project's root, displayed in another window.
Use a prefix argument ARG to indicate creation of a new process instead."
  (interactive "P")
  (projectile--run 'ghostel arg t))

(defun projectile-files-from-cmd (cmd directory)
  "Use a grep-like CMD to search for files within DIRECTORY.

CMD should include the necessary search params and should output
equivalently to grep -HlI (only unique matching filenames).
Returns a list of expanded filenames."
  (let ((default-directory directory))
    (mapcar (lambda (str)
              (concat directory
                      (string-remove-prefix "./" str)))
            (split-string
             (string-trim (shell-command-to-string cmd))
             "\n+"
             t))))

(defvar projectile-files-with-string-commands
  '((rg . "rg -lF --no-heading --color never ")
    (ag . "ag --literal --nocolor --noheading -l ")
    (ack . "ack --literal --nocolor -l ")
    (git . "git grep -HlIF ")
    ;; -r: recursive
    ;; -H: show filename for each match
    ;; -l: show only file names with matches
    ;; -I: no binary files
    ;; -F: interpret pattern as fixed string, not regexp
    (grep . "grep -rHlIF %s .")))

(defun projectile--rg-construct-command (search-term &optional file-ext)
  "Construct Rg option to search files by the extension FILE-EXT."
  (if (stringp file-ext)
      (concat (alist-get 'rg projectile-files-with-string-commands)
              "-g '"
              file-ext
              "' "
              search-term)
    (concat (alist-get 'rg projectile-files-with-string-commands)
            search-term)))

(defun projectile--ag-construct-command (search-term &optional file-ext)
  "Construct Ag option to search files by the extension FILE-EXT."
  (if (stringp file-ext)
      (concat (alist-get 'ag projectile-files-with-string-commands)
              "-G "
              (replace-regexp-in-string
               "\\*" ""
               (replace-regexp-in-string "\\." "\\\\." file-ext))
              "$ "
              search-term)
    (concat (alist-get 'ag projectile-files-with-string-commands)
            search-term)))

(defun projectile--ack-construct-command (search-term &optional file-ext)
  "Construct Ack option to search files by the extension FILE-EXT."
  (if (stringp file-ext)
      (concat "ack -g '"
              (replace-regexp-in-string
               "\\*" ""
               (replace-regexp-in-string "\\." "\\\\." file-ext))
              "$' | "
              (alist-get 'ack projectile-files-with-string-commands)
              "-x "
              search-term)
    (concat (alist-get 'ack projectile-files-with-string-commands)
            search-term)))

(defun projectile--git-grep-construct-command (search-term &optional file-ext)
  "Construct Grep option to search files by the extension FILE-EXT."
  (if (stringp file-ext)
      (concat (alist-get 'git projectile-files-with-string-commands)
              search-term
              "  -- '"
              file-ext
              "'")
    (concat (alist-get 'git projectile-files-with-string-commands)
            search-term)))

(defun projectile--grep-construct-command (search-term &optional file-ext)
  "Construct Grep option to search files by the extension FILE-EXT."
  (if (stringp file-ext)
      (concat (format (alist-get 'grep projectile-files-with-string-commands)
                      search-term)
              " --include '"
              file-ext
              "'")
    (format (alist-get 'grep projectile-files-with-string-commands)
            search-term)))

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
    (fileloop-initialize-replace (regexp-quote old-text) new-text filtered-files 'default)
    (fileloop-continue)))

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
    (fileloop-initialize-replace old-text new-text files 'default)
    (fileloop-continue)))

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
                :caller 'projectile-read-project)
             (projectile-acquire-root))))

;;;###autoload
(defun projectile-dired (&optional arg)
  "Open `dired' at the root of the project.
With a prefix argument ARG, prompt for a known project to open in dired."
  (interactive "P")
  (projectile--dired #'dired arg))

;;;###autoload
(defun projectile-dired-other-window (&optional arg)
  "Open `dired' at the root of the project in another window.
With a prefix argument ARG, prompt for a known project to open in dired."
  (interactive "P")
  (projectile--dired #'dired-other-window arg))

;;;###autoload
(defun projectile-dired-other-frame (&optional arg)
  "Open `dired' at the root of the project in another frame.
With a prefix argument ARG, prompt for a known project to open in dired."
  (interactive "P")
  (projectile--dired #'dired-other-frame arg))

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
    (dolist (command-map (list projectile-configure-cmd-map
                               projectile-compilation-cmd-map
                               projectile-install-cmd-map
                               projectile-package-cmd-map
                               projectile-test-cmd-map
                               projectile-run-cmd-map))
      (dolist (dir (hash-table-keys command-map))
        (when (string-prefix-p root dir)
          (remhash dir command-map))))
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

(defun projectile-install-command (compile-dir)
  "Retrieve the install command for COMPILE-DIR.

The command is determined like this:

- first we check `projectile-install-cmd-map' for the last
install command that was invoked on the project

- then we check for `projectile-project-install-cmd' supplied
via .dir-locals.el

- finally we check for the default install command for a
project of that type"
  (or (gethash compile-dir projectile-install-cmd-map)
      projectile-project-install-cmd
      (projectile-default-install-command (projectile-project-type))))

(defun projectile-package-command (compile-dir)
  "Retrieve the package command for COMPILE-DIR.

The command is determined like this:

- first we check `projectile-package-cmd-map' for the last
install command that was invoked on the project

- then we check for `projectile-project-package-cmd' supplied
via .dir-locals.el

- finally we check for the default package command for a
project of that type"
  (or (gethash compile-dir projectile-package-cmd-map)
      projectile-project-package-cmd
      (projectile-default-package-command (projectile-project-type))))

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
symbols, e.g. `compile' or `test') return the history specific to
that command type, so histories of different types don't bleed into
each other's prompts.  With COMMAND-TYPE nil return the combined
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
    (command command-map &key command-type show-prompt prompt-prefix save-buffers use-comint-mode)
  "Run a project COMMAND, typically a test- or compile command.

Cache the COMMAND for later use inside the hash-table COMMAND-MAP.

COMMAND-TYPE, when non-nil, is the lifecycle command type symbol
\(e.g. `compile' or `test') and is used to keep a per-type command
history for the prompt, in addition to the combined per-project one.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
by setting SHOW-PROMPT.  The prompt will be prefixed with PROMPT-PREFIX.

If SAVE-BUFFERS is non-nil save all projectile buffers before
running the command.

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
      (puthash default-directory command command-map)
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
    (unless command
      (user-error "No %scommand configured for project type `%s'"
                  (or prompt-prefix "") (projectile-project-type)))
    (unless (file-directory-p default-directory)
      (mkdir default-directory))
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

;;;###autoload
(defun projectile-configure-project (arg)
  "Run project configure command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let ((command (projectile-configure-command (projectile-compilation-dir)))
        (command-map (if (projectile--cache-project-commands-p) projectile-configure-cmd-map)))
    (projectile--run-project-cmd command command-map
                                 :command-type 'configure
                                 :show-prompt arg
                                 :prompt-prefix "Configure command: "
                                 :save-buffers t
                                 :use-comint-mode projectile-configure-use-comint-mode)))

;;;###autoload
(defun projectile-compile-project (arg)
  "Run project compilation command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.  Per project default command can be set through
`projectile-project-compilation-cmd'."
  (interactive "P")
  (let ((command (projectile-compilation-command (projectile-compilation-dir)))
        (command-map (if (projectile--cache-project-commands-p) projectile-compilation-cmd-map)))
    (projectile--run-project-cmd command command-map
                                 :command-type 'compile
                                 :show-prompt arg
                                 :prompt-prefix "Compile command: "
                                 :save-buffers t
                                 :use-comint-mode projectile-compile-use-comint-mode)))

;;;###autoload
(defun projectile-test-project (arg)
  "Run project test command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let ((command (projectile-test-command (projectile-compilation-dir)))
        (command-map (if (projectile--cache-project-commands-p) projectile-test-cmd-map)))
    (projectile--run-project-cmd command command-map
                                 :command-type 'test
                                 :show-prompt arg
                                 :prompt-prefix "Test command: "
                                 :save-buffers t
                                 :use-comint-mode projectile-test-use-comint-mode)))

;;;###autoload
(defun projectile-install-project (arg)
  "Run project install command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let ((command (projectile-install-command (projectile-compilation-dir)))
        (command-map (if (projectile--cache-project-commands-p) projectile-install-cmd-map)))
    (projectile--run-project-cmd command command-map
                                 :command-type 'install
                                 :show-prompt arg
                                 :prompt-prefix "Install command: "
                                 :save-buffers t
                                 :use-comint-mode projectile-install-use-comint-mode)))

;;;###autoload
(defun projectile-package-project (arg)
  "Run project package command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let ((command (projectile-package-command (projectile-compilation-dir)))
        (command-map (if (projectile--cache-project-commands-p) projectile-package-cmd-map)))
    (projectile--run-project-cmd command command-map
                                 :command-type 'package
                                 :show-prompt arg
                                 :prompt-prefix "Package command: "
                                 :save-buffers t
                                 :use-comint-mode projectile-package-use-comint-mode)))

;;;###autoload
(defun projectile-run-project (arg)
  "Run project run command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let ((command (projectile-run-command (projectile-compilation-dir)))
        (command-map (if (projectile--cache-project-commands-p) projectile-run-cmd-map)))
    (projectile--run-project-cmd command command-map
                                 :command-type 'run
                                 :show-prompt arg
                                 :prompt-prefix "Run command: "
                                 :use-comint-mode projectile-run-use-comint-mode)))

;;;###autoload
(defun projectile-repeat-last-command (show-prompt)
  "Run last projectile external command.

External commands are: `projectile-configure-project',
`projectile-compile-project', `projectile-test-project',
`projectile-install-project', `projectile-package-project',
and `projectile-run-project'.

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
  (when (and projectile-auto-discover projectile-project-search-path)
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
With a prefix ARG invokes `projectile-dispatch' (when available) instead
of `projectile-switch-project-action'."
  (interactive "P")
  (let ((projects (projectile-relevant-known-projects)))
    (if projects
        (projectile-completing-read
         "Switch to project: " projects
         :action (lambda (project)
                   (projectile-switch-project-by-name project arg))
         :caller 'projectile-read-project)
      (user-error "There are no known projects"))))

;;;###autoload
(defun projectile-switch-open-project (&optional arg)
  "Switch to a project we have currently opened.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-dispatch' (when available) instead
of `projectile-switch-project-action'."
  (interactive "P")
  (let ((projects (projectile-relevant-open-projects)))
    (if projects
        (projectile-completing-read
         "Switch to open project: " projects
         :action (lambda (project)
                   (projectile-switch-project-by-name project arg))
         :caller 'projectile-read-project)
      (user-error "There are no open projects"))))

;; The other-window/-frame switch commands reuse `projectile-switch-project'
;; wholesale (so the dir-locals dance, prefix-arg dispatch, and most-recent
;; tracking all stay in one place) and just rebind the post-switch action
;; around it.  This relies on the completion framework invoking its action
;; synchronously, which the supported ones (default, vertico, ivy, helm) do.
;;;###autoload
(defun projectile-switch-project-other-window (&optional arg)
  "Switch to a project we have visited before and display it in another window.

Like `projectile-switch-project', but runs
`projectile-switch-project-other-window-action' (by default
`projectile-find-file-other-window') after switching, so the project is
shown in another window.  With a prefix ARG invokes `projectile-dispatch'
(when available) instead."
  (interactive "P")
  (let ((projectile-switch-project-action projectile-switch-project-other-window-action))
    (projectile-switch-project arg)))

;;;###autoload
(defun projectile-switch-project-other-frame (&optional arg)
  "Switch to a project we have visited before and display it in another frame.

Like `projectile-switch-project', but runs
`projectile-switch-project-other-frame-action' (by default
`projectile-find-file-other-frame') after switching, so the project is
shown in another frame.  With a prefix ARG invokes `projectile-dispatch'
(when available) instead."
  (interactive "P")
  (let ((projectile-switch-project-action projectile-switch-project-other-frame-action))
    (projectile-switch-project arg)))

;;;###autoload
(defun projectile-switch-to-most-recent-project (&optional arg)
  "Switch to the project recorded in `projectile-most-recent-project'.
That's the project that was current before the most recent project
switch, so calling this from a buffer in the switched-to project takes
you back where you came from.  With a prefix ARG invokes
`projectile-dispatch' (when available) instead of
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
With a prefix ARG invokes `projectile-dispatch' (when available) instead
of `projectile-switch-project-action'."
  ;; let's make sure that the target directory exists and is actually a project
  ;; we ignore remote folders, as the check breaks for TRAMP unless already connected
  (unless (or (file-remote-p project-to-switch) (projectile-project-p project-to-switch))
    (projectile-remove-known-project project-to-switch)
    (error "Directory %s is not a project" project-to-switch))
  ;; Record the project we're leaving so `projectile-most-recent-project'
  ;; points at it after the switch (captured before `default-directory' is
  ;; rebound below).
  (let ((previous-project (projectile-project-root))
        (action (if (and arg (fboundp 'projectile-dispatch))
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
                      :caller 'projectile-read-project)))
  (unless (called-interactively-p 'any)
    (setq projectile-known-projects
          (seq-remove
           (lambda (proj) (string= project proj))
           projectile-known-projects))
    (projectile-merge-known-projects)
    (when projectile-verbose
      (message "Project %s removed from the list of known projects." project))))

;;;###autoload
(defun projectile-remove-current-project-from-known-projects ()
  "Remove the current project from the list of known projects."
  (interactive)
  (projectile-remove-known-project (abbreviate-file-name (projectile-acquire-root))))

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
    (push (file-name-as-directory (abbreviate-file-name project-root)) projectile-known-projects)
    (setq projectile-known-projects (seq-uniq projectile-known-projects))
    (projectile-merge-known-projects)))

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
  "Prompt for a variable and return its name."
  (completing-read "Variable: "
                   obarray
                   (lambda (v)
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
    (define-key map (kbd "4 a") #'projectile-find-other-file-other-window)
    (define-key map (kbd "4 b") #'projectile-switch-to-buffer-other-window)
    (define-key map (kbd "4 C-o") #'projectile-display-buffer)
    (define-key map (kbd "4 d") #'projectile-find-dir-other-window)
    (define-key map (kbd "4 D") #'projectile-dired-other-window)
    (define-key map (kbd "4 f") #'projectile-find-file-other-window)
    (define-key map (kbd "4 g") #'projectile-find-file-dwim-other-window)
    (define-key map (kbd "4 p") #'projectile-switch-project-other-window)
    (define-key map (kbd "4 t") #'projectile-find-implementation-or-test-other-window)
    (define-key map (kbd "5 a") #'projectile-find-other-file-other-frame)
    (define-key map (kbd "5 b") #'projectile-switch-to-buffer-other-frame)
    (define-key map (kbd "5 d") #'projectile-find-dir-other-frame)
    (define-key map (kbd "5 D") #'projectile-dired-other-frame)
    (define-key map (kbd "5 f") #'projectile-find-file-other-frame)
    (define-key map (kbd "5 g") #'projectile-find-file-dwim-other-frame)
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
    (define-key map (kbd "k") #'projectile-kill-buffers)
    (define-key map (kbd "l") #'projectile-find-file-in-directory)
    (define-key map (kbd "m") #'projectile-dispatch)
    (define-key map (kbd "o") #'projectile-multi-occur)
    (define-key map (kbd "p") #'projectile-switch-project)
    (define-key map (kbd "q") #'projectile-switch-open-project)
    (define-key map (kbd "r") #'projectile-replace)
    (define-key map (kbd "s s") #'projectile-search)
    (define-key map (kbd "s g") #'projectile-grep)
    (define-key map (kbd "s r") #'projectile-ripgrep)
    (define-key map (kbd "s a") #'projectile-ag)
    (define-key map (kbd "s x") #'projectile-find-references)
    (define-key map (kbd "S") #'projectile-save-project-buffers)
    (define-key map (kbd "t") #'projectile-toggle-between-implementation-and-test)
    (define-key map (kbd "T") #'projectile-find-test-file)
    (define-key map (kbd "v") #'projectile-vc)
    ;; project lifecycle external commands
    (define-key map (kbd "c o") #'projectile-configure-project)
    (define-key map (kbd "c c") #'projectile-compile-project)
    (define-key map (kbd "c p") #'projectile-package-project)
    (define-key map (kbd "c i") #'projectile-install-project)
    (define-key map (kbd "c t") #'projectile-test-project)
    (define-key map (kbd "c r") #'projectile-run-project)
    ;; TODO: Legacy keybindings that will be removed in Projectile 3
    (define-key map (kbd "C") #'projectile-configure-project)
    (define-key map (kbd "K") #'projectile-package-project)
    (define-key map (kbd "L") #'projectile-install-project)
    (define-key map (kbd "P") #'projectile-test-project)
    (define-key map (kbd "u") #'projectile-run-project)
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
;; `transient' is an optional dependency that requires Emacs 28.1+, so it can be
;; entirely absent (including on Emacs 27, which Projectile still supports and
;; where transient cannot even be installed).  The prefix is therefore defined
;; at load time only when transient is present, and via `eval' so the
;; `transient-define-prefix' macro is not needed at byte-compile time.  The menu
;; keys deliberately match the `projectile-command-map' bindings.
(when (require 'transient nil t)
  (eval
   '(transient-define-prefix projectile-dispatch ()
    "Dispatch menu for Projectile commands.

The switches in the Modifiers group tweak how the commands below run:
`--invalidate-cache' rebuilds the file cache first (file/dir commands),
`--regexp' searches for a regexp (ag/ripgrep), `--new-process' starts a
fresh process (shells), and `--display' opens the result in another
window or frame (file/buffer/project commands)."
    ["Modifiers"
     ("-i" "invalidate cache" "--invalidate-cache")
     ("-r" "regexp search" "--regexp")
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
      ("t" "toggle impl/test" projectile-dispatch-impl-or-test)]
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
      ("o" "multi-occur" projectile-multi-occur)
      ("r" "replace" projectile-replace)]]
    [["Project"
      ("p" "switch project" projectile-dispatch-switch-project)
      ("q" "switch open project" projectile-switch-open-project)
      ("A" "add known project" projectile-add-known-project)
      ("v" "vc" projectile-vc)]
     ["Lifecycle"
      ("cc" "compile" projectile-compile-project)
      ("ct" "test" projectile-test-project)
      ("cr" "run" projectile-run-project)
      ("co" "configure" projectile-configure-project)
      ("ci" "install" projectile-install-project)
      ("cp" "package" projectile-package-project)]
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
     ["Cache"
      ("i" "invalidate cache" projectile-invalidate-cache)
      ("z" "cache current file" projectile-cache-current-file)]])
   t))

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
         ["Jump between implementation file and test file" projectile-toggle-between-implementation-and-test])
        ("Buffers"
         ["Switch to buffer" projectile-switch-to-buffer]
         ["Kill project buffers" projectile-kill-buffers]
         ["Save project buffers" projectile-save-project-buffers]
         ["Recent files" projectile-recentf]
         ["Previous buffer" projectile-previous-project-buffer]
         ["Next buffer" projectile-next-project-buffer])
        ("Projects"
         ["Add known project" projectile-add-known-project]
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
         ["Install project" projectile-install-project]
         ["Package project" projectile-package-project]
         ["Run project" projectile-run-project]
         "--"
         ["Repeat last build command" projectile-repeat-last-command])
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
  (let ((remote (file-remote-p default-directory)))
    (projectile-maybe-limit-project-file-buffers)
    (when projectile-auto-update-cache
      (projectile-cache-files-find-file-hook))
    (projectile-track-known-projects-find-file-hook)
    (unless remote
      (when projectile-dynamic-mode-line
        (projectile-update-mode-line)))))

(defun projectile-maybe-limit-project-file-buffers ()
  "Limit the opened file buffers for a project.

The function simply kills the last buffer, as it's normally called
when opening new files."
  (when projectile-max-file-buffer-count
    (let ((project-buffers (projectile-project-buffer-files)))
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
  ;; NOTE: Use the project root (the cdr) directly rather than
  ;; `project-root', which doesn't exist on Emacs 27 (project.el provided
  ;; `project-roots' there instead).
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
    (add-hook 'window-configuration-change-hook #'projectile-update-mode-line-on-window-change)
    (advice-add 'compilation-find-file :around #'compilation-find-file-projectile-find-compilation-buffer)
    (advice-add 'delete-file :before #'delete-file-projectile-remove-from-cache))
   (t
    (remove-hook 'project-find-functions #'project-projectile)
    (remove-hook 'find-file-hook #'projectile-find-file-hook-function)
    (remove-hook 'projectile-find-dir-hook #'projectile-track-known-projects-find-file-hook)
    (remove-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook)
    (remove-hook 'window-configuration-change-hook #'projectile-update-mode-line-on-window-change)
    (advice-remove 'compilation-find-file #'compilation-find-file-projectile-find-compilation-buffer)
    (advice-remove 'delete-file #'delete-file-projectile-remove-from-cache))))

;;; savehist-mode - When `savehist-mode' is t, projectile-project-command-history will be saved.
;; See https://github.com/bbatsov/projectile/issues/1637 for more details
(defvar savehist-additional-variables nil)

(if (bound-and-true-p savehist-loaded)
    (add-to-list 'savehist-additional-variables 'projectile-project-command-history)
  (add-hook 'savehist-mode-hook
            (lambda()
              (add-to-list 'savehist-additional-variables 'projectile-project-command-history))))

;;;###autoload
(define-obsolete-function-alias 'projectile-global-mode 'projectile-mode "1.0")

(provide 'projectile)

;;; projectile.el ends here
