# Changelog

## master (unreleased)

### New Features

* Consider Ensime configuration file as root marker, `.ensime`.
* [#1057](https://github.com/bbatsov/projectile/issues/1057): Make it possible to disable automatic project tracking via `projectile-track-known-projects-automatically`.
* Added ability to specify test files suffix and prefix at the project registration.

### Changes

* [#1129](https://github.com/bbatsov/projectile/pull/1129): Fix TRAMP issues.
* Add R DESCRIPTION file to `projectile-project-root-files`.
* Ignore backup files in `projectile-get-other-files`.
* Ignore Ensime cache directory, `.ensime_cache`.
* [#364](https://github.com/bbatsov/projectile/issues/364): `projectile-add-known-project` can now be used interactively.
* `projectile-mode` is now a global mode.
* `projectile-find-tag` now defaults to xref on Emacs 25.1+.
* Add relation between `.h` and `.cc` files in `projectile-other-file-alist`.
* Cache the name of the current project for mode-line display of the project name.
* [#1078](https://github.com/bbatsov/projectile/issues/1078): For projectile-grep/ag use default value like grep/rgrep/ag.
* Don't treat `package.json` as a project marker.

### Bugs fixed

* [#1072](https://github.com/bbatsov/projectile/issues/1072): Create test files only within the project.
* [#1063](https://github.com/bbatsov/projectile/issues/1063): Support Fossil checkouts on Windows.
* [#1024](https://github.com/bbatsov/projectile/issues/1024): Do not cache ignored project files.
* [#1022](https://github.com/bbatsov/projectile/issues/1022): Scan for Fossil's checkout DB, not its config DB.
* [#1007](https://github.com/bbatsov/projectile/issues/1007): Make use of `projectile-go-function`.
* [#1011](https://github.com/bbatsov/projectile/issues/1011): Save project files before running project tests.
* [#1099](https://github.com/bbatsov/projectile/issues/1099): Fix the behaviour of `projectile-purge-dir-from-cache`.
* [#1067](https://github.com/bbatsov/projectile/issues/1067): Don't mess up `default-directory` after switching projects.

## 0.14.0 (2016-07-08)

### New features

* Add [elixir](http://elixir-lang.org) project type.
* Add [emacs-cask](https://github.com/cask/cask) project type.
* Add [boot-clj](https://github.com/boot-clj/boot) project type.
* Add [racket](http://racket-lang.org) project type.
* Add support for projects using gradlew script.
* Prefer Haskell stack projects over cabal projects.
* Add ability to use elisp functions for test, compile and run commands.
* Consider `TAGS` and `GTAGS` root markers.
* Add relation between the `.h`, `.cxx`, `.ixx` and `.hxx` files in `projectile-other-file-alist`.
* Add relation between the `.hpp` and `.cc` files in `projectile-other-file-alist`.
* Add support to specify project name either via `.dir-locals.el` or by providing a customized `projectile-project-name-function`.
* Add a command to switch between open projects (`projectile-switch-open-project`).
* Add a command to edit the .dir-locals.el file of the project (`projectile-edit-dir-locals`).
* Add file local variable `projectile-project-root`, which allows overriding the project root on a per-file basis. This allows navigating a different project from, say, an org file in a another git repository.
* Add `projectile-grep-finished-hook`.
* Ignore file suffixes listed in `projectile-globally-ignored-file-suffixes` when using `projectile-grep` and `projectile-ag`.
* Add `projectile-replace-regexp`, which supports replacement by regexp within a project. `projectile-replace` is now used solely for literal replacements.
* New command `projectile-run-shell` (<kbd>C-c p x s</kbd>).
* New command `projectile-run-eshell` (<kbd>C-c p x e</kbd>).
* New command `projectile-run-term` (<kbd>C-c p x t</kbd>).
* Let user unignore files in `.projectile` with the ! prefix.
* Add a command to add all projects in a directory to the cache (`projectile-discover-projects-in-directory`).
* Add a command to list dirty version controlled projects (`projectile-browse-dirty-projects`).

### Changes

* Prefer ag's internal .gitignore parsing.
* Added variable to control use of external find-tag implementations.
* Specify `--with-keep.source` argument when installing R projects

### Bugs fixed

* [#871](https://github.com/bbatsov/projectile/issues/871): Stop advice for `compilation-find-file` to override other advices.
* [#557](https://github.com/bbatsov/projectile/issues/557): stack overflow in `projectile-find-tag`.
* [#955](https://github.com/bbatsov/projectile/issues/955): Error while toggling between test and source file.
* [#952](https://github.com/bbatsov/projectile/issues/952): VCS submodules brought in even thought not descendent of project root.
* [#576](https://github.com/bbatsov/projectile/issues/576): `projectile-replace` stomps regular expressions.
* [#957](https://github.com/bbatsov/projectile/pull/957): When opening a specified file from the terminal, do not error inside of `projectile-cache-current-file`.
* [#984](https://github.com/bbatsov/projectile/pull/984): Error when a project is a symlink that changes target.
* [#1013](https://github.com/bbatsov/projectile/issues/1013): `projectile-project-buffer-p` may return incorrect result on Windows.

## 0.13.0 (2015-10-21)

### New features

* Add `projectile-before-switch-project-hook`.
* Add the ability to specify the project type via `.dir-locals.el`.
* Add support for projects using Midje.
* Add the ability to create missing tests automatically (controlled via the `projectile-create-missing-test-files` defcustom).
* Add the ability to dynamically decide if a project should be added to `projectile-known-projects` (via new `projectile-ignored-project-function` defcustom).
* Add the ability to register new project types dynamically with `projectile-register-project-type`.
* Add the ability to specify a project compilation and test commands via `.dir-locals.el`.
This is done via the variables `projectile-project-compilation-cmd` and `projectile-project-test-cmd`.
* [#489](https://github.com/bbatsov/projectile/issues/489): New interactive command `projectile-run-project`.
* Optionally run [monky](http://ananthakumaran.in/monky/) on Mercurial projects.
* Add the ability to specify a project compilation directory relative to the root directory via `.dir-locals.el` with the variable `projectile-project-compilation-dir`.
* When there is a selected region, projectile-ag, projectile-grep, projectile-replace and projectile-find-tag uses it's content as a search term instead of symbol at point.

### Changes

* Rename `projectile-switch-project-hook` to `projectile-after-switch-project-hook`.
* `projectile-compile-project` now offers appropriate completion
  targets even when called from a subdirectory.
* Add an argument specifying the regexp to search to `projectile-grep`.
* Use `help-projectile-grep` instead of `helm-find-file` when selecting a project.
* Omit current buffer from `projectile-switch-to-buffer` and `projectile-switch-to-buffer-other-window` choices.

### Bugs fixed

* [#721](https://github.com/bbatsov/projectile/issues/721#issuecomment-100830507): Remove current buffer from `helm-projectile-switch-project`.
* [#667](https://github.com/bbatsov/projectile/issues/667) Use `file-truename` when caching filenames to prevent duplicate/symlinked filepaths from appearing when opening a project file.
* [#625](https://github.com/bbatsov/projectile/issues/625): Ensure the directory has a trailing slash while searching for it.
* [#763](https://github.com/bbatsov/projectile/issues/763): Check for `projectile-use-git-grep` in `helm-projectile-grep`
* Fix `projectile-parse-dirconfig-file` to parse non-ASCII characters properly.

## 0.12.0 (2015-03-29)

### New features

* Replace Helm equivalent commands in `projectile-commander` when using Helm.
* Add replacement commands projectile-grep, projectile-ack and projectile-ag with its Helm version.
* Add virtual directory manager that allows to create/update (add or delete files) a Dired buffer based on Projectile files.
* Add a new Helm command: `helm-projectile-find-file-in-known-projects` that opens all files in all known projects.
* Add an action for `helm-projectile-switch-project` to delete multiple marked projects.
* Add the ability to retrieve files in all sub-projects under a project root.
* Add `projectile-find-file-dwim` and `helm-projectile-find-file-dwim` commands.
* Provide actual Helm commands for common Projectile commands.
* Use existing Helm actions and map in `helm-find-files` that allows `helm-source-projectile-files-list`
to behave like `helm-find-files`, such as multifile selection and opening or delete on selected files.
* Add compile action to `helm-projectile`.
* Allows using Eshell and Magit outside of a project in `helm-projectile`.
* Add Helm action for incremental grep in the selected projects.
* Add command projectile-find-other-file  Switch between files with
  the same  name but different extensions.
* Add Helm interface to switch project. For more details checkout the file
  README.md.
* Make the mode line format customizable with `projectile-mode-line`
* Add support for `cargo.toml` projects
* Try to use projectile to find files in compilation buffers
* Support `helm` as a completion system
* New command `projectile-project-info` displays basic info about the current project.
* New `defcustom` `projectile-globally-ignored-buffers` allows you to ignore
  buffers by name
* New `defcustom` `projectile-globally-ignored-file-suffixes` allows
  you to globally ignore files with particular extensions

### Changes

* get-other-files returns more accurate results for files with the same name placed under different directories
* Collect search tool (`grep`, `ag`, `ack`) keybindings under a common keymap prefix (`C-c p s`)
* Remove `defcustom` `projectile-remember-window-configs` in favor of
  `persp-projectile.el`.
* Progress reporter for the native indexing method.

### Bugs fixed

* Fix `projectile-regenerate-tags` to work in directories that include spaces.
* Prevent `projectile-kill-buffers` from trying to kill indirect
buffers.
* [#412](https://github.com/bbatsov/projectile/issues/412): Handle multiple possible targets in `projectile-toggle-between-implementation-or-test`.

## 0.11.0 (2014-05-27)

### New features

* Added support for default file glob pattern to `projectile-grep`
* added file existence cache with defcustoms
  `projectile-file-exists-remote-cache-expire` and
  `projectile-file-exists-local-cache-expire`.
* added new defcustoms `projectile-project-root-files-top-down-recurring`,
  `projectile-project-root-files-bottom-up` and
  `projectile-project-root-files-functions`.
* Added new command `projectile-save-project-buffers`.
* Added new command `projectile-cleanup-known-projects`.
* Added new commands `projectile-display-buffer`
  and`projectile-find-dir-other-window`.
* Added new interacive function `projectile-project-buffers-other-buffer`
  which runs new `projectile-project-buffers-non-visible` function, the former
  is bound to `C-c p ESC`.
* New variable `projectile-enable-idle-timer` turns on an idle timer
  which runs the hook `projectile-idle-timer-hook` every
  `projectile-idle-timer-seconds` seconds when non-nil.
* New defcustom `projectile-remember-window-configs` will make
  `projectile-switch-project` restore the most recent window configuration (if
  any) of the target project.
* New command `projectile-run-command-in-root`.
* New command `projectile-run-shell-command-in-root`.
* New command `projectile-run-async-shell-command-in-root`.
* New defcustom `projectile-use-git-grep` will make `projectile-grep` use `git grep`
for git projects.
* Added new `projectile-commander` methods ?v and ?R which run
  `projectile-vc` and `projectile-regenerate-tags`, respectively.
* `projectile-vc` will use `magit-status` if available.
* New functions `projectile-find-implementation-or-test` and
  `projectile-find-implementation-or-test-other-window`, the later is
  bound to `C-c p 4 t`.
* New defcustoms `projectile-test-prefix-function` and `projectile-test-suffix-function`
allow users to customize how projectile identifies test files by project type.
* `projectile-grep` will ask for a file pattern if invoked with a
  prefix argument.
* Subversion checkouts are now automatically detected.
* CVS checkouts are now automatically detected.
* added `projectile-persp-switch-project` command to make perspective
  mode work along with projectile.
* Changed `projectile-mode-line-lighter` to a defcustom variable to make
mode line indicator prefix customizable.
* New command `projectile-find-file-in-known-projects`.
* New defcustom `projectile-ignored-projects` allows you to specify projects
that shouldn't be added to the known projects list.
* New command `projectile-remove-current-project-from-known-projects`.
* New defcustom `projectile-buffers-filter-function`.
* New defcustom `projectile-sort-order`.
* New function `projectile-process-current-project-buffers`.
* New function `projectile-process-current-project-files`.

### Changes

* The presence of a `Makefile` is no longer taken as an indicator
  of the project root by default, since recursive make is unfortunately
  a common occurrence (affects `projectile-project-root-files`).
* Projectile is now able to find the project pertaining to a symlink
pointing to a version-controlled file.
* Drop `projectile-ack-function` defcustom.
* `projectile-command-map` is now the keymap referenced by the
  `projectile-keymap-prefix` in `projectile-mode-map`. This allows
  modification of the inner map, and allows additional prefix keys to
  reference it.

### Bugs fixed

* Modified `projectile-ack` to append to `ack-and-a-half-arguments`
  instead of overriding them.
* [#229] Fix `projectile-find-file-in-directory`'s behavior for project directories
* `projectile-toggle-between-implementation-or-test` shows
understandable error if current buffer is not visiting a file.
* [#244] Correct folder picked up by `projectile-ack` after project-switch.
* [#182] Invalidate project cache if .projectile is modified.

## 0.10.0 (2013-12-09)

### New features

* Added new command `projectile-find-file-other-window`.
* Added new command `projectile-switch-to-buffer-other-window`.
* Added new command `projectile-find-file-in-directory` that allows
  you to jump to files in any directory.
* `.projectile` is now always taken into account.
* `projectile-switch-project`'s behavior is now customizable via
  `projectile-switch-project-action`.
* Added support for Gradle projects.
* Added support for `Ag`.
* Added new command `projectile-purge-file-from-cache`.
* Added new command `projectile-purge-dir-from-cache`.
* Added new command `projectile-find-tag`.
* Added new command `projectile-commander`. It allows you to quickly
  run many Projectile commands with a single key. Very useful as a
  project-switching action.
* `projectile-switch-project` now supports a prefix argument. When it's present
the switch action is `projectile-commander`.

### Changes

* Replaced variable `projectile-use-native-indexing` with `projectile-indexing-method`.
* Corrected grammar on error message for not being in a project.

### Bug fixes

* `projectile-find-test-file` now properly displays only test files (#145).

## 0.9.2 (2013-07-16)

### New features

* `projectile-invalidate-cache` now accepts a prefix argument. When
  present you'll be prompted for the project whose cache to
  invalidate.
* New command `projectile-find-dir` works similar to
  `projectile-find-file` - displays the project's dirs and opens them
  with `dired`. It's bound to `C-c p d`.
* Added support for `grizzl` as a completion system.
* Added support for `fossil` projects.
* Added support for `Symfony 2` project.
* New command `projectile-clear-known-projects` removes all known projects.
* New command `projectile-remove-known-project` prompts you for a known project to remove.

### Bugs fixed

* Fixed `projectile-replace`, which was broken from the use of relative paths
* #103 - `projectile-switch-project` does not require a project to work
* Don't show hidden buffers in projectile-project-buffers

### Changes

* Rebound `projectile-compile-project` to <kbd>C-c p c</kbd>
* Rebound `projectile-dired` to <kbd>C-c p D</kbd>
* Reworked `projectile-compile-project` and `projectile-test-project`
  to be smarter, more configurable and closer in behavior to the stock
  `compile` command
* `projectile-switch-project` (<kbd>C-c p s</kbd>) now runs `projectile-find-file` instead of `dired`.

## 0.9.1 (2013-04-26)

### New features

* Display recentf files in helm-projectile.

### Bugs fixed

* #95 - handle properly missing project root

## 0.9.0 (2013-04-24)

### New features

* Use fast external tools to find project files when possible. This is the default option on all Unices.
* Removed obsolete command `projectile-reindex-project`.
* Removed obsolete command `projectile-open`.
* Introduced support for finding tests and switching between code and tests.
* Implement basic project type detection.
* Add a simple version reporting command projectile-version.
* Display relative paths to project files instead of disambiguated filenames.
* Directories listed in .projectile file are excluded when tags are generated.
* Remembers visited projects and may switch between them with `projectile-switch-project`.
* Supports `lein {compile|test}` in Clojure projects.
* Support projects only for subdirs of the project root.
* Add the ability to manually cache files.

### Bugs fixed

* #57 - properly set the current working dir, before invoking shell commands
* #71 - correct regenerate tags keybinding in the README

### Misc

* Move menu entry under `Tools`
* Show indexing message only when doing native project indexing
* Massive performance improvements
