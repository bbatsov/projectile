# Changelog

## master (unreleased)

### New features

* New defcustom `projectile-remember-window-configs` will make
  `projectile-switch-project` restore the most recent window configuration (if
  any) of the target project.
* New command `projectile-run-command-in-root`.
* New defcustom `projectile-use-git-grep` will make `projectile-grep` use `git grep`
for git projects.
* Added new `projectile-commander` methods ?v and ?R which run
  `projectile-vc` and `projectile-regenerate-tags`, respectively.
* `projectile-vc` will use `magit-status` if available.
* New functions `projectile-find-implementation-or-test` and
  `projectile-find-implementation-or-test-other-window`, the later is
  bound to `C-c p 4 t`.

### Changes

* The presence of a `Makefile` is no longer taken as an indicator
  of the project root by default, since recursive make is unfortunately
  a common occurrence (affects `projectile-project-root-files`).
* Projectile is now able to find the project pertaining to a symlink
pointing to a version-controlled file.
* Drop `projectile-ack-function` defcustom.

### Bugs fixed

* Modified `projectile-ack` to append to `ack-and-a-half-arguments`
  instead of overriding them.
* [#229] Fix `projectile-find-file-in-directory`'s behavior for project directories
* `projectile-toggle-between-implementation-or-test` shows
understandable error if current buffer is not visiting a file.
* [#244] Correct folder picked up by `projectile-ack` after project-switch.

## 0.10.0 (12/09/2013)

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
the switch action wis `projectile-commander`.

### Changes

* Replaced variable `projectile-use-native-indexing` with `projectile-indexing-method`.
* Corrected grammar on error message for not being in a project.

### Bug fixes

* `projectile-find-test-file` now properly displays only test files (#145).

## 0.9.2 (07/16/2013)

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

## 0.9.1 (04/26/2013)

### New features

* Display recentf files in helm-projectile.

### Bugs fixed

* #95 - handle properly missing project root

### Misc

## 0.9.0 (04/24/2013)

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
