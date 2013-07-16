# Changelog

## master (unreleased)

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
