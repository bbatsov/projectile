# Changelog

### master (unreleased)

### New features

### Changes

* Rebound `projectile-compile-project` to <kbd>C-c p c</kbd>
* Reworked `projectile-compile-project` and `projectile-test-project` to be smarter, more configurable and closer in behavior to the stock `compile` command

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
