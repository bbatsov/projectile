**Projectile** is a project interaction library for Emacs. Its goal is to
provide a nice set of features operating on a project level without
introducing external dependencies (when feasible). For instance -
finding project files has a portable implementation written in pure
Emacs Lisp without the use of GNU `find` (but for performance sake an
indexing mechanism backed by external commands exists as well).

Projectile tries to be practical - portability is great, but if some
external tools could speed up some task substantially and the tools
are available, Projectile will leverage them.

This library provides easy project management and navigation. The
concept of a project is pretty basic - just a folder containing
special file. Currently `git`, `mercurial`, `darcs` and `bazaar` repos
are considered projects by default. So are `lein`, `maven`, `sbt`,
`scons`, `rebar` and `bundler` projects. If you want to mark a folder
manually as a project just create an empty `.projectile` file in
it. Some of Projectile's features:

* jump to a file in project
* jump to files at point in project
* jump to a directory in project
* jump to a file in a directory
* jump to a project buffer
* jump to a test in project
* toggle between files with same names but different extensions (e.g. `.h` <-> `.c/.cpp`, `Gemfile` <-> `Gemfile.lock`)
* toggle between code and its test (e.g. `main.service.js` <-> `main.service.spec.js`)
* jump to recently visited files in the project
* switch between projects you have worked on
* kill all project buffers
* replace in project
* multi-occur in project buffers
* grep in project
* regenerate project etags or gtags (requires [ggtags](https://github.com/leoliu/ggtags)).
* visit project in dired
* run make in a project with a single key chord
* browse dirty version controlled projects

Here's a glimpse of Projectile in action:

![Projectile Screenshot](screenshots/projectile.png)

You can support my work on Projectile via [Salt](https://salt.bountysource.com/teams/projectile).
