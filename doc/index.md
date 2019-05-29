**Projectile** is a project interaction library for Emacs. Its goal is to
provide a nice set of features operating on a project level without
introducing external dependencies (when feasible). For instance -
finding project files has a portable implementation written in pure
Emacs Lisp without the use of GNU `find` (but for performance sake an
indexing mechanism backed by external commands exists as well).

!!! Tip

    In practical terms the fact that Projectile can index the files in
    a project without shelling out to `find`, `git` or whatever, means
    that unlike many similar tools it will work on Windows without any
    additional setup.

Projectile tries to be practical - portability is great, but if some
external tools could speed up some task substantially and the tools
are available, Projectile will leverage them.

Here's a glimpse of Projectile in action (find file in project using `ido`):

![Projectile Screenshot](screenshots/projectile.png)

Projectile provides easy project management and navigation. The
concept of a project is pretty basic - just a folder containing
special file. Currently most VCS repos (e.g. `git`, `mercurial`, etc)
are considered projects by default, as are directories containing
build tools (e.g. `maven`, `leiningen`, etc) or framework markers
(e.g. Ruby on Rails). If you want to mark a folder manually as a
project just create an empty `.projectile` file in it. Some of
Projectile's features:

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

!!! Info

    A bit of trivia for you - Projectile was my very first open-source project and
    it has a very special place in my heart!

You can support my work on Projectile via
 [PayPal](https://www.paypal.me/bbatsov),
 [Patreon](https://www.patreon.com/bbatsov) and
 [Salt](https://salt.bountysource.com/teams/projectile).
