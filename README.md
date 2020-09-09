# Projectile

[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/projectile-badge.svg)](http://melpa.org/#/projectile)
[![MELPA Stable](http://stable.melpa.org/packages/projectile-badge.svg)](http://stable.melpa.org/#/projectile)
[![Build Status](https://github.com/bbatsov/projectile/workflows/CI/badge.svg)](https://github.com/bbatsov/projectile/actions?query=workflow%3ACI)
[![Patreon](https://img.shields.io/badge/patreon-donate-orange.svg)](https://www.patreon.com/bbatsov)

## Synopsis

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
* check for dirty repositories
* toggle read-only mode for the entire project
* support for multiple minibuffer completion/selection libraries (e.g. `ido`, `ivy` and `helm`)

## Projectile in Action

Here's a glimpse of Projectile in action (using `ivy`):

![Projectile Demo](doc/modules/ROOT/assets/images/projectile-demo.gif)

In this short demo you can see:

* finding files in a project
* switching between implementation and test
* switching between projects

You can support my work on Projectile via
 [PayPal](https://www.paypal.me/bbatsov),
 [Patreon](https://www.patreon.com/bbatsov) and
 [GitHub Sponsors](https://github.com/sponsors/bbatsov).

## Quickstart

The instructions that follow are meant to get you from zero to a running Projectile setup
in a minute.  Visit the
[user manual](https://docs.projectile.mx) for (way) more
details.

### Installation

`package.el` is the built-in package manager in Emacs.

Projectile is available on the two major `package.el` community
maintained repos -
[MELPA Stable](http://stable.melpa.org)
and [MELPA](http://melpa.org).

You can install Projectile with the following command:

<kbd>M-x</kbd> `package-install` <kbd>[RET]</kbd> `projectile` <kbd>[RET]</kbd>

Alternatively, users of Debian 9 or later or Ubuntu 16.04 or later may
simply `apt-get install elpa-projectile`.

Finally add this to your Emacs config:

```elisp
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
```

Those keymap prefixes are just a suggestion. Feel free to put there whatever works best for you.

### Basic Usage

Enable `projectile-mode`, open a file in one of your projects and type a command such as <kbd>C-c p f</kbd>.

See the user manual for more details.

## Caveats

* Some operations like search (grep) depend (presently) on external
  utilities such as `find`.
* Using Projectile over TRAMP might be slow in certain cases.
* Some commands might misbehave on complex project setups (e.g. a git project with submodules)
* Projectile was mostly tested on Unix OS-es (e.g. GNU/Linux and macOS), so some functionality might not work well on Windows

## Known issues

Check out the project's
[issue list](https://github.com/bbatsov/projectile/issues?sort=created&direction=desc&state=open)
a list of unresolved issues. By the way - feel free to fix any of them
and sent me a pull request. :-)

## Contributors

Here's a [list](https://github.com/bbatsov/projectile/contributors) of all the people who have contributed to the
development of Projectile.

## Changelog

A fairly extensive changelog is available [here](CHANGELOG.md).

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg

## License

Copyright © 2011-2020 Bozhidar Batsov and
[contributors](https://github.com/bbatsov/projectile/contributors).

Distributed under the GNU General Public License, version 3
