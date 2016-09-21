[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/projectile-badge.svg)](http://melpa.org/#/projectile)
[![MELPA Stable](http://stable.melpa.org/packages/projectile-badge.svg)](http://stable.melpa.org/#/projectile)
[![Build Status](https://travis-ci.org/bbatsov/projectile.png?branch=master)](https://travis-ci.org/bbatsov/projectile)
[![Gratipay Team](https://img.shields.io/gratipay/team/projectile.svg?maxAge=2592000)](https://gratipay.com/projectile/)

## Synopsis

[![Join the chat at https://gitter.im/bbatsov/projectile](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/bbatsov/projectile?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

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

Here's a glimpse of Projectile in action:

![Projectile Screenshot](doc/screenshots/projectile.png)

You can support my work on Projectile via
 [Salt](https://salt.bountysource.com/teams/projectile) and
 [Gratipay](https://www.gratipay.com/projectile).

[![Support via Gratipay](https://cdn.rawgit.com/gratipay/gratipay-badge/2.1.3/dist/gratipay.png)](https://gratipay.com/projectile)

## Quickstart

The instructions that follow are meant to get you from zero to a running Projectile setup
in a minute.  See the
[official manual](http://projectile.readthedocs.org/en/latest/) for (way) more
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

### Basic Usage

Enable `projectile-mode`, open a file in one of your projects and type a command such as <kbd>C-c p f</kbd>.

See the user manual for more details.

## Extensions

There are a number of packages that built on top of the basic functionality provided by Projectile:

* [counsel-projectile](https://github.com/ericdanan/counsel-projectile) provides Ivy integration
* [helm-projectile](https://github.com/bbatsov/helm-projectile) provides Helm integration
* [persp-projectile](https://github.com/bbatsov/persp-projectile) provides perspective.el integration
* [projectile-rails](https://github.com/asok/projectile-rails) provides extra functionality for Ruby on Rails projects

## Caveats

* Some operations like search (grep) depend (presently) on external
  utilities such as `find`.

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
