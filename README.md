[![Build Status](https://travis-ci.org/bbatsov/projectile.png?branch=master)](https://travis-ci.org/bbatsov/projectile)

# Synopsis

Projectile is a project interaction library for Emacs. Its goal is to
provide a nice set of features operating on a project level without
introducing external dependencies. For instance - finding project
files is done in pure Emacs Lisp without the use of GNU find.

Projectile also tries to be practical - if some external tools could
speed up some task substantially and the tools are available,
Projectile will leverage them.

This library provides easy project management and navigation. The
concept of a project is pretty basic - just a folder containing
special file. Currently `git`, `mercurial` and `bazaar` repos are
considered projects by default. If you want to mark a folder
manually as a project just create an empty `.projectile` file in
it. Some of projectile's features:

* jump to a file in project
* jump to a project buffer
* jump to a test in project
* switch between projects you have worked on
* kill all project buffers
* replace in project
* multi-occur in project buffers
* grep in project
* regenerate project etags
* visit project in dired
* run make in a project with a single key chord

# Installation

The recommended way to install Projectile is via MELPA.

## Manual

Just drop `projectile.el`,
[dash.el](https://github.com/magnars/dash.el) and
[s.el](https://github.com/magnars/s.el) somewhere in your
`load-path`. I favour the folder `~/.emacs.d/vendor`:

```lisp
(add-to-list 'load-path "~/emacs.d/vendor")
(require 'projectile)
```

## Marmalade

If you're an Emacs 24 user or you have a recent version of `package.el`
you can install Projectile from the
[Marmalade](http://marmalade-repo.org/) repository.

## MELPA

If you're an Emacs 24 user or you have a recent version of `package.el`
you can install Projectile from the
[MELPA](http://melpa.milkbox.net) repository. The version of
Projectile there will always be up-to-date, but it might be unstable
(albeit rarely).

## Emacs Prelude

Projectile is naturally part of the
[Emacs Prelude](https://github.com/bbatsov/prelude). If you're a Prelude
user - projectile is already properly configured and ready for
action.

# Usage

## Basic setup

You can enable projectile globally like this:

```lisp
(projectile-global-mode)
```

To enable projectile only in select modes:

```lisp
(add-hook 'ruby-mode-hook 'projectile-on)
```

### Caching

Since indexing a big project is not exactly quick (especially in Emacs
Lisp), projectile support caching of the project's files. The caching
is enabled on Windows by default, since on Windows native project
indexing in Emacs Lisp is the only game in town.

To enable caching unconditionally use this snippet of code:

```lisp
(setq projectile-enable-caching t)
```

At this point you can try out a projectile command such as <kbd>C-c p f</kbd> (<kbd>M-x projectile-find-file RET</kbd>).

Running <kbd>C-u C-c p f</kbd> will invalidate the cache prior to
prompting you for a file to jump to.

Pressing <kbd>C-c p z</kbd> will add the currently visited file to the
cache for current project. Generally files created outside Emacs will
be added to the cache automatically the first time you open them.

### Using Projectile everywhere

If you want Projectile to be usable in every directory (even without the presence of project file):

```lisp
(setq projectile-require-project-root nil)
```

This might not be a great idea if you start projectile in your home folder for instance :-)

### Completion Options

If you don't like ido you can use regular completion as well:

```lisp
(setq projectile-completion-system 'default)
```

You might want to combine default completion with `icomplete-mode` for optimum results.

By default, projectile only shows the path to a file if there are
other files with the same name. You can force projectile to always
show the path (relative to the project root):

```lisp
(setq projectile-show-paths-function 'projectile-hashify-with-relative-paths)
```

## Interactive Commands

Here's a list of the interactive Emacs Lisp functions, provided by projectile:

Keybinding         | Description
-------------------|------------------------------------------------------------
<kbd>C-c p f</kbd> | Display a list of all files in the project. With a prefix argument it will clear the cache first.
<kbd>C-c p T</kbd> | Display a list of all test files(specs, features, etc) in the project.
<kbd>C-c p g</kbd> | Run grep on the files in the project.
<kbd>C-c p b</kbd> | Display a list of all project buffers currently open.
<kbd>C-c p o</kbd> | Runs `multi-occur` on all project buffers currently open.
<kbd>C-c p r</kbd> | Runs interactive query-replace on all files in the projects.
<kbd>C-c p i</kbd> | Invalidates the project cache (if existing).
<kbd>C-c p R</kbd> | Regenerates the projects `TAGS` file.
<kbd>C-c p k</kbd> | Kills all project buffers.
<kbd>C-c p d</kbd> | Opens the root of the project in `dired`.
<kbd>C-c p e</kbd> | Shows a list of recently visited project files.
<kbd>C-c p a</kbd> | Runs `ack` on the project. Requires the presence of `ack-and-a-half`.
<kbd>C-c p l</kbd> | Runs a standard compilation command for your type of project.
<kbd>C-c p p</kbd> | Runs a standard test command for your type of project.
<kbd>C-c p z</kbd> | Adds the currently visited to the cache.
<kbd>C-c p s</kbd> | Display a list of known projects you can switch to.

If you ever forget any of Projectile's keybindings just do a:

<kbd>C-c p C-h</kbd>

## Ignoring files

If you'd like to instruct Projectile to ignore certain files in a
project, when indexing it you can do so in the `.projectile` file by
adding each path to ignore, where the paths all are relative to the
root directory and start with a slash. Everything ignored should be
preceded with a - sign. Alternatively, not having any prefix at all
also means to ignore the directory or file pattern that follows.
Here's an example for a typical Rails application:

```
-/log
-/tmp
-/vendor
-/public/uploads
```

This would ignore the folders only at the root of the project.
Projectile also supports relative pathname ignores:

```
-tmp
-*.rb
-*.yml
-models
```

You can also ignore everything except certain subdirectories. This is
useful when selecting the directories to keep is easier than selecting
the directories to ignore, although you can do both. To select
directories to keep, that means everything else will be ignored.
Example:

```
+/src/foo
+/tests/foo
```

Keep in mind that you can only include subdirectories, not file
patterns.

If both directories to keep and ignore are specified, the directories
to keep first apply, restricting what files are considered. The paths
and patterns to ignore are then applied to that set.


## Helm Integration

Projectile can be integrated with
[Helm](https://github.com/emacs-helm/helm) via
`helm-c-source-projectile` source (available in `helm-projectile.el`). There is also an example function
for calling Helm with the Projectile file source. You can call it like
this:

```
M-x helm-projectile
```

or even better - bind it to a keybinding like this:

```lisp
(global-set-key (kbd "C-c h") 'helm-projectile)
```

Obviously you need to have Helm installed for this to work :-)

# Caveats

* Traversing the project directory programmatically (instead of using
  something like GNU find) is not very fast. On the other hand - it's
  portable. Unlike
  [find-file-in-project](https://github.com/bbatsov/find-file-in-project),
  projectile's jump-to-file will work on any OS.
* Some operations like search(grep) depend (presently) on external
  utilities such as `find`.

## Would you like to know more?

Check out the [Projectile's project page](http://batsov.com/projectile).

## Known issues

Check out the project's
[issue list](https://github.com/bbatsov/projectile/issues?sort=created&direction=desc&state=open)
a list of unresolved issues. By the way - feel free to fix any of them
and sent me a pull request. :-)

## Contributors

Here's a [list](https://github.com/bbatsov/projectile/contributors) of all the people who have contributed to the
development of Projectile.

## Contribution

Contribution is always welcome!

Install [carton](https://github.com/rejeep/carton) if you haven't
already, then:

```bash
$ cd /path/to/projectile
$ carton
```

Run all tests with:

```bash
$ make
```

## Bugs & Improvements

Bug reports and suggestions for improvements are always
welcome. github pull requests are even better! :-)

Together we can create the ultimate project management tool for Emacs.

Cheers,<br/>
Bozhidar
