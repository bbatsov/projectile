# Synopsis

Projectile is a project interaction library for Emacs. Its goal is to
provide a nice set of features operating on a project level without
introducing external dependencies. For instance - finding project
files is done in pure elisp without the use of GNU find.

This library provides easy project management and navigation. The
concept of a project is pretty basic - just a folder containing
special file. Currently `git`, `mercurial` and `bazaar` repos are
considered projects by default. If you want to mark a folder
manually as a project just create an empty `.projectile` file in
it. Some of projectile's features:

* jump to a file in project
* jump to a project buffer
* multi-occur in project buffers
* grep in project
* regenerate project etags

# Installation

## Manual
Just drop `projectile.el` somewhere in your `load-path`. I favour the
folder `~/.emacs.d/vendor`:

```
(add-to-list 'load-path "~/emacs.d/vendor")
```

You can enable projectile globally like this:

```
(require 'projectile)
(projectile-global-mode) ;; to enable in all buffers
```

To enable projectile only in select modes:

```
(add-hook 'ruby-mode-hook #'(lambda () (projectile-mode)))
```

## Marmalade

If you're an Emacs 24 user or you have a recent version of package.el
you can install projectile from the [Marmalade](http://marmalade-repo.org/) repository.

## Emacs Prelude

Projectile is naturally part of the
[Emacs Prelude](https://github.com/bbatsov/emacs-prelude). If you're a Prelude
user - projectile is already properly configured and ready for
action.

# Usage

Here's a list of the interactive Emacs Lisp functions, provided by projectile:

* projectile-jump-to-project-file (C-c p j)
* projectile-grep-in-project (C-c p f)
* projectile-replace-in-project (C-c p r)
* projectile-switch-to-buffer (C-c p b)
* projectile-multi-occur (C-c p o)
* projectile-regenerate-tags (C-c p t)
* projectile-invalidate-project-cache (C-c p i)

# Caveats

* Traversing the project directory programmatically (instead of using
  something like GNU find) is not very fast. On the other hand - it's
  portable. Unlike
  [find-file-in-project](https://github.com/bbatsov/find-file-in-project),
  projectile's jump-to-file will work on any OS.
* To compensate for the lack of speed - a cache is created when a
  project is traversed. That cache is not automatically updated
  (presently) so you might want to invalidate it manually from time to
  time (or disable it completely for small projects).
* Some operations like find/replace depend (presently) on external
  utilities such as find and perl.

# Contributors

None at the moment. You could be the first!
