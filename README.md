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

If you'd like to enable project files caching (useful in large
projects where indexing the project's file can take a while) add this
as well:

```
(setq projectile-enable-caching t)
```

## Marmalade

If you're an Emacs 24 user or you have a recent version of package.el
you can install projectile from the [Marmalade](http://marmalade-repo.org/) repository.

## Emacs Prelude

Projectile is naturally part of the
[Emacs Prelude](https://github.com/bbatsov/prelude). If you're a Prelude
user - projectile is already properly configured and ready for
action.

# Usage

Here's a list of the interactive Emacs Lisp functions, provided by projectile:

* `projectile-find-file` (C-c p f)
* `projectile-grep` (C-c p g)
* `projectile-switch-to-buffer` (C-c p b)
* `projectile-multi-occur` (C-c p o)
* `projectile-replace` (C-c p r)
* `projectile-invalidate-cache` (C-c p i)
* `projectile-regenerate-tags` (C-c p t)
* `projectile-kill-buffers` (C-c p k)

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

```
(global-set-key (kbd "C-c h") 'helm-projectile)
```

Obviously you need to have Helm installed for this to work :-)

# Caveats

* Traversing the project directory programmatically (instead of using
  something like GNU find) is not very fast. On the other hand - it's
  portable. Unlike
  [find-file-in-project](https://github.com/bbatsov/find-file-in-project),
  projectile's jump-to-file will work on any OS.
* To compensate for the lack of speed - a cache can be created when a
  project is traversed. That cache is not automatically updated
  (presently) so you might want to invalidate it manually from time to
  time (or disable it completely for small projects).
* Some operations like find/replace depend (presently) on external
  utilities such as find and perl.

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

## Bugs & Improvements

Bug reports and suggestions for improvements are always
welcome. github pull requests are even better! :-)

Together we can create the ultimate project management tool for Emacs.

Cheers,<br>
Bozhidar
