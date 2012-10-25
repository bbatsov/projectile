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
* run make in a project with a single key chord

# Installation

## Manual

Just drop `projectile.el` and [s.el](https://github.com/magnars/s.el)
somewhere in your `load-path`. I favour the folder
`~/.emacs.d/vendor`:

```lisp
(add-to-list 'load-path "~/emacs.d/vendor")
(require 'projectile)
```

## Marmalade

If you're an Emacs 24 user or you have a recent version of `package.el`
you can install projectile from the
[Marmalade](http://marmalade-repo.org/) repository.

## MELPA

If you're an Emacs 24 user or you have a recent version of `package.el`
you can install projectile from the
[MELPA](http://melpa.milkbox.net) repository. The version of
projectile there will always be up-to-date, but it might be unstable
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

Since indexing a big project is not exactly quick in Emacs Lisp,
projectile caches the project's files automatically. This means you'll
have to invalidate the cache from time to time when new files are
added to the project. If you're working
on a smaller project and you don't mind the reindexing delay you may
disable the cache:

```lisp
(setq projectile-enable-caching nil)
```

At this point you can try out a projectile command such as `C-c p f` (`projectile-find-file`).

If you want Projectile to be usable in every directory (even without the presence of project file):

```lisp
(setq projectile-require-project-file nil)
```

This might not be a great idea if you start projectile in your home folder for instance :-)

If you don't like ido you can use regular completion as well:

```lisp
(setq projectile-completion-system 'default)
```

You might want to combine default completion with `icomplete-mode` for optimum results.

## Interactive Commands

Here's a list of the interactive Emacs Lisp functions, provided by projectile:

* `projectile-find-file` (C-c p f)
* `projectile-grep` (C-c p g)
* `projectile-switch-to-buffer` (C-c p b)
* `projectile-multi-occur` (C-c p o)
* `projectile-replace` (C-c p r)
* `projectile-invalidate-cache` (C-c p i)
* `projectile-regenerate-tags` (C-c p t)
* `projectile-kill-buffers` (C-c p k)
* `projectile-dired` (C-c p d)
* `projectile-recentf` (C-c p e)
* `projectile-ack` (C-c p a)
* `projectile-compile-project` (C-c p l)
* `projectile-test-project` (C-c p p)

## Ignoring files

If you'd like to instruct Projectile to ignore certain files in a
project, when indexing it you can do so in the `.projectile`
file. Here's an example for a typical Rails application:

```
log
tmp
vendor
public/uploads
```

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

## Contribution

Contribution is always welcome!

Install [carton](https://github.com/rejeep/carton) if you haven't
already, then:

    $ cd /path/to/projectile
    $ carton

Run all tests with:

    $ make


## Bugs & Improvements

Bug reports and suggestions for improvements are always
welcome. github pull requests are even better! :-)

Together we can create the ultimate project management tool for Emacs.

Cheers,<br/>
Bozhidar
