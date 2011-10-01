# Synopsis

Projectile is a project interaction library for Emacs. Its goal is to
provide a nice set of features operating on a project level without
introducing external dependencies. For instance - finding project
files is done in pure elisp without the use of GNU find.

This library provides easy project management and navigation. The
concept of a project is pretty basic - just a folder containing
special file. Currently git, mercurial and bazaar repos are
considered projects by default. If you want to mark a folder
manually as a project just create an empty `.projectile` file in
it. Some of projectile's features:

* jump to a file in project
* jump to a project buffer
* multi-occur in project buffers
* grep in project
* regenerate project etags

# Installation

You can enable projectile globally like this:

```
(require 'projectile)
(projectile-global-mode) ;; to enable in all buffers
```

To enable projectile only in select modes:

```
(add-hook 'ruby-mode-hook #'(lambda () (projectile-mode)))
```

# Usage

Here's a list of the interactive Emacs Lisp functions, provided by projectile:

* projectile-jump-to-project-file (C-c p j)
* projectile-grep-in-project (C-c p f)
* projectile-replace-in-project (C-c p r)
* projectile-switch-to-buffer (C-c p b)
* projectile-multi-occur (C-c p o)
* projectile-regenerate-tags (C-c p t)
* projectile-invalidate-project-cache (C-c p i)

# Pitfalls



# Contributors
