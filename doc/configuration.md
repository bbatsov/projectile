In the typical style of Emacs, Projectile is **extremely** configurable.
Pretty much every aspect of its behaviour can be tweaked or extended.

In this section we'll go over some of the most common things you might
want to fine-tune to make Projectile fit your workflow better.

## Project indexing method

Projectile has three modes of operation - one is portable and is
implemented in Emacs Lisp (therefore it's *native* to Emacs and is
known as the `native indexing method`) and the other two (`hybrid` and
`alien`) rely on external commands like `find`, `git`, etc to
obtain the list of files in a project.

The `alien` indexing method optimizes to the limit the speed of
the `hybrid` indexing method.  This means that Projectile will not do
any processing or sorting of the files returned by the external commands
and you're going to get the maximum performance possible.  This behaviour
makes a lot of sense for most people, as they'd typically be putting
ignores in their VCS config (e.g. `.gitignore`) and won't care about
any additional ignores/unignores/sorting that Projectile might also
provide.

!!! Info

    By default the `alien` method is used on all operating systems except Windows.
    Prior to Projectile 2.0 `hybrid` used to be the default (but to make things
    confusing `hybrid` used to be known as `alien` back then).

To force the
use of native indexing in all operating systems:

```el
(setq projectile-indexing-method 'native)
```

To force the use of hybrid indexing in all operating systems:

```el
(setq projectile-indexing-method 'hybrid)
```

To force the use of alien indexing in all operating systems:

```el
(setq projectile-indexing-method 'alien)
```

This can speed up Projectile in Windows significantly (especially on
big projects). The disadvantage of this method is that it's not well
supported on Windows systems, as it requires setting up some Unix
utilities there. If there's problem, you can always use `native`
indexing mode.

### Alien indexing

The alien indexing works in a pretty simple manner - it simply shells
out to a command that returns the list of files within a project.
For version-controlled projects by default Projectile will use the
VCS itself to obtain the list of files. As an example, here is the
command that Projectile uses for Git projects:

```
git ls-files -zco --exclude-standard
```

For every supported VCS there's a matching Projectile defcustom holding the command
to invoke for it (e.g. `projectile-git-command`, `projectile-hg-command`, etc).

!!! Warning

    If you ever decide to tweak those keep in mind that the command should always be returning
    the list of files **relative** to the project root and the resulting file list should be 0-delimited
    (as opposed to newline delimited).

For non-VCS projects Projectile will invoke whatever is in `projectile-generic-command`. By default that's:

```
find . -type f -print0
```

!!! Tip

    It's a great idea to install [fd](https://github.com/sharkdp/fd) and use it as a replacement for both `git ls-files` (`fd` understands `.gitignore`) and `find`.
    The magic command you'll need with it is something like `fd . -0`.

## Sorting

You can choose how Projectile sorts files by customizing `projectile-sort-order`.

!!! Info

    Note that if Alien indexing is set, files are not sorted by Projectile at all.

The default is to not sort files:

```el
(setq projectile-sort-order 'default)
```

To sort files by recently opened:

```el
(setq projectile-sort-order 'recentf)
```

To sort files by recently active buffers and then recently opened files:

```el
(setq projectile-sort-order 'recently-active)
```

<!-- These URLs below are in HTML so that the parentheses in the URL fragments are properly recognised. -->

To sort files by <a href="https://en.wikipedia.org/wiki/MAC_times#Modification_time_(mtime)">modification time</a> (mtime):

```el
(setq projectile-sort-order 'modification-time)
```

To sort files by <a href="https://en.wikipedia.org/wiki/MAC_times#Access_time_(atime)">access time</a> (atime):

```el
(setq projectile-sort-order 'access-time)
```


## Caching

### Project files

Since indexing a big project is not exactly quick (especially in Emacs
Lisp), Projectile supports caching of the project's files. The caching
is enabled by default whenever native indexing is enabled.

To enable caching unconditionally use this snippet of code:

```el
(setq projectile-enable-caching t)
```

At this point you can try out a Projectile command such as <kbd>s-p f</kbd> (<kbd>M-x projectile-find-file RET</kbd>).

Running <kbd>C-u s-p f</kbd> will invalidate the cache prior to
prompting you for a file to jump to.

Pressing <kbd>s-p z</kbd> will add the currently visited file to the
cache for current project. Generally files created outside Emacs will
be added to the cache automatically the first time you open them.

The project cache is persistent and will be preserved during Emacs restarts.

You can purge an individual file from the cache with `M-x projectile-purge-file-from-cache` or an
entire directory with `M-x projectile-purge-dir-from-cache`.

### File exists cache

Projectile does many file existence checks since that is how it identifies a
project root. Normally this is fine, however in some situations the file system
speed is much slower than usual and can make emacs "freeze" for extended
periods of time when opening files and browsing directories.

The most common example would be interfacing with remote systems using
TRAMP/ssh. By default all remote file existence checks are cached

To disable remote file exists cache that use this snippet of code:

```el
(setq projectile-file-exists-remote-cache-expire nil)
```

To change the remote file exists cache expire to 10 minutes use this snippet
of code:

```el
(setq projectile-file-exists-remote-cache-expire (* 10 60))
```

You can also enable the cache for local file systems, that is normally not
needed but possible:

```el
(setq projectile-file-exists-local-cache-expire (* 5 60))
```

## Using Projectile everywhere

If you want Projectile to be usable in every directory (even without the presence of project file):

```el
(setq projectile-require-project-root nil)
```

!!! Tip

    This might not be a great idea if you start Projectile in your home folder for instance. :-)

## Switching projects

When running `projectile-switch-project` (<kbd>s-p p</kbd>) Projectile invokes
the command specified in `projectile-switch-project-action` (by default it is
`projectile-find-file`).

!!! Tip

    Invoking the command with a prefix argument (<kbd>C-u s-p p</kbd>) will trigger
    the Projectile Commander, which gives you quick access to most common commands
    you might want to invoke on a project.

Depending on your personal workflow and habits, you
may prefer to alter the value of `projectile-switch-project-action`:

### `projectile-find-file`

This is the default.  With this setting, once you have selected your
project via Projectile's completion system (see below), you will
remain in the completion system to select a file to visit. `projectile-find-file`
is capable of retrieving files in all sub-projects under the project root,
such as Git submodules. Currently, only Git is supported. Support for other VCS
will be added in the future.

### `projectile-find-file-in-known-projects`

Similar to `projectile-find-file` but lists all files in all known projects. Since
the total number of files could be huge, it is beneficial to enable caching for subsequent
usages.

### `projectile-find-file-dwim`

If point is on a filepath, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This
works even if the filename is incomplete, but there's only a single file
in the current project that matches the filename at point. For example,
if there's only a single file named "projectile/projectile.el" but the
current filename is "projectile/proj" (incomplete), projectile-find-file
still switches to "projectile/projectile.el" immediately because this
is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting. A
list of files is displayed when a filename appears more than one in the
project or the filename at point is a prefix of more than two files in a
project. For example, if `projectile-find-file' is executed on a
filepath like "projectile/", it lists the content of that directory.
If it is executed on a partial filename like "projectile/a", a list of
files with character 'a' in that directory is presented.

- If it finds nothing, display a list of all files in project for
  selecting.

### `projectile-dired`

```el
(setq projectile-switch-project-action #'projectile-dired)
```

With this setting, once you have selected your project, the top-level
directory of the project is immediately opened for you in a dired
buffer.

### `projectile-find-dir`

```el
(setq projectile-switch-project-action #'projectile-find-dir)
```

With this setting, once you have selected your project, you will
remain in Projectile's completion system to select a sub-directory of
your project, and then *that* sub-directory is opened for you in a
dired buffer.  If you use this setting, then you will probably also
want to set

```el
(setq projectile-find-dir-includes-top-level t)
```

in order to allow for the occasions where you want to select the
top-level directory.

## Completion Options

### Ido

By default Projectile uses `ido` as its completion system. `ido` is
extremely popular and it is built into Emacs.


!!! Tip

    As already noted above if you're going to use the `ido` completion it's
    **extremely highly** recommended that you install the optional
    [flx-ido package](https://github.com/lewang/flx), which provides a much
    more powerful alternative to `ido`'s built-in `flex` matching.

### Ivy (recommended)

Another completion option is [ivy](https://github.com/abo-abo/swiper):

```el
(setq projectile-completion-system 'ivy)
```

### Basic (Emacs's default)

If you don't like `ido` and `ivy` you can use regular completion:

```el
(setq projectile-completion-system 'default)
```

You might want to combine default completion with `icomplete-mode` for optimum results.

### Custom Completion Function

You can also set `projectile-completion-system` to a function:

```el
(setq projectile-completion-system #'my-custom-completion-fn)
(setq projectile-completion-system
      (lambda (prompt choices)
        ;; ...
        ))
```

An example of a custom completion function is
[this one](https://gist.github.com/rejeep/5933343), which only show
the file name (not including path) and if the file selected is not
unique, another completion with names relative to project root
appears.

## Regenerate tags

To be able to regenerate a project's tags via `projectile-tags-command`, you
should install and add to the PATH
[Exuberant Ctags](http://ctags.sourceforge.net/) instead of a plain ctags, which
ships with Emacs distribution.

## Idle Timer

Projectile can be configured to run the hook
`projectile-idle-timer-hook` every time Emacs is in a project and has
been idle for `projectile-idle-timer-seconds` seconds (default is 30
seconds).  To enable this feature, run:

```
M-x customize-group RET projectile RET
```

and set `projectile-enable-idle-timer` to non-nil.  By default,
`projectile-idle-timer-hook` runs `projectile-regenerate-tags`.  Add
additional functions to the hook using `add-hook`:

```el
(add-hook 'projectile-idle-timer-hook #'my-projectile-idle-timer-function)
```

## Mode line indicator

By default the minor mode indicator of Projectile appears in the form
" Projectile[ProjectName:ProjectType]". This is configurable via several custom variables:

* `projectile-mode-line-prefix` (by default " Projectile") controls the static part of the mode-line
* `projectile-dynamic-mode-line` (by default `t`) controls whether to display the project name & type part of the mode-line
* `projectile-mode-line-function` (by default `projectile-default-mode-line`) controls the actual function to be invoked to generate the mode-line. If you'd like to show different info you should supply a custom function to replace the default, for example `(setq projectile-mode-line-function '(lambda () (format " Proj[%s]" (projectile-project-name))))`

!!! Note

    The project name & type will not appear when editing remote files
    (via TRAMP), as recalculating the project name is a fairly slow operation there
    and would slow down a bit opening the files. They will also not appear for
    non-file buffers, as they get updated via `find-file-hook`.
