#### Indexing method

Projectile has two modes of operation - one is portable and is
implemented in Emacs Lisp(therefore it's native to Emacs and is known
as the `native indexing method`) and the other relies on external
commands like `find`, `git`, etc to obtain the list of files in a
project.

Since the native indexing mode is much slower, by default the second
method is used on all operating systems except Windows. To force the
use of native indexing in operating systems other than Windows:

```el
(setq projectile-indexing-method 'native)
```

To force the use of external indexing in Windows:

```el
(setq projectile-indexing-method 'alien)
```

This can speed up Projectile in Windows significantly. The disadvantage of this
method is that it's not well supported on Windows systems. If there's problem,
you can always use native indexing mode.

#### Caching

##### Project files

Since indexing a big project is not exactly quick (especially in Emacs
Lisp), Projectile supports caching of the project's files. The caching
is enabled by default whenever native indexing is enabled.

To enable caching unconditionally use this snippet of code:

```el
(setq projectile-enable-caching t)
```

At this point you can try out a Projectile command such as <kbd>C-c p f</kbd> (<kbd>M-x projectile-find-file RET</kbd>).

Running <kbd>C-u C-c p f</kbd> will invalidate the cache prior to
prompting you for a file to jump to.

Pressing <kbd>C-c p z</kbd> will add the currently visited file to the
cache for current project. Generally files created outside Emacs will
be added to the cache automatically the first time you open them.

The project cache is persistent and will be preserved during Emacs restarts.

You can purge an individual file from the cache with `M-x projectile-purge-file-from-cache` or an
entire directory with `M-x projectile-purge-dir-from-cache`.

##### File exists cache

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

#### Using Projectile everywhere

If you want Projectile to be usable in every directory (even without the presence of project file):

```el
(setq projectile-require-project-root nil)
```

This might not be a great idea if you start Projectile in your home folder for instance. :-)

#### Switching projects

When running `projectile-switch-project` (<kbd>C-c p p</kbd>) Projectile invokes
the command specified in `projectile-switch-project-action` (by default it is
`projectile-find-file`).

Depending on your personal workflow and habits, you
may prefer to alter the value of `projectile-switch-project-action`:

###### `projectile-find-file`

This is the default.  With this setting, once you have selected your
project via Projectile's completion system (see below), you will
remain in the completion system to select a file to visit. `projectile-find-file`
is capable of retrieving files in all sub-projects under the project root,
such as Git submodules. Currently, only Git is supported. Support for other VCS
will be added in the future.

###### `projectile-find-file-in-known-projects`

Similar to `projectile-find-file` but lists all files in all known projects. Since
the total number of files could be huge, it is beneficial to enable caching for subsequent
usages.

###### `projectile-find-file-dwim`

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

###### `projectile-dired`

```el
(setq projectile-switch-project-action #'projectile-dired)
```

With this setting, once you have selected your project, the top-level
directory of the project is immediately opened for you in a dired
buffer.

###### `projectile-find-dir`

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

#### Completion Options

##### Ido

By default Projectile uses `ido` as its completion system. `ido` is
extremely popular and it is built into Emacs.

As already noted above if you're going to use the `ido` completion it's
**extremely highly** recommended that you install the optional
[flx-ido package](https://github.com/lewang/flx), which provides a much
more powerful alternative to `ido`'s built-in `flex` matching.

##### Grizzl

Another completion option is [grizzl](https://github.com/grizzl/grizzl):

```el
(setq projectile-completion-system 'grizzl)
```

![Projectile Screenshot](screenshots/projectile-grizzl.png)

`grizzl`'s advantage is that it provides good fuzzy completion
(compared to `ido`'s less than stellar built-in flex matching, but inferior to `ido-flx`).

##### Basic (Emacs's default)

If you don't like `ido` and `grizzl` you can use regular completion:

```el
(setq projectile-completion-system 'default)
```

You might want to combine default completion with `icomplete-mode` for optimum results.

##### Custom Completion Function

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

##### Regenerate tags

To be able to regenerate a project's tags via `projectile-tags-command`, you
should install and add to the PATH
[Exuberant Ctags](http://ctags.sourceforge.net/) instead of a plain ctags, which
ships with Emacs distribution.

### Adding Custom Project Types

If a project you are working on is recognized incorrectly or you want to add your own type of projects you can add following to your Emacs initialization code

```el
(projectile-register-project-type 'npm '("package.json")
				  :compile "npm build"
				  :test "npm test"
				  :run "npm start"
				  :test-suffix ".spec")
```
What this does is:
1. add your own type of project, in this case `npm` package.
2. add a file in a root of the project that helps to identify the type, in this case it is `package.json`.
3. add *compile-command*, in this case it is `npm build`.
4. add *test-command*, in this case it is `npm test`.
5. add *run-command*, in this case it is `npm start`.
6. add test files suffix for toggling between implementation/test files, in this case it is `.spec`, so the implementation/test file pair could be `service.js`/`service.spec.js` for example.

### Customizing project root files

You can set the values of `projectile-project-root-files`,
`projectile-project-root-files-top-down-recurring`,
`projectile-project-root-files-bottom-up` and
`projectile-project-root-files-functions` to customize how project roots are
identified.

To customize project root files settings:

```
M-x customize-group RET projectile RET
```

#### File-local project root definitions

If you want to override the projectile project root for a specific
file, you can set the file-local variable `projectile-project-root`. This
can be useful if you have files within one project that are related to
a different project (for instance, Org files in one git repo that
correspond to other projects).

### Storing project settings

From project to project, some things may differ even in the same
language - coding styles, auto-completion sources, etc.  If you need
to set some variables according to the selected project, you can use a
standard Emacs feature called
[Per-directory Local Variables](http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html).
To use it you must create a file named `.dir-locals.el` (as specified
by the constant `dir-locals-file`) inside the project directory.  This
file should contain something like this:

```el
((nil . ((secret-ftp-password . "secret")
         (compile-command . "make target-x")
         (eval . (progn
                   (defun my-project-specific-function ()
                     ;; ...
                     )))))
 (c-mode . ((c-file-style . "BSD"))))
```

The top-level alist member referenced with the key `nil` applies to
the entire project.  A key with the name `eval` will evaluate its
corresponding value.  In the example above, this is used to create a
function.  It could also be used to e.g. add such a function to a key
map.

You can also quickly visit or create the `dir-locals-file` with
<kbd>C-c p E</kbd> (<kbd>M-x projectile-edit-dir-locals RET</kbd>).

Here are a few examples of how to use this feature with Projectile.

#### Configuring Projectile's Behavior

Projectile exposes many variables (via `defcustom`) which allow users
to customize its behavior.  Directory variables can be used to set
these customizations on a per-project basis.

You could enable caching for a project in this way:

```el
((nil . ((projectile-enable-caching . t))))
```

If one of your projects had a file that you wanted Projectile to
ignore, you would customize Projectile by:

```el
((nil . ((projectile-globally-ignored-files . ("MyBinaryFile")))))
```

If you wanted to wrap the git command that Projectile uses to list
the files in you repository, you could do:

```el
((nil . ((projectile-git-command . "/path/to/other/git ls-files -zco --exclude-standard"))))
```

If you want to use a different project name than how Projectile named
your project, you could customize it with the following:

```el
((nil . ((projectile-project-name . "your-project-name-here"))))
```

#### Configure a Project's Compilation, Test and Run commands

There are a few variables that are intended to be customized via `.dir-locals.el`.

* for compilation - `projectile-project-compilation-cmd`
* for testing - `projectile-project-test-cmd`
* for running - `projectile-project-run-cmd`

When these variables have their default value of `nil`, Projectile
runs the default command for the current project type.  You can
override this behavior by setting them to either a string to run an
external command or an Emacs Lisp function:

```el
(setq projectile-test-cmd #'custom-test-function)
```

### Idle Timer

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

### Mode line indicator

By default the minor mode indicator of Projectile appears in the form
" Projectile[ProjectName]". This is configurable via the custom variable
`projectile-mode-line`, which expects a sexp like
`'(:eval (format " Proj[%s]" (projectile-project-name)))`.

The project name will not appear by default when editing remote files
(via TRAMP), as recalculating the project name (this is done on every
keystroke) is a fairly slow operation there.
