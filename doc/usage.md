## Basic setup

!!! Note

    Everything in this section assumes you've enabled `projectile-mode`.

To add a project to Projectile's list of known projects, open a file
in the project. If you have a projects directory, you can tell
Projectile about all of the projects in it with the command `M-x
projectile-discover-projects-in-directory`.

You can go one step further and set a list of folders which Projectile
is automatically going to check for projects:

```el
(setq projectile-project-search-path '("~/projects/" "~/work/"))
```

If you're going to use the default `ido` completion it's
**extremely highly** recommended that you install the optional
[flx-ido package](https://github.com/lewang/flx), which provides a much
more powerful alternative to `ido`'s built-in `flex` matching.

## Interactive Commands

Here's a list of the interactive Emacs Lisp functions, provided by Projectile:

Keybinding         | Description
-------------------|------------------------------------------------------------
<kbd>C-c C-p f</kbd> | Display a list of all files in the project. With a prefix argument it will clear the cache first.
<kbd>C-c C-p F</kbd> | Display a list of all files in all known projects.
<kbd>C-c C-p g</kbd> | Display a list of all files at point in the project. With a prefix argument it will clear the cache first.
<kbd>C-c C-p 4 f</kbd> | Jump to a project's file using completion and show it in another window.
<kbd>C-c C-p 4 g</kbd> | Jump to a project's file based on context at point and show it in another window.
<kbd>C-c C-p 5 f</kbd> | Jump to a project's file using completion and show it in another frame.
<kbd>C-c C-p 5 g</kbd> | Jump to a project's file based on context at point and show it in another frame.
<kbd>C-c C-p d</kbd> | Display a list of all directories in the project. With a prefix argument it will clear the cache first.
<kbd>C-c C-p 4 d</kbd> | Switch to a project directory and show it in another window.
<kbd>C-c C-p 5 d</kbd> | Switch to a project directory and show it in another frame.
<kbd>C-c C-p T</kbd> | Display a list of all test files(specs, features, etc) in the project.
<kbd>C-c C-p l</kbd> | Display a list of all files in a directory (that's not necessarily a project)
<kbd>C-c C-p s g</kbd> | Run grep on the files in the project.
<kbd>M-- C-c C-p s g</kbd> | Run grep on `projectile-grep-default-files` in the project.
<kbd>C-c C-p v</kbd> | Run `vc-dir` on the root directory of the project.
<kbd>C-c C-p V</kbd> | Browse dirty version controlled projects.
<kbd>C-c C-p b</kbd> | Display a list of all project buffers currently open.
<kbd>C-c C-p 4 b</kbd> | Switch to a project buffer and show it in another window.
<kbd>C-c C-p 5 b</kbd> | Switch to a project buffer and show it in another frame.
<kbd>C-c C-p 4 C-o</kbd> | Display a project buffer in another window without selecting it.
<kbd>C-c C-p a</kbd> | Switch between files with the same name but different extensions.
<kbd>C-c C-p 4 a</kbd> | Switch between files with the same name but different extensions in other window.
<kbd>C-c C-p 5 a</kbd> | Switch between files with the same name but different extensions in other frame.
<kbd>C-c C-p o</kbd> | Runs `multi-occur` on all project buffers currently open.
<kbd>C-c C-p r</kbd> | Runs interactive query-replace on all files in the projects.
<kbd>C-c C-p i</kbd> | Invalidates the project cache (if existing).
<kbd>C-c C-p R</kbd> | Regenerates the projects `TAGS` file.
<kbd>C-c C-p j</kbd> | Find tag in project's `TAGS` file.
<kbd>C-c C-p k</kbd> | Kills all project buffers.
<kbd>C-c C-p D</kbd> | Opens the root of the project in `dired`.
<kbd>C-c C-p 4 D</kbd> | Opens the root of the project in `dired` in another window.
<kbd>C-c C-p 5 D</kbd> | Opens the root of the project in `dired` in another frame.
<kbd>C-c C-p e</kbd> | Shows a list of recently visited project files.
<kbd>C-c C-p E</kbd> | Opens the root `dir-locals-file` of the project.
<kbd>C-c C-p s s</kbd> | Runs `ag` on the project. Requires the presence of `ag.el`.
<kbd>C-c C-p !</kbd> | Runs `shell-command` in the root directory of the project.
<kbd>C-c C-p &</kbd> | Runs `async-shell-command` in the root directory of the project.
<kbd>C-c C-p C</kbd> | Runs a standard configure command for your type of project.
<kbd>C-c C-p c</kbd> | Runs a standard compilation command for your type of project.
<kbd>C-c C-p P</kbd> | Runs a standard test command for your type of project.
<kbd>C-c C-p t</kbd> | Toggle between an implementation file and its test file.
<kbd>C-c C-p 4 t</kbd> | Jump to implementation or test file in other window.
<kbd>C-c C-p 5 t</kbd> | Jump to implementation or test file in other frame.
<kbd>C-c C-p z</kbd> | Adds the currently visited file to the cache.
<kbd>C-c C-p p</kbd> | Display a list of known projects you can switch to.
<kbd>C-c C-p S</kbd> | Save all project buffers.
<kbd>C-c C-p m</kbd> | Run the commander (an interface to run commands with a single key).
<kbd>C-c C-p ESC</kbd> | Switch to the most recently selected Projectile buffer.

If you ever forget any of Projectile's keybindings just do a:

<kbd>C-c C-p C-h</kbd>

You can change the default keymap prefix `C-c C-p` like this:

```el
(setq projectile-keymap-prefix (kbd "C-c p"))
```

!!! Note

    `C-c p` used to be the default prefix up to version 1.1.

It is also possible to add additional commands to
`projectile-command-map` referenced by the prefix key in
`projectile-mode-map`. You can even add an alternative prefix for all
commands. Here's an example that adds `super-p` as the extra prefix:

```el
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
```

You can also bind the `projectile-command-map` to any other map you'd
like (including the global keymap).

For some common commands you might want to take a little shortcut and
leverage the fairly unused `Super` key (by default `Command` on Mac
keyboards and `Windows` on Win keyboards). Here's something you can
add to your Emacs config:

```el
(define-key projectile-mode-map [?\s-d] 'projectile-find-dir)
(define-key projectile-mode-map [?\s-p] 'projectile-switch-project)
(define-key projectile-mode-map [?\s-f] 'projectile-find-file)
(define-key projectile-mode-map [?\s-g] 'projectile-grep)
```

Note that the `Super` keybindings are not usable in Windows. Emacs
Prelude already adds those extra keybindings.

## Ignoring files

If you'd like to instruct Projectile to ignore certain files in a
project, when indexing it you can do so in the `.projectile` file by
adding each path to ignore, where the paths all are relative to the
root directory and start with a slash. Everything ignored should be
preceded with a `-` sign. Alternatively, not having any prefix at all
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

Finally, you can override ignored files. This is especially useful
when some files ignored by your VCS should be considered as part of
your project by projectile:

```
!/src/foo
!*.yml
```

When a path is overridden, its contents are still subject to ignore
patterns. To override those files as well, specify their full path
with a bang prefix.
