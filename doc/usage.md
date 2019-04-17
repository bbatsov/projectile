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

!!! Tip

    If you're going to use the default `ido` completion it's
    **extremely highly** recommended that you install the optional
    [flx-ido package](https://github.com/lewang/flx), which provides a much
    more powerful alternative to `ido`'s built-in `flex` matching.

Check out the ["Configuration"](configuration.md) section of the manual
for a lot more information about configuring Projectile.

## Basic Usage

Just open some file in a version-controlled (e.g. `git`) or a project
(e.g. `maven`) directory that's recognized by Projectile and you're
ready for action. Projectile happens to recognize out of the box every common
VCS and many popular project types for various programming languages.
You can learn more about Projectile's notion of a project [here](projects.md).

!!! Note

    The extent of the support for every VCS differs and Git is the best supported
    one. Projectile supports some advanced features like working with Git submodules
    and using `git-grep` instead GNU grep.

You need to know only a handful of Projectile commands to start benefiting from it.

* Find file in current project (<kbd>s-p f</kbd>)
* Switch project (<kbd>s-p p</kbd>)
* Grep in project (<kbd>s-p s g</kbd>)
* Replace in project (<kbd>s-p r</kbd>)
* Invoke a command via the Projectile Commander (<kbd>s-p m</kbd>)

The next section lists many more commands, but the basics can get you pretty far.

## Interactive Commands

!!! Note

    Projectile doesn't have a default key prefix for its commands, but all the examples
    in the manual assume you've opted for `s-p` (`super`-p).

Here's a list of the interactive Emacs Lisp functions, provided by Projectile:

Keybinding         | Description
-------------------|------------------------------------------------------------
<kbd>s-p f</kbd> | Display a list of all files in the project. With a prefix argument it will clear the cache first.
<kbd>s-p F</kbd> | Display a list of all files in all known projects.
<kbd>s-p g</kbd> | Display a list of all files at point in the project. With a prefix argument it will clear the cache first.
<kbd>s-p 4 f</kbd> | Jump to a project's file using completion and show it in another window.
<kbd>s-p 4 g</kbd> | Jump to a project's file based on context at point and show it in another window.
<kbd>s-p 5 f</kbd> | Jump to a project's file using completion and show it in another frame.
<kbd>s-p 5 g</kbd> | Jump to a project's file based on context at point and show it in another frame.
<kbd>s-p d</kbd> | Display a list of all directories in the project. With a prefix argument it will clear the cache first.
<kbd>s-p 4 d</kbd> | Switch to a project directory and show it in another window.
<kbd>s-p 5 d</kbd> | Switch to a project directory and show it in another frame.
<kbd>s-p T</kbd> | Display a list of all test files(specs, features, etc) in the project.
<kbd>s-p l</kbd> | Display a list of all files in a directory (that's not necessarily a project)
<kbd>s-p s g</kbd> | Run grep on the files in the project.
<kbd>M-- s-p s g</kbd> | Run grep on `projectile-grep-default-files` in the project.
<kbd>s-p v</kbd> | Run `vc-dir` on the root directory of the project.
<kbd>s-p V</kbd> | Browse dirty version controlled projects.
<kbd>s-p b</kbd> | Display a list of all project buffers currently open.
<kbd>s-p 4 b</kbd> | Switch to a project buffer and show it in another window.
<kbd>s-p 5 b</kbd> | Switch to a project buffer and show it in another frame.
<kbd>s-p 4 C-o</kbd> | Display a project buffer in another window without selecting it.
<kbd>s-p a</kbd> | Switch between files with the same name but different extensions.
<kbd>s-p 4 a</kbd> | Switch between files with the same name but different extensions in other window.
<kbd>s-p 5 a</kbd> | Switch between files with the same name but different extensions in other frame.
<kbd>s-p o</kbd> | Runs `multi-occur` on all project buffers currently open.
<kbd>s-p r</kbd> | Runs interactive query-replace on all files in the projects.
<kbd>s-p i</kbd> | Invalidates the project cache (if existing).
<kbd>s-p R</kbd> | Regenerates the projects `TAGS` file.
<kbd>s-p j</kbd> | Find tag in project's `TAGS` file.
<kbd>s-p k</kbd> | Kills all project buffers.
<kbd>s-p D</kbd> | Opens the root of the project in `dired`.
<kbd>s-p 4 D</kbd> | Opens the root of the project in `dired` in another window.
<kbd>s-p 5 D</kbd> | Opens the root of the project in `dired` in another frame.
<kbd>s-p e</kbd> | Shows a list of recently visited project files.
<kbd>s-p left</kbd> | Switch to the previous project buffer.
<kbd>s-p right</kbd> | Switch to the next project buffer.
<kbd>s-p E</kbd> | Opens the root `dir-locals-file` of the project.
<kbd>s-p s s</kbd> | Runs `ag` on the project, performing a literal search. Requires the presence of `ag.el`. With a prefix argument it will perform a regex search.
<kbd>s-p !</kbd> | Runs `shell-command` in the root directory of the project.
<kbd>s-p &</kbd> | Runs `async-shell-command` in the root directory of the project.
<kbd>s-p C</kbd> | Runs a standard configure command for your type of project.
<kbd>s-p c</kbd> | Runs a standard compilation command for your type of project.
<kbd>s-p P</kbd> | Runs a standard test command for your type of project.
<kbd>s-p t</kbd> | Toggle between an implementation file and its test file.
<kbd>s-p 4 t</kbd> | Jump to implementation or test file in other window.
<kbd>s-p 5 t</kbd> | Jump to implementation or test file in other frame.
<kbd>s-p z</kbd> | Adds the currently visited file to the cache.
<kbd>s-p p</kbd> | Display a list of known projects you can switch to.
<kbd>s-p S</kbd> | Save all project buffers.
<kbd>s-p m</kbd> | Run the commander (an interface to run commands with a single key).
<kbd>s-p ESC</kbd> | Switch to the most recently selected Projectile buffer.

If you ever forget any of Projectile's keybindings just do a:

<kbd>s-p C-h</kbd>

It is possible to add additional commands to
`projectile-command-map` referenced by the prefix key in
`projectile-mode-map`. You can add multiple keymap prefix for all
commands. Here's an example that adds `super-,` as a command prefix:

```el
(define-key projectile-mode-map (kbd "s-,") 'projectile-command-map)
```

You can also bind the `projectile-command-map` to any other map you'd
like (including the global keymap).

!!! Tip

    For some common commands you might want to take a little shortcut and
    leverage the fairly unused `Super` key (by default `Command` on Mac
    keyboards and `Windows` on Win keyboards).

Here's something you can
add to your Emacs config:

```el
(define-key projectile-mode-map [?\s-d] 'projectile-find-dir)
(define-key projectile-mode-map [?\s-p] 'projectile-switch-project)
(define-key projectile-mode-map [?\s-f] 'projectile-find-file)
(define-key projectile-mode-map [?\s-g] 'projectile-grep)
```

!!! Note

    Note that the `Super` keybindings are not usable in Windows, as Windows
    makes heavy use of such keybindings itself. Emacs
    Prelude already adds those extra keybindings.
