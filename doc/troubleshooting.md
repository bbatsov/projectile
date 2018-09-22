In case you run into issues here are a few tips that can help you diagnose the
problem.

Generally, it's not a bad idea to configure Emacs to spit the backtrace on error
(instead of just logging the error in the `*Messages*` buffer). You can toggle
this behavior by using <kbd>M-x</kbd> `toggle-debug-on-error`.

## Debugging Projectile commands

Emacs features a super powerful built-in
[Emacs Lisp debugger](http://www.gnu.org/software/emacs/manual/html_node/elisp/Edebug.html)
and using it is the best way to diagnose problems of any kind.

!!! Tip

    Here's a [great crash course](https://www.youtube.com/watch?v=odkYXXYOxpo) on
    using the debugger.

To debug some command you need to do the following:

* Figure out the name of the command you want to debug (e.g. by using <kbd>C-h k</kbd>
to see which command is associated with some keybinding)
* Find the source of the command (e.g. by using <kbd>M-x</kbd> `find-function`
  <kbd>RET</kbd> `function-name`)
* Press <kbd>C-u C-M-x</kbd> while in the body of the function
* Run the command again

At this point you'll be dropped in the debugger and you can step forward until
you find the problem.

## Profiling Projectile commands

Emacs comes with a [built-in
profiler](https://www.gnu.org/software/emacs/manual/html_node/elisp/Profiling.html). Using
it is pretty simple:

1. Start it with <kbd>M-x</kbd> `profiler-start`.
2. Invoke some commands.
3. Get the report with <kbd>M-x</kbd> `profiler-report`.

!!! Tip

    If you intend to share the profiling results with someone it's a good idea to
    save the report buffer to a file with <kbd>C-x C-w</kbd>.

## Commonly encountered problems (and how to solve them)

Sometimes a Projectile command might hang for a while (e.g. due to a bug or a
configuration issue). Such problems are super annoying, but are relatively easy
to debug. Here are a few steps you can take in such situations:

* Do <kbd>M-x</kbd> `toggle-debug-on-quit`
* Reproduce the problem
* Hit <kbd>C-g</kbd> around 10 seconds into the hang

This will bring up a backtrace with the entire function stack, including
function arguments. So you should be able to figure out what's going on (or at
least what's being required).

### I upgraded Projectile using `package.el` and nothing changed

Emacs doesn't load the new files, it only installs them on disk.  To see the
effect of changes you have to restart Emacs.
