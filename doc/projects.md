## Supported Project Types

One of the main goals of Projectile is to operate on a wide range of project types
without the need for any configuration. To achieve this it contains a lot of
project detection logic and project type specific logic.

### Version Control Systems

Projectile considers most version-controlled repos to be
a project. Out of the box Projectile supports:

* Git
* Mercurial
* Bazaar
* Subversion
* CVS
* Fossil
* Darcs

### File markers

Projectile considers many files to denote the root of a project. Usually those files
are the configuration files of various build tools. Out of the box the following are supported:

File              |  Project Type
------------------|----------------------
rebar.config      | Rebar project file
project.clj       | Leiningen project file
build.boot        | Boot-clj project file
deps.edn          | Clojure CLI project file
SConstruct        | Scons project file
pom.xml           | Maven project file
build.sbt         | SBT project file
gradlew           | Gradle wrapper script
build.gradle      | Gradle project file
.ensime           | Ensime configuration file
Gemfile           | Bundler file
requirements.txt  | Pip file
setup.py          | Setuptools file
tox.ini           | Tox file
composer.json     | Composer project file
Cargo.toml        | Cargo project file
mix.exs           | Elixir mix project file
stack.yaml        | Haskell's stack tool based project
info.rkt          | Racket package description file
DESCRIPTION       | R package description file
TAGS              | etags/ctags are usually in the root of project
GTAGS             | GNU Global tags
configure.in      | autoconf old style
configure.ac      | autoconf new style
cscope.out        | cscope
Makefile          | Make

There's also Projectile's own `.projectile` which serves both as a project marker
and a configuration file. We'll talk more about later in this section.

## Adding Custom Project Types

If a project you are working on is recognized incorrectly or you want
to add your own type of projects you can add following to your Emacs
initialization code

```el
(projectile-register-project-type 'npm '("package.json")
				  :compile "npm install"
				  :test "npm test"
				  :run "npm start"
				  :test-suffix ".spec")
```

What this does is:

  1. add your own type of project, in this case `npm` package.
  2. add a file in a root of the project that helps to identify the type, in this case it is `package.json`.
  3. add *compile-command*, in this case it is `npm install`.
  4. add *test-command*, in this case it is `npm test`.
  5. add *run-command*, in this case it is `npm start`.
  6. add test files suffix for toggling between implementation/test files, in this case it is `.spec`, so the implementation/test file pair could be `service.js`/`service.spec.js` for example.

The available options are:

Option            | Documentation
------------------|--------------------------------------------------------------------------------------------
:compilation-dir  | A path, relative to the project root, from where to run the tests and compilation commands.
:compile          | A command to compile the project.
:configure        | A command to configure the project. `%s` will be substituted with the project root.
:run              | A command to run the project.
:src-dir          | A path, relative to the project root, where the source code lives.
:test             | A command to test the project.
:test-dir         | A path, relative to the project root, where the test code lives.
:test-prefix      | A prefix to generate test files names.
:test-suffix      | A suffix to generate test files names.
:related-files-fn | A function to specify test/impl/other files in a more flexible way.

#### Returning Projectile Commands from a function

You can also pass a symbolic reference to a function into your project type definition if you wish to define the compile command dynamically:

```el
(defun my/compile-command ()
  "Returns a String representing the compile command to run for the given context"
  (cond
   ((and (eq major-mode 'java-mode)
         (not (string-match-p (regexp-quote "\\.*/test/\\.*") (buffer-file-name (current-buffer)))))
    "./gradlew build")
   ((eq major-mode 'web-mode)
    "./gradlew compile-templates")
   ))

(defun my/test-command ()
  "Returns a String representing the test command to run for the given context"
  (cond
   ((eq major-mode 'js-mode) "grunt test") ;; Test the JS of the project
   ((eq major-mode 'java-mode) "./gradlew test") ;; Test the Java code of the project
   ((eq major-mode 'my-mode) "special-command.sh") ;; Even Special conditions/test-sets can be covered
   ))

(projectile-register-project-type 'has-command-at-point '("file.txt")
                                  :compile 'my/compile-command
                                  :test 'my/test-command)
```

If you would now navigate to a file that has the `*.java` extension under the `./tests/` directory and hit `C-c c p` you
will see `./gradlew build` as the suggestion. If you were to navigate to a HTML file the compile command will have switched
to `./gradlew compile-templates`.

This works for:

  * `:configure`
  * `:compile`
  * `:compilation-dir`
  * `:run`

Note that your function has to return a string to work properly.

### Related file location

For simple projects, `:test-prefix` and `:test-suffix` option with string will
be enough to specify test prefix/suffix applicable regardless of file extensions
on any directory path. `projectile-other-file-alist` variable can be also set to
find other files based on the extension.

For the full control of finding related files, `:related-files-fn` option with a
custom function or a list of custom functions can be used. The custom function
accepts the relative file name from the project root and it should return the
related file information as plist with the following optional key/value pairs:

| Key    | Value                                                         | Command applicable                                                              |
|--------|---------------------------------------------------------------|---------------------------------------------------------------------------------|
| :impl  | matching implementation file if the given file is a test file | projectile-toggle-between-implementation-and-test, projectile-find-related-file |
| :test  | matching test file if the given file has test files.          | projectile-toggle-between-implementation-and-test, projectile-find-related-file |
| :other | any other files if the given file has them.                   | projectile-find-other-file, projectile-find-related-file                        |
| :foo   | any key other than above                                      | projectile-find-related-file                                                    |


For each value, following type can be used:

| Type                       | Meaning                                                                                                  |
|----------------------------|----------------------------------------------------------------------------------------------------------|
| string / a list of strings | Relative paths from the project root. The paths which actually exist on the file system will be matched. |
| a function                 | A predicate which accepts a relative path as the input and return t if it matches.                       |
| nil                        | No match exists.                                                                                         |

Notes:
 1. For a big project consisting of many source files, returning strings instead
    of a function can be fast as it does not iterate over each source file.
 2. There is a difference in behaviour between no key and `nil` value for the
    key. Only when the key does not exist, other project options such as
    `:test_prefix` or `projectile-other-file-alist` mechanism is tried.


#### Example - Same source file name for test and impl

```el
(defun my/related-files (path)
  (if (string-match (rx (group (or "src" "test")) (group "/" (1+ anything) ".cpp")) path)
      (let ((dir (match-string 1 path))
            (file-name (match-string 2 path)))
        (if (equal dir "test")
            (list :impl (concat "src" file-name))
          (list :test (concat "test" file-name)
                :other (concat "src" file-name ".def"))))))

(projectile-register-project-type
   ;; ...
   :related-files-fn #'my/related-files)
```

With the above example, src/test directory can contain the same name file for test and its implementation file.
For example, "src/foo/abc.cpp" will match to "test/foo/abc.cpp" as test file and "src/foo/abc.cpp.def" as other file.


#### Example - Different test prefix per extension
A custom function for the project using multiple programming languages with different test prefixes.
```el
(defun my/related-files(file)
  (let ((ext-to-test-prefix '(("cpp" . "Test")
                              ("py" . "test_"))))
    (if-let ((ext (file-name-extension file))
             (test-prefix (assoc-default ext ext-to-test-prefix))
             (file-name (file-name-nondirectory file)))
        (if (string-prefix-p test-prefix file-name)
            (let ((suffix (concat "/" (substring file-name (length test-prefix)))))
              (list :impl (lambda (other-file)
                            (string-suffix-p suffix other-file))))
          (let ((suffix (concat "/" test-prefix file-name)))
            (list :test (lambda (other-file)
                          (string-suffix-p suffix other-file))))))))
```

`projectile-find-related-file` command is also available to find and choose
related files of any kinds. For example, the custom function can specify the
related documents with ':doc' key. Note that `projectile-find-related-file` only
relies on `:related-files-fn` for now.

### Related file custom function helper 

`:related-files-fn` can accept a list of custom functions to combine the result
of each custom function. This allows users to write several custom functions
and apply them differently to projects.

Projectile includes a couple of helpers to generate commonly used custom functions.

| Helper name and params             | Purpose                                                     |
|------------------------------------|-------------------------------------------------------------|
| groups KIND GROUPS                 | Relates files in each group as the specified kind.          |
| extensions KIND EXTENSIONS         | Relates files with extensions as the specified kind.        |
| tests-with-prefix EXTENSION PREFIX | Relates files with prefix and extension as :test and :impl. |
| tests-with-suffix EXTENSION SUFFIX | Relates files with suffix and extension as :test and :impl. |

Each helper means `projectile-related-files-fn-helper-name` function.

#### Example usage of projectile-related-files-fn-helpers
```el
(setq my/related-files
      (list
       (projectile-related-files-fn-extensions :other '("cpp" "h" "hpp"))
       (projectile-related-files-fn-test-with-prefix "cpp" "Test")
       (projectile-related-files-fn-test-with-suffix "el" "_test")
       (projectile-related-files-fn-groups
        :doc
        '(("doc/common.txt"
           "src/foo.h"
           "src/bar.h")))))

(projectile-register-project-type
   ;; ...
   :related-files-fn #'my/related-files)
```

## Customizing project root files

You can set the values of `projectile-project-root-files`,
`projectile-project-root-files-top-down-recurring`,
`projectile-project-root-files-bottom-up` and
`projectile-project-root-files-functions` to customize how project roots are
identified.

To customize project root files settings:

```
M-x customize-group RET projectile RET
```

## Ignoring files

!!! Warning

    The contents of `.projectile` are ignored when using the
    `alien` project indexing method.

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

### File-local project root definitions

If you want to override the projectile project root for a specific
file, you can set the file-local variable `projectile-project-root`. This
can be useful if you have files within one project that are related to
a different project (for instance, Org files in one git repo that
correspond to other projects).

## Storing project settings

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
<kbd>s-p E</kbd> (<kbd>M-x projectile-edit-dir-locals RET</kbd>).

Here are a few examples of how to use this feature with Projectile.

## Configuring Projectile's Behavior

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

## Configure a Project's Compilation, Test and Run commands

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
