## Issues

Report issues and suggest features and improvements on the
[GitHub issue tracker](https://github.com/bbatsov/projectile/issues). Don't ask
questions on the issue tracker - use the [support channels](support.md) instead.

If you want to file a bug, please provide all the necessary info listed in
our issue reporting template (it's loaded automatically when you create a
new GitHub issue).

It's usually a good idea to try to reproduce (obscure) bugs in isolation. You
can do this by cloning Projectile's GitHub repo and running `make run-projectile` inside
it.  This will bring up Emacs with only the latest version of Projectile loaded. By
starting fresh, with the latest code, we can ensure that the problem at hand
isn't already fixed or caused by interactions with other packages.

## Patches

Patches in any form are always welcome! GitHub pull requests are even better! :-)

Before submitting a patch or a pull request make sure all tests are
passing and that your patch is in line with the [contribution
guidelines](https://github.com/bbatsov/projectile/blob/master/CONTRIBUTING.md).

## Documentation

Good documentation is just as important as good code.

Consider improving and extending the
this manual and the
[community wiki](https://github.com/bbatsov/projectile/wiki).

### Working on the Manual

The manual is generated from the markdown files in the
[doc](https://github.com/bbatsov/projectile/tree/master/doc) folder of Projectile's
GitHub repo and is published to [Read the Docs](readthedocs.org). The
[MkDocs](http://www.mkdocs.org/) tool is used to convert the markdown sources to
HTML.

To make changes to the manual you simply have to change the files under
`doc`. The manual will be regenerated automatically when changes to those files
are merged in `master` (or the latest stable branch).

You can install `MkDocs` locally and use the command `mkdocs serve` to see the
result of changes you make to the manual locally:

```sh
$ cd path/to/projectile/repo
$ mkdocs serve
```

If you want to make changes to the manual's page structure you'll have to edit
[mkdocs.yml](https://github.com/bbatsov/projectile/blob/master/mkdocs.yml).

## Donations

You can support the development of Projectile via
[Salt](https://salt.bountysource.com/teams/projectile),
[Gratipay](https://www.gratipay.com/projectile).

[![Support via Gratipay](https://cdn.rawgit.com/gratipay/gratipay-badge/2.1.3/dist/gratipay.png)](https://gratipay.com/projectile)

## Running the tests in batch mode

```sh
$ cd /path/to/projectile
$ make update
$ make compile
$ make test
```

Run all tests with:

```sh
$ make test
```

Tests should run fine in `shell-mode` or `term-mode`. It's also possible to use <kbd>M-x</kbd> `compile` (or `helm-make`).
