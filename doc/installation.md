## Prerequisites

You'll need to have Emacs installed (preferably the latest stable
release). If you're new to Emacs you might want to go through
[the guided tour of Emacs](https://www.gnu.org/software/emacs/tour/index.html)
and the built-in tutorial (just press <kbd>C-h t</kbd>).

!!! Note

    Projectile officially supports Emacs 25.1+.

## Installation

The recommended way to install Projectile is via `package.el`.

### Installation via package.el

Projectile is available on the two major `package.el` community
maintained repos -
[MELPA Stable](http://stable.melpa.org)
and [MELPA](http://melpa.org).

You can install Projectile with the following command:

<kbd>M-x package-install [RET] projectile [RET]</kbd>

or by adding this bit of Emacs Lisp code to your Emacs initialization file
(`.emacs` or `init.el`):

```el
(unless (package-installed-p 'projectile)
  (package-install 'projectile))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents [RET]</kbd>

Keep in mind that MELPA packages are built automatically from
the `master` branch, meaning bugs might creep in there from time to
time. Never-the-less, installing from MELPA is a reasonable way of
obtaining Projectile, as the `master` branch is normally quite stable
and serious regressions there are usually fixed pretty quickly.

!!! Tip

    If you don't want to (or can't) wait for MELPA to rebuild Projectile,
    you can easily build and install an up-to-date MELPA package locally yourself. Check out
    [this article](http://emacsredux.com/blog/2015/05/10/building-melpa-packages-locally/)
    for details on the subject.

Generally, users of the non-adventurous kind are advised to stick
with the stable releases, available from MELPA Stable.
You can pin Projectile to always use MELPA
Stable by adding this to your Emacs initialization:

```el
(add-to-list 'package-pinned-packages '(projectile . "melpa-stable") t)
```

Finally add this to your Emacs config:

```el
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)
```

!!! Note

    Those keymap prefixes are just a suggestion. Feel free to put
    there whatever works best for you.
    `C-c p` used to be the default prefix up to version 2.0, but
    starting with version 2.0 you have to select prefix key(s)
    yourself.

### Installation via use-package

`use-package` can be used to install Projectile via the `package.el`'s repositories
[MELPA Stable](http://stable.melpa.org) and [MELPA](http://melpa.org).

If you wanted to install the version of Projectile which is what is to be found in
the `master` branch, declare the following in your Emacs initialization file
(`.emacs` or `init.el`):

```el
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))
```

However, if you wanted to be a bit more conservative and only use the stable
releases of Projectile, you'd declare the following:

```el
(use-package projectile
  :ensure t
  :pin melpa-stable
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))
```

After placing one of the above s-expressions, evaluate it, for it to take effect
by entering: <kbd>C-x C-e</kbd>.

For further configuration options with `use-package`, consult the
official [use-package repository](https://github.com/jwiegley/use-package).

### Installation via el-get

Projectile is also available for installation from
the [el-get](https://github.com/dimitri/el-get) package manager.

Provided you've already installed `el-get` you can install Projectile with the
following command:

<kbd>M-x el-get-install [RET] projectile [RET]</kbd>

### Installation on Debian and Ubuntu

Users of Debian 9 or later or Ubuntu 16.10 or later may simply
`apt-get install elpa-projectile`.

Your favourite Linux distribution might be providing a Projectile package as well.

### Emacs Prelude

Projectile is bundled with
[Emacs Prelude](https://github.com/bbatsov/prelude). If you're a Prelude
user, Projectile is already properly configured and ready for
action.
