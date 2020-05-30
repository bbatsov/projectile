## Why did you name it Projectile?

I wanted a name that's not as boring as `project.el` and implies that your
interaction with projects is going to speed up significantly. :-)

## Does Projectile work with TRAMP?

Yeah, it does. I don't use TRAMP myself, however, so I never paid that
much attention to the TRAMP support. It's mostly community-maintained.

## Why does Projectile violate the rule not to bind keys in the user keymap?

I opted for the `C-c p` prefix fully aware that this violates [a very
established Emacs
convention](https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html),
mostly because it felt practical and because pressing `C-c C-p` is not
super convenient for many people. I've come to regret this decision,
though, and it will likely be reverted down the road.

**Update** This was changed in Projectile 2.0.

## Do you need some help cleanup up all those tickets that have piled up?

Certainly! In our [issue
tracker](https://github.com/bbatsov/projectile/issues/) we've got
plenty of tickets marked with `Help Wanted` or `Good First Issue` that
you can take a stab at, if you'd like to help out!
