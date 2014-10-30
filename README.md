[![MELPA](http://melpa.org/packages/flycheck-mercury-badge.svg)](http://melpa.org/#/flycheck-mercury)

flycheck-mercury
================

This library provides a [Flycheck](https://github.com/flycheck/flycheck) checker
for the logic / functional programming language Mercury using the [Melbourne
Mercury Compiler](http://mercurylang.org/). See
[here](https://github.com/flycheck/flycheck/pull/295) for the reasons why it is
not directly part of the `flycheck` codebase.

`mmc` provides an *errorcheck-only* mode where it only displays errors and
warning without doing any compilation.

![Emacs Screenshot with flycheck-mercury](https://github.com/flycheck/flycheck-mercury/raw/master/flycheck-mercury-screenshot.png)

Installation
------------

`flycheck-mercury` is available as Emacs 24 package on
[MELPA](http://melpa.org/).

If you want to install the version from github, first ensure that `flycheck` is
installed, then download this code and add the directory to your Emacs
`load-path`.

```lisp
(add-to-list 'load-path
   "path-to-flycheck-mercury")
```

Add to your `.emacs`:

```lisp
(eval-after-load 'flycheck
  '(require 'flycheck-mercury))
```

Make sure that the `mmc` binary is present on Emacs' `exec-path` and can be
executed.

Usage
-----

When `flycheck` is enabled (e.g. with `global-flycheck-mode`), Mercury buffers
will be automatically checked using this checker if you have prolog-mode with
support for Mercury installed.

`mercury-mmc` does *not* create any files, this includes interface files
(`*.int`, `*.int{0,2,3}`) which are required to correctly identify type
information etc. However, it will supply the option `-I` to `mmc` in order to
search for interface files in the directories contained in the list
`flycheck-mmc-interface-dirs`. These directories are relative to the position of
the file to check. If interfaces are reported as missing that actually exist, it
should be sufficient to add their location relative to the checked file to the
buffer-local variable `flycheck-mmc-interface-dirs`,

```lisp
(setq-default 'flycheck-mmc-interface-dirs '("<interface-directories>"))
```

This list defaults to `("Mercury/ints" "Mercury/int0s" "Mercury/int2s"
Mercury/int3s")`, i.e., the directory structure created by `mmc --make`.

So, in order to make `mercury-mmc` recognize any newly added predicate, you must
first create its corresponding interface files, e.g., by launching a compilation
of your program.

Output
------

`mercury-mmc` uses all three levels of error reporting: `error` for things which
prevent correct compilation, `warning` for things like unused variables
etc. which does not prevent compilation and `info` for inferred type signatures.

User Options
------------

The user options are defined via *buffer-local* variables. To change their
default value for new buffers use:

```lisp
(setq-default <variable> <new-value>)
```

`flycheck-mmc-max-message-width` defines the maximal length of a message line.
If the specified value is strictly positive, the line is truncated to this
length and `...` is added at the end (using `s-truncate`). A value less than or
equal to `0` has no effect. The default value is `0`.

`flycheck-mmc-max-message-lines` defines the maximal number of message lines for
a single source line.  If the specified value is strictly positive, the messages
are limited to this number of lines and `...` is added as final line. A value
less than or equal to `0` has no effect. The default value is `0`.


License
-------

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see http://www.gnu.org/licenses/.
