flycheck-mercury
================

This library provides a [flycheck](https://github.com/flycheck/flycheck) checker
for the logic / functional programming language Mercury using the
[Melbourne Mercury Compiler](http://mercurylang.org/). See
[here](https://github.com/flycheck/flycheck/pull/295) for the reasons why it is
not directy part of the `flycheck` codebase.

`mmc` provides an *errorcheck-only* mode where it only displays errors and
warning without doing any compilation.

Installation
------------

`flycheck-mercury` is available as Emacs 24 package on
[MELPA](http://melpa.milkbox.net/).

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
will be automatically checked using this checker if you have prolog-mode
with support for Mercury installed.

`flycheck-mercury` does not create any files, this includes interface files
(`*.int`, `*.int{0,2,3}`) required to correctly identify type information
etc. However, it will supply the option `-I` to `mmc` in order to search for
interface files in the directories contained in the list
`flycheck-mmc-interface-dirs`. These directories are relative to the position of
the file to check. If interfaces are reported as missing that actually exist, it
should be sufficient to add their location relative to the checked file to
`flycheck-mmc-interface-dirs`, i.e., using:

```lisp
(add-to-list 'flycheck-mmc-interface-dirs "<interface-directory>")
```

This list defaults to `("Mercury/ints" "Mercury/int0s" "Mercury/int2s"
Mercury/int3s")`, i.e., the directory structure created by `mmc --make`.

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
