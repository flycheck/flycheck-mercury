flycheck-mercury
================

This library provides a [flycheck](https://github.com/flycheck/flycheck) checker
for the logic / functional programming language Mercury using the [Melbourne
Mercury Compiler](http://mercurylang.org/). See
[here](https://github.com/flycheck/flycheck/pull/295) why it is not part of the
`flycheck` codebase.

`mmc` provides an *errorcheck-only* mode where it only displays errors and
warning without doing any compilation.

Installation
------------

`flycheck-mercury` is currently not available as Emacs 24 package.

To use it, first ensure that `flycheck` is installed, then download this code
and add the directory to your Emacs `load-path`.

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
