# janet-emacs-trial-kit

The two main purposes of the trial kit are to:

1. Make it easy to experience some Janet support in Emacs (>= 29.1 +
   integrated tree-sitter support) without having to:

    * Change your existing Emacs configuration [1]
    * Figure out how to configure Emacs for some Janet support
    * Manually fetch certain dependencies

2. Provide [a
   path](https://github.com/sogaiu/janet-editor-and-tooling-info/blob/master/doc/emacs.md)
   to find out about Janet-relevant Emacs packages.

![Demo](janet-emacs-trial-kit-linux.png?raw=true "Demo")

## Requirements

* C compiler ([gcc](https://gcc.gnu.org/),
  [clang](https://clang.llvm.org/), etc. invocable via `cc`)
* [emacs](https://emacs.org) (>= 29.1 with
  [tree-sitter](https://tree-sitter.github.io/) support)
* [git](https://git-scm.com/)
* [janet](https://janet-lang.org)

Windows note: Ensure `gcc.exe` is on your `PATH` (e.g. via: `scoop
install gcc`) - this is instead of `cc` mentioned above.

## What You Get

The Emacs setup provides:

* Basic Janet code handling (e.g. syntax highlighting and indentation)
  via [janet-ts-mode](https://github.com/sogaiu/janet-ts-mode)
* REPL interaction via [ajrepl](https://github.com/sogaiu/ajrepl)
* Linting via
  [flycheck-janet](https://github.com/sogaiu/flycheck-janet)
* Rainbow delimiters
* Dark theme

All without having to:

* Manually fetch certain dependencies
* Change your existing Emacs configuration
* Figure out how to configure Emacs for some Janet support

## Initial Setup

Initial invocations:

```
git clone https://github.com/sogaiu/janet-emacs-trial-kit
cd janet-emacs-trial-kit
janet jetk
```

The above lines may take some time to complete as there will likely
be:

* multiple git cloning operations,
* compiling of C code, and
* Emacs-related compilation

The launched Emacs may have a buffer named `*elpaca-bootstrap*` which
typically goes away after a bit to be replaced by at least one buffer
named `*elpaca-log*`.  Other buffers likely can be ignored or
dismissed.

`*elpaca-log*` will likely show a list of packages being downloaded.
Please wait for this to finish.  One can tell once there are no
longer any packages listed.

If you got this far, that should be it regarding setup.

See [here](doc/misc.md) for additional info if there were any issues
with the above sequence.

## Verifying Things Are Working

### Syntax Highlighting

I typically start by opening a `.janet` file.  This repository
contains `sample.janet` for this purpose.  Once opened, one should
see:

```janet
(+ 1 2)

(defn my-fn
  [x]
  (+ x 2))

(my-fn 3)
```

The code should be syntax-highlighted and the menu bar should include
menus for `Janet-TS` [2] and `Ajrepl`.

### REPL Support

To verify the REPL support works, do one of:

* `M-x ajrepl`
* Choose `Start REPL` from the `Ajrepl` menu

A second buffer named `*ajrepl-repl*` should appear with content like:

```
Janet 1.31.0-51a75e18 linux/x64/gcc - '(doc)' for help
repl:1:>
```

You can type things in that buffer directly, but I typically "send"
expressions or code to it via commands while the focus is in the
source buffer.

To try this out, put point (your cursor) at the end of `(+ 1 2)` on
the first line of `sample.janet` and do one of:

* `M-x ajrepl-send-expression-at-point`
* `C-x C-e`
* Choose the `Send expression at point` menu item from the `Ajrepl`
  menu

The content of the `*ajrepl-repl*` buffer should then be like:

```
Janet 1.31.0-51a75e18 linux/x64/gcc - '(doc)' for help
repl:1:> (+ 1 2)
3
```

## Typical Use

Start Emacs by:

```
janet jetk
```

## Operating Systems with Confirmed Success

* Android via Termux (`clang` as `cc`, `janet` built from source)
* Void Linux
* Windows 10

## Footnotes

[1] One of the new features of Emacs 29.1 is being able to start it up
using different information for intitialization.  If invoked with
`--init-directory=<some-dir>`, `<some-dir>` is sort of like another
`.emacs.d`.  This is the mechanism used by the code in this repository
to provide the feature of "not interfering with your existing Emacs
setup".

[2] There is only one item in the `Janet-TS` menu by default, but
`M-:` and then `(require 'janet-ts-experiment)` will populate the menu
with additional experimental features.
