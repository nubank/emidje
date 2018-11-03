[![cider]](https://github.com/clojure-emacs/cider)
[![clj-refactor]](https://github.com/clojure-emacs/clj-refactor.el)
[![melpa](http://melpa.org/packages/clj-refactor-badge.svg)](http://melpa.org/#/clj-refactor)
[![melpa-stable](http://stable.melpa.org/packages/clj-refactor-badge.svg)](http://stable.melpa.org/#/clj-refactor)
[! [midje]](https://github.com/marick/Midje)
[![midje-nrepl]](https://github.com/nubank/midje-nrepl)
[![nrepl]](https://github.com/nrepl/nrepl)

# Emidje

Test runner, report viewer and formatting tool for Midje within Emacs.

`Emidje` extends [Cider](cider) to provide support for [Midje](midje) tests in a
similar fashion as
[cider-test.el](https://github.com/clojure-emacs/cider/blob/master/cider-test.el)
does for Clojure tests. In fact, most of `Emidje` functionalities were strongly
inspired on `cider-test.el` features.

When installed, `Emidje` exposes the `emidje-mode`, an Emacs minor-mode that
complements `clojure-mode` and `cider-repl-mode` by providing a set of
keybindings to interact with `Midje` facts in a more pleasant way.

## Installation

`Emidje` is available on [Melpa](melpa) and [Melpa stable](melpa-stable), two
major `package.el` community maintained repositories. Soon, the most recommended
way of installing `Emidje` is through `package.el`, the native Emacs's package
manager.

You can install `Emidje` by typing the following command:

```el
M-x package-install [RET] emidje [RET]
```

## Usage

`Emidje` is a [nREPL](nrepl) client for [midje-nrepl](midje-nrepl). Therefore,
the later must be available in the classpath of your project (see
[midje-nrepl](midje-nrepl)'s documentation for more details).

If you start a `REPL` via `M-x cider-jack-in`, you don't need anything else;
`Emidje` will take care of injecting the correct version of `midje-nrepl` in
your `REPL` through `Cider` facilities. However, if you are connecting to a
running `nREPL` process, you need to add `midje-nrepl` manually either to your
project's `project.clj`, or in the `:user` profile found at
`~/.lein/profiles.clj`:

```clojure
:plugins [[nubank/midje-nrepl "x.x.x"]]
```

Replace the `x.x.x` above with the current installed `Emidje`'s version. Notice
that those versions should be kept in sync to make sure that the `nREPL` client
and the `nREPL` middleware are compatible (you will see a warning in the
`Cider`'s `REPL` when those versions don't match).

### Starting the REPL and loading facts

By default `Emidje` prevents `Midje` facts from being run when a given test
namespace is loaded. This means that `Midje` facts will no longer be checked
during the `REPL` startup or as a side effect of commands like `cider-eval-x`,
`cider-load-buffer`, `cider-refresh`, etc. This behavior is extremely useful,
mainly on huge projects with heavy and slow integration tests, because it make
the `REPL` startup faster and more reliable. But, if you are acquainted to run
`Midje` facts with `M-x cider-load-buffer` (`C-c C-k`), be aware that by default
this will no longer work with `Emidje`. This should not be a problem since one
of the core features of `Emidje` is a set of keybindings to run facts. However,
if you want to check facts on commands that load a namespace or a given `sexpr`,
type `M-x emidje-toggle-load-facts-on-eval` to disable this behavior on the
current buffer or `C-u M-x emidje-toggle-load-facts-on-eval` to disable it
globally. Alternatively, you can disable this feature entirely in your `init.el`
as follows:

```el
(setq-default emidje-load-facts-on-eval nil)
```

### Running tests

### Formatting tabular facts

### Customizing

Type `M-x customize-group [emidje]` to see a complete list of `Emidje` variables
that can be tweaked.
## License
Copyright © 2018 Nubank

Distributed under the Apache License, Version 2.0
