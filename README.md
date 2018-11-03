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

## License
Copyright © 2018 Nubank

Distributed under the Apache License, Version 2.0
