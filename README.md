# Emidje

Test runner, report viewer and formatting tool for Midje within Emacs.

`Emidje` extends [Cider][cider] to provide support for [Midje][midje] tests in a
similar fashion as
[cider-test.el](https://github.com/clojure-emacs/cider/blob/master/cider-test.el)
does for `clojure.test` tests. In fact, most of `Emidje` functionalities were
strongly inspired on `cider-test.el` features.

When installed, `Emidje` exposes the `emidje-mode`, an Emacs minor-mode that
complements `clojure-mode` and `cider-repl-mode` by providing a set of
keybindings to interact with `Midje` facts in a more pleasant way.

## Installation

`Emidje` is available on [Melpa][melpa] and [Melpa stable][melpa-stable], two
major `package.el` community maintained repositories. Soon, the most recommended
way of installing `Emidje` is through `package.el`, the native Emacs's package
manager.

You can install `Emidje` by typing the following command:

```el
M-x package-install [RET] emidje [RET]
```

After installing `emidje`, add the following line to your `init.el` file in
order to activate the `emidje-mode` whenever you visit a `Clojure` file or
switch to the `Cider's REPL` buffer:

```el
(emidje-setup)
```

Alternatively, you can write your own function to add `emidje-mode` to the
desired hooks:

```el
(defun my-clojure-hook ()
"Enable some minor modes to enhance Clojure development."
  (clj-refactor-mode)
  (emidje-mode))

(add-hook 'clojure-mode-hook #'my-clojure-hook)
```

## Usage

`Emidje` is a [nREPL][nrepl] client for [midje-nrepl][midje-nrepl]. Therefore,
the later must be available in the classpath of your project (see
[midje-nrepl][midje-nrepl]'s documentation for more details).

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
mainly on huge projects with heavy and slow integration tests, because it makes
the `REPL` startup faster and more reliable. But, if you are acquainted to run
`Midje` facts with `M-x cider-load-buffer` (`C-c C-k`), be aware that by default
this will no longer work with `Emidje`. This should not be a problem since one
of the core features of `Emidje` is a set of keybindings to run facts (more
about that below). However, if you want to check facts on commands that load a
namespace or a given `sexpr`, type `M-x emidje-toggle-load-facts-on-eval` to
disable this behavior on the current buffer or `C-u M-x
emidje-toggle-load-facts-on-eval` to disable it globally. Alternatively, you can
disable this feature entirely in your `init.el` as follows:

```el
(setq-default emidje-load-facts-on-eval nil)
```

### Running tests

Press `C-c C-j n` from a source buffer or `REPL` to run all tests related to the
namespace where you are currently in. As `cider-test.el` does, `Emidje` employs
a customizable inference logic to figure out the test namespace that corresponds
to the namespace where you are working on. This means that you can run all tests
defined in a given namespace without switching directly to it. Simply press the
aforementioned keys from the implementation namespace and `Emidje` will check
its facts.

Press `C-c C-j p` to run all tests defined in the project. The combination `C-c
C-j r` can be used to re-run tests that didn't pass in the last execution. Use
`C-c C-j t` to run the test at point.

### Interacting with reports

Test results are displayed in the `*midje-test-report*` buffer. You can switch
to this buffer by pressing `C-c C-j s` from a `clojure` namespace or from the
`REPL`. Once in this buffer, you can navigate across test results, jump to test
definitions, show stacktraces, etc. Follows a list of keybindings that you have
at your disposal:

| Keybinding | Description |
| --------------- | --------------------------------------------- |
| `e` | Show test error and stacktrace |
| `RET` or `M-.` | Jump to namespace or test definition at point |
| `n-r` | Move point to next test result |
| `p-r` | Move point to previous test result |
| `n-e` | Move point to next error |
| `p-e` | Move point to previous error |
| `n-f` | Move point to next failure |
| `p-f` | Move point to previous failure |

### Formatting tabular facts

Move point to the begining of a `tabular` fact and press `C-c C-j f` and it will
be formatted as a right aligned table.

### Customizing

Type `M-x customize-group [emidje]` to see a complete list of `Emidje` variables
that can be tweaked.

## Changelog

An extensive changelog is available [here](CHANGELOG.md).

## License
Copyright © 2018 Nubank

Distributed under the Apache License, Version 2.0

[cider]: https://github.com/clojure-emacs/cider
[melpa]: https://melpa.org/
[melpa-stable]: https://stable.melpa.org/
[midje]: https://github.com/marick/Midje
[midje-nrepl]: https://github.com/nubank/midje-nrepl
[nrepl]: https://github.com/nrepl/nrepl
