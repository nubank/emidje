# Change Log

All notable changes to this project will be documented in this file. This change
log follows the conventions of [keepachangelog.com](http://keepachangelog.com/)
and this project adheres to [Semantic
Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- [#15](https://github.com/nubank/emidje/pull/15): add the command
  `emidje-run-all-tests-popup`, bound to `c-c c-j P`. This keybinding shows a
  Magit popup that allow for users to set some switches/options to customize the
  test execution.
- [#15](https://github.com/nubank/emidje/pull/15): add ns filters to exclude
  and/or include namespaces to be tested. Those options can be set through the
  aforementioned popup.
- [#16](https://github.com/nubank/emidje/pull/16): add support for collecting
  profiling information from tests. The switch `-p` activate such option and
  those information are displayed under the section *profile* of the report
  buffer.
  - [#17](https://github.com/nubank/emidje/pull/17): add two new options to the
    popup menu: `test-exclusions` and `test-inclusions`. Those send the
    respective parameters to the nREPL middleware, by allowing for users to
    filter tests by metadata keywords.

### Changed
- `emidje-run-all-tests` (`c-c c-j p`) can't be called with a prefix argument
  anymore to select test paths. Instead, users should use `c-c c-j P` to set
  this option (and others) via a Magit popup menu.
- [#16](https://github.com/nubank/emidje/pull/16): as a consequence of the new
  added features, the report buffer has been changed to be more friendly. Now,
  related information are grouped in outline sections that may be expanded or
  collapsed.

### Fixed
- [#18](https://github.com/nubank/emidje/pull/18): avoid concatenating the minor
  mode's title with other minor modes by adding a white space in the beginning
  of the lighter.
  - [#20](https://github.com/nubank/emidje/pull/20): fix logic to determine
    whether the nREPL middleware should be injected in the REPL at Cider
    jack-in.

## [1.1.0] - 2018-12-19

### Added
- Allow for users to select the test path where tests will be run, by calling
  `emidje-run-all-tests` with a prefix argument.
- Allow for users to select an arbitrary namespace to run tests by calling
  `emidje-run-ns-tests` with a prefix argument.

## [1.0.1] - 2018-12-05

### Added
- Support for running builds on [CircleCI](https://circleci.com/).
- A set of unit tests through
  [Buttercup](https://github.com/jorgenschaefer/emacs-buttercup).

### Changed
- Do not call `emidje-enable-nrepl-middleware` inside `eval-after-load`. From
  now on users will be responsible for calling `emidje-setup` after Cider.

### Fixed
- Do not set markers after rendering the test report buffer.
- Get Emidje's version properly when the package has been installed from Melpa.
- Show proper descriptions for failing tabular facts by upgrading to the latest
  version of [midje-nrepl].

## [1.0.0] - 2018-11-13

### Added
- Initial version with a set of features for running Midje tests, viewing
  reports and formatting tabular facts within Emacs.

[Unreleased]: https://github.com/nubank/emidje/compare/1.1.0...HEAD
[1.1.0]: https://github.com/nubank/emidje/compare/1.0.1...1.1.0
[1.0.1]: https://github.com/nubank/emidje/compare/1.0.0...1.0.1
[midje-nrepl]: https://github.com/nubank/midje-nrepl
