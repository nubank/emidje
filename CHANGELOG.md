# Change Log

All notable changes to this project will be documented in this file. This change
log follows the conventions of [keepachangelog.com](http://keepachangelog.com/)
and this project adheres to [Semantic
Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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
- Show proper descriptions for failing tabular facts by upgrading to the latest version of [midje-nrepl].

## [1.0.0] - 2018-11-13

### Added
- Initial version with a set of features for running Midje tests, viewing
  reports and formatting tabular facts within Emacs.

[Unreleased]: https://github.com/nubank/emidje/compare/1.0.1...HEAD
[1.0.1]: https://github.com/nubank/emidje/compare/1.0.0...1.0.1
[midje-nrepl]: https://github.com/nubank/midje-nrepl
