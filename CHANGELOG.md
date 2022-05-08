# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Adds and configure package `imenu-list`.
- Adds and configures package `org-transclusion`.
- Adds and configures package `goggles`.

### Changed

- Uses `modify-all-frames-parameters` for setting transparency and
  also updates `wal/transparency`.
- Suggest using `.Xmodmap` file for hyper/caps switch.
- No longer uses the `consult` variants of yanking.

### Fixed

- Remove `eshell-mode-map` mapping in `eshell` configuration (the map
  belongs to `esh-mode` but is no longer required).
- Advice for `persp-format-name` takes names shorter than the
  truncation into account.

## [1.7.0] - 2021-04-30

### Added

- Added `quelpa` so non-MELPA packages can be installed from GitHub.
- Added my mode-line package `wal-line` using `quelpa` recipe.
- Package `ligature` is now installed using `quelpa`.
- Upgraded `commitlint` to latest version.

### Removed

- Package `org-bullets` in favor of using `org-modern`.

### Fixed

- Icons set for `popper` and `org-sticky-header` now use
  `all-the-icons-*` instead of the `doom-modeline` equivalent to
  decouple functionality from `doom-modeline`.
