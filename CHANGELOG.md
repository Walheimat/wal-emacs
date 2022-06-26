# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- New variable `wal/active-theme` set during initialization. Function
  `consult-theme` updates it. The variable is read when calling new
  function `wal/load-active-theme` (also called during
  initialization).
- Functions `query-replace` are now bound in `wal/colonel` to "q" and
  "Q".
- Temporary fix to make `kubernetes--save-window-state` work.
- Temporary fix to require `org-indent` because some package uses its
  face without requiring the package.
- Binds toggling built-in package `hideshow`.
- New function `wal/avy-mark-region` to do just that, bound to
  `wal/colonel` + "r SPC".
- Sets `persp-modestring-short` if even the truncated perspective
  names go beyond new threshold `wal/perspective-shorten-after`
  (default is 30 characters).
- Adds `explain-pause-mode` using `quelpa` to have a history of why
  Emacs was slow.

### Changed

- Custom variable `wal/perspective-max-modeline-length` was renamed to
  `wal/perspective-truncate-after`.
- Custom command line flag `--doctor` not sets new variable
  `wal/doctor` which both controls `use-package` collecting statistics
  and turns on `explain-pause-mode`.
- Updated `commitlint` dependencies.
- Dispatch/command map for `bookmark` now uses "C-c m".
- Lints `commitlint` config with `prettier`.

### Removed

- Package `doom-modeline`.
- Package `golden-ratio`.
- Package `company` and `slime-company`.
- Package `highlight-indent-guide` (temporarily).
- Hack to fix duplicate logs in `lsp-dart`.
- Hack to fix `kubernetes` function to refresh overview (about to be
  fixed upstream).
- Hack to fix autloads for `kubernetes-overview` (seemingly fixed
  upstream).

## [1.7.2] - 2021-06-12

Slug or snail.

### Added

- Warning emitted for non-GNU systems can now be ignored by setting
  `wal/ack-warnings` to `t`.
- Adds and configures packages `python-pytest`.
- Adds `org-speed` command "N" to log non-continuously (as in from
  now).
- Adds `ein` as a Python expansion pack extra.
- Adds `org-alert`.
- Configures `org-refile` to consider headlines up to level 3.

### Changed

- Added listing errors to `lsp-mode` transient.
- Improved inferior shell mappings for `python-mode`.
- Finding a file with projectile while not in a project is now advised
  to switch projects.
- Capture template for `org-roam` now uses slug with no date prefix.
- Commands from `projectile` are advised to only prompt if
  `universal-argument` is given.
- The `modus-themes` are now always loaded because of a customization
  quirk.
- Upgraded to latest `commitlint` version.

### Fixed

- Package `marginalia` is now required with errors ignored (on first
  start-up, the package won't be there).
- Expansion pack packages are now only loaded if installed.
- Advise function `kubernetes-utils--save-window-state` to use a
  function, not a macro so that it works (in Emacs 29).
- Disables `org-sticky-header-mode` in presentations.
- Borderless setting for `modus-themes` was fixed.
- Fixes `slime` requiring potentially missing `slime-company`.

## [1.7.1] - 2021-05-14

Less is less.

### Added

- Adds and configure package `imenu-list`.
- Adds and configures package `org-transclusion`.
- Adds and configures package `goggles`.

### Changed

- Uses `modify-all-frames-parameters` for setting transparency and
  also updates `wal/transparency`.
- Suggest using `.Xmodmap` file for hyper/caps switch.
- No longer uses the `consult` variants of yanking.
- The default `dap-mode` hydra is now bound in the smaller hydra.
- Offset for `fish-mode` is now 2.

### Fixed

- Remove `eshell-mode-map` mapping in `eshell` configuration (the map
  belongs to `esh-mode` but is no longer required).
- Advice for `persp-format-name` takes names shorter than the
  truncation into account.
- Always use copying for backup files to prevent problems with
  `lsp-mode` managed buffers.
- Custom `use-package` keyword `:wal-ways` should now work as intended
  (and not only if a different keyword was included in the state).
  Packages should now be automatically ensured again unless the
  evaluation of keyword, `wal/minimal` and `wal/minimal-exclude`
  resolves to `nil`.

## [1.7.0] - 2021-04-30

Locally sourced.

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
