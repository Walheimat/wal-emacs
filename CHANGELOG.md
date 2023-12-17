# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a
Changelog](https://keepachangelog.com/en/1.0.0/), and this project
(very loosely) adheres to [Semantic
Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- `wal-org-agenda-take-note` to do just that, bound to `C-c n`.
- Package `gumshoe` as alternative to `dogears` (might remove both
  again soon).
- `dinghy-rope` is now installed during `ensure`
- `winner` now ignores buffers that use a buffer because of `vertico`.

### Changed

- `vertico-sort-function` is no longer customized (meaning it uses
  `vertico-sort-history-alpha` again).
- `repeat-exit-timeout` was increased to 5 seconds.
- Bindings for `multiple-cursor` repeat map have been made more
  ergonomic.
- `org-clock` bindings now use `C-c {t,T}`.
- All `consult` commands are now bound in `consult` `transient`.
- The concept of pre-narrowing `consult` commands was removed again.
- `consult-buffer` and `project-find-file` now use `vertico-multiform`
  style `buffer`.
- `H-j` now does what `H-u` used to do: go to word, quick exit with
  selection.
- `wal-project-dired-root` was removed (since `project-dired` does the
  same thing).

### Fixed

- `org-habit` no longer force-loads `org-agenda`.

## [v2.2.3]

Whale form.

### Added

- `wal-update` now calls script that uses `package-vc-upgrade-all` to
  upgrade all `wal-bridge` and eventually tangles.
- `C-c v` is bound to `wal-org-capture-project-tasks`.
- While editing this config, the swimming whale `posframe` is
  displayed again for project buffers. Can be disabled by setting
  `wal-config-show-animation` to `nil`. All variables and functions
  have been renamed from `wal-ascii-whale-*` to
  `wal-config-animation-`.
- Package `surround`.
- Package `dogears` now has custom command `wal-dogears-list` which
  pops to or toggles the list. It was moved to `find` package.

### Changed

- `rg-isearch-current-file` is now bound to `M-s n` instead of `H-n`
  when `isearch` is active.
- `dogears` has idle timer enabled again and hooks removed.
- Commands executed through the prelude's `wal--compile` function
  don't display the buffer anymore. They can be displayed using new
  command `wal-show-compilation-result` bound in `whaler` to `r`.

### Fixed

- `org-agenda` is no longer implicitly loaded by `partial-recall`.
- `markdown-mode` and `org-mode` no longer enable `flycheck-mode`.
- `magit-process-buffer` has double the size.

## [v2.2.2]

Extrinsic vice.

### Added

- `other-frame` is now bound in `other-window-repeat-map`.
- Package `dogears` as a trial.
- My package `ship-mate` which is functionality spun out from this
  config.

### Changed

- Fringe mark for `bookmark` was disabled.
- `org-capture-templates` was reworked. Plain `p` now is for plain
  entries in the file (unnarrowed) while `t` is adding new tasks.
- [The following are included in `ship-mate`]
  - Commands added to (formerly) `wal-project-command` history no
    longer replace the one matched against. They're just inserted.
    [This is a feature of `ship-mate`].
  - `recompile` is now advised to set the `compile-history` to that of
    the last (formerly) `wal-project-*` command if the
    `compile-command` matches.

### Fixed

- [The following are included in `ship-mate`]
  - Updating the history of (formerly) `wal-project-*` commands now
    uses fuzzy matching instead of just checking the last
    `compile-command` (as this can lead to erroneous replacements).
  - (formerly) `wal-project-command` no longer infinitely extends a
    command's history.

## [v2.2.1]

Itchy and patchy.

### Added

- `wal-update` to invoke all necessary commands and Makefile targets
  to have the latest changes installed.
- `wal-project-command` now tries to update its history when
  `compilation-start` is run.

### Changed

- `compile` is bound again to `C-c i`.

### Fixed

- `wal-scratch-persist` no longer persists the
  `initial-scratch-message`.
- Additional scratch buffers are prepped correctly (initial message
  and mode).

## [v2.2.0]

Prelude to peace.

### Added

- Local variable `wal-org-capture-tasks-file` can now also be set to a
  path relative to `org-directory`.
- Package `org-habit-stats` as an extra of new `junk` expansion `org`.
- `org-stuck-projects` is now configured to use tag group "energy".

### Changed

- `org-agenda` buffers are ignored by `partial-recall`.
- Prelude package was renamed from `wal-prelude` to just `wal` after
  linting (requires re-running `make local` to update the
  bootstrapper).
- All library files are now tangled using `wal--tangle-target`.
- All internal path variable were renamed.
- Persistent tag `@growth` was replaced with `@wellbeing`.
- Functions `wal-package-{installed,built-in}-p` were moved to
  `wal-package`.
- `wal-consult-project` is now called with `:require-match t`.

### Fixed

- `all-the-icons-dired-mode` is now advised to not be triggered for
  non-GUI frames.
- `wal-ignore-if-not-installed` now uses `wal-package-installed-p` to
  make sure missed built-ins aren't ignored if they set `:hook`.
- Custom prefix map `wal-project-prefix-map` is now bound directly to
  avoid losing the prefix argument on first invocation.

## [v2.1.14]

Minimal comfort.

### Added

- Command `wal-org-clock-kill-current-task` to do just that.
- The minimal setup (with flag `--mini` or customizing `wal-minimal`)
  is now actually minimal, loading (for now) only built-in packages
  and marked packages.
- Initial scratch message now contains a comment about the setup.
- Custom variable `wal-hyper-mock` that defaults to `C-c w`. It is
  bound in `function-key-map` to apply hyper modifier. This makes
  `wal-use-hyper-prefix` obsolete.
- Package `corfu-terminal`.

### Changed

- Custom `use-package` keyword `:wal-ways` now defaults to `nil`.

### Removed

- Custom variable `wal-minimal-exclude`.
- Custom variable `wal-use-hyper-prefix` (see above).

### Fixed

- Macros for `wal-capture-flag` were moved to `wal-package` since
  otherwise packages in `wal-bridge` are not affected.

## [v2.1.13]

Fishing for complements.

### Changed

- `dashboard-refresh-buffer` is now bound in `ambassador` to `0`.
- Binding for `compile` (`C-c t`) was removed.
- Org tags are now grouped and no longer define select characters.
  This is because of the fix below; files can now freely define their
  own tags and both the predefined and the file-specific tags can be
  chosen.
- The package tags are now grouped under `package`.
- Neither `flycheck-mode` nor `flymake-mode` hook into `prog-mode`
  anymore. This is because `harpoon` now allows to set a checker
  function that can be overridden (which is done for `emacs-lisp-mode`
  and `gdscript-mode`).

### Fixed

- `org-tag-persistent-alist` is now set instead of `org-tag-alist`.

## [v2.1.12]

Visible introspection.

### Added

- Better package documentation.
- Local variable `wal-org-capture-tasks-file`. When set, locating the
  heading will try that file first. This allows tasks file living
  outside of a project again.
- Command `wal-config-org-tags-view` to search for tags in the config.
  When called with `C-u` matches all package tags.
- Adds tags to all package configurations to indicate where they are
  sourced from.
- Command map for `diff-hl` is now bound to `d` in `ambassador`
  (`docker` was moved to `o`).

### Changed

- Function `wal-insert` now uses keyword arguments for
  `allow-duplicates`, `before` and `quiet`.
- Binding for `j` in `dired-jump-map` is removed because it interferes
  with `dired-goto-file`.
- Bindings in `wal-project-prefix-map`.
- Completion of files with `cape` now uses `C-c /`.
- `emacs-lisp-mode` now uses `flymake`.

### Fixed

- Binding to `M-o` in `sgml-mode`'s `html-mode` is rebound to `C-M-o`
  in order to not interfere with `wal-switch-to-other-buffer`.

## [v2.1.11]

Longer safe.

### Changed

- Setting `project-vc-name` is marked as safe for strings.
- Setting the various custom local variables is now considered safe.
- `consult` is no longer advised to put the current buffer first.
- `gdscript-mode` was updated to use `eglot`, `flycheck`, and
  `wal-maybe-enable-tabs`.
- Package configurations are no longer divided into **Utility** and
  **Configuration** sections.

## Removed

- Commands `wal-tab-bar-switch-to-buffer-tab` and
  `wal-tab-bar-rename-from-project`.

## [v2.1.10]

Prompt inaction.

## Added

- `profiler` commands are now bound in `administrator`.
- The bootstrapper is no longer required to live at the end of the
  init file (although it will always be appended to the end on
  creation).
- `cargo-process-run-example` is now bound in Rust `major`.
- Tangling the config now notifies immediately.

### Changed

- `proselint` checker is disabled (has to be enabled manually).
- `wal-project-command` switches to `comint` with numeric prefix
  argument 0.
- `wal-project-command` no longer prompts unless called with a prefix
  argument as long as the history is non-empty.
- `consult-buffer` and `tab-switch` use `flat` completion.
- Autosave variables have been adjusted downwards (more autosaves).
- `multiple-cursors-mode` is now "prominent" in `minions`.

### Removed

- `wal-project-create-command` no longer accepts key `:comint`. All
  commands use `compile` by default again; see above.
- Functions `wal-delete-edit-or-kill`, `wal-pad-string` and
  `wal-reset-to-standard`.

### Fixed

- `wal-read-sensible-font-height` now uses `face-attribute` to
  retrieve the current value; no command sets custom variables
  anymore.
- Open and closed projects are now differentiated during completion
  (no duplicates).

## [v2.1.9]

Valued locals.

### Added

- Command `wal-switch-to-other-buffer` bound to `M-o`.
- Commands created by `wal-project-create-command` now also create
  variable `wal-project-{cmd}-reverse-mode` that allows overriding the
  default usage (`comint` or `compile`) for the given command.

### Changed

- Pre-narrowing in `consult` is off by default.
- `dired-auto-revert-buffer` is now set to
  `dired-directory-changed-p`.
- `savehist-additional-variables` is now set to `(kill-ring)`.
- `xref-search-program` is set to `ripgrep` if possible.
- `vertico-multiform-commands` now set `{switch-to,consult}-buffer`
  and `tab-switch` to use `unobtrusive`. Previous configurations are
  done via `vertico-multiform-categories`. Switching to the vertical
  view and back is bound to user-prefixed `i`.
- `initial-major-mode` is no longer set to `fundamental-mode`.
- `vertico-resize` is set to `t`.
- `transient` for `consult` now calls either `consult-org-heading` or
  `consult-outline` depending on the mode.
- User-prefixed `i` now uses `avy-goto-char-timer` with a timer of 0.4
  seconds instead of `avy-goto-char`.
- Project task `org-capture` template no longer adds the file path.

### Fixed

- `partial-recall` buffer source is no longer inserted before the
  default buffer source.
- `corfu-popuinfo` is no longer ensured.
- `make test` running `cask install` since the test folder might be
  more recent than the build folder (see below).
- `wal-project-local-value` now uses `project--value-in-dir` instead
  of visiting the root buffer and using `buffer-local-value`.
- The custom `dashboard` banners are now chosen by new override advice
  to `dashboard-choose-banner`. So having both GUI and terminal frames
  should yield the appropriate logo.
- `org-capture-templates` now set empty lines before and after instead
  of using newline.

### Removed

- Command `wal-consult-line` as `consult-line` already adds
  `thing-at-point` to history.
- Command `wal-consult-org-heading` as narrowing covers most cases.
- Package definitions in `Cask` file as installing the package via
  `cask` is not (yet) possible.

## [v2.1.8]

Dapper capper.

### Added

- Package `cape`; `cape-history` is bound for `eshell`.
- Configuration for `corfu-popupinfo` to replace `corfu-doc`.
- The `company` backend `dap-mode` is now adapted to work with
  `corfu`. This is done by overriding `dap-ui-repl-company-prefix` and
  mapping the backend with `cape`'s adapter function.
- `woman` is now bound in `administator.

### Changed

- `dired-hide-details-mode` is now turned on by default.

#### Removed

- `corfu-doc`.

## [v2.1.7]

Near prudence.

### Added

- `wal-project-command`s can now have multiple defaults that will be
  added to the commands history on creation. For example, settings
  `wal-project-build-default-cmd` to `'("make tangle" "make local")`
  will add both commands to the history leaving `"make tangle"` as the
  most recent entry.
- `repeat-exit-key` is now bound to `<return>`.
- `wal-org-hide-emphasis-markers` that will enable them (or disable
  them if called with a prefix argument) and fontify the buffer.
- `wal-insert` that works like `wal-insert-before` but has optional 5th
  argument to insert before instead (pushing previous 5th argument
  `quiet` to position 6).
- `use-package` keywords `:sinker` to add hook functions at the end
  and `:fhook` to add a hook to a `-functions` variable.

### Changed

- `org-hide-emphasis-marker` is no longer set as `t` (see above).
- `multiple-cursors` now uses a repeat map binding more commands.
- The only `wal-project-` commands created now are `build`, `install`,
  `clean`, `run`, new `execute` (the non-interactive equivalent of
  `run`) and `test`.

### Removed

- `wal-{make,run}[-*]` commands as the `wal-project-{*}` cover all scenarios
  now.
- `wal-insert-before` was removed (see above).

### Fixed

- `consult-flycheck` binding in `consult` `transient` no longer
  overrides `consult-recent-file`.

## [v2.1.6]

Turtle hour.

### Added

- `wal-project-update`.
- `wal-eshell` as a project-aware wrapper around `eshell`. It will
  create a new `eshell` buffer for a project or pop to an existing
  one.
- `vterm` and its utility functions were re-added and slightly
  amended.
- `wal-tab-bar-rename-from-project` to do just that.

### Changed

- The `corfu-auto-{delay,prefix}` were changed (or removed) for
  several languages.
- `eshell` buffers are now displayed at the bottom in a dedicated side
  window.
- `wal-project-{install,publish}` now use `comint-mode`.
- `wal-project-command`s now accept a prefix argument to toggle the
  mode they usually use and will give accurate information in their
  docstrings.

### Fixed

- `wal-run-test` now uses correct `bydi` function and loads
  `bydi-report`.
- `comint-mode` buffers are now part of the `consult` source for
  compilations.
- `wal-eshell` now requires `eshell`.

## [v2.1.5]

Here but mostly there.

### Added

- Key `:comint` that can be passed to `wal-project-create-command` to
  make sure a command is run in `comint-mode` instead.
- Command `wal-project-run` that uses the new option.
- Compilation buffers can now be narrowed using `c`. This `consult`
  source uses new function
  `wal-switch-to-buffer-obeying-display-actions`.

### Changed

- Subsections in `wal-emacs` were removed.
- `search-whitespace-regexp` was set to allow for matching any char
  with space.
- `consult-imenu` and `consult-outline` are now bound in `consult`
  transient.
- `dired-kill-when-opening-new-dired-buffer` is no longer enabled.
- When locating project tasks, the user is prompted to select a
  project if not in a project currently.

### Removed

- `wal-major-delight` and the silly names for certain major modes that
  were ignored in `treesit` variants.
- Custom bindings for `jinx` were removed.

### Fixed

- The advice added to `recompile` for `wal-project` commands no longer
  breaks its functionality outside of projects (notably affecting
  `rg`).
- Functions `wdb-nearby` and `wdb-farway` no longer add nil
  parameters. This might also have led to issues where windows could
  no longer be targeted by `other-window.`

## [v2.1.4]

Binky-brained.

### Added

- Command `wal-project-find-in-here` to find a project file in the
  `default-directory`.
- New project command `coverage` defaulting to `make coverage`.

### Changed

- Function `wal-insert-after` now allows passing optional fourth
  argument to quietly ignore a no-op instead of raising a user error.
- `org-mode` no longer runs `auto-fill-mode`. Its `major` now binds
  `visual-line-mode`. Package`visual-fill-column` is now explicitly
  declared as a package and runs when `visual-line-mode` is run.
- Command `wal-supernova` now quits windows until no target remains.
- Macro `wal-project-create-command` now uses `cl-defmacro` and has
  optional keys `key` (to not default it) and `default` to set an
  initial command.
- `org-capture` and custom `wal-org-capture-switch-to-project-task`
  are no longer part of the `transient` for `org-roam`. They're not
  bound using a `parallel` to `C-c c`.
- `smerge` now uses `C-c g` and `recompile` uses `C-c r`.

### Removed

- Helper package `wal-pacify` was moved to `dinghy`.

## [v2.1.3]

Prism break.

### Added

- Default value variables created by macro
  `wal-project-create-command` are now marked as `safe-local-variable`
  so that they don't need to be added manually every time.

### Changed

- `wal-use-package-ensure-elpa` no longer checks if a package is a
  `junk` package since this is now a feature of `junk` itself when
  `junk-use-package-setup` is called.
- Switched to using `dinghy`.
- `wal-settings` now considers `find-sibling-rules` a safe variable.

### Removed

- `consult-buffer-filter` is no longer customized.

### Fixed

- `wal-project-*` commands are only added to history if not present.

## [v2.1.2]

Far-away troubles.

### Added

- `org-agenda-todo-yesterday` is now bound in `org-agenda-mode-map`.

### Changed

- All test suites now new the newly added `bydi` patterns.
- Package `wal-line` was renamed to `whale-line`.
- `commitlint` rules now include fixed types.
- `wal-core-vc-packages` now use specs.

### Removed

- Function `wal-matches-in-string` as well as
  `wal-check-coverage--{calculate-coverage,add}` which are now part of
  `bydi` package.
- `junk--pack-p` is only used if bound.
- Package `whale-line` is loaded after `all-the-icons`.

### Fixed

- Docstring `tempel` templates should work (a bit) better.

## [v2.1.1]

Bridge twofer.

### Added

- New library file `wal-bridge` contains all of my own packages.

### Changed

- `partial-recall` was spun out as its own package.
- `harpoon` was spun out as its own package.
- `junk` was spun out as its own package.
- Testing macros were spun out as their own package as `bydi`.
- Pre-narrowing is now always enabled for `consult`.
- `wdb-{faraway,nearby}` now default to
  `display-buffer-use-some-window`.

## [v2.1.0]

Pizza time.

### Added

- The internal structure of `wal-partial-recall` (see below) now uses
  `cl-defstruct` to create memories and moments; memories are rings of
  moments and an original size; moments are buffers and timestamps
  that refer to when they were added; the ring will now grow if adding
  a new buffer would kick an existing buffer but that buffer is not
  older than `wal-partial-recall-threshold` (new custom variable,
  defaults to 60) seconds; buffers that belong to a different memory
  are reclaimed if visited when `wal-partial-recall-reclaim-threshold`
  (5 minutes by default) is exceeded.
- Custom variable `wal-consult-buffer-narrow-source` with options
  `recall` (the new default) and `project` that determines which source
  to pre-narrow to.
- Custom variable `wal-consult-pre-narrowed` commands that is set to
  `consult-buffer` to give more control of which commands should be
  narrowed if toggled on.
- The format used in `wal-run-test` and `wal-run-test-file` can now be
  toggled between `text` and `json` using
  `wal-run-test-toggle-format`.
- Tool package `wal-pacify` that checks all tangled files for
  byte-compile and doc issues using `flymake` now has a test suite.
- Convenience command `wal-config-load-test-helper` to do just that.
  Bound to `e` in `whaler`.

### Changed

- `wal-tab-buffers-*` functionality was renamed to
  `wal-partial-recall-*`. It is still associated with tabs. See above
  for new functionality. Tabs now get a custom key that is created on
  tab creation to keep track of their buffer history. This means tabs
  no longer need an explicit name to have one and that renaming them
  doesn't wipe out their association with their history.
- Recall limit can now be customized through `wal-total-recall-limit`.
- `whaler` was restructured.
- The ASCII whale animation is now done using indirect buffers meaning
  it can be enabled and disabled for individual buffers.
- `wal-run-test-file` now reads from test directory.
- The config buffers are no longer ignored by `consult`.

### Fixed

- Package `wal-settings` no longer attempts to create a site-lisp
  directory to make sure it only actually sets things (it will still
  recursively load it).
- Package `wal-settings` no longer defines any functions (moved to
  `wal-useful`).
- Package `wal-settings` no longer creates key bindings; this was
  moved to `wal-key-bindings`.
- All packages now explicitly require the packages they depend on.
  Only relevant if `wal-packages` is altered before bootstrapping.
- `wal-agenda-buffer-p` now uses `org-agenda-file-p`.
- Some unnecessary code was replaced in tests to make them run faster.
- Table entries for `wal-partial-recall` are deleted on
  `delete-frame`.

## [v2.0.3]

Name face.

### Added

- Tab buffers. Tabs that have been explicitly named now have a history
  of buffers found in their context. This is used in a `consult`
  buffer source that can be narrowed to using `t`.

### Changed

- Packages `font` and `look` were folded into `visuals`.
- `wdb-nearby` and `wdb-faraway` were refactored to accept (mostly)
  the same key arguments. `wdb-faraway` now has key parameter `bottom`
  to use `display-buffer-at-bottom`. `wdb-nearby` now allows setting
  either `direction` or `side`. Window widths and heights are no
  longer defaulted.
- `python-mode-major` no longer binds individual `send` commands.
  `inferior-python-mode` now enables `corfu`.
- Package `wal-func` was renamed to `wal-useful`.
- Package `wal-external` was renamed to `wal-package`.
- Running `make` alone should install everything now, including
  packages.
- Default setup for `org-src` is used.
- Customization is no longer part of `wal-settings`. It's now part of
  `wal-prelude` which makes the settings package optional.
- `wal-prelude-bootstrap` now only takes a single optional argument
  that sets its mode (`plain`, `cold`, `ensure` or default `normal`).

### Removed

- Commands `wal-set-indent-defaults` and `wal-reset-indent-defaults`.
  Indentation variables are customized in their respective packages
  instead.

### Fixed

- `wal-prelude` now renders the contents of the `init.el` template
  using the source directory. This should allow for the clone not
  having to live in the `user-emacs-directory` and have any chosen
  name.
- The messages buffer no longer has conflicting `display-buffer`
  entries.
- `lsp-ui-doc-show-with-mouse` is now disabled to avoid messing with
  help echos.
- Running Emacs with `--ensure` should no longer attempt to install
  built-in packages.
- Running `wal-prelude-tangle-config` now touches files in
  `wal-prelude--phony-build-dependencies` to avoid needless `cask
  install` runs.
- `switch-to-buffer-obey-display-actions` is no longer set to `t`
  since it leads to buffer position being lost on `switch-to-buffer`.

## [v2.0.2]

Check make.

### Added

- Command `recompile` is now bound to `C-c c`.
- Utility function `wal-project-local-value` that uses
  `buffer-local-value` on the project's root folder.
- Script `tools/update-version.sh` to update references to old tags.
- Command `wal-supernova` to quit windows displaying otherwise hidden
  buffers. Bound to `C-c o`.

### Changed

- Tool `wal-pacify` now uses `flymake` instead of `flycheck` to prep
  it for running in CI.
- Commands `wal-check-coverage` and `wal-create-json-coverage` were
  replaced by `wal-run-test` that runs `make test` and will
  create JSON coverage when called with `C-u`.
- Command `wal-run-test-file` to select a single file to run.

### Fixed

- Order in `wal-prelude` to allow `cold-boot` to work again.

## [v2.0.1]

Pleasure island.

### Added

- Command `wal-org-capture-switch-to-project-tasks` is now bound in
  `project-switch-commands` to `t`.
- Local variable `wal-project-parent-project` used in
  `wal-org-capture--find-project-tasks-heading`.
- Command `wal-project-switch-to-parent-project` using local
  `wal-project-parent-project`.
- `wal-prelude` now has a test suite and full coverage.
- `js-mode` and `java-mode` now enable `subword-mode`.

### Changed

- Function `wal-org-capture--find-project-tasks-heading` now gets the
  buffer-local heading as well buffer-local
  `wal-project-parent-project`.
- Package `diff-hl` was moved to `wal-vc`.
- Data files `init.eld` and `tempel.eld` now live in `data/`
  directory.
- Command `wal-config-root` was replaced with
  `wal-config-switch-project` to make use of
  `project-switch-commands`.
- User-prefixed `k` quick-completes during `vertico` and `corfu`
  completion.

### Fixed

- Package `diff-hl` is now loaded after `magit` again to avoid the
  hooks being possibly overridden.

### Removed

- Templates in `templates/` directory (data files were moved, see
  above).

## [v2.0.0]

Crank action.

### Added

- Command `wal-config-consult-org-heading` to find a heading in any of
  the library files (see below).

### Breaking Changes

- Installation now uses a `Makefile`; running `make install` should
  prepare everything (other than installing external packages).
- All code is now under `lib/` in separate Org files. The library
  files are prefixed with `wal-` so their buffers can be ignored in
  `consult`.
- The base package is now called `wal-config`, not just `wal`.

### Changed

- The `pacify` utilities were moved to `tools/wal-pacify.el`.

### Removed

- All files under `setup/`; all scripts other than the daemon setup
  have entries in the `Makefile`.
- Documents under `documents/`; the style-guide was integrated in
  `lib/wal-config.el`, the bindings and hyper guide in
  `lib/wal-key-bindings.el`, the debug templates in `lib/wal-lsp.org`.
- `wal/find-config` (which no longer makes sense, see above).

## [v1.12.1]

Tab start.

### Added

- Custom `consult-org-heading` that when called with
  `universal-argument` limits the scope of the completion to the
  current tree and doesn't use unobtrusive `vertico` completion.
- `tab-bar-mode` is now enabled but the tabs themselves are invisible.
  Switching tabs has user-prefixed `o` as a convenience binding. New
  tabs start with a `dashboard` buffer.
- `dired-do-query-replace-regexp` is now bound in `dired-mode-map` to
  `% r` to replace duplicate `dired-do-rename-regexp` binding.
- `treesit` and `lsp` are now enabled for `c-mode`.
- Function `wal/tab-bar-switch-to-buffer-tab` to switch to an owning
  tab. Bound in `embark-buffer-map` to `t`.
- Command `wal/org-capture-switch-to-project-tasks` bound in `roamer`
  using `p`.
- `c-mode` will now run `lsp` and `treesit` if possible.

### Changed

- `eww` and `eshell` are now bound to `C-c b` and `C-c e` respectively
  instead of in `administrator`.
- User-prefixed `i` is now bound to `avy-goto-char`.
- The `major` `transient` for `org-mode` was improved.
- `wal/org-agenda-file` is now a custom variable.
- `switch-to-buffer-obey-display-actions` is now set to `t`;
  `switch-to-buffer-in-dedicated-window` to `prompt`.

### Fixed

- Packages installed using `package-vc-install` are now added to
  `package-selected-packages` using
  `package--update-selected-packages` to make sure they are persisted.
- Hooks are no longer ignored for user-selected packages (mainly
  affects `vertico-directory`).
- `diff-hl-magit-pre-refresh` is called on `magit-pre-refresh-hook`.

## [v1.12.0]

Fountain boots.

### Added

- Custom `use-package-ensure-function` that also checks that packages
  aren't built in nor part of an expansion pack.
- Function `wal/browse-html-file` is now bound to `x` in
  `embark-file-map`.
- Variable `wal/core-vc-packages` of packages to be installed using
  `package-vc-install` if present; only package there is `wal-line`.

### Changed

- Package `winner` was moved to `wal-windows`.
- Non (M)ELPA packages are now installed using `package-vc-install`.
  This only affects package `wal-line` which will no longer be loaded
  in Emacs < 29. `junk` recipes also use that method with new function
  `junk-package-vc-install`.
- Templates are now completed using user-prefixed `\`.
- All coverage files are now created in `coverage/`.
- `custom-file` is now created using `make-empty-file`.
- `helpful-mode` uses `pop-to-buffer` again.
- User-prefixed `o` is bound to `consult-register-store`.
- Command `kubernetes-overview` is now bound in `ambassador`.

### Fixed

- Only wrapping `wal/project-command` is now advised with
  `wal/with-project-bounded-compilation` to avoid being asked to save
  unrelated buffers when calling a generated command while switching.
- `prettier` errors are advised to be shown as warnings so that they
  can be muted once and for all.
- Recipes are now correctly determined as packages in `junk--pack-p`.
- Package `slime` is now deferred.

### Removed

- Configurations for `tabulated-list`, `hideshow`, `ediff` and `dictionary`.
- All `:ensure t` settings in `use-package` forms that are no longer
  needed.
- Package `popper`.
- Package `diminish`.
- Packages `quelpa` and `quelpa-use-package`
- Package `iedit`.

## [v1.11.5]

Plunk into clunk.

### Added

- `rg-isearch-current-file` is now bound in `isearch-mode-map` using
  user-prefixed `n`.
- Capturing (project) tasks now uses local variable
  `wal/org-capture-tasks-heading` to find the file and heading using
  new function `wal/org-capture-find-project-task-location`.
- Command `wal/force-delete-other-windows`; bound in
  `other-window-repeat-map` using `C-k`.
- Package `jinx`.
- Command `wal/spill-paragraph` to do the opposite of
  `fill-paragraph`.

### Changed

- All leaders no longer have the `wal/` prefix.
- As the directories were removed, `org-agenda-files` is now single
  file `.agenda` in the `org-directory`.

### Fixed

- Custom `flycheck` checkers are now registered.

### Removed

- **Personal** section in favor of using a `default.el` file in the
  `site-lisp` directory.
- All custom directories other than the `wal/site-lisp-directory`.

## [v1.11.4]

Group effort.

### Added

- New function `wal/plist-keys` to extract keys from a plist.
- New command `wal/project-select-command` that replaces
  `wal/project-compile` in `project-switch-commands` to select any
  registered command.
- New command `wal/rg-project-literal` to do a literal search in
  project. This command also replaces the `project-switch-commands`
  search.
- `C-c n` is now a `parallel` command of `wal/org-clock-take-note` and
  `org-clock-goto`.

### Changed

- Pre-narrowing `consult` can now be toggled; pre-narrowing is off by
  default.
- `dap-mode` is now bound in `ambassador` (was bound in `lsp-mode`
  `transient` before).
- Custom command `wal/org-clock-take-note` was moved to `C-c n`
  (previously bound in `editor`).
  `wal/kill-ring-save-whole-buffer` was moved to `editor` (previously
  bound in `triple-minus`).
- Other bindings to delete, goto and display from `windmove` are now
  bound.
- User-prefixed `i` uses `parallel` again to either load or store
  register.
- `org-mode` `transient` was simplified slightly.
- `transient`s `whaler`, `consult` and `org-mode-captain` now have
  top/bottom grouping.
- Most `wdb/nearby` buffers allow getting targeted by `other-window`
  again.
- Renamed `proselint` configuration and disable exclamation check.
- `winner` and `windmove` now hook into `emacs-startup-hook`.

### Removed

- Package `vterm` has been mothballed.
- `transient` for `lsp-mode` in favor of binding the command-map at
  top level.

### Fixed

- Duplicate binding of "f" in `consult` `transient`.
- Regex for `junit` errors should now also work in `compile` when run
  by `mvn`.
- `wal/project-command` now resolves the default command value from
  the targeted project.
- The `Caskfile` now requires `compat` to not run into any
  compatibility issues in the pipeline.
- `org-tree-slide` resets text-scale adjustments after stopping.

## [v1.11.3]

Demilitarized keyboard.

### Changed

- Leader key `general` was renamed to `administrator`; leader key
  `captain` was renamed to `major`; `major` was renamed to
  `ambassador`; `colonel` was renamed to `whaler`; `trooper` was
  renamed to `roamer`; `lieutenant` was renamed to `editor`.
- `popper` is now bound to user-prefixed `\`; cycling is done adding
  meta key.
- `text-mode` now activates `flycheck` but is set up to only check on
  save.
-  Macro `wal/define-init-setup` now implies `wal/on-boot`.

### Added

- Configuration for built-in `windmove`.
- `treesit` and `lsp-mode` is enabled for `yaml`.
- Regex matching for JUnit errors.
- Advice to delay the popup of `transient`s; used by leaders `whaler`
  and `consult`.
- `flyspell` now has a repeat map and the default map is overridden by
  `wal/flyspell-map`.

### Fixed

- `electric-indent-mode` is disabled for `multiple-cursors-mode`.
- Undos `treemacs` attempt to force ignore its buffers (which is
  mainly annoying for `dap-mode` buffers).
- `wal/other-window` has a repeat map like the original
  `other-window`.
- `js-ts-mode` has the same templates as `js-mode`.

### Removed

- Package `ace-window` has been mothballed.
- Custom `wal/other-window`.

## [v1.11.2]

Echo gecko.

### Added

- `vertico-multiform-mode` is now enabled; several `consult` commands
  that concern visiting places in buffer now use the `buffer` display.
- Command `wal/rg-rerun-toggle-hidden` to do just that.
- `consult` is now a `transient`.

### Changed

- `wal/async-process` now re-uses the same buffer in
  `compilation-mode` so `recompile` can be used.
- `consult-buffer` queries that sort by visibility are advised to
  still put the current buffer first.
- `transient-show-popup` is now `t` again and
  `transient-mode-line-format` shows the buffer identification.
- `wal/kill-some-popups` now only asks for confirmation if called with
  prefix argument.
- `popper-echo-mode` is no longer on.
- `wal/consult-line` was inverted (again).
- CI jobs are now grouped by branch. The `pre-push` hook was moved to
  a CI job that is only triggered by message "pacify".
- Various bindings.
- Some variables were renamed to reflect intent over content.
- Commands to test the config now share function `wal/run-script`
  instead of their own implementation.

### Removed

- Package `goggles`, `code-review`, `forge`, `pug-mode`, `po-mode`,
  `graphql`, `jakt-mode`, `jenkinsfile-mode`, `tokei` and `typo-mode`
  were mothballed.
- Command `wal/push-mark`.
- Configuration for `zone`.

### Fixed

- All commands created with `wal/project-create-command` are now
  advised using `wal/with-project-bounded-compilation`.

## [v1.11.1]

Peak conformance.

### Added

- Macro `wal/project-create-command` that defines everything necessary
  based on a single symbol.
- Commands `wal/set-{fixed,variable}-font-height` now read the prefix
  argument to apply their changes only to the current frame.
- Command `wal/perform-cold-boot` to run the `cold-boot.sh` script
  interactively.
- Setting `which-key` replacements is now done using new macro
  `that-key`.
- `pre-push` hook that will run `flycheck` against the package files.
- Command `wal/mwim-beginnnig` to jump to `beginning-of-line-text.`
- Utility `wal/lsp-ignore-directory` that allows just passing
  directory names (used by `harpoon`).
- Custom `flycheck` checker for `less-css-mode`.

### Changed

- Value for `transient-show-popup` is `t` locally in
  `magit-status-mode`, meaning there's no delay.
- Command `wal/project-switch-project` was replaced by
  `wal/consult-project` that pre-narrows to open projects.
- `wdb/faraway` now checks for mode windows as well.
- `helpful` now uses `display-buffer` over `pop-to-buffer`.
- Keymap for `flycheck` was moved to `wal/major`.
- Binding for `completion-at-point` is now local when using `corfu` in
  `harpoon`.
- `wal/consult-clock-in` now clocks discontinuously if called with
  `C-u`.
- `consult{-global}-mark` no longer uses `parallel`.
- Max refile depth was increased by one.
- `smerge` now uses prefix `C-c r`.
- `flycheck`'s prefix map is now bound in `wal/major`.

### Removed

- Command `wal/split-window-the-other-way`.

### Fixed

- `wdb/faraway` now sets `inhibit-switch-frame` to do just that.
- Functionality concerning external packages was moved from
  `wal-settings` to `wal-external`.
- Variable name `json-encoding-default-indentation` when resetting
  indent defaults.
- `wal/kwim` now passes prefix argument to `kill{-whole}-line`.
- `org-refile` no longer passes prefix argument if it is numeric
  argument 5 (so that things can be refiled and not just visited).
- The JSON parsers for `prettier` should now also work for other JSON
  modes.

## [v1.11.0]

Silicon mountain.

### Added

- Following recipe from its wiki, `consult-buffer` is now pre-narrowed
  to project buffers by default. This is controlled by new custom
  variable `wal/consult-buffer-narrow-to-project`.
- Command `wal/project-switch-project` that filters by open projects.
  User-prefixed `h` now uses a `parallel` of this and the original.
- Utility function `wal/display-buffer-same-place-or-faraway` to
  display a buffer in another frame if possible, reusing its window,
  and only as a last resort in a pop-p window. This is used for custom
  `project` command buffers.
- Keymap `wal/zero-in` to bind finding commands.
- Keys `[` and `]` were added to `wal/key-reach`; leaders
  `wal/consult` and `wal/trooper` were added to map `wal/consult-map`
  and `wal/org-roam-captain`.
- Macro `harpoon` now has a mapping for modes whose `treesit`
  equivalent does not follow the `<name>-ts-mode` pattern.
- `js-json-mode` (and `json-ts-mode`) now have a `harpoon`.

### Changed

- Macro `wal/treesit` is now part of `harpoon`.
- Macro `harpoon` was greatly refactored again to account for
  `treesit`.
- The `transient` `harpoon` dispatches were renamed from
  `wal/<mode>-dispatch` to `mode-captain`.
- Macro `parallel` now differentiates by numeric and "normal" prefix
  argument. This means calling the B command requires `C-u 0` instead
  of `C-u`. This allows for both commands to consume the argument.
  Keyword `:universalize` allows converting numeric to non-numeric if
  the called command expects `'(4)` over `4` etc.
- `project` ignores `.ccls-cache`.
- Most `which-key` replacements that are not prefixes were removed.
- Transients for `winner` and `smerge` were replaced by using (and
  adding) `repeat-mode` maps.
- `display-buffer` utility functions were reduced to two cases:
  `wal/display-buffer-same-place-or-{faraway,nearby}`.
- The order of `wal/key-reach` has changed, pushing `colonel` and
  `general` to `[` and `]`.
- Non-`captain` dispatches no longer use custom transient.

### Removed

- Package `jsonian` in favor of using built-in `js-json-mode`.
- Package `emojify` in favor of relying on built-in emoji support.
- Several `parallel` uses. The transients were moved to `wal/major`.

## Fixed

- Template for `docstring` now indents the other lines.

## [v1.10.3]

Piece of junk.

### Added

- Buffer source for `consult` to narrow to (contributing) agenda
  buffers.
- Support for `treesit` in `harpoon` and using new macro
  `wal/treesit`. Currently only used for `js-mode` as a test.
- Ligatures can now be set through `harpoon` using keyword `:ligatures`.
- `project-switch-action` to open project root; command to compile
  project.
- Additional functions to be called are now part of `harpoon` using
  keywords `:functions` so that they're only called if they're bound.

### Changed

- User-prefixed `i` both stores and loads using `consult` again.
- `wal/misc-map` was renamed to `wal/triple-minus-mode`.
- `wal/java-mode-dispatch` now uses `transient-switches` instead of
  completion to switch mode and scope.
- `log4j-mode` is no longer enabled for all `*.log` files.
- Macro `wal/hook` was renamed to `harpoon` and refactored into
  meaningful sub-macros.
- Macro `wal/define-expansion-pack` was renamed to `junk-expand` and
  all underlying functionality to `junk-*`.
- Macro `wal/univ` was renamed to `parallel`. The function it creates
  now no longer just prefixes the first function's name with
  `wal/univ-*` but instead constructs `<a>||<b>` out of both names.
  `parallel` now determines callee on whether the prefix argument is
  numeric or not, which allows both commands to have a meaningful
  value.
- Function `wal/l` now toggles dedication.
- Tasks in `org/tasks` are filtered for `dashboard`.

### Removed

- Custom variable `wal/idle-delay` in favor of using distinct values
  for distinct cases.

### Fixed

- `popper` bindings are now bound in global map.
- `consult-customize` sets `:preview-key` using string (seems a recent
  change).
- Same fix for `embark`.
- `wal/dap-stopped` now calls `wal/dap` instead of missing
  `dap/hydra/body`, setting `transient-show-popup` to `t` so it's
  immediately shown.
- `wal/project-command` makes sure `project-vc-name` doesn't mess with
  any output.
- `wal/maybe-org-roam-refile` was renamed to `wal/org-refile` and now
  listens for numeric prefix argument to skip `org-roam`-specific
  behavior.

## [v1.10.2]

In cold boot.

### Added

- Scripts to simulate a cold boot (where nothing has been installed)
  in `setup/setup-cold-boot.sh` and `setup/wal-setup-cold-boot.el`.
- Function `wal/persist-scratch` now persists the content of all
  scratch buffers.
- Function `wal/bootstrap-config` now accepts second optional argument
  `cold-boot` to mimic a cold boot by setting `package-user-dir` to a
  temporary directory.
- The handler for `use-package` keyword `:hook` is now advised to only
  handle if the respective is either installed or built-in.

### Changed

- Idle delay was doubled.
- Org `transient` now binds `org-info-find-node`.
- The `org` transient now allow sorting from headings; prefixes were
  changed to free `s` (subtrees now use `t` and timestamps `.`).

### Removed

- Package `python-pytest` since it uses `projectile` under the hood
  which causes problems.
- `wal/dwim-key` in favor of unique bindings.
- Package `hydra` in favor of using `transient`. All hydras with the
  exception of `wal/fly` (which I don't use) have been replaced.

### Fixed

- Cold booting the configuration should now work.
- `wal/project-magit-status` now will not run if the (sub-)project is
  not version-controlled.
- LSP should no longer complain about missing `yasnippet`.
- `wal/load-theme` now captures errors if loading a theme fails.
- `ligature-set-ligature` is no longer called directly to avoid issues
  on a cold boot.
- `wal-prelude` now emits messages about what's going on and the error
  handling was moved to `wal/load-config` to stop after a single
  error.
- `wal/site-lisp` not existing should no longer cause issues.

## [v1.10.1]

Wooden dinosaur.

### Added

- Regular expression for `js` to follow errors in jest output.
- `wal/hook` now accepts key `shallow` to not meddle with indentation.
- Compilations buffers now filter ANSI colors.
- Package `iedit`.
- `project-switch-commands` `wal/project-consult-buffer` and
  `wal/project-magit-status`.
- Project commands now have a per-project history.

### Changed

- `slime` no longer requires `sbcl` executable to exist.
- Most custom additional bindings were removed from built-in maps. QoL
  bindings were kept.
- Package sections that have custom utility are now subdivided by
  Utility and Configuration sections.
- Setting `corfu-auto-{delay,prefix}` is now no longer tied to
  `wal/lsp`.
- User-prefixed `y` now loads from register.

### Removed

- Package `jest` because of its `projectile` dependency leading to
  issues.
- Calling `org-agenda-list` no longer stores a window configuration.
- Several `find` commands (and their bindings in config transient).

### Fixed

- `pixel-scroll-precision-mode` now only activated for 29+.

## [v1.10.0]

Plan vanilla.

### Added

- Commands `wal/project-test` and `wal/project-install` that work like
  `project-compile` but store the previously used command in a hash
  table using the project as the key.
- More robust mocking tools.
- Full test coverage.
- Function `wal/avy-goto-word` now goes to word in line first. Called
  with `C-u` goes to word in buffer, called with multiple `C-u` goes
  to word anywhere.
- `wal/scratch-buffer` now allows session creation/selection like
  `eshell`.
- Package `log4j-mode` as part of new `java` expansion pack.
- Function `wal/project-rg` as a replacement for
  `project-find-regexp`.

### Changed

- `use-package-always-ensure` is no longer set to `t` by default.
  Instead, new flag `wal/flag-ensure` can be set with `--ensure`.
- `corfu` is now turned on in `eshell`.
- The map for `wal/general` was flattened. Both `eshell` and `eww` can
  be accessed directly.
- Exit command `wal/delete-edit-or-kill` is now bound to `C-x C-c`
  (the default binding).
- `org-src` now splits window vertically when editing source code.

### Removed

- Package `projectile` in favor of using built-in `project`.
- Custom flag `wal/flag-deny`.
- Packages `esh-autosuggest` and `eshell-prompt-extras`.
- Binding of command `restart-emacs`.
- `hydra` to scroll other window.
- `paredit` in favor of `puni`.

### Fixed

- Coverage calculation now calculates covered / relevant lines.
- Coverage can be calculated for buffers with count greater than 9.

## [v1.9.4]

Sweet toot.

### Added

- Around 120 additional tests for (almost) all packages. There are no
  tests for the settings package as all it does is set things.
- `project-compile` is now advised to only save buffers that are part
  of the current project.
- Command `wal/dired-config-tests` to open the tests directory with
  Dired.

### Changed

- Tests, test helper and `cask` were moved out of the configuration
  into project root.
- `consult-flycheck` is now bound in `wal/consult-map`.
- User-prefixed `u` now binds new function `wal/avy-goto-word` that
  replaces `wal/avy-goto-word-in-line`.
- Packages are now loaded from `wal/bootstrap-config` which is part of
  `wal-prelude` instead of doing it in `wal` package.
- Various sections from `README.org` now have their own document under
  `docs`.
- `wal/consult-line` now uses `thing-at-point` unless called with
  `C-u`.

### Removed

- `lispy` in favor of `paredit`.
- Custom minor mode `wal/config-mode` that was used for editing the
  config (the functionality is now provided by `wal/config-dispatch`).

### Fixed

- Using `project` over `projectile` should now work with `wal-line`.

## [v1.9.3]

Time killer.

### Added

- Command `wal/kill-some-popups` to do just that.
- Template function `wal/tempel-comment` using `c` used in new
  templates.
- List function `wal/list-from` to create a list using an element as
  its first element.
- New command `wal/kwim` to kill forward or whole line depending on
  the position of point in the line using `mwim`; it is now bound to
  `C-k`.
- New command `wal/org-clock-take-note` to take a note for the
  currently clocked task. Bound to `t` in `wal/misc-map`.
- Binds `H-|` to `popper-cycle`.

### Changed

- `wal/avy-goto-line` now uses `beginning-of-line-text` when going to
  beginning of line.
- Command `popper-toggle-latest` is now bound to user-prefixed `\`.
- User-prefixed `]` no longer binds `popper-cycle` (see below).
- Prefix for `tempel` is now `,,` to avoid clashes in languages that
  use `>`.
- All consult bindings were moved to renamed keymap `wal/consult-map`
  that uses `wal/consult-key` for its binding (`[` by default). This
  key is also used in the `rg` and `lsp` transients.
- Dispatch `org-roam` was moved to user-prefixed `]`.
- User-prefixed `i` now loads and stores register using `wal/univ`.
- The `tempel-path` is extended by templates provided in
  `templates/tempel.eld`.

### Removed

- Package `mode-line-bell`.
- `C-;` is no longer bound in favor of using `C-x C-;`.
- Binding `M-o` is no longer bound to `wal/other-window`; this command
  now overrides `other-window` by binding `C-x o`.
- The window splitter movement functions `wal/edge-*` as well as the
  `hydra` that bound them.
- Package `dimmer`.
- Package `drag-stuff`.
- Package `highlight-quoted` (lisp extra).
- Package `highlight-number`.

### Fixed

- `multiple-cursors` now disables (and re-enables) `corfu-mode` when
  entering/exiting.

## [v1.9.2]

Good trim.

### Added

- Function `wal/push-mark` that pushes a mark without activating it.
  Bound to `<SPC>` in `lieutenant`.
- Command `org-agenda-list` is now advised to find the tasks directory
  and store the window configuration using custom variable
  `wal/org-agenda-register-char`.
- Command `wal/kill-some-file-buffers` to run `kill-some-buffers` on
  buffers associated with files. Bound to `b` in `wal/misc-map`.
- Function `wal/java-test-dwim` that uses `transient` switches to call
  the right elisp command.
- Package `tempel`, replacing `yasnippet`.

### Changed

- User-prefixed `i` is now bound to `consult-register-load`;
  `consult-register` is still available via `ctl-x-r-map`.
- Function `wal/message-in-a-bottle` now uses the blue whale as a
  default (although any other string may optionally be passed.
- Font functions now display the current font height/family.
- `wal/org-roam-dispatch` is now bound to user-prefixed `y`.
- Help-like buffers are now displayed using `wdb/pop-up` instead of
  `wdb/direction`. Some side-buffers are no longer targetable by
  `other-window`.
- `popper` no longer groups.
- `transient` for config prefixes checkers with `c`.
- `wal/config-ascii-whale` uses different border thicknesses for
  different whales.
- Messages buffer is now using `wdb/pop-up` and is considered a
  `popper` pop-up.
- `help-mode` buffers are no longer pop-ups.
- The bindings for commands from the `consult` package have changed.
  The main change is that `c` is used instead of `u` in foreign maps.
- User-prefixed `u` now binds `consult-register-store`.
- `org-roam` is now bound in `major` to `r`; `verb` to `v`.
- User-prefixed `l` now calls `wal/avy-goto-line` that goes to end by
  default and to beginning if called with `C-u`. Those two actions are
  no longer bound in `goto-map`.

### Removed

- Package `yasnippet` was removed in favor of `tempel`.
- Package `vundo`.
- Package `org-transclusion`.
- Package `crux`.
- Package `org-bullets`.
- Package `display-wttr`.

## [v1.9.1]

Very cash money

### Added

- New ASCII blue whale as the default for the config animation; new
  custom variable `wal/config-ascii-whale` can be set to `cachalot` to
  get the old one.

### Changed

- `wal/lighthouse` is now bound to `C-c p`.
- `wal/agenda` was removed in favor of using the dispatch.
- Default value for `wal/idle-delay` was greatly reduced again to
  **0.8**.
- `dap-mode` now shows locals and REPL; both buffers' display has been
  overridden. It also stores and reloads window configurations and
  re-displays the custom `hydra` when hitting a breakpoint.
- Consult functions are now defined in own map bound to `u` in
  `lieutenant`.
- Most of the sink of `major` was moved to `general` as they're
  built-in and relate to the editor.
- Consult functions are now defined in own map bound to `u` in
  `lieutenant`.
- `completion-at-point` is now bound globally using `C-M-i` (like in
  other maps).
- The blue whale is also used for `wal-line`.

### Fixed

- Function `pet-find-executable` is advised to return the argument it
  was called with if it returns `nil`.
- Adds new function `wal/aw-delete-other-windows` to
  `delete-other-windows` while ignoring window parameters.

## [v1.9.0]

Vague novelty.

### Added

- Package `lsp-sonarlint` as an LSP expansion pack.
- Custom variable `wal/dwim-key` that is used in `consult` to preview,
  in `transient` to show and in `corfu` to `corfu-quick-complete`.
- `dashboard` is now also shown for daemon.
- `dashboard` truncates path beginnings.
- New macro `setq-unless` to only set a variable if it is not already
  set as an alternative to mirror `defcustom` variables.
- New function `wal/kmacro` to start/stop recording; bound to `k` in
  `lieutenant`.

### Changed

- Default value for `wal/idle-delay` was increased to **1.6**.
- Customized leading char face for `ace-window` to be bigger and
  inherit `mode-line-emphasis`.
- Ligatures are now declared in individual packages.
- External package no longer use `C-c` prefix. Exceptions are
  functions using `consult` wrappers. They were mostly moved to
  `major`.
- Leader key `colonel` is now responsible for config-related actions.
- Map `wal/fly` is now mapped to user-prefixed `f` and is an amaranth
  `hydra`.
- User-prefixed `i` now runs `consult-register`.
- `i` in `lieutenant` now binds `ctl-x-r-map` (register and
  rectangle).
- `hydra`s use `lv` again.

### Removed

- Package `perspective` has been mothballed in favor of using
  registers.
- Various `defcustom` variables were removed or made into local
  variables.
- Dailies are no longer part of the agenda.
- User-prefixed `y` no longer expands `yasnippet`; instead the default
  tab expansion was re-enabled.

## [v1.8.1]

Digital proxemics.

### Added

- Macro `wal/transient-define-prefix-once` to ensure that
  re-evaluating macro will not re-define the transient (unless
  variable `wal/transient-may-redefine` is `t`)

### Changed

- Leaders have switched places again to have rising ranks mirror
  rising responsibility. The custom variables have been replaced by
  singular variable `wal/key-reach` that holds a list of keys ordered
  by how reachable they are.
- Leader `colonel` is currently out of service again; its bindings
  have been folded into `major`; this required some bindings to change
  (`vundo` now uses `u`, `winner` uses `w`).
- Leader `major` now has a sink again.
- `hydra` is now displayed in the minibuffer.
- `hydra`s have been reworked to only be on a single line.
- `flycheck` error list now uses `wdb/side` to show at the bottom
  instead.
- `scroll-{up,down}-command` are now bound in `wal/fly` using keys `,`
  and `.`.
- `wal/fly` no longer binds `avy` commands; instead the `goto-map` is
  now again bound to user-prefixed `l`; the command is now bound to
  user-prefixed `'`.
- `hydra` for `text-resize` now also binds global adjustment; the
  reset also resets global one.

### Fixed

- Flag capturing in **Settings** section was moved up to make sure
  flags `doctor` and `deny` are captured before `use-package` is
  configured.

## [v1.8.0]

From hero to zero.

### Added

- `iedit-mode` is now bound in `colonel` using `i`.
- Command `wal/vterm-run` to quickly run and display command executed
  in a `vterm` buffer.
- Package `org-habit` now has a configuration.
- New parameter `lieutenant` in `wal/hook` that will bind function
  `wal/<mode-name>-dispatch` to the `lieutenant` key. This now the
  default access to major-mode specific `transient`s.
- `org-super-agenda` now adds a new-line after the final group.
- `graphql` as a Web extra (implicit also `graphql-mode`).
- Command `wal/prettier-refresh` to do just that.
- New macro `wal/when-ready` to execute a body using appropriate hook
  after start-up.
- Package `jest` as a `javascript` extra.
- `xah-fly-key`-like `hydra` to move, jump and delete.
- Function `wal/advise-many` to do just that.

### Changed

- `popper` groups by `perspective` again.
- `projectile-per-project-compilation-buffer` is set to `t`.
- `css-mode` now uses `lsp-mode`; if `stylelint` executable is found,
  that checker is explicitly selected.
- `corfu-auto-prefix` is now 2 and `corfu-auto-delay` is 0.2 for
  `js-mode`-derived modes when using `lsp-mode`.
- `lispy` is no longer activated immediately, it is instead bound in
  `transient`.
- Leader keys `captain` and `major` have been swapped.
- The default keys for the leader keys has changed.
- `popper` now uses user-prefixed keys `[` and `]` instead of being
  bound in (now) `major`.

### Removed

- Bindings in `lieutenant` were moved to `C-c *` bindings.
- Macro `wal/major!` has been removed as it's obsoleted by new
  `lieutenant` bindings.
- Packages `js2-mode` and `rjsx-mode` were removed in favor of using
  built-in `js-mode`.
- Many `defcustom` variables that weren't particularly useful.

### Fixed

- Command `wal/avy-action-zip-to-char` is now called
  `avy-action-zip-to-char` to not have a garbled help text.
- Commands `dap-java-{debug,run}-test-{class,method}` are now advised
  to use `bash` shell as `fish` seems to garble the class paths.

## [v1.7.14]

Cooled beans.

### Added

- Function `wal/kill-dired-buffers-in-perspective` to do just that.
- Configuration for the XML language server in `lsp-mode`.
- Binds `lsp-javascript-rename-file` in transient for `js2-mode`.
- New custom variable `wal/hidpi` that signals whether the screen is
  HiDPI. Setting this to true currently just increases the fringe
  width.
- Package `jenkinsfile-mode` as an expansion pack.
- New option `:prog-like` to `wal/hook` macro. If set to `t`
  `prog-like-hook` hooks will be run. These include `yas-minor-mode`,
  `drag-stuff-mode` and similar enhancing modes.

### Changed

- Command to find dailies directory is now bound in `org-roam`'s
  transient.
- The recipes for using hyper key were switched since the less risky
  one indeed does work.
- Command `wal/agenda` now stores and tries to restore window
  configuration that is created on first call; can be forced to skip
  if called with `universal-argument`.
- Variable `dired-kill-when-opening-new-dired-buffer` is set to `t`
  again.
- Variable `wal/modern-emacs` was replaced by function
  `wal/modern-emacs-p`.
- The ligatures for `html-mode` were improved and copied for
  `nxml-mode`.
- Command `consult-ripgrep` is now available as a suffix of `rg-menu`
  `transient` using key `u`.
- All `transient`s have been improved by moving external suffixes to
  the respective packages.

### Fixed

- Function `org-store-log-note` is now advised to remove agenda
  buffers from the perspective to avoid cluttering.
- Function `wal/insert-after` check if item is already in list before
  inserting.

### Removed

- The `transient`s for register, yas, consult, perspective, and
  projectile.

## [v1.7.13]

Give them the boot.

### Added

- Macro `wal/on-boot` was added to only evaluate a body during
  bootstrapping through `wal/bootstrap`, controlled by variable
  `wal/booting` set in `wal/prelude`.
- Sub-packages that have side-effect code now use this macro.
- Function `wal/kill-async-process-buffers` to do just that; used for
  coverage check.
- Function `wal/matches-in-string` to collect all regex matches in a
  string.
- Various tests to increase coverage; tests for `wal-look` and
  `wal-fonts`.
- Adds `use-package` keyword `:wal-bind-keymap` to do just that.
- User-prefixed `l` now uses this feature to bind `goto-map`.
- New custom variable `wal/prefer-rjsx` that is used to hook into
  `js2-mode` and allows switching to `rjsx-mode` for plain JS files.
- Remove `M-o` binding from `mhtml-mode`.
- Files with ending `.tsx` now also load `typescript-mode`.
- Built-in packages `mhtml-mode` and `nxml-mode` now have a
  configuration.

### Changed

- Function `wal/async-process` now takes a fourth optional argument to
  suffix the buffer created.
- The test helper section was moved to **Additional Package Files**.
- Function `wal/check-coverage` now calculates the overall coverage.
- The coverage status was moved to section **Package-Like**.
- Directory `wal/site-lisp-directory` is itself added to the
  `load-path` again.
- User-prefixed `h` no calls `projectile-find-file` to free
  user-prefixed `l`.
- Various bindings in `goto-map` were added for `avy`.
- Indentation functions no longer call `hack-local-variables`; this is
  now done by `wal/hook` macro.
- History size for `vterm` was increased tenfold.
- Custom `hydra` for `dap-mode` now also bind `dap-disconnect` and
  `dap-ui-repl`.
- Switches back to `traditional` way of displaying `magit`.

### Fixed

- Package `use-package` is now required after it could have been installed.
- Adding MELPA and requiring `package` now is odne in the **External
  Packages** section.
- Various `transient`s no longer reference potentially undefined
  commands. They now instead use custom suffixes.

### Removed

- `walled-mode` was replaced by function `wal/l` that serves the same
  purpose but is more testable.
- Various superfluous footnotes.
- `projectile-maybe-read-command` is no longer advised since
  `universal-argument` is swallowed by `transient`.
- No longer fixes the `jdtls` version.

## [v1.7.12]

Covert operation.

### Added

- Package `wal-prelude` now provides all functionality to bootstrap
  the configuration. Both the provided `init.el` template and
  `wal-setup-ci.el` use it.
- A Cask file was added to anticipate the package-ification of the
  config and to run tests.
- Tests were added for functions moved out of `wal-settings`.
- New package `wal-external` was added to hold the code to install the
  core packages as well as the `quelpa` configuration.
- The tests are now covered using `undercover`; this is done in the CI
  env and can be done locally using new function `wal/check-coverage`.

### Changed

- All `use-package` definitions now use a more relaxed style, leaving
  a blank space between sections. Style-guide was updated as well.
- Guideline for variable definitions changed. Values should always
  start on the first line.
- `isearch-lazy-count` is now `t`.
- Variable `wal/experimental` was renamed to `wal/modern-emacs`.
- The tests were moved from `test/` to the README configuration.
- Functions were moved out of `wal-settings` to `wal-func`.
- Settings were moved out of `wal-func` to `wal-settings`.
- The style-guide and cheat-sheet were moved to `docs/`.

### Fixed

- Function `wal/flycheck-file` kills buffers even when there are no
  errors.

### Removed

- The legacy Org files were discarded.

## [v1.7.11]

Action-backed.

### Added

- New flag `--deny` to disable setting `use-package-always-ensure` to
  `t`.
- Adds GitHub CI configuration to run the tests. This uses new custom
  file `wal-setup-ci` to tangle the config and set the package path
  using `GITHUB_WORKSPACE` environment variable.
- Tests for `wal/truncate`, `wal/reset-to-standard` and
  `wal/dead-shell-p`.
- Package `lsp-pyright`; extra for Python expansion pack.
- Package `python-black`, available through `transient`.
- Macro `wal/hook` now allows setting additional ignores for
  `lsp-file-watch-ignored-directories` using key `:lsp-ignores`.
- Function `wal/in-python-project-p` to check just that.
- Function `wal/lsp-pyright-install-stubs` to do just that.
- Added function from `corfu` README to enable it in minibuffer.
- Adds package `pet` and uses it to set up virtual environment paths.
- Functions to view this changelog and see the diff range since latest
  tag added to the editing transient.
- Function `wal/set-cursor-type` to do just that, bound in `general`.
- Custom `wal/consult-line` to use `thing-at-point` if called with
  `C-u`.

### Changed

- Function `wal/dead-shell-p` was moved to `wal-func`.
- `C-c a` now switches to initial perspective, opens the tasks
  directory and then opens the agenda in one go using new function
  `wal/agenda`. Default `org-agenda` is no longer advised to skip the
  dispatch.
- Packages are now responsible for setting the virtual environment
  path.
- Macro `wal/lang-hook` is now called `wal/hook` and used for
  non-language major modes.
- Package `lispy` was customized and now respects
  `multiple-cursors-mode`.
- Function `wal/capture-flag` is now a macro.

### Removed

- Package `org-sticky-header` has been mothballed in favor of new
  segment in `wal-line`.
- Package `ctrlf` has been mothballed in favor using `consult-line`.
- Package `beacon` has been mothballed in favor of using `pulse`.
- The `gc-cons-threshold` is no longer meddled with when setting up
  the minibuffer.

### Fixed

- Function `wal/truncate` now defaults to a length of 8 if `max-len`
  argument isn't provided.
- Custom `completion-style` was removed as it was buggy.
- Positioning of whale animation while editing the config no longer
  subtracts the fringe width from the horizontal offset.
- Variable `read-process-output-max` is set to 1 MB as this is the
  default max for `/proc/sys/fs/pipe-max-size`. The associated custom
  variable was removed.

## [1.7.10]

It gets lispier.

### Added

- First test suite for testing list utility in `wal-func`.
- New function `wal/flycheck-file` to do just that.
- New function `wal/flycheck-config-packages` to check all config
  packages using function above.
- The style guide now requires that package configuration code blocks
  are structured and ideally have some comments explaining non-obvious
  statements.
- New package `lispy` for better paren traversal.

### Changed

- All used advice combinators now follow a template (detailed in
  style-guide) instead of just prefixing the advised function with
  `wal/advise` to the advised.
- Binding in `wal/general` to quit now calls `delete-frame` in daemon
  mode.
- All utility functions editing lists now expect a symbol as their
  first argument.
- Settings are set before activating global modes (needed for
  `recentf` fix below).
- Package `vundo` is now bound in `major`. Function `wal/transparency`
  is now bound in `general`.
- Helper function `wal/display-buffer-condition` now uses an alist to
  provide major-mode conditional.
- The ASCII whale animation is now bigger and slightly offset; the
  code for it now is in the new relaxed, unfurled style.
- The two existing flags `mini` and `doctor` now use the prefix
  `wal/flag`.

### Fixed

- Package `org-roam` loads on first call of `wal/org-roam` again;
  problem was that the `:config` was referencing custom variables not
  yet set.
- Sets `recentf-filename-handlers` to nil so that
  `consult--source-project-recent-file` works that doesn't respect
  abbreviation.
- Agenda files now get the expanded directory, not the symbol.
- Custom exit function should work in `-nw` Emacs sessions.
- The hook for `python-mode` no longer expects `poetry` to be present
  and the function to activate the virtual environment no longer
  pretends to work for non-poetry projects (this addresses a change in
  that package).
- A `pyvenv` hook now takes care of setting the correct `lsp-pylsp`
  variables.
- Python packages are no longer demanded.

## [1.7.9] - 2021-08-20

Hunt and edit.

### Added

- New custom flag `--mini` that will do the same thing as setting
  `wal/minimal` (only loading packages that don't have `:wal-ways` set
  to `nil`).
- Macro `wal-define-expansion-pack` now also accepts passing MELPA
  recipes using key `:recipes`. Recipes and packages are treated
  equally in the `marginalia` annotation.
- Command `helpful-kill-buffers` is not bound in `help-map` to `C-x`.
- Configuration and expansion pack for `jakt-mode`.
- Sets `org-cycle-separator-lines` to 1 to avoid cramped look when
  headlines are collapsed.
- Docker container pop-ups now have a regex for
  `popper-echo-transform`; the `docker-compose` one was improved as
  well.
- Speed command for refiling (`w`) in an `org-roam` buffer now uses
  `org-roam-refile` unless called with `universal-argument`.
- Binds `org-toggle-timestamp-type` in the mode's `transient`.
- Capture template for `org-roam` dailies now sets current time
  (inactive) and task as props.
- Package `ef-themes`, configuring it to not mess with org headings.
- Package `corfu` now uses custom `orderless` dispatch (taken from
  official docs) when editing `lsp-mode` buffer.
- `recentf-mode` is enabled explicitly.
- Command `consult-ripgrep` is now bound to `C-c g`; it also uses new
  advice `wal/with-big-vertico` to increase `vertico-count`.
- List helper function `wal/insert-after` to insert an element after a
  preceding item. This also replaces function
  `wal/insert-use-package-keyword`. It is also used to add custom
  `consult` source for Dired earlier.
- Moving items up and down now bound in `markdown-mode` with `M-{<up>,
  <down>}`.

### Changed

- The default key for the `transient` in `major` is `.` again since
  the sink is gone.
- Bindings using `wal/major!` now set `which-key` replacement string
  "MAJOR!".
- Custom `hydra`s are bound passing `which-key` replacement string to
  make them more readable (for example to have `text-scale` instead of
  `wal/text-scale/body`).
- Function `wal/flyspell` also just uses `flyspell` for replacement.
- Command `dictionary-lookup-definition` is now bound using key `f`.
- Many of the pop-ups are now using `wdb/side` which puts them at the
  bottom.
- Pop-ups are now grouped using `projectile` or `project` instead of
  `perspective` which often fails.
- Compilation buffers are now shown in a pop-up. So are `rg` buffers.
- The `regexp-builder` buffer is filtered by `consult`.
- Since the package is now available from MELPA, `ligature` no longer
  uses `quelpa`.
- The prefix for `org-roam` was changed back to `H-<SPC>`.
- Only the dailies of `org-roam` are now added to the agenda files.
- Instead of binding `C-c C-c` and `C-c C-k` to `server-edit` (and
  `abort`), the default quit action is now `wal/edit-or-kill` that
  aims to make the right call.
- `lsp-ui-sideline-mode` is no longer enabled by default.

### Removed

- Function `wal/install-packages` no longer has key `:on-done` to
  print a message after completion. This is now expected to be done by
  the caller.
- The `org-roam` capture template setting active timestamps was
  removed; as was the advice to select the default template when going
  to a date.

### Fixed

- Package `typo-mode` is deferred after `text-mode`.
- Default `org-capture` is bound to `o` in `org-roam` `transient`.
- Adds missing `defvar` declarations to allow for dynamic binding.
- Macro `wal/lang-hook` now consumes passed doc string by producing a
  full function definition before adding the hook.
- Package `gdscript-mode` is now called that in the expansion pack,
  not `gdscript-godot`.
- Clocking in using `consult` now switches to the initial perspective
  beforehand to avoid cluttering the current one with unrelated
  buffers.

## [1.7.8] - 2021-08-13

Captain my captain.

### Added

- Added `gdscript-mode` and configuration.
- If a buffer was requested from a server client `C-c C-c` will call
  `server-edit` and `C-c C-k` will call `server-edit-abort`.
- Added package `base16-theme`.
- All packages now set `lexical-binding` to `t`.
- Added functions from `consult` wiki to clock in on agenda items and
  to use `thing-at-point` for `consult-line`.

### Changed

- `M-q` is now bound to `vertico-quick-exit` in `vertico` map;
  `quick-insert` is bound to `C-q`.
- `M-q` is now bound to `coruf-quick-exit` in `corfu` map;
  `quick-complete` is bound to `C-q`.
- Bindings in `colonel` have been re-assigned across it and its
  re-added mirroring `sink`.
- Responsibilities for `major`, `captain` and `lieutenant` have
  changed. Buffer-related actions, whether from `major-` or
  `minor-mode` are bound in `major`; global (or perspective-wide)
  actions in `captain`. Commands bound to `lieutenant` have been moved
  to `major`; `lieutenant` is now responsible for command maps. The
  `transient`s for `major-mode`s are now bound in `major` using new
  variable `wal/major!-key` (default is `m`) to allow for the definer
  to still have a sink.
- The `transient` for `consult` was slightly improved, no longer
  binding already bound commands and instead binding `apropos`,
  `keep-lines` and opening files externally.
- `C-c c` now does a `completion-at-point`.
- Commands `popper-toggle-latest` and `wal/org-roam` were moved to
  `captain`.
- Macro `wal/major!` now uses `H-. m`.
- The effect of `goggles` is more pronounced.
- Zoning is now bound to `0` in `captain`.
- Bindings that are not a built-in sub-program were removed from
  `general-sink`.

### Fixed

- Commands bound in `transient` for `org-mode` are more context-aware.
- Fixes `corfu` and `orderless` actually working with `lsp-mode`.
- Adapts advice for `vertico-directory` to work again after advised
  function was removed.
- Makes `projectile` not pass potentially missing switch to `fd`.
- Loading of `mu4e` should work again by removing it from the
  expansion pack (which would check if it was installed using
  `package`). The command is now bound to `C-c m`.
- Deleting window with `ace-window` now uses `x` (previously `k` was
  bound which would be used to switch).
- Package `dumb-jump` now uses `rg`.

### Removed

- Package `restart-emacs` was mothballed in favor of newly built-in
  command of the same name.
- Package `imenu-list` was mothballed.

## [1.7.7] - 2021-08-03

Viz major.

### Added

- Several `transient`s for various `major-mode`s and libraries. The
  package is now demanded.
- New macro `wal/major!` to more easily bind to `H-. .`.
- Commands bound to `wal/colonel` are now described.

### Changed

- There are no bindings in `wal/major` anymore that belong to a
  particular `major-mode`, and it no longer has a sink. Instead the
  `wal/major-key` calls the mode's `transient` (see above).
- The dispatch for this config is now bound to `C-c 9` (in its
  buffer).
- Packages `projectile`, `perspective` and `yasnippet` no longer use
  `wal/univ` macro. The transients are now bound in `wal/captain`
  using the same key.
- Package `lsp-mode` no longer uses `H-h`, the transient is bound in
  `wal/captain` as well (using `l`).
- Command maps for `projectile`, `perspective` and `lsp-mode` are now
  bound using uppercase keys in `wal/captain`.
- Non-minor-mode command maps are bound using their keys.
- The `hydra` for `winner` as well as `hs-toggle-hiding` were moved to
  `captain`.
- Some `:defer` values were removed or reduced.

### Removed

- Package `explain-pause-mode` was mothballed.

### Fixed

- `general` binding for `kmacro` command map.
- Package configurations no longer use both `:demand` and `:commands`.

## [1.7.6] - 2021-07-28

Hook, line and syncer.

### Added

- Maps `org-next-visible-heading` (and `previous`) to `M-n` and `M-p`
  respectively.
- New function `wal/maybe-make-directory` to do just that.
- New macro `wal/lang-hook`. It controls indentation, fluff messages
  and whether `lsp-mode` needs to be turned on. This macro replaces
  (almost) all `wal/<mode>-hook` functions.
- `C-c s` now calls `consult-line`.
- The register dispatch has been greatly expanded and renamed.

### Changed

- The advice for `list-buffers--refresh` no longer filters if called
  with `C-u`.
- Function `wal/other-window` will now call `other-frame` if another
  frame exists.
- Disabling and enabling tabs now already takes care of hacking local
  variables appropriately (when disabling, only when called
  non-interactively).
- Use `M-q` for `vertico-quick` (like `corfu-quick`).
- The `transient` for `consult` now binds
  `wal/consult-ripgrep-ignored` again.
- Leader key `seargent` (`org-roam`) was removed. The package now has
  a `transient` that is bound to `C-c c`.
- The custom `beacon-blink` now uses `C-c b`.
- Finding daily file with `org-roam` is now advised to preselect the
  default template.
- `C-c r` now calls `transient` for register functions, no longer
  `completion-at-point`.
- Function `org-agenda` is now advised to not prompt and instead go to
  the day's agenda directly.

### Fixed

- Turning off `org-tree-slide` now only turns on modes that were
  actually turned off for it. `dimmer-mode` is now also turned off.
  `beacon-mode` is no longer turned on or off.

### Removed

- The unused `with-mode` keyword was removed from
  `wal/maybe-enable-tabs`.
- Leader key

## [1.7.5] - 2021-07-17

Hefty lefty.

### Added

- Function to write a `dap-mode` template into a JSON and save that as
  a file.
- Macro `wal/try` to do a safe `require` before executing body.
- Added `hydra` for `smerge-mode`.

### Changed

- `dap-mode`-Template for `debugpy` is now an interactive function.
- The `consult` dispatch is bound to `wal/univ`-ed `consult-buffer`
  again.
- `H-u` doubles for `C-u` now.
- Binds `yas-insert-snippet` in its `transient`.
- Adds `wal/univ` commands to `consult` and `projectile` configurations.
- Improves (and extends) the `transient` for `consult`.
- Binds `persp-forget-buffer` in its `transient`; the function
  definitions were shuffled a bit.
- Keymaps for `kmacro` and `bookmark` are now bound in `wal/captain`.
- Kubernetes is now user-prefixed (meaning the default is `H-8`).
- Moves non-letter user-prefixed bindings to use `C-c` instead. This
  was done to both relieve some pinky pressure and potentially free
  prefixed symbol keys.

### Fixed

- Several invocations of `H-u` call `universal-argument-more` now so
  it can be used like `C-u`. Previously only one argument was
  possible.

### Removed

- Package `perspective` is now always enabled, custom variable
  `wal/use-perspectives` was removed.
- Valid efforts are no longer set for `org-mode`.

## [1.7.4] - 2021-07-09

Perpetual transience.

### Added

- Replaced `json-mode` with `jsonian-mode`.
- Expansion pack extras can now be installed individually **and** in
  bulk.
- Going to clocked task bound in `goto-map` + "t".
- Explicit `transient` configuration.
- Binds `avy-goto-char` and `avy-goto-word-0` in `goto-map`.
- Binds more commands in the `transient` for `consult`.
- Creates `transient` maps for `projectile`, `perspective`,
  `yasnippet` and binds them to their keys using `wal/univ`.
- Creates `transient` for `verb` and its response buffers.

### Changed

- Response buffers for `verb` now use directional display.
- Improved bindings for `org-roam`, notably to bind capturing "today".
- Package `org-roam` is now an expansion itself.
- The templates used by `org-roam` have been slightly adapted; the one
  for daily files has an alternative one for adding an active
  timestamp.
- The expansion packs were slightly adapted.
- Package `yasnippet` was reverted to old hook-based configuration.
- Jumping to line and word was moved to `goto-map` using "l" and "w";
  function `wal/avy-jump` was renamed to `wal/avy-goto-word`.
- The freed keys ("j" and "l") are now used by `consult-buffer` and
  `projectile-` and `project-find-file` respectively.
- Package `projectile` uses "p" again for its prefixes.
- Package `lsp-mode` uses "l" again for its prefixes and "h" for the
  dispatch.
- Binding `:wal-bind` for "u" is bound to the dispatch for now.
- Some functions were moved to `wal-func`.
- Forces `compile-mode` to scroll.

### Removed

- Package `org-alert` was mothballed as it doesn't reliably work and
  freezes Emacs too much.

## [1.7.3] - 2021-06-26

Active pause.

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
- Hack to fix autoloads for `kubernetes-overview` (seemingly fixed
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
