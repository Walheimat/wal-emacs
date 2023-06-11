EMACS?=emacs
WAL_SOURCE_DIR?=$(CURDIR)
EMACS_INIT_FILE?=$(HOME)/.emacs.d/init.el
PACKAGE_MARKER=$(HOME)/.emacs.d/elpa/whale-line/whale-line.el

WITH_PRELUDE=$(EMACS) --batch -l ./wal-prelude.el

BOOTSTRAP_MODE=plain
BOOTSTRAP=--eval "(wal-prelude-bootstrap \"$(WAL_SOURCE_DIR)\" '$(BOOTSTRAP_MODE))"

INIT_CLEAR=nil
INIT=--eval "(wal-prelude-init \"$(EMACS_INIT_FILE)\" \"$(WAL_SOURCE_DIR)\" $(INIT_CLEAR))"

UPDATE_VERSION=./tools/update-version.sh

TEST_ARGS=
TEST_PRE_ARGS=

# Run `make V=1 {cmd}` to print commands
$(V).SILENT:

# -- Default goal

ifdef CI
install: ci
else
install: local
endif

# Do everything necessary for a local installation
.PHONY: local
local: build ensure-init $(PACKAGE_MARKER)

# Do everything necessary for a installation in CI environment
.PHONY: ci
ci: .cask

# Install Cask dependencies
.cask: build
	cask install

# Tangle all library files
build:
	$(WITH_PRELUDE) $(BOOTSTRAP)

# Make sure the user's init file contains the bootstrapper
.PHONY: ensure-init
ensure-init:
	$(WITH_PRELUDE) $(INIT)

# Make sure packages have been installed
$(PACKAGE_MARKER): BOOTSTRAP_MODE=ensure
$(PACKAGE_MARKER):
	$(WITH_PRELUDE) $(BOOTSTRAP)

# -- Checks

# Run tests using cask
.PHONY: test
test: build .cask
	$(TEST_PRE_ARGS) cask exec ert-runner $(TEST_ARGS)

# Check the package files with flycheck
.PHONY: pacify
pacify: build
	$(WITH_PRELUDE) $(BOOTSTRAP) -l ./tools/wal-pacify.el -f wal-pacify-check

# Simulate a cold boot
.PHONY: cold-boot
cold-boot: BOOTSTRAP_MODE=cold
cold-boot:
	$(WITH_PRELUDE) $(BOOTSTRAP)

# -- Utility

.PHONY: update-version
update-version:
	$(UPDATE_VERSION) Cask
	$(UPDATE_VERSION) lib/wal-config.org

# -- Commit linting setup

.PHONY: commits
commits: node_modules .husky/_/husky.sh

node_modules:
	npm install

.husky/_/husky.sh:
	npx husky install

# -- Cleaning

.PHONY: clean
clean:
	rm -rf build
	rm -rf .cask

.PHONY: clobber
clobber: clean
	git config --unset core.hooksPath
	rm -rf node_modules
	rm -f .husky/_/husky.sh

.PHONY: uninstall
uninstall: INIT_CLEAR=t
uninstall:
	$(WITH_PRELUDE) $(INIT)
