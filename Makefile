PACKAGE_MARKER=$(HOME)/.emacs.d/elpa/whale-line/whale-line.el

LOCAL_DEPS=build
LOCAL_PHONY_DEPS=ensure-init $(PACKAGE_MARKER)

include dinghy/emacs-package.mk

EMACS_INIT_FILE?=$(HOME)/.emacs.d/init.el

WITH_PRELUDE=$(EMACS) --batch -l ./wal-prelude.el

BOOTSTRAP_MODE=plain
BOOTSTRAP=--eval "(wal-prelude-bootstrap \"$(CURDIR)\" '$(BOOTSTRAP_MODE))"

INIT_CLEAR=nil
INIT=--eval "(wal-prelude-init \"$(EMACS_INIT_FILE)\" \"$(CURDIR)\" $(INIT_CLEAR))"

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

# Check the package files with flycheck
.PHONY: pacify
pacify: build
	$(WITH_PRELUDE) $(BOOTSTRAP) -l ./tools/wal-pacify.el -f wal-pacify-check

# Simulate a cold boot
.PHONY: cold-boot
cold-boot: BOOTSTRAP_MODE=cold
cold-boot:
	$(WITH_PRELUDE) $(BOOTSTRAP)

# -- Commit linting setup

.PHONY: commits
commits: node_modules .husky/_/husky.sh

node_modules:
	npm install

.husky/_/husky.sh:
	npx husky install

# -- Cleaning

.PHONY: clobber
clobber: clean
	git config --unset core.hooksPath
	rm -rf node_modules
	rm -f .husky/_/husky.sh

.PHONY: uninstall
uninstall: INIT_CLEAR=t
uninstall:
	$(WITH_PRELUDE) $(INIT)
