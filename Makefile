PACKAGE_MARKER=$(HOME)/.emacs.d/elpa/whale-line/whale-line.el

LOCAL_DEPS=build
LOCAL_PHONY_DEPS=ensure-init $(PACKAGE_MARKER)
CI_DEPS=build
PACIFY_DEPS=build

CURRENT_PACKAGE_VERSION=2.2.2
UPDATE_VERSION_FILES=lib/wal-config.org wal.el

include dinghy/emacs-package.mk

EMACS_INIT_FILE?=$(HOME)/.emacs.d/init.el

WITH_PRELUDE=$(EMACS) --batch -l ./wal.el

BOOTSTRAP_MODE=plain
BOOTSTRAP=--eval "(wal-bootstrap \"$(CURDIR)\" '$(BOOTSTRAP_MODE))"

INIT_CLEAR=nil
INIT=--eval "(wal-init \"$(EMACS_INIT_FILE)\" \"$(CURDIR)\" $(INIT_CLEAR))"

# Tangle all library files
build:
	$(WITH_PRELUDE) $(BOOTSTRAP)

.PHONY: tangle
tangle: clean
	$(WITH_PRELUDE) $(BOOTSTRAP)

# Make sure the user's init file contains the bootstrapper
.PHONY: ensure-init
ensure-init:
	$(WITH_PRELUDE) $(INIT)

# Make sure packages have been installed
$(PACKAGE_MARKER): BOOTSTRAP_MODE=ensure
$(PACKAGE_MARKER):
	$(WITH_PRELUDE) $(BOOTSTRAP)

# Update
update-git:
	$(info Pulling changes)
	git pull

.PHONY: update
update: update-git clean ensure-init $(PACKAGE_MARKER)

# -- Checks

# Simulate a cold boot
.PHONY: cold-boot
cold-boot: BOOTSTRAP_MODE=cold
cold-boot:
	$(WITH_PRELUDE) $(BOOTSTRAP)

.PHONY: uninstall
uninstall: INIT_CLEAR=t
uninstall:
	$(WITH_PRELUDE) $(INIT)
