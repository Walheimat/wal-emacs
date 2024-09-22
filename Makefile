# Dinghy setup

PACKAGE_MARKER=$(HOME)/.emacs.d/elpa/whale-line/whale-line.el

PACKAGE_NAME=wal-emacs
CURRENT_PACKAGE_VERSION=2.4.4
LOCAL_DEPS=build
LOCAL_PHONY_DEPS=ensure-init $(PACKAGE_MARKER)
CI_DEPS=build
PACIFY_DEPS=build
UPDATE_VERSION_FILES=lib/wal-config.org wal.el Makefile

include dinghy/emacs-package.mk

# Goals

WITH_PRELUDE=$(EMACS) --batch -l ./wal.el

EMACS_INIT_FILE?=$(HOME)/.emacs.d/init.el
INIT_CLEAR=nil
INIT=--eval "(wal-init \"$(EMACS_INIT_FILE)\" \"$(CURDIR)\" $(INIT_CLEAR))"

BOOTSTRAP_MODE=plain
BOOTSTRAP=--eval "(wal-bootstrap \"$(CURDIR)\" '$(BOOTSTRAP_MODE))"

.PHONY: tangle ensure-init force-ensure uninstall \
	update-git update upgrade cold-boot

## Installation

build:
	$(WITH_PRELUDE) $(BOOTSTRAP)

tangle: clean
	$(WITH_PRELUDE) $(BOOTSTRAP)

ensure-init:
	$(WITH_PRELUDE) $(INIT)

force-ensure: BOOTSTRAP_MODE=ensure
force-ensure:
	$(WITH_PRELUDE) $(BOOTSTRAP)

$(PACKAGE_MARKER): BOOTSTRAP_MODE=ensure
$(PACKAGE_MARKER):
	$(WITH_PRELUDE) $(BOOTSTRAP)

uninstall: INIT_CLEAR=t
uninstall:
	$(WITH_PRELUDE) $(INIT)

## Updating

update-git:
	$(info Pulling changes)
	git pull --recurse-submodules

update: update-git clean ensure-init $(PACKAGE_MARKER)

upgrade: BOOTSTRAP_MODE=upgrade
upgrade:
	$(WITH_PRELUDE) $(BOOTSTRAP) -f wal-upgrade

## Checks

cold-boot: BOOTSTRAP_MODE=cold
cold-boot:
	$(WITH_PRELUDE) $(BOOTSTRAP)
