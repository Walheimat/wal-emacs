EMACS?=emacs
WAL_SOURCE_DIR?=$(CURDIR)
EMACS_INIT_FILE?=$(HOME)/.emacs.d/init.el

WITH_PRELUDE=$(EMACS) --batch -l ./wal-prelude.el
BOOTSTRAP=--eval "(wal-prelude-bootstrap \"$(WAL_SOURCE_DIR)\" t)"
UPDATE_VERSION=./tools/update-version.sh
TEST_ARGS=

# Run `make V=1 {cmd}` to print commands
$(V).SILENT:

# -- Default goal

ifdef CI
install: .cask
else
install: build ensure
	$(info Run $(EMACS) with flag --ensure to install packages)
endif

# Tangle all library files
build:
	$(WITH_PRELUDE) $(BOOTSTRAP)

# Install Cask dependencies
.cask: build
	cask install

# Ensure that the user's init file contains the necessary code to
# bootstrap and load the configuration
ensure:
	$(WITH_PRELUDE) --eval "(wal-prelude--ensure-init \"$(EMACS_INIT_FILE)\" \"$(WAL_SOURCE_DIR)\")"


.PHONY: clean
clean:
	$(info Removing build folder)
	rm -rf build

.PHONY: clobber
clobber: clean
	$(info Removing Cask folder)
	rm -rf .cask

clean-install: clean install

# -- Checks

# Run tests using cask
.PHONY: test
test: build .cask
	cask exec ert-runner $(TEST_ARGS)

# Check the package files with flycheck
.PHONY: pacify
pacify: build
	$(WITH_PRELUDE) $(BOOTSTRAP) -l ./tools/wal-pacify.el -f wal-pacify-check

# Simulate a cold boot
.PHONY: cold-boot
cold-boot:
	$(WITH_PRELUDE) --eval "(wal-prelude-bootstrap \"$(WAL_SOURCE_DIR)\" nil t)"

# -- Utility

update-version:
	$(UPDATE_VERSION) Cask
	$(UPDATE_VERSION) lib/wal-config.org

# -- Commit linting setup

commits: node_modules .husky/_/husky.sh

node_modules:
	npm install

.husky/_/husky.sh:
	npx husky install

clean-commits:
	$(info Removing node modules and husky script)
	rm -rf ./node_modules
	rm -rf ./.husky/_
