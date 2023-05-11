EMACS?=emacs
WAL_SOURCE_DIR?=$(CURDIR)
EMACS_INIT_FILE?=$(HOME)/.emacs.d/init.el

WITH_PRELUDE=$(EMACS) --batch -l ./wal-prelude.el
BOOTSTRAP=--eval "(wal-prelude-bootstrap \"$(WAL_SOURCE_DIR)\" t)"
UPDATE_VERSION=./tools/update-version.sh
TEST_ARGS=

# Run `make V=1 {cmd}` to print commands
$(V).SILENT:

# Set up commit linting
commits:
	npm install
	npx husky install

# Simulate a cold boot
cold-boot:
	$(WITH_PRELUDE) --eval "(wal-prelude-bootstrap \"$(WAL_SOURCE_DIR)\" nil t)"

# Update references to old version
update-version:
	$(UPDATE_VERSION) Cask
	$(UPDATE_VERSION) lib/wal-config.org

# Check the package files with flycheck
pacify:
	$(WITH_PRELUDE) $(BOOTSTRAP) -l ./tools/wal-pacify.el -f wal-pacify-check

# Remove the build folder
.PHONY: clean
clean:
	$(info Removing build folder)
	rm -rf ./build

# Tangle all library files
tangle:
	$(WITH_PRELUDE) $(BOOTSTRAP)

# Ensure that the user's init file contains the necessary code to
# bootstrap and load the configuration
ensure:
	$(WITH_PRELUDE) --eval "(wal-prelude--ensure-init \"$(EMACS_INIT_FILE)\" \"$(WAL_SOURCE_DIR)\")"

install: clean ensure tangle
	$(info Run $(EMACS) with flag --ensure to install packages)

# Run tests using cask
.PHONY: test
test:
	cask exec ert-runner $(TEST_ARGS)

ci: tangle
	cask install
