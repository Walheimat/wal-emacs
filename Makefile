EMACS?=emacs
WAL_SOURCE_DIR?=$(CURDIR)
EMACS_INIT_FILE?=$(HOME)/.emacs.d/init.el

WITH_PRELUDE=$(EMACS) --batch -l ./wal-prelude.el
BOOTSTRAP=--eval "(wal-prelude-bootstrap \"$(WAL_SOURCE_DIR)\" t)"
TEST_ARGS=
ALLOW_PACIFY_FAILURE=nil

# Run `make V=1 {cmd}` to print commands
$(V).SILENT:

# Set up commit linting
commits:
	npm install
	npx husky install

# Simulate a cold boot
cold-boot:
	$(WITH_PRELUDE) --eval "(wal-prelude-bootstrap \"$(WAL_SOURCE_DIR)\" nil t)"

# Check the package files with flycheck
pacify:
	$(WITH_PRELUDE) $(BOOTSTRAP) -l ./tools/wal-pacify.el --eval "(wal-pacify-check $(ALLOW_PACIFY_FAILURE))"

# Remove the build folder
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

# Install with cask
cask-install:
	cask install

# Run tests using cask
cask-test:
	cask exec ert-runner $(TEST_ARGS)

ci: tangle cask-install cask-test pacify
