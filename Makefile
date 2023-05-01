EMACS?=emacs
WAL_SOURCE_DIR?=$(CURDIR)
EMACS_INIT_FILE?=$(HOME)/.emacs.d/init.el

# Run `make V=1 {cmd}` to print commands
$(V).SILENT:

# Set up commit linting
commits:
	npm install
	npx husky install

# Simulate a cold boot
cold-boot:
	$(EMACS) --batch -l ./wal-prelude.el --eval "(wal-prelude-bootstrap \"$(WAL_SOURCE_DIR)\" nil t)"

# Check the package files with flycheck
pacify:
	$(EMACS) --batch -l ./wal-prelude.el --eval "(wal-prelude-bootstrap \"$(WAL_SOURCE_DIR)\" t)" -l ./tools/wal-pacify.el

# Remove the build folder
clean:
	$(info Removing build folder)
	rm -rf ./build

# Tangle all library files
tangle:
	$(EMACS) --batch -l ./wal-prelude.el --eval "(wal-prelude-bootstrap \"$(WAL_SOURCE_DIR)\" t)"

# Ensure that the user's init file contains the necessary code to
# bootstrap and load the configuration
ensure:
	$(EMACS) --batch -l ./wal-prelude.el --eval "(wal-prelude--ensure-init \"$(EMACS_INIT_FILE)\" \"$(WAL_SOURCE_DIR)\")"

install: clean ensure tangle
	$(info Run $(EMACS) with flag --ensure to install packages)

ci-install: tangle
	cask install
