#!/bin/bash
#
# Set the SOURCE_DIR relative from the script and load the cold-boot
# script.

SETUP_DIR="$(dirname "$(realpath "$0")")"

cd "$SETUP_DIR/".. || exit 1

EMACS_SOURCE_DIR="$(pwd)"
export EMACS_SOURCE_DIR

emacs --batch -l "$SETUP_DIR/wal-setup-cold-boot.el"
