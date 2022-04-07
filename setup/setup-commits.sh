#!/bin/bash
#
# Install npm dependencies and husky.

# shellcheck disable=SC2119

source "./variables.sh"
source "./common.sh"

function wal::install_npm_dependencies() {
  signal "Installing ${bold}npm${reset} packages"

  if ! npm install &>"$WAL_LOG"; then
    die
  fi
  live
}

function wal::install_husky() {
  signal "Installing ${bold}husky${reset}"

  if ! npx husky install &>"$WAL_LOG"; then
    die
  fi
  live
}

if ensure_log_file; then
  cd ..

  echo -e "${blue}${whale}${reset} ${bold}[commit hooks setup]${reset}"

  wal::install_npm_dependencies
  wal::install_husky

  echo -e "${green}${whale}${reset} commit setup complete"
fi
