#!/bin/bash
#
# Copy or link init file.

# shellcheck disable=SC2119

source "./variables.sh"
source "./common.sh"

DIR="${0%/*}"

# Helper functions.

function ensure_no_init() {
  local userdir=$1
  local method=$2

  if [[ -e "$userdir/init.el" || -e "$HOME/.emacs" ]]; then
    echo -e "Found existing init file, can't $method" >"$WAL_LOG"
    die
  fi
}

function on_complete() {
  echo -e "\n${green}${whale}${reset} init file setup complete, you can restart Emacs"
}

# Main functions.

function wal::link_init_file() {
  local userdir="${1:-$HOME}"

  signal "(symbolically) linking init file to ${userdir@Q}"

  ensure_no_init "$userdir" "link"

  if ! ln -s "$(cd ..; pwd)/templates/init.el" "$userdir/init.el"; then
    die
  fi
  live

  on_complete
}

function wal::copy_init_file() {
  local userdir="${1:-$HOME}"

  signal "copying init file to ${userdir@Q}"

  ensure_no_init "$userdir" "copy"

  if ! cp "$DIR/../templates/init.el" "$userdir/init.el" &>"$WAL_LOG"; then
    die
  fi
  live

  on_complete
}

echo -e "${blue}${whale}${reset} ${bold}[init file setup]${reset}"

func=$1

case $func in
  link)
    wal::link_init_file "${@:2}"
    ;;
  copy)
    wal::copy_init_file "${@:2}"
    ;;
  *)
    echo -e "${red}${whale}${reset} call with 'link' or 'copy'"
    ;;
esac
