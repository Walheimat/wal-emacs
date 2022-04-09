#!/bin/bash
#
# Copy service template and enable the service.

# shellcheck disable=SC2119

source "./variables.sh"
source "./common.sh"

DIR="${0%/*}"

TARGET_PATH="$HOME/.config/systemd/user"

DEFAULT_EMACS_PATH="/usr/local/bin/emacs"

function wal::ensure() {
  if [[ ! -d "$TARGET_PATH" ]]; then
    signal "Creating path ${TARGET_PATH@Q}"
    if ! mkdir -p "$TARGET_PATH" &> "$WAL_LOG"; then
      die
    fi
    live
  fi

  if [[ -e "$TARGET_PATH/emacs.service" ]]; then
    echo -e "Found existing service configuration, won't override" >"$WAL_LOG"
    die
  fi
}

function wal::copy_service_template() {
  wal::ensure

  local emacs_path
  emacs_path=$(which emacs)

  signal "Copying ${bold}service template${reset} using ${emacs_path@Q}"

  if ! sed "s_${DEFAULT_EMACS_PATH}_${emacs_path}_" "$DIR/templates/emacs.service" > "$TARGET_PATH/emacs.service"; then
    die
  fi
  live
}

function wal::enable_service() {
  signal "Enabling service"

  if ! systemctl --user enable emacs &> "$WAL_LOG"; then
    die
  fi
  live
}

if ensure_log_file; then
  cd ..

  echo -e "${blue}${whale}${reset} ${bold}[daemon setup]${reset}"

  wal::copy_service_template
  wal::enable_service

  echo -e "${green}${whale}${reset} daemon setup complete"

  echo -e "\nMake sure to import PATH after login. For example (at end of file):\n
${bold}bash (.profile):${reset} systemctl --user import path
${bold}fish (config.fish):${reset} status is-login; and systemctl --user import-environment PATH"
fi
