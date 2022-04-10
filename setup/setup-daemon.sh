#!/bin/bash
#
# Copy service template and env template, then enable the service.

# shellcheck disable=SC2119

source "./variables.sh"
source "./common.sh"

DIR="${0%/*}"

SERVICE_TARGET_PATH="$HOME/.config/systemd/user"
ENV_TARGET_PATH="$HOME/.config/environment.d"

DEFAULT_EMACS_PATH="/usr/local/bin/emacs"

function wal::ensure() {
  if [[ ! -d "$SERVICE_TARGET_PATH" ]]; then
    signal "Creating path ${SERVICE_TARGET_PATH@Q}"
    if ! mkdir -p "$SERVICE_TARGET_PATH" &> "$WAL_LOG"; then
      die
    fi
    live
  fi

  if [[ ! -d "$ENV_TARGET_PATH" ]]; then
    signal "Creating path ${ENV_TARGET_PATH@Q}"
    if ! mkdir -p "$ENV_TARGET_PATH" &> "$WAL_LOG"; then
      die
    fi
    live
  fi

  if [[ -e "$SERVICE_TARGET_PATH/emacs.service" ]]; then
    echo -e "Found existing service configuration, won't override" >"$WAL_LOG"
    die
  fi

  if [[ -e "$ENV_TARGET_PATH/emacs-exec-test.conf" ]]; then
    echo -e "Found existing environment configuration, won't override" >"$WAL_LOG"
    die
  fi
}

function wal::copy_service_template() {
  local emacs_path
  emacs_path=$(which emacs)

  signal "Copying ${bold}service template${reset} using ${emacs_path@Q}"

  if ! sed "s_${DEFAULT_EMACS_PATH}_${emacs_path}_" "$DIR/templates/emacs.service" > "$SERVICE_TARGET_PATH/emacs.service"; then
    die
  fi
  live
}

function wal::copy_env_template() {
  signal "Copying ${bold}environment configuration template${reset}"

  if ! cp "$DIR/templates/emacs-exec.conf" "$ENV_TARGET_PATH/emacs-exec-test.conf"; then
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

function wal::describe_steps() {
  echo -e "\nYou'll need to make sure the service has all desired paths in PATH:

There are two ways to make this work.

One is to import PATH in your login shell:
${bold}bash:${reset} systemctl --user import-environment PATH
${bold}fish:${reset} status is-login; and systemctl --user import-environment PATH

The other is to set KEY=VALUE pairs in a configuration file that was created for you.
Check out 'emacs-exec.conf' at ${ENV_TARGET_PATH@Q} and edit."
}

if ensure_log_file; then
  cd ..

  echo -e "${blue}${whale}${reset} ${bold}[daemon setup]${reset}"

  wal::ensure
  wal::copy_service_template
  wal::copy_env_template
  wal::enable_service

  echo -e "${green}${whale}${reset} daemon setup complete"

  wal::describe_steps
fi
