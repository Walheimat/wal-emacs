#!/bin/bash

source "./variables.sh"

readonly SPINNER_ANIMATION_FRAME_LENGTH=0.2

#######################################
# Show a loading spinner.
#
# Globals:
#   SPINNER_ANIMATION_FRAME_LENGTH
#######################################
function animate_loading_spinner() {
  local i sp n
  sp='/-\|'
  n=${#sp}
  printf ' '
  while sleep "$SPINNER_ANIMATION_FRAME_LENGTH"; do
    printf "%s\b" "${sp:i++%n:1}"
  done
}

#######################################
# Maybe kill the last background process.
#######################################
function maybe_kill_bg() {
  local last_bg_pid="$!"

  if [[ -n "$last_bg_pid" ]]; then
    if ps -p "$last_bg_pid" > /dev/null; then
      kill "$last_bg_pid"
    fi
  fi
}

#######################################
# Echo message with loading spinner.
#######################################
function signal() {
  local message="$*"

  echo -ne "${message} ..."

  animate_loading_spinner &
}

#######################################
# Stop spinner and echo success message.
#
# Arguments:
#   The message to echo (optional).
#######################################
function live() {
  local message="${1:-done}"
  maybe_kill_bg

  echo -e "${green}${message}${reset}"
}

#######################################
# Stop spinner, echo fail message and exit.
#
# Arguments:
#   The message to echo (optional).
#######################################
function die() {
  local message="${1:-failed}"

  maybe_kill_bg

  echo -e "${red}${message}${reset}"

  cat "$WAL_LOG"

  exit 2
}
