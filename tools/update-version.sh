#!/bin/bash

# Get the latest two revisions
REV_LIST=$(git rev-list --tags --max-count=2)

# shellcheck disable=SC2086
IFS=$'\n' read -r -d '' -a TAGS < <(git describe --abbrev=0 --tags $REV_LIST)

# Remove prefix
PREV_VERSION=${TAGS[1]#v}
CUR_VERSION=${TAGS[0]#v}

FILE=$1

function wal::update_and_replace {
  local base
  base=$(basename "$FILE")

  echo "Updating ${base@Q}"

  sed -e "s/${PREV_VERSION}/${CUR_VERSION}/" "$FILE" > /tmp/"$base"-updated
  mv -f /tmp/"$base"-updated "$FILE"
}

# If file contains the old version string, replace it
if grep "$PREV_VERSION" "$FILE" >/dev/null; then
  wal::update_and_replace
fi
