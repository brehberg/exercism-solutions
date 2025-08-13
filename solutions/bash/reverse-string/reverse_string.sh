#!/usr/bin/env bash
#
# reverse a given string
set -eo pipefail

validate_args() {
  if ! [[ $# == 1 ]]; then
    echo "Usage: ${0##*/} <input>" >&2
    return 1
  fi
}

main() {
  validate_args "$@" || exit 1

  local input="$1"
  echo "${input}" | rev
}

main "$@"
