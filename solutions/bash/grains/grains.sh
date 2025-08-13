#!/usr/bin/env bash
#
# Calculate the number of grains of wheat on a chessboard
# given that the number on each square doubles.
set -eo pipefail

fail() {
  echo "Error: invalid input" >&2
  return 1
}

validate_arguments() {
  if ! [[ $# == 1 ]]; then
    echo "Usage: ${0##*/} <input>" >&2
    return 1
  fi

  if [[ $1 != "total" ]] && (($1 < 1 || $1 > 64)); then
    fail # There are 64 squares on a chessboard
  fi
}

main() {
  validate_arguments "$@" || exit 1

  if [[ "$1" == "total" ]]; then
    echo "2^64 - 1" | bc # 18446744073709551615
    exit 0
  fi

  local -i square="$1"
  echo "2^(${square} - 1)" | bc
}

main "$@"
