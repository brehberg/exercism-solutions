#!/usr/bin/env bash
#
# Given a string of digits, output all the contiguous substrings of
# length n in that string in the order that they appear.
set -eo pipefail

fail() {
  echo "$0 error: $1" >&2
  return 1
}

validate_arguments() {
  if ! [[ $# == 2 ]]; then
    echo "Usage: ${0##*/} <digits> <n>" >&2
    return 1
  elif [ -z "$1" ]; then
    fail "digit series cannot be empty"
  fi

  if (($2 == 0)); then
    fail "slice length cannot be zero"
  elif (($2 < 0)); then
    fail "slice length cannot be negative"
  elif (($2 > ${#1})); then
    fail "slice length cannot be greater than series length"
  fi
}

main() {
  validate_arguments "$@" || exit 1

  local -i digits=$1
  local -i n=$2

  local series=()
  for ((start = 0; start <= ${#digits} - n; start++)); do
    series+=("${digits:start:n}")
  done
  echo "${series[@]}"
}

main "$@"
