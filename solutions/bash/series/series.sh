#!/usr/bin/env bash
#
# Given a string of digits, output all the contiguous substrings of
# length n in that string in the order that they appear.
set -eo pipefail

validate_arguments() {
  if ! [[ $# == 2 ]]; then
    echo "Usage: ${0##*/} <digits> <n>" >&2
    return 1
  elif [ -z "$1" ]; then
    echo "$0 error: digit series cannot be empty" >&2
    return 1
  fi

  # use regex to check for integers with optional minus sign
  local reNumber='^-?[[:digit:]]+$'
  if ! [[ $1 =~ $reNumber ]]; then
    echo "$0 error: digit series $1 is not a number" >&2
    return 1
  fi
  if ! [[ $2 =~ $reNumber ]]; then
    echo "$0 error: slice length $2 is not a number" >&2
    return 1
  fi

  if (($2 == 0)); then
    echo "$0 error: slice length cannot be zero" >&2
    return 1
  elif (($2 < 0)); then
    echo "$0 error: slice length cannot be negative" >&2
    return 1
  elif ((${#1} < $2)); then
    echo "$0 error: slice length cannot be greater than series length" >&2
    return 1
  fi
}

main() {
  validate_arguments "$@" || exit 1

  local -i digits=$1
  local -i n=$2

  local series=()

  for ((start = 0; start <= (${#1} - n); start++)); do
    local substring=""
    for ((i = 0; i < n; i++)); do
      substring="${substring}${digits:(start + i):1}"
    done
    series+=("${substring}")
  done
  echo "${series[@]}"
}

main "$@"
