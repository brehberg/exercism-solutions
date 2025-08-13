#!/usr/bin/env bash
#
# An Armstrong number is a number that is the sum of its own
# digits each raised to the power of the number of digits.
set -eo pipefail

validate_arguments() {
  if ! [[ $# == 1 ]]; then
    echo "Usage: ${0##*/} <number>" >&2
    return 1
  fi
}

main() {
  validate_arguments "$@" || exit 1

  local number="$1"
  local sum=0

  for ((i = 0; i < ${#number}; ++i)); do
    local digit="${number:$i:1}"
    sum=$((sum + digit ** ${#number}))
  done

  ((sum == number)) && echo true || echo false
}

main "$@"
