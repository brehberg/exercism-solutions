#!/usr/bin/env bash
#
# Given a year, report if it is a leap year.
set -eo pipefail

validate_args() {
  if ! [[ $# == 1 ]]; then
    echo "Usage: ${0##*/} <year>" >&2
    return 1
  fi
  if [[ $1 =~ [^[:digit:]] ]]; then
    echo "Usage: ${0##*/} <year>" >&2
    return 1
  fi
}

main() {
  validate_args "$@" || exit 1

  local year="$1"

  is_divisible_by() {
    local n="$1"
    ((year % n == 0))
  }

  (is_divisible_by 4 && ! is_divisible_by 100 || is_divisible_by 400) && echo true || echo false
}

main "$@"
