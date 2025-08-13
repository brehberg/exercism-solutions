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

  divisible_by() {
    local n="$1"
    ((year % n == 0))
  }

  if (divisible_by 100); then
    (divisible_by 400) && echo true || echo false
  else
    (divisible_by 4) && echo true || echo false
  fi
}

main "$@"
