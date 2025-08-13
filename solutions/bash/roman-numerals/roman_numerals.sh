#!/usr/bin/env bash
#
# Description of function
set -eo pipefail

validate_args() {
  if ! [[ $# == 1 ]]; then
    echo "Usage: ${0##*/} <number>" >&2
    return 1
  fi
  if [[ $1 =~ [^[:digit:]] ]]; then
    echo "Usage: ${0##*/} <number>" >&2
    return 1
  fi
  if (($1 < 1 || $1 > 3999)); then
    echo "Error: invalid input <number> must be between 1 and 3999." >&2
    return 1
  fi
}

main() {
  validate_args "$@" || exit 1

  local -i number="$1"
  local roman=""

  append_literals() {
    local value="$1"
    local pattern="$2"
    while ((number >= value)); do
      number=$((number - value))
      roman+="${pattern}"
    done
  }

  append_literals 1000 "M"
  append_literals 900 "CM"
  append_literals 500 "D"
  append_literals 400 "CD"
  append_literals 100 "C"
  append_literals 90 "XC"
  append_literals 50 "L"
  append_literals 40 "XL"
  append_literals 10 "X"
  append_literals 9 "IX"
  append_literals 5 "V"
  append_literals 4 "IV"
  append_literals 1 "I"

  echo "${roman}"
}

main "$@"
