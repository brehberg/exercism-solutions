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
    local -i value="$1"
    local unit="$2"
    local half="$3"
    local next="$4"

    case $((number / value)) in
    1) roman+="${unit}" ;;
    2) roman+="${unit}${unit}" ;;
    3) roman+="${unit}${unit}${unit}" ;;
    4) roman+="${unit}${half}" ;;
    5) roman+="${half}" ;;
    6) roman+="${half}${unit}" ;;
    7) roman+="${half}${unit}${unit}" ;;
    8) roman+="${half}${unit}${unit}${unit}" ;;
    9) roman+="${unit}${next}" ;;
    esac
    number=$((number % value))
  }

  append_literals 1000 "M"
  append_literals 100 "C" "D" "M"
  append_literals 10 "X" "L" "C"
  append_literals 1 "I" "V" "X"

  echo "${roman}"
}

main "$@"
