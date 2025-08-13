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
    local -i digit="$((number / value))"

    case "${digit}" in
    # [digit]) Value ;;
    [123]) roman+="${unit}" ;;
    [4]) roman+="${unit}${half}" ;;
    [5678]) roman+="${half}" ;;
    [9]) roman+="${unit}${next}" ;;
    *) echo 0 ;;
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
