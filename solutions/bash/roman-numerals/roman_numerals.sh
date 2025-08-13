#!/usr/bin/env bash
#
# Convert a positive integer into a string representation of that
# integer in roman numeral form.
#
# ARGS
# $1 number - The number to turn into roman numeral
#
# EXAMPLE
# bash roman_numerals.sh 1
# return: 'I'
# bash roman_numerals.sh 3999
# return: 'MMMCMXCIX'
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
  local -i value=1000
  local roman=""

  append_literals() {
    local unit="$1"
    local half="$2"
    local full="$3"

    case $((number / value)) in
    1) roman+="${unit}" ;;
    2) roman+="${unit}${unit}" ;;
    3) roman+="${unit}${unit}${unit}" ;;
    4) roman+="${unit}${half}" ;;
    5) roman+="${half}" ;;
    6) roman+="${half}${unit}" ;;
    7) roman+="${half}${unit}${unit}" ;;
    8) roman+="${half}${unit}${unit}${unit}" ;;
    9) roman+="${unit}${full}" ;;
    esac
    number=$((number % value))
    value=$((value / 10))
  }

  append_literals "M"
  append_literals "C" "D" "M"
  append_literals "X" "L" "C"
  append_literals "I" "V" "X"

  echo "${roman}"
}

main "$@"
