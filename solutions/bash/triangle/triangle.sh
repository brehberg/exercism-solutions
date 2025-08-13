#!/usr/bin/env bash
#
# Determine if a triangle is equilateral, isosceles, or scalene.
set -eo pipefail

validate_args() {
  if ! [[ $# == 4 ]]; then
    echo "Usage: ${0##*/} [equilateral|isosceles|scalene] <Side1> <Side2> <Side3>" >&2
    return 1
  fi
}

main() {
  validate_args "$@" || exit 1

  local type="$1"
  (valid "${@:2}") && "${type}" "${@:2}" && echo true || echo false
}

valid() {
  # All side lengths must be positive and
  # side lengths cannot violate triangle inequality.
  (($(bc <<< "$1 > 0 && $2 > 0 && $3 > 0"))) \
    && (($(bc <<< "$1 + $2 >= $3 && $2 + $3 >= $1 && $1 + $3 >= $2")))
}

equilateral() {
  # An equilateral triangle has all three sides the same length.
  (($(bc <<< "$1 == $2 && $2 == $3")))
}

isosceles() {
  # An isosceles triangle has at least two sides the same length.
  (($(bc <<< "$1 == $2 || $2 == $3 || $1 == $3")))
}

scalene() {
  # A scalene triangle has all sides of different lengths.
  (! equilateral "$@" && ! isosceles "$@")
}

main "$@"
