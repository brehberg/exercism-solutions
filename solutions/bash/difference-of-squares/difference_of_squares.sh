#!/usr/bin/env bash
#
# Find the difference between the square of the sum and
# the sum of the squares of the first N natural numbers.
set -eo pipefail

validate_args() {
  if ! [[ $# == 2 ]]; then
    echo "Usage: ${0##*/} [square_of_sum|sum_of_squares|difference] <N>" >&2
    return 1
  fi
}

main() {
  validate_args "$@" || exit 1

  local function="$1"
  local num="$2"

  "${function}" "${num}"
}

# Calculate square of sum from 1 to a given end number.
square_of_sum() {
  local n="$1"
  echo $((((n + n * n) / 2) ** 2))
}

# Calculate sum of squares from 1 to a given end number.
sum_of_squares() {
  local n="$1"
  echo $(((n + 3 * n ** 2 + 2 * n ** 3) / 6))
}

# Return difference between the two sums for a given number.
difference() {
  local n="$1"
  echo $(("$(square_of_sum "$n")" - "$(sum_of_squares "$n")"))
}

main "$@"
