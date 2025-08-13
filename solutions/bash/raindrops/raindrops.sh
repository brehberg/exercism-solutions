#!/usr/bin/env bash
set -eo pipefail

# Produce a string of raindrop sounds that corresponding to potential factors.
# The rules of raindrops are that if a given number:
#	has 3 as a factor, add 'Pling' to the result.
#	has 5 as a factor, add 'Plang' to the result.
#	has 7 as a factor, add 'Plong' to the result.
#	does not have any as a factor, result should be the digits of the number.

divisible_by() {
  local d=$1
  local n=$2
  [[ $((n % d)) == 0 ]]
}

main() {
  local number=$1

  local result
  if (divisible_by 3 "$number"); then
    result="${result}Pling"
  fi
  if (divisible_by 5 "$number"); then
    result="${result}Plang"
  fi
  if (divisible_by 7 "$number"); then
    result="${result}Plong"
  fi

  echo "${result:=$number}"
}

main "$@"
