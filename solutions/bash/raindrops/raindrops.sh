#!/usr/bin/env bash

divisible_by() {
  local d=$1
  local n=$2
  [[ $(("$n" % "$d")) == 0 ]] && return 0 || return 1
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
