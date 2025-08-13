#!/usr/bin/env bash
set -eo pipefail

validate_arguments() {
  # validate command line arguments given as two numbers
  if [[ $# -lt 2 ]]; then
    echo "$0 error: not enough arguments" >&2
    return 1
  fi

  # use regex to check for numbers with optional minus sign
  local reNumber='^-?[0-9]+([.][0-9]+)?$'
  if ! [[ $1 =~ $reNumber ]]; then
    echo "$0 error: $1 is not a number" >&2
    return 1
  fi
  if ! [[ $2 =~ $reNumber ]]; then
    echo "$0 error: $2 is not a number" >&2
    return 1
  fi
}

distance() {
  # use Bash Calculator sqrt() to compute the distance from center
  local x=$1
  local y=$2
  bc <<< "scale=4; sqrt($x*$x+$y*$y)"
}

less_than() {
  # use bc again to check if the distance is less than given number
  local num=$1
  local dist=$2
  [[ $(bc <<< "$dist <= $num") -eq 1 ]]
}

main() {
  validate_arguments "$@" || exit 1

  local d
  d=$(distance "$@")

  local points
  if (less_than 1 "$d"); then
    points=10 # inner circle
  elif (less_than 5 "$d"); then
    points=5 # middle circle
  elif (less_than 10 "$d"); then
    points=1 # outer circle
  else
    points=0 # missed target
  fi

  echo "$points"
}

main "$@"
