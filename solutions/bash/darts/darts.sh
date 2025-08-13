#!/usr/bin/env bash

distance() {
  # use Bash Calculator sqrt() to compute the distance from center
  echo "scale=4; sqrt($1*$1+$2*$2)" | bc
}

less_than() {
  # use bc again to check if the distance is less than given number
  [[ $(bc <<< "$2 <= $1") -eq 1 ]] && exit 0 || exit 1
}

# validate command line arguments given as two numbers
if [[ $# -lt 2 ]]; then
  echo "$0 error: not enough arguments"
  exit 2
fi

re='^-?[0-9]+([.][0-9]+)?$' # regex to handle numbers with minus sign
if ! [[ $1 =~ $re ]]; then
  echo "$0 error: $1 is not a number"
  exit 2
fi
if ! [[ $2 =~ $re ]]; then
  echo "$0 error: $2 is not a number"
  exit 2
fi

d=$(distance "$@")

if (less_than 1 "$d"); then
  echo 10 # inner circle
elif (less_than 5 "$d"); then
  echo 5 # middle circle
elif (less_than 10 "$d"); then
  echo 1 # outer circle
else
  echo 0 # missed target
fi
