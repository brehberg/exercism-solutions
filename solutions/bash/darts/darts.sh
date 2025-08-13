#!/usr/bin/env bash

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

# Bash Calculator sqrt() to compute the distance from center
x=$1
y=$2
dist=$(echo "scale=4; sqrt($x*$x+$y*$y)" | bc)

# use bc again to compare the distance with different rings
if [[ $(bc <<< "$dist <= 1") -eq 1 ]]; then
  echo 10 # inner circle
elif [[ $(bc <<< "$dist <= 5") -eq 1 ]]; then
  echo 5 # middle circle
elif [[ $(bc <<< "$dist <= 10") -eq 1 ]]; then
  echo 1 # outer circle
else
  echo 0 # missed target
fi
