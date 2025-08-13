#!/usr/bin/env bash
set -eo pipefail

main() {
  # remove all white space from input string
  local id="${1//[[:blank:]]/}"

  # strings of length 1 or less are not valid
  if ((${#id} <= 1)); then
    echo false
    exit
  fi

  local sum=0
  for ((i = 1; i <= ${#id}; i++)); do
    # extract each character starting from the right (1=last)
    local n="${id:$((${#id} - i)):1}"

    # all non-digit characters are disallowed
    if [[ "$n" =~ [^[:digit:]] ]]; then
      echo false
      exit
    fi

    # double every second digit, starting from the right (if i is even)
    # if the doubling result would be greater than 9 then subtract 9
    if ((i % 2 != 0)); then
      sum=$((sum + n))
    elif ((n < 5)); then
      sum=$((sum + n * 2))
    else
      sum=$((sum + n * 2 - 9))
    fi
  done

  # if the sum is evenly divisible by 10, then the number is valid
  ((sum % 10 == 0)) && echo true || echo false
}

main "$@"
