#!/usr/bin/env bash

# remove all white space from input string
id=${1//[[:blank:]]/}

# strings of length 1 or less are not valid
if [[ ${#id} -le 1 ]]; then
  echo false
  exit
fi

sum=0
for ((i = 1; i <= ${#id}; i++)); do
  # extract each character starting from the right (1=last)
  n=${id:$((${#id} - i)):1}

  # all non-digit characters are disallowed
  if [[ $n =~ [^[:digit:]] ]]; then
    echo false
    exit
  fi

  # double every second digit, starting from the right (i is even)
  # if the doubling result would be greater than 9 then subtract 9
  if [[ $((i % 2)) -ne 0 ]]; then
    sum=$((sum + "$n"))
  elif [[ $n -lt 5 ]]; then
    sum=$((sum + "$n" * 2))
  else
    sum=$((sum + "$n" * 2 - 9))
  fi
done

# if the sum is evenly divisible by 10, then the number is valid
if [[ $((sum % 10)) -eq 0 ]]; then
  echo true
else
  echo false
fi
