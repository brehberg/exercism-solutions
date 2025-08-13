#!/usr/bin/env bash
set -eo pipefail

validate_arguments() {
  if [[ $# != 2 ]]; then
    echo "Usage: ${0##*/} <string1> <string2>"
    return 1
  fi

  local strand1="${1}" strand2="${2}"
  if [[ "${#strand1}" != "${#strand2}" ]]; then
    echo "${0##*/} error: strands must be of equal length"
    return 1
  fi
}

main() {
  validate_arguments "$@" || exit 1

  local strand1="${1}" strand2="${2}"
  local distance=0

  for ((i = 0; i < ${#strand1}; i++)); do
    local nucleotide1="${strand1:i:1}"
    local nucleotide2="${strand2:i:1}"

    if [[ "${nucleotide1}" != "${nucleotide2}" ]]; then
      distance=$((distance + 1))
    fi
  done

  echo "${distance}"
}

main "$@"
