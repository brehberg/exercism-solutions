#!/usr/bin/env bash
set -eo pipefail

validate_arguments() {
  if ! [[ $# == 2 && ($1 == 'encode' || $1 == 'decode') ]]; then
    echo "Usage: ${0##*/} {encode|decode} <text>" >&2
    return 1
  fi
}

main() {
  validate_arguments "$@" || exit 1

  local action=$1
  local input=$2

  local plain='abcdefghijklmnopqrstuvwxyz1234567890'
  local cipher='zyxwvutsrqponmlkjihgfedcba1234567890'

  clean() {
    # remove spaces and convert string to lowercase
    echo "${1//[[:blank:][:punct:]]/}" | tr '[:upper:]' '[:lower:]'
  }

  encode() {
    local text=$1
    # Encode a given plaintext to the corresponding ciphertext
    clean "$text" | tr "$plain" "$cipher" | fold -w5
  }

  decode() {
    local text=$1
    # Decode a given ciphertext to the corresponding plaintext
    clean "$text" | tr "$cipher" "$plain"
  }

  local output
  output=$("$action" "$input")
  echo ${output:+$output}
}

main "$@"
