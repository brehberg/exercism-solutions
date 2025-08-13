#!/usr/bin/env bash

plain='abcdefghijklmnopqrstuvwxyz1234567890'
cipher='zyxwvutsrqponmlkjihgfedcba1234567890'

encode() {
  # Encode a given plaintext to the corresponding ciphertext
  output=$(clean "$1" | tr $plain $cipher | fold -w5)
  echo ${output:+$output}
}

decode() {
  # Decode a given ciphertext to the corresponding plaintext
  clean "$1" | tr $cipher $plain
}

clean() {
  # remove spaces and convert string to lowercase
  echo "${1//[[:blank:][:punct:]]/}" | tr '[:upper:]' '[:lower:]'
}

if [[ $# == 2 && ($1 == 'encode' || $1 == 'decode') ]]; then
  "$@"
fi
