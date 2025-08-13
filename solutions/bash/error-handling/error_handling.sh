#!/usr/bin/env bash

validate_arguments() {
  if ! [[ $# == 1 ]]; then
    echo "Usage: ${0##*/} <person>"
    exit 1
  fi
}

main() {
  validate_arguments "$@"

  local name="$1"
  echo "Hello, $name"
}

main "$@"
