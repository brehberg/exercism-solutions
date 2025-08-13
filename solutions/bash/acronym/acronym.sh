#!/usr/bin/env bash
#
# Convert a phrase to its acronym.
set -eo pipefail

validate_arguments() {
  if ! [[ $# == 1 ]]; then
    echo "Usage: ${0##*/} <phrase>" >&2
    return 1
  fi
}

main() {
  validate_arguments "$@" || exit 1

  local phrase="$1"
  local acronym=""

  phrase=${phrase//[-]/ }        # hyphens are word separators
  phrase=${phrase//[[:punct:]]/} # other punctuation are removed

  for word in ${phrase}; do
    acronym="${acronym}${word:0:1}"
  done

  echo "${acronym}" | tr '[:lower:]' '[:upper:]'
}

main "$@"
