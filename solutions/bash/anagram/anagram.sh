#!/usr/bin/env bash
#
# Returns all candidates that are anagrams of, but not equal to, 'target'.
set -eo pipefail

validate_args() {
  if ! [[ $# == 2 ]]; then
    echo "Usage: ${0##*/} <target> <candidates>" >&2
    return 1
  fi
}

main() {
  validate_args "$@" || exit 1

  local target="$1"
  local candidates="$2"

  local base="${target^^}"
  local anagrams=()

  for candidate in ${candidates}; do
    (is_anagram "${candidate}") && anagrams+=("${candidate}")
  done
  echo "${anagrams[@]}"
}

is_anagram() {
  local word="${1^^}"
  [[ "${base}" != "${word}" &&
    $(sorted "${base}") == $(sorted "${word}") ]]
}

sorted() {
  echo "$1" | grep -o . | sort
}

main "$@"
