#!/usr/bin/env bash
#
# Given a word, compute the Scrabble score for that word.
set -eo pipefail

validate_arguments() {
  if ! [[ $# == 1 ]]; then
    echo "Usage: ${0##*/} <word>" >&2
    return 1
  fi
}

main() {
  validate_arguments "$@" || exit 1

  parse_word() {
    local word="$1"
    word=$(echo "${word}" | tr '[:lower:]' '[:upper:]')
    echo "${word}" | grep -o .
  }

  score_letter() {
    local letter="$1"

    case "${letter}" in
      # Letter)                              Value ;;
      A | E | I | O | U | L | N | R | S | T) echo 1 ;;
      D | G) echo 2 ;;
      B | C | M | P) echo 3 ;;
      F | H | V | W | Y) echo 4 ;;
      K) echo 5 ;;
      J | X) echo 8 ;;
      Q | Z) echo 10 ;;
      *) echo 0 ;;
    esac
  }

  local -i score=0
  for letter in $(parse_word "$1"); do
    score+=$(score_letter "${letter}")
  done

  echo "${score}"
}

main "$@"
