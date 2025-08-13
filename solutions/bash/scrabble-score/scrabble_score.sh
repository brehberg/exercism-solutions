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
    local word="${1^^}"        # uppercase input
    echo "${word}" | grep -o . # split letters
  }

  score_letter() {
    local letter="$1"

    case "${letter}" in
      # Letter)     Value ;;
      [AEIOULNRST]) echo 1 ;;
      [DG]) echo 2 ;;
      [BCMP]) echo 3 ;;
      [FHVWY]) echo 4 ;;
      [K]) echo 5 ;;
      [JX]) echo 8 ;;
      [QZ]) echo 10 ;;
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
