#!/usr/bin/env bash
#
# Given the position of two queens on a chess board, indicate whether
#  or not they are positioned so that they can attack each other.
set -eo pipefail

validate_args() {
  if ! [[ $# == 4 && $1 == '-w' && $3 == '-b' ]]; then
    echo "Usage: ${0##*/} -w <row,col> -b <row,col>" >&2
    return 1
  fi

  if [[ "$2" == "$4" ]]; then
    fail "$2 $4 same position"
  else
    check_position "$2" && check_position "$4"
  fi
}

main() {
  validate_args "$@" || exit 1

  local white_row="${2%%,*}" # drops from first occurrence of `,` to end
  local white_col="${2#*,}"  # drops from start up to first occurrence of `,`
  local black_row="${4%,*}"  # drops from last occurrence of `,` to end
  local black_col="${4##*,}" # drops from start up to last occurrence of `,`

  if [[ "${white_row}" == "${black_row}" ||
    "${white_col}" == "${black_col}" ]]; then
    echo true
  else
    local row_diff=$((white_row - black_row))
    local col_diff=$((white_col - black_col))
    [[ "${row_diff#-}" == "${col_diff#-}" ]] && echo true || echo false
  fi

}

check_position() {
  local row="${1%,*}"
  local col="${1#*,}"
  check_range "row" "${row}" && check_range "column" "${col}"
}

check_range() {
  local label="$1"
  local value="$2"
  if ((value < 0)); then
    fail "${value} ${label} not positive"
  elif ((value > 7)); then
    fail "${value} ${label} not on board"
  fi
}

fail() {
  echo "$0 error: $1" >&2
  return 1
}

main "$@"
