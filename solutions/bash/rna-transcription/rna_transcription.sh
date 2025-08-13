#!/usr/bin/env bash
#
# Given a DNA strand, return its RNA complement (per RNA transcription).
set -eo pipefail

validate_args() {
  if [[ "$1" =~ [^ACGT] ]]; then
    echo "Invalid nucleotide detected." >&2
    return 1
  fi
}

main() {
  validate_args "$@" || exit 1

  local strand="$1"

  echo "${strand}" | tr 'GCTA' 'CGAU'
}

main "$@"
