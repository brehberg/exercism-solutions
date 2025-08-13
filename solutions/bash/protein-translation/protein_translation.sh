#!/usr/bin/env bash
#
# Translate RNA sequences into proteins.
set -eo pipefail

validate_args() {
  if ! [[ $# == 1 ]]; then
    echo "Usage: ${0##*/} <strand>" >&2
    return 1
  fi
}

declare -r STOP_CODON="STOP"

from_codon() {
  local codon="$1"
  case $codon in
  AUG) echo "Methionine" ;;
  UUU | UUC) echo "Phenylalanine" ;;
  UUA | UUG) echo "Leucine" ;;
  UCU | UCC | UCA | UCG) echo "Serine" ;;
  UAU | UAC) echo "Tyrosine" ;;
  UGU | UGC) echo "Cysteine" ;;
  UGG) echo "Tryptophan" ;;
  UAA | UAG | UGA) echo $STOP_CODON ;;
  esac
}

main() {
  validate_args "$@" || exit 1

  local strand="$1"

  group_chunks() {
    echo "${strand}" | fold -w3
  }

  local result=()
  for chunk in $(group_chunks); do
    local amino_acid
    amino_acid="$(from_codon "${chunk}")"

    if [[ -z $amino_acid ]]; then
      echo "Invalid codon" >&2
      return 1
    fi
    if [[ $amino_acid == "$STOP_CODON" ]]; then
      break
    fi
    result+=("${amino_acid}")
  done

  echo "${result[@]}"
}

main "$@"
