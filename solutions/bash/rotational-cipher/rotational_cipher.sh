#!/usr/bin/env bash
#
# Implementats the rotational cipher, also sometimes called the Caesar cipher.
set -eo pipefail

validate_args() {
    if [[ $# != 2 || $2 =~ [^[:digit:]] ]]; then
        echo "Usage: ${0##*/} <plaintext> <shiftkey>" >&2
        return 1
    fi
}

main() {
    validate_args "$@" || exit 1

    local plaintext="$1"
    local -i shiftkey="$2"

    rotate_char() {
        local -i start in out
        start=$(printf "%d" "'$1")
        in=$(printf "%d" "'$2")
        out=$((start + ((in + shiftkey - start) % 26)))
        printf %b "\x$(printf %x ${out})"
    }

    local ciphertext=""
    for ((i = 0; i < ${#plaintext}; ++i)); do
        local char=${plaintext:$i:1}

        if [[ $char =~ [A-Z] ]]; then
            ciphertext="${ciphertext}$(rotate_char 'A' "${char}")"
        elif [[ $char =~ [a-z] ]]; then
            ciphertext="${ciphertext}$(rotate_char 'a' "${char}")"
        else
            ciphertext="${ciphertext}${char}"
        fi
    done

    echo "${ciphertext}"
}

main "$@"
