#!/usr/bin/env bash
#
# Determine the actions of a secret handshake based on the binary
# representation of the given `code`.
set -eo pipefail

validate_args() {
    if [[ $# != 1 || $1 =~ [^[:digit:]] ]]; then
        echo "Usage: ${0##*/} <code>" >&2
        return 1
    fi
}

main() {
    validate_args "$@" || exit 1

    local code="$1"
    local -a handshake

    reverse_handshake() {
        local -i length=${#handshake[@]}

        for ((i = 0; i < length / 2; ++i)); do
            local temp="${handshake[i]}"
            handshake[i]="${handshake[length - i - 1]}"
            handshake[length - i - 1]="${temp}"
        done
    }

    secret() {
        local -i mask=$1
        local action=$2

        if ((code & mask)); then
            if [[ $action == reverse ]]; then
                reverse_handshake
            else
                handshake+=("${action}")
            fi
        fi
    }

    secret 0x01 "wink"
    secret 0x02 "double blink"
    secret 0x04 "close your eyes"
    secret 0x08 "jump"
    secret 0x10 "reverse"

    echo "$(
        IFS=,
        echo "${handshake[*]}"
    )"
}

main "$@"
