#!/usr/bin/env bash
#
# Clean up user-entered phone numbers so that they can be sent SMS messages.
set -eo pipefail

validate_args() {
    if ! [[ $# == 1 ]]; then
        echo "Usage: ${0##*/} <input>" >&2
        return 1
    fi
}

main() {
    validate_args "$@" || exit 1

    local input="$1"
    # extract all digits from the given string
    local clean="${input//[^[:digit:]]/}"

    # all NANP-numbers share the same country code
    if [[ ${#clean} == 11 && ${clean:0:1} == 1 ]]; then
        clean="${clean:1}"
    fi

    # area and exchange codes only digits from 2 through 9
    if [[ ${#clean} != 10 ||
        ${clean:0:1} =~ [01] ||
        ${clean:3:4} =~ [01] ]]; then
        echo "Invalid number.  [1]NXX-NXX-XXXX N=2-9, X=0-9" >&2
        return 1
    fi

    echo "${clean}"
}

main "$@"
