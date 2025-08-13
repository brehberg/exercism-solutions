#!/usr/bin/env bash
#
# Implement a binary search algorithm.
set -eo pipefail

validate_args() {
    if [[ $# -lt 1 ]]; then
        echo "Usage: ${0##*/} <value> <array>" >&2
        return 1
    fi
    if [[ $1 =~ [^[:digit:]] ]]; then
        echo "Usage: ${0##*/} <value> <array>" >&2
        return 1
    fi
}

main() {
    validate_args "$@" || exit 1

    local -i value="$1"
    local -a array=("${@:2}")

    local -i low=0 high=$((${#array[@]} - 1))

    while ((low <= high)); do
        local -i mid=$((low + (high - low) / 2))
        local -i number=${array[$mid]}

        if ((number < value)); then
            low=$((mid + 1))
        elif ((number > value)); then
            high=$((mid - 1))
        else
            echo ${mid}
            return 0
        fi
    done

    echo -1 # Value was not found
}

main "$@"
