#!/usr/bin/env bash
#
# Calculate a resistance value from two colors
set -eo pipefail

main() {
    # Better Be Right Or Your Great Big Values Go Wrong
    local -A colors
    colors=(
        [black]=0 [brown]=1 [red]=2 [orange]=3 [yellow]=4
        [green]=5 [blue]=6 [violet]=7 [grey]=8 [white]=9
    )

    local tens_value="${colors["$1"]}"
    if [[ -z $tens_value ]]; then
        echo "Error: invalid color $1" >&2
        exit 1
    fi

    local ones_value="${colors["$2"]}"
    if [[ -z $ones_value ]]; then
        echo "Error: invalid color $2" >&2
        exit 1
    fi

    echo $((tens_value * 10 + ones_value))
}

main "$@"
