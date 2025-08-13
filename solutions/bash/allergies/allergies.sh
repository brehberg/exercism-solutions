#!/usr/bin/env bash
#
# Given a person's allergy score, determine whether or not they're
# allergic to a given item, and their full list of allergies.
set -eo pipefail

validate_args() {
    fail() {
        echo "$0 error: $1" >&2
        return 1
    }

    if ! [[ $# == 2 || $# == 3 ]]; then
        echo "Usage: ${0##*/} <score> [list|allergic_to <allergen>]" >&2
        return 1
    fi
    if [[ $2 == "allergic_to" && $# != 3 ]]; then
        echo "Usage: ${0##*/} <score> allergic_to <allergen>" >&2
        return 1
    fi
    if [[ $2 == "list" && $# != 2 ]]; then
        echo "Usage: ${0##*/} <score> list" >&2
        return 1
    fi

    if [[ $1 =~ [^[:digit:]] ]]; then
        fail "score $1 is not an integer"
    fi
}

main() {
    validate_args "$@" || exit 1

    local score="$1"
    local function="$2"
    local allergen="$3"

    local allergens=(
        eggs peanuts shellfish strawberries
        tomatoes chocolate pollen cats)

    list() {
        local -a allergies
        local value=1
        for item in "${allergens[@]}"; do
            if ((score & value)); then
                allergies+=("${item}")
            fi
            value=$((value << 1))
        done
        echo "${allergies[@]}"
    }

    allergic_to() {
        local regex="\<${allergen}\>"
        [[ $(list) =~ ${regex} ]] && echo true || echo false
    }

    "${function}"
}

main "$@"
