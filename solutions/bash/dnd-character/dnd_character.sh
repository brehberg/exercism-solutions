#!/usr/bin/env bash
#
# Description of function
set -eo pipefail

validate_args() {
    if [[ $# == 1 && $1 != "generate" ]]; then
        echo "Usage: ${0##*/} [generate|modifier <N>]" >&2
        return 1
    fi
    if [[ $# == 2 && $1 != "modifier" ]]; then
        echo "Usage: ${0##*/} [generate|modifier <N>]" >&2
        return 1
    fi
}

modifier() {
    local -i score="$1"
    echo $((score / 2 - 5))
}

roll_die() {
    local -i max="$1"
    echo $((1 + SRANDOM % max))
}

roll_ability() {
    local -a rolls
    rolls=("$(roll_die 6)" "$(roll_die 6)" "$(roll_die 6)" "$(roll_die 6)")

    local -i sum=0
    local -i min=10
    for i in "${rolls[@]}"; do
        sum+=$i
        min=$((min < i ? min : i))
    done
    echo $((sum - min))
}

main() {
    validate_args "$@" || exit 1

    local command="$1"

    if [[ ${command} == "modifier" ]]; then
        "${@:1}" && exit 0 # return ability score modifier value
    fi

    local -i constitution
    constitution=$(roll_ability)
    echo "strength $(roll_ability)"
    echo "dexterity $(roll_ability)"
    echo "constitution $constitution"
    echo "intelligence $(roll_ability)"
    echo "wisdom $(roll_ability)"
    echo "charisma $(roll_ability)"
    echo "hitpoints $((10 + $(modifier $constitution)))"
}

main "$@"
