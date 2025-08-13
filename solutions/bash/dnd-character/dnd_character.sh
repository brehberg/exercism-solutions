#!/usr/bin/env bash
#
# Random character generator for a game of Dungeons & Dragons
#
# usage: dnd_character.sh modifier N
# -> output expected ability score modifier
#
# usage: dnd_character.sh generate
# -> output each characteristic and ability value, one per line

set -eo pipefail

validate_args() {
    if [[ $# == 1 && $1 != generate ]]; then
        echo "Usage: ${0##*/} [generate|modifier <N>]" >&2
        return 1
    fi
    if [[ $# == 2 && $1 != modifier ]]; then
        echo "Usage: ${0##*/} [generate|modifier <N>]" >&2
        return 1
    fi
    if [[ $# == 2 && $2 =~ [^[:digit:]] ]]; then
        echo "Usage: ${0##*/} [generate|modifier <N>]" >&2
        return 1
    fi
}

readonly abilities=(
    strength
    dexterity
    constitution
    intelligence
    wisdom
    charisma
)

modifier() {
    local -i score="$1"
    echo $((score / 2 - 5))
}

roll_die() {
    local -i max="$1"
    echo $((1 + SRANDOM % max))
}

roll_ability() {
    local -i roll sum=0 min=100
    for _ in {1..4}; do
        roll=$(roll_die 6)
        sum+=$roll
        min=$((min < roll ? min : roll))
    done
    echo $((sum - min))
}

main() {
    validate_args "$@" || exit 1

    local command="$1"

    if [[ ${command} == modifier ]]; then
        "${@:1}" && exit 0 # return ability score modifier
    fi

    local -i score con
    for ability in "${abilities[@]}"; do
        score=$(roll_ability)
        echo "${ability}" "${score}"
        [[ $ability == constitution ]] && con="${score}"
    done
    echo "hitpoints $((10 + $(modifier $con)))"
}

main "$@"
