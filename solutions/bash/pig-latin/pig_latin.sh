#!/usr/bin/env bash
#
# Function to translate text from English to Pig Latin.
set -eo pipefail
set -f # Disable filename expansion (globbing).

validate_args() {
    if [[ $# -lt 1 ]]; then
        echo "Usage: ${0##*/} <phrase>" >&2
        return 1
    fi
}

translate_word() {
    local word="$1"

    if [[ $word =~ ^([aeiou]|xr|yt) ]]; then
        # If a word begins with a vowel sound, add
        # an "ay" sound to the end of the word
        echo "${word}ay"
    else
        # If a word begins with a consonant sound, move
        # it to the end of the word and then add "ay"
        echo "${word}" | sed -r 's/(.*qu|[^aeiou]+)([aeiouy].*)/\2\1ay/'
    fi
}

main() {
    validate_args "$@" || exit 1

    local phrase="$*"
    local result=""

    for word in ${phrase}; do
        result="${result} $(translate_word "${word}")"
    done

    echo "${result:1}"
}

main "$@"
