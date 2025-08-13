#!/usr/bin/env bash
#
# Implement basic list operations
if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
    echo "This library of functions should be sourced into another script" >&2
    exit 4
fi
bash_version=$((10 * BASH_VERSINFO[0] + BASH_VERSINFO[1]))
if ((bash_version < 43)); then
    echo "This library requires at least bash version 4.3" >&2
    return 4
fi

# Due to inherent bash limitations around word splitting and globbing,
# functions that are intended to *return a list* are instead required to
# receive a nameref parameter, the name of an array variable that will be
# populated in the list function. See filter, map and reverse functions.

# Append some elements to the given list.
list::append() {
    local -n __input
    __input=$1
    __input+=("${@:2}")
}

# Return only the list elements that pass the given function.
list::filter() {
    local -n __input
    local -n __final
    local func=$1
    __input=$2
    __final=$3
    for item in "${__input[@]}"; do
        "${func}" "${item}" && __final+=("${item}")
    done
}

# Transform the list elements, using the given function,
# into a new list.
list::map() {
    local -n __input
    local -n __final
    local func=$1
    __input=$2
    __final=$3
    for item in "${__input[@]}"; do
        __final+=("$("${func}" "${item}")")
    done
}

# Left-fold the list using the function and the initial value.
list::foldl() {
    local -n __input
    local func=$1
    local result=$2
    __input=$3
    for item in "${__input[@]}"; do
        result=$("${func}" "${result}" "${item}")
    done
    echo "${result}"
}

# Right-fold the list using the function and the initial value.
list::foldr() {
    local -n __input
    local func=$1
    local result=$2
    __input=$3
    for ((i = ${#__input[@]} - 1; i >= 0; i--)); do
        item="${__input[i]}"
        result=$("${func}" "${item}" "${result}")
    done
    echo "${result}"
}

# Return the list reversed
list::reverse() {
    local -n __input
    local -n __final
    __input=$1
    __final=$2
    for ((i = ${#__input[@]} - 1; i >= 0; i--)); do
        __final+=("${__input[i]}")
    done
}
