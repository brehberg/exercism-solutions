#!/usr/bin/env bash
#
# Determine what Bob will reply to someone when they
# say something to him or ask him a question.
set -eo pipefail

main() {
  local input="$1"

  # Bob only ever answers one of five things:
  local defaultReply="Whatever."
  local silenceReply="Fine. Be that way!"
  local questionReply="Sure."
  local yellingReply="Whoa, chill out!"
  local yellingQuestionReply="Calm down, I know what I'm doing!"

  is_silence() {
    local trimmed="${1//[[:space:]]/}"
    [[ -z "${trimmed}" ]]
  }

  is_yelling() {
    local input="$1"
    local -u upper="$1" # "${1^^}"
    local -l lower="$1" # "${1,,}"
    [[ "${input}" == "${upper}" && "${input}" != "${lower}" ]]
  }

  is_question() {
    local input="$1"
    [[ "${input}" =~ \?[[:space:]]*$ ]]
  }

  if (is_silence "${input}"); then
    echo "${silenceReply}"

  elif (is_yelling "${input}"); then
    if (is_question "${input}"); then
      echo "${yellingQuestionReply}"
    else
      echo "${yellingReply}"
    fi

  elif (is_question "${input}"); then
    echo "${questionReply}"
  else
    echo "${defaultReply}"
  fi
}

main "$@"
