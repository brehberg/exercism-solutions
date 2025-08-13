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

  if is_silence; then
    echo "${silenceReply}"

  elif is_yelling; then
    if is_question; then
      echo "${yellingQuestionReply}"
    else
      echo "${yellingReply}"
    fi

  elif is_question; then
    echo "${questionReply}"
  else
    echo "${defaultReply}"
  fi
}

is_silence() {
  local trimmed="${input//[[:space:]]/}"
  [[ -z "${trimmed}" ]]
}

is_yelling() {
  local anyUpper='[A-Z]'
  [[ "${input}" =~ ${anyUpper} && "${input}" == "${input^^}" ]]
}

is_question() {
  [[ "${input}" =~ \?[[:space:]]*$ ]]
}

main "$@"
