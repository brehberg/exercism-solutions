#!/usr/bin/env bash

# date command -d option can parse a variety of formats
seconds=$(TZ="UTC" date +%s -d "$1")

# add one gigasecond (one thousand million seconds)
result=$((seconds + 1000000000))

# use the `TZ` environment variable for the timezone
TZ="UTC" printf '%(%Y-%m-%dT%H:%M:%S)T\n' "$result"
