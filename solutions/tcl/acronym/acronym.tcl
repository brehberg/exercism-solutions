#!/usr/bin/env tclsh

proc abbreviate {phrase} {
    regsub -all {\Y[\w']+|[\s_]|\W} $phrase "" acronym
    return [string toupper $acronym]
}
