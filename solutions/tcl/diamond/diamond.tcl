proc diamond {letter} {
    if {$letter == "A"} { return A }

    # find starting index of given letter and create middle row
    set offset [scan "A" %c]
    set start [expr {[scan $letter %c] - $offset}]
    set index [expr $start]
    set center [repeatSpaces [expr {$start * 2 - 1}]]
    set output [list "$letter$center$letter"]

    # add additional rows for previous letters until reaching "A"
    while {$index > 1} {
        set index [expr {$index - 1}]
        set letter [format %c [expr {$offset + $index}]]
        set padding [repeatSpaces [expr {$start - $index}]]
        set center [repeatSpaces [expr {$index * 2 - 1}]]
        set output [enhanceOutput "$padding$letter$center$letter$padding" $output]
    }

    # add the first and last "A" rows to the final output
    set letter [format %c [expr $offset]]
    set padding [repeatSpaces [expr $start]]
    set output [enhanceOutput "$padding$letter$padding" $output]
    return [join $output "\n"]
}

proc repeatSpaces {n} {
    return [string repeat " " $n]
}

proc enhanceOutput {newRow existing} {
    set existing [linsert $existing 0 $newRow]
    set existing [linsert $existing end $newRow]
    return $existing
}