proc raindrops {number} {
    set Pling 3
    set Plang 5
    set Plong 7

    set drops ""
    # [info..] gives all variable names starting with P
    foreach varname [info vars P*] {
        # [set..] gives the value of variable to test for factor
        if {$number % [set $varname] == 0} {
            append drops $varname
        }
    }
    expr {$drops eq "" ? $number : $drops}
}
