Function Get-ResistorLabel() {
    <#
    .SYNOPSIS
    Implement a function to get the label of a resistor with three color-coded bands.

    .DESCRIPTION
    Given an array of colors from a resistor, decode their resistance values and return a string represent the resistor's label.

    .PARAMETER Colors
    The array repesent the 3 colors from left to right.

    .EXAMPLE
    Get-ResistorLabel -Colors @("red", "white", "blue")
    Return: "29 megaohms"
     #>
    [CmdletBinding()]
    Param(
        [string[]]$Colors
    )
    [string[]] $codes = Get-Colors
    
    [int64] $value = 10 * $codes.IndexOf($Colors[0]) 
    $value += $codes.IndexOf($Colors[1])
    $value *= [Math]::Pow(10, $codes.IndexOf($Colors[2]))
    
    [string] $prefix = ""
    switch -Regex ($value) {
        '0{9}$' { $prefix = "giga"; $value /= 1E9; Break }
        '0{6}$' { $prefix = "mega"; $value /= 1E6; Break }
        '0{3}$' { $prefix = "kilo"; $value /= 1E3; Break }
    }
    return ("{0} {1}ohms" -f $value, $prefix)    
}

Function Get-Colors() {
    <#
    .SYNOPSIS
    Return the list of all colors.

    .DESCRIPTION
    Return the list of all colors.

    .EXAMPLE
    Get-Colors
    #>

    # Better Be Right Or Your Great Big Values Go Wrong.
    return @("black", "brown", "red", "orange", "yellow", 
        "green", "blue", "violet", "grey", "white")
}
