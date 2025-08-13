Function Get-ResistorLabel() {
    <#
    .SYNOPSIS
    Implement a function to get the label of a resistor from its color-coded bands.

    .DESCRIPTION
    Given an array of 1, 4 or 5 colors from a resistor, decode their resistance values and return a string represent the resistor's label.

    .PARAMETER Colors
    The array represent the colors from left to right.

    .EXAMPLE
    Get-ResistorLabel -Colors @("red", "black", "green", "red")
    Return: "2 megaohms ±2%"

    Get-ResistorLabel -Colors @("blue", "blue", "blue", "blue", "blue")
    Return: "666 megaohms ±0.25%"
     #>
    [CmdletBinding()]
    Param(
        [string[]]$Colors
    )
    [string[]] $codes = Get-Colors

    if ($Colors.Length -eq 1) {
        return ("{0} ohms" -f $codes.IndexOf($Colors[0]))
    }

    [decimal] $value = 0
    if ($Colors.Length -eq 5) {
        $first, $Colors = $Colors
        $value = 100 * $codes.IndexOf($first)
    }
    $value += 10 * $codes.IndexOf($Colors[0])
    $value += $codes.IndexOf($Colors[1])
    $value *= [Math]::Pow(10, $codes.IndexOf($Colors[2]))
    
    [string] $prefix = ""
    switch ($value) {
        { $_ -gt 1E9 } { $prefix = "giga"; $value /= 1E9; Break }
        { $_ -gt 1E6 } { $prefix = "mega"; $value /= 1E6; Break }
        { $_ -gt 1E3 } { $prefix = "kilo"; $value /= 1E3; Break }
    }

    [decimal] $tolerance = 0
    switch ($Colors[3]) {
        'grey' { $tolerance = 0.05; Break }
        'violet' { $tolerance = 0.1; Break }
        'blue' { $tolerance = 0.25; Break }
        'green' { $tolerance = 0.5; Break }
        'brown' { $tolerance = 1; Break }
        'red' { $tolerance = 2; Break }
        'gold' { $tolerance = 5; Break }
        'silver' { $tolerance = 10; Break }
    }    
    return ("{0} {1}ohms ±{2}%" -f $value, $prefix, $tolerance)
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
