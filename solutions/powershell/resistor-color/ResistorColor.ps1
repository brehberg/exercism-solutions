Function Get-ColorCode() {
    <#
    .SYNOPSIS
    Translate a color to its corresponding color code.

    .DESCRIPTION
    Given a color, return its corresponding color code.

    .PARAMETER Color
    The color to translate to its corresponding color code.

    .EXAMPLE
    Get-ColorCode -Color "black"
    #>
    [CmdletBinding()]
    Param(
        [string]$Color
    )
    
    [string[]] $codes = Get-Colors
    return $codes.indexof($Color)
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
