Function Add-Gigasecond() {
    <#
    .SYNOPSIS
    Add a gigasecond to a date.

    .DESCRIPTION
    Take a moment and add a gigasecond to it.

    .PARAMETER Time
    A datetime object, to which a gigasecond will be added.

    .EXAMPLE
    Add-Gigasecond -Time
    #>
    [CmdletBinding()]
    Param(
        [DateTime]$Time
    )
    # A gigasecond is one thousand million seconds.
    [int] $gigasecond = 1000000000
    return $Time.AddSeconds($gigasecond)
}
