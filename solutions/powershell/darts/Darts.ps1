Function Invoke-Darts() {
    <#
    .SYNOPSIS
    Calculate the earned points in a single toss of a Darts game.

    .DESCRIPTION
    Take a coordinate of a point and calculate the distance from the center of the dartboard.
    Then depending on the distance and which concentric circle the point lies in, return the
    number of points earned.

    .PARAMETER X
    The X coordinate of the dart.

    .PARAMETER Y
    The Y coordinate of the dart.

    .EXAMPLE
    Invoke-Darts -X 0 -Y 10
    #>
    [CmdletBinding()]
    Param(
        [Double]$X,
        [Double]$Y
    )    
    [int] $points = 0
    [double] $dist = [Math]::Sqrt($X * $X + $Y * $Y)
    switch ($dist) {
        { $_ -le 1 } { $points = 10; Break }
        { $_ -le 5 } { $points = 5; Break }
        { $_ -le 10 } { $points = 1; Break }
        Default { $points = 0 }
    }
    return $points
}
