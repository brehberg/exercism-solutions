Function Invoke-Sieve() {
    <#
    .SYNOPSIS
    Create a program that implements the Sieve of Eratosthenes algorithm to find prime numbers.

    .DESCRIPTION
    The function take a limit (inclusive) and use the Sieve of Eratosthenes to find all the prime numbers between 2 and the limit.
    To use the Sieve of Eratosthenes, you first create a list of all the numbers between 2 and your given number.
    Then you repeat the following steps:
    1. Find the next unmarked number in your list. This is a prime number.
    2. Mark all the multiples of that prime number as composite (not prime).

    Repeating these steps until you've gone through every number in your list.
    At the end, all the unmarked numbers are prime.

    .PARAMETER Limit
    The limit (inclusive) to find all the prime numbers.

    .EXAMPLE
    Invoke-Sieve -Limit 10
    Return: @(2, 3, 5, 7)
     #>
    [CmdletBinding()]
    Param(
        [int]$Limit
    )
    $marked = @($false) * ($Limit + 1)
    for ($n = 2; $n -le $Limit; $n += 1) {
        if ($marked[$n]) { Continue }
        $n
        for ($m = $n * $n; $m -le $Limit; $m += $n)
        { $marked[$m] = $true }
    }
}