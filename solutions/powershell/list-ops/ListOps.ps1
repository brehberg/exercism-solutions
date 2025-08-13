<#
.SYNOPSIS
Implement basic list operations.

.DESCRIPTION
Implement a series of basic list operations, without using existing functions:
- `append` (*given two lists, add all items in the second list to the end of the first list*);
- `concatenate` (*given a series of lists, combine all items in all lists into one flattened list*);
- `filter` (*given a predicate and a list, return the list of all items for which `predicate(item)` is True*);
- `length` (*given a list, return the total number of items within it*);
- `map` (*given a function and a list, return the list of the results of applying `function(item)` on all items*);
- `foldl` (*given a function, a list, and initial accumulator, fold (reduce) each item into the accumulator from the left*);
- `foldr` (*given a function, a list, and an initial accumulator, fold (reduce) each item into the accumulator from the right*);
- `reverse` (*given a list, return a list with all the original items, but in reversed order*).

Note, the ordering in which arguments are passed to the fold functions (`foldl`, `foldr`) is significant.

.EXAMPLE
Invoke-Append -List1 @(1, 3) -List2 @(2, 4)
Returns: @(1, 3, 2, 4)

Invoke-Reverse -List @(1, 2, 3, 4, 5)
Returns: @(5, 4, 3, 2, 1)
#>
Function Invoke-Append() {
    [CmdletBinding()]
    Param(
        [object[]]$List1,
        [object[]]$List2
    )

    return $List1 + $List2
}

Function Invoke-Concatenate() {
    [CmdletBinding()]
    Param(
        [object[]]$Lists
    )

    [object[]] $final = @()
    foreach ($list in $Lists) {
        Invoke-Append $final $list
    }
    return $final
}

Function Invoke-Filter() {
    [CmdletBinding()]
    Param(
        [object[]]$List,
        [scriptblock]$Predicate
    )

    [object[]] $final = @()
    foreach ($item in $List) {
        [bool] $valid = Invoke-Command -ScriptBlock $Predicate -ArgumentList $item
        if ($valid) {
            Invoke-Append $final $item
        }        
    }
    return $final
}

Function Get-Length() {
    [CmdletBinding()]
    Param(
        [object[]]$List
    )

    [int] $count = 0
    foreach ($_ in $List) {
        $count += 1
    }
    return $count
}

Function Invoke-Map() {
    [CmdletBinding()]
    Param(
        [object[]]$List,
        [scriptblock]$Function
    )

    [object[]] $final = @()
    foreach ($item in $List) {
        [object] $mapped = Invoke-Command -ScriptBlock $Function -ArgumentList $item
        Invoke-Append $final $mapped
    }
    return $final
}

Function Invoke-Foldl() {
    [CmdletBinding()]
    Param(
        [scriptblock]$Function,
        [object[]]$List,
        [object]$Accumulator
    )

    [object] $result = $Accumulator
    foreach ($item in $List) {
        $result = Invoke-Command -ScriptBlock $Function -ArgumentList $result, $item
    }
    return $result
}

Function Invoke-Foldr() {
    [CmdletBinding()]
    Param(
        [scriptblock]$Function,
        [object[]]$List,
        [object]$Accumulator
    )

    [object] $result = $Accumulator
    foreach ($item in Invoke-Reverse $List) {
        $result = Invoke-Command -ScriptBlock $Function -ArgumentList $result, $item
    }
    return $result
}

Function Invoke-Reverse() {
    [CmdletBinding()]
    Param(
        [object[]]$List
    )

    [int] $length = Get-Length $List
    return $List[$length..0]
}
