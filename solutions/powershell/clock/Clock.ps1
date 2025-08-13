<#
.SYNOPSIS
    Implement a clock that handles times without dates.

.DESCRIPTION
    Implement a clock that handles times without dates in 24 hours format.
    You should be able to add and subtract minutes to it.
    Two clocks that represent the same time should be equal to each other.
    Note: Please try to implement the class and its method instead of using built-in module Datetime.

.EXAMPLE
    $clock1 = [Clock]::new(5,0)
    $clock1.ToString()
    Return: "05:00"

    $clock2 = [Clock]::new(6,-120)
    $clock2.Add(60).ToString()
    Return: "05:00"

    $clock1 -eq $clock2
    Return: $true
#>

class Clock {
    [int]$minutes = 0
    [int]hidden $perHour = 60
    [int]hidden $perDay = 1440

    Clock([int]$hours, [int]$mins) {
        $this.Add($hours * $this.perHour + $mins)
    }

    [Clock] Add([int]$mins) {                        
        $this.minutes = (($this.minutes + $mins) % $this.perDay + $this.perDay) % $this.perDay
        return $this
    }

    [string] ToString() {
        $Hours = [Math]::Floor($this.minutes / $this.perHour)
        return ("{0:D2}:{1:D2}" -f [int]$Hours, ($this.minutes % $this.perHour))
    }

    [bool] Equals($other) {
        return ($this.minutes -eq $other.minutes)
    }
}