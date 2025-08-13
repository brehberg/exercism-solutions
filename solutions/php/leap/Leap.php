<?php

declare(strict_types=1);

function isLeap(int $year):bool
{
    $isDivisible = function(int $n) use ($year):bool {
        return $year % $n == 0;
    };

    return $isDivisible(4) && 
        !$isDivisible(100) || 
        $isDivisible(400);
}
