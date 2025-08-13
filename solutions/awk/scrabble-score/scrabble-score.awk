#!/usr/bin/env gawk -f

BEGIN {   score = 0 }
{ 
    word = toupper($0);
    score += 1  * gsub(/[AEIOULNRST]/, "", word)
    score += 2  * gsub(/[DG]/,         "", word)
    score += 3  * gsub(/[BCMP]/,       "", word) 
    score += 4  * gsub(/[FHVWY]/,      "", word) 
    score += 5  * gsub(/[K]/,          "", word) 
    score += 8  * gsub(/[JX]/,         "", word) 
    score += 10 * gsub(/[QZ]/,         "", word) 
}
END { print toupper($0) "," score }