BEGIN {
    FPAT = "[[:alpha:]]"    
}

{
    noRepeats = "true"
    delete letters
    
    for ( i = 1; i <= NF; i++ ) { 
        if(letters[tolower($i)]++) {
            noRepeats = "false"
            break
        }
    }
    print noRepeats
}
