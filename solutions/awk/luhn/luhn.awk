BEGIN{
    FPAT = "[0-9]"
    gsub(/ /, "")
}

/[^0-9 ]/ {
    print "false"
    next
}

{
    sum = 0
    count = 0
    factor = 2 - (NF % 2);

    for ( i = 1; i <= NF; i++ ) { 
        digit = $i * factor;
        sum += (digit > 9) ? digit - 9 : digit;
        count += 1;
        factor = 3 - factor;
    }    

    print (count > 1 && sum % 10 == 0) ? "true" : "false"
}
