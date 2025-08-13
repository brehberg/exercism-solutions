BEGIN{
    FPAT = "[0-9]"
    # Spaces are allowed in the input, but they should be stripped.
    gsub(/ /, "")
}

/[^0-9 ]/ { 
    # All other non-digit characters are disallowed.
    print "false"
    next
}

{
    sum = 0
    factor = 2 - (NF % 2);

    for ( i = 1; i <= NF; i++ ) {
        # double every second digit, starting from the right
        # if greater than 9 then subtract 9 from the product
        digit = $i * factor;
        sum += (digit > 9) ? digit - 9 : digit;
        factor = 3 - factor;
    }

    # Strings of 1 digit or less are not valid.
    print (NF > 1 && sum % 10 == 0) ? "true" : "false"
}
