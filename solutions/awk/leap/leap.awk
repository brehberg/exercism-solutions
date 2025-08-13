function isDivisibleBy(n) {
    return $1 % n == 0
}

{
    print (isDivisibleBy(4) \
        && !isDivisibleBy(100) \
        || isDivisibleBy(400)) ? "true" : "false"
}