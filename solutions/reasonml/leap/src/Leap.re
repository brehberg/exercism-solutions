let isLeapYear = (year) => {
    let isDivisibleBy = n => 
        year mod n == 0

    isDivisibleBy(4) && 
        !isDivisibleBy(100) || 
        isDivisibleBy(400)
}