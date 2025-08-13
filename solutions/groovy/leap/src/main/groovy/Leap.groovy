class Leap {
    private Integer year
    
    Leap(Integer year) {
        this.year = year
    }

    def isLeapYear() {
        isDivisibleBy(4) && 
            !isDivisibleBy(100) || 
            isDivisibleBy(400)
    }

    private def isDivisibleBy(Integer n) {
        this.year % n == 0
    }
}
