class AllYourBase {
    private value = 0

    AllYourBase(inputBase, digits) {
        // convert sequence of digits in input base to whole integer value
        if (inputBase < 2) 
            throw new ArithmeticException("invalid input base")

        digits.each { digit -> 
            if (digit < 0 || digit >= inputBase)
                throw new ArithmeticException("digit out of range")
            value *= inputBase
            value += digit
        }
    }

    def rebase(outputBase) {
        // convert whole integer value to sequence of digits in output base
        if (outputBase < 2) 
            throw new ArithmeticException("invalid output base")

        def digits = []
        while (value >= outputBase) {
            digits.push(value % outputBase)
            value = value.intdiv(outputBase)
        }
        digits.push(value)
        digits
    }
}