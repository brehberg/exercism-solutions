class LargestSeriesProduct {
    static largestProduct(digits, span) {
        if (span < 0) throw new IllegalArgumentException("span must not be negative")
        if (span > digits.length()) throw new IllegalArgumentException("span must be smaller than string length")
        if (span == 0) return 1
        if (!digits.isNumber()) throw new IllegalArgumentException("digits input must only contain digits")        
        
        return digits
            .collect{ it as Integer }
            .collate(span, 1, false)
            .collect{ it.inject(1) { prod, n -> prod * n } }
            .max()
    }
}
