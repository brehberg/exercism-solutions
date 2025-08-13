object Series {
    def largestProduct(span: Int, digits: String): Option[Int] = {
        if (span < 0 || span > digits.length) return None
        if (digits.exists(!_.isDigit)) return None
        if (span == 0) return Some(1)

        var largest: Int = 0       
        for (window <- digits.filter(_.isDigit).map(_.asDigit).sliding(span)) {
            largest = math.max(largest, window.product)
        }        
        return Some(largest)
    }
}