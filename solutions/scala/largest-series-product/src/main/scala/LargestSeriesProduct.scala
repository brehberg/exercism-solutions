object Series {
    def largestProduct(span: Int, digits: String): Option[Int] = {
        if (span < 0 || span > digits.length) return None
        if (digits.exists(!_.isDigit)) return None
        if (span == 0) return Some(1)
        return Some(digits.map(_.asDigit).sliding(span).map(_.product).max)
    }
}