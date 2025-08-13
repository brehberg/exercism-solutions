data class Year(val year: Int) {
    val divisible_by = fun(n: Int): Boolean { return year % n == 0 }
    val isLeap: Boolean = divisible_by(4) && !divisible_by(100) || divisible_by(400)
}
