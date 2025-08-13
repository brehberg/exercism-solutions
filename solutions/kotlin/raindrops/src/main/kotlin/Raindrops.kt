object Raindrops {
    fun convert(n: Int): String = buildString {
        if (n % 3 == 0) append("Pling")
        if (n % 5 == 0) append("Plang")
        if (n % 7 == 0) append("Plong")
        if (isEmpty()) append(n)
    }
}
