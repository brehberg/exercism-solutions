object Pangram {

    fun isPangram(input: String): Boolean =
        ('a'..'z').all {input.contains(it, ignoreCase = true)}

}
