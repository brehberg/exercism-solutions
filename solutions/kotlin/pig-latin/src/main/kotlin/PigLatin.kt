object PigLatin {
    private val vowels = listOf("a", "e", "i", "o", "u", "xr", "yt")
    private val consonants = ('a'..'z')
            .map { it.toString() }
            .filterNot { it in vowels }
            .plus(listOf("ch", "qu", "th", "rh", "thr", "sch"))

    fun translate(input: String) = input
            .split(" ")
            .joinToString(" ") { it.toPig() }

    private fun String.toPig() = when {
        take(1) in vowels || take(2) in vowels -> this + "ay"
        drop(1).take(2) == "qu" -> drop(3) + take(3) + "ay"
        take(3) in consonants -> drop(3) + subSequence(0, 3) + "ay"
        take(2) in consonants -> drop(2) + subSequence(0, 2) + "ay"
        take(1) in consonants -> drop(1) + take(1) + "ay"
        else -> this
    }
}