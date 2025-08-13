class RotationalCipher(private val shiftKey: Int) {

    fun encode(text: String): String = text.map {
        when (it) {
            in 'a'..'z' -> rotate(it, 'a'.code)
            in 'A'..'Z' -> rotate(it, 'A'.code)
            else -> it
        }
    }.joinToString("")
    
    private fun rotate(c: Char, start: Int): Char =
       (start + (c.code + shiftKey - start) % 26).toChar()
}
