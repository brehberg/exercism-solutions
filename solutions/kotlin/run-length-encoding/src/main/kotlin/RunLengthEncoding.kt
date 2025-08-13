object RunLengthEncoding {

    fun encode(input: String): String {        
        if (input.isNullOrEmpty()) {
            return ""
        }

        var result = StringBuilder()        
        var prev = input[0]
        var count = 1

        for (char in input.toCharArray(1)) {
            if (char == prev) {
                count += 1
                continue
            }
            if (count > 1) {
                result.append(count)
            }
            result.append(prev)
            prev = char
            count = 1
        }        
        if (count > 1) {
            result.append(count)
        }
        result.append(prev)
        return result.toString()
    }

    fun decode(input: String): String {
        if (input.isNullOrEmpty()) {
            return ""
        }
        
        var result = StringBuilder()
        val groups = """\d*.""".toRegex()

        for (group in groups.findAll(input)) {
            val offset = group.value.length - 1
            val count = if (offset == 0) 1 else group.value.substring(0, offset).toInt()
            result.append(group.value.substring(offset).repeat(count))
        }
        return result.toString()
    }
}
