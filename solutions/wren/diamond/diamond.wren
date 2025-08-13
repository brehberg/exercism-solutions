class Diamond {
  static rows(letter) {
    if (letter == "A") return ["A"]
    
    var output = []
    var enhanceOutput = Fn.new { |newRow| newRow + output + newRow }
    var repeatSpaces = Fn.new { |n| " " * n }

    // find starting index of given letter and create middle row
    var offset = "A".codePoints[0]
    var start = letter.codePoints[0] - offset
    var index = start
    var padding = ""
    var center = repeatSpaces.call(start * 2 - 1)
    output = [letter + center + letter]

    // add additional rows for previous letters until reaching "A"
    while (index > 1) {
      index = index - 1
      letter = String.fromCodePoint(offset + index)
      padding = repeatSpaces.call(start - index)
      center = repeatSpaces.call(index * 2 - 1)
      output = enhanceOutput.call([padding + letter + center + letter + padding])
    }

    // add the first and last "A" rows to the final output
    padding = repeatSpaces.call(start)
    return enhanceOutput.call([padding + "A" + padding])
  }
}