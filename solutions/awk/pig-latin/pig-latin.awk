{ for (i = 1; i <= NF; i++) $i = translate($i) } 
1

function translate(word, m) {
    # If a word begins with a vowel sound, add
    # an "ay" sound to the end of the word    
    if (match(word, "^([aeiou]|xr|yt).*"))
        return word "ay"

    # If a word begins with a consonant sound, move
    # it to the end of the word and then add "ay"
    if (match(word, "^(.*qu|[^aeiou]+)([aeiouy].*)", m)) 
        return m[2] m[1] "ay"

    #  are there any other cases?
    return word
}