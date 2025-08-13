class ScrabbleScore {
    static def letterValues = [
        AEIOULNRST: 1,
        DG: 2,
        BCMP: 3,
        FHVWY: 4,
        K: 5,
        JX: 8,
        QZ: 10
    ]
    static scoreWord(String word) {
        word.toList().sum(0) { c -> letterValues.find { k, v -> k.contains(c.toUpperCase()) }.value }
    }
}
