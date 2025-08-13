/// Scrabble score for letters are valued as follows:
const LETTER_VALUE: &[u64; 27] = &[
    // A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q,  R, S, T, U, V, W, X, Y, Z,
    0, 1, 3, 3, 2, 1, 4, 2, 4, 1, 8, 5, 1, 3, 1, 1, 3, 10, 1, 1, 1, 1, 4, 4, 8, 4, 10,
];

/// Compute the Scrabble score for a word.
pub fn score(word: &str) -> u64 {
    word.to_ascii_lowercase()
        .chars()
        .filter(|c| c.is_ascii_alphabetic())
        .map(|c| LETTER_VALUE[c as usize - 96])
        .sum()
}
