/// Determine whether a sentence is a pangram.
const LOWERCASE_A: u8 = 97;
const UPPERCASE_A: u8 = 65;
const ALL_LETTERS: u32 = 0b11111111111111111111111111;

pub fn is_pangram(sentence: &str) -> bool {
    let mut letter_flags = 0;

    for letter in sentence.chars() {
        if letter.is_ascii_lowercase() {
            letter_flags |= 1 << (letter as u8 - LOWERCASE_A);
        } else if letter.is_ascii_uppercase() {
            letter_flags |= 1 << (letter as u8 - UPPERCASE_A);
        }
    }
    letter_flags == ALL_LETTERS
}
