/// Determine whether a candidate is an isogram.
const LOWERCASE_A: u8 = 97;
const UPPERCASE_A: u8 = 65;

pub fn check(candidate: &str) -> bool {
    let mut letter_flags = 0;

    for letter in candidate.chars() {
        let mut letter_index: isize = -1;

        if letter.is_ascii_lowercase() {
            letter_index = 1 << (letter as u8 - LOWERCASE_A);
        } else if letter.is_ascii_uppercase() {
            letter_index = 1 << (letter as u8 - UPPERCASE_A);
        }

        if letter_index == -1 {
            continue;
        }
        if letter_flags & letter_index > 0 {
            return false;
        }
        letter_flags |= letter_index
    }
    true
}
