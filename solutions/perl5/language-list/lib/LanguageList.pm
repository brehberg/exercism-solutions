package LanguageList;

use v5.38;

our @Languages;

# Add a language to the @Languages array
sub add_language ($language) {
    push(@Languages, $language);    
}

# Remove a language from the @Languages array
sub remove_language () {
    pop(@Languages);
}

# Fetch the first language from the @Languages array
sub first_language () {
    return $Languages[0];
}

# Fetch the last language from the @Languages array
sub last_language () {
    return $Languages[-1];
}

# Fetch a slice of languages from the @Languages array
sub get_languages (@elements) {
    return map { $Languages[$_-1] } @elements;
}

# Check if a language exists in the @Languages array
sub has_language ($language) {
    return grep {$_ eq $language} @Languages;
}
