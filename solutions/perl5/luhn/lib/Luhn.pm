package Luhn;

use v5.38;

use Exporter qw<import>;
our @EXPORT_OK = qw<is_luhn_valid>;

sub is_luhn_valid ($number) {
    # Spaces are allowed in the input, but they should be stripped.
    $number =~ s/\s//g;
    # All other non-digit characters are disallowed.
    return 0 if $number =~ m/[\D]/;
    
    my $count = 0, my $sum = 0;
    
    for (my $i = length $number; $i > 0; $i--) {
        my $digit = int(substr($number, ($i-1), 1));
        
        if (++$count % 2 == 0) {
            # double every second digit, starting from the right
            # if greater than 9 then subtract 9 from the product
            $digit = $digit * 2 - ($digit > 4 ? 9 : 0);
        }
        $sum += $digit;
    }

    # Strings of length 1 or less are not valid.
    return $count > 1 && $sum % 10 == 0;
}
