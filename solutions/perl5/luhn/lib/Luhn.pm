package Luhn;

use v5.38;

use Exporter qw<import>;
our @EXPORT_OK = qw<is_luhn_valid>;

sub is_luhn_valid ($number) {
    # Spaces are allowed in the input, but they should be stripped before checking.
    $number =~ s/\s//g;
    # All other non-digit characters are disallowed.
    if ($number =~ m/[\D]/) {
        return 0;
    }
    
    my $count = 0;
    my $sum = 0;
    
    for (my $i = length $number; $i > 0; $i--) {        
        $count += 1;
        my $number = int(substr($number, ($i-1), 1));        
        
        if ($count % 2 == 0) {
            # double every second digit, starting from the right
            # if greater than 9 then subtract 9 from the product
            $number = $number * 2 - ($number > 4 ? 9 : 0);
        }
        $sum += $number;        
    }

    # Strings of length 1 or less are not valid. 
    return $count > 1 && $sum % 10 == 0;
}
