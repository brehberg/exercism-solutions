unit module PhoneNumber;

constant @errors = (
   'letters not permitted',
   'punctuations not permitted',   
   'must not be greater than 11 digits',
   'must not be fewer than 10 digits',
   '11 digits must start with 1',
   'area code cannot start with zero',
   'area code cannot start with one',
   'exchange code cannot start with zero',
   'exchange code cannot start with one',
);

sub clean-number ($number) is export {
   $_ = $number;
   fail @errors[0] if m:i/<[A..Z]>/;
   fail @errors[1] if /<[@:!]>/;

   # extract all digits from the given string
   my @digits = m:g/\d/;
   fail @errors[2] if @digits > 11;
   fail @errors[3] if @digits < 10;

   # all NANP-numbers share the same country code
   if @digits == 11 {
      fail @errors[4] unless @digits[0] == 1;
      @digits .= skip;
   }

   # area and exchange codes only digits from 2 through 9
   fail @errors[5] if @digits[0] == 0;
   fail @errors[6] if @digits[0] == 1;
   fail @errors[7] if @digits[3] == 0;
   fail @errors[8] if @digits[3] == 1;

   @digits.join
}
