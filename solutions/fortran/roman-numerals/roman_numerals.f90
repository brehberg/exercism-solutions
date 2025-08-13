module roman_numerals
   implicit none
   private :: numeral
   public :: roman
contains

   function roman(num) result(s)
      integer, value :: num
      character(15) :: s
      integer :: multiple
      multiple = 1000

      s = trim(numeral(num, multiple, 'M', 'A', 'B')) &
         //trim(numeral(num, multiple, 'C', 'D', 'M')) &
         //trim(numeral(num, multiple, 'X', 'L', 'C')) &
         //trim(numeral(num, multiple, 'I', 'V', 'X'))
   end function roman

   function numeral(n, d, unit, half, full)
      integer, intent(inout) :: n, d
      character(len=1), intent(in) :: unit, half, full
      character(len=4) :: numeral
      integer :: digit, five, ones
      digit = n / d

      if ( digit == 9 ) then
         numeral = unit//full
      else if ( digit == 4 ) then
         numeral = unit//half
      else
         five = digit / 5
         ones = digit - five * 5
         numeral = repeat(half,five)//repeat(unit,ones)
      end if

      n = mod(n, d)
      d = d / 10
   end function numeral

end module roman_numerals
