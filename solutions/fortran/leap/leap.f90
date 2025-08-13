module leap
   implicit none
contains

   ! is_leap_year returns if a given year is a leap year.
   !
   ! Occurs on every year that is evenly divisible by 4
   ! except every year that is evenly divisible by 100
   ! unless the year is also evenly divisible by 400.
   logical function is_leap_year(year)
      integer :: year
      if ( is_divisible_by(year, 100) ) then
         is_leap_year = is_divisible_by(year, 400)
      else
         is_leap_year = is_divisible_by(year, 4)
      end if
   end function

   logical function is_divisible_by(n, d)
      integer :: n, d
      is_divisible_by = mod(n, d) == 0
   end function
end module

