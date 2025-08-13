module leap
   implicit none
contains

   ! is_leap_year returns if a given year is a leap year.
   !
   ! Occurs on every year that is evenly divisible by 4
   ! except every year that is evenly divisible by 100
   ! unless the year is also evenly divisible by 400.
   pure logical function is_leap_year(year)
      integer, intent(in) :: year
      is_leap_year = is_divisible_by(year, 4) &
         .and. .not. is_divisible_by(year, 100) &
         .or.  is_divisible_by(year, 400)
   end function

   pure logical function is_divisible_by(n, d)
      integer, intent(in) :: n, d
      is_divisible_by = mod(n, d) == 0
   end function
end module

