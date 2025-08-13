
module grains
   implicit none
contains

   double precision function square(n)
      integer :: n
      if ( n < 1 .or. n > 64 ) then
         square = -1D0
         return
      end if

      square = 2D0**(n-1)
   end function

   double precision function total()
      total = 2D0**64
   end function

end module
