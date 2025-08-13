
module collatz_conjecture
   implicit none
contains

   integer function steps(i)
      integer :: i, n
      steps = -1

      ! invalid starting value, must be a positive integer
      if ( i < 1 ) return

      steps = 0
      n = i
      do while ( n > 1 )
         if ( mod(n,2) == 0 ) then
            n = n / 2
         else
            n = 3*n + 1
         end if
         steps = steps + 1
      end do
   end function

end module
