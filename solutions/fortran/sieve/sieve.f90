module sieve
   implicit none
contains

   pure function primes(limit)
      integer, intent(in) :: limit
      integer, allocatable :: primes(:)
      integer :: i, j
      logical :: marked(2:limit)
      marked = .false.

      do i = 2,floor(sqrt(real(limit)))
         if ( .not. marked(i) ) then
            do j = i*i,limit,i
               marked(j) = .true.
            end do
         end if
      end do
      primes = pack([(i, i=2,limit)], .not. marked(2:))
   end function primes

end module sieve
