module hamming
   implicit none
contains

   logical function compute(strand1, strand2, distance)
      character(*) :: strand1, strand2
      integer :: distance, i

      distance = 0
      compute = len(strand1) == len(strand2)
      if ( .not. compute ) return  ! different lengths

      do i = 1,len(strand1)
         if ( strand1(i:i) /= strand2(i:i) ) &
            distance = distance + 1
      end do
   end function

end module hamming
