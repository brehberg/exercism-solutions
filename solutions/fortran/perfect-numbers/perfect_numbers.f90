
module perfect_numbers
   implicit none

contains

   pure character(len=9) function classify(num)
      integer, intent(in) :: num
      integer limit, total, i

      ! Classification is only possible for natural numbers
      classify = "ERROR"
      if ( num < 1 ) return

      classify = "deficient"
      if ( num == 1 ) return

      limit = floor(num**(1./2.))
      total = 1
      do i = 2,limit
         ! skip to the next loop cycle if i is not a factor
         if ( mod(num,i) /= 0 ) cycle
         total = total + i
         ! only add the corresponding factor if it is different
         if ( i /= num/i ) &
            total = total + num/i
      enddo

      classify = "perfect"
      if ( num > total ) classify = "deficient"
      if ( num < total ) classify = "abundant"
   end function

end module
