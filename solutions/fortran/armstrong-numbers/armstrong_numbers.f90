
module armstrong_numbers
   implicit none
contains

   ! An Armstrong number is a number that is the sum of its own digits
   ! each raised to the power of the number of digits.
   logical function isArmstrongNumber(num)
      integer, intent(in) :: num
      integer, dimension(:), allocatable :: digits
      integer :: count, i, rem, sum

      if ( num == 0 ) then
         isArmstrongNumber = .true.
         return
      endif

      ! determine number of digits in the number and allocate storage
      count = floor(log10(real(num))+1)
      allocate(digits(count))

      ! take advantage of integer division to determine each digit
      rem = num
      do i = 1,count
         digits(i) = rem - (rem/10)*10
         rem = rem/10
      enddo

      ! calculate to sum of digits each to the power of digit count
      sum = 0
      do i = 1,count
         sum = sum + digits(i)**count
      enddo

      isArmstrongNumber = ( num == sum )
   end function isArmstrongNumber
end module
