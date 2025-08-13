module difference_of_squares
   implicit none
contains

   ! Calculate square of sum from 1 to a given end number
   !   y = ((x + x^2) / 2)^2
   integer function square_of_sum(n)
      integer :: n, sum
      sum = (n+n*n)/2
      square_of_sum = sum*sum
   end function

   ! Calculate sum of squares from 1 to a given end number
   !   y = (x + 3x^2 + 2x^3) / 6
   integer function sum_of_squares(n)
      integer :: n
      sum_of_squares = (n + 3*n*n + 2*n*n*n)/6
   end function

   ! Calculate the difference between the square of the sum and
   ! the sum of the squares of the first N natural numbers
   integer function difference(n)
      integer :: n
      difference = square_of_sum(n) - sum_of_squares(n)
   end function

end module difference_of_squares
