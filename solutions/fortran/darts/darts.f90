module darts
   implicit none

contains

   pure integer function score(x, y) result(points)
      real, intent(in):: x, y
      real :: dist ! distance from center
      dist = sqrt(x * x + y * y)

      if (dist <= 1) then
         ! in the inner circle
         points = 10
      elseif (dist <= 5) then
         ! in the middle circle
         points = 5
      elseif (dist <= 10) then
         ! in the outer circle
         points = 1
      else
         ! outside the target
         points = 0
      end if
   end function score

end module darts
