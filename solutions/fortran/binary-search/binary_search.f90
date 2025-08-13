module binary_search
   implicit none
contains

   function find(array, val) result(idx)
      integer, dimension(:), intent(in) :: array
      integer, intent(in) :: val
      integer :: idx, low, mid, high

      idx = -1
      low = 1
      high = size(array)

      do while (low <= high)
         mid = low + (high - low) / 2
         if ( array(mid) < val) then
            low = mid + 1
         else if ( array(mid) > val) then
            high = mid - 1
         else
            idx = mid
            exit
         end if
      end do

   end function

end module
