module reverse_string
   implicit none
contains
   pure function reverse(input) result(reversed)
      character(*), intent(in) :: input
      character(len=len(input)) :: reversed
      integer :: n, k
      n = len_trim(input)
      forall(k=1:n) reversed(k:k) = input(n-k+1:n-k+1)
   end function
end module
