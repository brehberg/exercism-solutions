
module high_scores
   implicit none
contains

   function scores(score_list)
      integer, dimension(:), intent(in) :: score_list
      integer, dimension(size(score_list)) :: scores
      scores = score_list
   end function

   integer function latest(score_list)
      integer, dimension(:), intent(in) :: score_list
      latest = score_list(size(score_list))
   end function

   integer function personalBest(score_list)
      integer, dimension(:), intent(in) :: score_list
      personalBest = maxval(score_list)
   end function

   function personalTopThree(score_list)
      integer, dimension(:), intent(in) :: score_list
      integer, dimension(3) :: personalTopThree
      logical, allocatable :: mask(:)
      integer :: count, i

      ! create mask as logical array the same size as score_list
      count = size(score_list)
      allocate(mask(count))
      do i = 1,count
         mask(i) = .true.
      enddo

      ! find the largest non masked element, then copy and mask it
      personalTopThree = [0,0,0]
      do i = 1,count
         personalTopThree(i) = maxval(score_list,mask)
         mask(maxloc(score_list,mask)) = .false.
      enddo
   end function
end module
