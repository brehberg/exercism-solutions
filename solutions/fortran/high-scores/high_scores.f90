
module high_scores
   implicit none
contains

   pure function scores(score_list)
      integer, dimension(:), intent(in) :: score_list
      integer, dimension(size(score_list)) :: scores
      scores = score_list
   end function

   pure integer function latest(score_list)
      integer, dimension(:), intent(in) :: score_list
      latest = score_list(size(score_list))
   end function

   pure integer function personalBest(score_list)
      integer, dimension(:), intent(in) :: score_list
      personalBest = maxval(score_list)
   end function

   pure function personalTopThree(score_list)
      integer, dimension(:), intent(in) :: score_list
      integer, dimension(3) :: personalTopThree
      logical, dimension(size(score_list)) :: mask
      integer :: i, max_pos

      personalTopThree = 0
      mask = .true.

      ! find largest non-masked element, copy to output and mask it
      do i = 1,size(score_list)
         max_pos = maxloc(score_list,1,mask)
         personalTopThree(i) = score_list(max_pos)
         mask(max_pos) = .false.
      enddo
   end function
end module
