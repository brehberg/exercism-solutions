module isogram
   implicit none

contains

   logical function isIsogram(phrase) result(no_repeats)
      character(len=*), intent(in) :: phrase

      integer :: i, code, letters
      no_repeats = .true.
      letters = 0

      do i = 1, len(phrase)
         code = iachar(phrase(i:i))
         ! adjust ASCII code for case and ignore other characters
         if ( is_upper(code) ) then
            code = 1 + code - iachar('A')
         else if ( is_lower(code) ) then
            code = 1 + code - iachar('a')
         else
            cycle
         end if
         ! check is this letter has been seen before
         if ( btest(letters, code) ) then
            no_repeats = .false.
            exit
         endif
         letters = ibset(letters, code)
      end do
   end function isIsogram

   pure logical function is_upper(code)  ! ASCII A-Z
      integer,intent(in) :: code
      is_upper = code >= 65 .and. code <= 90
   end function is_upper

   pure logical function is_lower(code)  ! ASCII a-z
      integer,intent(in) :: code
      is_lower = code >= 97 .and. code <= 122
   end function is_lower

end module isogram
