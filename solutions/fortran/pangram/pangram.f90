module pangram
   implicit none
contains

   logical function is_pangram(sentence)
      character(*) :: sentence

      integer :: i, code, letters
      letters = 0

      do i = 1, len(sentence)
         code = iachar(sentence(i:i))
         if ( is_upper(code) ) then
            letters = ibset(letters, 1 + code - iachar('A'))
         else if ( is_lower(code) ) then
            letters = ibset(letters, 1 + code - iachar('a'))
         end if
      end do

      is_pangram = popcnt(letters) == 26
   end function is_pangram

   pure logical function is_upper(code)  ! ASCII A-Z
      integer,intent(in) :: code
      is_upper = code >= 65 .and. code <= 90
   end function is_upper

   pure logical function is_lower(code)  ! ASCII a-z
      integer,intent(in) :: code
      is_lower = code >= 97 .and. code <= 122
   end function is_lower

end module pangram
