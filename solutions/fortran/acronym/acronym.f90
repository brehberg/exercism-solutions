
module acronym
   implicit none
   private
   public :: abbreviate
contains

   pure function abbreviate(s)
      character(len=*), intent(in) :: s
      character(len=len_trim(s)) :: abbreviate
      logical :: inside_word, letter
      integer :: i, j, n

      inside_word = .false.
      n = len_trim(s)
      j = 0

      do i = 1,n
         letter = is_letter(s(i:i))
         if ( .not. inside_word .and. letter ) then
            j = j + 1
            abbreviate(j:j) = to_upper(s(i:i))
            inside_word = .true.
         else if ( inside_word .and. .not. letter ) then
            if ( s(i:i) .ne. "'" ) then
               inside_word = .false.
            end if
         end if
      end do

      abbreviate = abbreviate(1:j)
   end function

   pure logical function is_letter(c)
      character, intent(in) :: c

      select case (c)
       case ("a":"z","A":"Z")
         is_letter = .true.
       case default
         is_letter = .false.
      end select
   end function

   pure character function to_upper(c)
      character, intent(in) :: c

      select case (c)
       case ("a":"z")
         to_upper = achar(ichar(c) - 32)
       case default
         to_upper = c
      end select
   end function

end module
