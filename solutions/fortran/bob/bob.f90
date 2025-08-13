module bob
   implicit none
contains

   pure function hey(statement)
      character(len=*), intent(in) :: statement
      character(100) :: hey
      logical :: yelling, question

      if ( isSilence(statement) ) then
         hey = "Fine. Be that way!"
         return
      end if

      yelling = isYelling(statement)
      question = isQuestion(statement)
      if ( yelling .and. question ) then
         hey = "Calm down, I know what I'm doing!"
      else if ( yelling ) then
         hey = "Whoa, chill out!"
      else if ( question ) then
         hey = "Sure."
      else
         hey = "Whatever."
      end if
   end function hey

   pure logical function isSilence(str)
      character(len=*), intent(in) :: str
      integer :: i

      ! check string for non whitespace or ASCII tab character
      isSilence = .true.
      do i = 1,len(str)
         if(str(i:i) /= ' ' .and. str(i:i) /= achar( 9 )) then
            isSilence = .false.
            return
         end if
      end do
   end function isSilence

   pure logical function isQuestion(str)
      character(len=*), intent(in) :: str
      integer :: last_pos

      ! get position of last actual character from string
      last_pos = len(trim(str))
      isQuestion = ( str(last_pos:last_pos) == '?' )
   end function isQuestion

   pure logical function isYelling(str)
      character(len=*), intent(in) :: str
      isYelling = ( hasUpper(str) .and. noLower(str) )
   end function isYelling

   pure logical function hasUpper(string)
      character(len=*), intent(in) :: string
      integer :: i, code

      ! check for at least one uppercase ASCII code value
      hasUpper = .false.
      do i = 1,len(string)
         code = iachar(string(i:i))
         if(code >= iachar("A") .and. code <= iachar("Z")) then
            hasUpper = .true.
            return
         end if
      end do
   end function hasUpper

   pure logical function noLower(string)
      character(len=*), intent(in) :: string
      integer :: i, code

      ! check string for no lowercase ASCII code values
      noLower = .true.
      do i = 1,len(string)
         code = iachar(string(i:i))
         if(code >= iachar("a") .and. code <= iachar("z")) then
            noLower = .false.
         end if
      end do
   end function noLower
end module bob
