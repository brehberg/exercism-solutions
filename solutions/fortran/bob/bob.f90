module bob
   implicit none
contains

   function hey(statement)
      character(100) :: hey
      character(len=*), intent(in) :: statement
      logical :: silence, yelling, question

      silence = isSilence(statement)
      yelling = isYelling(statement)
      question = isQuestion(statement)

      if ( silence ) then
         hey = "Fine. Be that way!"
      else if ( yelling .and. question ) then
         hey = "Calm down, I know what I'm doing!"
      else if ( yelling ) then
         hey = "Whoa, chill out!"
      else if ( question ) then
         hey = "Sure."
      else
         hey = "Whatever."
      end if
   end function hey

   logical function isQuestion(str)
      character(len=*), intent(in) :: str

      ! get last actual character from input string
      integer :: pos
      character :: last
      pos = len(trim(str))
      last = str(pos:pos)

      isQuestion = ( last == '?' )
   end function isQuestion

   logical function isYelling(str)
      character(len=*), intent(in) :: str

      isYelling = ( str == toUpper(str) .and. str /= toLower(str) )
   end function isYelling

   logical function isSilence(str)
      character(len=*), intent(in) :: str
      character :: TAB = achar( 9 )
      integer :: i

      ! check string for whitespace ASCII characters
      isSilence = .true.
      do i = 1,len(str)
         if(str(i:i) /= ' ' .and. str(i:i) /= TAB) then
            isSilence = .false.
         end if
      end do
   end function

   function toUpper(string) result(upper)
      character(len=*), intent(in) :: string
      character(len=len(string)) :: upper
      integer :: i, code

      ! adjust ASCII code values to uppercase representation
      do i = 1,len(string)
         code = iachar(string(i:i))
         if(code >= iachar("a") .and. code <= iachar("z")) then
            upper(i:i) = achar(code - 32)
         else
            upper(i:i) = string(i:i)
         end if
      end do
   end function toUpper

   function toLower(string) result(lower)
      character(len=*), intent(in) :: string
      character(len=len(string)) :: lower
      integer :: i, code

      ! adjust ASCII code values to lowercase representation
      do i = 1,len(string)
         code = iachar(string(i:i))
         if(code >= iachar("A") .and. code <= iachar("Z")) then
            lower(i:i) = achar(code + 32)
         else
            lower(i:i) = string(i:i)
         end if
      end do
   end function toLower
end module bob
