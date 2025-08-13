module bob
  implicit none
contains

  function hey(statement)
    character(100) :: hey
    character(len=*), intent(in) :: statement

    logical :: silence
    logical :: yelling
    logical :: question

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

  function isQuestion(str)
    logical :: isQuestion
    character(len=*), intent(in) :: str
      
    ! get last actual character from input string
    integer :: pos
    character :: last
    pos = len(trim(str))
    last = str(pos:pos)

    isQuestion = ( last == '?' ) 
  end function isQuestion

  function isYelling(str)
    logical :: isYelling
    character(len=*), intent(in) :: str

    isYelling = ( str == toUpper(str) .and. str /= toLower(str) )
  end function isYelling

  function toUpper(string) result(upper)
    character(len=*), intent(in) :: string
    character(len=len(string)) :: upper
    integer :: i, j

    ! adjust ASCII code values to uppercase representation
    do i = 1,len(string)
      j = iachar(string(i:i))      
      if(j >= iachar("a") .and. j <= iachar("z")) then
        upper(i:i) = achar(j - 32)
      else
        upper(i:i) = string(i:i)
      end if
    end do
    end function toUpper

    function toLower(string) result(lower)
      character(len=*), intent(in) :: string
      character(len=len(string)) :: lower
      integer :: i, j
  
      ! adjust ASCII code values to lowercase representation
      do i = 1,len(string)
        j = iachar(string(i:i))
        if(j >= iachar("A") .and. j <= iachar("Z")) then
          lower(i:i) = achar(j + 32)
        else
          lower(i:i) = string(i:i)
        end if
      end do
    end function toLower

    function isSilence(str)
      logical :: isSilence
      character(len=*), intent(in) :: str    
      character :: tab = achar( 9 )
      integer :: i

      ! check string for whitespace ASCII code values
      isSilence = .true.
      do i = 1,len(str)
        if(str(i:i) /= ' ' .and. str(i:i) /= tab) then
          isSilence = .false.
        end if
      end do
    end function
end module bob
