module two_fer
   implicit none

contains

   function twoFer(name) result(phrase)
      character(*), intent(in), optional :: name
      character(:), allocatable :: phrase
      character(:), allocatable :: you

      if (present(name)) then
         you = name
      else
         you = 'you'
      end if

      phrase = 'One for '//you//', one for me.'
   end function twoFer

end module two_fer
