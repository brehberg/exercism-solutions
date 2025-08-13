module raindrops
   implicit none
contains

   pure character(len=20) function convert(i)
      integer, intent(in) :: i
      convert = ""

      if (mod(i,3) == 0) then
         convert = trim(convert)//"Pling"
      end if
      if (mod(i,5) == 0) then
         convert = trim(convert)//"Plang"
      end if
      if (mod(i,7) == 0) then
         convert = trim(convert)//"Plong"
      end if

      if (len_trim(convert) /= 0) return

      write(convert, '(I20)') i
      convert = adjustl(convert)

   end function convert

end module raindrops
