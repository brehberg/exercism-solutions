
module allergies
   implicit none

   private
   character(len=12), dimension(8) :: allergens = [character(len=12) :: &
      "eggs",         & ! (1)
      "peanuts",      & ! (2)
      "shellfish",    & ! (4)
      "strawberries", & ! (8)
      "tomatoes",     & ! (16)
      "chocolate",    & ! (32)
      "pollen",       & ! (64)
      "cats"]           ! (128)

   public allergicTo, allergicList

contains
   logical function allergicTo(allergy_str, allergy_key)
      character(len=*), intent(in) :: allergy_str
      integer, intent(in) :: allergy_key

      ! determine the allergy score value for given string
      integer :: code, i
      do i = 1,size(allergens)
         if ( allergens(i) == allergy_str ) then
            code = 2**(i-1)
            exit
         end if
      end do

      allergicTo = ( iand(allergy_key, code) == code )
   end function

   function allergicList(allergy_key)
      integer, intent(in) :: allergy_key
      character(len=100) :: allergicList
      integer :: code, i

      ! check allergy score for each value and concatenate matching keys
      allergicList = ''
      do i = 1,size(allergens)
         code = 2**(i-1)
         if ( iand(allergy_key, code) == code ) then
            if ( trim(allergicList) == '' ) then
               allergicList = allergens(i)
            else
               allergicList = trim(allergicList)//' '//allergens(i)
            end if
         end if
      end do
   end function
end module
