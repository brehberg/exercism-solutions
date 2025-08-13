
module allergies
   implicit none

   private
   character(len=12), parameter :: allergens(8) = [ &
      "eggs        ", & ! (1)
      "peanuts     ", & ! (2)
      "shellfish   ", & ! (4)
      "strawberries", & ! (8)
      "tomatoes    ", & ! (16)
      "chocolate   ", & ! (32)
      "pollen      ", & ! (64)
      "cats        "]   ! (128)

   public allergicTo, allergicList

contains
   logical function allergicTo(allergy_str, allergy_key)
      character(len=*), intent(in) :: allergy_str
      integer, intent(in) :: allergy_key

      ! check the value of allergy score code for given string
      integer :: i
      do i = 1,size(allergens)
         if ( allergens(i) == allergy_str ) then
            allergicTo = isAllergic(allergy_key, i)
            exit
         end if
      end do
   end function

   function allergicList(allergy_key)
      integer, intent(in) :: allergy_key
      character(len=100) :: allergicList
      integer :: i

      ! check allergy score for each value and concatenate matching keys
      allergicList = ''
      do i = 1,size(allergens)
         if ( isAllergic(allergy_key, i) ) then
            allergicList = trim(allergicList)//' '//allergens(i)
         end if
      end do
      allergicList = adjustl(allergicList)
   end function

   logical function isAllergic(allergy_key, allergy_pos)
      integer, intent(in) :: allergy_key, allergy_pos
      integer :: allergy_code
      allergy_code = 2**(allergy_pos-1)
      isAllergic =  ( iand(allergy_key, allergy_code) == allergy_code )
   end function
end module
