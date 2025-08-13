
module triangle
   implicit none

   interface equilateral
      module procedure equilateral_real
      module procedure equilateral_int
   end interface

   interface scalene
      module procedure scalene_real
      module procedure scalene_int
   end interface

   interface isosceles
      module procedure isosceles_real
      module procedure isosceles_int
   end interface

contains

   logical function equilateral_real(edges)
      real,dimension(3) :: edges
      equilateral_real = is_valid_real(edges) &
         .and. abs(edges(1)-edges(2)) < 1.0D-5 &
         .and. abs(edges(2)-edges(3)) < 1.0D-5
   end function

   logical function equilateral_int(edges)
      integer,dimension(3) :: edges
      equilateral_int = is_valid_int(edges) &
         .and. edges(1) == edges(2) &
         .and. edges(2) == edges(3)
   end function

   logical function isosceles_real(edges)
      real,dimension(3) :: edges
      isosceles_real = is_valid_real(edges) &
         .and. (abs(edges(1)-edges(2)) < 1.0D-5 &
         .or. abs(edges(2)-edges(3)) < 1.0D-5 &
         .or. abs(edges(1)-edges(3)) < 1.0D-5)
   end function

   logical function isosceles_int(edges)
      integer,dimension(3) :: edges
      isosceles_int = is_valid_int(edges) &
         .and. (edges(1) == edges(2) &
         .or. edges(2) == edges(3) &
         .or. edges(1) == edges(3))
   end function

   logical function scalene_real(edges)
      real,dimension(3) :: edges
      scalene_real = is_valid_real(edges) &
         .and. .not. equilateral_real(edges) &
         .and. .not. isosceles_real(edges)
   end function

   logical function scalene_int(edges)
      integer,dimension(3) :: edges
      scalene_int = is_valid_int(edges) &
         .and. .not. equilateral_int(edges) &
         .and. .not. isosceles_int(edges)
   end function

   logical function is_valid_real(edges)
      real,dimension(3) :: edges
      logical :: all_positive, violates_inequality

      ! All side lengths must be positive.
      all_positive = minval(edges, 1) > 0

      ! Side lengths cannot violate triangle inequality.
      select case (maxloc(edges, 1))
       case (1)
         violates_inequality = edges(2)+edges(3) < edges(1)
       case (2)
         violates_inequality = edges(1)+edges(3) < edges(2)
       case (3)
         violates_inequality = edges(1)+edges(2) < edges(3)
      end select

      is_valid_real = all_positive .and. .not. violates_inequality
   end function

   logical function is_valid_int(edges)
      integer,dimension(3) :: edges
      logical :: all_positive, violates_inequality

      ! All side lengths must be positive.
      all_positive = minval(edges, 1) > 0

      ! Side lengths cannot violate triangle inequality.
      select case (maxloc(edges, 1))
       case (1)
         violates_inequality = edges(2) + edges(3) < edges(1)
       case (2)
         violates_inequality = edges(1) + edges(3) < edges(2)
       case (3)
         violates_inequality = edges(1) + edges(2) < edges(3)
      end select

      is_valid_int = all_positive .and. .not. violates_inequality
   end function

end module
