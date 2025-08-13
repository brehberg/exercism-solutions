
module space_age
   use, intrinsic :: iso_fortran_env, only: dp=>real64
   implicit none
contains

   pure double precision function age_in_years(planet, seconds)
      character(len=*), intent(in) :: planet
      double precision, intent(in) :: seconds
      real(dp) :: orbital_period

      select case (planet)
       case("Mercury")
         orbital_period = 0.2408467_dp
       case("Venus")
         orbital_period = 0.61519726_dp
       case("Earth")
         orbital_period = 1.0_dp
       case("Mars")
         orbital_period = 1.8808158_dp
       case("Jupiter")
         orbital_period = 11.862615_dp
       case("Saturn")
         orbital_period = 29.447498_dp
       case("Uranus")
         orbital_period = 84.016846_dp
       case("Neptune")
         orbital_period = 164.79132_dp
      end select

      age_in_years = seconds / 31557600 / orbital_period
   end function

end module
