module CFL_mod

use Types_mod
use RHS_mod

implicit none

public :: fd1d_heat_explicit_cfl

contains
    subroutine fd1d_heat_explicit_cfl( k, t_num, t_min, t_max, x_num, x_min, x_max, cfl )

      implicit none

      real(KIND=DP), intent(out) :: cfl
      real(KIND=DP) :: dx
      real(KIND=DP) :: dt
      real(KIND=DP), intent(in) :: k
      real(KIND=DP), intent(in) :: t_max
      real(KIND=DP), intent(in) :: t_min
      integer(KIND=SI), intent(in) :: t_num
      real(KIND=DP), intent(in) :: x_max
      real(KIND=DP), intent(in) :: x_min
      integer(KIND=SI), intent(in) :: x_num
      
      dx = ( x_max - x_min ) / dble( x_num - 1 )
      dt = ( t_max - t_min ) / dble( t_num - 1 )

      cfl = k * dt / dx / dx

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  CFL stability criterion value = ', cfl

    end subroutine fd1d_heat_explicit_cfl
end module CFL_mod
