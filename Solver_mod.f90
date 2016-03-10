module Solver_mod

use Types_mod
use RHS_mod

public :: fd1d_hear_explicit
contains

    subroutine fd1d_heat_explicit( x_num, x, t, dt, cfl, h, h_new )

      implicit none

      integer(KIND=SI) :: x_num
      integer(KIND=SI) :: num 

      real(KIND=DP), intent(in) :: cfl
      real(KIND=DP) :: f(x_num)
      real(KIND=DP), intent(in) :: dt
      real(KIND=DP), intent(in) :: h(:)
      real(KIND=DP), intent(out) :: h_new(:)
      integer(KIND=SI) :: j
      real(KIND=DP), intent(in) :: t
      real(KIND=DP), intent(in) :: x(:)

      num = size(x(:),1)

      do j = 1, num
        f(j) = func( j, x_num, x )
      end do

      h_new(1) = 0.0_DP

      do j = 2, x_num - 1
        h_new(j) = h(j) + dt * f(j) + cfl * ( h(j-1) - 2.0_DP * h(j) + h(j+1) )
      end do

      ! set the boundary conditions again
      h_new(1) = 90.0_DP
      h_new(x_num) = 70.0_DP
    end subroutine fd1d_heat_explicit

end module Solver_mod
