module RHS_mod

use Types_mod

implicit none
public :: func

contains
    function func( j, x_num, x ) result ( d )
      integer(KIND=SI) :: j, x_num, num
      real(KIND=DP) :: d
      real(KIND=DP), intent(in) :: x(:)
      num = size(x(:),1)
      d = 0.0_DP
    end function func
end module RHS_mod
