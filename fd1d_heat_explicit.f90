program fd1d_heat_explicit_prb

      use Types_mod
      implicit none

      integer(KIND=SI), parameter :: t_num = 201
      integer(KIND=SI), parameter :: x_num = 21 
      
      real(KIND=DP) :: cfl
      real(KIND=DP) :: dt
      real(KIND=DP) :: h(x_num)
      real(KIND=DP) :: h_new(x_num)
      ! the "matrix" stores all x-values for all t-values
      ! remember Fortran is column major, meaning that rows are contiguous
      real(KIND=DP) :: hmat(x_num, t_num)
      integer(KIND=SI) :: i
      integer(KIND=SI) :: j
      real(KIND=DP) :: k

      real(KIND=DP) :: t(t_num)
      real(KIND=DP) :: t_max
      real(KIND=DP) :: t_min
      real(KIND=DP) :: x(x_num)
      real(KIND=DP) :: x_max
      real(KIND=DP) :: x_min

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FD1D_HEAT_EXPLICIT_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Test the FD1D_HEAT_EXPLICIT library.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FD1D_HEAT_EXPLICIT_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FD1D_HEAT_EXPLICIT_TEST01:'
      write ( *, '(a)' ) '  Compute an approximate solution to the time-dependent'
      write ( *, '(a)' ) '  one dimensional heat equation:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    dH/dt - K * d2H/dx2 = f(x,t)'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Run a simple test case.'

      ! heat coefficient
      k = 0.002_DP00

      ! the x-range values
      x_min = 0.0_DP00
      x_max = 1.0_DP00
      ! x_num is the number of intervals in the x-direction
      call r8vec_linspace( x_num, x_min, x_max, x )

      ! the t-range values. integrate from t_min to t_max
      t_min = 0.0_DP00
      t_max = 80.0_DP00

      ! t_num is the number of intervals in the t-direction
      dt = ( t_max - t_min ) / dble( t_num - 1 )
      call r8vec_linspace( t_num, t_min, t_max, t )

      ! get the CFL coefficient
      call fd1d_heat_explicit_cfl( k, t_num, t_min, t_max, x_num, x_min, x_max, cfl )

     if ( 0.5_DP00 <= cfl ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FD1D_HEAT_EXPLICIT_CFL - Fatal error!'
        write ( *, '(a)' ) '  CFL condition failed.'
        write ( *, '(a)' ) '  0.5 <= K * dT / dX / dX = CFL.'
        stop
      end if

      ! set the initial condition
      do j = 1, x_num
        h(j) = 50.0_DP00
      end do

      ! set the bounday condition
      h(1) = 90.0_DP00
      h(x_num) = 70.0_DP00

      ! initialise the matrix to the initial condition
      do i = 1, x_num
        hmat(i, 1) = h(i)
      end do

      ! the main time integration loop 
      do j = 2, t_num
        call fd1d_heat_explicit( x_num, x, t(j-1), dt, cfl, h, h_new )

        do i = 1, x_num
          hmat(i, j) = h_new(i)
          h(i) = h_new(i)
        end do
      end do

      ! write data to files
      call r8mat_write( 'h_test01.txt', x_num, t_num, hmat )
      call r8vec_write( 't_test01.txt', t_num, t )
      call r8vec_write( 'x_test01.txt', x_num, x )
    contains

    function func( j, x_num, x ) result ( d )
      integer(KIND=SI), intent(in) :: j, x_num = size(x(:),1)
      real(KIND=DP), intent(out) :: d
      real(KIND=DP), intent(in) :: x(:)

      d = 0.0_DP00
    end function func

    subroutine fd1d_heat_explicit( x_num, x, t, dt, cfl, h, h_new )

      implicit none

      integer(KIND=SI) :: x_num = size(x(:),1)

      real(KIND=DP), intent(in) :: cfl
      real(KIND=DP) :: f(x_num)
      real(KIND=DP), intent(in) :: dt
      real(KIND=DP), intent(in) :: h(x_num)
      real(KIND=DP), intent(out) :: h_new(x_num)
      integer(KIND=SI) :: j
      real(KIND=DP), intent(in) :: t
      real(KIND=DP), intent(in) :: x(:)

      do j = 1, x_num
        f(j) = func( j, x_num, x )
      end do

      h_new(1) = 0.0_DP00

      do j = 2, x_num - 1
        h_new(j) = h(j) + dt * f(j) + cfl * ( h(j-1) - 2.0_DP00 * h(j) + h(j+1) )
      end do

      ! set the boundary conditions again
      h_new(1) = 90.0_DP00
      h_new(x_num) = 70.0_DP00
    end subroutine fd1d_heat_explicit

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
      integer(KIND=SI), intent(in) :: x_num = size(x(:), 1)

      dx = ( x_max - x_min ) / dble( x_num - 1 )
      dt = ( t_max - t_min ) / dble( t_num - 1 )

      cfl = k * dt / dx / dx

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  CFL stability criterion value = ', cfl

    end subroutine fd1d_heat_explicit_cfl

    subroutine r8mat_write( output_filename, m, n, table )

      implicit none

      integer(KIND=SI), intent(in) :: m = size(table(:,:),1)
      integer(KIND=SI), intent(in) :: n = size(table(:,:),2)

      integer(KIND=SI) :: j
      character (len=*), intent(in) :: output_filename
      integer(KIND=SI) :: output_unit
      character (len=30) :: string 
      real(KIND=DP), intent(out) :: table(:,:)
 
      output_unit = 10
      open( unit = output_unit, file = output_filename, status = 'replace' )

      write ( string, '(a1,i8,a1,i8,a1,i8,a1)' ) '(', m, 'g', 24, '.', 16, ')'

      do j = 1, n
        write ( output_unit, string ) table(1:m, j)
      end do

      close( unit = output_unit )
    end subroutine r8mat_write

    subroutine r8vec_linspace ( n, a_first, a_last, a )

      implicit none

      integer(KIND=SI), intent(in) :: n = size(a(:),1)
      real(KIND=DP), intent(out) :: a(:)
      real(KIND=DP), intent(in) :: a_first
      real(KIND=DP), intent(in) :: a_last
      integer(KIND=SI) :: i

      do i = 1, n
        a(i) = ( dble( n - i ) * a_first + dble( i - 1 ) * a_last ) / dble( n - 1 )
      end do

    end subroutine r8vec_linspace

    subroutine r8vec_write ( output_filename, n, x )

      implicit none

      integer(KIND=SI) :: m 
      integer(KIND=SI), intent(in) :: n = size(x(:),1)

      integer(KIND=SI) :: j
      character(len=*), intent(in) :: output_filename
      integer(KIND=SI) :: output_unit
      real(KIND=DP), intent(out) :: x(:)

      output_unit = 11
      open( unit = output_unit, file = output_filename, status = 'replace' )

      do j = 1, n
        write ( output_unit, '(2x,g24.16)' ) x(j)
      end do

      close ( unit = output_unit )
  end subroutine r8vec_write

end program fd1d_heat_explicit_prb

