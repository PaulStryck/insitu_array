program main
  use insitu

  implicit none

  type(insitu_Array) :: Ao

  integer, parameter :: m = 2, n = 3
  real(4), target :: Ac(m*n)

  call random_number(Ac)


  Ao = insitu_Array(storage=Ac, shp=[m,n])
  call Ao%init()

  call pp_arr(Ao%A)
  call Ao%t()
  call pp_arr(Ao%A)


  contains
    subroutine pp_arr(Grid)
      real(4), intent(in) :: Grid(:,:)

      integer :: i,j, n,m
        n = size(Grid, 1)
        m = size(Grid, 2)

        write( * , "(*(g0.4))" ) ( (Grid(i,j)," ",j=1,m), new_line("A"), i=1,n)
     end subroutine
end program main
