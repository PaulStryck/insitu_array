module insitu
  implicit none

  type, public :: insitu_Array
    real(4), pointer :: storage(:)
    integer, dimension(2) :: shp
    real(4), pointer :: A(:,:) => null()

    contains
      procedure :: init => insitu_Array_init
      procedure :: t => insitu_Array_transpose
  end type insitu_Array


  contains
    subroutine insitu_Array_init(this)
      use, intrinsic :: iso_c_binding, only : c_loc, c_f_pointer
      class(insitu_Array), intent(inout) :: this

      call c_f_pointer(c_loc(this%storage), this%A, this%shp)
    end subroutine insitu_Array_init

    subroutine insitu_Array_transpose(this)
      use, intrinsic :: iso_c_binding
      class(insitu_Array), intent(inout) :: this

      call mipt(this%shp(2), this%shp(1), this%storage)

      nullify(this%A)
      call c_f_pointer(c_loc(this%storage), this%A, [this%shp(2), this%shp(1)])
      this%shp = shape(this%A)

    end subroutine insitu_Array_transpose


    subroutine mipt(n, m, A)
      integer, intent(in) :: n, m
      real(4), pointer, intent(in out) :: A(:)
      integer :: cnt, q, k, i, j
      real(4) :: tmp
      logical(kind=1), allocatable :: flg(:)

      allocate(flg(m*n))

      flg = .false.
      q = m * n - 1

      do cnt = 1, m*n-2
        k = m * cnt - q * (cnt / n)
        do while (k > cnt)
          k = m * k - q * (k / n)
        end do
        if (k == cnt) then
          i = k
          tmp = A(i+1)
          do
            j = m * i - q * (i / n)
            if (j == k)  exit

            A(i+1) = A(j+1)

            i = j

          end do
          A(i+1) = tmp
        end if
      end do

      deallocate(flg)

    end subroutine mipt

end module insitu
