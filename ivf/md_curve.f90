module md_curve
    ! The curve object
    use select_precision, only: wp

    implicit none

    private
    public tpcurve

    type tpline
        integer :: linesize
        integer :: linestyle
        character(len=:), allocatable :: linecolor
    end type

    type tpmarker
        integer :: markersize
        integer :: markerstyle
        character(len=:), allocatable :: markercolor
    end type

    type tpcurve
        !Curve object
        real(wp), allocatable           :: x(:), y(:)
        integer                         :: ndata
        character(len=:), allocatable   :: title
        character(len=:), allocatable   :: linespec  !line and marker specification
        type(tpline)   :: line
        type(tpmarker) :: marker
    contains
        procedure, pass(curv) :: set_data
        procedure, pass(curv) :: destroy_me
    end type

contains
    !TODO: other routines shall be developed for line spec.
    !TODO: other routines shall be developed for marker spec etc.
    subroutine set_data(curv, x, y)
        class(tpcurve):: curv
        real(wp), intent(in)    :: x(:)
        real(wp), intent(in)    :: y(:)
        curv%x = x
        curv%y = y
        curv%ndata=size(x)
    end subroutine

    subroutine destroy_me(curv)
        ! destroy_me
        class(tpcurve), intent(inout) :: curv
        if (allocated(curv%x)) then
            deallocate(curv%x)
        end if
        if (allocated(curv%y)) then
            deallocate(curv%y)
        end if

    end subroutine destroy_me


end module md_curve
