module md_plot
    ! Implements the plot object in ogpf
    ! Rev 0.10
    use select_precision, only: wp
    use md_errhandling
    use md_helperproc
    use md_curves

    implicit none
    character(len=*), parameter :: md_name='md_plot'

    private
    public :: tpplot



    !TODO: add grid object
    !TODO:


    type tpplot
        private
        integer, public :: total_curves = 0
        type(linked_list)  :: curves


        character(len=:), allocatable  :: txtplottitle
        character(len=:), allocatable  :: txtxlabel
        character(len=:), allocatable  :: txtylabel
        character(len=:), allocatable  :: txtoptions
        character(len=8)               :: txtplotscale

        logical :: hasplottitle =    .false.
        logical :: hasxlabel    =    .false.
        logical :: hasylabel    =    .false.
        logical :: haszlabel    =    .false.
        logical :: hasxrange    =    .false.
        logical :: hasyrange    =    .false.
        logical :: haszrange    =    .false.
        logical :: hasoptions   =    .false.
        real(wp):: xrange(2) ! x-range [xmin, xmax]
        real(wp):: yrange(2) ! y-range [ymin, ymax]

    contains
        private
        procedure, pass, private :: plot2d_vector_vs_vector
        procedure, pass, private :: plot2d_matrix_vs_vector
        generic,         public  :: plot    => plot2d_vector_vs_vector, &
            plot2d_matrix_vs_vector
        procedure, pass, public ::  print_list ! to be removed later
        procedure, pass, public ::  write2scriptfile

        ! NOTE (Mohammad#1#02/08/18): Added in v3: to be completed

        procedure, pass, public :: options      => set_options
        procedure, pass, public :: title        => set_plottitle
        procedure, pass, public :: xlabel       => set_xlabel
        procedure, pass, public :: ylabel       => set_ylabel
        procedure, pass, public :: axis         => set_axes_range
        procedure, pass, public :: plotscale    => set_plotscale
        procedure, pass, public :: reset        => reset_to_defaults

    end type tpplot

contains



    !..............................................................................
    subroutine set_plotscale(this,strsclae)
        !..............................................................................
        !Set the plot title
        class(tpplot):: this
        character(len=*), intent(in) :: strsclae
        this%txtplotscale = trim(strsclae)
    end subroutine set_plotscale


    !..............................................................................
    subroutine set_axes_range(this,rng)
        ! set the range of axes in the form of [xmin, xmax, ymin, ymax]
        !
        class(tpplot):: this
        real(wp), intent(in) :: rng(:)
        integer :: n
        n=size(rng,dim=1)
        select case(n)
            case(2) !Only the range for x-axis has been sent
                this%hasxrange=.true.
                this%xrange=rng(1:2)
            case(4)
                this%hasxrange=.true.
                this%hasyrange=.true.
                this%xrange=rng(1:2)
                this%yrange=rng(3:4)
            case default
                print*, 'gpf error: wrong axis range setting!'
                return
        end select
        ! TODO (Mohammad#2): to be corected for default case Rev 0.18
    end subroutine set_axes_range


    !..............................................................................
    subroutine set_options(this,stropt)
        !..............................................................................
        !Set the plot title
        class(tpplot):: this
        character(len=*), intent(in) :: stropt
        !    Integer, Save :: strlength=0
        !    strlength=strlength+len_trim(stropt)
        !change in rev 0.18
        !    if (strlength < len_options) then

        if ( len_trim(stropt)>0 ) then
            this%txtoptions=trim(this%txtoptions)//';'//trim(stropt)
        end if

        !    else
        !        print*, 'ogpf warning: the length of options exceeds than the set value :', len_options
        !        print*, 'options is truncated to ', len_options
        !        this%txtOptions=trim(this%txtOptions)//';'//trim(string)
        !    end if
        this%hasoptions=.true.
    end subroutine set_options


    !..............................................................................
    subroutine set_plottitle(this,string)
        !..............................................................................
        !Set the plot title
        class(tpplot):: this
        character(len=*), intent(in) :: string
        this%txtplottitle=trim(string)
        this%hasplottitle=.true.
    end subroutine set_plottitle


    !..............................................................................
    subroutine set_xlabel(this,string)
        !..............................................................................
        !Set the xlabel
        class(tpplot):: this
        character(len=*), intent(in) :: string
        this%txtxlabel=trim(string)
        this%hasxlabel=.true.
    end subroutine set_xlabel


    !..............................................................................
    subroutine set_ylabel(this,string)
        !..............................................................................
        !Set the ylabel
        class(tpplot):: this
        character(len=*), intent(in) :: string
        this%txtylabel=trim(string)
        this%hasylabel=.true.
    end subroutine set_ylabel




    !..............................................................................
    subroutine reset_to_defaults(this)
        !..............................................................................
        !Reset all ogpf properties (params to their default values
        class(tpplot):: this

        this%txtoptions  =  ""
        this%txtplottitle=  ""

        this%txtxlabel   =  ""
        this%txtylabel   =  ""

        this%hasoptions  =  .false.
        this%hasplottitle=  .false.

        this%hasxlabel   =  .false.
        this%hasylabel   =  .false.

        this%hasxrange   =  .false.
        this%hasyrange   =  .false.

    end subroutine reset_to_defaults


    !..............................................................................
    subroutine plot2d_vector_vs_vector( this, &
            x1, y1, ls1, &
            x2, y2, ls2, &
            x3, y3, ls3, &
            x4, y4, ls4, hold_status  )
        !..........................................................................
        ! This procedure plots:
        !   1. A vector against another vector (xy plot)
        !   2. A vector versus its element indices.
        !   3. Can accept up to 4 data sets as x,y pairs!
        ! Arguments
        ! xi, yi vectors of data series,
        ! lsi a string maximum 80 characters containing the line spec, legends, ...

        class(tpplot):: this
        ! Input vector
        real(wp),  intent(in)                          :: x1(:)
        real(wp),  intent(in), optional                :: y1(:)
        character(len=*),  intent(in), optional        :: ls1

        real(wp),  intent(in), dimension(:), optional  :: x2
        real(wp),  intent(in), dimension(:), optional  :: y2
        character(len=*),  intent(in), optional        :: ls2

        real(wp),  intent(in), dimension(:), optional  :: x3
        real(wp),  intent(in), dimension(:), optional  :: y3
        character(len=*),  intent(in), optional        :: ls3

        real(wp),  intent(in), dimension(:), optional  :: x4
        real(wp),  intent(in), dimension(:), optional  :: y4
        character(len=*),  intent(in), optional        :: ls4

        logical, intent(in)                            :: hold_status

        !   Local variables
        character(len=*), parameter :: proc_name='plot2D_vector_vs_vector'
        type(element), allocatable  :: pltcrvs(:)  !curves in one plot call
        integer:: nx1
        integer:: ny1
        integer:: nx2
        integer:: ny2
        integer:: nx3
        integer:: ny3
        integer:: nx4
        integer:: ny4
        integer:: i
        integer:: ierr
        integer:: number_of_curves
        character(len=3) ::   curve_type
        character(len=80)::  pltstring(4)  !Four 80 character lines

        !Initialize variables
        curve_type=''
        pltstring=''
        !   Check the input
        nx1=size(x1)
        if ((present(y1) )) then
            ny1=size(y1)
            if (checkdim(nx1,ny1)) then
                curve_type='xy1'
                number_of_curves=1
            else
                call gp_error(bad_array_size,md_name,proc_name,'length of x1 and y1 doesnot match')
            end if
        else !plot only x againest its element indices
            curve_type='xi'
            number_of_curves=1
        end if

        pltstring(1) = set_lspec(ls1)

        if (present(x2) .and. present (y2)) then
            nx2=size(x2)
            ny2=size(y2)
            if (checkdim(nx2,ny2)) then
                curve_type='xy2'
                number_of_curves=2
            else
                call gp_error(bad_array_size,md_name,proc_name,'length of x2 and y2 doesnot match')
            end if
            pltstring(2) = set_lspec(ls2)
        end if

        if (present(x3) .and. present (y3)) then
            nx3=size(x3)
            ny3=size(y3)
            if (checkdim(nx3,ny3)) then
                curve_type='xy3'
                number_of_curves=3
            else
                call gp_error(bad_array_size,md_name,proc_name,'length of x3 and y3 doesnot match')
            end if
            !Process line spec for 3rd data set if present
            pltstring(3) = set_lspec(ls3)
        end if

        if (present(x4) .and. present (y4)) then
            nx4=size(x4)
            ny4=size(y4)
            if (checkdim(nx4,ny4)) then
                curve_type='xy4'
                number_of_curves=4
            else
                call gp_error(bad_array_size,md_name,proc_name,'length of x4 and y4 doesnot match')
            end if
            !Process line spec for 4th data set if present
            pltstring(4) = set_lspec(ls4)
        end if
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        !Allocate the curves
        allocate(pltcrvs(number_of_curves), stat=ierr)
        if (ierr /=0) then
            call gp_error(alloc_err,md_name,proc_name,'allocation error in md_plot for pltcrvs')
        end if

        ! Write the linespec into pltcrvs
        do i=1, number_of_curves
            pltcrvs(i)%linespec= pltstring(i)
        end do

        ! Write xy data into pltcrvs
        select case (curve_type)
            case ('xi')
                call pltcrvs(1)%set_data([(real(i, wp), i=1, nx1)], x1)
            case ('xy1')
                call pltcrvs(1)%set_data(x1, y1)
            case ('xy2')
                call pltcrvs(1)%set_data(x1, y1)
                call pltcrvs(2)%set_data(x2, y2)
            case ('xy3')
                call pltcrvs(1)%set_data(x1, y1)
                call pltcrvs(2)%set_data(x2, y2)
                call pltcrvs(3)%set_data(x3, y3)
            case ('xy4')
                call pltcrvs(1)%set_data(x1, y1)
                call pltcrvs(2)%set_data(x2, y2)
                call pltcrvs(3)%set_data(x3, y3)
                call pltcrvs(4)%set_data(x4, y4)
        end select

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! Store data in curves linked list
        ! Debug (Mohammad#1#02/08/18): for debug: remove later ...
        !

        print*, 'md_plot: plot2D_vector_vs_vector: total%curve:', this%total_curves

        if (.not. hold_status) then             ! hold is off
            if (this%total_curves > 0) then     ! there is already some curves
                call this%curves%delete()       ! Destroy the previous curves
                this%total_curves = 0           ! reset curve numbers
            end if
        end if

        !!!        if (this%total_curves == 0) then
        !!!            this%curves = list() ! create the list of curves
        !!!        end if
        do i=1, number_of_curves !append curves
            call this%curves%add( pltcrvs(i) )
        end do
        this%total_curves = this%total_curves + number_of_curves


        !: End of plot2D_vector_vs_vector
    end subroutine plot2d_vector_vs_vector


    !..............................................................................
    subroutine  plot2d_matrix_vs_vector(this, xv,ymat, lspec, hold_status)
        ! plot2D_matrix_vs_vector accepts a vector xv and a matrix ymat and plots
        ! columns of ymat against xv
        !
        ! lspec is an optional string defines the line specification for each data series
        ! Linespec for each curve (set of data) is separated by semicolon ";"
        ! wrong linespec cause to ignore setting line specification
        ! using the same linespec


        class(tpplot):: this
        ! Input arrays
        real(wp),  intent(in)                       :: xv(:)
        real(wp),  intent(in)                       :: ymat(:,:)
        character(len=*),  intent(in), optional     :: lspec
        logical, intent(in)                         :: hold_status
        !----------------------------------------------------------------------
        ! Local variables
        character(len=*), parameter :: proc_name='plot2D_matrix_vs_vector'
        type(element), allocatable  :: pltcrvs(:)  !curves in one plot call
        integer:: nx
        integer:: ny
        integer:: ns
        integer:: number_of_curves
        integer:: i
        integer:: j
        integer:: ierr
        character(len=80), allocatable ::  pltstring(:), lst(:)

        !*******************************************************************************
        !   Check the input
        nx=size(xv)
        ny=size(ymat,dim=1)
        if (.not. checkdim(nx,ny)) then
            call gp_error(bad_array_size,md_name, proc_name, &
                'length of xv is not equal to row number in ymat')
        end if


        ! Process linespec
        number_of_curves=size(ymat,dim=2)
        allocate(pltstring(number_of_curves), stat=ierr)
        if (ierr /=0) then
            call gp_error(alloc_err,md_name,proc_name)
        end if


        ! assume no linespec is available
        pltstring(1:number_of_curves) = ''

        if ( present(lspec) ) then

            call splitstring2array(lspec,lst,';')
            ns = size(lst, dim=1)

            if (ns == number_of_curves) then
                ! there is a linespec for each curve
                pltstring = lst
            elseif (ns < number_of_curves) then
                ! not enough linespec
                do i=1, ns
                    pltstring(i) = lst(i)
                end do
                pltstring(ns+1:number_of_curves) = ''
            else ! ns > number_of curves
                print*, md_name//': plot2d_matrix_vs_vector: wrong number of linespec'
                print*, 'semicolon ";" acts as delimiter, check the linespec'
            end if
        end if


        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! allocate memory for the curves
        allocate(pltcrvs(number_of_curves), stat=ierr)
        if (ierr /=0) then
            call gp_error(alloc_err,md_name,proc_name,'allocation error in md_plot for pltcrvs')
        end if

        ! Write the linespec and x-y data into pltcrvs
        do j=1, number_of_curves
            pltcrvs(j)%linespec= trim(pltstring(j))
            call pltcrvs(j)%set_data(xv,ymat(:,j))
        end do


        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! Store data in curves linked list
        if (.not. hold_status) then             ! hold is off
            if (this%total_curves > 0) then     ! there is already some curves
                this%total_curves = 0           ! reset
                call this%curves%delete()       ! calear all previous curves
            end if
        end if

        !!!        if (this%total_curves == 0) then
        !!!            this%curves = list() ! create the list of curves
        !!!        end if
        do i=1, number_of_curves !append curves
            call  this%curves%add( pltcrvs(i) )
        end do


        this%total_curves = this%total_curves + number_of_curves

        !: End of plot2D_matrix_vs_vector
    end subroutine  plot2d_matrix_vs_vector





    !...............................................................................
    subroutine print_list(this)
        class(tpplot), intent(in) :: this
        ! TODO (Mohammad#1#02/11/18): Just for debug purpose. To be removed latter

        ! local variables
        type(element)  :: curve
        integer        :: i, j
        integer        :: n

        write(*,*) "Print the list of figure"
        !
        ! Loop over the list
        !
        n= this%curves%len()

        print '(a,I2)', 'Total number of curves: ', n

        do i=1, n
            call this%curves%get(i,curve)
            print '(a,I2)', "Curve ordinal number ", i
            print '(a)', "linespec: " // curve%linespec
            print '(a,I2)', "ndata: ", curve%ndata
            do j=1,   curve%ndata
                print '(I2,2x,G12.6,3x,G12.6)', j, curve%x(j), curve%y(j)
            end do
            print*, '-----------------------------------------'
        end do

    end subroutine print_list



    ! change on 2018-02-04
    subroutine write2scriptfile(this, file_unit)
        ! write2scriptfile: writes the plot settings and curves xy data into
        ! a script file indicated by file_unit

        class(tpplot) :: this
        integer, intent(in) :: file_unit
        !Local variables
        integer :: i, n
        type(element) :: curve
        character(len=80):: lststring



        if (this%hasplottitle) then
            write ( file_unit, '(a)' ) 'set title  "' // trim(this%txtplottitle)// '"'
        else ! to prevent previous title to be used
            write ( file_unit, '(a)' ) 'set title'
        end if
        if (this%hasxlabel) then
            write ( file_unit, '(a)' ) 'set xlabel "'// trim(this%txtxlabel)//'"'
        else ! to prevent previous xlabel to be used
            write ( file_unit, '(a)' ) 'set xlabel'
        end if
        if (this%hasylabel) then
            write ( file_unit, '(a)' ) 'set ylabel "'//trim(this%txtylabel)//'"'
        else ! to prevent previous ylabel to be used
            write ( file_unit, '(a)' ) 'set ylabel'
        end if

! FIXME (Mohammad#1#02/11/18): check to see if the setting can be reset

        ! Check with plot scale: i.e linear, logx, logy, or log xy
        select case (this%txtplotscale)
            case ('semilogx')
                write ( file_unit, '(a)' ) 'set logscale  x'
                write ( file_unit, '(a)' ) 'unset logscale  y'
            case ('semilogy')
                write ( file_unit, '(a)' ) 'set logscale  y'
                write ( file_unit, '(a)' ) 'unset logscale  x'
            case ('loglog')
                write ( file_unit, '(a)' ) 'set logscale  xy'
            case default !For linear xy plot or 3D plots
                write ( file_unit, '(a)' ) 'unset logscale  x'
                write ( file_unit, '(a)' ) 'unset logscale  y'
        end select

        ! set the x-axis and y-axis range
        if (this%hasxrange) then
            write ( file_unit, '(a,g0,a,g0,a)' ) 'set xrange [',this%xrange(1),':',this%xrange(2),']'
        end if

        if (this%hasyrange) then
            write ( file_unit, '(a,g0,a,g0,a)' ) 'set yrange [',this%yrange(1),':',this%yrange(2),']'
        end if

        ! write the plot data into script file
        n= this%curves%len() !totalnumber of curves
        if (n==1) then
            call this%curves%get(n,curve)
            call process_linespec(n, lststring, curve%linespec)
            write(file_unit, '(a)') trim(lststring)
        else
            do i=1, n-1
                call this%curves%get(i, curve)
                call process_linespec(i, lststring, curve%linespec)
                write ( file_unit, '(a)' ) trim(lststring) // '\'
            end do
            call this%curves%get(n, curve)
            call process_linespec(n, lststring, curve%linespec)
            write ( file_unit, '(a)' )   trim(lststring)
        end if

        ! write the xy data into script file
        do i=1, n
            call this%curves%get(i, curve)
            call write_xydata(file_unit, curve%ndata, &
                curve%x, curve%y)
        end do

    end subroutine write2scriptfile

    function set_lspec(lsn) result(ls)
        ! set_lspec
        character(len=*), intent(in), optional :: lsn
        character(len=:), allocatable :: ls
        if (present (lsn)) then
            ls = trim(lsn)
        else
            ls = ''
        end if

    end function set_lspec

end module md_plot

