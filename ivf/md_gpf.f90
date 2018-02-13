module md_gpf
    ! TODO (Mohammad#1#02/12/18): To add loglog loglogm, loglogv
    ! TODO (Mohammad#1#02/12/18): To add splot
    ! TODO (Mohammad#1#02/08/18): To add capabilty to add labels and options before any plot command
    ! FIXME (Mohammad#1#02/11/18): Add loglog, logxm, logym

    ! Done (Mohammad#1#02/12/18): semilogxm, semilogym
    ! Done (Mohammad#1#02/11/18): fplot
    ! Done (Mohammad#1#02/11/18): linespec for matrix plot

    use select_precision, only: wp
    use md_helperproc !, only: meshgrid, linspace !check this later
    use md_parameters
    use md_errhandling
    use md_figures

    implicit none
    character(len=*), parameter :: md_name='md_gpf'

    private
    public :: gpf
    public :: meshgrid, linspace, arange

    type gpf

        private
        type(linked_list)      :: figures ! a linked list of figures
        type(element), pointer :: actfig  ! a pointer to active figure
        logical  :: hold_status = .false. ! toggle or set the "hold" state of the plotting

        character(len=:), allocatable :: scriptfile   ! used for writing gnuplot scripts
        character(len=:), allocatable  :: txtoptions  ! general and session wide settings
        logical            :: hasoptions =      .false.

    contains

        private

        procedure, pass, private :: sub_make_vector_plot
        procedure, pass, private :: sub_make_matrix_plot
        procedure, pass, private :: is_exist_figure
        procedure, pass, private :: is_exist_plot

        procedure, pass, public :: figure    => sub_create_figure
        procedure, pass, public :: multiplot => sub_multiplot

        generic, public :: plot      => sub_make_vector_plot, sub_make_matrix_plot
        procedure, pass, public :: show      => sub_finalize_and_draw_plots
        procedure, pass, public :: holdon    => sub_holdon
        procedure, pass, public :: holdoff   => sub_holdoff

        procedure, pass, public :: xlabel  => sub_xlabel
        procedure, pass, public :: ylabel  => sub_ylabel
        procedure, pass, public :: title   => sub_plottitle
        procedure, pass, public :: axis    => sub_set_axis
        procedure, pass, public :: fplot   => sub_fplot
        procedure, pass, public :: options => sub_set_options


        procedure, pass, private :: semilogxv
        procedure, pass, private :: semilogxm
        procedure, pass, private :: semilogyv
        procedure, pass, private :: semilogym
        !        procedure, pass, private :: loglogv
        !        procedure, pass, private :: loglogm
        !
        !
        generic, public   :: semilogx        => semilogxv, semilogxm
        generic, public   :: semilogy        => semilogyv, semilogym
        !        generic, public   :: loglog          => loglogv, loglogm



    end type

contains


    !..............................................................................
    subroutine sub_set_options(this,stropt)
        !Set the general and session wide options for gnuplot

        class(gpf):: this
        character(len=*), intent(in) :: stropt

        if ( len_trim(stropt)>0 ) then
            this%txtoptions=trim(this%txtoptions)//';'//trim(stropt)
        end if

        this%hasoptions=.true.

    end subroutine sub_set_options




    !..............................................................................
    subroutine sub_fplot(this, func,xrange,np,lspec)
        !..............................................................................
        ! sub_fplot, plots a function in the range xrange=[xmin, xamx] with np points
        ! if np isnot sent, then np=50 is assumed!
        ! func is the name of function to be plotted

        class(gpf):: this
        interface
            function func(x)
                import :: wp
                real(wp), intent(in) :: x
                real(wp) :: func
            end function func
        end interface
        real(wp), intent(in) :: xrange(2)
        integer, optional, intent(in):: np
        character(len=*),  intent(in), optional :: lspec ! line specification

        ! local variables
        integer:: n
        integer:: i
        integer:: alloc_err
        real(wp), allocatable :: x(:)
        real(wp), allocatable :: y(:)

        if (present(np)) then
            n=np
        else
            n=50
        end if
        allocate(x(1:n), y(1:n), stat=alloc_err)
        if (alloc_err /=0) then
            call gp_error(alloc_err,md_name,'fplot','Allocation error in fplot procedure...')
        end if
        !Create set of xy data
        x=linspace(xrange(1),xrange(2), n)
        y=[ (func(x(i)), i=1, n) ]

        call this%plot(x1=x,y1=y,ls1=lspec)

        ! free up memory
        if (allocated(x)) deallocate(x)
        if (allocated(y)) deallocate(y)


    end subroutine sub_fplot



    !..............................................................................
    subroutine semilogxv(this, x1, y1, ls1, &
            x2, y2, ls2, &
            x3, y3, ls3, &
            x4, y4, ls4  )
        !..............................................................................
        !   This procedure is the same as plotXY with logarithmic x axis
        !..............................................................................
        class(gpf):: this
        ! Input vector
        real(wp),  intent(in)            :: x1(:)
        real(wp),  intent(in), optional  :: y1(:)
        character(len=*),  intent(in), optional   ::  ls1

        real(wp),  intent(in), dimension(:), optional  :: x2
        real(wp),  intent(in), dimension(:), optional  :: y2
        character(len=*),  intent(in), optional        :: ls2

        real(wp),  intent(in), dimension(:), optional  :: x3
        real(wp),  intent(in), dimension(:), optional  :: y3
        character(len=*),  intent(in), optional        :: ls3

        real(wp),  intent(in), dimension(:), optional  :: x4
        real(wp),  intent(in), dimension(:), optional  :: y4
        character(len=*),  intent(in), optional        :: ls4


        ! local variable
        integer :: pltno

        call this%plot(x1,y1,ls1,x2,y2,ls2,x3,y3,ls3,x4,y4,ls4)

        pltno = this%actfig%multiplot%loc
        call this%actfig%plots(pltno)%plotscale('semilogx')


    end subroutine semilogxv



    SUBROUTINE  semilogxm(this, xv,ymat, lspec)
    !..............................................................................
    !Plots a matrix against a vector with logarithmic x-axis
    !For more information see plot2D_matrix_vs_vector procedure
    !Everything is the same except the x-axis scale
    !..............................................................................

    IMPLICIT NONE
    CLASS(gpf):: this
    ! Input arrays
    REAL(wp),  INTENT(IN)    :: xv(:)
    REAL(wp),  INTENT(IN)    :: ymat(:,:)
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   :: lspec


      ! local variable
        integer :: pltno
        call sub_make_matrix_plot(this,xv,ymat,lspec)


        pltno = this%actfig%multiplot%loc
        call this%actfig%plots(pltno)%plotscale('semilogx')


    End SUBROUTINE semilogxm


    SUBROUTINE  semilogym(this, xv,ymat, lspec)
    !..............................................................................
    !Plots a matrix against a vector with logarithmic x-axis
    !For more information see plot2D_matrix_vs_vector procedure
    !Everything is the same except the x-axis scale
    !..............................................................................

    IMPLICIT NONE
    CLASS(gpf):: this
    ! Input arrays
    REAL(wp),  INTENT(IN)    :: xv(:)
    REAL(wp),  INTENT(IN)    :: ymat(:,:)
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   :: lspec


      ! local variable
        integer :: pltno
        call sub_make_matrix_plot(this,xv,ymat,lspec)


        pltno = this%actfig%multiplot%loc
        call this%actfig%plots(pltno)%plotscale('semilogy')


    End SUBROUTINE semilogym


    !..............................................................................
    subroutine semilogyv(this, x1, y1, ls1, &
            x2, y2, ls2, &
            x3, y3, ls3, &
            x4, y4, ls4  )
        !..............................................................................
        !   This procedure is the same as plotXY with logarithmic x axis
        !..............................................................................
        class(gpf):: this
        ! Input vector
        real(wp),  intent(in)            :: x1(:)
        real(wp),  intent(in), optional  :: y1(:)
        character(len=*),  intent(in), optional   ::  ls1

        real(wp),  intent(in), dimension(:), optional  :: x2
        real(wp),  intent(in), dimension(:), optional  :: y2
        character(len=*),  intent(in), optional        :: ls2

        real(wp),  intent(in), dimension(:), optional  :: x3
        real(wp),  intent(in), dimension(:), optional  :: y3
        character(len=*),  intent(in), optional        :: ls3

        real(wp),  intent(in), dimension(:), optional  :: x4
        real(wp),  intent(in), dimension(:), optional  :: y4
        character(len=*),  intent(in), optional        :: ls4


        ! local variable
        integer :: pltno

        call this%plot(x1,y1,ls1,x2,y2,ls2,x3,y3,ls3,x4,y4,ls4)

        pltno = this%actfig%multiplot%loc
        call this%actfig%plots(pltno)%plotscale('semilogy')


    end subroutine semilogyv


    subroutine sub_xlabel(this, strlabel)
        ! sub_xlabel
        class(gpf) :: this
        character(len=*) :: strlabel

        ! local variable
        integer :: pltno


        if ( this%is_exist_plot() ) then
            pltno = this%actfig%multiplot%loc
            call this%actfig%plots(pltno)%xlabel(strlabel)
        else
            call gp_error(not_found,md_name,'sub_xlabel','No active plot found!')
        end if


    end subroutine sub_xlabel


    subroutine sub_ylabel(this, strlabel)
        ! sub_xlabel
        class(gpf) :: this
        character(len=*) :: strlabel

        ! local variable
        integer :: pltno

        if ( this%is_exist_plot() ) then
            pltno = this%actfig%multiplot%loc
            call this%actfig%plots(pltno)%ylabel(strlabel)
        else
            call gp_error(not_found,md_name,'sub_ylabel','No active plot found!')
        end if



    end subroutine sub_ylabel


    !..............................................................................
    subroutine sub_plottitle(this, string)
        !..............................................................................
        !Set the plot title
        class(gpf) :: this
        character(len=*) :: string

        ! local variable
        integer :: pltno


        if ( this%is_exist_plot() ) then
            pltno = this%actfig%multiplot%loc
            call this%actfig%plots(pltno)%title(string)
        else
            call gp_error(not_found,md_name,'sub_plottitle','No active plot found!')
        end if

    end subroutine sub_plottitle


    !..............................................................................
    subroutine sub_set_axis(this,rng)
        !..............................................................................
        !Set the z label
        class(gpf):: this
        real(wp), intent(in) :: rng(:)


        ! local variable
        integer :: pltno

        pltno = this%actfig%multiplot%loc
        call this%actfig%plots(pltno)%axis(rng)

    end subroutine sub_set_axis




    !..............................................................................
    subroutine sub_create_figure(this, fig_title, term_type, term_size)
        ! Create a new figure and append it to the linked_list of
        ! figures

        class(gpf)                             :: this
        character(len=*), intent(in), optional :: fig_title
        character(len=*), intent(in), optional :: term_type
        integer,          intent(in), optional :: term_size(2)

        ! local variables
        type(element) :: new_fig
        integer :: inode

        !!        integer :: ierr,
        integer :: term_number

        ! create a new figure
        term_number = this%figures%len()  ! starts from zero
        call new_fig%sub_constructor(fig_title, term_type, term_size, term_number)

        !append new figure to the linked_list of figures
        call this%figures%add(new_fig)

        ! set a pointer to the active figure
        inode = this%figures%len()
        call this%figures%getp(inode, this%actfig)

        ! FIXME (Mohammad#1#02/08/18): add the debug options for logging latter
        print*, 'Figure number: ', inode

    end subroutine sub_create_figure




    !..............................................................................
    subroutine sub_make_vector_plot( this, &
            x1, y1, ls1,            &
            x2, y2, ls2,            &
            x3, y3, ls3,            &
            x4, y4, ls4  )
        !..............................................................................
        ! This procedure plots:
        !   1. A vector against another vector (xy plot)
        !   2. A vector versus its element indices.
        !   3. Can accept up to 4 data sets as x,y pairs!
        ! Arguments
        ! xi, yi vectors of data series,
        ! lsi a string maximum 80 characters containing the line specification, legends, ...

        class(gpf):: this
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



        ! check if at least a figure and multiplot layout is exist
        call this%is_exist_figure()

        associate(mplt => this%actfig%multiplot)

            if ( .not. (this%hold_status) ) then  ! hold is off, so start a new plot in a new space
                mplt%loc = mplt%loc + 1

                ! check if the number of drawn plots is more than the number of spaces in multiplot
                if (mplt%loc > mplt%rows * mplt%cols) then ! rewind, start from one
                    mplt%loc = 1
                    mplt%overwriten = .true.
                    print*, "md_gpf: sub_make_plot Many plot and greater than number of subplots"
                    print*, 'it is rewinded'
                    print*, 'current plot location:', mplt%loc
                end if
            else ! hold is on
                if (mplt%loc == 0) then ! this is the first plot
                    mplt%loc=1
                end if

            end if

            ! Debug (Mohammad#1#02/08/18): remove later

            print*, 'mplt%loc: ', mplt%loc

            call this%actfig%plots(mplt%loc)%plot( &
                x1, y1, ls1, &
                x2, y2, ls2, &
                x3, y3, ls3, &
                x4, y4, ls4, this%hold_status)

        end associate

    end subroutine sub_make_vector_plot


    !..............................................................................
    subroutine sub_make_matrix_plot( this, xv, ymat, lspec)
        !..............................................................................

        class(gpf):: this
        ! Input arrays
        real(wp),  intent(in)                       :: xv(:)
        real(wp),  intent(in)                       :: ymat(:,:)
        character(len=*),  intent(in), optional     :: lspec


        ! check for existance of figure and multiplot layout
        call this%is_exist_figure()


        associate(mplt => this%actfig%multiplot)

            if ( .not. (this%hold_status) ) then  ! hold is off, so start a new plot in a new space
                mplt%loc = mplt%loc + 1

                ! check if the number of drawn plots is more than the number of spaces in multiplot
                if (mplt%loc > mplt%rows * mplt%cols) then ! rewind, start from one
                    mplt%loc = 1
                    mplt%overwriten = .true.
                    print*, "md_gpf: sub_make_plot Many plot and greater than number of subplots"
                    print*, 'it is rewinded'
                    print*, 'current plot location:', mplt%loc
                end if
            else ! hold is on
                if (mplt%loc == 0) then ! this is the first plot
                    mplt%loc=1
                end if

            end if

            ! Debug (Mohammad#1#02/08/18): remove later

            print*, 'mplt%loc: ', mplt%loc

            call this%actfig%plots(mplt%loc)%plot( &
                xv, ymat, lspec, this%hold_status)

        end associate

    end subroutine sub_make_matrix_plot



    subroutine is_exist_figure(this)
        ! is_exist_figure
        ! checks existance of a figure and a multiplot layout
        ! if there is not such objects, they will be created!
        ! this subroutine help to start plotting directly by plot
        ! command whithout the need to create a figure or multiplot
        ! layout manually
        class(gpf) :: this

        ! check if there exist at least one figure
        ! if not create it
        if (this%figures%len() < 1) then ! there is no figure
            ! add a figure
            call this%figure()
        end if

        ! check if there exist a multiplot layout
        ! if not create a 1x1 layout
        if (.not. (this%actfig%multiplot%set)) then ! there is no multiplot layout
            ! create a 1x1 layout
            call this%multiplot(1,1)
        end if


    end subroutine is_exist_figure


    function is_exist_plot(this) result(flag)
        ! is_exist_plot
        ! checks the existance of an active plot
        class(gpf) :: this
        logical :: flag

        flag = .true.

        ! check if there exist at least one figure
        if (this%figures%len() < 1) then
            ! there is no active figure yet
            flag = .false.
            return
        end if

        ! check if there exist a multiplot layout
        if (.not. (this%actfig%multiplot%set)) then
            ! there is no multiplot layout
            flag = .false.
            return
        end if

        ! check if there is an active plot
        if ( this%actfig%multiplot%loc < 1) then
            ! there is no plot yet
            flag = .false.
        end if



    end function is_exist_plot



    !..............................................................................
    subroutine sub_multiplot(this, rows, cols)
        !..............................................................................

        ! This subroutine sets flag and number of rows and columns in case
        ! of multiplot layout

        class(gpf):: this
        integer, intent(in) :: rows
        integer, intent(in) :: cols


        ! check to see if there is any active figure
        ! if not create one. This is useful when user
        ! does not create a figure and assume there is an open
        ! figure
        if (this%figures%len() < 1) then ! there is no figure
            ! add a figure
            call this%figure()
        end if

        ! set multiplot cols and rows
        call this%actfig%set_multiplot_rows(rows)
        call this%actfig%set_multiplot_cols(cols)


        !Allocate new plots
        call this%actfig%sub_allocate_plots(rows*cols)
        ! set the multiplot layout flag
        this%actfig%multiplot%set = .true.


    end subroutine sub_multiplot



    subroutine sub_holdon(this)
        ! Set the plot hold status to on
        ! gp will keep the axes for further plots

        class(gpf)                    :: this

        this%hold_status = .true.

    end subroutine sub_holdon


    subroutine sub_holdoff(this)
        ! Set the plot hold status to off
        ! gp will clear the axes for new plots

        class(gpf)                    :: this

        this%hold_status = .false.

    end subroutine sub_holdoff


    !..............................................................................
    subroutine sub_finalize_and_draw_plots(this)
        !..............................................................................
        !This subroutine writes all the plots and settings into a txt file and
        !call gnuplot program to create and draw the plots
        class(gpf):: this

        ! local variables
        integer ::i, j ! these are counters
        integer :: file_unit
        integer :: nf, np
        type(element) :: fig


        !00. create output file
        this%scriptfile  = gnuplot_scriptfile_name //'00'//gnuplot_file_extension
        call create_outputfile(this%scriptfile, file_unit)

        !! Some general settings
        !! Remove later
!TODO (Mohammad#1#02/12/18): Remove later and substitute with general settings
        write(file_unit,'(a)') 'set style data linespoints'



        !00. write the session wide gnuplot options
        if (this%hasoptions) then
            call writestring(file_unit, this%txtoptions)
        end if

        ! get the number of figures
        nf = this%figures%len()

        do i=1, nf
            call this%figures%get(i, fig)
            !01. write multiplot setting

            ! Writing the figure data
            ! write terminal setting
            call fig%sub_write_term_setting(file_unit)


            if (fig%multiplot%set) then  ! there is multiplot layout
                call fig%sub_write_multiplot_set_unset(file_unit, setflag= .true.)

                if (fig%multiplot%overwriten) then
                    np = fig%multiplot%rows * fig%multiplot%cols
                else
                    np = fig%multiplot%loc
                end if
            else ! there is no multiplot layout
                np=1
            end if

            ! write each plot data into script file
            do j =1, np
                call fig%plots(j)%write2scriptfile(file_unit)
            end do

            !xx. unset multiplot
            if (fig%multiplot%set) then  ! there is multiplot layout
                call fig%sub_write_multiplot_set_unset(file_unit, setflag= .false.)
            end if

        end do


        !yy. close script file
        close (file_unit)
        !zz. call the gnuplot
        call execute_command_line ('gnuplot '//this%scriptfile // ' -persist ')

    end subroutine sub_finalize_and_draw_plots


end module md_gpf

