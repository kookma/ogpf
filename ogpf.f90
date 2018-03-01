!-------------------------------------------------------------------------------
!    GnuPlot Interface
!-------------------------------------------------------------------------------
!    Purpose:   Object Based Interface to GnuPlot from Fortran (ogpf)
!    Platform:  Windows XP/Vista/7/10
!               (It should work on other platforms, see the finalize_plot subroutine below)
!    Language:  Fortran 2003 and 2008
!    Requires:  1. Fortran 2003 compiler (e.g gfortran 5, IVF 12.1, ...)
!                  There is only two more features needs Fortran 2008 standard
!                  execute_command_line and passing internal function as argument.
!               2. gnuplot 5 and higher (other previous version can be used
!    Author:    Mohammad Rahmani
!               Chem Eng Dep., Amirkabir Uni. of Tech
!               Tehran, Ir
!               url:    aut.ac.ir/m.rahmani
!               github: github.com/kookma
!               email:  m[dot]rahmani[at]aut[dot]ac[dot]ir

! Revision History


! Revision:  0.20
! Date:     Feb 20th, 2018
!  - ogpf now supports animation for 2D and 3D plots
!  - rewrite contour and surface plot
!  - select_precision has been merged into ogpf
!  - new add_script procedure replaced old script
!  - new run_script procedure
!  - writestring procedure removed
!  - linespec for plor2d_matrix_vs_plot now is a single dynamic string
!  - splot now uses datablok instead of inline data
!  - meshgrid now support full grid vector
!  - arange a numpy similar function to create a range in the form of [xa, xa+dx, xa+2*dx, ...]



! Revision:  0.19
! Date:     Jan 15th, 2018
!  - new contour plot procedure


! Revision:  0.18
! Date:     Dec 22th, 2017
!   Major revision
!   - The dynamic string allocation of Fortran 2003 is used (some old compilers
!     does not support this capability)
!   - Multiple windows plot now supported
!   - Multiplot now supported
!   - Gnuplot script file extension is changed from .plt to .gp
!   - Default window size (canvas) changed to 640x480
!   - Persist set to on (true) by default
!   - A separate subroutine is used now to create the output file for gnuplot commands
!   - A separate subroutine is used now to finalize the output

!


! Revision:  0.17
! Date:     Dec 18th, 2017
!   Minor corrections
!   - Correct the meshgrid for wrong dy calculation when ygv is sent by two elements.
!   - Remove the subroutine ErrHandler (development postponed to future release)


! Revision:  0.16
! Date:     Feb 11th, 2016
!   Minor corrections
!   Correct the lspec processing in plot2D_matrix_vs_vector
!   Now, it is possible to send less line specification and gpf will cycle through lspec

! Revision:  0.15
! Date:     Apr 20th, 2012
!   Minor corrections
!   Use of select_precision module and working precision: wp

! Revision:  0.14
! Date:     Mar 28th, 2012
!   Minor corrections
!   Use of import keyboard and removing the Precision module
!   Length of Title string increased by 80 chars


! Revision:  0.13
! Date:     Feb 12th, 2012
!   Minor corrections
!   Added axis method which sets the axis limits for x-axis, y-axis and z-axis
!   Added Precision module



! Version:  0.12
! Date:     Feb 9th, 2012
!   Minor corrections
!   New semilogx, semilogy, loglog methods
!   New options method, allow to be called several times to set the gnuplot options



! Version:  0.11
! Date:     Feb 9th, 2012
!   Minor corrections
!   Use of NEWUINT specifier from Fortran 2008
!   Added configuration parameters
!   Extra procedures have been removed
!   Temporary file is now deleted using close(...,status='delete')

!
! Version:  0.1
! Date:     Jan 5th, 2012
! First object-based version

module ogpf

    implicit none

    private
    public arange, linspace, meshgrid, wp

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! select precision
    !
    !     sp: kind for single precision
    !     dp: kind for double precision
    !
    !     wp: kind for working precision (set to either sp or dp)
    integer, parameter :: sp = kind( 1.0 )
    integer, parameter :: dp = kind( 1.0d0 )

    integer, parameter :: wp = dp
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    !
    character(len=*), parameter :: md_name = 'ogpf libray'
    character(len=*), parameter :: md_rev  = 'Rev. 0.2 of Feb 22, 2018'
    character(len=*), parameter :: md_lic  = 'Licence: MIT'

    ! Configuration parameters
    ! The terminal and font have been set for Windows operating system
    ! Correct to meet the requirements on other OS like Linux and Mac.
    character(len=*), parameter ::  gnuplot_term_type= 'wxt'                      ! Output terminal
    character(len=*), parameter ::  gnuplot_term_font= 'verdana,10'               ! font
    character(len=*), parameter ::  gnuplot_term_size= '320,240'                  ! plot window size
    character(len=*), parameter ::  gnuplot_output_filename='ogpf_temp_script.gp' ! temporary file for output

    integer                     ::  file_unit                                     ! file unit identifier


    type, public :: gpf
        ! the gpf class

        private

        character(len=:), allocatable  :: txtplottitle
        character(len=:), allocatable  :: txtxlabel
        character(len=:), allocatable  :: txtylabel
        character(len=:), allocatable  :: txtzlabel
        character(len=:), allocatable  :: txtoptions
        character(len=:), allocatable  :: txtscript
        character(len=:), allocatable  :: txtdatastyle ! 2D: linespoints, 3D: lines

        logical :: hasplottitle   = .false.
        logical :: hasxlabel      = .false.
        logical :: hasylabel      = .false.
        logical :: haszlabel      = .false.
        logical :: hasxrange      = .false.
        logical :: hasyrange      = .false.
        logical :: haszrange      = .false.
        logical :: hasoptions     = .false.
        logical :: hasanimation   = .false.

        real(wp)           :: xrange(2), yrange(2), zrange(2)

        integer, public    :: pause_seconds = 0  ! keep plot on screen for this value in seconds

        character(len=:), allocatable   :: msg      !Message from plot procedures
        integer                         :: status=0 !Status from plot procedures
        character(len=25)               :: txtfilename=gnuplot_output_filename
        character(len=8)                :: plotscale

        integer :: frame_number

    contains

        private

        procedure, pass, private :: plot2d_vector_vs_vector
        procedure, pass, private :: plot2d_matrix_vs_vector

        procedure, pass, private :: semilogxv
        procedure, pass, private :: semilogxm
        procedure, pass, private :: semilogyv
        procedure, pass, private :: semilogym
        procedure, pass, private :: loglogv
        procedure, pass, private :: loglogm

        ! public procedures
        procedure, pass, public :: options      => set_options
        procedure, pass, public :: title        => set_plottitle
        procedure, pass, public :: xlabel       => set_xlabel
        procedure, pass, public :: ylabel       => set_ylabel
        procedure, pass, public :: zlabel       => set_zlabel
        procedure, pass, public :: axis         => set_axis
        procedure, pass, public :: filename     => set_filename
        procedure, pass, public :: reset        => reset_to_defaults

        generic, public         :: plot       => plot2d_vector_vs_vector, plot2d_matrix_vs_vector
        generic, public         :: semilogx   => semilogxv, semilogxm
        generic, public         :: semilogy   => semilogyv, semilogym
        generic, public         :: loglog     => loglogv, loglogm
        procedure, pass, public :: surf       => splot
        procedure, pass, public :: contour    => cplot
        procedure, pass, public :: fplot      => function_plot
        procedure, pass, public :: add_script => addscript
        procedure, pass, public :: run_script => runscript
        !Rev 0.20
        procedure, pass, public :: animation_start => sub_animation_start
        procedure, pass, public :: animation_show  => sub_animation_show


    end type gpf


contains

    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section One: Set/Get Methods for ogpf object
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    subroutine set_axis(this,rng)
        !..............................................................................
        !Set the z label
        !..............................................................................

        class(gpf):: this
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
            case(6)
                this%hasxrange=.true.
                this%hasyrange=.true.
                this%haszrange=.true.
                this%xrange=rng(1:2)
                this%yrange=rng(3:4)
                this%zrange=rng(5:6)
            case default
                print*, 'gpf error: wrong axis range setting!'
                return
        end select

    end subroutine set_axis


    subroutine set_filename(this,string)
        !..............................................................................
        !Set a file name for plot command output
        !This file can be used later by gnuplot as an script file to reproduce the plot
        !..............................................................................

        class(gpf):: this
        character(len=*), intent(in) :: string

        this%txtfilename=trim(string)

    end subroutine set_filename


    subroutine set_options(this,stropt)
        !..............................................................................
        !Set the plot options
        !..............................................................................

        class(gpf):: this
        character(len=*), intent(in) :: stropt

        if (len_trim(this%txtoptions) == 0 ) then
            this%txtoptions = '' ! initialize string
        end if
        if ( len_trim(stropt)>0 ) then
            this%txtoptions = this%txtoptions // splitstr(stropt)
        end if

        this%hasoptions=.true.

    end subroutine set_options



    subroutine set_plottitle(this,string)
        !..............................................................................
        !Set the plot title
        !..............................................................................

        class(gpf):: this
        character(len=*), intent(in) :: string
        this%txtplottitle=trim(string)
        this%hasplottitle=.true.

    end subroutine set_plottitle


    subroutine set_xlabel(this,string)
        !..............................................................................
        !Set the xlabel
        !..............................................................................
        class(gpf):: this
        character(len=*), intent(in) :: string
        this%txtxlabel=trim(string)
        this%hasxlabel=.true.

    end subroutine set_xlabel


    subroutine set_ylabel(this,string)
        !..............................................................................
        !Set the ylabel
        !..............................................................................
        class(gpf):: this
        character(len=*), intent(in) :: string
        this%txtylabel=trim(string)
        this%hasylabel=.true.

    end subroutine set_ylabel


    subroutine set_zlabel(this,string)
        !..............................................................................
        !Set the z label
        !..............................................................................
        class(gpf):: this
        character(len=*), intent(in) :: string
        this%txtzlabel=trim(string)
        this%haszlabel=.true.

    end subroutine set_zlabel



    subroutine reset_to_defaults(this)
        !..............................................................................
        !Reset all ogpf properties (params to their default values
        !...............................................................................
        class(gpf):: this

        this%txtplottitle   = ""
        this%txtxlabel      = ""
        this%txtylabel      = ""
        this%txtoptions     = ""
        this%txtfilename    = gnuplot_output_filename

        this%hasoptions     = .false.
        this%hasplottitle   = .false.
        this%hasxlabel      = .false.
        this%hasylabel      = .false.
        this%haszlabel      = .false.
        this%hasxrange      = .false.
        this%hasyrange      = .false.
        this%haszrange      = .false.

    end subroutine reset_to_defaults


    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Two: Main Plotting Routines
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    subroutine plot2d_vector_vs_vector(this, x1, y1, ls1, &
            x2, y2, ls2, &
            x3, y3, ls3, &
            x4, y4, ls4  )
        !..............................................................................
        ! This procedure plots:
        !   1. A vector against another vector (xy plot)
        !   2. A vector versus its element indices.
        !   3. Can accept up to 4 data sets as x,y pairs!
        ! Arguments
        ! xi, yi vectors of data series,
        ! lsi a string maximum 80 characters containing the line specification, legends, ...
        !..............................................................................
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

        !   Local variables
        !----------------------------------------------------------------------

        integer:: nx1
        integer:: ny1
        integer:: nx2
        integer:: ny2
        integer:: nx3
        integer:: ny3
        integer:: nx4
        integer:: ny4
        integer::           number_of_plots
        character(len=3)::  plottype
        integer:: i
        character(len=80)::  pltstring(4)  !Four 80 character lines

        !Initialize variables
        plottype=''
        pltstring=''
        !   Check the input
        nx1=size(x1)
        if ((present(y1) )) then
            ny1=size(y1)
            if (checkdim(nx1,ny1)) then
                plottype='xy1'
                number_of_plots=1
            else
                print*, 'gpf error: length of x1 and y1 doesnot match'
                return
            end if
        else !plot only x againest its element indices
            plottype='xi'
            number_of_plots=1
        end if

        !Process line spec for first data set if present
        call process_linespec(1, pltstring(1),ls1)

        if (present(x2) .and. present (y2)) then
            nx2=size(x2)
            ny2=size(y2)
            if (checkdim(nx2,ny2)) then
                plottype='xy2'
                number_of_plots=2
            else
                return
            end if
            !Process line spec for 2nd data set if present
            call process_linespec(2, pltstring(2),ls2)
        end if

        if (present(x3) .and. present (y3)) then
            nx3=size(x3)
            ny3=size(y3)
            if (checkdim(nx3,ny3)) then
                plottype='xy3'
                number_of_plots=3
            else
                return
            end if
            !Process line spec for 3rd data set if present
            call process_linespec(3, pltstring(3),ls3)
        end if

        if (present(x4) .and. present (y4)) then
            nx4=size(x4)
            ny4=size(y4)
            if (checkdim(nx4,ny4)) then
                plottype='xy4'
                number_of_plots=4
            else
                return
            end if
            !Process line spec for 4th data set if present
            call process_linespec(4, pltstring(4),ls4)
        end if


        !> Rev 0.2 animation

        if (.not. (this%hasanimation) ) then
            ! create the ouput file for writting gnuplot script
            call create_outputfile(this, file_unit)
        else
            this%frame_number = this%frame_number + 1
        end if

        ! set default line style for 2D plot, can be overwritten
        this%txtdatastyle = 'linespoints'

        ! Write plot title, axis labels and other annotations
        call processcmd(this, file_unit)

        ! Write plot command and line styles and legend if any
        if (number_of_plots ==1) then
            write ( file_unit, '(a)' )  trim(pltstring(1))
        else
            write ( file_unit, '(a)' )  ( trim(pltstring(i)) // ' \' , i=1, number_of_plots-1)
            write ( file_unit, '(a)' )  trim(pltstring(number_of_plots))
        end if
        ! Write xy data into file
        select case (plottype)
            case ('xi')
                call write_xydata(file_unit,nx1,x1)
            case ('xy1')
                call write_xydata(file_unit,nx1,x1,y1)
            case ('xy2')
                call write_xydata(file_unit,nx1,x1,y1)
                call write_xydata(file_unit,nx2,x2,y2)
            case ('xy3')
                call write_xydata(file_unit,nx1,x1,y1)
                call write_xydata(file_unit,nx2,x2,y2)
                call write_xydata(file_unit,nx3,x3,y3)
            case ('xy4')
                call write_xydata(file_unit,nx1,x1,y1)
                call write_xydata(file_unit,nx2,x2,y2)
                call write_xydata(file_unit,nx3,x3,y3)
                call write_xydata(file_unit,nx4,x4,y4)
        end select

        !> Rev 0.2
        ! if there is no animation finalize
        if (.not. (this%hasanimation)) then
            call finalize_plot(this, file_unit)
        else
            write(file_unit, '(a, I2)') 'pause ', this%pause_seconds
        end if


        !: End of plot2D_vector_vs_vector
    end subroutine plot2d_vector_vs_vector



    subroutine  plot2d_matrix_vs_vector(this, xv,ymat, lspec)
        !..............................................................................
        !plot2D_matrix_vs_vector accepts a vector xv and a matrix ymat and plots columns of ymat against xv
        !lspec is an optional array defines the line specification for each data series
        !If a single element array is sent for lspec then all series are plotted using the same
        !linespec

        implicit none
        class(gpf):: this
        ! Input arrays
        real(wp),  intent(in)                       :: xv(:)
        real(wp),  intent(in)                       :: ymat(:,:)
        character(len=*),  intent(in), optional     :: lspec
        !----------------------------------------------------------------------
        !       Local variables
        integer:: nx
        integer:: ny
        integer:: ns
        integer:: number_of_curves
        integer::  i
        integer:: j
        integer:: ierr
        character(len=80), allocatable ::  pltstring(:), lst(:)
        !

        !*******************************************************************************
        !   Check the input
        nx=size(xv)
        ny=size(ymat,dim=1)
        if (.not. checkdim(nx,ny)) then
            print*, 'gpf error: The length of arrays does not match'
            return
        end if

        !> Rev 0.2 animation

        if (.not. (this%hasanimation) ) then
            ! create the ouput file for writting gnuplot script
            call create_outputfile(this, file_unit)
        else
            this%frame_number = this%frame_number + 1
        end if

        ! set default line style for 2D plot, can be overwritten
        this%txtdatastyle = 'linespoints'

        ! Write titles and other annotations
        call processcmd(this, file_unit)

        ! Write plot command and line styles and legend if any
        number_of_curves=size(ymat,dim=2)
        allocate(pltstring(number_of_curves), stat=ierr)
        if (ierr /=0) then
            print*, 'allocation error'
            return
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
            else ! ns > number_of curves
                print*, 'ogpf: plot2d_matrix_vs_vector: wrong number of linespec'
                print*, 'semicolon ";" acts as delimiter, check the linespec'
            end if
        end if

        if ( present(lspec) ) then

            call process_linespec(1,pltstring(1),lst(1))
            ns=size(lst)
            ! gpf will cylce through line specification, if number of specification passed
            ! is less than number of plots
            do i=1, number_of_curves
                j=mod(i-1, ns) + 1
                call process_linespec(i, pltstring(i), lst(j))
            end do
        else !No lspec is available
            pltstring(1)=' plot "-" notitle,'
            pltstring(2:number_of_curves-1)='"-" notitle,'
            pltstring(number_of_curves)='"-" notitle'
        end if

        ! Write plot command and line styles and legend if any
        write ( file_unit, '(a)' ) ( trim(pltstring(i)) // ' \' , i=1, number_of_curves-1)
        write ( file_unit, '(a)' )   trim(pltstring(number_of_curves))

        ! Write data into script file
        do j=1, number_of_curves
            do i = 1, nx
                write ( file_unit, * ) xv(i),ymat(i,j)
            end do
            write ( file_unit, '(a)' ) 'e'  !end of jth set of data
        end do




        !> Rev 0.2
        ! if there is no animation finalize
        if (.not. (this%hasanimation)) then
            call finalize_plot(this, file_unit)
        else
            write(file_unit, '(a, I2)') 'pause ', this%pause_seconds
        end if

        !Release memory
        if (allocated(pltstring)) then
            deallocate(pltstring)
        end if
        !: End of plot2D_matrix_vs_vector
    end subroutine  plot2d_matrix_vs_vector


    subroutine splot(this, x, y, z, lspec, palette)
        !..............................................................................
        ! splot create a surface plot
        ! Rev 0.19: Feb 16, 2018
        ! datablock is used instead of  gnuplot inline file "-"
        class(gpf):: this
        ! Input vector
        real(wp),  intent(in)            :: x(:,:)
        real(wp),  intent(in), optional  :: y(:,:)
        real(wp),  intent(in), optional  :: z(:,:)
        character(len=*),  intent(in), optional   ::  lspec
        character(len=*),  intent(in), optional   ::  palette

        !   Local variables
        !----------------------------------------------------------------------
        integer:: ncx
        integer:: nrx
        integer:: i
        integer:: j
        logical:: xyz_data
        character(len=80)::  pltstring
        character(len=*), parameter ::  datablock = '$xyz'

        pltstring=''
        !   Check the input data
        ncx=size(x,dim=2)
        nrx=size(x,dim=1)
        if (present(y) .and. present(z)) then
            xyz_data=.true.
        elseif (present(y)) then
            print*, "gpf error: Z matrix was not sent to 3D plot routine"
            return
        else
            xyz_data=.false.
        end if

        !> Rev 0.2 animation

        if (.not. (this%hasanimation) ) then
            ! create the ouput file for writting gnuplot script
            call create_outputfile(this, file_unit)
        else
            this%frame_number = this%frame_number + 1
        end if

        ! set default line style for 3D plot, can be overwritten
        this%txtdatastyle = 'lines'

        ! Write titles and other annotations
        call processcmd(this, file_unit)

        ! Write xy data into file
        write ( file_unit, '(a)' ) '#data x y z'
        ! Rev 0.20
        ! write the $xyz datablocks
        write( file_unit, '(a)' )  datablock // ' << EOD'
        if (xyz_data) then
            do j=1,ncx
                do i=1, nrx
                    write ( file_unit, * ) x(i,j), y(i,j), z(i,j)
                enddo
                write( file_unit, '(a)' )  !put an empty line
            enddo
            write ( file_unit, '(a)' ) 'EOD'  !end of datablock
        else !only Z has been sent (i.e. single matrix data)
            do j=1,ncx
                do i=1, nrx
                    write ( file_unit, * ) i, j, x(i,j)
                enddo
                write( file_unit, '(a)' )  !put an empty line
            enddo
            write ( file_unit, '(a)' ) 'EOD'  !end of datablock
        end if


        !write the color palette into gnuplot script file
        if (present(palette)) then
            write ( file_unit, '(a)' )  color_palettes(palette)
            write ( file_unit, '(a)' )  'set pm3d' ! a conflict with lspec
        end if


        if ( present(lspec) ) then
            if (hastitle(lspec)) then
                pltstring='splot ' // datablock // ' ' // trim(lspec)
            else
                pltstring='splot ' // datablock // ' notitle '//trim(lspec)
            end if
        else
            pltstring='splot ' // datablock // ' notitle '
        end if

        write ( file_unit, '(a)' ) trim(pltstring)


        !> Rev 0.2: animation
        ! if there is no animation finalize
        if (.not. (this%hasanimation)) then
            call finalize_plot(this, file_unit)
        else
            write(file_unit, '(a, I2)') 'pause ', this%pause_seconds
        end if

        !: End of splot
    end subroutine splot


    subroutine cplot(this, x, y, z, lspec, palette)
        !..............................................................................
        !   Rev 0.19
        !   cplot creates a contour plot based on the three dimensional data

        class(gpf):: this
        ! Input vector
        real(wp),  intent(in)            :: x(:,:)
        real(wp),  intent(in), optional  :: y(:,:)
        real(wp),  intent(in), optional  :: z(:,:)
        character(len=*),  intent(in), optional   ::  lspec
        character(len=*),  intent(in), optional   ::  palette

        !   Local variables
        !----------------------------------------------------------------------

        integer:: ncx
        integer:: nrx
        integer:: i
        integer:: j
        logical:: xyz_data
        character(len=80)::  pltstring
        character(len=*), parameter ::  datablock  = '$xyz'
        !       character(len=*), parameter ::  cntr_table = '$xyz_contour'

        pltstring=''
        !   Check the input data
        ncx=size(x,dim=2)
        nrx=size(x,dim=1)
        if (present(y) .and. present(z)) then
            xyz_data=.true.
        elseif (present(y)) then
            print*, "gpf error: Z matrix was not sent to 3D plot routine"
            return
        else
            xyz_data=.false.
        end if

        !> Rev 0.2 animation

        if (.not. (this%hasanimation) ) then
            ! create the ouput file for writting gnuplot script
            call create_outputfile(this, file_unit)
        else
            this%frame_number = this%frame_number + 1
        end if

        ! set default line style for 3D plot, can be overwritten
        this%txtdatastyle = 'lines'

        ! Write titles and other annotations
        call processcmd(this, file_unit)


        ! Write xy data into file
        write ( file_unit, '(a)' ) '#data x y z'
        ! write the $xyz datablocks
        write( file_unit, '(a)' )  datablock // ' << EOD'
        if (xyz_data) then
            do j=1,ncx
                do i=1, nrx
                    write ( file_unit, fmt=* ) x(i,j), y(i,j), z(i,j)
                enddo
                write( file_unit, '(a)' )  !put an empty line
            enddo
            write ( file_unit, '(a)' ) 'EOD'  !end of datablock
        else !only Z has been sent (i.e. single matrix data)
            do j=1,ncx
                do i=1, nrx
                    write ( file_unit, fmt=* ) i, j, x(i,j)
                enddo
                write( file_unit, '(a)' )  !put an empty line
            enddo
            write ( file_unit, '(a)' ) 'EOD'  !end of datablock
        end if


        ! create the contour lines
        write ( file_unit, '(a)' ) ! empty line
        write ( file_unit, '(a)' ) '# create the contour'
        write ( file_unit, '(a)' ) 'set contour base'
        write ( file_unit, '(a)' ) 'set cntrparam levels 14'
        write ( file_unit, '(a)' ) 'unset surface'
        write ( file_unit, '(a)' ) 'set view map'


        !write the color palette into gnuplot script file
        if (present(palette)) then
            write ( file_unit, '(a)' )  color_palettes(palette)
            write ( file_unit, '(a)' )  'set pm3d' ! a conflict with lspec
        end if


        write ( file_unit, '(a)' ) ! empty line

        if ( present(lspec) ) then
            if (hastitle(lspec)) then
                pltstring='splot ' // datablock // ' ' // trim(lspec)
            else
                pltstring='splot ' // datablock // ' notitle '//trim(lspec)
            end if
        else
            pltstring='splot ' // datablock // ' notitle '
        end if

        write ( file_unit, '(a)' ) trim(pltstring)

        !> Rev 0.20
        ! if there is no animation finalize
        if (.not. (this%hasanimation)) then
            call finalize_plot(this, file_unit)
        else
            write(file_unit, '(a, I2)') 'pause ', this%pause_seconds
        end if

        !: End of cplot
    end subroutine cplot


    subroutine function_plot(this, func,xrange,np)
        !..............................................................................
        ! fplot, plot a function in the range xrange=[xmin, xamx] with np points
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
            stop "Allocation error in fplot procedure..."
        end if
        !Create set of xy data
        x=linspace(xrange(1),xrange(2), n)
        y=[ (func(x(i)), i=1, n) ]

        call plot2d_vector_vs_vector(this,x,y)

        ! cleanup memory
        if (allocated(x)) deallocate(x)
        if (allocated(y)) deallocate(y)


    end subroutine function_plot


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

        this%plotscale='semilogx'
        call plot2d_vector_vs_vector(this, x1, y1, ls1, x2, y2, ls2, x3, y3, ls3, x4, y4, ls4  )
        ! Set the plot scale as linear. It means log scale is off
        this%plotscale='linear'

    end subroutine semilogxv


    !..............................................................................
    subroutine semilogyv(this, x1, y1, ls1, &
            x2, y2, ls2, &
            x3, y3, ls3, &
            x4, y4, ls4  )
        !..............................................................................
        !   This procedure is the same as plotXY with logarithmic y axis
        !..............................................................................
        class(gpf):: this
        ! Input vector
        real(wp),  intent(in)                       :: x1(:)
        real(wp),  intent(in), optional             :: y1(:)
        character(len=*),  intent(in), optional     :: ls1

        real(wp),  intent(in), dimension(:), optional   :: x2
        real(wp),  intent(in), dimension(:), optional   :: y2
        character(len=*),  intent(in), optional         :: ls2

        real(wp),  intent(in), dimension(:), optional   :: x3
        real(wp),  intent(in), dimension(:), optional   :: y3
        character(len=*),  intent(in), optional         :: ls3

        real(wp),  intent(in), dimension(:), optional   :: x4
        real(wp),  intent(in), dimension(:), optional   :: y4
        character(len=*),  intent(in), optional         :: ls4

        this%plotscale='semilogy'
        call plot2d_vector_vs_vector(this, x1, y1, ls1, x2, y2, ls2, x3, y3, ls3, x4, y4, ls4  )
        ! Set the plot scale as linear. It means log scale is off
        this%plotscale='linear'


    end subroutine semilogyv



    subroutine loglogv(this, x1, y1, ls1, &
            x2, y2, ls2, &
            x3, y3, ls3, &
            x4, y4, ls4  )
        !..............................................................................
        !   This procedure is the same as plotXY with logarithmic xy axis
        !..............................................................................
        class(gpf):: this
        ! Input vector
        real(wp),  intent(in)            :: x1(:)
        real(wp),  intent(in), optional  :: y1(:)
        character(len=*),  intent(in), optional   ::  ls1

        real(wp),  intent(in), dimension(:), optional  :: x2
        real(wp),  intent(in), dimension(:), optional  :: y2
        character(len=*),  intent(in), optional       :: ls2

        real(wp),  intent(in), dimension(:), optional  :: x3
        real(wp),  intent(in), dimension(:), optional  :: y3
        character(len=*),  intent(in), optional       :: ls3

        real(wp),  intent(in), dimension(:), optional  :: x4
        real(wp),  intent(in), dimension(:), optional  :: y4
        character(len=*),  intent(in), optional   ::  ls4

        this%plotscale='loglog'
        call plot2d_vector_vs_vector(this, x1, y1, ls1, x2, y2, ls2, x3, y3, ls3, x4, y4, ls4  )
        ! Set the plot scale as linear. It means log scale is off
        this%plotscale='linear'

    end subroutine loglogv



    subroutine  semilogxm(this, xv,ymat, lspec)
        !..............................................................................
        !Plots a matrix against a vector with logarithmic x-axis
        !For more information see plot2D_matrix_vs_vector procedure
        !Everything is the same except the x-axis scale
        !..............................................................................

        implicit none
        class(gpf):: this
        ! Input arrays
        real(wp),  intent(in)    :: xv(:)
        real(wp),  intent(in)    :: ymat(:,:)
        character(len=*),  intent(in), optional   :: lspec

        this%plotscale='semilogx'
        call plot2d_matrix_vs_vector(this, xv,ymat, lspec)
        ! Set the plot scale as linear. It means log scale is off
        this%plotscale='linear'


    end subroutine semilogxm



    subroutine  semilogym(this, xv,ymat, lspec)
        !..............................................................................
        !Plots a matrix against a vector with logarithmic y-axis
        !For more information see plot2D_matrix_vs_vector procedure
        !Everything is the same except the x-axis scale
        !..............................................................................

        implicit none
        class(gpf):: this
        ! Input arrays
        real(wp),  intent(in)                       :: xv(:)
        real(wp),  intent(in)                       :: ymat(:,:)
        character(len=*),  intent(in), optional     :: lspec

        this%plotscale='semilogy'
        call plot2d_matrix_vs_vector(this, xv,ymat, lspec)
        ! Set the plot scale as linear. It means log scale is off
        this%plotscale='linear'


    end subroutine semilogym


    subroutine  loglogm(this, xv,ymat, lspec)
        !..............................................................................
        !Plots a matrix against a vector with logarithmic x-axis and y-axis
        !For more information see plot2D_matrix_vs_vector procedure
        !Everything is the same except the axes scale
        !..............................................................................

        implicit none
        class(gpf):: this
        ! Input arrays
        real(wp),  intent(in)    :: xv(:)
        real(wp),  intent(in)    :: ymat(:,:)
        character(len=*),  intent(in), optional   :: lspec

        this%plotscale='loglog'
        call plot2d_matrix_vs_vector(this, xv,ymat, lspec)
        ! Set the plot scale as linear. It means log scale is off
        this%plotscale='linear'


    end subroutine loglogm



    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Three: Animation Routines
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    subroutine sub_animation_start(this, delay)
        !-------------------------------------------------------------------------------
        ! sub_animation_start: set the setting to start an animation
        ! it simply set flags and open a script file to write data
        !-------------------------------------------------------------------------------
        class(gpf) :: this
        integer, intent(in), optional :: delay

        if (present(delay)) then
            this%pause_seconds = delay
        else
            this%pause_seconds = 2  ! delay in second
        end if

        this%frame_number = 0

        ! create the ouput file for writting gnuplot script
        call create_outputfile(this, file_unit)
        this%hasanimation = .true.

    end subroutine sub_animation_start


    subroutine sub_animation_show(this)
        !-------------------------------------------------------------------------------
        ! sub_animation_show: simply resets the animation flags
        ! and finalize the plotting.
        !-------------------------------------------------------------------------------

        class(gpf) :: this
        this%frame_number = 0
        this%hasanimation = .false.

        call finalize_plot(this, file_unit)

    end subroutine sub_animation_show




    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Four: Gnuplot direct scriptting
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    subroutine addscript(this,strcmd)
        !..............................................................................
        ! addscript: accepts all type of gnuplot command as a string and store it
        ! in global txtscript to be later sent to gnuplot
        !..............................................................................

        class(gpf):: this
        character(len=*), intent(in) :: strcmd
        ! Integer, Save :: strlength=0
        ! strlength=strlength+len_trim(stropt)

        if (len_trim(this%txtscript) == 0 ) then
            this%txtscript = '' ! initialize string
        end if
        if ( len_trim(strcmd)>0 ) then
            this%txtscript = this%txtscript // splitstr(strcmd)
        end if

    end subroutine addscript



    subroutine runscript(this)
        !..............................................................................
        ! runscript sends the the script string (txtstring) into a script
        ! file to be run by gnuplot
        !..............................................................................

        class(gpf):: this

        !REV 0.18: a dedicated subroutine is used to create the output file
        call create_outputfile(this, file_unit)

        ! set default line style, can be overwritten
        this%txtdatastyle = 'linespoints'
        !write the script
        call processcmd(this, file_unit)
        write(unit=file_unit, fmt='(a)') this%txtscript

        ! close the file and call gnuplot
        call finalize_plot(this, file_unit)

    end subroutine runscript





    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Five: gnuplot command processing and data writing to script file
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    subroutine process_linespec(order, lsstring, lspec)
        !..............................................................................
        ! process_linespec accepts the line specification and interpret it into
        ! a format to be sent to gnuplot
        !..............................................................................

        integer, intent(in) :: order !1 for the first data series
        character(len=*), intent(out) :: lsstring
        character(len=*), intent(in), optional :: lspec

        select case(order)
            case(1)
                if ( present(lspec) ) then
                    if (hastitle(lspec)) then
                        lsstring='plot "-" '//trim(lspec)
                    else
                        lsstring='plot "-" notitle '//trim(lspec)
                    end if
                else
                    lsstring='plot "-" notitle '
                end if
            case  default !e.g. 2, 3, 4, ...
                if (present(lspec)) then
                    if (hastitle(lspec)) then
                        lsstring=', "-" '// trim(lspec)
                    else
                        lsstring=', "-" notitle '// trim(lspec)
                    end if
                else
                    lsstring=', "-" notitle '
                end if
        end select
    end subroutine process_linespec



    subroutine processcmd(this, file_unit)
        !..............................................................................
        !   This subroutine writes all the data into plot file
        !   to be read by gnuplot
        !..............................................................................

        class(gpf)              :: this
        integer, intent(in)     :: file_unit

        ! The following lines set the gnuplot terminal
        ! The data style to lines+symbols:linespoints
        ! Can be overwritten by options

        ! write signature
        write ( file_unit, '(a)' ) '# ' // md_name
        write ( file_unit, '(a)' ) '# ' // md_rev
        write ( file_unit, '(a)' ) '# ' // md_lic
        write ( file_unit, '(a)' )  ! emptyline

        ! write the global settings
        write ( file_unit, '(a)' ) '# gnuplot global setting'
        write ( file_unit, '(a)' ) 'set style data '//this%txtdatastyle  !set data style
        write ( file_unit, '(a)' )  ! emptyline

        ! Write options
        if ( this%hasoptions ) then
            write( unit = file_unit, fmt= '(a)') '# options'
            write( unit = file_unit, fmt= '(a)') this%txtoptions
            write ( file_unit, '(a)' )  ! emptyline
        end if

        ! Check with plot scale: i.e linear, logx, logy, or log xy
        select case (this%plotscale)
            case ('semilogx')
                write ( file_unit, '(a)' ) 'set logscale  x'
            case ('semilogy')
                write ( file_unit, '(a)' ) 'set logscale  y'
            case ('loglog')
                write ( file_unit, '(a)' ) 'set logscale  xy'
            case default !For linear xy plot or 3D plots
                !pass
        end select


        write( unit = file_unit, fmt= '(a)') '# axes setting'
        if (this%hasxrange) then
            write ( file_unit, '(a,f0.0,a,f0.0,a)' ) 'set xrange [',this%xrange(1),':',this%xrange(2),']'
        end if

        if (this%hasyrange) then
            write ( file_unit, '(a,f0.0,a,f0.0,a)' ) 'set yrange [',this%yrange(1),':',this%yrange(2),']'
        end if

        if (this%haszrange) then
            write ( file_unit, '(a,f0.0,a,f0.0,a)' ) 'set zrange [',this%zrange(1),':',this%zrange(2),']'
        end if


        write( unit = file_unit, fmt= '(a)') '# plot annotation'
        if (this%hasplottitle) then
            write ( file_unit, '(a)' ) 'set title  "' // trim(this%txtplottitle)// '"'
        end if
        if (this%hasxlabel) then
            write ( file_unit, '(a)' ) 'set xlabel "'// trim(this%txtxlabel)//'"'
        end if
        if (this%hasylabel) then
            write ( file_unit, '(a)' ) 'set ylabel "'//trim(this%txtylabel)//'"'
        end if
        if (this%haszlabel) then
            write ( file_unit, '(a)' ) 'set zlabel "'//trim(this%txtzlabel)//'"'
        end if
        write ( file_unit, '(a)' )  ! emptyline

    end subroutine processcmd




    function color_palettes(palette_name) result(str)
        !...............................................................................
        ! color_palettes create color palette as a
        ! string to be written into gnuplot script file
        ! the palettes credit goes to: Anna Schnider (https://github.com/aschn) and
        ! Hagen Wierstorf (https://github.com/hagenw)
        !...............................................................................
        character(len=*), intent(in)  :: palette_name
        character(len=:), allocatable :: str

        ! local variables
        character(len=1)  :: strnum
        character(len=11) :: strblank
        integer :: j
        integer :: maxcolors

        ! define the color palettes
        character(len=:), allocatable :: pltname
        character(len=7) :: palette(9) ! palettes with maximum 9 colors

        maxcolors = 8 ! default number of discrete colors
        palette=''
        select case ( lcase(trim(adjustl(palette_name))) )
            case ('set1')
                pltname='set1'
                palette(1:maxcolors)=[&
                    "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", &
                    "#FF7F00", "#FFFF33", "#A65628", "#F781BF" ]
            case ('set2')
                pltname='set2'
                palette(1:maxcolors)=[&
                    "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", &
                    "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3" ]
            case ('set3')
                pltname='set3'
                palette(1:maxcolors)=[&
                    "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", &
                    "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5" ]
            case ('palette1')
                pltname='palette1'
                palette(1:maxcolors)=[&
                    "#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", &
                    "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC" ]
            case ('palette2')
                pltname='palette2'
                palette(1:maxcolors)=[&
                    "#B3E2CD", "#FDCDAC", "#CDB5E8", "#F4CAE4", &
                    "#D6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC" ]
            case ('paired')
                pltname='paired'
                palette(1:maxcolors)=[&
                    "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", &
                    "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00" ]
            case ('dark2')
                pltname='dark2'
                palette(1:maxcolors)=[&
                    "#1B9E77", "#D95F02", "#7570B3", "#E7298A", &
                    "#66A61E", "#E6AB02", "#A6761D", "#666666" ]
            case ('accent')
                pltname='accent'
                palette(1:maxcolors)=[&
                    "#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", &
                    "#386CB0", "#F0027F", "#BF5B17", "#666666" ]
            case ('jet')
                ! Matlab jet palette
                maxcolors = 9
                pltname='jet'
                palette(1:maxcolors)=[&
                    '#000090', '#000fff', '#0090ff', '#0fffee', &
                    '#90ff70', '#ffee00', '#ff7000', '#ee0000', '#7f0000' ]
            case default
                print*, md_name // ": color_palettes: wrong palette name"
                print*, 'gnuplot default palette will be used!'
                str=' ' ! empty palette is returned!
                return
        end select

        ! generate the gnuplot palette as a single multiline string
        str      = '# Define the ' // pltname // ' pallete' // new_line(' ')
        str      = str // 'set palette defined ( \' // new_line(' ')
        strblank = '           '
        do j=1, maxcolors - 1
            write(unit=strnum, fmt='(I1)' ) j-1
            str = str // strblank // strnum // ' "' // palette(j) // '",\' // new_line(' ')
        end do

        j=maxcolors
        write(strnum, fmt='(I1)') j
        str = str // strblank // strnum // ' "' // palette(j) // '" )' // new_line(' ')

    end function color_palettes



    subroutine write_xydata(file_unit,ndata,x,y)
        !..............................................................................
        ! Writes set of xy data into a file
        !..............................................................................

        integer, intent(in) :: file_unit
        integer, intent(in) :: ndata
        real(wp), intent(in) :: x(:)
        real(wp), intent(in), optional :: y(:)

        integer:: i

        ! TODO (Mohammad#1#12/22/17): The format string shall be modified to write the
        ! number in more suitable form
        ! Rev 0.18
        if (present(y) ) then !both x and y are present, data are xy set
            do i = 1, ndata
                write ( file_unit, * ) x(i), y(i)
            end do
        else !only x is passed, data are index-x set
            do i = 1, ndata
                write ( file_unit, * ) x(i)
            end do
        end if
        write ( file_unit, '(a)' ) 'e'  !end of set of data
    end subroutine write_xydata



    subroutine create_outputfile(this, file_unit)
        !..............................................................................
        ! Create an output file, assign a file_unit
        ! for writing the gnuplot commands
        !..............................................................................

        ! Rev 0.18
        class(gpf), intent(inout)   :: this
        integer, intent(out)        :: file_unit

        integer :: lu
        integer :: m
        integer :: io_stat
        logical :: isopen
        m = 200
        do lu = m, 10, -1
            inquire (unit=lu, opened=isopen, iostat=io_stat)
            if (io_stat.ne.0) cycle
            if (.not.isopen) exit
        end do

        file_unit = lu


        open ( unit = file_unit, file = this%txtfilename, status = 'replace', iostat = this%status )

        if (this%status /= 0 ) then
            this%msg= "gpf error: cannot open file for output"
            print*, this%msg
            stop
        end if
        ! write the general setting
        write(unit=file_unit, fmt='(a)') 'set term ' // gnuplot_term_type // &
            ' size ' // gnuplot_term_size // ' enhanced font "' // &
            gnuplot_term_font // '"'

    end subroutine create_outputfile


    subroutine finalize_plot(this, file_unit)
        !..............................................................................
        ! To finalize the writing of gnuplot commands and close the output file.
        !..............................................................................
        class(gpf) :: this
        integer, intent(in) :: file_unit
        integer :: status_

        close ( unit = file_unit )                                         ! close the script file
        call execute_command_line ('gnuplot -persist '//this%txtfilename)  !   Now plot the results

    end subroutine finalize_plot



    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Six: Utility and helper procedures
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    function hastitle(string)
        !..............................................................................
        ! check to see if the plot title (used as legend = key)
        !..............................................................................

        character(len=*), intent(in) :: string
        logical:: hastitle
        integer:: idx1
        integer:: idx2

        idx1=index( lcase(string),'title')     !Check if title is passed
        idx2=index(' ' // lcase(string),' t ') !Check if the abbreviated title 't' is passed. Extra space is added
        ! at the beginning of string to find starting 't'
        if (idx1 /=0 .or. idx2 /=0 ) then
            hastitle=.true.
        else
            hastitle=.false.
        end if

    end function hastitle


    function checkdim(nx,ny)
        !..............................................................................
        ! checkdim checks the equality of dimensions of two vector
        !..............................................................................

        integer, intent(in):: nx
        integer, intent(in):: ny
        logical:: checkdim
        if (nx/=ny) then
            checkdim=.false.
        else
            checkdim=.true.
        end if

    end function checkdim



    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !> Section Seven: String utility Routines
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




    pure function splitstr(str) result(spstr)
        !..............................................................................
        !splitstr, separate a string using ";" delimiters
        !..............................................................................

        character(len=*), intent(in)    :: str

        ! local variables
        character, parameter          :: delimiter=';'
        character(len=:), allocatable :: spstr
        integer ::  n
        integer ::  m
        integer ::  k


        k=len_trim(str) !length with removed trailing blanks
        n=scan(str,delimiter)
        if (n==0) then  ! This is a single statement
            spstr = adjustl(str) // new_line(' ')
            return
        end if

        ! for two or more statements separated by ;
        spstr = ''
        m=1
        do while (n/=0 .and. m<k)
            if (n/=1) then
                spstr = spstr // adjustl(str(m:m+n-2)) // new_line(' ')
            end if
            m=n+m
            n=scan(str(m:k),delimiter)
        end do
        if (m<k) then !write the last statement
            spstr = spstr// adjustl(str(m:k)) // new_line(' ')
        end if
    end function splitstr



    subroutine splitstring2array(strin, strarray, delimiter)
        ! splitstring splits a string to an array of
        ! substrings based on a selected delimiter
        ! note:
        !    a. any facing space/blank in substrings will be removed
        !    b. two adjacent delimiter treats as an empty substring between them
        !    c. facing and trailing delimiter treats as empty substring at the fornt and end


        character(len=*),  intent(in)               :: strin
        character(len=80), allocatable, intent(out) :: strarray(:)
        character(len=1),  optional,    intent(in)  :: delimiter

        ! local variables
        integer   :: m, n
        integer   :: i, idx
        character(len=len(strin)) :: strtmp
        character(len=1)  :: delimiter_

        ! 0. check the existance of delimiter
        if (present(delimiter)) then
            delimiter_ = delimiter
        else
            delimiter_ = ';'
        end if


        ! 1. remove initial blanks if any
        strtmp=trim (adjustl(strin) )

        ! 2. count the number substrings separated by delimiter
        n = count( [ (strtmp(i:i), i=1, len_trim(strtmp)) ] == delimiter_)

        ! 3. allocate the output string array
        allocate(strarray(n+1))

        ! 4. extract substrings and store in array one by one
        m=1
        do i=1, n
            idx=index(strtmp(m:),delimiter_)
            strarray(i) = adjustl( strtmp(m:m+idx-2) )
            m = m + idx
        end do
        strarray(n+1)=adjustl(strtmp(m:) )


    end subroutine splitstring2array


    function lcase(strinput)
        !..............................................................................
        ! Return the string (strInput) in lowercase
        !..............................................................................

        character(len=*), intent(in) :: strinput
        character(len=len(strinput)):: lcase
        integer:: i
        integer:: n
        character(1):: chr

        do i=1, len(strinput)
            chr=strinput(i:i)
            n=ichar(chr)
            if (n >=65 .and. n <= 90) then
                lcase(i:i)=char(n+32)
            else
                lcase(i:i)=chr
            end if
        end do
    end function lcase




    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Eight: Math helper function
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    function arange(xa, xb, dx)
        !..............................................................................
        !   returns a vector in the form of [xa, xa+dx, xa+2*dx, ...]
        !   the number of elements is calculated as m = n+ 1,
        !   where n= int ( (xa-xb)/dx) ).
        !   arange is similar to colon in Matlab and arange in Python!
        !
        !   NOTE:
        !    - If n calculated as zero, result is [xa]
        !    - If n calculated as Inf (dx=0), a fatal error will be raised
        !    - If n calculated as negative value (e.g xa<xb or dx<0.0), a
        !      fatal error will be raised
        !..............................................................................

        real(wp), intent(in)            :: xa
        real(wp), intent(in)            :: xb
        real(wp), intent(in), optional  :: dx
        real(wp), allocatable           :: arange(:)

        !   Local vars
        real(wp):: dxl
        integer:: i
        integer:: n
        integer:: ierr

        ! check the presence of dx and its correctness
        if (present(dx)) then
            dxl = dx
            if ( abs(dx) <= tiny(dx)) then
                print*, "arange procedure: Fatal Error: wrong dx, use a dx > 0.0 "
                stop
            end if
        else
            dxl = 1.0_wp
        end if

        if ( (xa < xb) .and. (dx < 0.0_wp) ) then
            print*, "arange procedure: Fatal Error: wrong dx, use a dx > 0.0 "
            stop
        end if

        n = int( (xb-xa)/ dxl) ! n+1 is the number of elements

        allocate(arange(n), stat=ierr)

        if (ierr /= 0) then
            print*, "arange procedure: Fatal Error, allocation failed in arange function"
            stop
        end if

        arange = [(xa + i*dxl, i=0, n)]

    end function arange


    function linspace(a,b,n_elements)
        !..............................................................................
        !   returns a linearly spaced vector with n points in [a, b]
        !   if n is omitted, 100 points will be considered
        !..............................................................................

        real(wp), intent(in)           :: a
        real(wp), intent(in)           :: b
        integer,  intent(in), optional :: n_elements
        real(wp), allocatable          :: linspace(:)

        !   Local vars
        real(wp) :: dx
        integer  :: i
        integer  :: n
        integer  :: ierr

        if (present(n_elements)) then
            if (n_elements <=1 ) then
                print*, "linspace procedure: Error: wrong value of n_elements, use an n_elements > 1"
                stop
            end if
            n=n_elements
        else
            n=100
        end if

        allocate(linspace(n), stat=ierr)
        if (ierr /= 0) then
            print*, "linspace procedure: Fatal Error, Allocation failed in linspace function"
            stop
        end if

        dx=(b-a)/real((n-1),wp)
        linspace=[(i*dx+a, i=0,n-1)]

    end function linspace



    subroutine meshgrid(x,y,xgv,ygv, ierr)
        !..............................................................................
        !meshgrid generate mesh grid over a rectangular domain of [xmin xmax, ymin, ymax]
        ! Inputs:
        !     xgv, ygv are grid vectors in form of full grid data
        ! Outputs:
        !     X and Y are matrix each of size [ny by nx] contains the grid data.
        !     The coordinates of point (i,j) is [X(i,j), Y(i,j)]
        !     ierr: The error flag
        !     """
        !     # Example
        !     # call meshgrid(X, Y, [0.,1.,2.,3.],[5.,6.,7.,8.])
        !     # X
        !     # [0.0, 1.0, 2.0, 3.0,
        !     #  0.0, 1.0, 2.0, 3.0,
        !     #  0.0, 1.0, 2.0, 3.0,
        !     #  0.0, 1.0, 2.0, 3.0]
        !     #
        !     #Y
        !     #[ 5.0, 5.0, 5.0, 5.0,
        !     #  6.0, 6.0, 6.0, 6.0,
        !     #  7.0, 7.0, 7.0, 7.0,
        !     #  8.0, 8.0, 8.0, 8.0]
        !..............................................................................
        ! Rev 0.2, Feb 2018
        ! New feature added: xgv and ygv as full grid vector are accepted now

        ! Arguments
        real(wp), intent(out), allocatable  :: x(:,:)
        real(wp), intent(out), allocatable  :: y(:,:)
        real(wp), intent(in)                :: xgv(:) ! x grid vector [start, stop, step] or [start, stop]
        real(wp), intent(in),  optional     :: ygv(:) ! y grid vector [start, stop, step] or [start, stop]
        integer,  intent(out), optional     :: ierr   ! the error value

        ! Local variables
        integer:: sv
        integer:: nx
        integer:: ny
        logical:: only_xgv_available

        ! Initial setting
        only_xgv_available  = .false.
        sv=0 !Assume no error

        nx=size(xgv, dim=1)

        if (present(ygv)) then
            ny = size(ygv, dim=1)
        else
            only_xgv_available=.true.
            ny=nx
        end if

        allocate(x(ny,nx),y(ny,nx),stat=sv)
        if (sv /=0) then
            print*, "allocataion erro in meshgrid"
            stop
        end if

        x(1,:)    = xgv
        x(2:ny,:) = spread(xgv, dim=1, ncopies=ny-1)

        if (only_xgv_available) then
            y=transpose(x)
        else
            y(:,1)    = ygv
            y(:,2:nx) = spread(ygv,dim=2,ncopies=nx-1)
        end if

        if (present(ierr)) then
            ierr=sv
        end if

    end subroutine meshgrid


    !End of ogpf
end module ogpf
