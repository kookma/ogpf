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
!
!
! Acknowledgement:
! Special thanks to Hagen Wierstorf (http://www.gnuplotting.org)
! For vluable codes and examples on using gnuplot
! Some examples and color palletes are provided by gnuplotting.
!


! Revision History

! Revision 0.22
! Date: Mar 9th, 2018
! - a new procedure called use_extra_configuration is used to set general gnuplot settings
! - new type for labels (xlabel, ylabel, zlabel, title,...)
! - all lables now accept text color, font name, font size, rorate by degree
! - Secondary axes can use different scale (linear or logarithmic)
! - subroutine plot2d_matrix_vs_matrix(xmat,ymat)
!    now plots a matrix columns ymat aganist another matrix column xmat
! - added more examples

! Revision 0.21
! Date: Mar 8th, 2018
! - new axes to plot command to use secondary axes added!


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
!  - new num2str routines



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
    public num2str

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

    ! Library information
    character(len=*), parameter :: md_name = 'ogpf libray'
    character(len=*), parameter :: md_rev  = 'Rev. 0.22 of March 9th, 2018'
    character(len=*), parameter :: md_lic  = 'Licence: MIT'

    ! ogpf Configuration parameters
    ! The terminal and font have been set for Windows operating system
    ! Correct to meet the requirements on other OS like Linux and Mac.
    character(len=*), parameter ::  gnuplot_term_type = 'wxt'                      ! Output terminal
    character(len=*), parameter ::  gnuplot_term_font = 'verdana,10'               ! font
    character(len=*), parameter ::  gnuplot_term_size = '640,480'   !'960,840'                  ! plot window size
    character(len=*), parameter ::  gnuplot_output_filename='ogpf_temp_script.gp' ! temporary file for output
    ! extra configuration can be set using ogpf object

    ! module procedure
    interface num2str ! convert integer, real, double precision into string
        module procedure num2str_i4
        module procedure num2str_r4
        module procedure num2str_r8
    end interface

    !> 0.22
    ! tplabel is a structure for gnuplot labels including
    ! title, xlabel, x2label, ylabel, ...
    integer, parameter, private :: NOT_INITIALIZED = -32000
    type tplabel
        logical                       :: has_label = .false.
        character(len=:), allocatable :: lbltext
        character(len=:), allocatable :: lblcolor
        character(len=:), allocatable :: lblfontname
        integer                       :: lblfontsize = NOT_INITIALIZED
        integer                       :: lblrotate   = NOT_INITIALIZED
    end type tplabel


    type, public :: gpf
        ! the gpf class implement the object for using gnuplot from fortran in a semi-interactive mode!
        ! the fortran actually do the job and write out the commands and data in a single file and then
        ! calls the gnuplot by shell command to plot the data

        private

        !> 0.22
        type(tplabel) :: tpplottitle
        type(tplabel) :: tpxlabel
        type(tplabel) :: tpx2label
        type(tplabel) :: tpylabel
        type(tplabel) :: tpy2label
        type(tplabel) :: tpzlabel

        character(len=:), allocatable  :: txtoptions    ! a long string to store all type of gnuplot options
        character(len=:), allocatable  :: txtscript     ! a long string to store gnuplot script
        character(len=:), allocatable  :: txtdatastyle  ! lines, points, linepoints

        logical :: hasxrange      = .false.
        logical :: hasx2range     = .false.
        logical :: hasyrange      = .false.
        logical :: hasy2range     = .false.
        logical :: haszrange      = .false.
        logical :: hasoptions     = .false.
        logical :: hasanimation   = .false.
        logical :: hasfilename    = .false.
        logical :: hasfileopen    = .false.

        real(wp)           :: xrange(2), yrange(2), zrange(2)
        real(wp)           :: x2range(2), y2range(2)
        character(len=8)   :: plotscale


        ! multiplot parameters
        logical :: hasmultiplot = .false.
        integer :: multiplot_rows
        integer :: multiplot_cols
        integer :: multiplot_total_plots

        ! animation
        integer  :: pause_seconds = 0  ! keep plot on screen for this value in seconds
        integer                         :: frame_number   ! frame number in animation

        ! use for debugging and error handling
        character(len=:), allocatable   :: msg      !Message from plot procedures
        integer                         :: status=0 !Status from plot procedures

        !
        integer                         :: file_unit      ! file unit identifier
        character(len=:), allocatable   :: txtfilename    ! the name of physical file
                                                          ! to write the gnuplot script


        ! ogpf preset configuration (kind of gnuplot initialization)
        logical :: preset_configuration = .true.


    contains

        private

        ! local private procedures
        procedure, pass, private :: preset_gnuplot_config

        procedure, pass, private :: plot2d_vector_vs_vector
        procedure, pass, private :: plot2d_matrix_vs_vector
        procedure, pass, private :: plot2d_matrix_vs_matrix

        procedure, pass, private :: semilogxv
        procedure, pass, private :: semilogxm
        procedure, pass, private :: semilogyv
        procedure, pass, private :: semilogym
        procedure, pass, private :: loglogv
        procedure, pass, private :: loglogm

        !> 0.22
        procedure, pass, private :: set_label

        ! public procedures
        procedure, pass, public :: options      => set_options
        procedure, pass, public :: title        => set_plottitle
        procedure, pass, public :: xlabel       => set_xlabel
        procedure, pass, public :: x2label      => set_x2label
        procedure, pass, public :: ylabel       => set_ylabel
        procedure, pass, public :: y2label      => set_y2label
        procedure, pass, public :: zlabel       => set_zlabel
        procedure, pass, public :: axis         => set_axis
        procedure, pass, public :: axis_sc      => set_secondary_axis
        procedure, pass, public :: filename     => set_filename
        procedure, pass, public :: reset        => reset_to_defaults
        procedure, pass, public :: preset       => use_preset_configuration


        procedure, pass, public :: multiplot  => sub_multiplot
        generic, public         :: plot       => plot2d_vector_vs_vector, &
                                                 plot2d_matrix_vs_vector, &
                                                 plot2d_matrix_vs_matrix
        generic, public         :: semilogx   => semilogxv, semilogxm
        generic, public         :: semilogy   => semilogyv, semilogym
        generic, public         :: loglog     => loglogv, loglogm

        procedure, pass, public :: surf       => splot  ! 3D surface plot
        procedure, pass, public :: contour    => cplot  ! contour plot

        procedure, pass, public :: fplot      => function_plot

        procedure, pass, public :: add_script => addscript
        procedure, pass, public :: run_script => runscript

        procedure, pass, public :: animation_start => sub_animation_start
        procedure, pass, public :: animation_show  => sub_animation_show

    end type gpf


contains

    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section One: Set/Get Methods for ogpf object
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    subroutine use_preset_configuration(this,flag)
        !..............................................................................
        !Set a flag to tell ogpf if the customized gnuplot configuration should
        !be used
        !..............................................................................

        class(gpf):: this
        logical, intent(in) :: flag

        ! default is true
        this%preset_configuration = flag

    end subroutine use_preset_configuration



    subroutine set_filename(this,string)
        !..............................................................................
        !Set a file name for plot command output
        !This file can be used later by gnuplot as an script file to reproduce the plot
        !..............................................................................

        class(gpf):: this
        character(len=*), intent(in) :: string

        this%txtfilename = trim(string)
        this%hasfilename = .true.

    end subroutine set_filename


    subroutine set_options(this,stropt)
        !..............................................................................
        ! Set the plot options. This is a very powerfull procedure accepts many types
        ! of gnuplot command and customization
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




    subroutine set_axis(this,rng)
        !..............................................................................
        !Set the axes limits in form of [xmin, xmax, ymin, ymax, zmin, zmax]
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


    subroutine set_secondary_axis(this,rng)
        !..............................................................................
        !Set the secondary axes limits in form of [x2min, x2max, y2min, y2max]
        !..............................................................................

        class(gpf):: this
        real(wp), intent(in) :: rng(:)
        integer :: n
        n=size(rng,dim=1)
        select case(n)
            case(2) !Only the range for x2-axis has been sent
                this%hasx2range=.true.
                this%x2range=rng(1:2)
            case(4)
                this%hasx2range=.true.
                this%hasy2range=.true.
                this%x2range=rng(1:2)
                this%y2range=rng(3:4)
            case default
                print*, 'gpf error: wrong axis range setting!'
                return
        end select

    end subroutine set_secondary_axis


    subroutine set_plottitle(this, string, textcolor, font_size, font_name, rotate)
        !..............................................................................
        !Set the plot title
        !..............................................................................
        class(gpf):: this
        character(len=*), intent(in)           :: string
        character(len=*), intent(in), optional :: textcolor
        integer, optional                      :: font_size
        character(len=*), intent(in), optional :: font_name
        integer, optional                      :: rotate

        call this%set_label('plot_title', string, textcolor, font_size, font_name, rotate)

    end subroutine set_plottitle


    subroutine set_xlabel(this, string, textcolor, font_size, font_name, rotate)
        !..............................................................................
        !Set the xlabel
        !..............................................................................
        class(gpf):: this
        character(len=*), intent(in)           :: string
        character(len=*), intent(in), optional :: textcolor
        integer, optional                      :: font_size
        character(len=*), intent(in), optional :: font_name
        integer, optional                      :: rotate

        call this%set_label('xlabel', string, textcolor, font_size, font_name, rotate)

    end subroutine set_xlabel


    subroutine set_x2label(this, string, textcolor, font_size, font_name, rotate)
        !..............................................................................
        !Set the x2label
        !..............................................................................
        class(gpf):: this
        character(len=*), intent(in)           :: string
        character(len=*), intent(in), optional :: textcolor
        integer, optional                      :: font_size
        character(len=*), intent(in), optional :: font_name
        integer, optional                      :: rotate

        call this%set_label('x2label', string, textcolor, font_size, font_name, rotate)

    end subroutine set_x2label


    subroutine set_ylabel(this, string, textcolor, font_size, font_name, rotate)
        !..............................................................................
        !Set the ylabel
        !..............................................................................
        class(gpf):: this
        character(len=*), intent(in)           :: string
        character(len=*), intent(in), optional :: textcolor
        integer, optional                      :: font_size
        character(len=*), intent(in), optional :: font_name
        integer, optional                      :: rotate

        call this%set_label('ylabel', string, textcolor, font_size, font_name, rotate)

    end subroutine set_ylabel



    subroutine set_y2label(this, string, textcolor, font_size, font_name, rotate)
        !..............................................................................
        !Set the y2label
        !..............................................................................
        class(gpf):: this
        character(len=*), intent(in)           :: string
        character(len=*), intent(in), optional :: textcolor
        integer, optional                      :: font_size
        character(len=*), intent(in), optional :: font_name
        integer, optional                      :: rotate

        call this%set_label('y2label', string, textcolor, font_size, font_name, rotate)

    end subroutine set_y2label


    subroutine set_zlabel(this, string, textcolor, font_size, font_name, rotate)
        !..............................................................................
        !Set the zlabel
        !..............................................................................
        class(gpf):: this
        character(len=*), intent(in)           :: string
        character(len=*), intent(in), optional :: textcolor
        integer, optional                      :: font_size
        character(len=*), intent(in), optional :: font_name
        integer, optional                      :: rotate

        call this%set_label('zlabel', string, textcolor, font_size, font_name, rotate)

    end subroutine set_zlabel


    !> 0.22

    subroutine set_label(this, lblname, lbltext, lblcolor, font_size, font_name, rotate)
        !..............................................................................
        ! Set the text, color, font, size and rotation for labels including
        ! title, xlabel, x2label, ylabel, ....
        !..............................................................................

        class(gpf):: this
        character(len=*), intent(in)           :: lblname
        character(len=*), intent(in)           :: lbltext
        character(len=*), intent(in), optional :: lblcolor
        character(len=*), intent(in), optional :: font_name
        integer, optional :: font_size
        integer, optional                      :: rotate

        ! local variable
        type(tplabel) :: label

        label%has_label = .true.
        label%lbltext   = trim(lbltext)

        if (present(lblcolor)) then
            label%lblcolor = lblcolor
        end if

        if (present(font_name)) then
            label%lblfontname = font_name
        end if

        if (present(font_size)) then
            label%lblfontsize = font_size
        end if

        if (present(rotate)) then
            label%lblrotate = rotate
        end if

        select case (lblname)
            case ('xlabel')
                this%tpxlabel     = label
            case ('x2label')
                this%tpx2label    = label
            case ('ylabel')
                this%tpylabel     = label
            case ('y2label')
                this%tpy2label    = label
            case ('zlabel')
                this%tpzlabel     = label
            case ('plot_title')
                this%tpplottitle  = label
        end select


    end subroutine set_label



    subroutine reset_to_defaults(this)
        !..............................................................................
        !Reset all ogpf properties (params to their default values
        !...............................................................................
        class(gpf):: this

        this%preset_configuration    = .true.
        this%txtfilename             = gnuplot_output_filename

        if (allocated(this%txtoptions))    deallocate(this%txtoptions)
        if (allocated(this%txtscript))     deallocate(this%txtscript)
        if (allocated(this%txtdatastyle))  deallocate(this%txtdatastyle)
        if (allocated(this%msg))           deallocate(this%msg)

        this%hasoptions            = .false.

        this%hasxrange             = .false.
        this%hasx2range            = .false.
        this%hasyrange             = .false.
        this%hasy2range            = .false.
        this%haszrange             = .false.

        this%pause_seconds         = 0
        this%status                = 0
        this%hasanimation          = .false.
        this%hasfileopen           = .false.
        this%hasmultiplot          = .false.

        this%plotscale             = ''
        this%tpplottitle%has_label =.false.
        this%tpxlabel%has_label    =.false.
        this%tpx2label%has_label   =.false.
        this%tpylabel%has_label    =.false.
        this%tpy2label%has_label   =.false.
        this%tpzlabel%has_label    =.false.


    end subroutine reset_to_defaults


    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Two: Main Plotting Routines
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    subroutine sub_multiplot(this, rows, cols)
        !..............................................................................
        ! This subroutine sets flag and number of rows and columns in case
        ! of multiplot layout
        !..............................................................................

        class(gpf):: this
        integer, intent(in) :: rows
        integer, intent(in) :: cols

        ! ogpf does not support multiplot in animation mode
        if (this%hasanimation) then
            print*, md_name // ': ogpf does not support animation in multiplot mode'
            stop
        end if

        ! set multiplot cols and rows
        if (rows> 0 ) then
            this%multiplot_rows = rows
        else

        end if
        if (cols > 0 ) then
            this%multiplot_cols = cols
        else

        end if

        ! set the multiplot layout flag and plot numbers
        this%hasmultiplot = .true.
        this%multiplot_total_plots = 0

        ! create the ouput file for writting gnuplot script
        call create_outputfile(this)


    end subroutine sub_multiplot


    subroutine plot2d_vector_vs_vector(this, x1, y1, ls1, axes1, &
            x2, y2, ls2, axes2, &
            x3, y3, ls3, axes3, &
            x4, y4, ls4, axes4  )
        !..............................................................................
        ! This procedure plots:
        !   1. A vector against another vector (xy plot)
        !   2. A vector versus its element indices (yi plot).
        !   3. Can accept up to 4 data sets as x,y pairs!
        ! Arguments
        ! xi, yi vectors of data series,
        ! lsi a string maximum 80 characters containing the line specification,
        ! legends, ...
        ! axesi is the axes for plotting: secondary axes are x2, and y2
        !..............................................................................

        class(gpf):: this
        ! Input vector
        real(wp),  intent(in)                          :: x1(:)  ! vector of data for x
        real(wp),  intent(in), optional                :: y1(:)  ! vector of data for y
        character(len=*),  intent(in), optional        :: ls1    ! line specification
        character(len=*),  intent(in), optional        :: axes1

        real(wp),  intent(in), dimension(:), optional  :: x2
        real(wp),  intent(in), dimension(:), optional  :: y2
        character(len=*),  intent(in), optional        :: ls2
        character(len=*),  intent(in), optional        :: axes2

        real(wp),  intent(in), dimension(:), optional  :: x3
        real(wp),  intent(in), dimension(:), optional  :: y3
        character(len=*),  intent(in), optional        :: ls3
        character(len=*),  intent(in), optional        :: axes3

        real(wp),  intent(in), dimension(:), optional  :: x4
        real(wp),  intent(in), dimension(:), optional  :: y4
        character(len=*),  intent(in), optional        :: ls4
        character(len=*),  intent(in), optional        :: axes4

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
        character(len=80) ::  pltstring(4)  ! Four 80 characters string

        !Initialize variables
        plottype  = ''
        pltstring = ''

        !   Check the input
        nx1=size(x1)
        if ((present(y1) )) then
            ny1=size(y1)
            if (checkdim(nx1,ny1)) then
                plottype='xy1'
                number_of_plots=1
            else
                print*, md_name // ':plot2d_vector_vs_vector:' // 'length of x1 and y1 does not match'
                return
            end if
        else !plot only x againest its element indices
            plottype='xi'
            number_of_plots=1
        end if

        !Process line spec and axes set for first data set if present
        call process_linespec(1, pltstring(1), ls1, axes1)


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
            call process_linespec(2, pltstring(2), ls2, axes2)
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
            call process_linespec(3, pltstring(3), ls3, axes3)
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
            call process_linespec(4, pltstring(4), ls4, axes4)
        end if


        call create_outputfile(this)

        ! Write plot title, axis labels and other annotations
        call processcmd(this)

        ! Write plot command and line styles and legend if any
        if (number_of_plots ==1) then
            write ( this%file_unit, '(a)' )  trim(pltstring(1))
        else
            write ( this%file_unit, '(a)' )  ( trim(pltstring(i)) // ' \' , i=1, number_of_plots-1)
            write ( this%file_unit, '(a)' )  trim(pltstring(number_of_plots))
        end if
        ! Write xy data into file
        select case (plottype)
            case ('xi')
                call write_xydata(this%file_unit,nx1,x1)
            case ('xy1')
                call write_xydata(this%file_unit,nx1,x1,y1)
            case ('xy2')
                call write_xydata(this%file_unit,nx1,x1,y1)
                call write_xydata(this%file_unit,nx2,x2,y2)
            case ('xy3')
                call write_xydata(this%file_unit,nx1,x1,y1)
                call write_xydata(this%file_unit,nx2,x2,y2)
                call write_xydata(this%file_unit,nx3,x3,y3)
            case ('xy4')
                call write_xydata(this%file_unit,nx1,x1,y1)
                call write_xydata(this%file_unit,nx2,x2,y2)
                call write_xydata(this%file_unit,nx3,x3,y3)
                call write_xydata(this%file_unit,nx4,x4,y4)
        end select

        !> Rev 0.2
        ! if there is no animation finalize
        if (.not. (this%hasanimation)) then
            call finalize_plot(this)
        else
            write(this%file_unit, '(a, I2)') 'pause ', this%pause_seconds
        end if


        !: End of plot2D_vector_vs_vector
    end subroutine plot2d_vector_vs_vector



    subroutine  plot2d_matrix_vs_vector(this, xv,ymat, lspec)
        !..............................................................................
        ! plot2D_matrix_vs_vector accepts a vector xv and a matrix ymat and plots
        ! columns of ymat against xv. lspec is an optional array defines the line
        ! specification for each data series. If a single element array is sent for
        ! lspec then all series are plotted using the same linespec
        !..............................................................................

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
        integer:: i
        integer:: j
        integer:: ierr
        character(len=80), allocatable ::  pltstring(:), lst(:)
        !

        !*******************************************************************************
        !   Check the input
        nx=size(xv)
        ny=size(ymat,dim=1)
        if (.not. checkdim(nx,ny)) then
            print*, md_name // ':plot2d_matrix_vs_vector:' // 'The length of arrays does not match'
            return
        end if
        ! create the outfile to write the gnuplot script
        call create_outputfile(this)

        ! Write titles and other annotations
        call processcmd(this)

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
        write ( this%file_unit, '(a)' ) ( trim(pltstring(i)) // ' \' , i=1, number_of_curves-1)
        write ( this%file_unit, '(a)' )   trim(pltstring(number_of_curves))

        ! Write data into script file
        do j=1, number_of_curves
            do i = 1, nx
                write ( this%file_unit, * ) xv(i),ymat(i,j)
            end do
            write ( this%file_unit, '(a)' ) 'e'  !end of jth set of data
        end do


        !> Rev 0.2
        ! if there is no animation finalize
        if (.not. (this%hasanimation)) then
            call finalize_plot(this)
        else
            write(this%file_unit, '(a, I2)') 'pause ', this%pause_seconds
        end if

        !Release memory
        if (allocated(pltstring)) then
            deallocate(pltstring)
        end if
        !: End of plot2D_matrix_vs_vector
    end subroutine  plot2d_matrix_vs_vector



    subroutine  plot2d_matrix_vs_matrix(this, xmat,ymat, lspec)
        !..............................................................................
        ! plot2D_matrix_vs_matrix accepts a matrix xmat and a matrix ymat and plots
        ! columns of ymat against columns of xmat. lspec is an optional array defines
        ! the line specification for each data series. If a single element array is
        ! sent for lspec then all series are plotted using the same linespec
        !..............................................................................

        implicit none
        class(gpf):: this
        ! Input arrays
        real(wp),  intent(in)                       :: xmat(:,:)
        real(wp),  intent(in)                       :: ymat(:,:)
        character(len=*),  intent(in), optional     :: lspec
        !----------------------------------------------------------------------
        !       Local variables
        integer:: mx, nx
        integer:: my, ny
        integer:: ns
        integer:: number_of_curves
        integer:: i
        integer:: j
        integer:: ierr
        character(len=80), allocatable ::  pltstring(:), lst(:)
        !

        !*******************************************************************************
        !   Check the input
        ! check number of rows
        mx=size(xmat,dim=1)
        my=size(ymat,dim=1)
        if (.not. checkdim(mx,my)) then
            print*, md_name // ':plot2d_matrix_vs_matrix:' // 'The length of arrays does not match'
            return
        end if
        ! check number of rows
        nx=size(xmat,dim=2)
        ny=size(ymat,dim=2)
        if (.not. checkdim(nx,ny)) then
            print*, 'gpf error: The number of columns are different, check xmat, ymat'
            return
        end if


        ! create the outfile to write the gnuplot script
        call create_outputfile(this)

        ! Write titles and other annotations
        call processcmd(this)

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
                print*, md_name // ': plot2d_matrix_vs_matrix:'//' wrong number of linespec'
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
        write ( this%file_unit, '(a)' ) ( trim(pltstring(i)) // ' \' , i=1, number_of_curves-1)
        write ( this%file_unit, '(a)' )   trim(pltstring(number_of_curves))

        ! Write data into script file
        do j=1, number_of_curves
            do i = 1, mx
                write ( this%file_unit, * ) xmat(i,j),ymat(i,j)
            end do
            write ( this%file_unit, '(a)' ) 'e'  !end of jth set of data
        end do

        !> Rev 0.2
        ! if there is no animation finalize
        if (.not. (this%hasanimation)) then
            call finalize_plot(this)
        else
            write(this%file_unit, '(a, I2)') 'pause ', this%pause_seconds
        end if

        !Release memory
        if (allocated(pltstring)) then
            deallocate(pltstring)
        end if
        !: End of plot2D_matrix_vs_vector
    end subroutine  plot2d_matrix_vs_matrix


    subroutine splot(this, x, y, z, lspec, palette)
        !..............................................................................
        ! splot create a surface plot
        ! datablock is used instead of  gnuplot inline file "-"
        !..............................................................................

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

        ! set default line style for 3D plot, can be overwritten
        this%txtdatastyle = 'lines'
        ! create the script file for writting gnuplot commands and data
        call create_outputfile(this)

        ! Write titles and other annotations
        call processcmd(this)

        ! Write xy data into file
        write ( this%file_unit, '(a)' ) '#data x y z'
        ! Rev 0.20
        ! write the $xyz datablocks
        write( this%file_unit, '(a)' )  datablock // ' << EOD'
        if (xyz_data) then
            do j=1,ncx
                do i=1, nrx
                    write ( this%file_unit, * ) x(i,j), y(i,j), z(i,j)
                enddo
                write( this%file_unit, '(a)' )  !put an empty line
            enddo
            write ( this%file_unit, '(a)' ) 'EOD'  !end of datablock
        else !only Z has been sent (i.e. single matrix data)
            do j=1,ncx
                do i=1, nrx
                    write ( this%file_unit, * ) i, j, x(i,j)
                enddo
                write( this%file_unit, '(a)' )  !put an empty line
            enddo
            write ( this%file_unit, '(a)' ) 'EOD'  !end of datablock
        end if


        !write the color palette into gnuplot script file
        if (present(palette)) then
            write ( this%file_unit, '(a)' )  color_palettes(palette)
            write ( this%file_unit, '(a)' )  'set pm3d' ! a conflict with lspec
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

        write ( this%file_unit, '(a)' ) trim(pltstring)


        !> Rev 0.2: animation
        ! if there is no animation finalize
        if (.not. (this%hasanimation)) then
            call finalize_plot(this)
        else
            write(this%file_unit, '(a, I2)') 'pause ', this%pause_seconds
        end if

        !: End of splot
    end subroutine splot


    subroutine cplot(this, x, y, z, lspec, palette)
        !..............................................................................
        !   Rev 0.19
        !   cplot creates a contour plot based on the three dimensional data
        !..............................................................................

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

        ! set default line style for 3D plot, can be overwritten
        this%txtdatastyle = 'lines'
        ! create the script file for writting gnuplot commands and data
        call create_outputfile(this)

        ! Write titles and other annotations
        call processcmd(this)

        ! Write xy data into file
        write ( this%file_unit, '(a)' ) '#data x y z'
        ! write the $xyz datablocks
        write( this%file_unit, '(a)' )  datablock // ' << EOD'
        if (xyz_data) then
            do j=1,ncx
                do i=1, nrx
                    write ( this%file_unit, fmt=* ) x(i,j), y(i,j), z(i,j)
                enddo
                write( this%file_unit, '(a)' )  !put an empty line
            enddo
            write ( this%file_unit, '(a)' ) 'EOD'  !end of datablock
        else !only Z has been sent (i.e. single matrix data)
            do j=1,ncx
                do i=1, nrx
                    write ( this%file_unit, fmt=* ) i, j, x(i,j)
                enddo
                write( this%file_unit, '(a)' )  !put an empty line
            enddo
            write ( this%file_unit, '(a)' ) 'EOD'  !end of datablock
        end if


        ! create the contour lines
        write ( this%file_unit, '(a)' ) ! empty line
        write ( this%file_unit, '(a)' ) '# create the contour'
        write ( this%file_unit, '(a)' ) 'set contour base'
        write ( this%file_unit, '(a)' ) 'set cntrparam levels 14'
        write ( this%file_unit, '(a)' ) 'unset surface'
        write ( this%file_unit, '(a)' ) 'set view map'


        !write the color palette into gnuplot script file
        if (present(palette)) then
            write ( this%file_unit, '(a)' )  color_palettes(palette)
            write ( this%file_unit, '(a)' )  'set pm3d' ! a conflict with lspec
        end if


        write ( this%file_unit, '(a)' ) ! empty line

        if ( present(lspec) ) then
            if (hastitle(lspec)) then
                pltstring='splot ' // datablock // ' ' // trim(lspec)
            else
                pltstring='splot ' // datablock // ' notitle '//trim(lspec)
            end if
        else
            pltstring='splot ' // datablock // ' notitle '
        end if

        write ( this%file_unit, '(a)' ) trim(pltstring)

        !> Rev 0.20
        ! if there is no animation finalize
        if (.not. (this%hasanimation)) then
            call finalize_plot(this)
        else
            write(this%file_unit, '(a, I2)') 'pause ', this%pause_seconds
        end if

        !: End of cplot
    end subroutine cplot


    subroutine function_plot(this, func,xrange,np)
        !..............................................................................
        ! fplot, plot a function in the range xrange=[xmin, xamx] with np points
        ! if np is not sent, then np=50 is assumed!
        ! func is the name of function to be plotted
        !..............................................................................

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


    subroutine semilogxv(this, x1, y1, ls1, axes1, &
            x2, y2, ls2, axes2, &
            x3, y3, ls3, axes3, &
            x4, y4, ls4, axes4  )
        !..............................................................................
        !   This procedure is the same as plotXY with logarithmic x1 and x2 axes
        !..............................................................................

        class(gpf):: this
        ! Input vector
        real(wp),  intent(in)                          :: x1(:)  ! vector of data for x
        real(wp),  intent(in), optional                :: y1(:)  ! vector of data for y
        character(len=*),  intent(in), optional        :: ls1    ! line specification
        character(len=*),  intent(in), optional        :: axes1

        real(wp),  intent(in), dimension(:), optional  :: x2
        real(wp),  intent(in), dimension(:), optional  :: y2
        character(len=*),  intent(in), optional        :: ls2
        character(len=*),  intent(in), optional        :: axes2

        real(wp),  intent(in), dimension(:), optional  :: x3
        real(wp),  intent(in), dimension(:), optional  :: y3
        character(len=*),  intent(in), optional        :: ls3
        character(len=*),  intent(in), optional        :: axes3

        real(wp),  intent(in), dimension(:), optional  :: x4
        real(wp),  intent(in), dimension(:), optional  :: y4
        character(len=*),  intent(in), optional        :: ls4
        character(len=*),  intent(in), optional        :: axes4
        this%plotscale='semilogx'
        call plot2d_vector_vs_vector(this, &
            x1, y1, ls1, axes1, &
            x2, y2, ls2, axes2, &
            x3, y3, ls3, axes3, &
            x4, y4, ls4, axes4  )
        ! Set the plot scale as linear. It means log scale is off
        this%plotscale='linear'

    end subroutine semilogxv


    !..............................................................................
    subroutine semilogyv(this, x1, y1, ls1, axes1, &
            x2, y2, ls2, axes2, &
            x3, y3, ls3, axes3, &
            x4, y4, ls4,axes4  )
        !..............................................................................
        !   This procedure is the same as plotXY with logarithmic y1 and y2 axes
        !..............................................................................

        class(gpf):: this
        ! Input vector
        real(wp),  intent(in)                          :: x1(:)  ! vector of data for x
        real(wp),  intent(in), optional                :: y1(:)  ! vector of data for y
        character(len=*),  intent(in), optional        :: ls1    ! line specification
        character(len=*),  intent(in), optional        :: axes1

        real(wp),  intent(in), dimension(:), optional  :: x2
        real(wp),  intent(in), dimension(:), optional  :: y2
        character(len=*),  intent(in), optional        :: ls2
        character(len=*),  intent(in), optional        :: axes2

        real(wp),  intent(in), dimension(:), optional  :: x3
        real(wp),  intent(in), dimension(:), optional  :: y3
        character(len=*),  intent(in), optional        :: ls3
        character(len=*),  intent(in), optional        :: axes3

        real(wp),  intent(in), dimension(:), optional  :: x4
        real(wp),  intent(in), dimension(:), optional  :: y4
        character(len=*),  intent(in), optional        :: ls4
        character(len=*),  intent(in), optional        :: axes4

        this%plotscale='semilogy'
        call plot2d_vector_vs_vector(this, &
            x1, y1, ls1, axes1, &
            x2, y2, ls2, axes2, &
            x3, y3, ls3, axes3, &
            x4, y4, ls4, axes4  )
        ! Set the plot scale as linear. It means log scale is off
        this%plotscale='linear'


    end subroutine semilogyv



    subroutine loglogv(this, x1, y1, ls1, axes1, &
            x2, y2, ls2, axes2, &
            x3, y3, ls3, axes3, &
            x4, y4, ls4, axes4  )
        !..............................................................................
        !   This procedure is the same as plotXY with logarithmic x1, y1, x2, y2 axes
        !..............................................................................

        class(gpf):: this
        ! Input vector
        real(wp),  intent(in)                          :: x1(:)  ! vector of data for x
        real(wp),  intent(in), optional                :: y1(:)  ! vector of data for y
        character(len=*),  intent(in), optional        :: ls1    ! line specification
        character(len=*),  intent(in), optional        :: axes1

        real(wp),  intent(in), dimension(:), optional  :: x2
        real(wp),  intent(in), dimension(:), optional  :: y2
        character(len=*),  intent(in), optional        :: ls2
        character(len=*),  intent(in), optional        :: axes2

        real(wp),  intent(in), dimension(:), optional  :: x3
        real(wp),  intent(in), dimension(:), optional  :: y3
        character(len=*),  intent(in), optional        :: ls3
        character(len=*),  intent(in), optional        :: axes3

        real(wp),  intent(in), dimension(:), optional  :: x4
        real(wp),  intent(in), dimension(:), optional  :: y4
        character(len=*),  intent(in), optional        :: ls4
        character(len=*),  intent(in), optional        :: axes4


        this%plotscale='loglog'
        call plot2d_vector_vs_vector(this, &
            x1, y1, ls1, axes1, &
            x2, y2, ls2, axes2, &
            x3, y3, ls3, axes3, &
            x4, y4, ls4, axes4  )
        ! Set the plot scale as linear. It means log scale is off
        this%plotscale='linear'

    end subroutine loglogv



    subroutine  semilogxm(this, xv, ymat, lspec)
        !..............................................................................
        !Plots a matrix against a vector with logarithmic x-axis
        !For more information see plot2D_matrix_vs_vector procedure
        !Everything is the same except the x-axis scale
        !..............................................................................

        implicit none
        class(gpf)                                :: this
        ! Input arrays
        real(wp),  intent(in)                     :: xv(:)
        real(wp),  intent(in)                     :: ymat(:,:)
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
        class(gpf)                                  :: this
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
        class(gpf)                                :: this
        ! Input arrays
        real(wp),  intent(in)                     :: xv(:)
        real(wp),  intent(in)                     :: ymat(:,:)
        character(len=*),  intent(in), optional   :: lspec

        this%plotscale='loglog'
        call plot2d_matrix_vs_vector(this, xv,ymat, lspec)
        ! Set the plot scale as linear. It means log scale is off
        this%plotscale='linear'


    end subroutine loglogm



    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Three: Animation Routines
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    subroutine sub_animation_start(this, pause_seconds)
        !-------------------------------------------------------------------------------
        ! sub_animation_start: set the setting to start an animation
        ! it simply set flags and open a script file to write data
        !-------------------------------------------------------------------------------
        class(gpf)                    :: this
        integer, intent(in), optional :: pause_seconds


        ! ogpf does not support multiplot with animation at the same time
        if (this%hasmultiplot) then
            print*, md_name // ': does not support animation in multiplot mode!'
            stop
        end if


        if (present(pause_seconds)) then
            this%pause_seconds = pause_seconds
        else
            this%pause_seconds = 2  ! delay in second
        end if

        this%frame_number = 0

        ! create the ouput file for writting gnuplot script
        call create_outputfile(this)
        this%hasfileopen  = .true.
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

        call finalize_plot(this)

    end subroutine sub_animation_show




    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Four: Gnuplot direct scriptting
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    subroutine addscript(this,strcmd)
        !..............................................................................
        ! addscript: accepts all type of gnuplot command as a string and store it
        ! in global txtscript to be later sent to gnuplot
        !..............................................................................

        class(gpf)                   :: this
        character(len=*), intent(in) :: strcmd

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
        call create_outputfile(this)

        !write the script
        call processcmd(this)
        write(unit=this%file_unit, fmt='(a)') this%txtscript

        ! close the file and call gnuplot
        call finalize_plot(this)

    end subroutine runscript



    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Five: gnuplot command processing and data writing to script file
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    subroutine process_axes_set(axes_set, axes)
        !..............................................................................
        ! process_axesspec accepts the axes set and interpret it into
        ! a format to be sent to gnuplot.
        ! the axes set can be one of the following set
        ! x1y1, x1y2, x2y1, x2y2
        !..............................................................................

        character(len=*), intent(in)  :: axes_set
        character(len=4), intent(out) :: axes


        if (len_trim (adjustl(axes_set)) == 0) then
            axes=''
            return
        end if

        select case ( lcase(trim (adjustl (axes_set) ) ) )
            case ('x1y1')
                axes='x1y1'
            case ('x1y2')
                axes='x1y2'
            case ('x2y1')
                axes='x2y1'
            case ('x2y2')
                axes='x2y2'
            case default ! wrong strings
                print*, md_name // ':process_axes_set:' // ' wrong axes set is sent.'// new_line(' ') &
                    // 'axes set can be on of: x1y1, x1y2, x2y1, x2y2'
                axes=''
                return
        end select

    end subroutine process_axes_set



    subroutine process_linespec(order, lsstring, lspec, axes_set)
        !..............................................................................
        ! process_linespec accepts the line specification and interpret it into
        ! a format to be sent to gnuplot
        !..............................................................................

        integer, intent(in) :: order !1 for the first data series
        character(len=*), intent(out) :: lsstring
        character(len=*), intent(in), optional :: lspec
        character(len=*), intent(in), optional :: axes_set

        !local variables
        character(len=4)  :: axes
        character(len=10) :: axes_setting

        !check the axes set
        axes_setting = ''
        if ( present (axes_set)) then
            call process_axes_set(axes_set, axes)
            if (len(trim(axes))> 0 ) then
                axes_setting = ' axes ' // axes
            end if
        end if

        select case(order)
            case(1)
                if ( present(lspec) ) then
                    if (hastitle(lspec)) then
                        lsstring='plot "-" '//trim(lspec) // axes_setting
                    else
                        lsstring='plot "-" notitle '//trim(lspec) // axes_setting
                    end if
                else
                    lsstring='plot "-" notitle' // axes_setting
                end if
            case  default !e.g. 2, 3, 4, ...
                if (present(lspec)) then
                    if (hastitle(lspec)) then
                        lsstring=', "-" '// trim(lspec) // axes_setting
                    else
                        lsstring=', "-" notitle '// trim(lspec) // axes_setting
                    end if
                else
                    lsstring=', "-" notitle' // axes_setting
                end if
        end select
    end subroutine process_linespec



    subroutine processcmd(this)
        !..............................................................................
        !   This subroutine writes all the data into plot file
        !   to be read by gnuplot
        !..............................................................................

        class(gpf) :: this

        ! write the plot style for data
        ! this is used only when 3D plots (splot, cplot) is used
        if (allocated(this%txtdatastyle)) then
            write ( this%file_unit, '(a)' ) 'set style data '//this%txtdatastyle  !set data style
            write ( this%file_unit, '(a)' )  ! emptyline
        end if


        ! Write options
        if ( this%hasoptions ) then
            write( unit = this%file_unit, fmt= '(a)') ' '
            write( unit = this%file_unit, fmt= '(a)') '# options'
            write( unit = this%file_unit, fmt= '(a)') this%txtoptions
            write ( this%file_unit, '(a)' )  ! emptyline
        end if

        ! Check with plot scale: i.e linear, logx, logy, or log xy
        write( unit = this%file_unit, fmt= '(a)') ' '
        write( unit = this%file_unit, fmt= '(a)') '# plot scale'
        select case (this%plotscale)
            case ('semilogx')
                write ( this%file_unit, '(a)' ) 'set logscale  x'
            case ('semilogy')
                write ( this%file_unit, '(a)' ) 'set logscale  y'
            case ('loglog')
                write ( this%file_unit, '(a)' ) 'set logscale  xy'
            case default !for no setting
                !pass
        end select

        !!>0.22
        ! write annotation
        write( unit = this%file_unit, fmt= '(a)') ' '
        write( unit = this%file_unit, fmt= '(a)') '# Annotation: title and labels'
        call write_label(this, 'plot_title')
        call write_label(this, 'xlabel'    )
        call write_label(this, 'x2label'   )
        call write_label(this, 'ylabel'    )
        call write_label(this, 'y2label'   )
        call write_label(this, 'zlabel'    )

        ! axes range
        write( unit = this%file_unit, fmt= '(a)') ' '
        write( unit = this%file_unit, fmt= '(a)') '# axes setting'
        if (this%hasxrange) then
            write ( this%file_unit, '(a,f0.0,a,f0.0,a)' ) 'set xrange [',this%xrange(1),':',this%xrange(2),']'
        end if
        if (this%hasyrange) then
            write ( this%file_unit, '(a,f0.0,a,f0.0,a)' ) 'set yrange [',this%yrange(1),':',this%yrange(2),']'
        end if
        if (this%haszrange) then
            write ( this%file_unit, '(a,f0.0,a,f0.0,a)' ) 'set zrange [',this%zrange(1),':',this%zrange(2),']'
        end if

        ! secondary axes range
        if (this%hasx2range) then
            write ( this%file_unit, '(a,f0.0,a,f0.0,a)' ) 'set x2range [',this%x2range(1),':',this%x2range(2),']'
        end if
        if (this%hasy2range) then
            write ( this%file_unit, '(a,f0.0,a,f0.0,a)' ) 'set y2range [',this%y2range(1),':',this%y2range(2),']'
        end if
        ! finish by new line
        write ( this%file_unit, '(a)' )  ! emptyline

    end subroutine processcmd



    subroutine write_label(this, lblname)
        !..............................................................................
        !   This subroutine writes the labels into plot file
        !   to be read by gnuplot
        !..............................................................................


        ! write_label
        class(gpf)                    :: this
        character(len=*)              :: lblname

        ! local var
        character(len=:), allocatable :: lblstring
        character(len=:), allocatable :: lblset
        type(tplabel)                 :: label

        select case (lblname)
            case ('xlabel')
                if (.not. (this%tpxlabel%has_label) ) then
                    return ! there is no label
                end if
                lblset = 'set xlabel "'
                label = this%tpxlabel
            case ('x2label')
                if (.not. (this%tpx2label%has_label) ) then
                    return ! there is no label
                end if
                lblset = 'set x2label "'
                label = this%tpx2label
            case ('ylabel')
                if (.not. (this%tpylabel%has_label) ) then
                    return ! there is no label
                end if
                lblset = 'set ylabel "'
                label = this%tpylabel
            case ('y2label')
                if (.not. (this%tpy2label%has_label) ) then
                    return ! there is no label
                end if
                lblset = 'set y2label "'
                label = this%tpy2label
            case ('zlabel')
                if (.not. (this%tpzlabel%has_label) ) then
                    return ! there is no label
                end if
                lblset = 'set zlabel "'
                label = this%tpzlabel
            case ('plot_title')
                if (.not. (this%tpplottitle%has_label) ) then
                    return ! there is no label
                end if
                lblset = 'set title "'
                label = this%tpplottitle
        end select

        lblstring = ''
        ! if there is a label continue to set it
        lblstring                  = lblstring // lblset // trim(label%lbltext)//'"'
        if (allocated(label%lblcolor)) then
            lblstring              = lblstring // ' tc "' //trim(label%lblcolor) // '"'
        end if
        ! set font and size
        if (allocated(this%tpxlabel%lblfontname)) then
            lblstring              = lblstring // ' font "'// trim(label%lblfontname) // ','
            if (label%lblfontsize /= NOT_INITIALIZED) then
                lblstring          = lblstring // num2str(label%lblfontsize) //'"'
            else
                lblstring          = lblstring //'"'
            end if
        else ! check if only font size has been given
            if (label%lblfontsize /= NOT_INITIALIZED ) then
                lblstring          = lblstring // ' font ",' // num2str(label%lblfontsize) //'"'
            end if
        end if
        ! set rotation
        if (label%lblrotate       /= NOT_INITIALIZED ) then
            lblstring              = lblstring // ' rotate by ' // num2str(label%lblrotate )
        end if


        ! write to ogpf script file
        write ( this%file_unit, '(a)' ) lblstring


    end subroutine write_label



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
        character(len=1)              :: strnumber
        character(len=11)             :: strblank
        integer                       :: j
        integer                       :: maxcolors

        ! define the color palettes
        character(len=:), allocatable :: pltname
        character(len=7)              :: palette(9) ! palettes with maximum 9 colors

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
        str                 = '# Define the ' // pltname // ' pallete' // new_line(' ')
        str                 = str // 'set palette defined ( \' // new_line(' ')
        strblank            = '           ' ! pad certain number of paces
        do j=1, maxcolors - 1
            write(unit      =strnumber, fmt='(I1)' ) j-1
            str             = str // strblank // strnumber // ' "' // palette(j) // '",\' // new_line(' ')
        end do

        j                   =maxcolors
        write(strnumber, fmt='(I1)') j
        str                 = str // strblank // strnumber // ' "' // palette(j) // '" )' // new_line(' ')

    end function color_palettes



    subroutine write_xydata(file_unit,ndata,x,y)
        !..............................................................................
        ! Writes set of xy data into a file
        !..............................................................................

        integer,  intent(in)           ::  file_unit
        integer,  intent(in)           ::  ndata
        real(wp), intent(in)           ::  x(:)
        real(wp), intent(in), optional ::  y(:)

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



    subroutine create_outputfile(this)
        !..............................................................................
        ! Create an output file, assign a file_unit
        ! for writing the gnuplot commands
        !..............................................................................

        ! Rev 0.18
        class(gpf), intent(inout)   :: this

        if (this%hasfileopen) then
            ! there is nothing to do, file has been already open!
            return
        end if

        !> Rev 0.2 animation

        ! animation handling
        if (this%hasanimation ) then
            this%frame_number = this%frame_number + 1 ! for future use
        end if

        ! Open the output file

        if (.not. (this%hasfilename)) then ! check if no file has been set by user
            this%txtfilename=gnuplot_output_filename
        end if

        open ( newunit = this%file_unit, file = this%txtfilename, status = 'replace', iostat = this%status )


        if (this%status /= 0 ) then
            print*, "md_helperproc, create_outputfile: cannot open file for output"
            stop
        end if


        ! Set the gnuplot terminal, write ogpf configuration (customized setting)
        ! Can be overwritten by options

        ! write signature
        write ( this%file_unit, '(a)' ) '# ' // md_name
        write ( this%file_unit, '(a)' ) '# ' // md_rev
        write ( this%file_unit, '(a)' ) '# ' // md_lic
        write ( this%file_unit, '(a)' )  ! emptyline

        ! write the global settings
        write ( this%file_unit, '(a)' ) '# gnuplot global setting'
        write(unit=this%file_unit, fmt='(a)') 'set term ' // gnuplot_term_type // &
            ' size ' // gnuplot_term_size // ' enhanced font "' // &
            gnuplot_term_font // '"' // &
            ' title "' // md_name // ': ' // md_rev //'"'   ! library name and version

        ! write the preset configuration for gnuplot (ogpf customized settings)
        if (this%preset_configuration) then
            call this%preset_gnuplot_config()
        end if
        ! write multiplot setting
        if (this%hasmultiplot) then
            write(this%file_unit, fmt='(a, I2, a, I2)') 'set multiplot layout ', &
                this%multiplot_rows, ',',  this%multiplot_cols
        end if
        ! set flag true for file is opened
        this%hasfileopen = .true.

    end subroutine create_outputfile


    subroutine preset_gnuplot_config(this)
        !..............................................................................
        ! To write the preset configuration for gnuplot (ogpf customized settings)
        !..............................................................................
        class(gpf) :: this

        write(this%file_unit, fmt='(a)')
        write(this%file_unit, fmt='(a)') '# ogpf extra configuration'
        write(this%file_unit, fmt='(a)') '# -------------------------------------------'


        ! color definition
        write(this%file_unit, fmt='(a)') '# color definitions'
        write(this%file_unit, fmt='(a)') 'set style line 1 lc rgb "#800000" lt 1 lw 2'
        write(this%file_unit, fmt='(a)') 'set style line 2 lc rgb "#ff0000" lt 1 lw 2'
        write(this%file_unit, fmt='(a)') 'set style line 3 lc rgb "#ff4500" lt 1 lw 2'
        write(this%file_unit, fmt='(a)') 'set style line 4 lc rgb "#ffa500" lt 1 lw 2'
        write(this%file_unit, fmt='(a)') 'set style line 5 lc rgb "#006400" lt 1 lw 2'
        write(this%file_unit, fmt='(a)') 'set style line 6 lc rgb "#0000ff" lt 1 lw 2'
        write(this%file_unit, fmt='(a)') 'set style line 7 lc rgb "#9400d3" lt 1 lw 2'
        write(this%file_unit, fmt='(a)')
        ! axes setting
        write(this%file_unit, fmt='(a)') '# Axes'
        write(this%file_unit, fmt='(a)') 'set border linewidth 1.15'
        write(this%file_unit, fmt='(a)') 'set tics nomirror'
        write(this%file_unit, fmt='(a)')

        write(this%file_unit, fmt='(a)') '# grid'
        write(this%file_unit, fmt='(a)') '# Add light grid to plot'
        write(this%file_unit, fmt='(a)') 'set style line 102 lc rgb "#d6d7d9" lt 0 lw 1'
        write(this%file_unit, fmt='(a)') 'set grid back ls 102'
        write(this%file_unit, fmt='(a)')
        ! set the plot style
        write(this%file_unit, fmt='(a)') '# plot style'
        write(this%file_unit, fmt='(a)') 'set style data linespoints'
        write(this%file_unit, fmt='(a)')

        write(this%file_unit, fmt='(a)') '# -------------------------------------------'
        write(this%file_unit, fmt='(a)') ''


    end subroutine preset_gnuplot_config



    subroutine finalize_plot(this)
        !..............................................................................
        ! To finalize the writing of gnuplot commands/data and close the output file.
        !..............................................................................
        class(gpf) :: this

        ! check for multiplots
        if (this%hasmultiplot) then
            if (this%multiplot_total_plots < this%multiplot_rows * this%multiplot_cols - 1 ) then
                ! increment the number of plots
                this%multiplot_total_plots = this%multiplot_total_plots + 1
                return ! do not finalize plot, still there is places in multiplot
            else
                ! close multiplot
                write(this%file_unit, fmt='(a)') 'unset multiplot'
                ! reset multiplot flag
                this%hasmultiplot = .false.

            end if
        end if

        close ( unit = this%file_unit )   ! close the script file
        this%hasfileopen = .false.        ! reset file open flag
        this%hasanimation = .false.
        ! Use shell command to run gnuplot
        call execute_command_line ('gnuplot -persist ' // this%txtfilename)  !   Now plot the results

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
        !..............................................................................
        ! splitstring splits a string to an array of
        ! substrings based on a selected delimiter
        ! note:
        !    a. any facing space/blank in substrings will be removed
        !    b. two adjacent delimiter treats as an empty substring between them
        !    c. facing and trailing delimiter treats as empty substring at the fornt and end
        !..............................................................................

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


    function num2str_i4(number_in)
        !..............................................................................
        ! num2str_int: converts integer number to string
        !..............................................................................

        integer(kind=4), intent(in)     :: number_in
        character(len=:), allocatable   :: num2str_i4

        ! local variable
        character(len=range(number_in)) :: strnm
        write(unit=strnm, fmt='(I0)') number_in
        num2str_i4 = trim(strnm)

    end function num2str_i4

    function num2str_r4(number_in, strfmt)
        !..............................................................................
        ! num2str_r4: converts single precision real number to string
        ! strfmt is the optional format string
        !..............................................................................

        real(kind=4), intent(in)                :: number_in
        character(len=*), intent(in), optional  :: strfmt
        character(len=:), allocatable           :: num2str_r4

        ! local variable
        character(len=range(number_in)) :: strnm


        if (present(strfmt)) then
            write(unit=strnm, fmt= '('//trim(strfmt)//')' ) number_in
        else
            write(unit=strnm, fmt='(G0)') number_in
        end if

        num2str_r4 = trim(strnm)

    end function num2str_r4


    function num2str_r8(number_in, strfmt)
        !..............................................................................
        ! num2str_real: converts double precision real number to string
        ! strfmt is the optional format string
        !..............................................................................

        real(kind=8), intent(in)                :: number_in
        character(len=*), intent(in), optional  :: strfmt
        character(len=:), allocatable           :: num2str_r8

        ! local variable
        character(len=range(number_in)) :: strnm

        if (present(strfmt)) then
            write(unit=strnm, fmt= '('//trim(strfmt)//')' ) number_in
        else
            write(unit=strnm, fmt='(G0)') number_in
        end if

        num2str_r8 = trim(strnm)

    end function num2str_r8


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
