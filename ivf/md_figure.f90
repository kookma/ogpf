module md_figure

    use md_parameters     !load gnuplot constants
    use md_helperproc
    use md_plot

    implicit none

    private
    public tpfigure

    type tpmultiplot
        logical :: set    ! true means there is a multiplot
        integer :: rows   ! number of rows in multiplot
        integer :: cols   ! number of cols
        integer :: loc    ! current location of plot in multiplot layout
        logical :: overwriten ! plots are more than the multiplot spaces
    end type tpmultiplot

    type tpfigure
        ! private

        character(len=:), allocatable :: fig_title
        !Terminal properties
        character(len=:), allocatable   :: term_type
        integer                         :: term_number
        integer                         :: term_size(2)
        character(len=7)                :: term_bgcolor

        !Multiplot properties
        type(tpmultiplot) :: multiplot

        type(tpplot), allocatable :: plots(:) !Number of plots in one figure
        !Other properties


    contains
        procedure, pass :: sub_constructor
        procedure, pass :: set_fig_title
        procedure, pass :: set_multiplot_rows
        procedure, pass :: set_multiplot_cols
        procedure, pass :: sub_allocate_plots

        procedure, pass :: set_terminal_type
        procedure, pass :: set_terminal_size
        procedure, pass :: set_terminal_bgcolor

        procedure, pass :: sub_write_term_setting
        procedure, pass :: sub_write_multiplot_set_unset

    end type


contains


    !..............................................................................
    subroutine sub_allocate_plots(this, number_of_plots)
        ! allocate the plots for a figure! This is works with multiplot
        ! when there is no multiplot layout, a single plot is allocated
        !..............................................................................
        class(tpfigure):: this
        integer, intent(in), optional :: number_of_plots
        !local vars
        integer :: ierr

        if (.not. present(number_of_plots)) then
            return
        end if
        if (number_of_plots <= 0) then
            print*, 'md_figure, sub_allocate_plots: wrong input for the number_of_plots'
            stop
        end if
        if (allocated(this%plots)) then
            print*, 'md_figure, sub_allocate_plots plots are already allocated'
            stop
        end if
        allocate(this%plots(number_of_plots), stat=ierr)
        if (ierr /=0) then
            print*, 'md_figure, sub_allocate_plots: allocation error '
            stop
        end if


    end subroutine sub_allocate_plots


    !..............................................................................
    subroutine set_fig_title(this, fig_title)
        !..............................................................................
        class(tpfigure):: this
        character(len=*), intent(in), optional :: fig_title
        if (.not. present(fig_title)) then
            return
        end if
        if (len(trim(fig_title))> 0) then
            this%fig_title=trim(fig_title)
        end if
    end subroutine set_fig_title


    !..............................................................................
    subroutine set_terminal_type(this, ttype)
        !..............................................................................
        class(tpfigure):: this
        character(len=*), intent(in), optional :: ttype

        if (.not. present(ttype)) then
            return
        end if
        select case (lcase(ttype))
            case ('wxt')
                this%term_type = 'wxt'
            case ('windows')
                this%term_type = 'windows'
            case default
                print*, 'gpf, md_figure Wrong terminal name'
                stop
        end select
    end subroutine set_terminal_type

    !..............................................................................
    subroutine set_terminal_size(this, tsize)
        !..............................................................................
        class(tpfigure):: this
        integer, intent(in), optional :: tsize(2)

        if (.not. present(tsize)) then
            return
        end if
        if ( tsize(1) > 0 .and. tsize(2)>0 ) then
            this%term_size=tsize
        else
            print*, 'gpf, md_figure Wrong terminal size'
            stop
        end if
    end subroutine set_terminal_size

    !..............................................................................
    subroutine set_terminal_bgcolor(this, bgcolor)
        !..............................................................................
        class(tpfigure):: this
        character(len=*), intent(in) :: bgcolor

        !TODO: to be developed later
        !to check the validity
        this%term_bgcolor = bgcolor

    end subroutine set_terminal_bgcolor

    !..............................................................................
    subroutine set_multiplot_rows(this, rows)
        !..............................................................................
        class(tpfigure):: this
        integer, intent(in) :: rows
        if (rows > 0) then
            !!!! this%mplt_rows=rows
            this%multiplot%rows = rows
        else
            print*, 'gpf, md_figure sub_multiplot: Wrong number of rows'
            stop
        end if
    end subroutine set_multiplot_rows

    !..............................................................................
    subroutine set_multiplot_cols(this, cols)
        !..............................................................................
        class(tpfigure):: this
        integer, intent(in) :: cols
        if (cols > 0) then
            this%multiplot%cols = cols
            !!!!this%mplt_cols = cols
        else
            print*, 'gpf, md_figure sub_multiplot: Wrong number of columns'
            stop
        end if
    end subroutine set_multiplot_cols


    !..............................................................................
    subroutine sub_constructor(this, fig_title, term_type, term_size, term_number, term_bgcolor)
        !..............................................................................
        class(tpfigure) :: this
        character(len=*), intent(in), optional :: fig_title
        character(len=*), intent(in), optional :: term_type
        integer,          intent(in), optional :: term_size(2)
        integer,          intent(in), optional :: term_number
        character(len=*), intent(in), optional :: term_bgcolor

        !local var

        if (present(fig_title)) then
            call this%set_fig_title (fig_title)
        else
            this%fig_title    =  gnuplot_term_title
        end if

        if (present(term_type)) then
            call this%set_terminal_type ( term_type)
        else
            this%term_type    =  gnuplot_term_type
        end if

        if (present(term_size)) then
            call this%set_terminal_size (term_size)
        else
            this%term_size    =  gnuplot_term_size
        end if


        if (present(term_bgcolor)) then
            call this%set_terminal_bgcolor (term_bgcolor)
        else
            this%term_bgcolor =  gnuplot_term_bgcolor
        end if

        if (present(term_number)) then
            this%term_number = term_number
        else
            this%term_number  =  0
        end if
        ! multiplot setting
        this%multiplot%set        = .false.
        this%multiplot%overwriten = .false.
        this%multiplot%loc        = 0
        this%multiplot%rows       = 1
        this%multiplot%cols       = 1


        ! Debug (Mohammad#1#02/08/18): print the term number
        print*, "md_figure: sub_constructor: term number: ", term_number

    end subroutine sub_constructor




    subroutine sub_write_term_setting(this, file_unit)
        ! write_term_setting
        ! the terminal settings are written to script file
        class(tpfigure)     :: this
        integer, intent(in) :: file_unit

        write(file_unit, fmt='(a,I3,a,I4,a,I4,a)') &
            'set term ' // trim(this%term_type), this%term_number, &
            ' size ', this%term_size(1), ',', this%term_size(2), &
            ' title "' // trim(this%fig_title) // '"'

    end subroutine sub_write_term_setting


    !..............................................................................
    subroutine sub_write_multiplot_set_unset(this, file_unit, setflag)
        !..............................................................................
        !Set and unset multiplot layout
        class(tpfigure) :: this
        integer, intent(in) :: file_unit
        logical, intent(in) :: setflag !true = set multiplot, flase = unset multiplot


        if (setflag) then !set multiplot
            write(file_unit,'(a,i2,a,i2)') 'set multiplot layout ', &
                this%multiplot%rows, &
                ',', this%multiplot%cols
        else !unset multiplot
            write(file_unit, '(a)') 'unset multiplot'
        end if

    end subroutine sub_write_multiplot_set_unset




end module
