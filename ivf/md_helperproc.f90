module md_helperproc
    use select_precision, only: wp

    implicit none
    character(len=*), parameter, private :: md_name='md_helperproc'
    !     private
    !    public :: process_linespec, &
        !           meshgrid, &
        !           linspace, &
        !           checkDim, &
        !           lcase


contains
    !..............................................................................
    subroutine write2gnuplot(filename, persist)
        !..............................................................................
        !   This subroutine call gnuplot through system command
        implicit none
        character(len=*):: filename
        logical, intent(in) :: persist

        !local vars
        ! TODO (MOhammad#1): To be corected, Error pron persist processing
        if  (persist) then
            !Fortran standard recommend to use call execute_command_line to invoke another program
            !from within Fortran, the old method was to use call system
            !Here by default Fortran standard is used, if you have a compiler does not support
            !call execute_command_line, uncomment the following call system

            !CALL system('gnuplot -persist '//fileName)              !Obsolete method, use with old compilers
            call execute_command_line('gnuplot -persist '//filename) !Fortran standard
        else
            !CALL system ('gnuplot '//fileName) !Obsolete method, use with old compilers
            call execute_command_line ('gnuplot '//filename)
        end if



        ! TODO (Mohammad#1#12/22/17): The persist has a problem, to be corrected!

        !   The following lines actually do the deletion of temporary file
        !   This method is used to have a portable code!
        !
        !OPEN ( Newunit = lun, FILE = fileName, STATUS = 'old',IOSTAT = ios )
        !IF (ios /= 0 ) THEN
        !    PRINT*, "gpf error: cannot open file for output"
        !    RETURN !An error has been occurred
        !END IF
        !Close(lun,status='keep')  !delete file

    end subroutine write2gnuplot


    subroutine create_outputfile(filename, file_unit)
        ! Create an output file, assign a file_unit
        ! for writing the gnuplot commands
        ! Rev 0.18
        character(len=*), intent(in)  :: filename
        integer, intent(out)          :: file_unit

        !local variables
        integer :: ierr

        ! Open the output file
        open ( newunit = file_unit, file = filename, status = 'replace', iostat = ierr )

        if (ierr /= 0 ) then
            print*, "md_helperproc, create_outputfile: cannot open file for output"
            stop
        end if


    end subroutine create_outputfile


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


    !..............................................................................
    subroutine process_linespec(order, lsstring, lspec)
        !..............................................................................
        !Accepts the line specification and interpret it into a format
        !to be sent to gnuplot

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



    !!!
    !!!!..............................................................................
    !!!    subroutine ProcessCmd(this, file_unit)
    !!!!..............................................................................
    !!!    !   This subroutine writes all the data into plot file
    !!!    !   to be read by gnuplot
    !!!
    !!!    class(gpf), intent(in)  :: this
    !!!    integer, intent(in)     :: file_unit
    !!!    ! The following lines set the gnuplot terminal
    !!!    ! The data style to lines+symbols:linespoints
    !!!    ! Can be overwritten by options
    !!!
    !!!     ! Write options
    !!!    if ( this%hasOptions ) then
    !!!        call WriteString(trim(this%txtOptions),file_unit)
    !!!    end if
    !!!    ! Check with plot scale: i.e linear, logx, logy, or log xy
    !!!    select case (this%plotscale)
    !!!    case ('semilogx')
    !!!        write ( file_unit, '(a)' ) 'set logscale  x'
    !!!    case ('semilogy')
    !!!        write ( file_unit, '(a)' ) 'set logscale  y'
    !!!    case ('loglog')
    !!!        write ( file_unit, '(a)' ) 'set logscale  xy'
    !!!    case default !For linear xy plot or 3D plots
    !!!        !pass
    !!!    end select
    !!!    !   Write the plot options to script file
    !!!
    !!!    if (this%hasxrange) then
    !!!        write ( file_unit, '(a,g0,a,g0,a)' ) 'set xrange [',this%xrange(1),':',this%xrange(2),']'
    !!!    end if
    !!!
    !!!    if (this%hasyrange) then
    !!!        write ( file_unit, '(a,g0,a,g0,a)' ) 'set yrange [',this%yrange(1),':',this%yrange(2),']'
    !!!    end if
    !!!
    !!!    if (this%haszrange) then
    !!!        write ( file_unit, '(a,g0,a,g0,a)' ) 'set xrange [',this%zrange(1),':',this%zrange(2),']'
    !!!    end if
    !!!
    !!!    if (this%hasPlotTitle) then
    !!!        write ( file_unit, '(a)' ) 'set title  "' // trim(this%txtPlotTitle)// '"'
    !!!    end if
    !!!    if (this%hasXlabel) then
    !!!        write ( file_unit, '(a)' ) 'set xlabel "'// trim(this%txtXlabel)//'"'
    !!!    end if
    !!!    if (this%hasYlabel) then
    !!!        write ( file_unit, '(a)' ) 'set ylabel "'//trim(this%txtYlabel)//'"'
    !!!    end if
    !!!    if (this%hasZlabel) then
    !!!        write ( file_unit, '(a)' ) 'set zlabel "'//trim(this%txtZlabel)//'"'
    !!!    end if
    !!!
    !!!    end subroutine ProcessCmd
    !!!


    !..............................................................................
    subroutine write_xydata(file_unit, ndata, x, y)
        ! Writes set of xy data into a file
        integer, intent(in)     :: file_unit
        integer, intent(in)     :: ndata
        real(wp), intent(in)            :: x(:)
        real(wp), intent(in), optional  :: y(:)
        !local variable
        integer:: i

        ! TODO (Mohammad#1#12/22/17): The format string shall be modified to write the number in more suitable form
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



    !..............................................................................
    subroutine writestring(file_unit, str)
        ! WriteString, separate a string using ";" delimiter
        ! and writes each statement in a separate line in the file indicated by file_unit
        ! This routine is used for writting the general options to gnuplot script file
        integer, intent(in)             :: file_unit
        character(len=*), intent(in)    :: str
        !local variable
        character, parameter              :: delimiter=';'
        integer ::  n
        integer ::  m
        integer ::  k


        k=len_trim(str) !length with removed trailing blanks
        n=scan(str,delimiter)
        if (n==0) then  !This is a single statement
            write(file_unit,'(a)') str
            return
        end if
        m=1
        do while (n/=0 .and. m<k)
            if (n/=1) then
                write(file_unit,'(a)') str(m:m+n-2)
            end if
            m=n+m
            n=scan(str(m:k),delimiter)
        end do
        if (m<k) then !write the last statement
            write(file_unit,'(a)') str(m:k)
        end if
    end subroutine writestring


    !..............................................................................
    function hastitle(string)

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

    !..............................................................................
    function lcase(strinput)

        ! Return the string (strInput) in lowercase
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

    !..............................................................................
    function checkdim(nx,ny)
        !..............................................................................
        ! This is a helper function
        ! It checks the equality of dimensions of two vector
        integer, intent(in):: nx
        integer, intent(in):: ny
        logical:: checkdim
        if (nx/=ny) then
            checkdim=.false.
        else
            checkdim=.true.
        end if

    end function checkdim




! Helper functions


    function arange(a, b, dx) result(u)
        ! returns an array u = [a, a+dx, a+2*dx, ..., b-dx]
        ! if dx is absent, then it will be assumed as 1.0
        ! Based on a code by Ondrej Certik

        real(wp), intent(in) :: a, b
        real(wp), intent(in), optional :: dx
        real(wp), allocatable :: u(:)
        !
        ! Example
        ! -------
        !
        ! real(dp), allocatable :: u(:)
        ! u=arange(1, 5, 1, u)   ! u = [1, 2, 3, 4]

        ! local variable
        integer  :: n, i, ierr
        real(wp) :: dxl

        if (present(dx)) then
            dxl = dx
        else
            dxl = 1.0_wp
        end if

        n = int((b-a) / dxl)

        allocate(u(n), stat=ierr)
        if (ierr /= 0) then
            print '(a)', md_name // ': arange: ' // 'allocatation failed'
            stop
        end if
        do i = 1, n
            u(i) = a + (i-1)*dxl
        end do

    end function arange

    !..............................................................................
    function linspace(a,b,n_elements)
        !..............................................................................
        !   returns a linearly spaced vector with n points in [a, b]
        !   if n is omitted, 100 points will be considered
        !
        real(wp), intent(in) ::a
        real(wp), intent(in) :: b
        integer, intent(in), optional ::  n_elements
        real(wp), allocatable:: linspace(:)
        !   Local vars
        real(wp):: dx
        integer:: i
        integer:: n
        integer:: ierr
        if (present(n_elements)) then
            n=n_elements
        else
            n=100
        end if

        allocate(linspace(n), stat=ierr)
        if (ierr /= 0) then
            print*, "Fatal Error, Allocation failed in linspace function"
            return
        end if
        dx=(b-a)/real((n-1),wp)
        linspace=[(i*dx+a, i=0,n-1)]
    end function linspace





    !..............................................................................
    subroutine meshgrid(x,y,xgv,ygv, ierr)
        !..............................................................................
        !meshgrid generate mesh grid over a rectangular domain of [xmin xmax, ymin, ymax]
        ! Inputs:
        !     xgv, ygv are vector in form of [start, stop, step] or [start, stop]
        !     when step has not been given, nx=m and ny=n are used to calculate steps
        !     and generate nx and ny data points respectively.
        !     The default value for both nx, ny is 25.
        ! Outputs:
        !     X and Y are matrix each of size [ny by nx] contains the grid data.
        !     The coordinates of point (i,j) is [X(i,j), Y(i,j)]
        !     ierr: The error flag
        !     """
        !     # Example
        !     # call meshgrid(X, Y, [0,3,1],[5,8,1])
        !     # X
        !     # [[0.0, 1.0, 2.0, 3.0],
        !     # [0.0, 1.0, 2.0, 3.0],
        !     # [0.0, 1.0, 2.0, 3.0],
        !     # [0.0, 1.0, 2.0, 3.0]]
        !     #
        !     #Y
        !     #[[5.0, 5.0, 5.0, 5.0],
        !     # [6.0, 6.0, 6.0, 6.0],
        !     # [7.0, 7.0, 7.0, 7.0],
        !     # [8.0, 8.0, 8.0, 8.0]]




        ! Arguments
        real(wp), intent(out), allocatable  :: x(:,:)
        real(wp), intent(out), allocatable  :: y(:,:)
        real(wp), intent(in)                :: xgv(:)
        real(wp), intent(in),  optional     :: ygv(:)
        integer,  intent(out), optional     :: ierr !Return the error flag
        ! Local variables
        integer:: i
        integer:: sv
        integer:: nx
        integer:: ny
        integer:: m
        integer:: n
        real(wp):: dx
        real(wp):: dy
        logical:: only_x_available

        ! Initial setting
        m=25
        n=25
        only_x_available=.false.
        sv=0 !Assume no error
        select case(size(xgv))
            case (2) ! The step has not given
                nx=m
                dx=(xgv(2)-xgv(1))/real(nx-1, wp)
            case (3)
                dx=xgv(3)
                nx=int((xgv(2)-xgv(1))/dx)+1
            case default
                print*, "wrong x vector to meshgrid"
                sv=1
        end select

        if (present(ygv)) then
            select case(size(ygv))
                case (2)
                    ny=n
                    dy=(ygv(2)-ygv(1))/real(ny-1, wp)
                case (3)
                    dy=ygv(3)
                    ny=int((ygv(2)-ygv(1))/dy)+1
                case default
                    print*, "wrong y vector to meshgrid"
                    sv=1
            end select
        else
            only_x_available=.true.
            ny=nx
        end if

        allocate(x(ny,nx),y(ny,nx),stat=sv)
        if (sv /=0) then
            print*, "allocation error in meshgrid"
        end if

        x(1,:)=[(xgv(1)+real((i-1),wp)*dx, i=1, nx)]
        x(2:ny,:)=spread(x(1,:),dim=1,ncopies=ny-1)

        if (only_x_available) then
            y=transpose(x)
        else
            y(:,1)=[(ygv(1)+real((i-1),wp)*dy, i=1, ny)]
            y(:,2:nx)=spread(y(:,1),dim=2,ncopies=nx-1)
        end if

        if (present(ierr)) then
            ierr=sv
        end if

    end subroutine meshgrid




end module md_helperproc
