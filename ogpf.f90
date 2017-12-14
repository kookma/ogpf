    !-------------------------------------------------------------------------------
    !    GnuPlot Interface
    !-------------------------------------------------------------------------------
    !    Purpose:   Object Based Interface to GnuPlot from Fortran (ogpf)
    !    Platform:  Windows XP/Vista/7/10
    !               (It should work on other platforms, see the Write2GnuPlot subroutine below)
    !    Language:  Fortran 2003 and 2008
    !    Requires:  1. Fortran 2003 compiler (e.g gfortran 4.7, IVF 12.1, ...)
    !               2. gnuplot 4.5 and higher (other previous version can be used
    !    Author:    Mohammad Rahmani
    !               Chem Eng Dep., Amirkabir Uni. of Tech
    !               Tehran, Ir
    !               url: aut.ac.ir/m.rahmani
    !               email: m[dot]rahmani[at]aut[dot]ac[dot]ir

    ! Revision History

    ! Version:  0.16
    ! Date:     Feb 11th, 2016
    !   Minor corrections
    !   Correct the lspec processing in plot2D_matrix_vs_vector
    !   Now, it is possible to send less line specification and gpf will cycle through lspec

    ! Version:  0.15
    ! Date:     Apr 20th, 2012
    !   Minor corrections
    !   Use of select_precision module and working precision: wp

    ! Version:  0.14
    ! Date:     Mar 28th, 2012
    !   Minor corrections
    !   Use of import keyboard and removing the Precision module
    !   Length of Title string increased by 80 chars


    ! Version:  0.13
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

MODULE ogpf
    use select_precision, only: wp
    IMPLICIT NONE
    PRIVATE
    PUBLIC linspace, meshgrid, wp

    ! Configuration parameters
    CHARACTER(LEN=*), PARAMETER :: gnuplot_terminal='wxt'             !Output terminal
    CHARACTER(LEN=*), PARAMETER :: gnuplot_font_name='calibri,10'     !font
    CHARACTER(LEN=*), PARAMETER :: gnuplot_plot_size='480,360'        !plot window size
    CHARACTER(LEN=*), PARAMETER :: gnuplot_datastyle='linespoints'    !data style
    CHARACTER(LEN=*), PARAMETER :: gnuplot_output_filename='ogpf_temp_script_file.plt'   !temporary file for output

!   Can be adjusted as required
    Integer, parameter :: len_options=320
    Integer, parameter :: len_labels=80
    Integer, parameter :: len_msg=80



    TYPE, PUBLIC :: gpf
        PRIVATE
        CHARACTER(LEN=len_labels)  :: txtPlotTitle=""
        CHARACTER(LEN=len_labels)  :: txtXlabel=""
        CHARACTER(LEN=len_labels)  :: txtYlabel=""
        CHARACTER(LEN=len_labels)  :: txtZlabel=""
        CHARACTER(LEN=len_options) :: txtOptions=""  !Four 80 characters lines
        LOGICAL            :: DisplayPlot=.TRUE.
        LOGICAL            :: Persist = .FALSE.
        LOGICAL            :: hasPlotTitle =.FALSE.
        LOGICAL            :: hasXlabel =   .FALSE.
        LOGICAL            :: hasYlabel =   .FALSE.
        LOGICAL            :: hasZlabel =   .FALSE.
        LOGICAL            :: hasxrange =   .FALSE.
        LOGICAL            :: hasyrange =   .FALSE.
        LOGICAL            :: haszrange =   .FALSE.
        LOGICAL            :: hasOptions =  .FALSE.
        REAL(wp)            :: xrange(2), yrange(2), zrange(2)

        CHARACTER(LEN=len_msg)      :: Msg=""   !Message from plot procedures
        INTEGER                     :: Status=0 !Status from plot procedures
        CHARACTER(LEN=25)           :: txtfileName=gnuplot_output_filename
        Character(Len=8)            :: plotscale


    CONTAINS
        PROCEDURE, PASS, PUBLIC :: options  => set_options
        PROCEDURE, PASS, PUBLIC :: title   => set_PlotTitle
        PROCEDURE, PASS, PUBLIC :: xlabel  => set_xlabel
        PROCEDURE, PASS, PUBLIC :: ylabel  => set_ylabel
        PROCEDURE, PASS, PUBLIC :: zlabel  => set_zlabel
        PROCEDURE, PASS, PUBLIC :: axis  => set_axis
        PROCEDURE, PASS, PUBLIC :: FileName  => set_FileName
        PROCEDURE, PASS, PUBLIC :: reset => reset_to_defaults
        PROCEDURE, PASS, PUBLIC :: hold => set_Persist

        PROCEDURE, PASS, PRIVATE :: plot2D_vector_vs_vector
        PROCEDURE, PASS, PRIVATE :: plot2D_matrix_vs_vector
        GENERIC, PUBLIC   :: plot   => plot2D_vector_vs_vector, plot2D_matrix_vs_vector
        PROCEDURE, PASS, PUBLIC :: surf => splot
        PROCEDURE, PASS, PUBLIC :: fplot
        PROCEDURE, PASS, Private :: semilogxv
        PROCEDURE, PASS, Private :: semilogxm
        PROCEDURE, PASS, private :: semilogyv
        PROCEDURE, PASS, private :: semilogym
        PROCEDURE, PASS, private :: loglogv
        PROCEDURE, PASS, private :: loglogm
        GENERIC, PUBLIC   :: semilogx   => semilogxv, semilogxm
        GENERIC, PUBLIC   :: semilogy   => semilogyv, semilogym
        GENERIC, PUBLIC   :: loglog   => loglogv, loglogm
        PROCEDURE, PASS, PUBLIC :: script => gnuplotScript
    END TYPE gpf


CONTAINS


!..............................................................................
    SUBROUTINE fplot(this, func,xrange,np)
!..............................................................................
    ! fplot, plot a function in the range xrange=[xmin, xamx] with np points
    ! if np isnot sent, then np=50 is assumed!
    ! func is the name of function to be plotted

    CLASS(gpf):: this
    INTERFACE
        FUNCTION func(x)
            Import :: wp
            REAL(wp), INTENT(IN) :: x
            REAL(wp) :: func
        END FUNCTION func
    END INTERFACE
    REAL(wp), INTENT(IN) :: xrange(2)
    INTEGER, OPTIONAL, INTENT(IN):: np

    INTEGER:: n
    INTEGER:: i
    INTEGER:: alloc_err
    REAL(wp), ALLOCATABLE :: x(:)
    REAL(wp), ALLOCATABLE :: y(:)

    IF (present(np)) THEN
        n=np
    ELSE
        n=50
    END IF
    ALLOCATE(x(1:n), y(1:n), STAT=alloc_err)
    IF (alloc_err /=0) THEN
        STOP "Allocation error in fplot procedure..."
    END IF
    !Create set of xy data
    x=linspace(xrange(1),xrange(2), n)
    y=[ (func(x(i)), i=1, n) ]

    CALL plot2D_vector_vs_vector(this,x,y)


    IF (allocated(x)) DEALLOCATE(x)
    IF (allocated(y)) DEALLOCATE(y)


    END SUBROUTINE fplot


    !..............................................................................
    SUBROUTINE semilogxv(this, x1, y1, ls1, &
    x2, y2, ls2, &
    x3, y3, ls3, &
    x4, y4, ls4  )
    !..............................................................................
    !   This procedure is the same as plotXY with logarithmic x axis
    !..............................................................................
    CLASS(gpf):: this
    ! Input vector
    REAL(wp),  INTENT(IN)            :: x1(:)
    REAL(wp),  INTENT(IN), OPTIONAL  :: y1(:)
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   ::  ls1

    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: x2
    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: y2
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL       :: ls2

    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: x3
    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: y3
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL       :: ls3

    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: x4
    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: y4
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   ::  ls4

    this%plotscale='semilogx'
    call plot2D_vector_vs_vector(this, x1, y1, ls1, x2, y2, ls2, x3, y3, ls3, x4, y4, ls4  )
    ! Set the plot scale as linear. It means log scale is off
    this%plotscale='linear'

    End SUBROUTINE semilogxv


    !..............................................................................
    SUBROUTINE semilogyv(this, x1, y1, ls1, &
    x2, y2, ls2, &
    x3, y3, ls3, &
    x4, y4, ls4  )
    !..............................................................................
    !   This procedure is the same as plotXY with logarithmic y axis
    !..............................................................................
    CLASS(gpf):: this
    ! Input vector
    REAL(wp),  INTENT(IN)            :: x1(:)
    REAL(wp),  INTENT(IN), OPTIONAL  :: y1(:)
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   ::  ls1

    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: x2
    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: y2
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL       :: ls2

    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: x3
    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: y3
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL       :: ls3

    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: x4
    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: y4
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   ::  ls4

    this%plotscale='semilogy'
    call plot2D_vector_vs_vector(this, x1, y1, ls1, x2, y2, ls2, x3, y3, ls3, x4, y4, ls4  )
    ! Set the plot scale as linear. It means log scale is off
    this%plotscale='linear'


    End SUBROUTINE semilogyv



    SUBROUTINE loglogv(this, x1, y1, ls1, &
    x2, y2, ls2, &
    x3, y3, ls3, &
    x4, y4, ls4  )
    !..............................................................................
    !   This procedure is the same as plotXY with logarithmic x axis
    !..............................................................................
    CLASS(gpf):: this
    ! Input vector
    REAL(wp),  INTENT(IN)            :: x1(:)
    REAL(wp),  INTENT(IN), OPTIONAL  :: y1(:)
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   ::  ls1

    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: x2
    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: y2
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL       :: ls2

    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: x3
    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: y3
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL       :: ls3

    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: x4
    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: y4
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   ::  ls4

    this%plotscale='loglog'
    call plot2D_vector_vs_vector(this, x1, y1, ls1, x2, y2, ls2, x3, y3, ls3, x4, y4, ls4  )
    ! Set the plot scale as linear. It means log scale is off
    this%plotscale='linear'

    End SUBROUTINE loglogv



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
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   :: lspec(:)

    this%plotscale='semilogx'
    call plot2D_matrix_vs_vector(this, xv,ymat, lspec)
    ! Set the plot scale as linear. It means log scale is off
    this%plotscale='linear'


    End SUBROUTINE semilogxm



    SUBROUTINE  semilogym(this, xv,ymat, lspec)
    !..............................................................................
    !Plots a matrix against a vector with logarithmic y-axis
    !For more information see plot2D_matrix_vs_vector procedure
    !Everything is the same except the x-axis scale
    !..............................................................................

    IMPLICIT NONE
    CLASS(gpf):: this
    ! Input arrays
    REAL(wp),  INTENT(IN)    :: xv(:)
    REAL(wp),  INTENT(IN)    :: ymat(:,:)
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   :: lspec(:)

    this%plotscale='semilogy'
    call plot2D_matrix_vs_vector(this, xv,ymat, lspec)
    ! Set the plot scale as linear. It means log scale is off
    this%plotscale='linear'


    End SUBROUTINE semilogym


!..............................................................................
    SUBROUTINE  loglogm(this, xv,ymat, lspec)
!..............................................................................
    !Plots a matrix against a vector with logarithmic x-axis and y-axis
    !For more information see plot2D_matrix_vs_vector procedure
    !Everything is the same except the axes scale
    !..............................................................................

    IMPLICIT NONE
    CLASS(gpf):: this
    ! Input arrays
    REAL(wp),  INTENT(IN)    :: xv(:)
    REAL(wp),  INTENT(IN)    :: ymat(:,:)
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   :: lspec(:)

    this%plotscale='loglog'
    call plot2D_matrix_vs_vector(this, xv,ymat, lspec)
    ! Set the plot scale as linear. It means log scale is off
    this%plotscale='linear'


    End SUBROUTINE loglogm


!..............................................................................
    SUBROUTINE plot2D_vector_vs_vector(this, x1, y1, ls1, &
    x2, y2, ls2, &
    x3, y3, ls3, &
    x4, y4, ls4  )
!..............................................................................
    !   This procedure plots:
    !   1. A vector against another vector (xy plot)
    !   2. A vector versus its element indices.
    !   3. Can accept up to 4 data sets as x,y pairs!
    ! Arguments
    ! xi, yi vectors of data series,
    ! lsi a string maximum 80 characters containing the line specification, legends, ...

    CLASS(gpf):: this
    ! Input vector
    REAL(wp),  INTENT(IN)            :: x1(:)
    REAL(wp),  INTENT(IN), OPTIONAL  :: y1(:)
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   ::  ls1

    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: x2
    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: y2
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL       :: ls2

    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: x3
    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: y3
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL       :: ls3

    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: x4
    REAL(wp),  INTENT(IN), DIMENSION(:), OPTIONAL  :: y4
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   ::  ls4

    !   Local variables
    !----------------------------------------------------------------------

    INTEGER:: nx1
    INTEGER:: ny1
    INTEGER:: nx2
    INTEGER:: ny2
    INTEGER:: nx3
    INTEGER:: ny3
    INTEGER:: nx4
    INTEGER:: ny4
    INTEGER:: number_of_plots
    CHARACTER(LEN=3)::  plotType
    INTEGER:: file_unit
    INTEGER:: i
    CHARACTER(LEN=80)::  pltstring(4)  !Four 80 character lines

    !Initialize variables
    plotType=''
    pltstring=''
    !   Check the input
    nx1=size(x1)
    IF ((Present(y1) )) THEN
        ny1=size(y1)
        IF (checkDim(nx1,ny1)) THEN
            plotType='xy1'
            number_of_plots=1
        ELSE
            PRINT*, 'gpf error: length of x1 and y1 doesnot match'
            RETURN
        END IF
    ELSE !plot only x againest its element indices
        plotType='xi'
        number_of_plots=1
    END IF

    !Process line spec for first data set if present
    CALL process_linespec(1, pltstring(1),ls1)

    IF (present(x2) .AND. present (y2)) THEN
        nx2=size(x2)
        ny2=size(y2)
        IF (checkDim(nx2,ny2)) THEN
            plotType='xy2'
            number_of_plots=2
        ELSE
            RETURN
        END IF
        !Process line spec for 2nd data set if present
        CALL process_linespec(2, pltstring(2),ls2)
    END IF

    IF (present(x3) .AND. present (y3)) THEN
        nx3=size(x3)
        ny3=size(y3)
        IF (checkDim(nx3,ny3)) THEN
            plotType='xy3'
            number_of_plots=3
        ELSE
            RETURN
        END IF
        !Process line spec for 3rd data set if present
        CALL process_linespec(3, pltstring(3),ls3)
    END IF

    IF (present(x4) .AND. present (y4)) THEN
        nx4=size(x4)
        ny4=size(y4)
        IF (checkDim(nx4,ny4)) THEN
            plotType='xy4'
            number_of_plots=4
        ELSE
            RETURN
        END IF
        !Process line spec for 4th data set if present
        CALL process_linespec(4, pltstring(4),ls4)
    END IF

    ! Open the output file
    OPEN ( Newunit = file_unit, FILE = this%txtfileName, STATUS = 'replace',IOSTAT = this%Status )

    IF (this%Status /= 0 ) THEN
        this%Msg= "gpf error: cannot open file for output"
        PRINT*, this%Msg
        RETURN !An error has been occurred
    END IF

    ! Write plot title, axis labels and other annotations
    CALL ProcessCmd(this, file_unit)

    ! Write plot command and line styles and legend if any
    IF (number_of_plots ==1) THEN
        WRITE ( file_unit, '(a)' ) trim(pltstring(1))
    ELSE
        WRITE ( file_unit, '(a)' ) ( trim(pltstring(i)) // ' \' , i=1, number_of_plots-1)
        WRITE ( file_unit, '(a)' )   trim(pltstring(number_of_plots))
    END IF
    ! Write xy data into file
    SELECT CASE (plotType)
    CASE ('xi')
        CALL write_xydata(file_unit,nx1,x1)
    CASE ('xy1')
        CALL write_xydata(file_unit,nx1,x1,y1)
    CASE ('xy2')
        CALL write_xydata(file_unit,nx1,x1,y1)
        CALL write_xydata(file_unit,nx2,x2,y2)
    CASE ('xy3')
        CALL write_xydata(file_unit,nx1,x1,y1)
        CALL write_xydata(file_unit,nx2,x2,y2)
        CALL write_xydata(file_unit,nx3,x3,y3)
    CASE ('xy4')
        CALL write_xydata(file_unit,nx1,x1,y1)
        CALL write_xydata(file_unit,nx2,x2,y2)
        CALL write_xydata(file_unit,nx3,x3,y3)
        CALL write_xydata(file_unit,nx4,x4,y4)
    END SELECT


    IF (this%persist .AND. this%DisplayPlot) THEN
        CLOSE ( UNIT = file_unit )
        WRITE(*,*)
        WRITE(*,*) 'This is gnuplot interactive mode!'
        WRITE(*,*)  'Type q and press enter to exit'
        WRITE(*,*)
    ELSE
        WRITE ( file_unit, '(a)' ) 'pause -1 "Press a key to continue..."'
        WRITE ( file_unit, '(a)' ) 'q'
        CLOSE ( UNIT = file_unit )
    END IF
    !   Now plot the results
    IF (this%DisplayPlot) THEN
        CALL Write2GnuPlot(this%txtfileName,this%Persist)
    ELSE
        this%DisplayPlot=.TRUE. !Reset display plot to its value
        this%txtfileName=gnuplot_output_filename
    END IF
    !
    !: End of plot2D_vector_vs_vector
    END SUBROUTINE plot2D_vector_vs_vector


!..............................................................................
    SUBROUTINE splot(this, X, Y, Z, lspec)
!..............................................................................
    CLASS(gpf):: this
    ! Input vector
    REAL(wp),  INTENT(IN)            :: X(:,:)
    REAL(wp),  INTENT(IN), OPTIONAL  :: Y(:,:)
    REAL(wp),  INTENT(IN), OPTIONAL  :: Z(:,:)
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   ::  lspec

    !   Local variables
    !----------------------------------------------------------------------

    INTEGER:: ncx
    INTEGER:: nrx
    INTEGER:: file_unit
    INTEGER:: i
    INTEGER:: j
    LOGICAL:: xyz_data
    CHARACTER(LEN=80)::  pltstring

    pltstring=''
    !   Check the input data
    ncx=size(X,dim=2)
    nrx=size(X,dim=1)
    IF (present(Y) .AND. present(Z)) THEN
        xyz_data=.TRUE.
    ELSEIF (present(Y)) THEN
        PRINT*, "gpf error: Z matrix was not sent to 3D plot routine"
        RETURN
    ELSE
        xyz_data=.FALSE.
    END IF

    ! Open the output file
    OPEN ( Newunit = file_unit, FILE = this%txtfileName, STATUS = 'replace',IOSTAT = this%Status )
    IF (this%Status /= 0 ) THEN
        this%Msg= "gpf error: cannot open file for output"
        PRINT*, this%Msg
        RETURN !An error has been occurred
    END IF


    ! Set the plot scale as linear. It means log scale is off
    this%plotscale='linear'
    ! Write titles and other annotations
    CALL ProcessCmd(this, file_unit)

    IF ( present(lspec) ) THEN
        IF (hastitle(lspec)) THEN
            pltstring='splot "-" '//trim(lspec)
        ELSE
            pltstring='splot "-" notitle '//trim(lspec)
        END IF
    ELSE
            pltstring='splot "-" notitle '
        END IF

    WRITE ( file_unit, '(a)' ) trim(pltstring)

    ! Write xy data into file
    WRITE ( file_unit, '(a)' ) '#data x y z'


    IF (xyz_data) THEN
        DO j=1,ncx
            DO i=1, nrx
                WRITE ( file_unit, * ) X(i,j), Y(i,j), Z(i,j)
            ENDDO
            WRITE( file_unit, '(a)' )  !put an empty line
        ENDDO
        WRITE ( file_unit, '(a)' ) 'e'  !end of set of data
    ELSE !only Z has been sent (i.e. single matrix data)
        DO j=1,ncx
            DO i=1, nrx
                WRITE ( file_unit, * ) i, j, X(i,j)
            ENDDO
            WRITE( file_unit, '(a)' )  !put an empty line
        ENDDO
        WRITE ( file_unit, '(a)' ) 'e'  !end of set of data
    END IF

    IF (this%persist .AND. this%DisplayPlot) THEN
        CLOSE ( UNIT = file_unit )
        WRITE(*,*)
        WRITE(*,*) 'This is gnuplot interactive mode!'
        WRITE(*,*)  'Type q and press enter to exit'
        WRITE(*,*)
    ELSE
        WRITE ( file_unit, '(a)' ) 'pause -1 "Press a key to continue..."'
        WRITE ( file_unit, '(a)' ) 'q'
        CLOSE ( UNIT = file_unit )
    END IF
    !   Now plot the results
    IF (this%DisplayPlot) THEN
        CALL Write2GnuPlot(this%txtfileName,this%Persist)
    ELSE
        this%DisplayPlot=.TRUE. !Reset display plot to its value
        this%txtfileName='ogpf_temp_script_file.plt'
    END IF
    !
    !: End of splot
    END SUBROUTINE splot


!..............................................................................
    SUBROUTINE Write2GnuPlot(fileName, Persist)
!..............................................................................
    !   This subroutine call gnuplot through system command
    IMPLICIT NONE
    CHARACTER(LEN=*):: fileName
    LOGICAL, INTENT(IN) :: Persist

    !local vars
    Integer :: lun, ios

    IF  (Persist) THEN
        !Fortran standard recommend to use call execute_command_line to invoke another program
        !from within Fortran, the old method was to use call system
        !Here by default Fortran standard is used, if you have a compiler does not support
        !call execute_command_line, uncomment the following call system

        !CALL system('gnuplot -persist '//fileName)              !Obsolete method, use with old compilers
        CALL execute_command_line('gnuplot -persist '//fileName) !Fortran standard
    ELSE
        !CALL system ('gnuplot '//fileName) !Obsolete method, use with old compilers
        CALL execute_command_line ('gnuplot '//fileName)
    END IF

!   The following lines actually do the deletion of temporary file
!   This method is used to have a portable code!

    OPEN ( Newunit = lun, FILE = fileName, STATUS = 'old',IOSTAT = ios )
    IF (ios /= 0 ) THEN
        PRINT*, "gpf error: cannot open file for output"
        RETURN !An error has been occurred
    END IF
    Close(lun,status='delete')  !delete file

    END SUBROUTINE Write2GnuPlot


!..............................................................................
    SUBROUTINE WriteString(str, file_unit)
!..............................................................................
    !WriteString, separate a string using ";" delimiters
    !and write each statement in a separate line in the file indicated by file_unit
    CHARACTER(LEN=*), INTENT(IN):: str
    INTEGER, INTENT(IN)   :: file_unit
    CHARACTER, PARAMETER :: delimiter=';'
    INTEGER::n
    INTEGER:: m
    INTEGER:: k


    k=len_trim(str) !length with removed trailing blanks
    n=scan(str,delimiter)
    IF (n==0) THEN  !This is a single statement
        WRITE(file_unit,'(a)') str
        RETURN
    END IF
    m=1
    DO WHILE (n/=0 .AND. m<k)
        IF (n/=1) THEN
            WRITE(file_unit,'(a)') str(m:m+n-2)
        END IF
        m=n+m
        n=scan(str(m:k),delimiter)
    END DO
    IF (m<k) THEN !write the last statement
        WRITE(file_unit,'(a)') str(m:k)
    END IF
    END SUBROUTINE WriteString


!..............................................................................
    SUBROUTINE ProcessCmd(this, file_unit)
!..............................................................................
    !   This subroutine writes all the data into plot file
    !   to be read by gnuplot

    CLASS(gpf), INTENT(IN)   :: this
    INTEGER, INTENT(IN)     :: file_unit
    ! The following lines set the gnuplot terminal
    ! The data style to lines+symbols:linespoints
    ! Can be overwritten by options
    WRITE ( file_unit, '(a)' ) 'set term ' //gnuplot_terminal// &
                               ' font ' // '"'// gnuplot_font_name // '"' // &
                               ' size '// gnuplot_plot_size          !set output terminal
    WRITE ( file_unit, '(a)' ) 'set style data '//gnuplot_datastyle !set data style

     ! Write options
    IF ( this%hasOptions ) THEN
        CALL WriteString(trim(this%txtOptions),file_unit)
    END IF
    ! Check with plot scale: i.e linear, logx, logy, or log xy
    Select case (this%plotscale)
    case ('semilogx')
        WRITE ( file_unit, '(a)' ) 'set logscale  x'
    case ('semilogy')
        WRITE ( file_unit, '(a)' ) 'set logscale  y'
    case ('loglog')
        WRITE ( file_unit, '(a)' ) 'set logscale  xy'
    case default !For linear xy plot or 3D plots
        !pass
    end select
    !   Write the plot options to script file

    if (this%hasxrange) then
        WRITE ( file_unit, '(a,g0,a,g0,a)' ) 'set xrange [',this%xrange(1),':',this%xrange(2),']'
    end if

    if (this%hasyrange) then
        WRITE ( file_unit, '(a,g0,a,g0,a)' ) 'set yrange [',this%yrange(1),':',this%yrange(2),']'
    end if

    if (this%haszrange) then
        WRITE ( file_unit, '(a,g0,a,g0,a)' ) 'set xrange [',this%zrange(1),':',this%zrange(2),']'
    end if

    if (this%hasPlotTitle) then
        WRITE ( file_unit, '(a)' ) 'set title  "' // trim(this%txtPlotTitle)// '"'
    end if
    if (this%hasXlabel) then
        WRITE ( file_unit, '(a)' ) 'set xlabel "'// trim(this%txtXlabel)//'"'
    end if
    if (this%hasYlabel) then
        WRITE ( file_unit, '(a)' ) 'set ylabel "'//trim(this%txtYlabel)//'"'
    end if
    if (this%hasZlabel) then
        WRITE ( file_unit, '(a)' ) 'set zlabel "'//trim(this%txtZlabel)//'"'
    end if

    END SUBROUTINE ProcessCmd


!..............................................................................
    SUBROUTINE gnuplotScript(this, strScript)
!..............................................................................
    ! write a gnuplot script in a file and then call gnuplot to execute the script
    CLASS(gpf):: this
    CHARACTER(LEN=*), INTENT(IN):: strScript
    !local variables
    INTEGER:: file_unit

    ! Open the output file
    OPEN ( Newunit = file_unit, FILE = this%txtfileName, STATUS = 'replace',IOSTAT = this%Status )
    IF (this%Status /= 0 ) THEN
        this%Msg= "gpf error: cannot open file for output"
        PRINT*, this%Msg
        RETURN !An error has been occurred
    END IF


    ! Write gnuplot script in the file
    CALL writestring(strScript, file_unit)

    IF (this%persist .AND. this%DisplayPlot) THEN
        CLOSE ( UNIT = file_unit )
        WRITE(*,*)
        WRITE(*,*) 'This is gnuplot interactive mode!'
        WRITE(*,*)  'Type q and press enter to exit'
        WRITE(*,*)
    ELSE
        WRITE ( file_unit, '(a)' ) 'pause -1 "Press a key to continue..."'
        WRITE ( file_unit, '(a)' ) 'q'
        CLOSE ( UNIT = file_unit )
    END IF
    !   Now plot the results
    IF (this%DisplayPlot) THEN
        CALL Write2GnuPlot(this%txtfileName,this%Persist)
    ELSE
        this%DisplayPlot=.TRUE. !Reset display plot to its value
        this%txtfileName=gnuplot_output_filename
    END IF

    END SUBROUTINE gnuplotScript


!..............................................................................
    SUBROUTINE reset_to_defaults(this)
!..............................................................................
    !Reset all params to their default values
    CLASS(gpf):: this
    this%txtPlotTitle=""
    this%txtXlabel=""
    this%txtYlabel=""
    this%txtOptions=""
    this%txtfileName=gnuplot_output_filename
    this%DisplayPlot=.True.
    this%hasOptions= .FALSE.
    this%hasPlotTitle= .FALSE.
    this%hasXlabel= .FALSE.
    this%hasYlabel= .FALSE.
    this%hasZlabel= .FALSE.
    this%hasxrange= .FALSE.
    this%hasyrange= .FALSE.
    this%haszrange= .FALSE.

    this%persist= .FALSE.
    END SUBROUTINE reset_to_defaults


!..............................................................................
    SUBROUTINE set_FileName(this,string)
!..............................................................................
    !Set a file name for plot command output
    !This file can be used later by gnuplot as an script to reproduce the plot
    CLASS(gpf):: this
    CHARACTER(LEN=*), INTENT(IN) :: string
    this%txtfileName=trim(string)
    this%DisplayPlot=.FALSE.  ! Set dispalyplot to false to only write plot commands into file
    END SUBROUTINE set_FileName


!..............................................................................
    SUBROUTINE set_options(this,string)
!..............................................................................
    !Set the plot title
    CLASS(gpf):: this
    CHARACTER(LEN=*), INTENT(IN) :: string
    Integer, Save :: strlength=0
    strlength=strlength+len_trim(string)
    if (strlength < len_options) then
        this%txtOptions=trim(this%txtOptions)//';'//trim(string)
    else
        print*, 'ogpf warning: the length of options exceeds than the set value :', len_options
        print*, 'options is truncated to ', len_options
        this%txtOptions=trim(this%txtOptions)//';'//trim(string)
    end if
    this%hasOptions=.true.
    END SUBROUTINE set_options


!..............................................................................
    SUBROUTINE set_PlotTitle(this,string)
!..............................................................................
    !Set the plot title
    CLASS(gpf):: this
    CHARACTER(LEN=*), INTENT(IN) :: string
    this%txtPlotTitle=trim(string)
    this%hasPlotTitle=.true.
    END SUBROUTINE set_PlotTitle


!..............................................................................
    SUBROUTINE set_xlabel(this,string)
!..............................................................................
    !Set the xlabel
    CLASS(gpf):: this
    CHARACTER(LEN=*), INTENT(IN) :: string
    this%txtXlabel=trim(string)
    this%hasXlabel=.true.
    END SUBROUTINE set_xlabel


!..............................................................................
    SUBROUTINE set_ylabel(this,string)
!..............................................................................
    !Set the ylabel
    CLASS(gpf):: this
    CHARACTER(LEN=*), INTENT(IN) :: string
    this%txtYlabel=trim(string)
    this%hasYlabel=.true.
    END SUBROUTINE set_ylabel


!..............................................................................
    SUBROUTINE set_zlabel(this,string)
!..............................................................................
    !Set the z label
    CLASS(gpf):: this
    CHARACTER(LEN=*), INTENT(IN) :: string
    this%txtZlabel=trim(string)
    this%hasZlabel=.true.
    END SUBROUTINE set_zlabel


!..............................................................................
   SUBROUTINE set_axis(this,rng)
!..............................................................................
    !Set the z label
    CLASS(gpf):: this
    REAL(wp), INTENT(IN) :: rng(:)
    Integer :: n
    n=size(rng,dim=1)
    select case(n)
    case(2)
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

    END SUBROUTINE set_axis


!..............................................................................
    SUBROUTINE set_Persist(this,string)
!..............................................................................
    !Set persist for gnuplot
    ! -on  keeps gnuplot in interactive mode
    ! -off closess gnuplot after drawing the plot
    CLASS(gpf):: this
    CHARACTER(LEN=*), INTENT(IN) :: string
    SELECT CASE(lcase(string))
    CASE ('on')
        this%Persist=.TRUE.
    CASE ('off')
        this%Persist=.FALSE.
    CASE DEFAULT
        this%Persist=.FALSE.
    END SELECT
    END SUBROUTINE set_Persist

    !!..............................................................................
    !    FUNCTION LineStyle(string)
    !        !..............................................................................
    !
    !        ! This function accepts both abbreviated and full text line style and
    !        ! return the correct linestyle value for gnuplot
    !        ! If use pass wrong value, 'lines' is passed as the line style
    !        CHARACTER(LEN=*), INTENT(IN) :: string
    !        CHARACTER(LEN=11 ):: LineStyle
    !
    !        SELECT CASE( lcase(string) )
    !            CASE ('linespoints','lp')
    !                LineStyle='linespoints'
    !            CASE ('points','p')
    !                LineStyle='points'
    !            CASE ('lines','l')
    !                LineStyle='lines'
    !            CASE DEFAULT  ! In the case of error use lines
    !                LineStyle='lines'
    !        END SELECT
    !    END FUNCTION LineStyle
    !

    !           *************** Utility Subs *******************
    !           ************************************************



!..............................................................................
    SUBROUTINE ErrHandler(Msg)
!..............................................................................
    CHARACTER(LEN=*), INTENT(IN)    :: Msg
    WRITE(6,*)    Msg // ' failed'
    WRITE(6,*)    'Error in gpf '
    WRITE(6,*)    'See gpf GUI Module....   '
    STOP
    END SUBROUTINE ErrHandler


!..............................................................................
    SUBROUTINE  plot2D_matrix_vs_vector(this, xv,ymat, lspec)
!..............................................................................
    !plot2D_matrix_vs_vector accepts a vector xv and a matrix ymat and plots columns of ymat against xv
    !lspec is an optional array defines the line specification for each data series
    !If a single element array is sent for lspec then all series are plotted using the same
    !linespec

    IMPLICIT NONE
    CLASS(gpf):: this
    ! Input arrays
    REAL(wp),  INTENT(IN)    :: xv(:)
    REAL(wp),  INTENT(IN)    :: ymat(:,:)
    CHARACTER(LEN=*),  INTENT(IN), OPTIONAL   :: lspec(:)
    !----------------------------------------------------------------------
    !       Local variables
    INTEGER:: nx
    INTEGER:: ny
    INTEGER:: file_unit
    INTEGER:: ns
    INTEGER:: number_of_plots
    INTEGER::  i
    INTEGER:: j
    INTEGER:: ierr
    CHARACTER(LEN=80), ALLOCATABLE ::  pltstring(:)
    !

    !*******************************************************************************
    !   Check the input
    nx=size(xv)
    ny=size(ymat,Dim=1)
    IF (.NOT. checkDim(nx,ny)) THEN
        PRINT*, 'gpf error: The length of arrays does not match'
        RETURN
    END IF

    ! Open the output file
    OPEN ( Newunit = file_unit, FILE = this%txtfileName, STATUS = 'replace',IOSTAT = this%Status )
    IF (this%Status /= 0 ) THEN
        this%Msg= "gpf error: cannot open file for output"
        PRINT*, this%Msg
        RETURN !An error has been occurred
    END IF

    ! Write titles and other annotations
    CALL ProcessCmd(this, file_unit)
    !   Process legends and style
    number_of_plots=size(ymat,Dim=2)
    ALLOCATE(pltstring(number_of_plots), STAT=ierr)
    IF (ierr /=0) THEN
        PRINT*, 'allocation error'
        RETURN
    END IF

    IF ( present(lspec) ) THEN
        CALL process_linespec(1,pltstring(1),lspec(1))
        ns=size(lspec)
        ! gpf will cylce through line specification, if number of specification passed
        ! is less than number of plots
        DO i=1, number_of_plots
            j=mod(i-1, ns) + 1
            CALL process_linespec(i, pltstring(i), lspec(j))
        END DO
    ELSE !No lspec is available
        pltstring(1)=' plot "-" notitle,'
        pltstring(2:number_of_plots-1)='"-" notitle,'
        pltstring(number_of_plots)='"-" notitle'
    END IF

     ! Write plot command and line styles and legend if any
    WRITE ( file_unit, '(a)' ) ( trim(pltstring(i)) // ' \' , i=1, number_of_plots-1)
    WRITE ( file_unit, '(a)' )   trim(pltstring(number_of_plots))



    !   Write data into script file
    DO j=1, number_of_plots
        DO i = 1, nx
            WRITE ( file_unit, * ) xv(i),ymat(i,j)
        END DO
        WRITE ( file_unit, '(a)' ) 'e'  !end of jth set of data
    END DO

    IF (this%persist .AND. this%DisplayPlot) THEN
        CLOSE ( UNIT = file_unit )
        WRITE(*,*)
        WRITE(*,*) 'This is gnuplot interactive mode!'
        WRITE(*,*)  'Type q and press enter to exit'
        WRITE(*,*)
    ELSE
        WRITE ( file_unit, '(a)' ) 'pause -1 "Press a key to continue..."'
        WRITE ( file_unit, '(a)' ) 'q'
        CLOSE ( UNIT = file_unit )
    END IF
    !   Now plot the results
    IF (this%DisplayPlot) THEN
        CALL Write2GnuPlot(this%txtfileName,this%Persist)
    ELSE
        this%DisplayPlot=.TRUE. !Reset display plot to its value
        this%txtfileName='ogpf_temp_script_file.plt'
    END IF
    !Release memory
    IF (allocated(pltstring)) THEN
        DEALLOCATE(pltstring)
    END IF
    !: End of plot2D_matrix_vs_vector
    END SUBROUTINE  plot2D_matrix_vs_vector


!..............................................................................
    SUBROUTINE process_linespec(order, lsstring, lspec)
!..............................................................................
    !Accepts the line specification and interpret it into a format
    !to be sent to gnuplot

    INTEGER, INTENT(IN) :: order !1 for the first data series
    CHARACTER(LEN=*), INTENT(OUT) :: lsstring
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: lspec

    SELECT CASE(order)
    CASE(1)
        IF ( present(lspec) ) THEN
            IF (hastitle(lspec)) THEN
                lsstring='plot "-" '//trim(lspec)
            ELSE
                lsstring='plot "-" notitle '//trim(lspec)
            END IF
        ELSE
            lsstring='plot "-" notitle '
        END IF
    CASE  DEFAULT !e.g. 2, 3, 4, ...
        IF (present(lspec)) THEN
            IF (hastitle(lspec)) THEN
                lsstring=', "-" '// trim(lspec)
            ELSE
                lsstring=', "-" notitle '// trim(lspec)
            END IF
        ELSE
            lsstring=', "-" notitle '
        END IF
    END SELECT
    END SUBROUTINE process_linespec



!..............................................................................
    FUNCTION hastitle(string)
!..............................................................................
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL:: hastitle
    INTEGER:: idx1
    INTEGER:: idx2

    idx1=index( lcase(string),'title')     !Check if title is passed
    idx2=index(' ' // lcase(string),' t ') !Check if the abbreviated title 't' is passed. Extra space is added
                                           ! at the beginning of string to find starting 't'
    IF (idx1 /=0 .OR. idx2 /=0 ) THEN
        hastitle=.TRUE.
    ELSE
        hastitle=.FALSE.
    END IF

    END FUNCTION hastitle



    !!!!Other Utility


!..............................................................................
    FUNCTION lcase(string)
!..............................................................................
    CHARACTER(LEN=*), INTENT(IN) :: string
    CHARACTER(LEN=len(string)):: lcase
    INTEGER:: i
    INTEGER:: n
    CHARACTER(1):: chr

    DO i=1, len(string)
        chr=string(i:i)
        n=ichar(chr)
        IF (n >=65 .AND. n <= 90) THEN
            lcase(i:i)=char(n+32)
        ELSE
            lcase(i:i)=chr
        END IF
    END DO
    END FUNCTION lcase


!..............................................................................
    FUNCTION checkDim(nx,ny)
!..............................................................................
    INTEGER, INTENT(IN):: nx
    INTEGER, INTENT(IN):: ny
    LOGICAL:: checkDim
    IF (nx/=ny) THEN
        PRINT*,  ' gpf - Fatal error!'//char(13)// &
        ' The length of x, and y vectors must be equal.'
        checkDim=.FALSE.
    ELSE
        checkDim=.TRUE.
    END IF

    END FUNCTION checkDim


!..............................................................................
    SUBROUTINE write_xydata(file_unit,ndata,x,y)
!..............................................................................
    ! Writes set of xy data into a file
    INTEGER, INTENT(IN) :: file_unit
    INTEGER, INTENT(IN) :: ndata
    REAL(wp), INTENT(IN) :: x(:)
    REAL(wp), INTENT(IN), Optional :: y(:)

    INTEGER:: i
    if (present(y) ) then !both x and y are present, data are xy set
        DO i = 1, ndata
            WRITE ( file_unit, * ) x(i), y(i)
        END DO
    else !only x is passed, data are index-x set
        DO i = 1, ndata
            WRITE ( file_unit, * ) x(i)
        END DO
    end if
    WRITE ( file_unit, '(a)' ) 'e'  !end of set of data
    END SUBROUTINE write_xydata


!..............................................................................
    FUNCTION linspace(a,b,n_elements)
!..............................................................................
    !   returns a linearly spaced vector with n points in [a, b]
    !   if n is omitted, 100 points will be considered
    !
    REAL(wp), INTENT(IN) ::a
    REAL(wp), INTENT(IN) :: b
    INTEGER, INTENT(IN), OPTIONAL ::  n_elements
    REAL(wp), ALLOCATABLE:: linspace(:)
    !   Local vars
    REAL(wp):: dx
    INTEGER:: i
    INTEGER:: n
    INTEGER:: ierr
    IF (Present(n_elements)) THEN
        n=n_elements
    ELSE
        n=100
    END IF

    ALLOCATE(linspace(n), STAT=ierr)
    IF (ierr /= 0) THEN
        PRINT*, "Fatal Error, Allocation failed in linspace function"
        RETURN
    END IF
    dx=(b-a)/real((n-1),wp)
    linspace=[(i*dx+a, i=0,n-1)]
    END FUNCTION linspace


!..............................................................................
    SUBROUTINE meshgrid(X,Y,xgv,ygv, ierr)
!..............................................................................
    !meshgrid generate mesh grid over a rectangular domain of [xmin xmax, ymin, max]
    !     xgv, ygv are vector in form of [start, stop, step] or [start, stop]
    !     when step has not been given, nx=m and ny=n are used to calculate steps
    !     and generate nx and ny data points respectively.
    !     Now these values are set to 25.
    !     X and Y are matrix each of size [ny by nx] contains the grid data.
    !     The coordinates of point (i,j) is [X(i,j), Y(i,j)]
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
    REAL(wp), INTENT(OUT), ALLOCATABLE :: X(:,:)
    REAL(wp), INTENT(OUT), ALLOCATABLE :: Y(:,:)
    REAL(wp), INTENT(IN) :: xgv(:)
    REAL(wp), INTENT(IN),  OPTIONAL  :: ygv(:)
    INTEGER, INTENT(OUT), OPTIONAL :: ierr !Return the error code
    ! Local variables
    INTEGER:: i
    INTEGER:: sv
    INTEGER:: nx
    INTEGER:: ny
    INTEGER:: m
    INTEGER:: n
    REAL(wp):: dx
    REAL(wp):: dy
    LOGICAL:: only_x_available

! Initial setting
    m=25
    n=25
    only_x_available=.FALSE.
    sv=0 !Assume no error
    SELECT CASE(size(xgv))
    CASE (2)
        nx=m
        dx=(xgv(2)-xgv(1))/real(nx-1, wp)
    CASE (3)
        dx=xgv(3)
        nx=int((xgv(2)-xgv(1))/dx)+1
    CASE DEFAULT
        PRINT*, "wrong x vector to meshgrid"
        sv=1
    END SELECT

    IF (present(ygv)) THEN
        SELECT CASE(size(ygv))
        CASE (2)
            dy=ygv(2)-ygv(1)
            ny=n
        CASE (3)
            dy=ygv(3)
            ny=int((ygv(2)-ygv(1))/dy)+1
        CASE DEFAULT
            PRINT*, "wrong y vector to meshgrid"
            sv=1
        END SELECT
    ELSE
        only_x_available=.TRUE.
        ny=nx
    END IF

    ALLOCATE(X(ny,nx),Y(ny,nx),STAT=sv)
    IF (sv /=0) THEN
        PRINT*, "allocataion erro in meshgrid"
    END IF

    X(1,:)=[(xgv(1)+real((i-1),wp)*dx, i=1, nx)]
    X(2:ny,:)=spread(X(1,:),dim=1,ncopies=ny-1)

    IF (only_x_available) THEN
        Y=transpose(X)
    ELSE
        Y(:,1)=[(ygv(1)+real((i-1),wp)*dy, i=1, ny)]
        Y(:,2:nx)=spread(Y(:,1),dim=2,ncopies=nx-1)
    END IF

    IF (present(ierr)) THEN
        ierr=sv
    END IF

    END SUBROUTINE meshgrid



!End of ogpf
END MODULE ogpf
