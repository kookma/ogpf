!-------------------------------------------------------------------------------
!    GnuPlot Interface
!-------------------------------------------------------------------------------
!    Purpose:   Object Based Interface to GnuPlot from Fortran (ogpf)
!    Platform:  Windows XP/Vista/7/10
!               (It should work on other platforms, see the Write2GnuPlot subroutine below)
!    Language:  Fortran 2003 and 2008
!    Requires:  1. Fortran 2003 compiler (e.g gfortran 4.7, IVF 12.1, ...) or 2008
!               2. gnuplot 4.5 and higher (other previous version can be used
!    Author:    Mohammad Rahmani
!               Chem Eng Dep., Amirkabir Uni of Tech
!               Tehran, Ir
!               url: aut.ac.ir/m.rahmani
!               email: m[dot]rahmani[at]aut[dot]ac[dot]ir
!   License:    MIT

! This file demonstrate the capability of ogpf module
! An object based Fortran interface to gnuplot
!
! Acknowledgement:
! Special thanks to Hagen Wierstorf (http://www.gnuplotting.org)
! For vluable codes and examples on using gnuplot
! Some examples and color palletes are provided by gnuplotting.
!
!
! Revision 0.22
! Date: Mar 9th, 2018
! - see ogpf.f90 for details
! - more examples to reflect the new features

! Revision:  0.20
! Date:     Feb 20th, 2018
!  - more examples
!  - animation of 2D and 3D plots

! Revision:  0.19
! Date:     Jan 15th, 2018
!  - new contour plot procedure


! Revision:  0.18
! Date:     Dec 22th, 2017
! More example based on ogpf 0.18

! Version:  0.17
! Date:     Dec 18th, 2017
!   Minor corrections
! - Multi window plots using script



! Version:  0.16
! Date:     Feb 11th, 2016
!   Minor corrections
!   Correct the lspec processing in plot2D_matrix_vs_vector
!   Some examples revised!


! Version:  0.15
! Date:     Apr 20th, 2012
!   Minor corrections
!   Use of select_precision module and working precision: wp


! Version:  0.14
! Date:     Mar 28th, 2012
!   Minor corrections
!   use of import keyboard and  removing ogpf precision module


! Version:  0.13
! Date:     Feb 12th, 2012
!   Minor corrections
!   Added more samples
!   Use ogpf precision module



! Version:  0.12
! Date:     Feb 9th, 2012
! New examples for semilogx, semilogy, loglog
! New set options method

! Version:  0.11
! Date:     Feb 9th, 2012

program demo

    use ogpf
    implicit none
    ! parameters
    ! local variables
    integer:: i=0


    mainloop: do
        print*  ! empty line
        print*, "gpf: gnuplot from Fortran Demonstration"
        print*, "Example 1: Demo for xy plot"
        print*, "Example 2: Line specification"
        print*, "Example 3: Plot several data series at the same time"
        print*, "Example 4: Plot four data series at the same time"
        print*, "Example 5: Use line style, line colors and more..."
        print*, "Example 6: An interesting plot, sin(x) and its zero on the same plot"
        print*, "Example 7: Plot a matrix against a vector"
        print*, "Example 8: Plot a matrix against a vector and set the linespec and legend"
        print*, "Example 9: Use gnuplot for animation"
        print*, "Example 10: Use ogpf options"
        print*, "Example 11: simple polar plot"
        print*, "Example 12: A plot with logarithmic x axis"
        print*, "Example 13: A matrix plot with logarithmic y axis"
        print*, "Example 14: A loglog plot"
        print*, "Example 15: Plotting a function"
        print*, "Example 16: Save the gnuplot script into a file for future use"
        print*, "Example 17: Multi window plots, using script"
        print*, "Example 18: Running an external script file"
        print*, "Example 19: Multiple linestyle in matrix plot"
        print*, "Example 20: Scatter plot"
        print*, "Example 21: Stem plot"
        print*, "Example 22: Stem plot animation"
        print*, "Example 23: Another animation using matrix plot"
        print*, "Example 24: Multiplot layout"
        print*, "Example 25: Multiplot layout followed by simple plot"
        print*, "Example 26: Plot matrix vs. matrix"

        print*, "Example 27: Using secondary y axis"
        print*, "Example 28: Using secondary x and y axis"
        print*, "Example 29: Using color and size for title and labels"
        print*, "Example 30: More on labels color and size with secondary axes"
        print*
        print*, "***   Surface and Contour Plots ***"
        print*
        print*, "Example 101: Simple 3D plot using surf"
        print*, "Example 102: Surface plot and color palette "
        print*, "Example 103: Surface plot with hidden details and its contour"
        print*, "Example 104: Cylindrical mapping"
        print*, "Example 105: More contour plot"
        print*, "Example 106: Animation of 3D plots"
        print*, "Example 106: Multiplot layout in 3D"
        print*, "Example 107: Multiplot layout for 3D data"
        print*, "Example 108: Plot a 2D grid"

        print*
        write (unit=*, fmt='(a)') "2D plots: select an example: 1 through 30"
        write (unit=*, fmt='(a)') "3D plots: select an example: 101 through 108"
        write (unit=*, fmt='(a)', advance='no') "enter 0 for exit:  "
        read*, i

        select case(i)
            case(1)
                call exmp01
            case(2)
                call exmp02
            case(3)
                call exmp03
            case(4)
                call exmp04
            case(5)
                call exmp05
            case(6)
                call exmp06
            case(7)
                call exmp07
            case(8)
                call exmp08
            case(9)
                call exmp09
            case(10)
                call exmp10
            case(11)
                call exmp11
            case(12)
                call exmp12
            case(13)
                call exmp13
            case(14)
                call exmp14
            case(15)
                call exmp15
            case(16)
                call exmp16
            case(17)
                call exmp17
            case(18)
                call exmp18
            case(19)
                call exmp19
            case(20)
                call exmp20
            case(21)
                call exmp21
            case(22)
                call exmp22
            case(23)
                call exmp23
            case(24)
                call exmp24
            case(25)
                call exmp25
            case(26)
                call exmp26
            case(27)
                call exmp27
            case(28)
                call exmp28
            case(29)
                call exmp29
           case(30)
                call exmp30

                ! 3D plots

            case(101)
                call exmp101
            case(102)
                call exmp102
            case(103)
                call exmp103
            case(104)
                call exmp104
            case(105)
                call exmp105
            case(106)
                call exmp106
            case(107)
                call exmp107
           case(108)
                call exmp108


            case (0)
                print*, "Program terminated successfully"
                exit mainloop
            case default
                print*, "Try again, use a valid example number"
                print*, "Enter 0 to exit"
        end select
        print*
        print*, "press any key to continue..."
        read*
    end do mainloop


contains


    !...............................................................................
    !Example 1: A very basic example
    !...............................................................................
    subroutine exmp01

        type(gpf):: gp
        integer, parameter:: n=17
        real(wp):: x(n)
        real(wp):: y(n)
        ! Input data
  !      x=dble([-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8])
        x=dble([-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8])

        x= abs(x) + 5

        y=dble([66,51,38,27,18,11,6,3,2,3,6,11,18,27,38,51,66])

        ! Annotation: set title, xlabel, ylabel
        call gp%title('Example 1. A simple xy plot','#990011')
        call gp%xlabel('my x axis ...','#99aa33',font_name="Tahoma")
       ! call gp%ylabel('my y axis ...')
        call gp%options('set border lc "#99aa33"; set ylabel "my label..." tc "#99aa33"')

        call gp%options('set logscale y2')
        call gp%plot(x, y)

    end subroutine exmp01


    !...............................................................................
    !Example 2: Set line specification and legends
    !...............................................................................
    subroutine exmp02

        type(gpf):: gp
        integer, parameter:: n=17
        real(wp):: x(n)
        real(wp):: y(n)
        ! Input data
        x=dble([-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8])
        y=dble([66,51,38,27,18,11,6,3,2,3,6,11,18,27,38,51,66])

        ! Annotation: set title, xlabel, ylabel, line specification
        call gp%title('Example 2. A simple xy plot')
        call gp%xlabel('my x axis ...')
        call gp%ylabel('my y axis ...')

        !Call Plot to draw a vector against a vector of data
        !The last argument defines the line specification
        call gp%plot(x,y,'with linespoints lt 2 pt 4')
    end subroutine exmp02


    !...............................................................................
    ! Example 3: Plot several data set at the same time
    !...............................................................................
    subroutine exmp03

        type(gpf):: g
        integer, parameter:: n = 50
        integer, parameter:: m = 65
        real(wp):: x(n)
        real(wp):: y(n)
        real(wp):: xv(m)
        real(wp):: yv(m)
        real(wp), parameter :: pi=4.0_wp*atan(1.0_wp)

        ! Input data
        x=linspace(-pi,pi,n)  !linspace is a utility function in ogpf module
        y=sin(x)              !linspace(a,b,n) create a linear vector in [a,b] with n elements

        xv=linspace(0.d0, 2.d0*pi,m)
        yv=cos(2.d0*xv)

        ! Annotation, set title, xlabel, ylabel
        call g%title('Example 3. Plot two data series using gnuplot')
        call g%xlabel(' x axis ...')
        call g%ylabel(' y axis ...')
        call g%options('set key top left')

        ! Sample 1: Plot to draw two set of data
        call g%plot(x,y,'title "sin"','', xv,yv,'title "cos"')

        !Sample 2: Use keyword arguments to plot the same example as above
        call g%title('Example 3. Another plot using keyword arguments...')
        call g%plot(x1=x,y1=y,ls1='pt 8',x2=xv,y2=yv,ls2='title "cos(2x)"')

        ! Sample 3: An xy plot with line specification no legends
        call g%title('Example 3. Another plot without keyword arguments...')
        call g%plot(x,y,'title "sin(x)" with points pt 6 lc "blue"', '',xv,yv,'title "cos(2x)" pt 7 lc rgb "#993300"')

    end subroutine exmp03


    !...............................................................................
    ! Example 4: Plot four data series, the maximum number of data series can be plotted!
    !...............................................................................
    subroutine exmp04

        type(gpf):: gp
        integer, parameter:: n=50
        integer, parameter:: m=65
        real(wp):: x(n)
        real(wp):: y(n)
        real(wp):: xv(m)
        real(wp):: yv(m)
        real(wp), parameter :: pi=4.d0*atan(1.d0)
        ! Input data
        x=linspace(-pi,pi,n)  !linspace is a utility function from module ogpf
        y=sin(x)

        xv=linspace(0.d0, 2.d0*pi,m)
        yv=cos(2.d0*xv)
        !           This is the maximum number of plot can be drawn at the same time
        !           If you have more data see, you can plot can be used with matrices!
        call gp%title('Example 4. Plot four data sets using gnuplot')
        call gp%options('set key top left; set grid')

        call gp%plot(x,y, 'title "sin(x)"', '', &
            xv,yv, 'with lp lt 6 title "cos(2x)"', '', &
            xv, 2.d0*yv, 'title "2cos(2x)" lt 7', '', &
            xv, 0.5d0*yv, 'title "0.5cos(2x)" with points pt 8')

        ! Another example with keyboard arguments
        call gp%plot(x1=x,y1=y,x2=xv,y2=yv)

    end subroutine exmp04


    !...............................................................................
    ! Example 5: Use line style, line color and moree
    !...............................................................................
    subroutine exmp05

        type(gpf):: gplot
        integer, parameter:: n=50
        real(wp):: x(n)
        real(wp):: ys(n)
        real(wp):: yc(n)
        real(wp):: ysc(n)
        real(wp), parameter :: pi=4.d0*atan(1.d0)
        ! Input data
        x=linspace(-2.d0*pi,2.d0*pi,n)  !linspace is a utility function from module Utils
        ys=  sin(x)
        yc=  exp(-0.1d0*x)*cos(x)
        ysc= sin(x/2.d0)*cos(2.d0*x)

        ! Annotation, set title, xlabel, ylabel
        call gplot%title('Example 5. A sample with customized line style')
        call gplot%xlabel('x, rad')
        call gplot%ylabel('y, dimensionless')

        ! Plot to draw three set of data
        call gplot%plot( &
            x,ys, 'title "sin" with lines lt 5 lc rgb "#0008B0"', '', &
            x,yc, 'title "cos" with points lt 6 lc rgb "#FF1100"', '', &
            x,ysc,'title "sin(x/2)cos(2x)" with lp lt 7 lc rgb "#00AA04"' )

    end subroutine exmp05


    !...............................................................................
    ! Example 6: Plot a single point along with a series of data
    !...............................................................................
    subroutine exmp06

        type(gpf):: gplot
        integer, parameter:: n=125

        real(wp):: x(n)
        real(wp):: y(n)

        real(wp), parameter :: pi=4.d0*atan(1.d0)
        ! Input data
        x=linspace(0.d0,pi*2.d0,n)  !linspace is a utility function from module Utils
        y=sin(6.d0*x)*exp(-x)


        ! Annotation, set title, xlabel, ylabel
        call gplot%title('Example 6. A sample shows f(x) and its zero on the plot')
        call gplot%xlabel('x, rad')
        call gplot%ylabel('f(x) = sin(6x)exp(-x) dimensionless')
        call gplot%options('set grid')

        ! Plot to draw two set of data, a series and a single point
        call gplot%plot(x,y,'title "sin(6x)exp(-x)" with lines lt 2 lw 3', '', &
            [pi],[0.d0],'title "zero" with points pt 7 ps 3 lc rgb "#FF0000"')
    end subroutine exmp06


    !...............................................................................
    ! Example 7: Plot a matrix against a vector
    !...............................................................................
    subroutine exmp07

        type(gpf):: g
        integer, parameter:: n=25
        real(wp), parameter :: pi=4.d0*atan(1.d0)
        real(wp):: x(n)
        real(wp):: y(n,6)

        !Create data
        x=linspace(-pi,pi,n)
        y(:,1)=sin(x)
        y(:,2)=cos(x)
        y(:,3)=cos(0.5d0*x)
        y(:,4)=sin(0.5d0*x)
        y(:,5)=sin(x)*cos(x)
        y(:,6)=sin(x)*exp(-x**2)


        !Draw the matrix y against vector x
        call g%title  ('Example 7. Plotting a Matrix against a vector')
        call g%xlabel ('my x axis')
        call g%ylabel ('my y axis')
        call g%plot   (x, y)
    end subroutine exmp07


    !...............................................................................
    ! Example 08. Plot a matrix against a vector and set the linespec and legend (key)
    !...............................................................................
    subroutine exmp08

        type(gpf):: matplot
        integer, parameter:: n=25, m=6
        integer :: i
        real(wp):: tf
        real(wp):: vo
        real(wp):: g
        real(wp):: t(n)
        real(wp):: y(n,m)

        !Create data
        tf=10.d0
        g=32.d0;
        t=linspace(0.d0,tf,n)
        do i = 1, m
            vo = 25.0d0 * i
            y(:, i) = vo*t-0.5d0*g*t**2
        end do

        !Draw the matrix y againest vector x
        call matplot%title('Example 8. Plotting a Matrix against a vector')
        call matplot%xlabel ('t, sec')
        call matplot%ylabel ('y, feet')
        call matplot%options('set xrange[0:10];set yrange [0:400];')
        call matplot%plot(t, y)

        !Another Matrix plot with legends and line specification
        call matplot%title('Example 8.2: Matrix plot, legends and linespec')
        call matplot%plot(t, 2.0d0*y(:,3:4), &
            lspec='t "vo=100" w lp lt 6 ps 3 lw 2;&
            & t "v=125" w lp lt 7 ps 3 lw 2 lc rgb "#ad6000"')

    end subroutine exmp08


    !...............................................................................
    ! Example 09: Use gnuplot for annimation
    !...............................................................................
    subroutine exmp09

        type(gpf):: gp
        integer, parameter::   n  = 35
        real(wp), parameter :: pi = 4.d0*atan(1.d0)
        real(wp):: x(n)
        real(wp):: y(n), z(n)
        integer :: i


        x=linspace(-pi, pi,n)
        y = 0.0_wp
        z = 0.0_wp
        call gp%animation_start(1) ! start animation, set delay is one second between frames
        call gp%axis([-pi, pi, -1.2_wp, 1.2_wp])
        call gp%title('A beautiful animation using ogpf library', textcolor='#aa5500')
        ! add frames
        do i=1, n, 1
            y(i) = sin(x(i))
            z(i) = cos(x(i))
            ! each plot command adds one frame
            call gp%plot(x(1:i),y(1:i), 'w lines lc "red" lw 2','', &
                x(i:i), y(i:i),'w points ps 3 pt 7 lc "red"','', &
                x(1:i),z(1:i), 'w lines lc "blue" lw 2', '',&
                x(i:i), z(i:i), 'w points ps 3 pt 7 lc "blue"' )
        end do
        ! finalize and show frames one by one with delay between them
        ! as set by animation_start
        call gp%animation_show()


    end subroutine exmp09


    !...............................................................................
    ! Example 10: Use gnuplot options
    !...............................................................................
    subroutine exmp10()


        type(gpf):: mp
        real(wp):: x(15)
        real(wp):: y(15)

        !Options is a dynamic length string and can set all kind of gnuplot
        call mp%options('set style line 1 lc rgb "#0060ad" lt 1 lw 2 pt 5 ps 1.5 # --- blue')
        call mp%options('set style line 2 lc rgb "#ad6000" lt 2 lw 2 pt 6 ps 1.5 # --- red')
        call mp%options('set style line 3 lc rgb "#00ad00" lt 2 lw 2 pt 7 ps 1.5 # --- green')
        ! this is a multipart string spanned over several lines
        call mp%options('&
            &set style data linespoints;&
            &set xrange [0.1:100];&
            &set yrange [0.01:10000];&
            &set autoscale')
        call mp%options('set key top left') ! set the key location

        x=linspace(0.1d0,100d0,15);
        y=x**2;
        call mp%title("Example 10. x vs. x^2")
        call mp%plot(x1=x, y1=1.50*y, ls1='t "y=1.5x^2" ls 1', &
            x2=x, y2=2.00*y, ls2='t "y=2.0x^2" ls 2', &
            x3=x, y3=2.50*y, ls3='t "y=2.5x^2" ls 3')
        call mp%reset()
        call mp%title('Reset to initial setting')
        call mp%plot(x,2*y)
    end subroutine exmp10


    !...............................................................................
    ! Example 11: A simple polar plot
    !...............................................................................
    subroutine exmp11
        type(gpf):: gp
        integer, parameter :: n=125
        real(wp):: t(n)
        real(wp):: r(n)
        real(wp):: pi=4.d0*atan(1.d0)
        !1. reset gplot
        !!!   CALL gp%reset()
        ! TODOD: There is a problem with reset, persist is off by reset
        !2. set option, and set plot as polar
        call gp%options("&
            &set polar;&
            &set trange [-pi:pi]")

        ! 3. create data
        t=linspace(-pi,pi,n)
        r=sin(3.d0*t)

        !Annotation, set title, xlabel, ylabel
        call gp%title("Example 11: simple polar plot")
        call gp%xlabel("x,...")
        call gp%ylabel("y,...")

        !Call plot method
        call gp%plot(t,r, 'title "sin(3t)"')
        call gp%plot(t, cos(4*t))

    end subroutine exmp11


    !...............................................................................
    ! Example 12: A simple plot with logarithmic x axis
    !...............................................................................
    subroutine exmp12
        type(gpf):: gp
        integer, parameter :: n=70
        real(wp):: x(n)
        real(wp):: y(n)


        ! 1. create data
        x=linspace(0.0d0,10.d0,n)
        y=10**(exp(-x/2.d0)*sin(2.d0*x))


        !Annotation, set title, xlabel, ylabel
        call gp%title("Example 12: A semi-log x plot")
        call gp%xlabel("x,logarithmic scale")
        call gp%ylabel("y, normal scale")

        !Sample 1
        call gp%semilogx(y,x)


    end subroutine exmp12


    !...............................................................................
    ! Example 13: A simple matrix plot with logarithmic y axis
    !...............................................................................
    subroutine exmp13
        type(gpf):: gp
        integer, parameter :: n=25
        real(wp):: x(n)
        real(wp):: y(n)


        ! 1. create data
        x=linspace(0.1d0,10.d0,n)
        y=5.d0*x**3+4.d0*x**2+3.d0*x+1.d0

        !Annotation, set title, xlabel, ylabel
        call gp%title("Example 13: A simple matrix plot with semi-log y")
        call gp%ylabel("y,logarithmic scale")
        call gp%xlabel("x, normal scale")

        ! plot a matrix against vector in logarithmic y axis with line specification
        call gp%semilogy(x,reshape([y,10.d0*y],[n,2]), 'with lines lt 8; with points pt 7')

    end subroutine exmp13


    !...............................................................................
    ! Example 14: A simple plot with logarithmic xy axes
    !...............................................................................
    subroutine exmp14
        type(gpf):: gp
        integer, parameter :: n=75
        real(wp):: x(n)
        real(wp):: y(n)
        real(wp):: pi=4.d0*atan(1.d0)

        ! 1. create data
        x=exp(linspace(0.d0,2.d0*pi,n))
        y=50.d0+exp( 3.d0* linspace(0.d0,2.d0*pi,n) )

        ! 2. Annotation, set title, xlabel, ylabel
        call gp%title("Example 14: A loglog plot")
        call gp%xlabel("x,logarithmic scale")
        call gp%ylabel("y,logarithmic scale")

        ! 3. Set grid on
        call gp%options('set grid xtics ytics mxtics')

        ! 4. Call plot method
        call gp%loglog(x,y)

    end subroutine exmp14

    !...............................................................................
    ! Example 15: Plotting a function
    !...............................................................................
    subroutine exmp15()
        type(gpf):: gp
        real(wp):: pi=4.d0*atan(1.d0)

        call gp%title("Example 15. Plotting a function using fplot")
        call gp%xlabel("x...")
        call gp%ylabel("y...")
        ! call fplot to plot a function in range of [a, b] with n points
        call gp%fplot(myfun,[0d0,15.d0*pi],150)

    end subroutine exmp15

    function myfun(x)
        ! The function to be plotted
        ! see example 15
        real(wp), intent(in) :: x
        real(wp):: myfun
        myfun=x*sin(x)
    end function myfun


    !...............................................................................
    ! Example 16: Save the gnuplot script into a file for future use
    !...............................................................................
    subroutine exmp16()
        type(gpf):: gp
        real(wp):: pi=4.d0*atan(1.d0)
        integer, parameter :: n=100
        real(wp) :: x(n)
        real(wp) :: y(n)
        real(wp) :: z(n)

        ! create data
        x = linspace(-pi, 3.0d0*pi)
        y = sin(2.0d0*x)*exp(-x/5.0d0)
        z = cos(2.0d0*x)*exp(-x/5.0d0)

        ! several gnuplot optuons
        call gp%options('set border linewidth 1.5')
        call gp%options('set style line 1 lc rgb "#ad6009" lt 1 lw 2 pt 7 ps 1.5 # --- red like')
        call gp%options('set style line 2 lc rgb "#00ad09" lt 2 lw 2 pt 6 ps 1.5 # --- green like')
        call gp%options('unset key')
        call gp%options('set grid')
        call gp%options('set ytics 1')
        call gp%options('set tics scale 0.75')

        call gp%title("Example 16. Save the script into file for future use")
        call gp%xlabel("x...")
        call gp%ylabel("y...")

        ! save the script into a file
        call gp%filename("Example16.gp")
        call gp%plot(x, y, ls1='with lp ls 1', x2=x, y2=z, ls2='with lp ls 2')

        print*  ! empty line
        print*, 'Plot commands were written in Example16.gp successfully'
        print*, 'Open gnuplot and load this script file to plot the results!'
        print*  ! empty line

    end subroutine exmp16


    !...............................................................................
    ! Example 17: Plot in two separate windows using script
    !...............................................................................
    subroutine exmp17()
        ! Script is used to create multi window plot
        ! Each "set term wxt <number>" creates a new window
        type(gpf):: gp

        call gp%add_script('set term wxt 0 title "My first plot" size 640,480')
        call gp%add_script('set title "Example 17. Multi windows plot using script"')
        call gp%add_script('plot x*x+2*x+1')
        call gp%add_script('set term wxt 1 title "My second plot"')
        call gp%add_script('set ylabel "xsin(x)"')
        call gp%add_script('plot x*sin(x)')

        call gp%run_script()

    end subroutine exmp17

    !...............................................................................
    ! Example 18: Running an external script file
    !...............................................................................

    subroutine exmp18()

        !Use gnuplot script
        !to send a special external script file to gnuplot
        !the file is an external file here is called "simple.plt"
        type(gpf):: gp

        ! add some options and commands
        call gp%title("Example 18. Running an external script file")
        call gp%add_script('load "sample_script.gp" ')

        ! run script
        call gp%run_script()

    end subroutine exmp18


    !...............................................................................
    !Example 19: USE multiple linespec in plotting a matrix vs a vector
    !...............................................................................
    subroutine exmp19
        ! see also example 8
        type(gpf):: gp
        integer, parameter:: n=17
        real(wp):: x(n)
        real(wp):: y(n,8)
        integer :: j
        ! Input data
        x=linspace(0.d0,10.d0,17)
        do j=1, 8
            y(:,j)=dble(3*j)
        end do


        !Annotation, set title, xlabel, ylabel
        call gp%title('Example 19. Matrix plot with different line specification')
        call gp%xlabel('my x axis ...')
        call gp%ylabel('my y axis ...')
        call gp%options('set key bottom right')
        call gp%axis([-2.d0,20.d0,-2.d0,20.d0])

        !Call Plot to draw a matrix against a vector of data
        call gp%plot(x,y,lspec='with linespoints lt 1 title "lt 1";&
            & with lp lt 2 title "lt 2";&
            & with lp lt 3 title "lt 3";&
            & with lp lt 4 title "lt 4";&
            & with lp lt 5 title "lt 5";&
            & with lp lt 6 title "lt 6";&
            & with lp lt 7 title "lt 7";&
            & with lp lt 8 title "lt 8"')


    end subroutine exmp19

    !...............................................................................
    ! Example 20: Making a scatter plot
    !...............................................................................
    subroutine exmp20()
        type(gpf):: gp
        integer,  parameter :: n=750
        real(wp) :: x(n), y(n), ym(n), noise(n), d(n)
        real(wp) :: a, b

        ! generate data
        a = 00.0_wp
        b = 05.0_wp
        ! 1. generate the model data
        x  = linspace(a,b,n)
        ym = sqrt(x)  ! model data


        ! 2. generate the measured data with noise (ensembles the experimental data)
        call random_number(noise)       ! generate noise in [0, 1]
        d = (b-a)/100.0_wp * (x-a)**2   ! define the deviation function
        d = 2.0_wp*(noise - 0.5_wp) * d ! distribute noise around y=0
        y = ym + d


        call gp%title('Example 20. Scatter plot')
        call gp%xlabel('x,....')
        call gp%options('set key top left')
        call gp%options('set autoscale fix')
        call gp%options('set style line 1 lc rgb "blue" lt 1 lw 2 pt 6 ps 1.5')
        call gp%plot(x, y,  't "exp data" with points pt 6 ps 1.2 lc rgb "#ad2060"', &
            x2=x, y2=ym, ls2='t "mode: y=sqrt(x)" w lines lt 1 lw 3 lc "blue"')

        ! plot only the experimental data
        call gp%title('Example 20. Scatter plot: data with noise')
        call gp%plot(x,y,'w l lc "blue"', x2=x, y2=ym, ls2='w l lc "dark-red" lw 2')


    end subroutine exmp20

    !...............................................................................
    ! Example 21: Making a stem plot
    !...............................................................................
    subroutine exmp21()
        type(gpf):: gp
        integer,  parameter :: n=50
        real(wp) :: x(n), y(n)
        real(wp), parameter :: pi=4.0_wp*atan(1.0_wp)

        ! generate data
        x = linspace(0.0_wp, 4.0_wp*pi, n)
        y = exp(-x/4.0)*sin(x)

        call gp%title('Example 21. Stem plot')
        ! making plot
        call gp%plot(x,y, 'with impulses lw 2.5', &
            x2=x, y2=y,  ls2='with points pt 7')

    end subroutine exmp21


    !...............................................................................
    ! Example 22: Stem plot animation
    !...............................................................................
    subroutine exmp22()

        type(gpf):: gp
        integer,  parameter :: n=50
        real(wp) :: x(n), y(n)
        real(wp), parameter :: pi=4.0_wp*atan(1.0_wp)
        integer :: i

        ! generate data
        x = linspace(0.0_wp, 4.0_wp*pi, n)
        y = exp(-x/4.0)*sin(x)
        ! important, set the xy axis range
        call gp%axis([0.0_wp, 4.0_wp*pi, -1.0_wp, 1.0_wp])

        ! start animation
        call gp%animation_start(pause_seconds=1) ! one second delay between frames
        do i=1,n ! add frames
            ! each plot command adds one frame
            call gp%plot(x(1:i), y(1:i), ls1='with impulses lw 2', &
                x2=x(1:i), y2=y(1:i),  ls2='with points pt 6')
        end do
        ! finalize and show all frames in ornithological order with pause as
        ! set by animation_start
        call gp%animation_show()

    end subroutine exmp22


    !...............................................................................
    ! Example 23: Another animation using matrix plot
    !...............................................................................
    subroutine exmp23()
        type(gpf):: gp
        integer,  parameter :: n=55
        real(wp), parameter :: pi=4.0_wp*atan(1.0_wp)
        real(wp) :: x(n), y(n,2)
        integer :: i

        ! generate data
        x = linspace(0.0_wp, 4.0_wp*pi, n)
        y(:,1) = exp(-x/4.0)*sin(x)
        y(:,2) = exp(-x/8.0)*cos(x)

        call gp%axis([0.0_wp, 4.0*pi, -0.8_wp, 1.0_wp])

        call gp%animation_start(1)
        do i=1, n, 2
            ! making plot: add frames
            call gp%plot(x(1:i),y(1:i, :), 'with lines lw 2')
        end do
        call gp%animation_show()

    end subroutine exmp23


    !...............................................................................
    ! Example 24: Use of multiplot layout
    !...............................................................................
    subroutine exmp24()

        type(gpf):: gp
        integer,  parameter :: n=25
        real(wp), parameter :: pi=4.0_wp*atan(1.0_wp)
        real(wp) :: x(n), y(n,2)
        x=linspace(-pi, pi, n)
        y(:,1) = sin(x)*exp(-x/2.0)
        y(:,2) = cos(x)*(1-exp(-x/5.0))


        call gp%multiplot(2,1)  ! create a 2x1 multiplot layout

        call gp%title('Example 24. Multiplot layout, first row')
        call gp%xlabel('x1, ...')
        call gp%ylabel('y1, ...')
        call gp%plot(x, y(:,1), 'pt 7 lw 2 lc "red"') ! plot in first row

        call gp%title('Example 24. Multiplot layout,second row')
        call gp%xlabel('x2, ...')
        call gp%ylabel('y2, ...')

        call gp%plot(x, y(:,2), 'pt 6 lw 2 lc "blue"') ! plot in the second

    end subroutine exmp24


    !...............................................................................
    ! Example 25: Use multiplot followed by other plot command
    !...............................................................................
    subroutine exmp25()

        type(gpf):: gp
        integer,  parameter :: n=25
        real(wp), parameter :: pi=4.0_wp*atan(1.0_wp)
        real(wp) :: x(n), y(n,4)
        integer :: i

        x=linspace(-pi, pi, n)
        y(:,1) = sin(x)
        y(:,2) = sin(x)*cos(x)
        y(:,3) = (1-x)*sin(x)
        y(:,4) = (1-x)*cos(x)

        ! general options
        call gp%options('set tics font ",8"')

        call gp%multiplot(2,2)
        do i=1, 4
            call gp%plot(x, y(:,i), 'lt 4 pt 6')
        end do
        ! a new window will be started when all places in the multiplot
        ! layout is occupied. The multiplot window will be closed
        call gp%plot(x,y)
    end subroutine exmp25



    !...............................................................................
    !Example 26: Plot matrix against matrix
    !...............................................................................
    subroutine exmp26
        ! see also example 8
        type(gpf):: gp
        integer, parameter:: n=35
        integer, parameter:: m=3
        real(wp)  :: x(n,m)
        real(wp)  :: y(n,m)

        ! create data
        x(:,1) = linspace(-3.15d0,3.15d0,n)
        x(:,2) = 3.15d0 + x(:,1)
        x(:,3) = 3.15d0 - x(:,1)

        y(:,1) =1.0d0 * sin(x(:,1)) * (1-x(:,1))
        y(:,2) =2.0d0 * sin(x(:,1)) * (1-x(:,1))
        y(:,3) =3.0d0 * sin(x(:,1)) * (1-x(:,1))


        !Annotation, set title, xlabel, ylabel
        call gp%title('Example 26. Plot matrix vs. matrix')
        call gp%xlabel('my x axis ...')
        call gp%ylabel('my y axis ...')
        call gp%options('set key bottom left')


        !Call Plot to draw a matrix against a vector of data
        call gp%plot(x,y,lspec='t "y1" lw 2 ps 2 pt 6; t "y2" lw 2 ps 2 pt 7; t "y3" lw 2 ps 2 pt 9')

    end subroutine exmp26


    !...............................................................................
    !Example 27: Using secondary y axis
    !...............................................................................
    subroutine exmp27()

        type(gpf):: gp
        integer, parameter:: n1=50, n2=75
        real(wp):: x(n1), u(n1)
        real(wp):: t(n2), v(n2)

        ! Input data
        x = linspace(-5._wp,5._wp,n1)
        t = linspace(0._wp,15._wp,n2)
        u = sin(x) * cos(x)
        v = t*sin(t)
        ! Annotation: set title, xlabel, ylabel
        call gp%title('Example 27. Using secondary y axis')
        call gp%xlabel('primary x axis ...')
        call gp%ylabel('primary y axis ...')
        call gp%y2label('secondary y axis ...')

        ! turn on the y2 tics
        call gp%options('set y2tics')

        ! plot first data set with primary axes (defualt)
        ! and the second data set with secondary y axis
        call gp%plot(x, u, ls1= 't "primary y axis"', x2=t, y2=v, axes2='x1y2', ls2='t "secondary y axis"')

    end subroutine exmp27


    !...............................................................................
    !Example 28: Using secondary x and secondary y axis
    !...............................................................................
    subroutine exmp28()
        ! exmp28
        type(gpf):: gp
        integer, parameter:: n=50
        real(wp):: t(n), v(n)

        ! Input data
        t = linspace(-3.1415d0, 3.1415d0, n)
        v = sin(t) * cos(t)

        ! Annotation: set title, xlabel, ylabel
        call gp%title  ('Example 28. Plot using secondary x and y axis')
        call gp%xlabel ('primary x axis ...')
        call gp%ylabel ('primary y axis ...')
        call gp%x2label('secondary x axis ...')
        call gp%y2label('secondary y axis ...')

        ! use different color for secondary axis
        call gp%options('set x2tics tc "red"; set y2tics tc "red"')
        call gp%options('set xtics tc "blue"; set ytics tc "blue"')

        call gp%plot(t,v, axes1='x1y1', ls1='lc "blue" lw 2', x2=t, y2=t*t*v, axes2='x2y2', ls2='lc "red"')

    end subroutine exmp28

    !...............................................................................
    !Example 29: Use color, font and size for labels
    !...............................................................................
    subroutine exmp29

        type(gpf):: gp
        real(wp), dimension(100) :: x, y

        ! create data
        call random_number(x)
        call random_number(y)

        !Annotation, set title, xlabel, ylabel with color and font and rotation
        call gp%title('Example 29. Use color, font and size for labels', textcolor='blue', font_size=14)
        call gp%xlabel('rotated ...',   textcolor='red', font_size=14, font_name='arial', rotate=10)
        call gp%ylabel('my y axis ...', textcolor='violet')
        call gp%options('set key bottom left')
        call gp%options('set style data points')


        !Call Plot to draw a matrix against a vector of data
        call gp%plot(x,y, 'lt 5 ps 1.25')

    end subroutine exmp29


    !...............................................................................
    !Example 30: More on colors and size of labels
    !...............................................................................
    subroutine exmp30()
        ! exmp30 shows more customization on ogpf
        type(gpf) :: plt

        integer, parameter :: n=40
        integer, parameter :: m=80
        real(wp), dimension(n)  :: t1, u1
        real(wp), dimension(m)  :: t2, u2

        character(len=:), allocatable :: colorp, colors
        character(len=:), allocatable :: ls1, ls2

        ! define rgb colors for primary and secondary axes
        colorp = '"#D95F02"'
        colors = '"#7570B3"'

        ! set the line specification and legends
        ls1='t "primary"   lt 4 lw 1.25 ps 1.2 lc rgb  '  // colorp
        ls2='t "secondary" lt 5 lw 1.25 ps 1.2 lc rgb '  // colors

        ! create data
        t1 = linspace(-10._wp, 10._wp, n)
        t2 = linspace(-3.14_wp, 3.14_wp, m)

        u1 = 0.5_wp * t1**2
        u2 = exp(-t2)*sin(2.0*t2)


        ! turn on the second axes
        call plt%options('set xtics tc ' // colorp)
        call plt%options('set ytics tc ' // colorp)

        call plt%options('set x2tics tc ' // colors)
        call plt%options('set y2tics tc ' // colors)

        ! set labels and title
        call plt%title('Example 30. A customizes plot', textcolor='#E7298A', font_size = 15)
        call plt%xlabel('primary x', textcolor=colorp(2:8), font_size = 9)
        call plt%ylabel('primary y', textcolor=colorp(2:8), font_size = 9)

        call plt%x2label('secondary x', textcolor=colors(2:8), font_size = 12)
        call plt%y2label('secondary y', textcolor=colors(2:8), font_size = 12)

        ! plot data
        call plt%plot(t1,u1,ls1=ls1 , axes1='x1y1', x2=t2,y2=u2, ls2=ls2, axes2='x2y2')

    end subroutine exmp30



    !-------------------------------------------------------------------------------
    ! 3D Plots
    !-------------------------------------------------------------------------------

    ! surface plot examples  (gnuplot equivalent routine is splot)
    ! contour plot examples
    ! animation of 3D plots


    !...............................................................................
    !Example 101: A simple 3d plot
    !...............................................................................
    subroutine exmp101

        type(gpf):: gp
        real(wp), allocatable:: x(:,:)
        real(wp), allocatable:: y(:,:)
        real(wp), allocatable:: z(:,:)
        real(wp):: a=0.5d0
        real(wp):: b=2.0d0
        real(wp), allocatable :: xgrid(:)
        real(wp), allocatable :: ygrid(:)
        integer:: m
        integer:: n

        ! create 3D data
        m=55 ! number of grid points in y direction
        n=25 ! number of grid points in x direction
        xgrid=linspace(-10.0_wp, 10.0_wp, m)
        ygrid=linspace(0.0_wp, 5.0_wp, n)
        allocate( z(m,n) )

        call meshgrid(x, y, xgrid, ygrid) ! generate the 2D grid data
        z=(x**2/a - y**2/b)

        ! annotation
        call gp%title('Example 101: Simple 3D plot')
        call gp%xlabel('x-axis,...')
        call gp%ylabel('y-axis,...')
        call gp%zlabel('z-axis,...')

        !plot the 3D data
        call gp%surf(x, y, z, lspec='t "default color spec"' ) ! color palette: gnuplot default
        call gp%surf(x, y, z, lspec='t "Ann Schnider set1"', palette='set1' ) ! color palette: set1
        call gp%surf(x, y, z, lspec='t "Matlab Jet"', palette='jet' ) ! color palette: Matlab jet
    end subroutine exmp101


    !...............................................................................
    !Example 102: Another simple surface plot with legend and line color
    !...............................................................................
    subroutine exmp102()
        type(gpf):: gp

        real(wp), parameter :: pi=4.0_wp*atan(1.0_wp)

        real(wp), allocatable:: x(:,:)
        real(wp), allocatable:: y(:,:)
        real(wp), allocatable:: z(:,:)
        integer:: m
        integer:: n

        ! generate data
        call meshgrid(x, y, linspace(-0.75_wp*pi, 0.75_wp*pi, 35) ) ! xgrid == ygrid
        m=size(x,1)
        n=size(x,2)
        allocate( z(m,n) )

        !z= sin(x) * cos (y)
        where (x**2 + y**2 == 0.0_wp)
            z=1.0_wp
        elsewhere
            z=sin(x**2+y**2)/(x**2+y**2)
        end where


        call gp%title('Example 102: Simple 3D plot with color palette')
        call gp%xlabel('x-axis,...')
        call gp%ylabel('y-axis,...')

        !plot the 3D data
        CALL gp%surf(X,Y,Z, palette='jet')

    end subroutine exmp102


    !...............................................................................
    !Example 103: A beautiful surface plot with hidden details
    !...............................................................................
    subroutine exmp103()

        type(gpf):: gp
        real(wp), allocatable:: x(:,:)
        real(wp), allocatable:: y(:,:)
        real(wp), allocatable:: z(:,:)
        real(wp):: a=-0.5_wp
        real(wp):: b= 0.5_wp
        real(wp):: pi= 4.0_wp * atan(1.0_wp)
        integer:: m
        integer:: n

        ! create 3D data
        call meshgrid( x, y, linspace(a, b, 55) )
        m=size(x,1)
        n=size(x,2)
        allocate( z(m,n) )
        z= cos(2.0*pi*x) * sin(2.0*pi*y)



        ! annotation
        call gp%title('Example 103: A beautiful surface plot with hidden details')
        call gp%options('set hidden3d')
        call gp%options('set tics font ",8"')
        call gp%options('unset key')

        !plot the 3D data
        call gp%surf(x, y, z, palette='jet' ) ! color palette: Matlab jet
        ! contour
        call gp%contour(x,y,z, palette='set1')

    end subroutine exmp103


    !...............................................................................
    !Example 104: Cylindrical mapping
    !...............................................................................
    subroutine exmp104()
        type(gpf):: gp
        integer, parameter :: m = 35
        integer, parameter :: n = 15
        real(wp)           :: xv(m), yv(n)
        real(wp), dimension(:,:), allocatable:: x,y,z
        real(wp):: pi= 4.0_wp * atan(1.0_wp)

        xv = linspace(0.0_wp, pi, m)
        yv = linspace(0.0_wp, pi, n)
        call meshgrid(x,y, xv, yv)
        allocate( z(size(x,dim=1), size(x, dim=2)) )
        z = sin(y)


        ! advanced options
        call gp%options('set mapping cylindrical')
        call gp%options('unset tics')
        call gp%options('unset border')
        call gp%options('set view 147,312')
        call gp%options('set hidden3d')

        !
        call gp%title('Example 104. Cylindrical Mapping')
        call gp%surf(x,y,z)

    end subroutine exmp104

    !...............................................................................
    !Example 105: contour plot
    !...............................................................................
    subroutine exmp105()
        type(gpf):: gp

        real(wp), allocatable:: x(:,:)
        real(wp), allocatable:: y(:,:)
        real(wp), allocatable:: z(:,:)
        integer:: m
        integer:: n
        real(wp), parameter :: pi=4.0_wp*atan(1.0_wp)

        ! create the xyz data
        call meshgrid(x, y, linspace(-2.0_wp,2.0_wp, 65), linspace(-2.0_wp,3.0_wp, 65)  )

        m=size(x,1)
        n=size(x,2)
        allocate( z(m,n) )

        z = x * exp(-x**2 - y**2)

        call gp%options('unset key')
        call gp%options('unset surface')
        call gp%axis([real(wp):: -2, 2, -2, 3])

        !plot the contour
        call gp%options('unset border; unset tics')
        call gp%title('Example 105: Surface plot')
        call gp%surf(x,y,z, palette='accent')
        call gp%title('Example 105: Contour plot')
        call gp%contour(x,y,z, palette='jet')

    end subroutine exmp105


    !...............................................................................
    !Example 106: Animation of surface plot
    !...............................................................................
    subroutine exmp106()

        type(gpf):: gp
        integer,  parameter :: n=25, m=55
        real(wp) :: xv(m), yv(n), t
        real(wp), allocatable :: x(:,:), y(:,:), z(:,:)
        real(wp), parameter :: pi=4.0_wp*atan(1.0_wp)

        ! generate data
        xv = linspace(0.0_wp, 2.0_wp*pi, m)
        yv = linspace(0.0_wp, 2.0_wp*pi, n)
        call meshgrid(x, y, xv, yv)
        z = sin(x) + cos(y)

        call gp%title('Example 106. Animation of surface plot')
        call gp%axis([0.0_wp, 2.0*pi, 0.0_wp, 2.0*pi])
        call gp%options('unset colorbox')
        call gp%options('set ticslevel 0')
        call gp%axis([0.0_wp, 2.0*pi, 0.0_wp, 2.0*pi, -2.0_wp, 2.0_wp])

        call gp%animation_start(1)
        t=0.050_wp
        do
            ! add frames
            call gp%surf(x, y, sin(t*pi/2.0)*z, palette='jet')
            t=t+0.1_wp
            if (t > 1.0_wp) exit
        end do
        ! show frames in ornithological order with a pause set in
        ! animation_start
        call gp%animation_show()

    end subroutine exmp106

    !...............................................................................
    !Example 107: Multiplot layout for 3D data
    !...............................................................................
    subroutine exmp107()
        type(gpf):: gp

        real(wp), allocatable:: x(:,:)
        real(wp), allocatable:: y(:,:)
        real(wp), allocatable:: z1(:,:)
        real(wp), allocatable:: z2(:,:)
        integer:: m
        integer:: n
        real(wp), parameter :: pi=4.0_wp*atan(1.0_wp)

        ! create the xyz data
        call meshgrid(x, y, linspace(-pi,pi, 60)  )

        m=size(x,1)
        n=size(x,2)
        allocate( z1(m,n) )
        allocate( z2(m,n) )

        z1 = sin(x) + cos(y)
        z2 = sin(x) * cos(y)

        call gp%options('unset key')
        call gp%axis([-pi,pi,-pi,pi])
        call gp%options('unset colorbox')
        call gp%options('set autoscale fix')
        call gp%options('unset tics')

        !plot the contour
        call gp%title('Example 107: surface plot')
        call gp%multiplot(1,2)
        call gp%surf(x,y,z1)
        call gp%surf(x,y,z2)

        call gp%multiplot(2,1)
        call gp%title('Example 107: Contour plot')
        call gp%options('set colorbox')
        call gp%options('set tics')
        call gp%options('set tics font ",8"') ! font size for tics
        call gp%contour(x,y,z1, palette='jet')
        call gp%contour(x,y,z2, palette='set1')


    end subroutine exmp107


    !...............................................................................
    !Example 108: Plod a 2D grid
    !...............................................................................
    subroutine exmp108
        type(gpf):: gp
        integer, parameter:: n=10
        real(wp), allocatable :: x(:,:)
        real(wp), allocatable :: y(:,:), z(:,:)

    ! create data
    call meshgrid(x,y,linspace(1.0d0, 10.0d0, n))
    allocate(z(n,n))
    z=1.0d0

    ! set gnuplot  options
    call gp%options('unset border') ! turn off border line (axes)
    call gp%options('unset tics')   ! turn off axes values (tics)
    call gp%options('set view map') ! set viewpoint top down
    ! plot using linespec
    call gp%surf(x,y,z,lspec='w lp ps 2 pt 6 lc ""')

    end subroutine exmp108


end program demo
