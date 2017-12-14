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


! This file demonstrate the capability of ogpf module
! An object based Fortran interface to gnuplot



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

PROGRAM Demo

    USE ogpf
    IMPLICIT NONE
    INTEGER:: i=0
    PRINT *, 'gpf: gnuplot in Fortran Demonstration'
    PRINT*, "Example 1: Demo for xy plot"
    PRINT*, "Example 2: Line specification"
    PRINT*, "Example 3: Plot several data series at the same time"
    PRINT*, "Example 4: Plot four data series at the same time"
    PRINT*, "Example 5: Use line style, line colors and more..."
    PRINT*, "Example 6: An interesting plot, sin(x) and its zero on the same plot"
    PRINT*, "Example 7: Plot a matrix against a vector"
    PRINT*, "Example 8: Plotting a Matrix against a vector"
    PRINT*, "Example 9: A simple xy plot in an interactive session"
    PRINT*, "Example 10: x vs. x^2"
    PRINT*, "Example 11: simple polar plot"
    PRINT*, "Example 12: A plot with logarithmic x axis"
    PRINT*, "Example 13: A plot with logarithmic y axis"
    PRINT*, "Example 14: A plot with logarithmic x and y axes"
    PRINT*, "Example 15: A fplot example"
    PRINT*, "Example 16: Script files"
    PRINT*, "Example 17: Another sample script file"
    PRINT*, "Example 19: Line types in gnuplot"
    PRINT*, "Example 18: Running an external script file"
    PRINT*, "Example 20: Simple 3D plot using splot"
    PRINT*, "Example 21: Simple 3D plot using splot"



    Mainloop: DO
    PRINT*,
    PRINT*, "select an example: 1 through 21"
    PRINT*, "enter 0 for exit"
     READ*, i
    SELECT CASE(i)
     CASE(1)
        CALL Exmp01
     CASE(2)
        CALL Exmp02
     CASE(3)
        CALL Exmp03
     CASE(4)
        CALL Exmp04
     CASE(5)
        CALL Exmp05
     CASE(6)
        CALL Exmp06
     CASE(7)
        CALL Exmp07
     CASE(8)
        CALL Exmp08
    CASE(9)
        CALL Exmp09
     CASE(10)
        CALL Exmp10
     CASE(11)
        CALL Exmp11
     CASE(12)
        CALL Exmp12
     CASE(13)
        CALL Exmp13
     CASE(14)
        CALL Exmp14
    CASE(15)
        CALL Exmp15
    CASE(16)
        CALL Exmp16
    CASE(17)
        CALL Exmp17
     CASE(18)
        CALL Exmp18
    CASE(19)
        Call Exmp19
    CASE(20)
        CALL Exmp20
    CASE(21)
        CALL Exmp21
    CASE(101)
        CALL Exmp101
    CASE (0)
        PRINT*, "Program terminated successfully"
        EXIT Mainloop
    CASE DEFAULT
        PRINT*, "Try again, use a valid example number"
        PRINT*, "Enter 0 to exit"
    END SELECT
    END DO MainLoop
CONTAINS

    SUBROUTINE Exmp01
        !...............................................................................
        !Example 1: A very basic example
        !...............................................................................
        TYPE(gpf):: gp
        INTEGER, PARAMETER:: n=17
        Real(wp):: x(n)
        Real(wp):: y(n)
        ! Input data
        x=dble([-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8])
        y=dble([66,51,38,27,18,11,6,3,2,3,6,11,18,27,38,51,66])

        ! Annotation: set title, xlabel, ylabel
        CALL gp%title('Example 1. A simple xy plot')
        CALL gp%xlabel('my x axis ...')
        CALL gp%ylabel('my y axis ...')
        Call gp%options('set style data linespoints')
        !Call Plot to draw a vector against a vector of data
        CALL gp%plot(x, y)
    END SUBROUTINE Exmp01


    SUBROUTINE Exmp02
        !...............................................................................
        !Example 2: Set line specification and legends
        !...............................................................................
        TYPE(gpf):: gp
        INTEGER, PARAMETER:: n=17
        Real(wp):: x(n)
        Real(wp):: y(n)
        ! Input data
        x=dble([-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8])
        y=dble([66,51,38,27,18,11,6,3,2,3,6,11,18,27,38,51,66])

        ! Annotation: set title, xlabel, ylabel, line specification
        CALL gp%title('Example 2. A simple xy plot')
        CALL gp%xlabel('my x axis ...')
        CALL gp%ylabel('my y axis ...')

        !Call Plot to draw a vector against a vector of data
        !The last argument defines the line specification
        CALL gp%plot(x,y,'with linespoints lt 2 pt 4')
    END SUBROUTINE Exmp02


    SUBROUTINE Exmp03
        !...............................................................................
        ! Example 3: Plot several data set at the same time
        !...............................................................................
        TYPE(gpf):: g
        INTEGER, PARAMETER:: n=50
        INTEGER, PARAMETER:: m=65
        Real(wp):: x(n)
        Real(wp):: y(n)
        Real(wp):: xv(m)
        Real(wp):: yv(m)
        Real(wp), PARAMETER :: pi=4.d0*atan(1.d0)
        ! Input data
        x=linspace(-pi,pi,n)  !linspace is a utility function in ogpf module
        y=sin(x)              !linspace(a,b,n) create a linear vector in [a,b] with n elements

        xv=linspace(0.d0, 2.d0*pi,m)
        yv=cos(2.d0*xv)

        ! Annotation, set title, xlabel, ylabel
        CALL g%title('Example 3. Plot two data series using gnuplot')
        CALL g%xlabel(' x axis ...')
        CALL g%ylabel(' y axis ...')

        ! Sample 1: Plot to draw two set of data
        CALL g%plot(x,y,'title "sin"',xv,yv,'title "cos"')

        !Sample 2: Use keyword arguments to plot the same example as above
        CALL g%title('Example 3. Another plot using keyword arguments...')
        CALL g%plot(x1=x,y1=y,ls1='with points pt 8',x2=xv,y2=yv,ls2='title "cos(2x)"')

        ! Sample 3: An xy plot with line specification no legends
        CALL g%title('Example 3. Another plot using keyword arguments...')
        CALL g%plot(x,y,'title "sin(x)" lt 6', xv,yv,'title "cos(2x)" with points pt 7 lc rgb "#993300"')

    END SUBROUTINE Exmp03

    SUBROUTINE Exmp04
        !...............................................................................
        ! Example 4: Plot four data series, the maximum number of data series can be plotted!
        !...............................................................................
        TYPE(gpf):: gp
        INTEGER, PARAMETER:: n=50
        INTEGER, PARAMETER:: m=65
        Real(wp):: x(n)
        Real(wp):: y(n)
        Real(wp):: xv(m)
        Real(wp):: yv(m)
        Real(wp), PARAMETER :: pi=4.d0*atan(1.d0)
        ! Input data
        x=linspace(-pi,pi,n)  !linspace is a utility function from module ogpf
        y=sin(x)

        xv=linspace(0.d0, 2.d0*pi,m)
        yv=cos(2.d0*xv)
        !           This is the maximum number of plot can be drawn at the same time
        !           If you have more data see, you can plot can be used with matrices!
        CALL gp%title('Example 4. Plot four data sets using gnuplot')
        CALL gp%plot(x,y, 'title "sin(x)"', &
        xv,yv, 'with lp lt 6 title "cos(2x)"', &
        xv, 2.d0*yv, 'title "2cos(2x)" lt 7', &
        xv, 0.5d0*yv, 'title "0.5cos(2x)" with points pt 8')

        ! Another example with keyboard arguments
        CALL gp%FileName('exmp04.plt') ! The plot commands are saved into exmp04.plt for further use
        CALL gp%plot(x1=x,y1=y,x2=xv,y2=yv)

    END SUBROUTINE Exmp04

    SUBROUTINE Exmp05
        !...............................................................................
        ! Example 5: Use line style and legends
        !...............................................................................
        TYPE(gpf):: gplot
        INTEGER, PARAMETER:: n=50
        Real(wp):: x(n)
        Real(wp):: ys(n)
        Real(wp):: yc(n)
        Real(wp):: ysc(n)
        Real(wp), PARAMETER :: pi=4.d0*atan(1.d0)
        ! Input data
        x=linspace(-2.d0*pi,2.d0*pi,n)  !linspace is a utility function from module Utils
        ys=sin(x)
        yc=exp(-0.1d0*x)*cos(x)
        ysc=sin(x/2.d0)*cos(2.d0*x)

        ! Annotation, set title, xlabel, ylabel
        CALL gplot%title('Example 5. A sample with style and legends')
        CALL gplot%xlabel('x, rad')
        CALL gplot%ylabel('y, dimensionless')

        ! Plot to draw three set of data
        CALL gplot%plot(x,ys,'title "sin" with lines lt 5 lc rgb "#0008B0"', &
                        x,yc,'title "cos" with points lt 6 lc rgb "#FF1100"', &
                        x,ysc,'title "sin(x/2)cos(2x)" with lp lt 7 lc rgb "#00AA04"' )

    END SUBROUTINE Exmp05

   SUBROUTINE Exmp06
        !...............................................................................
        ! Example 6: Plot a single point along with a series of data
        !...............................................................................
        TYPE(gpf):: gplot
        INTEGER, PARAMETER:: n=125

        Real(wp):: x(n)
        Real(wp):: y(n)

        Real(wp), PARAMETER :: pi=4.d0*atan(1.d0)
        ! Input data
        x=linspace(0.d0,pi*2.d0,n)  !linspace is a utility function from module Utils
        y=sin(6.d0*x)*exp(-x)


        ! Annotation, set title, xlabel, ylabel
        CALL gplot%title('Example 6. A sample shows sin(x) and its zero on the plot')
        CALL gplot%xlabel('x, rad')
        CALL gplot%ylabel('sin(x), dimensionless')
        Call gplot%options('set grid')

        ! Plot to draw two set of data, a series and a single point
        CALL gplot%plot(x,y,'title "sin(x)" with lines lt 3', &
                        [pi],[0.d0],'title "zero" with points pt 7 ps 2 lc rgb "#FF0000"')
    END SUBROUTINE Exmp06


    SUBROUTINE Exmp07
    !...............................................................................
    ! Example 7: Plot a matrix against a vector
    !...............................................................................
        TYPE(gpf):: g
        INTEGER, PARAMETER:: n=25
        Real(wp), PARAMETER :: pi=4.d0*atan(1.d0)
        Real(wp):: x(n)
        Real(wp):: y(n,6)

        !Create data
        x=linspace(-pi,pi,n)
        y(:,1)=sin(x)
        y(:,2)=cos(x)
        y(:,3)=cos(0.5d0*x)
        y(:,4)=sin(0.5d0*x)
        y(:,5)=sin(x)*cos(x)
        y(:,6)=sin(x)*exp(-x**2)


        !Draw the matrix y against vector x
        CALL g%title  ('Example 7. Plotting a Matrix against a vector')
        CALL g%xlabel ('my x axis')
        CALL g%ylabel ('my y axis')
        CALL g%plot   (x, y)
    END SUBROUTINE Exmp07

    SUBROUTINE Exmp08
    !...............................................................................
    !Plot a matrix against a vector
    !...............................................................................
        TYPE(gpf):: MatPlot
        INTEGER, PARAMETER:: n=25
        Real(wp):: tf
        Real(wp):: vo
        Real(wp):: g
        Real(wp):: t(n)
        Real(wp):: y(n,6)

        !Create data
        tf=10.d0
        g=32.d0;
        t=linspace(0.d0,tf,n)
        vo=150.d0;
        y(:,1)=vo*t-0.5d0*g*t**2
        vo=125.d0;
        y(:,2)=vo*t-0.5d0*g*t**2
        vo=100.d0;
        y(:,3)=vo*t-0.5d0*g*t**2
        vo=75.d0;
        y(:,4)=vo*t-0.5d0*g*t**2
        vo=50.d0;
        y(:,5)=vo*t-0.5d0*g*t**2
        vo=25.d0;
        y(:,6)=vo*t-0.5d0*g*t**2

        !Draw the matrix y againest vector x
        CALL MatPlot%title('Example 8. Plotting a Matrix against a vector')
        CALL MatPlot%xlabel ('t, sec')
        CALL MatPlot%ylabel ('y, feet')
        call MatPlot%Options('set xrange[0:10];set yrange [0:400];')
        CALL MatPlot%plot(t, y)

        !Another Matrix plot with legends [line specification]
        CALL MatPlot%title('Example 8.2: Using legends when plotting a matrix against a vector')
        Call MatPlot%plot(t, y(:,1:2), lspec=["t 'vo=150'", "t 'vo=125'"])
    END SUBROUTINE Exmp08



   SUBROUTINE Exmp09
        !...............................................................................
        ! Example 09: Use gnuplot interactively
        !...............................................................................
        TYPE(gpf):: xyplot
        INTEGER, PARAMETER:: n=17
        Real(wp):: x(n)
        Real(wp):: y(n)
        ! Input data
        x=dble([-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8])
        y=dble([66,51,38,27,18,11,6,3,2,3,6,11,18,27,38,51,66])

        ! Annotation, set title, xlabel, ylabel
        CALL xyplot%title('Example 9. A simple xy plot in an interactive session')
        CALL xyplot%xlabel('my x axis ...')
        CALL xyplot%ylabel('my y axis ...')

        ! Keep gnuplot open for an interactive session
        ! You can enter any valid gnuplot commands and finally
        ! type q to exit
        CALL xyplot%hold('on')
        ! Call Plot to draw a vector against a vector of data
        CALL xyplot%plot(x,y)

     END SUBROUTINE Exmp09



    SUBROUTINE Exmp10()
        !Use gnuplot options
        TYPE(gpf):: mp
        Real(wp):: x(10)
        Real(wp):: y(10)
        !Option is a string of 320 character and can set all kind of gnuplot
        !global options

        call mp%options(&
               "set logscale xy;&
               &set xrange [0.1:100];&
               &set yrange [0.01:10000];")

        x=linspace(0.1d0,100d0,10); y=x**2;
        CALL mp%title("Example 10. x vs. x^2")
        CALL mp%plot(x,y)
        CALL mp%reset()
        CALL mp%plot(x,2*y)
    END SUBROUTINE Exmp10


!...............................................................................
! Example 11: A simple polar plot
!...............................................................................
    SUBROUTINE Exmp11
        TYPE(gpf):: gp
        INTEGER, PARAMETER :: n=75
        Real(wp):: t(n)
        Real(wp):: r(n)
        Real(wp):: pi=4.d0*atan(1.d0)
    !1. reset gplot
     CALL gp%reset()

    !2. set option, and set plot as polar
        call gp%options("&
            &set polar;&
            &set trange [-pi/2:pi/2]")

    ! 3. create data
        t=linspace(-pi/2.d0,pi/2.d0,n)
        r=sin(3.d0*t)

    !Annotation, set title, xlabel, ylabel
    CALL gp%title("Example 11: simple polar plot")
    CALL gp%xlabel("x,...")
    CALL gp%ylabel("y,...")

    !Call plot method
    CALL gp%plot(t,r)

    END SUBROUTINE Exmp11


!...............................................................................
! Example 12: A simple plot with logarithmic x axis
!...............................................................................
    SUBROUTINE Exmp12
        TYPE(gpf):: gp
        INTEGER, PARAMETER :: n=70
        Real(wp):: x(n)
        Real(wp):: y(n)


    ! 1. create data
        x=linspace(0.0d0,10.d0,n)
        y=10**(exp(-x/2.d0)*sin(2.d0*x))


    !Annotation, set title, xlabel, ylabel
    CALL gp%title("Example 12: A semi-log x plot")
    CALL gp%xlabel("x,logarithmic scale")
    CALL gp%ylabel("y, normal scale")

    !Sample 1
    CALL gp%semilogx(y,x)


    END SUBROUTINE Exmp12

!...............................................................................
! Example 13: A simple plot with logarithmic y axis
!...............................................................................
    SUBROUTINE Exmp13
        TYPE(gpf):: gp
        INTEGER, PARAMETER :: n=25
        Real(wp):: x(n)
        Real(wp):: y(n)


    ! 1. create data
        x=linspace(0.1d0,10.d0,n)
        y=5.d0*x**3+4.d0*x**2+3.d0*x+1.d0

    !Annotation, set title, xlabel, ylabel
    CALL gp%title("Example 13: A semi-log y plot")
    CALL gp%ylabel("y,logarithmic scale")
    CALL gp%xlabel("x, normal scale")

    !Sample 1: Call plot method
    CALL gp%semilogy(x,reshape([y,10.d0*y],[n,2]),[character(16)::'with lines lt 8','with points pt 7'])

    !Sample 2: plot a matrix against vector in logarithmic y axis with line specification
    CALL gp%semilogy(x,reshape([y,10.d0*y],[n,2]),[character(16)::'with lines lt 8','with points pt 7'])

    END SUBROUTINE Exmp13



!...............................................................................
! Example 14: A simple plot with logarithmic xy axes
!...............................................................................
    SUBROUTINE Exmp14
        TYPE(gpf):: gp
        INTEGER, PARAMETER :: n=75
        Real(wp):: x(n)
        Real(wp):: y(n)
        Real(wp):: pi=4.d0*atan(1.d0)

    ! 1. create data
        x=exp(linspace(0.d0,2.d0*pi,n))
        y=50.d0+exp( 3.d0* linspace(0.d0,2.d0*pi,n) )

    !Annotation, set title, xlabel, ylabel
    CALL gp%title("Example 14: A semi-log y plot")
    CALL gp%xlabel("x,logarithmic scale")
    CALL gp%ylabel("y,logarithmic scale")
    !Set grid on
    Call gp%options('set grid xtics ytics mxtics')


    !Call plot method
    CALL gp%loglog(x,y)

    END SUBROUTINE Exmp14


     SUBROUTINE Exmp15()
        !A demo to plot a function using fplot method
        TYPE(gpf):: gp
        Real(wp):: pi=4.d0*atan(1.d0)

        CALL gp%title("Example 15. A fplot example")
        CALL gp%xlabel("x...")
        CALL gp%ylabel("y...")
        CALL gp%fplot(myfun,[0d0,15.d0*pi],150)
        PRINT*, 'Plot commands were written in Example15.plt successfully'
        PRINT*, 'Open gnuplot and load this script file to plot the results!'
    END SUBROUTINE Exmp15

    FUNCTION myfun(x)
        !The function to be plotted
        !See example 15
        Real(wp), INTENT(IN) :: x
        Real(wp):: myfun
        myfun=x*sin(x)
    END FUNCTION myfun

    SUBROUTINE Exmp16()
        !Demo for gnuplot script
        !A script is a gnuplot set of commands to be sent through a file for gnuplot

        TYPE(gpf):: gp

        CALL gp%title("Example 16. Script files")
        !Example 16-1
        CALL gp%script("set xrange[-pi:pi]; set title 'script plot'; plot sin(x)")

        !Example 16-2
        CALL gp%script("splot [-2:2][-2:2] exp(-(x**2 + y**2))*cos(x/4)*sin(y)*cos(2*(x**2+y**2))")

        !Example 16-3
        CALL gp%script("set polar;plot t*sin(t);&
                        &set trange [-2*pi:2*pi]; set rrange [0:3];plot t*sin(t)")

    END SUBROUTINE Exmp16


    SUBROUTINE Exmp17()
        !Demo for gnuplot script
        TYPE(gpf):: gp
        CALL gp%title("Example 17. Another sample script file")
        CALL gp%script('splot x**2+y**2')

    END SUBROUTINE Exmp17

    SUBROUTINE Exmp18()
        !Use gnuplot script
        !to send a special script file to gnuplot
        !the file is an external file here is called "simple.dem"
        TYPE(gpf):: gp
        CALL gp%title("Example 18. Running an external script file")
        CALL gp%script("load 'simple.plt'")

    END SUBROUTINE Exmp18

SUBROUTINE Exmp19
        !...............................................................................
        !Example 19: Use multiple linespec in plotting a matrix vs a vector
        !...............................................................................
        TYPE(gpf):: gp
        INTEGER, PARAMETER:: n=17
        Real(wp):: x(n)
        Real(wp):: y(n,8)
        Integer :: j
        ! Input data
        x=linspace(0.d0,10.d0,17)
        do j=1, 8
            y(:,j)=dble(2*j)
        end do

        ! Sample I. Plot on the screen
        !Annotation, set title, xlabel, ylabel
        CALL gp%title('Example 1. A simple xy plot')
        CALL gp%xlabel('my x axis ...')
        CALL gp%ylabel('my y axis ...')
        Call gp%options('set style data linespoints')
        call gp%axis([-2.d0,20.d0,-2.d0,20.d0])
        !Call Plot to draw a vector against a vector of data
       !! call gp%axis([-12.d0,12.d0])
        CALL gp%plot(x,y,lspec=['with linespoints lt 1 title "lt 1"',&
                                'with linespoints lt 2 title "lt 2"',&
                                'with linespoints lt 3 title "lt 3"',&
                                'with linespoints lt 4 title "lt 4"',&
                                'with linespoints lt 5 title "lt 5"',&
                                'with linespoints lt 6 title "lt 6"',&
                                'with linespoints lt 7 title "lt 7"',&
                                'with linespoints lt 8 title "lt 8"'])


    END SUBROUTINE Exmp19







! 3D Plots
 SUBROUTINE Exmp20
        !A simple 3d plot
        TYPE(gpf):: gp
        Real(wp), ALLOCATABLE:: X(:,:)
        Real(wp), ALLOCATABLE:: Y(:,:)
        Real(wp), ALLOCATABLE:: Z(:,:)
        Real(wp):: a=0.5d0
        Real(wp):: b=2.0d0
        INTEGER:: m
        INTEGER:: n
        CALL meshgrid(X,Y,dble([-10,10,2]),dble([-10,10,2]))
        m=size(X,1)
        n=size(X,2)
        ALLOCATE( Z(m,n) )
        Z=(X**2/a - Y**2/b)

        !#Annotation, label colors are also set!
        CALL gp%FileName('Example20.plt')
        !Here options has been called several times instead of one time with a long string
        call gp%options("set style data lines")
        call gp%options("unset key;set contour base")
        call gp%options("set xyplane relative 0.1")

        CALL gp%title('Example 20: Simple 3D plot using splot')
        CALL gp%xlabel('x-axis,...')
        CALL gp%ylabel('y-axis,...')
        CALL gp%zlabel('z-axis,...')

        !plot the 3D data
        CALL gp%surf(Z,lspec= 'title "x^2/a - y^2/b"')
    END SUBROUTINE Exmp20

    SUBROUTINE Exmp21()
         !Another simple 3d plot
        TYPE(gpf):: gp
        Real(wp), ALLOCATABLE:: X(:,:)
        Real(wp), ALLOCATABLE:: Y(:,:)
        Real(wp), ALLOCATABLE:: Z(:,:)
        INTEGER:: m
        INTEGER:: n
        CALL meshgrid(X, Y, [Real(wp)::-15.,15.,0.75] )
        m=size(X,1)
        n=size(X,2)
        ALLOCATE( Z(m,n) )
        !Z=X*X*exp(-X*X) * Y*Y*exp(-Y*Y)
        !Z=x*y**3-y*x**3
        ! Z=-1/(X**2+Y**2+0.25)
        Z=sin(sqrt(x**2+y**2))/sqrt(x**2+y**2+0.025)

        CALL gp%title('Example 21: Simple 3D plot using splot')
        CALL gp%xlabel('x-axis,...')
        CALL gp%ylabel('y-axis,...')
        CALL gp%zlabel('z-axis,...')

        !plot the 3D data
        !CALL gp%surf(X,Y,Z,lspec= 'title "Sample 3D plot"')
        CALL gp%surf(Z) !,lspec= 'title "Sample 3D plot"')

    END SUBROUTINE Exmp21


    Subroutine Exmp101()
!Used in documentation, may be removed later.
        TYPE(gpf):: gp
        INTEGER, PARAMETER:: n=75, m=100
        Real(wp), Parameter:: pi=4.d0*atan(1.0d0)
        Real(wp):: v(n)
        Real(wp):: w(n)

        Real(wp):: x(m)
        Real(wp):: Y(m,3)
        Integer :: j

        !Example 1: vector plot
        v=linspace(-pi,pi,n)
        w=[(2**(-v(j)/2.d0)*sin(v(j)), j=1,n)]
        call gp%plot(v,w,"title 'simple plot' pt 7 lc rgbcolor 'blue'") !vector versus vector

        !Example 2: matrix plot
        x=linspace(-4.d0,1.d0,m)
        Y(:,1)=2.d0*exp(-x)*sin(2.d0*x);
        Y(:,2)=2.d0*exp(-x)*cos(2.d0*x);
        Y(:,3)=2.d0*exp(-x)*sin(x/2.d0);


        call gp%plot(x,Y) !matrix versus vector

    end subroutine Exmp101


END PROGRAM Demo
