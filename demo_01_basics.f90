module md_demo_01_basics
    ! md_demo_01_basics
    ! basic examples to demonstartes the ogp libarary for
    ! data visulization in modern fortran using gnuplot


    ! Mohammad Rahmani
    ! Chemical Engineering Department
    ! Amirkabir University of Technology
    ! Tehran, Ir
    ! m.rahmani[at]aut.ac.ir
    ! https:://github.com/kookma
    !
    ! Thursday 2018-02-08


    ! load gpf library
    use md_gpf
    use select_precision, only : wp

    ! parameters and variables
    implicit none
    !!!!
    !!!!    private
    !!!!    public :: Exmp01_basic, &
        !!!!        Exmp02_basic, &
        !!!!        Exmp03_basic, &
        !!!!        Exmp04_basic

contains


    subroutine exmp01_basic
        !...............................................................................
        ! Example 1 basics: Plot a simple x-y data set
        !...............................................................................

        ! create an instance of ogp
        type(gpf):: gp

        ! local variables and parameters
        integer, parameter:: n=50
        real(wp):: xv(n)
        real(wp):: yv(n)
        real(wp), parameter :: pi=4.d0*atan(1.d0)

        ! create data
        xv=linspace(-pi,2.0d0*pi,n)  ! linspace is a utility function in ogpf module
        yv=sin(xv)                   ! linspace(a,b,n) create a linear vector
        ! in [a,b] with n elements


        ! start plotting

        ! create a new figure
        call gp%figure(fig_title="ogp Example 01 Basics")

        ! plot data
        call gp%plot(xv,yv, 'title "plt 1. sin(x)"')

        ! add plot title
        call gp%title('My first plot in ogp')

        ! add x and y labels
        call gp%xlabel('this is x label')
        call gp%ylabel('this is y label')

        ! show plot
        call gp%show()


    end subroutine exmp01_basic


    subroutine exmp02_basic
        !...............................................................................
        ! Example 3 basics: Plot several set of data
        !...............................................................................

        ! create an instance of ogp
        type(gpf):: gp

        ! local variables and parameters
        real(wp), parameter :: pi=4.d0*atan(1.d0)
        real(wp), allocatable :: xv(:)
        real(wp), allocatable :: yv(:), zv(:), wv(:)


        ! create data
        xv=arange(0.0d0,pi,0.1d0)  ! arange(a,b,s) is an ogp helper function creates a
        yv = xv*sin(xv)            ! vector from a to b by step s
        zv = sin(xv)
        wv = cos(xv)

        ! plot data
        call gp%plot(xv, yv, 'title "x.sin(x)"', &
                     xv, zv, 'title "sin(x)"', &
                     xv, wv, 'title "cos(x)"')


        ! add plot title
        call gp%title('Plot several set of data with together')

        ! add x and y labels
        call gp%xlabel('this is x label')
        call gp%ylabel('this is y label')

        ! show plot
        call gp%show()

    end subroutine exmp02_basic




    subroutine exmp03_basic
        !...............................................................................
        ! Example 3 basics: Plot using figures (multi window)
        !...............................................................................

        ! create an instance of ogp
        type(gpf):: gp

        ! local variables and parameters
        real(wp), parameter :: pi=4.d0*atan(1.d0)
        real(wp), allocatable :: xv(:)
        real(wp), allocatable :: yv(:), zv(:), wv(:)


        ! create data
        xv=arange(0.0d0,pi,0.1d0)
        yv = sin(xv)
        zv = cos(xv)
        wv = xv*sin(xv)

        ! create a figure
        call gp%figure('My first window: sin and cos functions')
        ! plot data
        call gp%plot(xv, yv,'t "sin(x)"', xv, zv, 'title "cos(x)"')
        ! add plot title
        call gp%title('My first plot window')
        ! add x and y labels
        call gp%xlabel('x, rad')
        call gp%ylabel('sin(x), cos(x)')


         ! create second figure
        call gp%figure('Second window: x.sin')
        ! plot data
        call gp%plot(xv, wv,'t "x.sin(x)" lw 2.5 lt 7 ps 2 lc rgb "#bbee22"')
        ! add plot title
        call gp%title('Second winodw\nPractice linespecification')
        ! add x and y labels
        call gp%xlabel('x, ....')
        call gp%ylabel('x.sin(x)')

        ! show both figures
        call gp%show()

    end subroutine exmp03_basic


    subroutine exmp04_basic
        !...............................................................................
        ! Example 1 basics: Subplots and multiplots window
        !...............................................................................

        ! create an instance of ogp
        type(gpf):: gp

        ! local variables and parameters
        integer, parameter:: n=250
        real(wp), parameter:: a=-5.0_wp, b=5.0_wp
        real(wp):: xv(n)
        real(wp):: yv(n), zv(n)


        ! create random data
        call random_number(xv)
        call random_number(yv)
        call random_number(zv)
        ! map random data from [0,1] into [a,b]
        xv=(b-a)*xv+a
        yv=(b-a)*yv+a
        zv=(b-a)*zv+a


        ! start plotting

        ! create a new figure
        call gp%figure(fig_title="Multiplot window")

        ! add multiplot layout
        call gp%multiplot(1,2)

        ! add plots to figure
        ! the places will be filled row wise
        ! multiplot(1,2) -> 1
        call gp%plot(xv,yv, 't "x-y data" with points pt 7 ps 1.5 lc rgb "red"')
        call gp%title('Random number: first set')
        call gp%xlabel('x label')
        call gp%ylabel('y label')
        call gp%axis([-10.0_wp,10.0_wp,-10.0_wp,10.0_wp])

        ! plot at second place: multiplot(1,2) -> 2
        call gp%plot(xv, zv, 't "x-z data" with points pt 6 ps 1.25 lc rgb "blue"')
        call gp%holdon()
        call gp%plot(yv, zv, 't "y-z data" w points pt 6 ps 1.25 lc "dark-green"')
        call gp%title('Random number: second set')
        call gp%xlabel('x label')


        ! show plots
        call gp%show()

    end subroutine exmp04_basic

    subroutine exmp05_basic
        !...............................................................................
        ! Example 5 basics: Plot a simple x-y data with different options
        !...............................................................................

        ! create an instance of ogp
        type(gpf):: gp

        ! local variables and parameters
        integer, parameter:: n=35
        real(wp):: xv(n)
        real(wp):: yv(n)
        real(wp), parameter :: pi=4.d0*atan(1.d0)

        ! create data
        xv=linspace(-pi,2.0d0*pi,n)  !linspace is a utility function in ogpf module
        yv=sin(xv)                   ! linspace(a,b,n) create a linear vector
        ! in [a,b] with n elements


        ! start plotting

        ! create a new figure
        call gp%figure(fig_title="ogp Example 01 Basics")

        ! add multiplot layout
        call gp%multiplot(1,1)

        !add plots to figure
        call gp%plot(xv,yv, 'title "plt 1. sin(x)"')

        ! show plots
        call gp%show()

        ! rewrite the previous plot with new options
        ! linetype: 6, line with: 2, point type: 6
        ! point size (ps): 2
        call gp%plot(x1=xv, y1=yv, ls1='title "sin(x)" lt 3 lw 2 pt 6 ps 2')
        call gp%show()

    end subroutine exmp05_basic



    subroutine exmp06_basic
        !...............................................................................
        ! Example 3 basics: Plot a several data set at the same time
        !...............................................................................
        ! Ref:
        ! https://www.r-bloggers.com/four-beautiful-python-r-matlab-and-mathematica-plots-with-latex/

        ! create an instance of ogp
        type(gpf):: gp

        ! local variables and parameters
        integer, parameter:: n=201
        real(wp), dimension(n) :: xv(n)
        real(wp), dimension(4,n) :: jv
        integer :: i

        ! create data
        xv=linspace(0.0_wp,20.0_wp,n)  !linspace is a utility function in ogpf module
        do i=0, 3
            jv(i+1,:) = bessel_jn(i, xv)
        end do

        ! start plotting

        ! create a new figure
        call gp%figure(fig_title="ogp Example 03 Basics")

        ! add multiplot layout
        call gp%multiplot(1,1)

        !add plots to figure
        call gp%plot(x1=xv, y1=jv(1,:), ls1='title "J0" w lines lw 1.5', &
            x2=xv, y2=jv(2,:), ls2='title "J1" w lines lw 1.5', &
            x3=xv, y3=jv(3,:), ls3='title "J2" w lines lw 1.5', &
            x4=xv, y4=jv(4,:), ls4='title "J3" w lines lw 1.5'  &
            )

        ! show plots
        call gp%show()

    end subroutine exmp06_basic



    subroutine exmp07_basic
        !...............................................................................
        ! Example 4 basics: Plot a several data set at the same time
        !...............................................................................
        ! Ref:
        ! https://www.r-bloggers.com/four-beautiful-python-r-matlab-and-mathematica-plots-with-latex/

        ! create an instance of ogp
        type(gpf):: gp

        ! local variables and parameters
        integer, parameter:: n=201, m=4
        real(wp), dimension(n) :: xv(n)
        real(wp), dimension(n, m) :: jv

        integer :: i


        ! create data
        ! first set
        xv=linspace(0.0_wp, 20.0_wp, n)  !linspace is a utility function in ogpf module
        do i=1, m
            jv(:, i) = bessel_jn(i-1, xv)
        end do

        ! start plotting

        ! create a new figure
        call gp%figure(fig_title="ogp Example 7.a Basics")

        ! add multiplot layout
        call gp%multiplot(2,2)


        !add plots to figure
        call gp%plot(x1=xv, y1=jv(:,1), ls1='title "J0" w lines lw 1.5 lc "blue"  ')
        call gp%title('Bessel J0')
        call gp%xlabel('x,...')
        call gp%ylabel('Jn,...')
        !!
        !!
        !!        call gp%plot(x1=xv, y1=jv(:,2), ls1='title "J1" w lines lw 1.5 lc "red"   ')
        !!  !!      call gp%title('Bessel J1')
        !!        call gp%plot(x1=xv, y1=jv(:,3), ls1='title "J2" w lines lw 1.5 lc "dark-green" ')
        !!        call gp%plot(x1=xv, y1=jv(:,4), ls1='title "J3" w lines lw 1.5 lc "brown"')
        !
        !!
        !!        ! another example
        !!        ! Plot several data with together
        !!        call gp%figure(fig_title="ogp Example 04.b Basics")
        !!        call gp%plot(x1=xv, y1=jv(:,1), ls1='title "J0" w lines lw 1.5', &
            !!            x2=xv, y2=jv(:,2), ls2='title "J1" w lines lw 1.5', &
            !!            x3=xv, y3=jv(:,3), ls3='title "J2" w lines lw 1.5', &
            !!            x4=xv, y4=jv(:,4), ls4='title "J3" w lines lw 1.5'  &
            !!            )
        !!        call gp%title('plot several vectors at the same time')
        !!        call gp%xlabel('The x values')
        !!        call gp%ylabel('Bessel Jn')
        !!! Debug (Mohammad#1#02/11/18): The plot title before any plot cause errors. This is true for label also
        !!

        ! show plots
        call gp%show()

    end subroutine exmp07_basic


    !...............................................................................
    ! Example 08: A simple plot with logarithmic axes
    !...............................................................................
    subroutine exmp08_basic
        type(gpf):: gp
        integer, parameter :: n=570
        real(wp), allocatable:: x1(:)
        real(wp), allocatable:: y1(:)
        real(wp):: x2(n)
        real(wp):: y2(n)
        real(wp), parameter :: pi=4.d0*atan(1.d0)

        ! 1. create data
        x1=arange(0.01_wp,20.0_wp,0.01_wp)
        y1=sin(2.0*pi*x1)


        x2=linspace(0.0d0,10.d0,n)
        y2=10**(exp(-x2/2.d0)*sin(2.d0*x2))


        ! se general options
        call gp%options('set grid mxtics mytics xtics ytics lt 1 lc rgb "gray70", lt 1 lc rgb "gray90"')


        ! sample 1.
        call gp%figure('semilogx')
        call gp%semilogx(x1,y1,'w lines lw 1.5 lc rgb "#55aa00"')
        call gp%title('plot using logarithmic x scale')



        ! sample 2
        call gp%figure('semilogy plot')
        call gp%semilogy(x2,y2, 'w lines lw 1.5 lc rgb "#ff5588"')
        call gp%title('plot using logarithmic y scale')

        call gp%holdon()
        call gp%plot(x2,1.5*y2,'w lines')

        call gp%figure()
        call gp%plot(x2,y2,'w lines lc "dark-red" t "new plot"')

        call gp%show()


    end subroutine exmp08_basic




    subroutine exmp09_basic
        !...............................................................................
        ! Example 09: Plot a matrix against a vector
        !...............................................................................
        type(gpf):: gp
        integer, parameter  :: n=4
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

        call gp%plot   (x, y)
        !Draw the matrix y against vector x
        call gp%title  ('Example 9. Plotting a Matrix against a vector')
        call gp%xlabel ('my x axis')
        call gp%ylabel ('my y axis')

        call gp%show()
    end subroutine exmp09_basic

    subroutine exmp10_basic
        !...............................................................................
        !Plot a matrix against a vector
        !...............................................................................
        type(gpf):: matplot
        integer, parameter:: n=35, m=6
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

        call matplot%plot(t, y)
        !Draw the matrix y againest vector x
        call matplot%title('Example 10.a Plotting a Matrix against a vector')
        call matplot%xlabel ('t, sec')
        call matplot%ylabel ('y, feet')
        call matplot%axis( [0._wp,10._wp, 0._wp, 400._wp] )

        call matplot%holdon()

        !Another Matrix plot with legends [line specification]
        call matplot%plot(t, 2.5d0*y(:,2:4)) !, lspec='t "vo=125.0";t "vo=187.5"; t "vo=250.0"')
        call matplot%title('Example 10.b Using legends when plotting a matrix against a vector')

        call matplot%show()

    end subroutine exmp10_basic




    subroutine exmp11_basic
        !...............................................................................
        ! Example 11: Plot a matrix against a vector in logarithmic x-scale
        !...............................................................................
        type(gpf):: gp
        integer, parameter  :: n=300
        real(wp), parameter :: pi=4.d0*atan(1.d0)
        real(wp):: x(n)
        real(wp):: y(n,3)

        !Create data
        x=[linspace(0.01_wp, pi/4.0, 75), linspace(pi/4.0, 3.0*pi, 225)]
        y(:,1)=sin(pi*x)
        y(:,2)=cos(0.5*pi*x)
        y(:,3)=cos(1.5*pi*x)

        call gp%options('set style data lines')
        call gp%options('set key left top')
        call gp%semilogx(x, y, 't "sin(\pi x)" lw 1.5; t "cos(0.5\pi x)" lw 1.5; t "cos(1.5\pi x)" lw 2.0')
        !Draw the matrix y against vector x
        call gp%title  ('Example 11. Plotting a Matrix in logarithmic x-scale')
        call gp%xlabel ('x-logarithmic')
        call gp%ylabel ('sin, cos')

        call gp%show()
    end subroutine exmp11_basic




    !...............................................................................
    ! Example 12: A simple plot with logarithmic axes
    !...............................................................................
    subroutine exmp12_basic
        type(gpf):: gp
        integer, parameter :: n=570
        real(wp):: x(n)
        real(wp):: y(n,2)
        real(wp), parameter :: pi=4.d0*atan(1.d0)

        ! 1. create data
        x=linspace(0.01d0,08.d0,n)
        y(:,1)=10**(exp(-x/2.d0)*sin(6.d0*x))
        y(:,2)=10**(exp(-x/2.d0)*sin(3.d0*x))


        ! se general options
        call gp%options('set grid mxtics mytics xtics ytics lt 1 lc rgb "gray70", lt 1 lc rgb "gray90"')

        ! start plot
        call gp%figure('semilogy matrix plot')
        call gp%semilogy(x,y, 'w lines lw 1.5 lc rgb "dark-blue" t "y(:,1)";'// &
                              'w lines lc "dark-red" t "y(:,2)"')
        call gp%title('plot a matrix using logarithmic y scale')
        call gp%xlabel('x, rad ...')
        call gp%ylabel('logarithmic y ...')

        call gp%show()


    end subroutine exmp12_basic


    subroutine Exmp15_basic
        !...............................................................................
        ! Example 11 basics: Plot multiple functions using fplot
        !...............................................................................
        type(gpf) :: gp

        ! local variables
        real(wp), parameter :: pi=4.d0*atan(1.d0)

        ! create a new figure
        call gp%figure(fig_title="ogp Example 11: fplot")

        ! plot the function fcosin using fplot
        call gp%fplot(fcosin, [-2.0*pi, 2.0*pi], 45,'t "x.cos(x)" lt 7 lw 1.5 ps 1.5 lc rgb "#00bb00"')


        ! add annotation
        call gp%title(' Plotting using fplot')
        call gp%xlabel('x, radian')
        call gp%ylabel('x.cos(x)')

        ! set the axes range
        call gp%axis([-2.0*pi, 2.0*pi])

        ! show plots
        call gp%show()

    contains
        ! nested function
        function fcosin(x) result (y)
            ! myfun computes x.sin(x)

            real(wp), intent(in) :: x
            real(wp) :: y
            y= x*cos(x)
        end function fcosin


    end subroutine Exmp15_basic



    subroutine Exmp16_basic
        !...............................................................................
        ! Example 16 basics: Plot multiple functions using fplot
        !...............................................................................
        type(gpf) :: gp

        ! local variables
        real(wp), parameter :: pi=4.d0*atan(1.d0)
        intrinsic :: dtan
        ! create a new figure
        call gp%figure(fig_title="ogp Example 12: multiple fplot")


        ! hold plot
        call gp%holdon()

        ! plot an intrinsic function
        call gp%fplot(dtan, [-1.326_wp, 1.326_wp],   30,'t "tan(x)" lt 7 lw 1.5 ps 1.5 lc "red"')
        call gp%fplot(dtan, [pi/2.0+0.25, 4.0_wp],   30,'t "tan(x)" lt 7 lw 1.5 ps 1.5 lc "red"')
        call gp%fplot(dtan, [-4.0_wp, -pi/2.0-0.25], 30,'t "tan(x)" lt 7 lw 1.5 ps 1.5 lc "red"')


        ! plot the function fsin using fplot
        call gp%fplot(fsin, [-4.0_wp, 4.0_wp], 35,'t "x.sin(x)" lt 7 lw 1.5 ps 1.5 lc "blue"')
        ! add another function plot (fcosin)
        call gp%fplot(fcosin, [-4.0_wp, 4.0_wp], 45,'t "x.cos(x)" lt 7 lw 1.5 ps 1.5 lc rgb "#00bb00"')


        ! add annotation
        call gp%title(' Plotting using fplot')
        call gp%xlabel('x, radian')
        call gp%ylabel('x.cos(x), tan(x), sin(x)/x')



        ! set the axes range
        call gp%axis([-4.0_wp, 4.0_wp, -4.0_wp, 4.0_wp])
        ! show plots
        call gp%show()

    contains
        ! nested functions
        function fsin(x) result (y)
            ! myfun computes x.sin(x)

            real(wp), intent(in) :: x
            real(wp) :: y
            if (x /= 0._wp) then
                y= sin(x)/x
            else
                y=1.0_wp
            end if

        end function fsin

        function fcosin(x) result (y)
            ! myfun computes x.sin(x)

            real(wp), intent(in) :: x
            real(wp) :: y
            y= x*cos(x)
        end function fcosin


    end subroutine Exmp16_basic

end module md_demo_01_basics



!!!
!!!    subroutine exmp02_basic
!!!        !...............................................................................
!!!        ! Example 2 basics: Plot two x-y data sets
!!!        !...............................................................................
!!!        !
!!!        ! Plot a circle centered at the point (4,3) with a radius equal to 2. Use axis equal to use
!!!        ! equal data units along each coordinate direction.
!!!        ! create an instance of ogp
!!!        type(gpf):: gp
!!!
!!!        ! local variables and parameters
!!!        integer, parameter:: n=50
!!!        real(wp):: theta(n)
!!!        real(wp):: xv(n), yv(n)
!!!        real(wp), parameter :: pi=4.d0*atan(1.d0)
!!!        real(wp) :: r, xc, yc
!!!
!!!        ! create data
!!!        r  = 2.0d0
!!!        xc = 4.0d0
!!!        yc = 3.0d0
!!!
!!!        theta=linspace(0.0d0,2.0d0*pi,n)  !linspace is a utility function in ogpf module
!!!
!!!        xv = r*cos(theta) + xc;
!!!        yv = r*sin(theta) + yc;
!!!
!!!        ! plot data
!!!        call gp%plot(xv,yv)
!!!
!!!        ! add plot title
!!!        call gp%title('Circle plot')
!!!
!!!        ! add x and y labels
!!!        call gp%xlabel('this is x label')
!!!        call gp%ylabel('this is y label')
!!!
!!!        ! show plot
!!!        call gp%show()
!!!
!!!    end subroutine exmp02_basic
!!!



!!!!
!!!!
!!!!
!!!!
!!!!
!!!!
!!!!    subroutine Exmp011
!!!!        !...............................................................................
!!!!        ! Example 1: Plot several data set at the same time
!!!!        !...............................................................................
!!!!        type(gpf):: gp, gp1
!!!!        integer, parameter:: n=50
!!!!        integer, parameter:: m=65
!!!!        real(wp):: x(n)
!!!!        real(wp):: y(n)
!!!!        real(wp):: xv(m)
!!!!        real(wp):: yv(m)
!!!!        real(wp), parameter :: pi=4.d0*atan(1.d0)
!!!!
!!!!        ! Input data
!!!!        x=linspace(-pi,2.0d0*pi,n)  !linspace is a utility function in ogpf module
!!!!        y=sin(x)              !linspace(a,b,n) create a linear vector in [a,b] with n elements
!!!!
!!!!        xv=linspace(0.d0, 2.d0*pi,m)
!!!!        yv=cos(2.d0*xv)*exp(-xv)
!!!!
!!!!        ! create a new figure
!!!!        call gp%figure(fig_title="ogp Example 01")
!!!!        ! add multiplot layout
!!!!        call gp%multiplot(3,1)
!!!!
!!!!        !add plots to figure
!!!!        call gp%plot(xv,yv,   'title "plt 1. sin(x)" with lp lt 5 pt 8') !,xv,yv,'title "cos" with lp')
!!!!        call gp%plot(xv,yv, 'title "plt 2. cos(x)" with lp')
!!!!        call gp%plot(x1=x,y1=y,ls1='with linespoints pt 8 lt 6',x2=xv,y2=yv,ls2='title "plt 3. cos(2x)"')
!!!! !!       call gp%plot(xv,yv, 'title "plt 1112. cos(x)" with lp')
!!!! !!       call gp%plot(x,sin(x)*cos(x))
!!!! !call gp%show()
!!!!call gp%show()
!!!!stop
!!!!
!!!!        call gp%figure(fig_title="ogp Example 02")
!!!!        call gp%multiplot(1,1)
!!!!        call gp%plot(xv, yv, ls1='title "my second fig"')
!!!!      !!  call gp%xlabel('My x')
!!!!
!!!!        call gp%figure(fig_title="ogp Example 03")
!!!!        call gp%multiplot(1,2)
!!!!        call gp%plot(yv, ls1='title "yi vs. i"')
!!!!        call gp%plot(xv, yv)
!!!!        call gp%show()
!!!!
!!!!stop
!!!!        call gp1%figure(fig_title="ogp Example 04")
!!!!        call gp1%multiplot(1,2)
!!!!        call gp1%plot(x,y)
!!!!!        call gp%hold(.true.)
!!!!        call gp1%plot(xv)
!!!!        call gp1%show()
!!!!
!!!!    end subroutine Exmp011



!!!!!
!!!!!    subroutine exmp01
!!!!!        !...............................................................................
!!!!!        ! Example 1: Plot several data set at the same time
!!!!!        !...............................................................................
!!!!!        type(tpplot):: plt
!!!!!        integer, parameter:: n=3, nn=4
!!!!!        integer, parameter:: m=5
!!!!!        real(wp):: x(n), xx(nn), yy(nn)
!!!!!        real(wp):: y(n)
!!!!!        real(wp):: xv(m)
!!!!!        real(wp):: yv(m)
!!!!!        real(wp), parameter :: pi=4.d0*atan(1.d0)
!!!!!
!!!!!        ! Input data
!!!!!        x=linspace(-pi,2.0d0*pi,n)  ! linspace is a utility function in ogpf module
!!!!!        y=sin(x)                    ! linspace(a,b,n) create a linear vector in [a,b] with n elements
!!!!!
!!!!!        xv=linspace(0.d0, 2.d0*pi,m)
!!!!!        yv=cos(2.d0*xv)*exp(-xv)
!!!!!
!!!!!
!!!!!        xx=linspace(-5.d0,5.d0, nn)
!!!!!        yy=sin(xx)+cos(xx)
!!!!!
!!!!!        ! Sample 1: Plot to draw two set of data
!!!!!        call plt%plot(x1=xv, y1=yv, ls1='title "crv 1."', &
    !!!!!            x2=xv, y2=yv, ls2='title "crv 2."', &
    !!!!!            x3=x,  y3=y,  ls3='title "crv 3. sin(x)" with lp lt 5 pt 8', &
    !!!!!            hold_status=.false. ) !,xv,yv,'title "cos" with lp')
!!!!!       call plt%print_list()
!!!!!
!!!!!
!!!!!        call plt%plot(xv,yv, 'title "crv 4. cos(x)" with lp', hold_status=.false.)
!!!!!        call plt%plot(x1=x, y1=y, ls1='title "crv 5." with linespoints pt 8 lt 6', &
    !!!!!            x2=xv,y2=yv,ls2='title "crv 6. cos(2x)"', hold_status=.true.)
!!!!!        call plt%plot(xv,yv, 'title "crv 7. cos(x)" with lp', hold_status=.false.)
!!!!!        call plt%plot(linspace(0._wp,1._wp,6), ls1='title "crv 8."', hold_status=.true.)
!!!!!
!!!!!        call plt%plot(xx,yy, 'title "crv 9."', hold_status=.true.)
!!!!!
!!!!!        call plt%print_list()
!!!!!    end subroutine exmp01


!subroutine exmp08
!    !...............................................................................
!    !Plot a matrix against a vector
!    !...............................................................................
!    type(tpplot):: matplot
!    integer, parameter:: n=10, ncol=6
!    integer :: i
!    real(wp):: tf
!    real(wp):: vo
!    real(wp):: g
!    real(wp):: t(n)
!    real(wp):: y(n,ncol)
!
!    real(wp):: xv(n)
!    real(wp):: yv(n)
!
!
!    !Create data
!    tf=10.d0
!    g=32.d0;
!    t=linspace(0.d0,tf,n)
!    do i = 1, ncol
!        vo = 25.0d0 * i
!        y(:, i) = vo*t-0.5d0*g*t**2
!    end do
!
!
!    xv=linspace(0.d0, 6.2832d0, n)
!    yv=cos(2.d0*xv)*exp(-xv)
!
!
!    !Add a vector plot
!    call matplot%plot(xv,yv, 'title "crv 1. cos(x)" with lp', hold_status=.false.)
!
!
!    !Draw the matrix y againest vector x
!    call matplot%plot(t, y,hold_status=.true.)
!    !call matplot%print_list()
!    !stop
!    !Another Matrix plot with legends [line specification]
!    call matplot%plot(t, y(:,3:4), lspec=["t 'vo=100'", "t 'vo=125'"],hold_status=.true.)
!
!  !Add a vector plot
!  call matplot%plot(xv,yv, 'title "crv 10. "', hold_status=.true.)
!
!  call matplot%print_list()
!
!end subroutine exmp08



