module md_demo_02_intermediates
    ! md_demo_02_intermediates

    ! load gpf library
    use md_gpf
    use select_precision, only : wp


    ! parameters and variables
    implicit none

contains

   subroutine exmp01_intermediate
        !...............................................................................
        ! Example 1 intermediate: Plot a simple x-y data set
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
        yv=sin(xv)


        ! set some gnuplot session wide options
        call gp%options('unset key')
        call gp%options('set style data boxes')
        call gp%options('set boxwidth 0.15')
        call gp%options('set xtic rotate by -45 scale 0')


        ! start plotting
        ! create a new figure
        call gp%figure(fig_title="ogp Example 01 Intermediate")

        ! plot data
        call gp%plot(xv,yv, 'title "plt 1. sin(x)"')

        ! add plot title
        call gp%title('Plot with general options')

        ! add x and y labels
        call gp%xlabel('this is x label')
        call gp%ylabel('this is y label')

        ! set axis range
        call gp%axis([0.0_wp,pi, 0.0_wp, 1.20_wp])

        ! show plot
        call gp%show()


    end subroutine exmp01_intermediate


end module md_demo_02_intermediates
