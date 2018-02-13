! Mohammad Rahmani
! Chemical Engineering Department
! Amirkabir University of Technology
! Tehran, Ir
! m.rahmani[at]aut.ac.ir
! https:://github.com/kookma
!
! Tuesday 2018-02-13

program demo
    use select_precision, only: wp

    use md_demo_01_basics
    use md_demo_02_intermediates
    use md_demo_03_advanced
    implicit none
    integer :: order


    do
        print*, 'select an example: 1:12'
        read*, order
        if (order == 0) exit
        call demonstration(order)
    end do


contains
    subroutine demonstration(order)
        ! demonstration
        integer, intent(in) :: order



        select case (order)
            case (1)
                call exmp01_basic()
            case (2)
                call exmp02_basic()
            case (3)
                call exmp03_basic()
            case (4)
                call exmp04_basic()
            case (5)
                call exmp05_basic()
            case (6)
                call exmp06_basic()
            case (7)
                call exmp07_basic()
            case (8)
                call exmp08_basic()
            case (9)
                call exmp09_basic()
            case (10)
                call exmp10_basic()
            case (11)
                call exmp11_basic()
           case (12)
                call exmp12_basic()
            case default
                print*, 'There is no such example'
        end select

        ! for intel only
        !    print*, 'press any key to continue ...'
        !    read*

    end subroutine demonstration

end program demo


