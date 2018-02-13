module md_errhandling
    implicit none
    !private

    public :: gp_err_code
    public :: gp_error

    enum, bind(c)

        enumerator :: gp_err_code = 0 ! we use this to name the enumeration
        enumerator :: alloc_err = 101
        enumerator :: bad_array_size  = 102
        enumerator :: not_found = 103
    end enum


contains
    subroutine gp_error(code, md_name, proc_name, msg)
        ! gp_error
        integer(kind(gp_err_code)), intent(in) :: code
        character(len=*), intent(in) :: md_name
        character(len=*), intent(in) :: proc_name
        character(len=*), intent(in), optional :: msg
        print*
        print*, '--------------------------------------------------'
        print*, 'ogp Error occured'
        print*, '--------------------------------------------------'
        print*, 'ogp halted'
        print*, 'Error code:', code
        print*, 'loc: ', md_name //':'// proc_name
        if (present(msg)) then
            print*, 'Error message: ', msg
        end if
        stop
    end subroutine gp_error


end module md_errhandling
