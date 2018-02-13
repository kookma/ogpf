module md_parameters
    ! This module defines constants for ogpf

    implicit none

    private

    public :: &
        gnuplot_term_type,          &
        gnuplot_term_size,          &
        gnuplot_term_number,        &
        gnuplot_term_bgcolor,       &
        gnuplot_term_title,         &
        gnuplot_scriptfile_name,    &
        gnuplot_file_extension

    !...............................................................................
    !Terminal setting parameters
    character(len=*), parameter :: gnuplot_term_type    = "wxt" !other termials: windows, qt, png,
    integer, parameter          :: gnuplot_term_size(2) = [720,480]
    integer, parameter          :: gnuplot_term_number  = 0
    character(len=7), parameter :: gnuplot_term_bgcolor = "#ffffff" !background color is white
    character(len=*), parameter :: gnuplot_term_title   = "ogpf window"

    character(len=*), parameter :: gnuplot_scriptfile_name   = "ogpf_scriptfile_" !temporary file for output
    character(len=*), parameter :: gnuplot_file_extension    = ".gpt"             !temporary file extension

    !...............................................................................
    !Color constants

    !...............................................................................
    !Line styles

    !...............................................................................
    !Marker styles


end module md_parameters
