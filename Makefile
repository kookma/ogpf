#-------------------------------------------------------------------------------
#    GnuPlot Interface
#-------------------------------------------------------------------------------
#    Purpose:   Object Based Interface to GnuPlot from Fortran (ogpf)
#    Platform:  Windows XP/Vista/7/10
#               (It should work on other platforms, see the Write2GnuPlot subroutine below)
#    Language:  Fortran 2003 and 2008
#    Requires:  1. Fortran 2003 compiler (e.g gfortran 4.7, IVF 12.1, ...) or 2008
#               2. gnuplot 4.5 and higher (other previous version can be used
#    Author:    Mohammad Rahmani
#               Chem Eng Dep., Amirkabir Uni of Tech
#               Tehran, Ir
#               url: aut.ac.ir/m.rahmani
#               email: m[dot]rahmani[at]aut[dot]ac[dot]ir
#
# This Makefile can compile, link and create the 
# demo.exe to demonstrate the capabilities of ogpf code
# This Makefile works on all version of Windows and can be modified
# to work on Linux and mac

all:
	gfortran -Wall -O -c select_precision.f90 
	gfortran -Wall -O -c ogpf.f90 
	gfortran -Wall -O -c demo.f90 
	gfortran -o demo.exe select_precision.o ogpf.o demo.o
clean:
	rm *.o
	rm *.mod
	rm demo.exe
demo:
	demo.exe