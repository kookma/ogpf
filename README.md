## ogpf
Object Based Interface to GnuPlot from Fortran (ogpf)

### GnuPlot Interface

	Purpose:   Object Based Interface to GnuPlot from Fortran (ogpf)
	Platform:  Windows XP/Vista/7/10, Linux
			   (It should work on other platforms, e.g Mac see the Write2GnuPlot subroutine in ogpf.f90)
	Language:  Fortran 2003 and 2008
	Requires:  1. Fortran 2003 compiler (e.g gfortran 4.7, IVF 12.1, and later ...)
			       2. gnuplot 4.5 and higher (other previous version can be used
	Author:    Mohammad Rahmani
			       Chem Eng Dep., Amirkabir Uni. of Tech

### Demo
There is a collection of example in demo.f90 to show the capabilities of ogpf.

### Easy to use
To use ogpf in your project, add these to files to your fortran project (code)
* ogpf.f90 (the main library)
* select_precision.f90 (to choose the working precision)
For details see 'demo.f90'
