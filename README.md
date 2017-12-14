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

### Procedures
**plot**

* plot a single vector v
* plot a vector y against vector x
* plot up to four pairs of x-y set at the same time
* plot a matrix Y versus a vector x

**surf**

* surface plot
* mesh plot
* contour plot

**script**
 use a customised script to plot data from inside a fortran code
**meshgrid**
 generate mesh grid over a rectangular domain of [xmin xmax, ymin, max]
**linspace**
 returns a linearly spaced vector with n points in [a, b]

### Demo
There is a collection of examples in demo.f90 to show the capabilities of ogpf.

### Easy to use
To use ogpf in your project, add these to files to your fortran project (code)
* ogpf.f90 (the main library)
* select_precision.f90 (to choose the working precision)
For details see 'demo.f90'
