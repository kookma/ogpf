# Modern-Fortran-Programming
## ogpf

Object Based Interface to GnuPlot from Fortran (ogpf)

### GnuPlot Interface

Purpose:   Object Based Interface to GnuPlot from Fortran (ogpf)
Platform:  Windows XP/Vista/7/10
           (It should work on other platforms, see the Write2GnuPlot subroutine below)
Language:  Fortran 2003 and 2008
Requires:  1. Fortran 2003 compiler (e.g gfortran 4.7, IVF 12.1, and later ...)
           2. gnuplot 4.5 and higher (other previous version can be used
Author:    Mohammad Rahmani
		   Chem Eng Dep., Amirkabir Uni. of Tech
           Tehran, Ir


#### Revision History

Version:  0.16
Date:     Feb 11th, 2016
  Minor corrections
  Correct the lspec processing in plot2D_matrix_vs_vector
  Now, it is possible to send less line specification and gpf will cycle through lspec

Version:  0.15
Date:     Apr 20th, 2012
  Minor corrections
  Use of select_precision module and working precision: wp

Version:  0.14
Date:     Mar 28th, 2012
  Minor corrections
  Use of import keyboard and removing the Precision module
  Length of Title string increased by 80 chars


Version:  0.13
Date:     Feb 12th, 2012
  Minor corrections
  Added axis method which sets the axis limits for x-axis, y-axis and z-axis
  Added Precision module



Version:  0.12
Date:     Feb 9th, 2012
  Minor corrections
  New semilogx, semilogy, loglog methods
  New options method, allow to be called several times to set the gnuplot options



Version:  0.11
Date:     Feb 9th, 2012
  Minor corrections
  Use of NEWUINT specifier from Fortran 2008
  Added configuration parameters
  Extra procedures have been removed
  Temporary file is now deleted using close(...,status='delete')


Version:  0.1
Date:     Jan 5th, 2012
First object-based version




# Modern Fortran Programming
This is a collection of repositories for developing fortran codes in modern fortran 2003, 2008, and later standard.
It contains several light projects including: 
+ ogpf: Object based interface to gnuplot from fortran 
+ rkf: RKF revisited[under development] 
+ . . .
