# https://linuxgazette.net/133/luana.html

set size ratio -1
 set nokey
 set noxtics
 set noytics
 set noborder
 set parametric
#
 x(t) = (R-r)*cos(t) + p*cos((R-r)*t/r)
 y(t) = (R-r)*sin(t) - p*sin((R-r)*t/r)
#
# Greatest common divisor:
 gcd(x,y) = (x%y==0 ? y : gcd(y,x%y))

# change these integer parameters to get very nice drawings
 R = 100; r = 2; p = 70; res = 175

 #
 rr = abs(r)
 nturns = rr / gcd(R,rr)
 samp = 1 + res * nturns
 set samples samp
#
plot [t=0:nturns*2*pi] x(t),y(t)  